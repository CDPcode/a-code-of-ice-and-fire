module Westeros.SouthOfTheWall.TypeVer where

import qualified Westeros.SouthOfTheWall.Symtable as ST

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Bifunctor (second)

data Type
    = IntT
    | FloatT
    | CharT
    | BoolT
    | AtomT
    | AliasT String Type
    | ArrayT Type Int
    | TupleT [Type]
    | StructT [(String, Type)]
    | UnionT [(String, Type)]
    | PointerT Type
    | NullT
    | VoidT
    | TypeError
    deriving (Eq)

instance Show Type where
    show IntT = "Lanninteger"
    show FloatT = "Freyt"
    show CharT = "Starkhar"
    show BoolT = "Boolton"
    show AtomT = "Barathom"
    show (AliasT name t) = "Alias " ++ name ++ " to " ++ show t
    show (ArrayT t d) = "Lord Commander of [ " ++ show d ++ " dimentions of " ++ show t ++ " ]"
    show (TupleT ts) = "White Walker possessing ( 1" ++ unwords (map show ts) ++ " )"
    show (StructT t) = "King " ++ show t
    show (UnionT t) = "God of many faces " ++ show t
    show (PointerT t) = "Spearwife of * " ++ show t ++ " *"
    show NullT = "Null Pointer"
    show VoidT = "No One"
    show TypeError = "Type error: you should not be seeing this but you probably will"


getTypeFromString :: ST.Type -> ST.MonadParser Type
getTypeFromString base = case base of
    "_int"     -> return IntT
    "_float"   -> return FloatT
    "_char"    -> return CharT
    "_bool"    -> return BoolT
    "_atom"    -> return AtomT

    otherType  -> do
        espType <- ST.lookupST otherType
        case espType of
            Just entry  -> case ST.additional entry of
                Just info -> case info of
                    ST.AliasMetaData ST.ByName pointedType -> do
                        pType <- getTypeFromString pointedType
                        return $ AliasT otherType pType
                    ST.AliasMetaData ST.ByStructure pointedType -> getTypeFromString pointedType
                    ST.DopeVector tp dim -> do
                        arrType <- getTypeFromString tp
                        return $ ArrayT arrType dim
                    ST.PointedType tp -> do
                        ptrType <- getTypeFromString tp
                        return $ PointerT ptrType
                    ST.StructScope scope -> do
                        symT <- get
                        let fields = ST.filterByScopeST symT scope
                        types <- buildRecordTypes $ map (second head) $ M.toList fields
                        return $ StructT types
                    ST.UnionScope scope -> do
                        symT <- get
                        let fields = ST.filterByScopeST symT scope
                        types <- buildRecordTypes $ map (second head) $ M.toList fields
                        return $ UnionT types
                    ST.TupleTypes xs             -> do
                        types <- mapM getTypeFromString xs
                        return $ TupleT types
                    _                            -> return TypeError
                Nothing -> return TypeError
            Nothing -> return TypeError


buildRecordTypes :: [(String, ST.SymbolInfo)] -> ST.MonadParser [(String, Type)]
buildRecordTypes [] = return []
buildRecordTypes ((name, info):xs) = do
    rest <- buildRecordTypes xs
    t <- getTypeFromString $ fromJust $ ST.symbolType info
    return $ (name, t) : rest

-- Use to check if recursive types have some leave with a type error.
notTypeError :: Type -> Bool
notTypeError (TupleT xs)  = all notTypeError xs
notTypeError (PointerT e) = notTypeError e
notTypeError (ArrayT tp _) = notTypeError tp
notTypeError TypeError    = False
notTypeError _            = True

-- TODO: Should we add here checkAssignable struct tuple (?)
checkAssignable :: Type -> Type -> Bool
checkAssignable PointerT{} NullT = True
checkAssignable (PointerT lType) (PointerT rType) = checkAssignable lType rType
checkAssignable (TupleT lTypes) (TupleT rTypes) = and $ zipWith checkAssignable lTypes rTypes
checkAssignable (StructT lTypes) (TupleT rTypes) = 
    and $ zipWith checkAssignable (map snd lTypes) rTypes
checkAssignable (StructT lTypes) (StructT rTypes) = 
    and $ zipWith checkAssignable (map snd lTypes) (map snd rTypes)
checkAssignable VoidT _ = False
checkAssignable _ VoidT = False
checkAssignable (AliasT lType _) (AliasT rType _) = lType == rType
checkAssignable _ (AliasT _ _) = False
checkAssignable (AliasT _ _) _ = False
checkAssignable _ TypeError = True
checkAssignable TypeError _ = True
checkAssignable lType rType = lType == rType

isPrimitiveType :: Type -> Bool
isPrimitiveType IntT = True
isPrimitiveType FloatT = True
isPrimitiveType CharT = True
isPrimitiveType BoolT = True
isPrimitiveType AtomT = True
isPrimitiveType _ = False

isPrimitiveAlias :: Type -> Bool
isPrimitiveAlias (AliasT _ t) = isPrimitiveType t
isPrimitiveAlias _ = False

isRecordOrTupleType :: Type -> Bool
isRecordOrTupleType (TupleT _)  = True
isRecordOrTupleType (AliasT _ t) = isRecordOrTupleType t
isRecordOrTupleType t = isRecordType t

isRecordType :: Type -> Bool
isRecordType (StructT _) = True
isRecordType (UnionT _)  = True
isRecordType (AliasT _ t) = isRecordType t
isRecordType _ = False

isArrayType :: Type -> Bool
isArrayType (ArrayT _ _) = True
isArrayType (AliasT _ t) = isArrayType t
isArrayType _ = False

isStringType :: Type -> Bool
isStringType (ArrayT CharT 1) = True
isStringType (AliasT _ t) = isStringType t
isStringType _ = False

isPointerType :: Type -> Bool
isPointerType (PointerT _) = True
isPointerType (AliasT _ t) = isPointerType t
isPointerType _ = False

isIntegerType :: Type -> Bool
isIntegerType IntT = True
isIntegerType (AliasT _ t) = isIntegerType t
isIntegerType _ = False

isPointerToArray :: Type -> Bool
isPointerToArray (PointerT t) = isArrayType t
isPointerToArray (AliasT _ t) = isPointerToArray t
isPointerToArray _ = False

isPointerToRecordOrTuple :: Type -> Bool
isPointerToRecordOrTuple (PointerT t) = isRecordOrTupleType t
isPointerToRecordOrTuple (AliasT _ t) = isPointerToRecordOrTuple t
isPointerToRecordOrTuple _ = False

isCompositeType :: Type -> Bool
isCompositeType t = isRecordOrTupleType t || isArrayType t

isCasteable :: Type -> Type -> Bool
isCasteable source dest = source `elem` simpleType && dest `elem` simpleType
    where simpleType = [IntT, BoolT, CharT, FloatT]