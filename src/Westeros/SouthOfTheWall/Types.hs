module Westeros.SouthOfTheWall.Types (
      Type (..)
    , getTypeFromString
    , buildTypesFromDict
    , notTypeError
    , isArrayType
    , isCasteable
    , isPrimitiveType
    , isPrimitiveOrPointerType
    , checkAssignable
    , isRecordOrTupleType
    , isRecordType
    , isPointerToArray
    , isIntegerType
    , isStringType
    , isPointerType
    , isPointerToRecordOrTuple
    , isCompositeType
    , getPointedTypeString
    , getContainedTypeString
    , getTupleContainedTypeString
    , getTypeWidth
    ,getTypeAlign) where

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
    | MultiReturnT [Type]
    | StructT Int [(String, Type)]
    | UnionT Int [(String, Type)]
    | PointerT Type
    | NullT
    | TypeError
    | NothingT
    deriving (Eq)

instance Show Type where
    show IntT = "Lanninteger"
    show FloatT = "Freyt"
    show CharT = "Starkhar"
    show BoolT = "Boolton"
    show AtomT = "Barathom"
    show (AliasT name t) = "House " ++ name ++ " that comes from the lineage of <<" ++ show t ++ ">>"
    show (ArrayT t d) = "Lord Commander of [ " ++ show d ++ " armies of " ++ show t ++ " ]"
    show (TupleT ts) = "White Walker possessing ( " ++ unwords (map show ts) ++ " )"
    show (StructT _ t) = "King of { " ++ unwords (map (show . snd) t) ++ " }"
    show (UnionT _ t) = "God of many faces ruling over { " ++ unwords (map (show . snd) t) ++ " }"
    show (PointerT t) = "Spearwife of * " ++ show t ++ " *"
    show NullT = "Rickon"
    show TypeError = "Type error: you should not be seeing this but you probably will"
    show NothingT = "No One"
    show (MultiReturnT ts) = show $ TupleT ts


getTypeFromString :: ST.Type -> ST.MonadParser Type
getTypeFromString base = case base of

    -- Primitive types
    "_int"          -> return IntT
    "_float"        -> return FloatT
    "_char"         -> return CharT
    "_bool"         -> return BoolT
    "_atom"         -> return AtomT
    "_null"         -> return NullT
    "_type_error"   -> return TypeError

    -- Composite types
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
                        types <- buildTypesFromDict $ map (second head) $ M.toList fields
                        return $ StructT scope types
                    ST.UnionScope scope -> do
                        symT <- get
                        let fields = ST.filterByScopeST symT scope
                        types <- buildTypesFromDict $ map (second head) $ M.toList fields
                        return $ UnionT scope types
                    ST.TupleTypes xs             -> do
                        types <- mapM getTypeFromString xs
                        return $ TupleT types
                    _                            -> return TypeError
                Nothing -> return TypeError
            Nothing -> return TypeError

getPointedTypeString :: ST.Type -> ST.MonadParser (Maybe ST.Type)
getPointedTypeString base = case base of
    typeString  -> do
        espType <- ST.lookupST typeString
        case espType of
            Just entry  -> case ST.additional entry of
                Just info -> case info of
                    ST.PointedType tp -> return $ Just tp
                    _ -> return Nothing
                Nothing -> return Nothing
            Nothing -> return Nothing

getContainedTypeString :: ST.Type -> ST.MonadParser (Maybe ST.Type)
getContainedTypeString base = case base of
    typeString  -> do
        espType <- ST.lookupST typeString
        case espType of
            Just entry  -> case ST.additional entry of
                Just info -> case info of
                    ST.DopeVector tp _ -> return $ Just tp
                    _ -> return Nothing
                Nothing -> return Nothing
            Nothing -> return Nothing

getTupleContainedTypeString :: ST.Type -> Int -> ST.MonadParser (Maybe ST.Type)
getTupleContainedTypeString base idx = case base of
    typeString  -> do
        espType <- ST.lookupST typeString
        case espType of
            Just entry  -> case ST.additional entry of
                Just info -> case info of
                    ST.TupleTypes tps | idx < length tps -> return $ Just $ tps !! idx
                    _ -> return Nothing
                Nothing -> return Nothing
            Nothing -> return Nothing

getTypeWidth :: ST.Type -> ST.MonadParser (Maybe Int)
getTypeWidth typeString = do
    espType <- ST.lookupST typeString
    case espType of
        Just ST.SymbolInfo { ST.typeInfo = Just ST.TypeInfo { ST.width = w } } -> return $ Just w
        _ -> return Nothing

getTypeAlign :: ST.Type -> ST.MonadParser (Maybe Int)
getTypeAlign typeString = do
    espType <- ST.lookupST typeString
    case espType of
        Just ST.SymbolInfo { ST.typeInfo = Just ST.TypeInfo { ST.align = a } } -> return $ Just a
        _ -> return Nothing


buildTypesFromDict :: [(String, ST.SymbolInfo)] -> ST.MonadParser [(String, Type)]
buildTypesFromDict [] = return []
buildTypesFromDict ((name, info):xs) = do
    rest <- buildTypesFromDict xs
    t <- getTypeFromString $ fromJust $ ST.symbolType info
    return $ (name, t) : rest

-- Use to check if recursive types have some leave with a type error.
notTypeError :: Type -> Bool
notTypeError (TupleT xs)  = all notTypeError xs
notTypeError (PointerT e) = notTypeError e
notTypeError (ArrayT tp _) = notTypeError tp
notTypeError (StructT _ xs) = all (notTypeError . snd) xs
notTypeError (UnionT _ xs)  = all (notTypeError . snd) xs
notTypeError TypeError    = False
notTypeError _            = True

checkAssignable :: Type -> Type -> Bool
checkAssignable PointerT{} NullT = True
checkAssignable (TupleT lTypes) (TupleT rTypes) =
    length lTypes == length rTypes && and (zipWith checkAssignable lTypes rTypes)
checkAssignable (StructT _ lTypes) (TupleT rTypes) =
    length lTypes == length rTypes && and (zipWith checkAssignable (map snd lTypes) rTypes)
checkAssignable (StructT _ lTypes) (StructT _ rTypes) =
    length lTypes == length rTypes &&
        all (\((ln, lt), (rn, rt)) -> ln == rn && checkAssignable lt rt) (zip lTypes rTypes)
checkAssignable _ (MultiReturnT _) = False
checkAssignable _ NothingT = False
checkAssignable _ TypeError = True
checkAssignable TypeError _ = True
checkAssignable lType rType = lType == rType

isPrimitiveType :: Type -> Bool
isPrimitiveType IntT = True
isPrimitiveType FloatT = True
isPrimitiveType CharT = True
isPrimitiveType BoolT = True
isPrimitiveType AtomT = True
isPrimitiveType (AliasT _ t) = isPrimitiveType t
isPrimitiveType _ = False

isRecordOrTupleType :: Type -> Bool
isRecordOrTupleType (TupleT _)  = True
isRecordOrTupleType (AliasT _ t) = isRecordOrTupleType t
isRecordOrTupleType t = isRecordType t

isRecordType :: Type -> Bool
isRecordType (StructT _ _) = True
isRecordType (UnionT _ _)  = True
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
isCasteable source dest = isPrimitiveType source && isPrimitiveType dest

isPrimitiveOrPointerType :: Type -> Bool
isPrimitiveOrPointerType t = isPrimitiveType t || isPointerType t