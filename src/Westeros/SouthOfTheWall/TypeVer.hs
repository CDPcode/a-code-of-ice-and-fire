module Westeros.SouthOfTheWall.TypeVer where

import qualified Westeros.SouthOfTheWall.Symtable as ST

import Control.Monad.RWS

data Type
    = IntT
    | FloatT
    | CharT
    | BoolT
    | AtomT

    | AliasT String

    | ArrayT Type Int
    | TupleT [Type]
    | StructT Int
    | UnionT Int
    | PointerT Type

    | NullT
    | VoidT

    | TypeError
    deriving (Eq, Show)

checkAssignable :: Type -> Type -> Bool
checkAssignable PointerT{} NullT = True
checkAssignable (PointerT lType) (PointerT rType) = checkAssignable lType rType
checkAssignable (TupleT lTypes) (TupleT rTypes) = and $ zipWith checkAssignable lTypes rTypes
checkAssignable VoidT _ = False
checkAssignable _ VoidT = False
checkAssignable lType rType = lType == rType

isPrimitiveType :: Type -> Bool
isPrimitiveType IntT = True
isPrimitiveType FloatT = True
isPrimitiveType CharT = True 
isPrimitiveType BoolT = True 
isPrimitiveType AtomT = True
isPrimitiveType _ = False

isRecordOrTupleType :: Type -> Bool 
isRecordOrTupleType (StructT _) = True
isRecordOrTupleType (UnionT _)  = True
isRecordOrTupleType (TupleT _)  = True
isRecordOrTupleType _ = False 

isArrayType :: Type -> Bool 
isArrayType (ArrayT _ _) = True 
isArrayType _ = False 

isPointerType :: Type -> Bool
isPointerType (PointerT _) = True
isPointerType _ = False 

isIntegerType :: Type -> Bool 
isIntegerType IntT = True 
isIntegerType _ = False

isPointerToArray :: Type -> Bool 
isPointerToArray (PointerT t) = isArrayType t
isPointerToArray _ = False 

-- This interface will provide type check consistency for their instances.
--
-- The propper way to use it is to implement exhaustive instances for everything in
-- the language susceptible of having a type and then performing type queries when
-- necessary.
class Typeable a where
    typeQuery :: a -> ST.MonadParser Type


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
                    ST.AliasMetaData _ aliasName -> return $ AliasT aliasName
                    ST.DopeVector tp scope       -> do
                        arrType <- getTypeFromString tp
                        return $ ArrayT arrType scope
                    ST.PointedType tp            -> do
                        ptrType <- getTypeFromString tp
                        return $ PointerT ptrType
                    ST.StructScope scope         -> return $ StructT scope
                    ST.UnionScope  scope         -> return $ UnionT scope
                    ST.TupleTypes xs             -> do
                        types <- mapM getTypeFromString xs
                        return $ TupleT types

                    _                            -> return TypeError

                Nothing -> return TypeError

            Nothing -> return TypeError


-- ^ Use to check if recursive types have some leave with a type error.
notTypeError :: Type -> Bool
notTypeError (TupleT xs)  = all notTypeError xs
notTypeError (PointerT e) = notTypeError e
notTypeError TypeError    = False
notTypeError _            = True