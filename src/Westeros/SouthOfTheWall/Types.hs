module Westeros.SouthOfTheWall.TypeVer where

import qualified Westeros.SouthOfTheWall.Symtable as ST

import Control.Monad.RWS

data Type
    = IntT
    | FloatT
    | CharT
    | BoolT
    | AtomT
    | AliasT String Type
    | ArrayT Type Int
    | TupleT [Type]
    | StructT Int
    | UnionT Int
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

-- Use to check if recursive types have some leave with a type error.
notTypeError :: Type -> Bool
notTypeError (TupleT xs)  = all notTypeError xs
notTypeError (PointerT e) = notTypeError e
notTypeError (ArrayT tp _) = notTypeError tp
notTypeError TypeError    = False
notTypeError _            = True

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
isPrimitiveType (AliasT _ t) = isPrimitiveType t
isPrimitiveType _ = False

isRecordOrTupleType :: Type -> Bool
isRecordOrTupleType (StructT _) = True
isRecordOrTupleType (UnionT _)  = True
isRecordOrTupleType (TupleT _)  = True
isRecordOrTupleType (AliasT _ t) = isRecordOrTupleType t
isRecordOrTupleType _ = False

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

isCompositeType :: Type -> Bool
isCompositeType t = isRecordOrTupleType t || isArrayType t

isCasteable :: Type -> Type -> Bool
isCasteable source dest = source `elem` simpleType && dest `elem` simpleType
    where simpleType = [IntT, BoolT, CharT, FloatT]