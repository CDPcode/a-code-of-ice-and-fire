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

    | StringT
    | ArrayT Type Int
    | TupleT [Type]
    | StructT Int
    | UnionT Int
    | PointerT Type

    | NullT
    | VoidT

    | TypeError
    deriving (Eq, Show)

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
    "_string"  -> return StringT

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