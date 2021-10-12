module Westeros.SouthOfTheWall.TypeChecking where

import Control.Monad                    (replicateM_)
import Control.Monad.RWS                ( MonadState(get) )
import Data.Foldable                    (foldl')

import Westeros.SouthOfTheWall.Types  ( typeQuery )

import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Types as T
import qualified Westeros.SouthOfTheWall.AST as AST

import Data.List (findIndex)
import Data.Maybe (fromJust)

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
                    ST.StructScope scope         -> return $ StructT scope
                    ST.UnionScope  scope         -> return $ UnionT scope
                    ST.TupleTypes xs             -> do
                        types <- mapM getTypeFromString xs
                        return $ TupleT types

                    _                            -> return TypeError
                Nothing -> return TypeError
            Nothing -> return TypeError

-- This interface will provide type check consistency for their instances.
--
-- The propper way to use it is to implement exhaustive instances for everything in
-- the language susceptible of having a type and then performing type queries when
-- necessary.
class Typeable a where
    typeQuery :: a -> ST.MonadParser Type

instance Typeable Expr where

    -- Simple Types
    typeQuery (IntLit    _) = return T.IntT
    typeQuery (CharLit   _) = return T.CharT
    typeQuery (FloatLit  _) = return T.FloatT
    typeQuery (AtomLit   _) = return T.AtomT
    typeQuery (StringLit _) = return $ T.ArrayT T.CharT 1
    typeQuery TrueLit       = return T.BoolT
    typeQuery FalseLit      = return T.BoolT
    typeQuery NullLit       = return T.NullT

    -- Literal Arrays
    typeQuery (ArrayLit []) = error "Empty literal array not supported"

    typeQuery (ArrayLit array) = do
        types <- mapM (typeQuery . getExpr) array
        let arrType = head types
        if all T.notTypeError types
            then if foldl' (\acc b -> arrType == b && acc ) True types
                then return $ T.ArrayT arrType (length array)
                else do
                    let errorInd  = fromJust $ findIndex (/= arrType) types
                        errorExpr = array !! errorInd
                        errorTk   = getToken errorExpr
                        pos       = Tk.position errorTk
                    ST.insertError $ Err.TE (Err.HeterogeneusArrayType pos)
                    return T.TypeError
        else return T.TypeError

    -- Literal Tuples
    typeQuery (TupleLit parts) = do
        types <- mapM (typeQuery . getExpr) parts
        let res = T.TupleT types
        if T.notTypeError res
            then return res
            else return T.TypeError

    -- Function Calls
    typeQuery (FuncCall symbol args) = do
        types <- mapM (typeQuery . getExpr) args
        entry <- ST.lookupFunction symbol $ length args
        case entry of
            Just function -> do
                case ST.additional function of
                    Just (ST.FunctionMetaData info) -> do
                        let params = ST.parameters info
                        paramTypes <- mapM getTypeFromString params
                        
                        let cond1 = all T.notTypeError paramTypes
                            cond2 = and $ zipWith (==) paramTypes types
                        if cond1 && cond2 then do
                            let functionType = ST.symbolType function
                            case functionType of
                                Just typeName -> getTypeFromString typeName
                                Nothing -> return T.VoidT
                        else return T.TypeError
                    Nothing -> do
                        let err = Err.FunctionWithoutMD symbol
                        ST.insertError $ Err.TE err
                        return T.TypeError
                    _ -> return T.TypeError
            Nothing -> do
                let err = Err.NotAFunction symbol
                ST.insertError $ Err.TE err
                return T.TypeError

    -- Binary Operators
    typeQuery (BinOp op a b)  = binOpCheck op a b

    -- Unary Operators

    typeQuery (UnOp Neg a) = do
        x <- typeQuery (getExpr a)
        let validTypes   = [T.IntT, T.FloatT]
            tkErrPos     = Tk.position $ getToken a
            correctTypes = map show validTypes
        if x `elem` validTypes
            then return x
            else do
                let err = Err.InvalidTypeUnOp (show Neg) (show x) correctTypes tkErrPos
                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (UnOp Deref p) = do
        mustBePtr <- typeQuery (getExpr p)
        case mustBePtr of
            T.PointerT e
                | e == T.TypeError -> return T.TypeError
                | otherwise        -> return e
            _          -> do
                let err = Err.InvalidDereference (show mustBePtr) (Tk.position $ getToken p)
                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (AccesField expr symbol) = do
        accessExpr <- typeQuery $ getExpr expr

        let isRecordType = case accessExpr of
                T.StructT scope -> Just scope
                T.UnionT scope  -> Just scope
                _               -> Nothing

        case isRecordType of
            (Just scope) -> do

                symT <- get

                let inScope         = ST.filterByScopeST symT scope
                    possibleEntries = ST.findDictionary inScope symbol
                    position        = Tk.position $ getToken expr

                case possibleEntries of

                    Just [entry] -> case ST.symbolType entry of

                        Just strType -> T.getTypeFromString strType
                        Nothing      -> do
                            let err = Err.UnTypedRecordField symbol position
                            ST.insertError $ Err.TE err
                            return T.TypeError

                    Nothing       -> do
                        let err = Err.RecordFieldNotFound symbol scope position
                        ST.insertError $ Err.TE err
                        return T.TypeError
                    _             -> do
                        let err = Err.RepeatedRecordField symbol scope position
                        ST.insertError $ Err.TE err
                        return T.TypeError
            Nothing -> do
                let name = Tk.cleanedString $ getToken expr
                    pos  = Tk.position $ getToken expr
                    err = Err.NotARecordType name pos
                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (ActiveField expr symbol) = do
        unionExpr <- typeQuery $ getExpr expr

        let position = Tk.position $ getToken expr

        case unionExpr of
            (T.UnionT scope) -> do
                symT <- get

                let inScope         = ST.filterByScopeST symT scope
                    possibleEntries = ST.findDictionary inScope symbol

                case possibleEntries of

                    Just [entry] -> case ST.symbolType entry of

                        Just _       -> return T.BoolT
                        Nothing      -> do
                            let err = Err.UnTypedRecordField symbol position
                            ST.insertError $ Err.TE err
                            return T.TypeError

                    Nothing      -> do
                        let err = Err.RecordFieldNotFound symbol scope position
                        ST.insertError $ Err.TE err
                        return T.TypeError

                    _            -> do
                        let err = Err.RepeatedRecordField symbol scope position
                        ST.insertError $ Err.TE err
                        return T.TypeError

            _              -> do
                let name = Tk.cleanedString $ getToken expr
                    pos  = Tk.position $ getToken expr
                    err = Err.NotAnUnion name pos
                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (AccesIndex arr inds)  = do

        arrType  <- typeQuery (getExpr arr)
        indTypes <- mapM (typeQuery . getExpr) inds
        let cond1 = T.notTypeError arrType
            cond2 = all T.notTypeError indTypes
            cond3 = foldl' (\acc b -> T.IntT == b && acc ) True indTypes
        if cond1 && cond2
            then if cond3
                then case arrType of
                    T.ArrayT tp dim -> do
                        if length inds == dim
                            then return tp
                            else do
                                let pos = Tk.position $ getToken arr
                                ST.insertError $ Err.TE $ Err.DimMissmatch (show dim) (show $ length inds) pos
                                return T.TypeError
                    _ -> do
                            let errorTp = show arrType
                                pos = Tk.position $ getToken arr
                            ST.insertError $ Err.TE $ Err.InvalidIndexedType errorTp pos
                            return T.TypeError
                else do
                    let errorInd      = fromJust $ findIndex (/=T.IntT) indTypes
                        errorTp       = show $ indTypes !! errorInd
                        errorExprTk   = getToken (inds !! errorInd)
                        exprString    = Tk.cleanedString errorExprTk
                        pos           = Tk.position errorExprTk
                        err         = Err.InvalidIndexType errorTp exprString pos
                    ST.insertError $ Err.TE err
                    return T.TypeError
            else return T.TypeError

    -- check if ind is in the range
    typeQuery (TupleIndex tupleExpr ind) = do
        tupleType <- typeQuery (getExpr tupleExpr)
        if T.notTypeError tupleType then
            case tupleType of
                T.TupleT xs -> return $ xs !! ind
                _           -> do
                    let invalidType = Tk.cleanedString $ getToken tupleExpr
                        pos         = Tk.position $ getToken tupleExpr
                        err       = Err.NotATupleType invalidType pos
                    ST.insertError $ Err.TE err
                    return T.TypeError
            else return T.TypeError

    typeQuery (Cast expr symbol) = do
        destType <- T.getTypeFromString symbol
        exprType <- typeQuery $ getExpr expr

        if isCasteable exprType destType
            then return destType
            else do
                let position = Tk.position $ getToken expr
                    err    = Err.NonCasteableTypes (show exprType) (show destType) position

                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (IdExpr symbol) = do

        symbolType <- ST.lookupST symbol
        case symbolType of
            Just entry -> case ST.symbolType entry of
                Just tp -> T.getTypeFromString tp
                Nothing -> do
                    let err = Err.UnTypedId symbol
                    ST.insertError $ Err.TE err
                    return T.TypeError
            Nothing -> do
                let err = Err.IdNotFound symbol
                ST.insertError $ Err.TE err
                return T.TypeError

binOpCheck :: BinOp -> Expression -> Expression -> ST.MonadParser T.Type
binOpCheck bop a b = do
    x <- typeQuery (getExpr a)
    d <- typeQuery (getExpr b)

    let (validTypes, returnType)
            | bop `elem` [Sum,Sub,Prod,Mod,Div] = ([T.IntT, T.FloatT], x)
            | bop `elem` [Eq,Neq,Lt,Gt,Leq,Geq] = ([T.IntT, T.BoolT, T.FloatT, T.CharT], T.BoolT)
            | otherwise                         = ([T.BoolT], T.BoolT)

        tkErrPos = Tk.position $ getToken a
        lType    = show x
        rType    = show d
        correctTypes = map show validTypes

    if x == d
        then if x `elem` validTypes
                then return returnType
                else do
                    let err = Err.InvalidTypesBinOp (show bop) (lType,rType) correctTypes tkErrPos
                    ST.insertError $ Err.TE err
                    return T.TypeError
        else do
            let err = Err.InconsistentTypesBinOp (show bop) (lType, rType) correctTypes tkErrPos
            ST.insertError $ Err.TE err
            return T.TypeError


buildAndCheckExpr :: Tk.Token -> Expr -> ST.MonadParser Expression
buildAndCheckExpr tk expr = do
    exprType <- typeQuery expr

    return $ Expression {
           getToken = tk,
           getExpr  = expr,
           getType  = exprType
        }


checkPrimitiveType :: Expression -> ST.MonadParser ()
checkPrimitiveType expr = do
    if T.isPrimitiveType $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr )

checkCompositeType :: Expression -> ST.MonadParser ()
checkCompositeType expr = do
    if T.isCompositeType $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr )

checkRecordOrTupleType :: Expression -> ST.MonadParser ()
checkRecordOrTupleType expr = do
    if T.isRecordOrTupleType $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr)

checkArrayType :: Expression -> ST.MonadParser ()
checkArrayType expr = do
    if T.isArrayType $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr)

checkPointerType :: Expression -> ST.MonadParser ()
checkPointerType expr = do
    if T.isPointerType $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr)

checkIntegerTypes :: [Expression] -> ST.MonadParser ()
checkIntegerTypes = mapM_ checkIntegerType

checkIntegerType :: Expression -> ST.MonadParser ()
checkIntegerType expr = do
    if T.isIntegerType $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr)

checkPointerToArrayType :: Expression -> ST.MonadParser ()
checkPointerToArrayType expr = do
    if T.isPointerToArray $ getType expr
        then return()
        else ST.insertError $ Err.TE (Err.UnexpectedType (show $ getType expr) $ Tk.position $ getToken expr)

