module Westeros.SouthOfTheWall.TypeChecking (
      Typeable (..)
    , binOpCheck
    , buildAndCheckExpr
    , checkPrimitiveType
    , checkCompositeType
    , checkRecordOrTupleType
    , checkArrayType
    , checkPointerType
    , checkPointerToArrayType
    , checkPointerToRecordOrTupleType
    , checkIntegerType
    , checkIntegerTypes
    ) where

import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Types as T
import qualified Westeros.SouthOfTheWall.AST as AST

import Data.List (findIndex, find)
import Data.Maybe (fromJust)
import Control.Monad.RWS (unless, get)
import Data.Bifunctor (second)
import qualified Data.Map.Strict as M
import qualified Westeros.SouthOfTheWall.AST as T

-- This interface will provide type check consistency for their instances.
--
-- The propper way to use it is to implement exhaustive instances for everything in
-- the language susceptible of having a type and then performing type queries when
-- necessary.
class Typeable a where
    typeQuery :: a -> ST.MonadParser (T.Type, String)

instance Typeable AST.Expr where

    -- Simple Types
    typeQuery (AST.IntLit    _) = return (T.IntT, ST.int)
    typeQuery (AST.CharLit   _) = return (T.CharT, ST.char)
    typeQuery (AST.FloatLit  _) = return (T.FloatT, ST.float)
    typeQuery (AST.AtomLit   _) = return (T.AtomT, ST.atom)
    typeQuery (AST.StringLit _) = return (T.ArrayT T.CharT 1, ST.string)
    typeQuery AST.TrueLit       = return (T.BoolT, ST.bool)
    typeQuery AST.FalseLit      = return (T.BoolT, ST.bool)
    typeQuery AST.NullLit       = return (T.NullT, ST.nullptr)

    -- Literal Arrays
    typeQuery (AST.ArrayLit array) = do
        let types = map AST.getType array
            arrType = head types
        if all T.notTypeError types
            then case findIndex (/= arrType) types of
                Nothing -> case arrType of
                    T.ArrayT tp dim -> return (T.ArrayT tp (dim+1), undefined)
                    _ -> return (T.ArrayT arrType 1, undefined)
                Just errorInd -> do
                    let errorExpr = array !! errorInd
                        errorTk   = AST.getToken errorExpr
                        pos       = Tk.position errorTk
                    ST.insertError $ Err.TE (Err.HeterogeneusArrayType pos)
                    return (T.TypeError, ST.tError)
            else return (T.TypeError, ST.tError)

    -- Literal Tuples
    typeQuery (AST.TupleLit parts) = do
        let types = map AST.getType parts
            res = T.TupleT types
        if T.notTypeError res
            then return (res, undefined)
            else return (T.TypeError, ST.tError)

    -- Function Calls
    typeQuery (AST.FuncCall symbol args) = do
        let types = map AST.getType args
        if all T.notTypeError types
            then do
                entry <- ST.lookupFunction symbol $ length args
                case entry of
                    Just ST.SymbolInfo {
                        ST.additional=(Just (
                            ST.FunctionMetaData ST.FunctionInfo{
                                ST.fScope=scope,
                                ST.returns=returns,
                                ST.defined=defined
                            })
                        )
                    } -> do if defined
                            then do
                                symT <- get
                                returnTypes <- mapM T.getTypeFromString returns
                                let params = ST.filterByScopeST symT scope
                                params' <- T.buildTypesFromDict $ map (second head) $ M.toList params
                                let paramTypes = map snd params'
                                if all T.notTypeError paramTypes && all T.notTypeError returnTypes
                                    then case findIndex not $ zipWith T.checkAssignable paramTypes types of
                                        Nothing -> do
                                            valid <- checkValidLValueArgs (map (second head) $ M.toList params) args
                                            if valid
                                                then
                                                    case returnTypes of
                                                        [] -> return (T.NothingT, "")
                                                        [t] -> return (t, head returns)
                                                        _ -> return (T.MultiReturnT returnTypes, "")
                                                else
                                                    return (T.TypeError, ST.tError)
                                        Just idx -> do
                                            let errorExpr = args !! idx
                                                errorTk   = AST.getToken errorExpr
                                                errorTp   = types !! idx
                                                err = Err.UnexpectedType (show errorTp) (show $ paramTypes !! idx) (Tk.position errorTk)
                                            ST.insertError $ Err.TE err
                                            return (T.TypeError, ST.tError)
                                    else return (T.TypeError, ST.tError)
                            else
                                return (T.TypeError, ST.tError)
                    _ -> return (T.TypeError, ST.tError)
            else return (T.TypeError, ST.tError)

    -- Binary Operators
    typeQuery (AST.BinOp op a b)  = binOpCheck op a b

    -- Unary Operators
    typeQuery (AST.UnOp AST.Neg a) = do
        let validTypes = [T.IntT, T.FloatT]
        case AST.getType a of
            x | x `elem` validTypes -> return (x, AST.getTypeStr a)
            T.TypeError -> return (T.TypeError, ST.tError)
            x -> do
                let tkErrPos = Tk.position $ AST.getToken a
                    err = Err.UnexpectedType (show x) (show Err.Bool) tkErrPos
                ST.insertError $ Err.TE err
                return (T.TypeError, ST.tError)

    typeQuery (AST.UnOp AST.Not a) = do
        case AST.getType a of
            T.BoolT -> return (T.BoolT, ST.bool)
            T.TypeError -> return (T.TypeError, ST.tError)
            x -> do
                let tkErrPos = Tk.position $ AST.getToken a
                let err = Err.UnexpectedType (show x) (show Err.Bool) tkErrPos
                ST.insertError $ Err.TE err
                return (T.TypeError, ST.tError)

    typeQuery (AST.UnOp AST.Deref p) = do
        case AST.getType p of
            T.PointerT e
                | e == T.TypeError -> return (T.TypeError, ST.tError)
                | otherwise        -> do
                    let typeStr = AST.getTypeStr p
                    pointedTypeStr <- T.getPointedTypeString typeStr
                    case pointedTypeStr of
                        Just tp -> return (e, tp)
                        Nothing -> error "Pointer type got but no string pointed type :c"
            T.TypeError -> return (T.TypeError, ST.tError)
            x          -> do
                let err = Err.UnexpectedType (show x) (show Err.Pointer) (Tk.position $ AST.getToken p)
                ST.insertError $ Err.TE err
                return (T.TypeError, ST.tError)

    typeQuery (AST.AccesField expr symbol) = do

        case AST.getType expr of
            T.StructT sc fields -> do
                case find (\p -> fst p == symbol) fields of
                    Just (_, t) -> do
                        mInfo <- ST.lookupInScopeST symbol sc
                        case mInfo of
                            Just ST.SymbolInfo { ST.symbolType = Just typeStr } -> return (t, typeStr)
                            _  -> error "id not found in struct that should have it"
                    Nothing -> do
                        let err = Err.InvalidField symbol (Tk.position $ AST.getToken expr)
                        ST.insertError $ Err.TE err
                        return (T.TypeError, ST.tError)
            T.UnionT sc fields -> do
                case find (\p -> fst p == symbol) fields of
                    Just (_, t) -> do
                        mInfo <- ST.lookupInScopeST symbol sc
                        case mInfo of
                            Just ST.SymbolInfo { ST.symbolType = Just typeStr } -> return (t, typeStr)
                            _  -> error "id not found in struct that should have it"
                    Nothing -> do
                        let err = Err.InvalidField symbol (Tk.position $ AST.getToken expr)
                        ST.insertError $ Err.TE err
                        return (T.TypeError, ST.tError)
            _ -> return (T.TypeError, ST.tError)

    typeQuery (AST.ActiveField expr symbol) = do

        case AST.getType expr of
            T.UnionT _ fields -> do
                case find (\(s,_) -> s == symbol) fields of
                    Just _ -> return (T.BoolT, ST.bool)
                    Nothing -> do
                        let err = Err.InvalidField symbol (Tk.position $ AST.getToken expr)
                        ST.insertError $ Err.TE err
                        return (T.TypeError, ST.tError)
            _ -> return (T.TypeError, ST.tError)

    typeQuery (AST.AccesIndex arr inds)  = do

        let arrType    = AST.getType arr
            arrTypeStr = AST.getTypeStr arr
            indTypes   = map AST.getType inds
        if T.notTypeError arrType && all T.notTypeError indTypes
            then case arrType of
                T.ArrayT tp dim -> do
                    case findIndex (/= T.IntT) indTypes of
                        Nothing -> do
                            if length inds == dim
                                then do
                                    mTypeStr <- T.getContainedTypeString arrTypeStr
                                    case mTypeStr of
                                        Just typeStr -> return (tp, typeStr)
                                        Nothing -> error "Array does not contain type string in symbol table"
                                else do
                                    let pos = Tk.position $ AST.getToken arr
                                    ST.insertError $ Err.PE $ Err.IndexOutOfBounds dim (length inds) pos
                                    return (T.TypeError, ST.tError)
                        Just idx -> do
                            let errorTp       = show $ indTypes !! idx
                                pos           = Tk.position $ AST.getToken $ inds !! idx
                                err         = Err.UnexpectedType errorTp (show Err.Int) pos
                            ST.insertError $ Err.TE err
                            return (T.TypeError, ST.tError)
                _ -> do
                    let errorTp = show arrType
                        pos = Tk.position $ AST.getToken arr
                    ST.insertError $ Err.TE $ Err.UnexpectedType errorTp (show Err.Array) pos
                    return (T.TypeError, ST.tError)
            else return (T.TypeError, ST.tError)


    typeQuery (AST.TupleIndex tupleExpr ind) = do

        let tupleType    = AST.getType tupleExpr
            tupleTypeStr = AST.getTypeStr tupleExpr
        if T.notTypeError tupleType then
            case tupleType of
                T.TupleT xs -> do
                    if ind < length xs
                        then do
                            mTypeStr <- T.getTupleContainedTypeString tupleTypeStr ind
                            case mTypeStr of
                                Just typeStr -> return (xs !! ind, typeStr)
                                Nothing -> error "Tuple does not contain type string in symbol table"
                        else do
                            let pos = Tk.position $ AST.getToken tupleExpr
                            ST.insertError $ Err.PE $ Err.IndexOutOfBounds ind (length xs) pos
                            return (T.TypeError, ST.tError)
                _       -> do
                    let invalidType = Tk.cleanedString $ AST.getToken tupleExpr
                        pos         = Tk.position $ AST.getToken tupleExpr
                        err       = Err.InvalidExprType invalidType pos
                    ST.insertError $ Err.TE err
                    return (T.TypeError, ST.tError)
            else return (T.TypeError, ST.tError)

    typeQuery (AST.Cast expr symbol) = do

        destType <- T.getTypeFromString symbol
        let exprType = AST.getType expr
        if T.notTypeError destType && T.notTypeError exprType
            then if T.isCasteable exprType destType
                then return (destType, symbol)
                else do
                    let position = Tk.position $ AST.getToken expr
                        err = Err.NonCasteableTypes (show exprType) (show destType) position
                    ST.insertError $ Err.TE err
                    return (T.TypeError, ST.tError)
            else return (T.TypeError, ST.tError)

    typeQuery (AST.IdExpr symbol) = do

        maybeEntry <- ST.lookupST symbol
        case maybeEntry of
            Just ST.SymbolInfo{ST.category=category, ST.symbolType=tp} -> do
                if category `elem` [ST.Variable, ST.Constant, ST.Parameter]
                    then do
                        t <- T.getTypeFromString $ fromJust tp
                        return (t, fromJust tp)
                    else return (T.TypeError, ST.tError)
            Nothing -> return (T.TypeError, ST.tError)

binOpCheck :: AST.BinOp -> AST.Expression -> AST.Expression -> ST.MonadParser (T.Type, String)
binOpCheck bop a b = do

    let firstType = AST.getType a
        secondType = AST.getType b
        (validTypes, returnType)
            | bop `elem` numericalOp = (numericalTypes, firstType)
            | bop `elem` relationalOp = (primitiveTypes, T.BoolT)
            | bop == AST.Mod = ([T.IntT], T.IntT)
            | otherwise = ([T.BoolT], T.BoolT)

    if firstType `elem` validTypes
        then if secondType `elem` validTypes
            then if firstType == secondType
                then case returnType of
                    T.IntT   -> return (returnType, ST.int)
                    T.FloatT -> return (returnType, ST.int)
                    T.BoolT  -> return (returnType, ST.int)
                    _        -> error "Binary operator returning something other than int, float or bool"
                else do
                    let err = Err.IncompatibleTypes (show firstType) (show secondType) (Tk.position $ AST.getToken b)
                    ST.insertError $ Err.TE err
                    return (T.TypeError, ST.tError)
            else do
                let err = Err.InvalidExprType (show secondType) (Tk.position $ AST.getToken b)
                ST.insertError $ Err.TE err
                return (T.TypeError, ST.tError)
        else do
            let err = Err.InvalidExprType (show firstType) (Tk.position $ AST.getToken a)
            ST.insertError $ Err.TE err

            unless (secondType `elem` validTypes) $ do
                let er = Err.InvalidExprType (show secondType) (Tk.position $ AST.getToken b)
                ST.insertError $ Err.TE er
            return (T.TypeError, ST.tError)
    where
        numericalOp = [AST.Sum, AST.Sub, AST.Prod, AST.Div]
        numericalTypes = [T.IntT, T.FloatT]
        relationalOp = [AST.Eq, AST.Neq, AST.Lt, AST.Gt, AST.Leq, AST.Geq]
        primitiveTypes = [T.IntT, T.BoolT, T.FloatT, T.CharT, T.AtomT]

checkValidLValueArgs :: [(String, ST.SymbolInfo)] -> [AST.Expression] -> ST.MonadParser  Bool
checkValidLValueArgs [] _ = return True
checkValidLValueArgs _ [] = return True
checkValidLValueArgs ((_, info):xs) (expr : exprs)= do
    rest <- checkValidLValueArgs xs exprs
    case ST.additional info of
        Just (ST.ParameterType ST.Reference) -> do
            return $ rest && T.isValidLValue expr
        _ -> return rest

buildAndCheckExpr :: Tk.Token -> AST.Expr -> ST.MonadParser AST.Expression
buildAndCheckExpr tk expr = do
    (exprType, typeStr) <- typeQuery expr

    return $ AST.Expression
        {  AST.getToken = tk
        ,  AST.getExpr  = expr
        ,  AST.getType  = exprType
        ,  AST.getTypeStr = typeStr
        }

checkPrimitiveType :: AST.Expression -> ST.MonadParser ()
checkPrimitiveType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isPrimitiveType x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.InvalidExprType (show tp) (Tk.position $ AST.getToken expr)

checkCompositeType :: AST.Expression -> ST.MonadParser ()
checkCompositeType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isCompositeType x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.InvalidExprType (show tp) (Tk.position $ AST.getToken expr)

checkRecordOrTupleType :: AST.Expression -> ST.MonadParser ()
checkRecordOrTupleType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isRecordOrTupleType x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.InvalidExprType (show tp) (Tk.position $ AST.getToken expr)

checkArrayType :: AST.Expression -> ST.MonadParser ()
checkArrayType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isArrayType x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.UnexpectedType (show tp) (show Err.Array) (Tk.position $ AST.getToken expr)

checkPointerType :: AST.Expression -> ST.MonadParser ()
checkPointerType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isPointerType x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.UnexpectedType (show tp) (show Err.Pointer) (Tk.position $ AST.getToken expr)

checkIntegerTypes :: [AST.Expression] -> ST.MonadParser ()
checkIntegerTypes = mapM_ checkIntegerType

checkIntegerType :: AST.Expression -> ST.MonadParser ()
checkIntegerType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isIntegerType x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.UnexpectedType (show tp) (show Err.Int) (Tk.position $ AST.getToken expr)

checkPointerToArrayType :: AST.Expression -> ST.MonadParser ()
checkPointerToArrayType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isPointerToArray x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.InvalidExprType (show tp) (Tk.position $ AST.getToken expr)

checkPointerToRecordOrTupleType :: AST.Expression -> ST.MonadParser ()
checkPointerToRecordOrTupleType expr = do
    let tp = AST.getType expr
    case tp of
        T.TypeError -> return()
        x | T.isPointerToRecordOrTuple x -> return ()
        _ -> ST.insertError $ Err.TE $ Err.InvalidExprType (show tp) (Tk.position $ AST.getToken expr)