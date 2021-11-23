module Westeros.SouthOfTheWall.TypeChecking where
    
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

-- This interface will provide type check consistency for their instances.
--
-- The propper way to use it is to implement exhaustive instances for everything in
-- the language susceptible of having a type and then performing type queries when
-- necessary.
class Typeable a where
    typeQuery :: a -> ST.MonadParser T.Type

instance Typeable AST.Expr where

    -- Simple Types
    typeQuery (AST.IntLit    _) = return T.IntT
    typeQuery (AST.CharLit   _) = return T.CharT
    typeQuery (AST.FloatLit  _) = return T.FloatT
    typeQuery (AST.AtomLit   _) = return T.AtomT
    typeQuery (AST.StringLit _) = return $ T.ArrayT T.CharT 1
    typeQuery AST.TrueLit       = return T.BoolT
    typeQuery AST.FalseLit      = return T.BoolT
    typeQuery AST.NullLit       = return T.NullT                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

    -- Literal Arrays
    typeQuery (AST.ArrayLit array) = do
        let types = map AST.getType array
            arrType = head types
        if all T.notTypeError types
            then case findIndex (/= arrType) types of
                Nothing -> case arrType of
                    T.ArrayT tp dim -> return $ T.ArrayT tp (dim+1)
                    _ -> return $ T.ArrayT arrType 1
                Just errorInd -> do
                    let errorExpr = array !! errorInd
                        errorTk   = AST.getToken errorExpr
                        pos       = Tk.position errorTk
                    ST.insertError $ Err.TE (Err.HeterogeneusArrayType pos)
                    return T.TypeError
            else return T.TypeError

    -- Literal Tuples
    typeQuery (AST.TupleLit parts) = do
        let types = map AST.getType parts
            res = T.TupleT types
        if T.notTypeError res
            then return res
            else return T.TypeError

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
                                params <- T.buildTypesFromDict $ map (second head) $ M.toList params
                                let paramTypes = map snd params
                                if all T.notTypeError paramTypes && all T.notTypeError returnTypes 
                                    then case findIndex not $ zipWith T.checkAssignable paramTypes types of
                                        Nothing -> 
                                            case returnTypes of
                                                [] -> return T.NothingT
                                                [t] -> return t
                                                _ -> return $ T.MultiReturnT returnTypes
                                        Just idx -> do
                                            let errorExpr = args !! idx
                                                errorTk   = AST.getToken errorExpr
                                                errorTp   = types !! idx
                                                err = Err.UnexpectedType (show errorTp) (show $ paramTypes !! idx) (Tk.position errorTk)
                                            ST.insertError $ Err.TE err
                                            return T.TypeError
                                    else return T.TypeError
                            else
                                return T.TypeError
                    Nothing -> return T.TypeError
            else return T.TypeError

    -- Binary Operators
    typeQuery (AST.BinOp op a b)  = binOpCheck op a b

    -- Unary Operators
    typeQuery (AST.UnOp AST.Neg a) = do
        let validTypes = [T.IntT, T.FloatT]
        case AST.getType a of
            x | x `elem` validTypes -> return x
            T.TypeError -> return T.TypeError
            x -> do
                let tkErrPos = Tk.position $ AST.getToken a
                    err = Err.UnexpectedType (show x) (show Err.Bool) tkErrPos
                ST.insertError $ Err.TE err
                return T.TypeError
    
    typeQuery (AST.UnOp AST.Not a) = do
        case AST.getType a of
            T.BoolT -> return T.BoolT
            T.TypeError -> return T.TypeError
            x -> do
                let tkErrPos = Tk.position $ AST.getToken a
                let err = Err.UnexpectedType (show x) (show Err.Bool) tkErrPos
                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (AST.UnOp AST.Deref p) = do
        case AST.getType p of
            T.PointerT e
                | e == T.TypeError -> return T.TypeError
                | otherwise        -> return e
            T.TypeError -> return T.TypeError
            x          -> do
                let err = Err.UnexpectedType (show x) (show Err.Pointer) (Tk.position $ AST.getToken p)
                ST.insertError $ Err.TE err
                return T.TypeError

    typeQuery (AST.AccesField expr symbol) = do

        case AST.getType expr of
            T.StructT fields -> do 
                case find (\p -> fst p == symbol) fields of
                    Just (_, t) -> return t
                    Nothing -> do
                        let err = Err.InvalidField symbol (Tk.position $ AST.getToken expr)
                        ST.insertError $ Err.TE err
                        return T.TypeError
            T.UnionT fields -> do 
                case find (\p -> fst p == symbol) fields of
                    Just (_, t) -> return t
                    Nothing -> do
                        let err = Err.InvalidField symbol (Tk.position $ AST.getToken expr)
                        ST.insertError $ Err.TE err
                        return T.TypeError
            _ -> return T.TypeError

    typeQuery (AST.ActiveField expr symbol) = do

        case AST.getType expr of
            T.UnionT fields -> do
                case find (\(s,_) -> s == symbol) fields of
                    Just _ -> return T.BoolT
                    Nothing -> do
                        let err = Err.InvalidField symbol (Tk.position $ AST.getToken expr)
                        ST.insertError $ Err.TE err
                        return T.TypeError
            _ -> return T.TypeError

    typeQuery (AST.AccesIndex arr inds)  = do

        let arrType  = AST.getType arr
            indTypes = map AST.getType inds
        if T.notTypeError arrType && all T.notTypeError indTypes
            then case arrType of
                T.ArrayT tp dim -> do
                    case findIndex (/= T.IntT) indTypes of
                        Nothing -> do
                            if length inds == dim
                                then return tp
                                else if length inds < dim
                                    then return $ T.ArrayT tp (dim - length inds)
                                    else do
                                        let pos = Tk.position $ AST.getToken arr
                                        ST.insertError $ Err.PE $ Err.IndexOutOfBounds dim (length inds) pos
                                        return T.TypeError
                        Just idx -> do
                            let errorTp       = show $ indTypes !! idx
                                pos           = Tk.position $ AST.getToken $ inds !! idx
                                err         = Err.UnexpectedType errorTp (show Err.Int) pos
                            ST.insertError $ Err.TE err
                            return T.TypeError      
                _ -> do
                    let errorTp = show arrType
                        pos = Tk.position $ AST.getToken arr
                    ST.insertError $ Err.TE $ Err.UnexpectedType errorTp (show Err.Array) pos
                    return T.TypeError
            else return T.TypeError
        
    
    typeQuery (AST.TupleIndex tupleExpr ind) = do
        
        let tupleType = AST.getType tupleExpr
        if T.notTypeError tupleType then
            case tupleType of
                T.TupleT xs -> do
                    if ind < length xs
                        then return $ xs !! ind
                        else do
                            let pos = Tk.position $ AST.getToken tupleExpr
                            ST.insertError $ Err.PE $ Err.IndexOutOfBounds ind (length xs) pos
                            return T.TypeError
                _       -> do
                    let invalidType = Tk.cleanedString $ AST.getToken tupleExpr
                        pos         = Tk.position $ AST.getToken tupleExpr
                        err       = Err.InvalidExprType invalidType pos
                    ST.insertError $ Err.TE err
                    return T.TypeError
            else return T.TypeError

    typeQuery (AST.Cast expr symbol) = do

        destType <- T.getTypeFromString symbol
        let exprType = AST.getType expr
        if T.notTypeError destType && T.notTypeError exprType
            then if T.isCasteable exprType destType
                then return destType
                else do
                    let position = Tk.position $ AST.getToken expr
                        err = Err.NonCasteableTypes (show exprType) (show destType) position
                    ST.insertError $ Err.TE err
                    return T.TypeError
            else return T.TypeError

    typeQuery (AST.IdExpr symbol) = do

        maybeEntry <- ST.lookupST symbol
        case maybeEntry of
            Just ST.SymbolInfo{ST.category=category, ST.symbolType=tp} -> do
                if category `elem` [ST.Variable, ST.Constant, ST.Parameter]
                    then T.getTypeFromString $ fromJust tp
                    else do
                        return T.TypeError
            Nothing -> do
                return T.TypeError

binOpCheck :: AST.BinOp -> AST.Expression -> AST.Expression -> ST.MonadParser T.Type
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
                then return returnType
                else do
                    let err = Err.IncompatibleTypes (show firstType) (show secondType) (Tk.position $ AST.getToken b)
                    ST.insertError $ Err.TE err
                    return T.TypeError
            else do
                let err = Err.InvalidExprType (show secondType) (Tk.position $ AST.getToken b)
                ST.insertError $ Err.TE err
                return T.TypeError
        else do
            let err = Err.InvalidExprType (show firstType) (Tk.position $ AST.getToken a)
            ST.insertError $ Err.TE err
            
            unless (secondType `elem` validTypes) $ do
                let er = Err.InvalidExprType (show secondType) (Tk.position $ AST.getToken b)
                ST.insertError $ Err.TE er
            return T.TypeError
    where 
        numericalOp = [AST.Sum, AST.Sub, AST.Prod, AST.Div]
        numericalTypes = [T.IntT, T.FloatT]
        relationalOp = [AST.Eq, AST.Neq, AST.Lt, AST.Gt, AST.Leq, AST.Geq]
        primitiveTypes = [T.IntT, T.BoolT, T.FloatT, T.CharT, T.AtomT]


buildAndCheckExpr :: Tk.Token -> AST.Expr -> ST.MonadParser AST.Expression
buildAndCheckExpr tk expr = do
    exprType <- typeQuery expr

    return $ AST.Expression {
           AST.getToken = tk,
           AST.getExpr  = expr,
           AST.getType  = exprType
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