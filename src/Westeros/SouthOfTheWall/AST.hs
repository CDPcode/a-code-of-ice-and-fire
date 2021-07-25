module Westeros.SouthOfTheWall.AST where

import Control.Monad                    (replicateM_)
import Control.Monad.RWS                ( MonadState(put, get) )
import Data.Foldable                    (foldl')
import Data.Maybe                       (fromJust)

import Westeros.SouthOfTheWall.TypeVer  ( typeQuery )

import qualified Westeros.SouthOfTheWall.Error as Err 
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.TypeVer as T

import Data.List (elemIndex, findIndex)

-- AST
data Program = Program Global FunctionDeclarations Main deriving (Show, Eq)

type Id = String
type Main = [Instruction]
type Global = [Instruction]
type FunctionDeclarations = [FunctionDeclaration]
type FunctionDeclaration = [Instruction]

data Expression = Expression
       { getExpr :: Expr
       , getType :: T.Type
       , getToken :: Tk.Token
       } deriving (Show, Eq)

data Expr
    = IntLit    Int
    | CharLit   Char
    | FloatLit  Float
    | StringLit String
    | AtomLit   String
    | TrueLit
    | FalseLit
    | NullLit
    | ArrayLit    [Expression]
    | TupleLit    [Expression]
    | FuncCall    Id [Expression]
    | BinOp       BinOp Expression Expression
    | UnOp        UnOp Expression
    | AccesField  Expression Id
    | ActiveField Expression Id
    | AccesIndex  Expression [Expression]
    | TupleIndex  Expression Int
    -- | Cast        Expression Type
    | IdExpr      Id
    deriving (Show, Eq)

data BinOp
    = Sum
    | Sub
    | Prod
    | Div
    | Mod
    | Eq
    | Neq
    | Lt
    | Gt
    | Leq
    | Geq
    | And
    | Or
    deriving (Show, Eq)

data UnOp
    = Neg
    | Deref
    deriving (Show, Eq)

data Instruction
    = SimpleAssign      Expression Expression
    | MultAssign        [Expression] Expression
    | Print             Expression
    | Read              Expression
    | Return            [Expression]
    | If                IfInst
    | Switch            Expression [Case]
    | For               Id Expression Expression [Instruction]
    | While             Expression [Instruction]
    | FuncCallInst      Expression
    | New               Expression
    | Free              Expression
    | ExitInst          String
    | EmptyInst
    | Continue
    | Break
    deriving (Show, Eq)

data Case
    = Case    String [Instruction]
    | Default [Instruction]
    deriving (Show, Eq)

data IfInst
    = IfThen     Expression [Instruction]
    | IfThenElse Expression [Instruction] [Instruction]
    deriving (Show, Eq)

-- Type Checking
instance T.Typeable Expr where
    typeQuery (IntLit    _) = return T.IntT
    typeQuery (CharLit   _) = return T.CharT
    typeQuery (FloatLit  _) = return T.FloatT
    typeQuery (AtomLit   _) = return T.AtomT
    typeQuery (StringLit _) = return T.StringT
    typeQuery TrueLit       = return T.BoolT
    typeQuery FalseLit      = return T.BoolT

    typeQuery NullLit       = return T.NullT

    typeQuery (ArrayLit array@(x:xs)) = do

        types <- mapM (typeQuery . getExpr) array

        let arrType = head types
            cond1   = all T.notTypeError types
            cond2   = foldl' (\acc b -> arrType == b && acc ) True types

        if cond1
            then if cond2
                then return $ T.ArrayT arrType (length array)
                else do
                    let Just errorInd = findIndex (/= arrType) types
                        errorExpr     = array !! errorInd
                        errorTk       = getToken errorExpr
                        str           = Tk.cleanedString errorTk
                        pos           = Tk.position errorTk

                    ST.insertError $ Err.TE (Err.HeterogeneusArrayType str pos) 
                    return T.TypeError
            else return T.TypeError

    typeQuery (ArrayLit []) = error "Empty literal array not supported"

    typeQuery (TupleLit parts) = do

        types <- mapM (typeQuery . getExpr) parts

        let res = T.TupleT types

        if T.notTypeError res
            then return res
            else return T.TypeError

    typeQuery (FuncCall id args) = do

        types <- mapM (typeQuery . getExpr) args
        symT  <- get

        entry <- ST.lookupFunction id $ length args
        case entry of

            Just function -> do
                case ST.additional function of
                    
                    Just (ST.FunctionMetaData info) -> do
                        let params = ST.parameters info

                        paramTypes <- mapM T.getTypeFromString params

                        let cond1 = all T.notTypeError paramTypes
                            cond2 = and $ zipWith (==) paramTypes types

                        if cond1 && cond2 then do

                            let functionType = ST.symbolType function
                            case functionType of
                                Just typeName -> T.getTypeFromString typeName
                                Nothing -> return T.VoidT -- Ojo, esto está mal. Hay que implementar un tipo para las funciones

                        else return T.TypeError

                    Nothing                         -> do
                        let error = Err.FunctionWithoutMD id
                        ST.insertError $ Err.TE error 
                        return T.TypeError

                    _ -> return T.TypeError

            Nothing -> do
                let error = Err.NotAFunction id
                ST.insertError $ Err.TE error 
                return T.TypeError
                

    typeQuery (BinOp op a b)  = binOpCheck op a b

    typeQuery (UnOp Neg a) = do
        x <- typeQuery (getExpr a)

        let validTypes   =  [T.IntT, T.FloatT]
            tkErrPos     = Tk.position $ getToken a        
            correctTypes = map show validTypes

        if x `elem` validTypes 
            then return x
            else do
                let error = Err.InvalidTypeUnOp (show Neg) (show x) correctTypes tkErrPos
                ST.insertError $ Err.TE error
                return T.TypeError 

    typeQuery (UnOp Deref p) = do
        mustBePtr <- typeQuery (getExpr p)

        case mustBePtr of
            T.PointerT e
                | e == T.TypeError -> return T.TypeError
                | otherwise        -> return e
            _          -> do
                let error = Err.InvalidDereference (show mustBePtr) (Tk.position $ getToken p)
                ST.insertError $ Err.TE error
                return T.TypeError

    typeQuery (AccesField expr id) = do
        accessExpr <- typeQuery $ getExpr expr

        let isRecordType = case accessExpr of
                T.StructT scope -> Just scope
                T.UnionT scope  -> Just scope
                _               -> Nothing

        case isRecordType of
            (Just scope) -> do 
            
                symT <- get

                let inScope         = ST.filterByScopeST symT scope
                    possibleEntries = ST.findDictionary inScope id
                    position        = Tk.position $ getToken expr

                case possibleEntries of

                    Just [entry] -> case ST.symbolType entry of

                        Just strType -> T.getTypeFromString strType
                        Nothing      -> do
                            let error = Err.UnTypedRecordField id scope position
                            ST.insertError $ Err.TE error
                            return T.TypeError

                    Nothing       -> do
                        let error = Err.RecordFieldNotFound id scope position
                        ST.insertError $ Err.TE error
                        return T.TypeError
                    _             -> do
                        let error = Err.RepeatedRecordField id scope position 
                        ST.insertError $ Err.TE error
                        return T.TypeError
            _              -> do
                let name = Tk.cleanedString $ getToken expr 
                    pos  = Tk.position $ getToken expr 
                    error = Err.NotARecordType name pos
                ST.insertError $ Err.TE error
                return T.TypeError

    typeQuery (ActiveField expr id) = do
        unionExpr <- typeQuery $ getExpr expr

        let position = Tk.position $ getToken expr 

        case unionExpr of
            (T.UnionT scope) -> do
                symT <- get

                let inScope         = ST.filterByScopeST symT scope
                    possibleEntries = ST.findDictionary inScope id

                case possibleEntries of

                    Just [entry] -> case ST.symbolType entry of

                        Just _       -> return T.BoolT  
                        Nothing      -> do 
                            let error = Err.UnTypedRecordField id scope position
                            ST.insertError $ Err.TE error
                            return T.TypeError

                    Nothing      -> do
                        let error = Err.RecordFieldNotFound id scope position
                        ST.insertError $ Err.TE error
                        return T.TypeError

                    _            -> do
                        let error = Err.RepeatedRecordField id scope position 
                        ST.insertError $ Err.TE error
                        return T.TypeError

            _              -> do
                let name = Tk.cleanedString $ getToken expr 
                    pos  = Tk.position $ getToken expr 
                    error = Err.NotAnUnion name pos
                ST.insertError $ Err.TE error
                return T.TypeError

    typeQuery (AccesIndex arr inds)  = do
        arrType  <- typeQuery (getExpr arr)
        indTypes <- mapM (typeQuery . getExpr) inds

        let cond1 = T.notTypeError arrType
            cond2 = all T.notTypeError indTypes
            cond3 = foldl (\acc b -> T.IntT == b && acc ) True indTypes

        if cond1 && cond2 
            
            then if cond3
                then return $ T.ArrayT arrType (length indTypes)
                else do
                    let Just errorInd = findIndex (/=T.IntT) indTypes
                        errorTp       = show $ indTypes !! errorInd

                        errorExprTk   = getToken (inds !! errorInd)
                        exprString    = Tk.cleanedString errorExprTk
                        pos           = Tk.position errorExprTk 
                        error         = Err.InvalidIndexType errorTp exprString pos

                    ST.insertError $ Err.TE error
                    return T.TypeError

            else return T.TypeError

    typeQuery (TupleIndex tupleExpr ind) = do
        tupleType <- typeQuery (getExpr tupleExpr)

        if T.notTypeError tupleType then
            case tupleType of
                T.TupleT xs -> return $ xs !! ind
                _           -> do
                    let invalidType = show tupleType 
                        pos         = Tk.position $ getToken tupleExpr
                        error       = Err.NotATupleType invalidType pos
                    ST.insertError $ Err.TE error
                    return T.TypeError
            else return T.TypeError

    typeQuery (IdExpr id)       = do
        symT <- get

        idType <- ST.lookupST id

        case idType of
            Just entry -> case ST.symbolType entry of
                Just tp -> T.getTypeFromString tp
                Nothing -> do 
                    let err = Err.UnTypedId id
                    ST.insertError $ Err.TE err

                    return T.TypeError

            Nothing    -> do
                let err = Err.IdNotFound id
                ST.insertError $ Err.TE err

                return T.TypeError


binOpCheck :: BinOp -> Expression -> Expression -> ST.MonadParser T.Type
binOpCheck bop a b = do
    x <- typeQuery (getExpr a)
    d <- typeQuery (getExpr b)

    let validTypes 
         | bop `elem` [Sum,Sub,Prod,Mod,Div] = [T.IntT, T.FloatT]
         | bop `elem` [Eq,Neq,Lt,Gt,Leq,Geq] = [T.IntT, T.BoolT, T.FloatT, T.CharT]
         | otherwise                         = [T.BoolT]

        tkErrPos = Tk.position $ getToken a           
        lType    = show x
        rType    = show d 
        correctTypes = map show validTypes

    if x == d 
        then if x `elem` validTypes
            then return x
            else do 
                let error = Err.InvalidTypesBinOp (show bop) (lType,rType) correctTypes tkErrPos

                ST.insertError $ Err.TE error
                return T.TypeError 
        else do 
            let error = Err.InconsistentTypesBinOp (show bop) (lType,rType) correctTypes tkErrPos
                    
            ST.insertError $ Err.TE error
            return T.TypeError 


buildAndCheckExpr :: Tk.Token -> Expr -> ST.MonadParser Expression
buildAndCheckExpr tk expr = do
    exprType <- typeQuery expr

    return $ Expression { 
           getToken = tk,
           getExpr  = expr, 
           getType  = exprType
        }

-- Pretty print AST
putStrIdent :: Int -> String -> IO ()
putStrIdent n str = do
    replicateM_ n (putStr "| ")
    putStrLn $ escapeChars str
  where
    escapeChars ('\n':xs) = '\\' : 'n' : escapeChars xs
    escapeChars ('\t':xs) = '\\' : 't' : escapeChars xs
    escapeChars ('\\':xs) = '\\' : '\\' : escapeChars xs
    escapeChars (x:xs) = x : escapeChars xs
    escapeChars [] = []

prettyPrintProgram :: Program -> IO ()
prettyPrintProgram (Program global decs main) = do
    putStrIdent 0 $ "Program: "
    prettyPrintGlobal 1 global
    prettyPrintFunctionDecs 1 decs
    prettyPrintMain 1 main

prettyPrintGlobal :: Int -> Global -> IO ()
prettyPrintGlobal n global = do
    putStrIdent n "Global Scope:"
    mapM_ (prettyPrintInstruction (n+1)) global

prettyPrintFunctionDecs :: Int -> FunctionDeclarations -> IO ()
prettyPrintFunctionDecs n decs = do
    putStrIdent n "Functions Definition:"
    mapM_ (prettyPrintFunctionDec (n+1)) decs

prettyPrintFunctionDec :: Int -> FunctionDeclaration -> IO ()
prettyPrintFunctionDec n insts = do
    putStrIdent n "func performing:"
    mapM_ (prettyPrintInstruction (n+1)) insts

prettyPrintMain :: Int -> Main -> IO ()
prettyPrintMain n main = do
    putStrIdent n "Main:"
    mapM_ (prettyPrintInstruction (n+1)) main

prettyPrintInstruction :: Int -> Instruction -> IO ()
prettyPrintInstruction n (SimpleAssign e0 e1) = do
    putStrIdent n "Simple Assign"
    prettyPrintExpression (n+1) e0
    putStrIdent n "receives"
    prettyPrintExpression (n+1) e1
prettyPrintInstruction n (MultAssign e0 e1) = do
    putStrIdent n "Multiple Assign"
    mapM_ (prettyPrintExpression (n+1)) e0
    putStrIdent n "receives"
    prettyPrintExpression (n+1) e1
prettyPrintInstruction n (Print e) = do
    putStrIdent n "Print"
    prettyPrintExpression (n+1) e
prettyPrintInstruction n (Read e) = do
    putStrIdent n "Read"
    prettyPrintExpression (n+1) e
prettyPrintInstruction n (If inst) = prettyPrintIf n inst
prettyPrintInstruction n (Switch e cases) = do
    putStrIdent n "Switch"
    prettyPrintExpression (n+1) e
    putStrIdent n "Cases"
    mapM_ (prettyPrintCase (n+1)) cases
prettyPrintInstruction n (For id lower upper insts) = do
    putStrIdent n $ "For " ++ id
    putStrIdent n "lowerbound"
    prettyPrintExpression (n+1) lower
    putStrIdent n "upperbound"
    prettyPrintExpression (n+1) upper
    putStrIdent n "performing"
    mapM_ (prettyPrintInstruction (n+1)) insts
prettyPrintInstruction n (While expr insts) = do
    putStrIdent n "While"
    prettyPrintExpression (n+1) expr
    putStrIdent n "performing"
    mapM_ (prettyPrintInstruction (n+1)) insts
prettyPrintInstruction n (FuncCallInst expr) = do
    putStrIdent n "Function call instruction"
    prettyPrintExpression (n+1) expr
prettyPrintInstruction n (New expr) = do
    putStrIdent n "New"
    prettyPrintExpression (n+1) expr
prettyPrintInstruction n (Free expr) = do
    putStrIdent n "Free"
    prettyPrintExpression (n+1) expr
prettyPrintInstruction n (ExitInst prog) = do
    putStrIdent n $ "Abort " ++ prog
prettyPrintInstruction n inst = putStrIdent n $ show inst

prettyPrintExpression :: Int -> Expression -> IO ()
prettyPrintExpression n Expression{getExpr = (IntLit x)} = putStrIdent n $ show x
prettyPrintExpression n Expression{getExpr = (FloatLit x)} = putStrIdent n $ show x
prettyPrintExpression n Expression{getExpr = (CharLit x)} = putStrIdent n $ "\'" ++ show x ++ "\'"
prettyPrintExpression n Expression{getExpr = (StringLit x)} = putStrIdent n $ "\"" ++ x ++ "\""
prettyPrintExpression n Expression{getExpr = (AtomLit x)} = putStrIdent n $ "atom " ++ x
prettyPrintExpression n Expression{getExpr = TrueLit} = putStrIdent n "True"
prettyPrintExpression n Expression{getExpr = FalseLit} = putStrIdent n "False"
prettyPrintExpression n Expression{getExpr = NullLit} = putStrIdent n "Null"
prettyPrintExpression n Expression{getExpr = (ArrayLit exprs)} = do
    putStrIdent n "Array literal"
    mapM_ (prettyPrintExpression (n+1)) exprs
prettyPrintExpression n Expression{getExpr = (TupleLit exprs)} = do
    putStrIdent n "Tuple literal"
    mapM_ (prettyPrintExpression (n+1)) exprs
prettyPrintExpression n Expression{getExpr = (FuncCall id exprs)} = do
    putStrIdent n $ "Call function " ++ id ++ " with arguments"
    mapM_ (prettyPrintExpression (n+1)) exprs
prettyPrintExpression n Expression{getExpr = (BinOp op e0 e1)} = do
    putStrIdent n $ show op
    prettyPrintExpression (n+1) e0
    prettyPrintExpression (n+1) e1
prettyPrintExpression n Expression{getExpr = (UnOp op e)} = do
    putStrIdent n $ show op
    prettyPrintExpression (n+1) e
prettyPrintExpression n Expression{getExpr = (AccesField e id)} = do
    putStrIdent n $ "Field " ++ id ++ " of"
    prettyPrintExpression (n+1) e
prettyPrintExpression n Expression{getExpr = (ActiveField e id)} = do
    putStrIdent n $ "Check active field " ++ id ++ " of union"
    prettyPrintExpression (n+1) e
prettyPrintExpression n Expression{getExpr = (AccesIndex e es)} = do
    putStrIdent n "Index array"
    prettyPrintExpression (n+1) e
    putStrIdent n "with indices"
    mapM_ (prettyPrintExpression (n+1)) es
prettyPrintExpression n Expression{getExpr = (TupleIndex e idx)} = do
    putStrIdent n "Index tuple"
    prettyPrintExpression (n+1) e
    putStrIdent n $ "with index " ++ show idx
--prettyPrintExpression n Expression{getExpr = (Cast e tp)} = do
--    putStrIdent n "Cast expression"
--    prettyPrintExpression (n+1) e
--    putStrIdent n "to type"
--    prettyPrintType (n+1) tp
prettyPrintExpression n Expression{getExpr = (IdExpr id)} = putStrIdent n $ "id: " ++ id

prettyPrintIf :: Int -> IfInst -> IO ()
prettyPrintIf n (IfThen cond ifBlock) = do
    putStrIdent n "If"
    prettyPrintExpression (n+1) cond
    putStrIdent n "Then"
    mapM_ (prettyPrintInstruction (n+1)) ifBlock
prettyPrintIf n (IfThenElse cond ifBlock elseBlock) = do
    putStrIdent n "If"
    prettyPrintExpression (n+1) cond
    putStrIdent n "Then"
    mapM_ (prettyPrintInstruction (n+1)) ifBlock
    putStrIdent n "Else"
    mapM_ (prettyPrintInstruction (n+1)) elseBlock

prettyPrintCase :: Int -> Case -> IO ()
prettyPrintCase n (Case atom block) = do
    putStrIdent n $ "Case " ++ atom
    mapM_ (prettyPrintInstruction (n+1)) block
prettyPrintCase n (Default block) = do
    putStrIdent n "Default"
    mapM_ (prettyPrintInstruction (n+1)) block


