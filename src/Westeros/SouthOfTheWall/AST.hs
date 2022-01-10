module Westeros.SouthOfTheWall.AST (
      Program (..)
    , Id
    , Main
    , Global
    , FunctionDeclaration
    , FunctionDeclarations
    , Expression (..)
    , Expr (..)
    , BinOp (..)
    , UnOp (..)
    , Instruction (..)
    , IfInst (..)
    , Case (..)
    , isValidLValue
    , isFunctionCall
    , prettyPrintProgram
    ) where

import Control.Monad (replicateM_)

import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Types as T

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
       , getTypeStr :: String
       } deriving (Show, Eq)

data Expr
    = IntLit    Int
    | CharLit   Char
    | FloatLit  Float
    | StringLit String
    | AtomLit   Int
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
    | Cast        Expression String
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
    | Not
    deriving (Show, Eq)

data Instruction
    = SimpleAssign      Expression Expression
    | MultAssign        [Expression] Expression
    | Print             Expression
    | PrintStr          String
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
    = Case    Int [Instruction]
    | Default [Instruction]
    deriving (Show, Eq)

data IfInst
    = IfThen     Expression [Instruction]
    | IfThenElse Expression [Instruction] [Instruction]
    deriving (Show, Eq)

isValidLValue :: Expression -> Bool
isValidLValue = isValidLValue' . getExpr

isValidLValue' :: Expr -> Bool
isValidLValue' IdExpr{}         = True
isValidLValue' AccesField{}     = True
isValidLValue' TupleIndex{}     = True
isValidLValue' AccesIndex{}     = True
isValidLValue' (UnOp Deref _)   = True
isValidLValue' _                = False

isFunctionCall :: Expression -> Bool
isFunctionCall = isFunctionCall' . getExpr

isFunctionCall' :: Expr -> Bool
isFunctionCall' (FuncCall _ _) = True
isFunctionCall' _             = False

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
    putStrIdent 0 "Program: "
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
prettyPrintInstruction n (For symbol lower upper insts) = do
    putStrIdent n $ "For " ++ symbol
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
prettyPrintExpression n expr@Expression{getExpr = (IntLit x)} = putStrIdent n $ show x ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = (FloatLit x)} = putStrIdent n $ show x ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = (CharLit x)} = putStrIdent n $ "\'" ++ show x ++ "\'" ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = (StringLit x)} = putStrIdent n $ "\"" ++ x ++ "\"" ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = (AtomLit x)} = putStrIdent n $ "atom " ++ show x ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = TrueLit} = putStrIdent n $ "True" ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = FalseLit} = putStrIdent n  $"False" ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = NullLit} = putStrIdent n $ "Null" ++ " (" ++ show (getType expr) ++ ")"
prettyPrintExpression n expr@Expression{getExpr = (ArrayLit exprs)} = do
    putStrIdent n $ "Array literal" ++ " (" ++ show (getType expr) ++ ")"
    mapM_ (prettyPrintExpression (n+1)) exprs
prettyPrintExpression n expr@Expression{getExpr = (TupleLit exprs)} = do
    putStrIdent n $ "Tuple literal" ++ " (" ++ show (getType expr) ++ ")"
    mapM_ (prettyPrintExpression (n+1)) exprs
prettyPrintExpression n expr@Expression{getExpr = (FuncCall symbol exprs)} = do
    putStrIdent n $ "Call function " ++ symbol ++ " with arguments" ++ " (" ++ show (getType expr) ++ ")"
    mapM_ (prettyPrintExpression (n+1)) exprs
prettyPrintExpression n expr@Expression{getExpr = (BinOp op e0 e1)} = do
    putStrIdent n $ show op ++ " (" ++ show (getType expr) ++ ")"
    prettyPrintExpression (n+1) e0
    prettyPrintExpression (n+1) e1
prettyPrintExpression n expr@Expression{getExpr = (UnOp op e)} = do
    putStrIdent n $ show op ++ " (" ++ show (getType expr) ++ ")"
    prettyPrintExpression (n+1) e
prettyPrintExpression n expr@Expression{getExpr = (AccesField e symbol)} = do
    putStrIdent n $ "Field " ++ symbol ++ " of" ++ " (" ++ show (getType expr) ++ ")"
    prettyPrintExpression (n+1) e
prettyPrintExpression n expr@Expression{getExpr = (ActiveField e symbol)} = do
    putStrIdent n $ "Check active field " ++ symbol ++ " of union" ++ " (" ++ show (getType expr) ++ ")"
    prettyPrintExpression (n+1) e
prettyPrintExpression n expr@Expression{getExpr = (AccesIndex e es)} = do
    putStrIdent n $ "Index array" ++ " (" ++ show (getType expr) ++ ")"
    prettyPrintExpression (n+1) e
    putStrIdent n "with indices"
    mapM_ (prettyPrintExpression (n+1)) es
prettyPrintExpression n expr@Expression{getExpr = (TupleIndex e idx)} = do
    putStrIdent n $ "Index tuple" ++ " (" ++ show (getType expr) ++ ")"
    prettyPrintExpression (n+1) e
    putStrIdent n $ "with index " ++ show idx
prettyPrintExpression n Expression{getExpr = (Cast e tp)} = do
    putStrIdent n "Cast expression"
    prettyPrintExpression (n+1) e
    putStrIdent n "to type"
    putStrIdent n tp
prettyPrintExpression n Expression{getExpr = (IdExpr sym)} = putStrIdent n $ "id: " ++ sym

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
    putStrIdent n $ "Case " ++ show atom
    mapM_ (prettyPrintInstruction (n+1)) block
prettyPrintCase n (Default block) = do
    putStrIdent n "Default"
    mapM_ (prettyPrintInstruction (n+1)) block
