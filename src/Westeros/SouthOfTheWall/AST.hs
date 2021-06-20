module Westeros.SouthOfTheWall.AST where

import Control.Monad        (replicateM_)

import Westeros.SouthOfTheWall.Tokens as Tk

-- AST
data Program = Program Header FunctionNames Global FunctionDeclarations Main Aliases deriving (Show)

-- data Id = Id Token Int deriving (Show, Eq)
type Id = String
type Header = String
type FunctionNames = [(Id, Int)]
type Global = [Declaration]
type Main = [Instruction]
type Aliases = [AliasDeclaration]
type FunctionDeclarations = [FunctionDeclaration]

data FunctionDeclaration = FunctionDeclaration Id [Parameter] [Type] [Instruction] deriving (Show)

data Declaration
    = VarDeclaration VariableDeclaration (Maybe Expression)
    | ConstantDeclaration Id Type Expression
    deriving (Show)

data Parameter = Parameter ParamType Id Type deriving (Show)

data ParamType
    = Ref
    | Value
    deriving (Show)

data AliasDeclaration = Alias Id Type AliasType deriving (Show)

data AliasType
    = StrongAlias
    | WeakAlias
    deriving (Show)

data VariableDeclaration
    = SimpleDeVarDeclaration  Id Type
    | ArrayVarDeclaration   Id Type [Expression]
    deriving (Show)

data Type
    = IntT
    | FloatT
    | CharT
    | AtomT
    | BoolT
    | StringT
    | ArrayT    Type Int
    | StructT   [VariableDeclaration]
    | UnionT    [VariableDeclaration]
    | TupleT    [Type]
    | PointerT  Type
    | AliasT    Id
    deriving (Show)

data Expression = Expression
       { getExpr :: Expr
       , getType :: Type
       , getToken :: Tk.Token
       } deriving (Show)

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
    | Cast        Expression Type
    | IdExpr      Id
    deriving (Show)

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
    deriving (Show)

data UnOp
    = Neg
    | Deref
    deriving (Show)

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
    | DeclarationInst   Declaration
    | FuncCallInst      Expression
    | New               Expression
    | Free              Expression
    | ExitInst          String
    | EmptyInst
    | Continue
    | Break
    deriving (Show)

data Case
    = Case    String [Instruction]
    | Default [Instruction]
    deriving (Show)

data IfInst
    = IfThen     Expression [Instruction]
    | IfThenElse Expression [Instruction] [Instruction]
    deriving (Show)


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
prettyPrintProgram (Program header contents global decs main alias) = do
    putStrIdent 0 $ "Program: " ++ header
    prettyPrintContents 1 contents
    prettyPrintGlobal 1 global
    prettyPrintFunctionDecs 1 decs
    prettyPrintMain 1 main
    prettyPrintAliases 1 alias

prettyPrintContents :: Int -> FunctionNames -> IO ()
prettyPrintContents n names = do
    putStrIdent n "Functions:"
    mapM_ printName names
  where
    printName :: (Id, Int) -> IO ()
    printName (id, args) = putStrIdent (n+1) (id ++ " " ++ show args)

prettyPrintGlobal :: Int -> Global -> IO ()
prettyPrintGlobal n global = do
    putStrIdent n "Global Scope:"
    mapM_ (prettyPrintDeclaration (n+1)) global

prettyPrintFunctionDecs :: Int -> FunctionDeclarations -> IO ()
prettyPrintFunctionDecs n decs = do
    putStrIdent n "Functions Definition:"
    mapM_ (prettyPrintFunctionDec (n+1)) decs

prettyPrintMain :: Int -> Main -> IO ()
prettyPrintMain n main = do
    putStrIdent n "Main:"
    mapM_ (prettyPrintInstruction (n+1)) main

prettyPrintAliases :: Int -> Aliases -> IO ()
prettyPrintAliases n aliases = do
    putStrIdent n "Aliases:"
    mapM_ (prettyPrintAlias (n+1)) aliases

prettyPrintDeclaration :: Int -> Declaration -> IO ()
prettyPrintDeclaration n (VarDeclaration vDec Nothing) = prettyPrintVarDec n vDec
prettyPrintDeclaration n (VarDeclaration vDec (Just e)) = do
    prettyPrintVarDec n vDec
    prettyPrintExpression (n+1) e
prettyPrintDeclaration n (ConstantDeclaration id tp expr) = do
    putStrIdent n $ "const " ++ id ++ "of type"
    prettyPrintType (n+1) tp
    putStrIdent n $ "with value:"
    prettyPrintExpression (n+1) expr

prettyPrintFunctionDec :: Int -> FunctionDeclaration -> IO ()
prettyPrintFunctionDec n (FunctionDeclaration id params types insts) = do
    putStrIdent n $ "func " ++ id ++ "with params"
    mapM_ (prettyPrintParam (n+1)) params
    putStrIdent n "returning"
    mapM_ (prettyPrintType (n+1)) types
    putStrIdent n "performing"
    mapM_ (prettyPrintInstruction (n+1)) insts

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
prettyPrintInstruction n (DeclarationInst dec) = prettyPrintDeclaration n dec
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

prettyPrintAlias :: Int -> AliasDeclaration -> IO ()
prettyPrintAlias n (Alias id tp aliasType) = do
    putStrIdent n $ show aliasType ++ " " ++ id
    prettyPrintType (n+1) tp

prettyPrintVarDec :: Int -> VariableDeclaration -> IO ()
prettyPrintVarDec n (SimpleDeVarDeclaration id tp) = do
    putStrIdent n $ "var " ++ id ++ " of type"
    prettyPrintType (n+1) tp
prettyPrintVarDec n (ArrayVarDeclaration id tp exprs) = do
    putStrIdent n $ "var " ++ id ++ " of type"
    prettyPrintType (n+1) tp
    putStrIdent n "with sizes"
    mapM_ (prettyPrintExpression (n+1)) exprs

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
prettyPrintExpression n Expression{getExpr = (Cast e tp)} = do
    putStrIdent n "Cast expression"
    prettyPrintExpression (n+1) e
    putStrIdent n "to type"
    prettyPrintType (n+1) tp
prettyPrintExpression n Expression{getExpr = (IdExpr id)} = putStrIdent n $ "id: " ++ id

prettyPrintType :: Int -> Type -> IO ()
prettyPrintType n (ArrayT tp x) = do
    putStrIdent n $ "Type: " ++ show x ++ "-dimensional array of"
    prettyPrintType (n+1) tp
prettyPrintType n (StructT decs) = do
    putStrIdent n "Type: struct with fields"
    mapM_ (prettyPrintVarDec (n+1)) decs
prettyPrintType n (UnionT decs) = do
    putStrIdent n "Type: union with fields"
    mapM_ (prettyPrintVarDec (n+1)) decs
prettyPrintType n (TupleT tps) = do
    putStrIdent n "Type: tuple with fields of type"
    mapM_ (prettyPrintType (n+1)) tps
prettyPrintType n (PointerT tp) = do
    putStrIdent n "Type: pointer to"
    prettyPrintType (n+1) tp
prettyPrintType n tp = putStrIdent n $ "Type: " ++ show tp

prettyPrintParam :: Int -> Parameter -> IO ()
prettyPrintParam n (Parameter pType id tp) = do
    putStrIdent n $ show pType ++ " " ++ id ++ " of type"
    prettyPrintType (n+1) tp

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


