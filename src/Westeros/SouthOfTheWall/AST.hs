module Westeros.SouthOfTheWall.AST where

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
    | ArrayVarDeclaration   Id Type [Expr]
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
       { getExpression :: Expr
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
    = SimpleAssign Expression Expression
    | MultAssign   [Expression] Expression
    | Print        Expression
    | Read         Expression
    | Return       [Expression]
    | If IfInst
    | Switch       Expression [Case]
    | For          Id Expression Expression [Instruction]
    | While        Expression [Instruction]
    | DeclarationInst Declaration
    | FuncCallInst Expression
    | New          Expression
    | Free         Expression
    | ExitInst     String
    | EmptyInst 
    | Continue 
    | Break
    deriving (Show)

data Case 
    = Case    Expression [Instruction]
    | Default [Instruction]
    deriving (Show)

data IfInst 
    = IfThen     Expression [Instruction]
    | IfThenElse Expression [Instruction] [Instruction] 
    deriving (Show)