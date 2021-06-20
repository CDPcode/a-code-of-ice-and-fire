module Westeros.SouthOfTheWall.AST where

import Westeros.SouthOfTheWall.Tokens as Tk

type Size = Int 

-- AST 
data Program = Program Header FunctionNames Global FunctionDec Main Aliases 

type Header = String
type FunctionNames = [(Id, Int)]
type Global = [Declaration]
type FunctionDec = Id [Argument] [Type] [Ins]
type Main = [Instruction]
type Aliases = [AliasDeclaration]

data Definition = Definition Declaration (Maybe Expression)

data Argument = Argument Declaration Bool

data AliasDeclaration = Alias Id Type AliasType

data DecType
    = Const
    | Var

data AliasType
    = Strong 
    | Weak 

data Id = Id Token Int deriving (Show, Eq)

data Declaration 
    = Simple  Id Type DecType
    | Array   Id Type DecType [Size]
    | Struct  Id [Declaration]
    | Union   Id [Declaration]
    | Pointer Id Type
    deriving (Eq, Show)

data Type 
    = IntT 
    | FloatT 
    | CharT 
    | AtomT 
    | BoolT 
    | StringT
    | ArrayT    Type Int
    | StructT   [(Id, Type)]
    | UnionT    [(Id, Type)]
    | TupleT    [Type]
    | PointerT  Type
    | AliasT    String
    deriving (Eq, Show)

data Expression = Expression
       { getExpression :: Expr
       , getType :: Type
       , getToken :: Tk.Token
       } deriving Show

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
    | FuncCall    Expression [Expression]
    | BinOp       BinOp Expression Expression
    | UnOp        UnOp Expression
    | AccesField  Expression Expression 
    | ActiveField Expression Expression
    | AccesIndex  Expression Expression
    | Cast        Expression Type
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
    | And
    | Or
    deriving (Show, Eq)

data UnOp 
    = Neg 
    | Deref
    deriving (Show, Eq)

data Instruction
  = SimpleAssign Expression Expression
  | MultAssign   [Expression] Expression
  | Print        Expression
  | Read         Expression
  | Return       [Expression]
  | Return 
  | If IfInst
  | Switch       Expression [Case]
  | For          Id Expression Expression [Instruction]
  | While        Expression [Instruction]
  | DefinitionInst Definition
  | FuncCallInst Id [Expression]
  | New          Type
  | Free         Expression
  | ExitInst     Expression
  | EmptyInst 
  | Continue 
  | Break
  deriving (Eq, Show)

data Case 
    = Case    Expression [Instruction]
    | Default [Instruction]

data IfInst 
    = If     Expression [Instruction]
    | IfElse Expression [Instruction] [Instruction]