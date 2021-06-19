module Westeros.SouthOfTheWall.AST where

import Westeros.SouthOfTheWall.Tokens as Tk

data Program = Program Header FunctionNames Global FunctionDec Main Aliases 

type Header = String
type FunctionNames = [(Id, Int)]
type Global = [Declaration]
type FunctionDec = Id [Argument] [Type] [Ins]
type Main = [Instruction]
type Aliases = [AliasDeclaration]

data Definition = Definition Declaration (Maybe Expression)

data Argument = Argument Declaration Bool

data AliasDeclaration = Alias { name :: Id, dataType :: Type, aliasType :: AliasType }

data DecType
    = Const
    | Var

data AliasType
    = Strong 
    | Weak 

data Id = Id Token Int deriving (Show, Eq)

data Declaration 
    = Simple  { name :: Id, dataType :: Type, decType :: DecType }
    | Array   { name :: Id, dataType :: Type, decType :: DecType, sizes :: [Int] }
    | Struct  { name :: Id, fields :: [Declaration] }
    | Union   { name :: Id, fields :: [Declaration] }
    | Pointer { name :: Id, dataType :: Type }
    deriving (Eq, Show)

data Type 
    = IntT 
    | FloatT 
    | CharT 
    | AtomT 
    | BoolT 
    | StringT
    | ArrayT    { arrayType :: Type, arrrayDim :: Int }
    | StructT   { structFields :: [(Id, Type)] }
    | UnionT    { unionOptions :: [(Id, Type)] }
    | TupleT    { tupleTypes :: [Type] }
    | PointerT  { pointerType :: Type }
    | AliasT    { aliasName :: String }
    deriving (Eq, Show)

data Expression = Expression
       { getExpression :: Expr
       , getType :: Type
       , getToken :: Tk.Token
       } deriving Show

data Expr
    = IntLit    { val :: Int }
    | CharLit   { val :: Char }
    | FloatLit  { val :: Float }
    | StringLit { val :: String }
    | AtomLit   { val :: String }
    | TrueLit
    | FalseLit
    | NullLit
    | ArrayLit    { elems :: [Expression] }
    | TupleLit    { elems :: [Expression] }
    | FuncCall    { fname :: Expression, args :: [Expression] }
    | BinOp       { op :: BinOp, opr1 :: Expression, opr2 :: Expression }
    | UnOp        { op :: UnOp, opr :: Expression }
    | AccesField  { obj :: Expression, field :: Expression } 
    | ActiveField { obj :: Expression, field :: Expression }
    | AccesIndex  { obj :: Expression, index :: Expression }
    | Cast        { obj :: Expression, castType ::  Type }
    | IdExpr      { id :: Id }
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
  = SimpleAssign { lvalue :: Expression, rvalue :: Expression }
  | MultAssign   { lvalues :: [Expression], rvalue :: Expression }
  | Print        { expr :: Expression }
  | Read         { expr :: Expression }
  | Return       { values :: [Expression] }
  | Return 
  | If IfInst
  | Switch       { cond :: Expression, cases :: [Case] }
  | For          { it :: Id, lb :: Expression, ub :: Expression, inst :: [Instruction] }
  | While        { cond :: Expression, instr :: [Instruction] }
  | Definition Definition
  | FuncCallInst { fName :: Id, args :: [Expression] }
  | New          { tp :: Type }
  | Free         { expr :: Expression }
  | ExitInst     { expr :: Expression }
  | EmptyInst 
  | Continue 
  | Break
  deriving (Eq, Show)

data Case 
    = Case    { atom :: Expression,  instr :: [Instruction] }
    | Default { instr :: [Instruction] }

data IfInst 
    = If     { cond :: Expression, instr :: [Instruction] }
    | IfElse { cond :: Expression, true :: [Instruction], false :: [Instruction] }