module Westeros.SouthOfTheWall.Grammar where

import Westeros.SouthOfTheWall.Tokens as Tk

data ExprWrapper = ExprWrapper
       { getExpression :: Expr
       , getToken :: Tk.Token
       } deriving Show

data BinOp 
    = Addition | Substraction | Product | Division | Modulo
    | Eq | Neq | Lt | Gt | Let | Get 
    deriving Show

data UnOp = Not  deriving Show

data Expr
    = IntLit Int
    | CharLit Char 
    | FloatLit Float
    | Trillean -- OJO
    | Bop BinOp Expr Expr
    | Uop UnOp Expr Expr
   deriving Show