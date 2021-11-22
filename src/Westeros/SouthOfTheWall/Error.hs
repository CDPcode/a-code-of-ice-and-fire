module Westeros.SouthOfTheWall.Error(
    Error(..),
    ParserError(..),
    TypeError(..),
    ExpectedTypes(..)
) where

import qualified Westeros.SouthOfTheWall.Tokens as Tk 

data Error
    = PE ParserError
    | TE TypeError

data ParserError
    = RedeclareFunction String Int Tk.Position
    | RedefineFunction String Int Tk.Position
    | UndefinedFunction String Int Tk.Position
    | UndeclaredFunction String Int Tk.Position
    | NonCallableExpression Tk.Position
    | RedeclaredName String Tk.Position
    | UndeclaredName String Tk.Position
    | NoLoop Tk.Position
    | MultiAssignmentLengthMissmatch Int Int Tk.Position
    | IndexOutOfBounds Int Int Tk.Position
    | SyntaxErr String Tk.Position
    | SyntaxErrEOF
    deriving Show

data TypeError

    = UnexpectedType String String Tk.Position
    | InvalidExprType String Tk.Position
    | InvalidField String Tk.Position
    | IncompatibleTypes String String Tk.Position
    | HeterogeneusArrayType Tk.Position
    | InvalidLValue Tk.Position
    | ConstantReassignment Tk.Position
    | NonCasteableTypes String String Tk.Position
    deriving (Show, Eq)

data ExpectedTypes
    = Bool
    | Int
    | Pointer
    | Atom
    | Array
    deriving (Eq)

instance Show ExpectedTypes where
    show Bool = "Boolton"
    show Int = "Lanninteger"
    show Pointer = "Spearwife"
    show Atom = "Barathom"
    show Array = "Lord Commander"
