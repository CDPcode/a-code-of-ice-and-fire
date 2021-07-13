module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Position(..),Token(..))

data Error 
    = PE ParserError 
    | TE TypeError

-- ^ Sorted in occurrence order
data ParserError
    = FRepeatedDeclarations String Tk.Position
    | FRepeatedDefinitions String Tk.Position
    | InvalidNArgsDef String Int Tk.Position
    | FDefinitionWithoutDeclaration String Tk.Position
    | RepeatedAliasName String Tk.Position
    -- ^ Preparser related

    | UndefinedFunction String Tk.Position
    | RedeclaredParameter String Tk.Position
    | RedeclaredName String Tk.Position
    | UndefinedVar String Tk.Position
    | InvalidVar String String Tk.Position
    | RedeclaredVar String Tk.Position
    | RedeclaredConstant String Tk.Position
    | ExpectedFunction String String Tk.Position
    -- ^ Parser related

    | SyntaxErr Tk.Token
    | SyntaxErrEOF
    -- ^ parseError :: [Tk.Token] -> a/syntax related
    deriving Show

data TypeError 
    = VoidForNow
    deriving (Show, Eq)