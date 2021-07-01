module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Position(..))

data Error 
    = PE ParserError 
    | TE TypeError

-- ^ Sorted in occurrence order
data ParserError
    = FRepeatedDeclarations String
    | InvalidNArgsDef String Int
    | FDefinitionWithoutDeclaration String
    | RepeatedAliasName String 
    | FRepeatedDefinitions String     
    -- ^ Preparser related

    | UndefinedFunction String String
    | RedeclaredParemeter String Tk.Position
    | RedeclaredName String Tk.Position
    | UndefinedVar String Tk.Position
    | InvalidVar String String Tk.Position
    | RedeclaredVar String Tk.Position
    | RedeclaredConstant String Tk.Position
    | ExpectedFunction String String Tk.Position
    -- ^ Parser related
    deriving Show

data TypeError 
    = Nothing