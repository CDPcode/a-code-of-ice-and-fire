module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token (..))

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

    | DeclaredAndNotDefined
    | InvalidNargsCalls
    | SameScopeRedefinition
    | UndefinedIdentifier 
    deriving Show

data TypeError 
    = Nothing