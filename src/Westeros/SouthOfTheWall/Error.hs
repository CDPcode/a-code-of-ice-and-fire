module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token (..))


data ParserError  
    = FDefinitionWithoutDeclaration
    | FRepeatedDeclarations
    | FRepeatedDefinitions
    | RepeatedAliasDefinitions
    | AliasFunctionNameConflict

    | DeclaredAndNotDefined
    | InvalidNargsCalls
    | SameScopeRedefinition
    | UndefinedIdentifier 
    deriving Show