module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token (..))


data ParserError a 
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