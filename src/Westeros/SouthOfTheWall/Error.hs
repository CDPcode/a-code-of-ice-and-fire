module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token (..))


newtype Context a = Context { context :: (a,Tk.Token) }

class CompilerError a where
    buildContext :: a -> Context a 

instance (Show a) => Show ( Context a ) where
    show e = undefined

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