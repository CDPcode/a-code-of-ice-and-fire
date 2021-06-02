module Westeros.SouthOfTheWall.Tokens (
    Token(..),
    AbstractToken(..),
    Position(..),
    Error(..)
) where

{- Relevant tokens datatypes -}

data Position = Position {
        row :: Int,
        col :: Int
    }

data AbstractToken = TkString | TkComment

data Token = Token 
    { aToken :: AbstractToken
    , capturedString :: String
    , cleanedString :: String
    , position :: Position
    }

newtype Error = Error { lexerError :: String } 