module Westeros.SouthOfTheWall.Tokens (
    Token(..),
    AbstractToken(..),
    Position(..),
    Error(..)
) where

{- Relevant tokens datatypes -}

newtype Error = Error { lexerError :: String } 

data Position = Position {
        row :: Int,
        col :: Int
    }

data Token = Token 
    { aToken :: AbstractToken
    , capturedString :: String
    , cleanedString :: String
    , position :: Position
    }

data AbstractToken = 
    TkString 
    | TkComment
    -- Type tokens
    | TknVar
    | TknConst
    | TknType
    | TknBeginAlias
    | TknStrongAlias
    | TknWeekAlias
    | TknInt
    | TknIntLit
    | TknFloat
    | TknFloatLit
    | TknTrilean
    | TknTrue
    | TknNeutral
    | TknFalse
    | TknChar
    | TknCharLit
    | TknBeginCompType
    | TknEndCompType
    | TknStruct
    | TknComma
    | TknUnion
    | TknArray
    -- Assignment tokens
    | TknAssign
    | TknBeginMultAssign
    | TknEndMultAssign
    | TknTupleAsign
    -- Operators tokens
    | TknMinus
    | TknPlus
    | TknMult
    | TknMod
    | TknNegate
    | TknEqual
    | TknNotEqual
    | TknLessThan
    | TknGreaterThan
    | TknLessEqThan
    | TknGreaterEqThan
    | TknBeginExit
    | TknEndExit
    -- IO tokens
    | TknRead
    | TknPrint
    -- Procedure Tokens
    | TknPass -- OJO
    | TknFunctionArgs
    | TknBeginReturnVals
    | TknEndReturnVals
    | TknReturnOpen
    | TknReturnClose
    | TknProcCallOpen
    | TknProcCallArgs
    | TknProcCallClose

    | TknVoid
    -- Repetition Tokens
    | TknFor
    | TknForLB
    | TknForUB
    | TknWhile
    | TknWhileDecoration
    | TknContinue
    | TknBreak 
    -- Selection Tokens
    | TknBeginSelection
    | TknSelectionDecorator
    | TknTrueBranch
    | TknUnknownBranch
    | TknFalseBranch 
    | TknEndSelection