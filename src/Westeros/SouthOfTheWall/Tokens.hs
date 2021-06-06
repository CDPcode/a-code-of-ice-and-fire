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
    TknComment
    | TknString 

    -- Type tokens
    | TknProgramStart
    | TknProgramName
    | TknVar
    | TknConst
    | TknType
    | TknBeginAlias
    | TknStrongAlias
    | TknWeakAlias
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
    | TknEndIDCompType
    | TknStruct
    | TknUnion
    | TknArray
    | TknStringLit
    | TknPointer
    | TknArraySize
    | TnkStringSize
    | TknArrayDecl
    | TnkStringDecl

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
    | TknNot
    | TknAnd
    | TknOr
    | TknBoolEqual
    | TknBoolNotEqual

    -- Composite Types Operators
    | TknStructField
    | TknBeginUnionQuestion
    | TknUnionQuestion
    | TknEndUnionQuestion
    | TknUnionField
    | TknBeginIndex
    | TknBeginTupleIndex
    | TknEndIndex

    -- IO tokens
    | TknRead
    | TknPrint

    -- Procedure Tokens
    | TknFirstMain
    | TknLastMain
    | TknPass -- OJO
    | TknFunctionArgs
    | TknBeginReturnVals
    | TknEndReturnVals
    | TknReturnOpen
    | TknReturnClose
    | TknProcCallOpen
    | TknProcCallArgs
    | TknProcCallClose
    | TknValueArg
    | TknReferenceArg

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

    -- Identifiers
    | TknID
    | TknArgNumber

    -- Punctuation
    | TknComma
    | TknDot
    
    -- Expressions
    | TknOpenParenthesis
    | TknCloseParenthesis