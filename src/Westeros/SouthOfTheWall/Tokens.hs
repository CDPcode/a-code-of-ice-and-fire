module Westeros.SouthOfTheWall.Tokens (
    Token(..),
    AbstractToken(..),
    Position(..),
    Error(..)
) where

{- Relevant tokens datatypes -}

newtype Error = Error { lexerError :: String } 
    deriving (Show)

data Position = Position {
        row :: Int,
        col :: Int
    }
    deriving (Eq,Show)

data Token = Token 
    { aToken :: AbstractToken
    , capturedString :: String
    , cleanedString :: String
    , position :: Position
    }
    deriving (Eq,Show)



data AbstractToken = 
    -- Program Start
    TknProgramStart   
    | TknProgramName 

    -- Type Declaration
    | TknVar     
    | TknConst 
    | TknVarPointer
    | TknType
    | TknConstValue
    | TknBeginAlias
    | TknStrongAlias
    | TknWeakAlias

    -- Simple Types
    | TknInt
    | TknFloat
    | TknBool
    | TknChar
    | TknAtom
    | TknVoid
    
    -- Literals
    | TknTrue
    | TknFalse
    | TknIntLit
    | TknFloatLit
    | TknCharLit
    | TknBeginArrayLit
    | TknEndArrayLit
    | TknBeginTupleLit
    | TknEndTupleLit
    | TknNull
    | TknNaturalLit

    -- Composite Types
    | TknBeginCompTypeId
    | TknEndCompTypeId
    | TknBeginArray
    | TknEndArray
    | TknString
    | TknBeginSizes
    | TknEndSizes
    | TknBeginStruct
    | TknEndStruct
    | TknBeginUnion
    | TknEndUnion
    | TknPointerType
    | TknBeginTuple
    | TknEndTuple
    | TknStringLit

    -- Type conversion
    | TknCast

    -- Operators
    | TknAssign
    | TknTupleAssign
    | TknPlus
    | TknMinus
    | TknMult
    | TknDivide
    | TknNegate
    | TknMod
    | TknAnd
    | TknOr
    | TknEqual
    | TknNotEqual
    | TknLessThan
    | TknGreaterThan
    | TknLessEqThan
    | TknGreaterEqThan

    -- Composite types operators
    | TknStructField
    | TknUnionQuery
    | TknUnionField
    | TknBeginIndex
    | TknEndIndex
    | TknTupleSelect
    | TknNew
    | TknDereference
    | TknFree

    -- Exit Statement
    | TknBeginExit
    | TknEndExit

    -- IO
    | TknRead
    | TknPrint

    -- Empty Statement
    | TknPass

    -- Procedures definition
    | TknBeginFuncDecl
    | TknFunctionItem
    | TknGlobalDec
    | TknMain
    | TknBeginFunctionParams
    | TknEndFunctionParams 
    | TknBeginReturnVals
    | TknEndReturnVals
    | TknReturnOpen
    | TknReturnClose
    | TknValueParam
    | TknReferenceParam

    -- Blocks
    | TknOpenBlock
    | TknCloseBlock

    -- Procedure Call
    | TknProcCallOpen
    | TknProcCallArgs
    | TknProcCallClose

    -- Determinate Repetition
    | TknFor
    | TknForLB
    | TknForUB
    | TknEndFor

    -- Undeterminate Repetition
    | TknWhile
    | TknWhileDecorator
    | TknEndWhile

    -- Non-Structured flow
    | TknContinue
    | TknBreak

    -- Simple Selection
    | TknBeginSimpleSelection
    | TknSimpleSelectionDecorator
    | TknElse
    | TknEndSimpleSelection

    -- Multiple Selection
    | TknBeginMultipleSelection
    | TknMultipleSelectionDecorator
    | TknBranch
    | TknEndMultipleSelection

    -- Identifiers
    | TknID
    | TknParamNumber

    -- Atoms
    | TknNothing
    | TknAtomLit

    -- Appendix
    | TknAliasDec

    -- Dot, Comman
    | TknComma
    | TknDot

    -- Miscelaneous symbols
    | TknOpenParenthesis
    | TknCloseParenthesis

    -- Comments
    | TknComment
   deriving (Show,Eq)