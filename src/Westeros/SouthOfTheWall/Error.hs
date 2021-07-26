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
    | UndefinedType String Tk.Position
    -- ^ Preparser related

    | UndefinedFunction String Tk.Position
    | RedeclaredParameter String Tk.Position
    | RedeclaredName String Tk.Position
    | UndefinedVar String Tk.Position
    | InvalidVar String String Tk.Position
    | RedeclaredVar String Tk.Position
    | RedeclaredConstant String Tk.Position
    | ExpectedFunction String String Tk.Position
    | NonCallableExpression Tk.Position
    -- ^ Parser related

    | SyntaxErr Tk.Token
    | SyntaxErrEOF
    -- ^ parseError :: [Tk.Token] -> a/syntax related
    deriving Show

data TypeError
    {- Arrays -}
    = HeterogeneusArrayType String Tk.Position
    | InvalidIndexType String String Tk.Position
        -- ^ Invalid Type
        -- ^ Expression cleaned String
        -- ^ Position

    {- Binary operations -}

    | InconsistentTypesBinOp String (String,String) [String] Tk.Position
        -- ^ Operation
        -- ^ Type of Operands
        -- ^ List of correct types
        -- ^ Position of first operand
    | InvalidTypesBinOp String (String, String) [String] Tk.Position
    | InvalidTypeUnOp String String [String] Tk.Position
        -- ^ Operation
        -- ^ Type Of Operand
        -- ^ Correct Types
        -- ^ Position

    | InvalidDereference String Tk.Position

    {- Record Access -}

    | RecordFieldNotFound String Int Tk.Position
    | RepeatedRecordField String Int Tk.Position
    | UnTypedRecordField String Int Tk.Position
    | NotARecordType String Tk.Position
    | NotAnUnion String Tk.Position

    {- Tuple -}

    | NotATupleType String Tk.Position

    {- Id --}

    | IdNotFound String
    | UnTypedId String

    {- Function -}

    | NotAFunction String
    | FunctionWithoutMD String

    {- Cast -}

    | NonCasteableTypes String String Tk.Position

    {- Contextual ones -}

    | InvalidWhileType String Tk.Position 
    | WrongForBoundType String String Tk.Position
    | WrongSwitchType String Tk.Position
    | InvalidIfType String Tk.Position

    | InvalidNew String Tk.Position 
    | InvalidFree String Tk.Position 

    deriving (Show, Eq)
