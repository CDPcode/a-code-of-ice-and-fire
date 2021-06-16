{
module Westeros.SouthOfTheWall.Parser (parse) where
    import qualified Westeros.SouthOfTheWall.Tokens as Tk
    import qualified Westeros.SouthOfTheWall.AST as Ast
}

%name                parse 
%tokentype            { Tk.Token }
%error                { parseError }
-- TODO: %monad expr to properly handle errors

-- Token aliases definitions 
%token

-- Program
{ Tk.Token { Tk.aToken=Tk.TknProgramStart } }
{ Tk.Token { Tk.aToken=Tk.TknProgramName } }

{ Tk.Token { Tk.aToken=Tk.TknVar } }
{ Tk.Token { Tk.aToken=Tk.TknVar } }
{ Tk.Token { Tk.aToken=Tk.TknConst } }
{ Tk.Token { Tk.aToken=Tk.TknVarPointer } }
{ Tk.Token { Tk.aToken=Tk.TknType } }
{ Tk.Token { Tk.aToken=Tk.TknConstValue } }
{ Tk.Token { Tk.aToken=Tk.TknBeginAlias } }
{ Tk.Token { Tk.aToken=Tk.TknStrongAlias } }
{ Tk.Token { Tk.aToken=Tk.TknWeakAlias } }
{ Tk.Token { Tk.aToken=Tk.TknInt } }
{ Tk.Token { Tk.aToken=Tk.TknFloat } }
{ Tk.Token { Tk.aToken=Tk.TknTrilean } }
{ Tk.Token { Tk.aToken=Tk.TknChar } }
{ Tk.Token { Tk.aToken=Tk.TknAtom } }
{ Tk.Token { Tk.aToken=Tk.TknVoid } }
{ Tk.Token { Tk.aToken=Tk.TknTrue } }
{ Tk.Token { Tk.aToken=Tk.TknFalse } }
{ Tk.Token { Tk.aToken=Tk.TknIntLit } }
{ Tk.Token { Tk.aToken=Tk.TknFloatLit } }
{ Tk.Token { Tk.aToken=Tk.TknCharLit } }
{ Tk.Token { Tk.aToken=Tk.TknCharLit } }
{ Tk.Token { Tk.aToken=Tk.TknBeginArrayLit } }
{ Tk.Token { Tk.aToken=Tk.TknEndArrayLit } }
{ Tk.Token { Tk.aToken=Tk.TknBeginTupleLit } }
{ Tk.Token { Tk.aToken=Tk.TknEndTupleLit } }
{ Tk.Token { Tk.aToken=Tk. }
{ Tk.Token { Tk.aToken=Tk.TknNull } }
{ Tk.Token { Tk.aToken=Tk.TknNaturalLit } }
{ Tk.Token { Tk.aToken=Tk.TknBeginCompTypeId } }
{ Tk.Token { Tk.aToken=Tk.TknEndCompTypeId } }
{ Tk.Token { Tk.aToken=Tk.TknBeginArray } }
{ Tk.Token { Tk.aToken=Tk.TknEndArray } }
{ Tk.Token { Tk.aToken=Tk.TknString } }
{ Tk.Token { Tk.aToken=Tk.TknBeginSizes } }
{ Tk.Token { Tk.aToken=Tk.TknEndSizes } }
{ Tk.Token { Tk.aToken=Tk.TknBeginStruct } }
{ Tk.Token { Tk.aToken=Tk.TknEndStruct } }
{ Tk.Token { Tk.aToken=Tk.TknBeginUnion } }
{ Tk.Token { Tk.aToken=Tk.TknEndUnion } }
{ Tk.Token { Tk.aToken=Tk.TknPointerType } }
{ Tk.Token { Tk.aToken=Tk.TknBeginTuple } }
{ Tk.Token { Tk.aToken=Tk.TknEndTuple } }
{ Tk.Token { Tk.aToken=Tk.TknStringLit }  }
{ Tk.Token { Tk.aToken=Tk.TknCast } }
{ Tk.Token { Tk.aToken=Tk.TknAssign } }
{ Tk.Token { Tk.aToken=Tk.TknTupleAssign } }
{ Tk.Token { Tk.aToken=Tk.TknPlus } }
{ Tk.Token { Tk.aToken=Tk.TknMinus } }
{ Tk.Token { Tk.aToken=Tk.TknMult } }
{ Tk.Token { Tk.aToken=Tk.TknDivide } }
{ Tk.Token { Tk.aToken=Tk.TknNegate } }
{ Tk.Token { Tk.aToken=Tk.TknMod } }
{ Tk.Token { Tk.aToken=Tk.TknAnd } }
{ Tk.Token { Tk.aToken=Tk.TknOr } }
{ Tk.Token { Tk.aToken=Tk.TknEqual } }
{ Tk.Token { Tk.aToken=Tk.TknNotEqual } }
{ Tk.Token { Tk.aToken=Tk.TknLessThan } }
{ Tk.Token { Tk.aToken=Tk.TknGreaterThan } }
{ Tk.Token { Tk.aToken=Tk.TknLessEqThan } }
{ Tk.Token { Tk.aToken=Tk.TknGreaterEqThan } }
{ Tk.Token { Tk.aToken=Tk.TknStructField } }
{ Tk.Token { Tk.aToken=Tk.TknUnionQuery } }
{ Tk.Token { Tk.aToken=Tk.TknUnionField } }
{ Tk.Token { Tk.aToken=Tk.TknBeginIndex } }
{ Tk.Token { Tk.aToken=Tk.TknEndIndex } }
{ Tk.Token { Tk.aToken=Tk.TknTupleSelect } }
{ Tk.Token { Tk.aToken=Tk.TknPtr } }
{ Tk.Token { Tk.aToken=Tk.TknDereference } }
{ Tk.Token { Tk.aToken=Tk.TknFree } }
{ Tk.Token { Tk.aToken=Tk.TknBeginExit } }
{ Tk.Token { Tk.aToken=Tk.TknEndExit } }
{ Tk.Token { Tk.aToken=Tk.TknRead } }
{ Tk.Token { Tk.aToken=Tk.TknPrint } }
{ Tk.Token { Tk.aToken=Tk.TknPass } }
{ Tk.Token { Tk.aToken=Tk.TknBeginFuncDecl } }
{ Tk.Token { Tk.aToken=Tk.TknFunctionItem } }
{ Tk.Token { Tk.aToken=Tk.TknGlobalDec } }
{ Tk.Token { Tk.aToken=Tk.TknMain } }
{ Tk.Token { Tk.aToken=Tk.TknBeginFunctionParams }
{ Tk.Token { Tk.aToken=Tk.ctionParams } }
{ Tk.Token { Tk.aToken=Tk.TknBeginReturnVals } }
{ Tk.Token { Tk.aToken=Tk.dReturnVals } }
{ Tk.Token { Tk.aToken=Tk.TknReturnOpen } }
{ Tk.Token { Tk.aToken=Tk.TknReturnClose } }
{ Tk.Token { Tk.aToken=Tk.TknValueArg } }
{ Tk.Token { Tk.aToken=Tk.TknReferenceArg } }
{ Tk.Token { Tk.aToken=Tk.TknOpenBlock } }
{ Tk.Token { Tk.aToken=Tk.TknCloseBlock } }
{ Tk.Token { Tk.aToken=Tk.TknProcCallOpen } }
{ Tk.Token { Tk.aToken=Tk.TknProcCallArgs } }
{ Tk.Token { Tk.aToken=Tk.TknProcCallClose } }
{ Tk.Token { Tk.aToken=Tk.TknFor } }
{ Tk.Token { Tk.aToken=Tk.TknForLB } }
{ Tk.Token { Tk.aToken=Tk.TknForUB } }
{ Tk.Token { Tk.aToken=Tk.TknEndFor } }
{ Tk.Token { Tk.aToken=Tk.TknWhile } }
{ Tk.Token { Tk.aToken=Tk.TknWhileDecorator } }
{ Tk.Token { Tk.aToken=Tk.TknWhileEnd} }
{ Tk.Token { Tk.aToken=Tk.TknContinue } }
{ Tk.Token { Tk.aToken=Tk.TknBreak } }
{ Tk.Token { Tk.aToken=Tk.TknBeginSimpleSelection } }
{ Tk.Token { Tk.aToken=Tk.TknSimpleSelectionDecorator } }
{ Tk.Token { Tk.aToken=Tk.TknElse } }
{ Tk.Token { Tk.aToken=Tk.TknEndSimpleSelection } }
{ Tk.Token { Tk.aToken=Tk.TknBeginMultipleSelection } }
{ Tk.Token { Tk.aToken=Tk.TknMultipleSelectionDecorator } }
{ Tk.Token { Tk.aToken=Tk.TknBranch } }
{ Tk.Token { Tk.aToken=Tk.TknEndMultipleSelection } }
{ Tk.Token { Tk.aToken=Tk.TknID } }
{ Tk.Token { Tk.aToken=Tk.TknArgNumber } }
{ Tk.Token { Tk.aToken=Tk.TknArgNumber } }
{ Tk.Token { Tk.aToken=Tk.TknArgNumber } }
{ Tk.Token { Tk.aToken=Tk.TknArgNumber } }
{ Tk.Token { Tk.aToken=Tk.TknNothing } }
{ Tk.Token { Tk.aToken=Tk.TknAtomLit } }
{ Tk.Token { Tk.aToken=Tk.TknAliasDec } }
{ Tk.Token { Tk.aToken=Tk.TknComma } }
{ Tk.Token { Tk.aToken=Tk.TknDot } }
{ Tk.Token { Tk.aToken=Tk.TknOpenParenthesis } }
{ Tk.Token { Tk.aToken=Tk.TknCloseParenthesis } }
{ Tk.Token { Tk.aToken=Tk.TknOpenParenthesis } }
{ Tk.Token { Tk.aToken=Tk.TknCloseParenthesis } }
{ Tk.Token { Tk.aToken=Tk.TknComment }  }


-- Precedences and Associativities 

%% -- Grammar

-- Program

-- A program is :
-- a header
-- a table of contents
-- a prologue, which is a function definition
-- a (possibly empty) list of function definitions
--      Each function definition holds:
--          a (possibly empty) list of instructions along with selection and repetition
-- an epilogue which is again, a function definition.
-- comments at any point as long as they do not interfere with an expression

-- Program --


-- Subrutines --


-- Types --


-- Instructions --


-- Expressions --


{
	-- Helper functions
-- TODO:
-- Error Function

parseError :: [Tk.Token] -> a
parseError _ = undefined
}