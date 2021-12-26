{
module Westeros.SouthOfTheWall.Parser (parse) where

import qualified Westeros.SouthOfTheWall.AST as AST
import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Types as T
import qualified Westeros.SouthOfTheWall.TypeChecking as TC
import qualified Westeros.SouthOfTheWall.TACGeneration as TAC
import Data.Maybe (fromJust)
import Control.Monad.RWS ( MonadState(put, get), RWST, when, unless )
import Data.List (find, findIndex)


}

%name                 parse
%tokentype            { Tk.Token }
%error                { parseError }
%monad                { ST.MonadParser }

-- Token aliases definitions
%token

-- Program
programStart    { Tk.Token { Tk.aToken=Tk.TknProgramStart } }
programName     { Tk.Token { Tk.aToken=Tk.TknProgramName } }
var             { Tk.Token { Tk.aToken=Tk.TknVar } }
const           { Tk.Token { Tk.aToken=Tk.TknConst } }
pointerVar      { Tk.Token { Tk.aToken=Tk.TknVarPointer } }
type            { Tk.Token { Tk.aToken=Tk.TknType } }
constValue      { Tk.Token { Tk.aToken=Tk.TknConstValue } }
beginAlias      { Tk.Token { Tk.aToken=Tk.TknBeginAlias } }
strongAlias     { Tk.Token { Tk.aToken=Tk.TknStrongAlias } }
weakAlias       { Tk.Token { Tk.aToken=Tk.TknWeakAlias } }
int             { Tk.Token { Tk.aToken=Tk.TknInt } }
float           { Tk.Token { Tk.aToken=Tk.TknFloat } }
bool            { Tk.Token { Tk.aToken=Tk.TknBool } }
char            { Tk.Token { Tk.aToken=Tk.TknChar } }
atom            { Tk.Token { Tk.aToken=Tk.TknAtom } }
void            { Tk.Token { Tk.aToken=Tk.TknVoid } }
true            { Tk.Token { Tk.aToken=Tk.TknTrue } }
false           { Tk.Token { Tk.aToken=Tk.TknFalse } }
intLit          { Tk.Token { Tk.aToken=Tk.TknIntLit } }
floatLit        { Tk.Token { Tk.aToken=Tk.TknFloatLit } }
charLit         { Tk.Token { Tk.aToken=Tk.TknCharLit } }
'{{'            { Tk.Token { Tk.aToken=Tk.TknBeginArrayLit } }
'}}'            { Tk.Token { Tk.aToken=Tk.TknEndArrayLit } }
'[['            { Tk.Token { Tk.aToken=Tk.TknBeginTupleLit } }
']]'            { Tk.Token { Tk.aToken=Tk.TknEndTupleLit } }
null            { Tk.Token { Tk.aToken=Tk.TknNull } }
naturalLit      { Tk.Token { Tk.aToken=Tk.TknNaturalLit } }
beginCompTypeId { Tk.Token { Tk.aToken=Tk.TknBeginCompTypeId } }
endCompTypeId   { Tk.Token { Tk.aToken=Tk.TknEndCompTypeId } }
beginArray      { Tk.Token { Tk.aToken=Tk.TknBeginArray } }
endArray        { Tk.Token { Tk.aToken=Tk.TknEndArray } }
string          { Tk.Token { Tk.aToken=Tk.TknString } }
beginSz         { Tk.Token { Tk.aToken=Tk.TknBeginSizes } }
endSz           { Tk.Token { Tk.aToken=Tk.TknEndSizes } }
beginStruct     { Tk.Token { Tk.aToken=Tk.TknBeginStruct } }
endStruct       { Tk.Token { Tk.aToken=Tk.TknEndStruct } }
beginUnion      { Tk.Token { Tk.aToken=Tk.TknBeginUnion } }
endUnion        { Tk.Token { Tk.aToken=Tk.TknEndUnion } }
pointerType     { Tk.Token { Tk.aToken=Tk.TknPointerType } }
beginTuple      { Tk.Token { Tk.aToken=Tk.TknBeginTuple } }
endTuple        { Tk.Token { Tk.aToken=Tk.TknEndTuple } }
stringLit       { Tk.Token { Tk.aToken=Tk.TknStringLit }  }
cast            { Tk.Token { Tk.aToken=Tk.TknCast } }
':='            { Tk.Token { Tk.aToken=Tk.TknAssign } }
':=='           { Tk.Token { Tk.aToken=Tk.TknTupleAssign } }
'+'             { Tk.Token { Tk.aToken=Tk.TknPlus } }
'-'             { Tk.Token { Tk.aToken=Tk.TknMinus } }
'*'             { Tk.Token { Tk.aToken=Tk.TknMult } }
'/'             { Tk.Token { Tk.aToken=Tk.TknDivide } }
'~'             { Tk.Token { Tk.aToken=Tk.TknNegate } }
'%'             { Tk.Token { Tk.aToken=Tk.TknMod } }
and             { Tk.Token { Tk.aToken=Tk.TknAnd } }
or              { Tk.Token { Tk.aToken=Tk.TknOr } }
not             { Tk.Token { Tk.aToken=Tk.TknNot } }
'='             { Tk.Token { Tk.aToken=Tk.TknEqual } }
'!='            { Tk.Token { Tk.aToken=Tk.TknNotEqual } }
'<'             { Tk.Token { Tk.aToken=Tk.TknLessThan } }
'>'             { Tk.Token { Tk.aToken=Tk.TknGreaterThan } }
'<='            { Tk.Token { Tk.aToken=Tk.TknLessEqThan } }
'>='            { Tk.Token { Tk.aToken=Tk.TknGreaterEqThan } }
'<-'            { Tk.Token { Tk.aToken=Tk.TknStructField } }
'?'             { Tk.Token { Tk.aToken=Tk.TknUnionQuery } }
'->'            { Tk.Token { Tk.aToken=Tk.TknUnionField } }
'['             { Tk.Token { Tk.aToken=Tk.TknBeginIndex } }
']'             { Tk.Token { Tk.aToken=Tk.TknEndIndex } }
'[('            { Tk.Token { Tk.aToken=Tk.TknTupleSelect } }
new             { Tk.Token { Tk.aToken=Tk.TknNew } }
deref           { Tk.Token { Tk.aToken=Tk.TknDereference } }
free            { Tk.Token { Tk.aToken=Tk.TknFree } }
beginExit       { Tk.Token { Tk.aToken=Tk.TknBeginExit } }
endExit         { Tk.Token { Tk.aToken=Tk.TknEndExit } }
read            { Tk.Token { Tk.aToken=Tk.TknRead } }
print           { Tk.Token { Tk.aToken=Tk.TknPrint } }
pass            { Tk.Token { Tk.aToken=Tk.TknPass } }
beginFuncDec    { Tk.Token { Tk.aToken=Tk.TknBeginFuncDecl } }
item            { Tk.Token { Tk.aToken=Tk.TknFunctionItem } }
globalDec       { Tk.Token { Tk.aToken=Tk.TknGlobalDec } }
main            { Tk.Token { Tk.aToken=Tk.TknMain } }
beginFuncParams { Tk.Token { Tk.aToken=Tk.TknBeginFunctionParams } }
endFuncParams   { Tk.Token { Tk.aToken=Tk.TknEndFunctionParams } }
beginReturnVals { Tk.Token { Tk.aToken=Tk.TknBeginReturnVals } }
endReturnVals   { Tk.Token { Tk.aToken=Tk.TknEndReturnVals } }
returnOpen      { Tk.Token { Tk.aToken=Tk.TknReturnOpen } }
returnClose     { Tk.Token { Tk.aToken=Tk.TknReturnClose } }
valueArg        { Tk.Token { Tk.aToken=Tk.TknValueParam } }
refArg          { Tk.Token { Tk.aToken=Tk.TknReferenceParam } }
'{'             { Tk.Token { Tk.aToken=Tk.TknOpenBlock } }
'}'             { Tk.Token { Tk.aToken=Tk.TknCloseBlock } }
'(('            { Tk.Token { Tk.aToken=Tk.TknProcCallOpen } }
procCallArgs    { Tk.Token { Tk.aToken=Tk.TknProcCallArgs } }
'))'            { Tk.Token { Tk.aToken=Tk.TknProcCallClose } }
for             { Tk.Token { Tk.aToken=Tk.TknFor } }
forLB           { Tk.Token { Tk.aToken=Tk.TknForLB } }
forUB           { Tk.Token { Tk.aToken=Tk.TknForUB } }
endFor          { Tk.Token { Tk.aToken=Tk.TknEndFor } }
while           { Tk.Token { Tk.aToken=Tk.TknWhile } }
whileDec        { Tk.Token { Tk.aToken=Tk.TknWhileDecorator } }
endWhile        { Tk.Token { Tk.aToken=Tk.TknEndWhile } }
continue        { Tk.Token { Tk.aToken=Tk.TknContinue } }
break           { Tk.Token { Tk.aToken=Tk.TknBreak } }
if              { Tk.Token { Tk.aToken=Tk.TknBeginSimpleSelection } }
then            { Tk.Token { Tk.aToken=Tk.TknSimpleSelectionDecorator } }
else            { Tk.Token { Tk.aToken=Tk.TknElse } }
endif           { Tk.Token { Tk.aToken=Tk.TknEndSimpleSelection } }
switch          { Tk.Token { Tk.aToken=Tk.TknBeginMultipleSelection } }
switchDec       { Tk.Token { Tk.aToken=Tk.TknMultipleSelectionDecorator } }
case            { Tk.Token { Tk.aToken=Tk.TknBranch } }
endSwitch       { Tk.Token { Tk.aToken=Tk.TknEndMultipleSelection } }
id              { Tk.Token { Tk.aToken=Tk.TknID } }
argNumber       { Tk.Token { Tk.aToken=Tk.TknParamNumber } }
nothing         { Tk.Token { Tk.aToken=Tk.TknNothing } }
atomLit         { Tk.Token { Tk.aToken=Tk.TknAtomLit } }
aliasDec        { Tk.Token { Tk.aToken=Tk.TknAliasDec } }
','             { Tk.Token { Tk.aToken=Tk.TknComma } }
'.'             { Tk.Token { Tk.aToken=Tk.TknDot } }
'('             { Tk.Token { Tk.aToken=Tk.TknOpenParenthesis } }
')'             { Tk.Token { Tk.aToken=Tk.TknCloseParenthesis } }
comment         { Tk.Token { Tk.aToken=Tk.TknComment }  }


-- Precedences and Associativities
%nonassoc ':=' ':=='
%left ','
%left cast
%left and or
%nonassoc '=' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '%' '/'
%right '~'
%left deref
%left ']'
%nonassoc '?'
%left '->'
%right '<-'
%left '('

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
PROGRAM :: { AST.Program }
    : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                         { AST.Program $3 (reverse $4) $5 }
    | ALIASES HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                 { AST.Program $4 (reverse $5) $6 }

HEADER :: { }
    : programStart programName                                                      { }

CONTENTS :: { }
    : beginFuncDec FUNCTION_DECLARATIONS                                            { }

FUNCTION_DECLARATIONS :: { }
    : item globalDec FUNCTION_NAMES item main                                       { }

FUNCTION_NAMES :: { }
    : {- empty -}                                                                   { }
    | FUNCTION_NAMES item id argNumber                                              {% do
                                                                                        let name = Tk.cleanedString $3
                                                                                            params = read (Tk.cleanedString $4) :: Int
                                                                                            pos = Tk.position $3
                                                                                        function <- ST.lookupFunction name params
                                                                                        case function of
                                                                                            Nothing -> ST.insertError $ Err.PE (Err.UndefinedFunction name params pos)
                                                                                            Just info -> if ST.defined $ ST.getFunctionMetaData info
                                                                                                then return ()
                                                                                                else ST.insertError $ Err.PE (Err.UndefinedFunction name params pos)
                                                                                    }

GLOBAL :: { [AST.Instruction] }
    : globalDec '{' DECLARATIONS '}'                                                { reverse $3 }

MAIN :: { [AST.Instruction] }
    : main FUNCTION_BODY                                                            { $2 }

ALIASES :: { }
    : aliasDec ALIAS_DECLARATIONS                                                   { }

ALIAS_DECLARATIONS :: { }
    : ALIAS_DECLARATION                                                             { }
    | ALIAS_DECLARATIONS ALIAS_DECLARATION                                          { }

-- Subrutines --

FUNCTIONS :: { [AST.FunctionDeclaration] }
    : {- empty -}                                                                   { [] }
    | FUNCTIONS FUNCTION                                                            { $2 : $1 }

FUNCTION :: { AST.FunctionDeclaration }
    : FUNCTION_DEF FUNCTION_BODY CLOSE_SCOPE                                        { $2 }

FUNCTION_DEF :: { () }
    : id OPEN_SCOPE FUNCTION_PARAMETERS FUNCTION_RETURN                             {% ST.openFunction (Tk.cleanedString $1) $3 }

FUNCTION_PARAMETERS :: { Int }
    : beginFuncParams PARAMETER_LIST endFuncParams                                  { $2 }

PARAMETER_LIST :: { Int }
    : void                                                                          { 0 }
    | PARAMETERS                                                                    { $1 }

PARAMETERS :: { Int }
    : PARAMETER                                                                     { 1 }
    | PARAMETERS ',' PARAMETER                                                      { $1 + 1 }

PARAMETER :: { () }
    : PARAMETER_TYPE id type TYPE                                                   {% do
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.IdExpr (Tk.cleanedString $2)
                                                                                        TC.checkPrimitiveType expr
                                                                                    }
    | beginCompTypeId PARAMETER_TYPE id endCompTypeId TYPE                          {% do
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr (Tk.cleanedString $3)
                                                                                        TC.checkCompositeType expr
                                                                                    }
    | beginCompTypeId PARAMETER_TYPE pointerVar id endCompTypeId TYPE               {% do
                                                                                        expr <- TC.buildAndCheckExpr $4 $ AST.IdExpr (Tk.cleanedString $4)
                                                                                        TC.checkPointerType expr
                                                                                    }

PARAMETER_TYPE :: { ST.ParameterType }
    : valueArg                                                                      { ST.Value }
    | refArg                                                                        { ST.Reference }

FUNCTION_RETURN :: { () }
    : beginReturnVals RETURN_TYPES endReturnVals                                    {% checkValidReturnTypes $1 $2 }

RETURN_TYPES :: { [ST.Type] }
    : void                                                                          { [] }
    | TYPES                                                                         { reverse $1 }

TYPES :: { [ST.Type] }
    : TYPE                                                                          { [$1] }
    | TYPES ',' TYPE                                                                { $3 : $1 }

FUNCTION_BODY :: { [AST.Instruction] }
    : '{' INSTRUCTIONS '}'                                                          { reverse $2 }

TYPE :: { ST.Type }
    : PRIMITIVE_TYPE                                                                { $1 }
    | COMPOSITE_TYPE                                                                { $1 }
    | id                                                                            {% do
                                                                                        let alias = Tk.cleanedString $1
                                                                                        info <- ST.lookupST alias
                                                                                        -- Already checked in preparser that the symbol exists
                                                                                        case info of
                                                                                            Nothing -> do
                                                                                                return ST.tError
                                                                                            Just info -> do
                                                                                                case ST.additional info of
                                                                                                    Just (ST.AliasMetaData ST.ByName _) -> return alias
                                                                                                    Just (ST.AliasMetaData ST.ByStructure tp) -> return tp
                                                                                                    _ -> return ST.tError
                                                                                            _ -> return ST.tError
                                                                                    }

PRIMITIVE_TYPE :: { ST.Type }
    : int                                                                           { ST.int }
    | float                                                                         { ST.float }
    | char                                                                          { ST.char }
    | bool                                                                          { ST.bool }
    | atom                                                                          { ST.atom }

COMPOSITE_TYPE :: { ST.Type }
    : beginArray naturalLit TYPE endArray                                           {% ST.genTypeSymbol }
    | string                                                                        {% ST.genTypeSymbol }
    | pointerType TYPE                                                              {% ST.genTypeSymbol }
    | beginStruct BEGIN_RECORD SIMPLE_DECLARATIONS END_RECORD endStruct             {% ST.genTypeSymbol }
    | beginUnion BEGIN_RECORD SIMPLE_DECLARATIONS END_RECORD endUnion               {% ST.genTypeSymbol }
    | beginTuple TUPLE_TYPES endTuple                                               {% ST.genTypeSymbol }

BEGIN_RECORD :: { ST.Scope }
    : OPEN_SCOPE                                                                    {% do
                                                                                        ST.openRecord
                                                                                        return $1
                                                                                    }

END_RECORD :: { () }
    : CLOSE_SCOPE                                                                   {% ST.closeRecord }

TUPLE_TYPES :: { }
    : {- empty -}                                                                   { }
    | TYPES                                                                         { }

DECLARATIONS :: { [AST.Instruction] }
    : {- empty -}                                                                   { [] }
    | DECLARATIONS DECLARATION                                                      { case $2 of
                                                                                        Nothing -> $1
                                                                                        Just inst -> inst : $1 }
    | DECLARATIONS comment                                                          { $1 }

DECLARATION :: { Maybe AST.Instruction }
    : SIMPLE_DECLARATION '.'                                                        { Nothing }
    | SIMPLE_DECLARATION ':=' EXPR '.'                                              {% do
                                                                                        checkAssignment $2 [$1] $3 True
                                                                                        return $ Just (AST.SimpleAssign $1 $3)
                                                                                    }
    | SIMPLE_DECLARATION ':==' EXPR '.'                                             {% do
                                                                                        checkAssignment $2 [$1] $3 True
                                                                                        return $ Just (AST.SimpleAssign $1 $3)
                                                                                    }
    | CONST_DECLARATION '.'                                                         {% return $ Just $1 }

SIMPLE_DECLARATIONS :: { }
    : SIMPLE_DECLARATION                                                            {  }
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                    {  }

SIMPLE_DECLARATION :: { AST.Expression }
    : PRIMITIVE_DECLARATION                                                         { $1 }
    | COMPOSITE_DECLARATION                                                         { $1 }

PRIMITIVE_DECLARATION :: { AST.Expression }
    : var id type TYPE                                                              {% do
                                                                                        countOpenRecords <- ST.currentOpenRecords
                                                                                        unless (countOpenRecords > 0) $ do
                                                                                            ST.insertId $2 ST.Variable $4 Nothing
                                                                                        let symbol = Tk.cleanedString $2
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.IdExpr symbol
                                                                                        TC.checkPrimitiveType expr
                                                                                        return expr
                                                                                    }

COMPOSITE_DECLARATION :: { AST.Expression }
    : beginCompTypeId var id endCompTypeId TYPE                                     {% do
                                                                                        countOpenRecords <- ST.currentOpenRecords
                                                                                        unless (countOpenRecords > 0) $ do
                                                                                            ST.insertId $3 ST.Variable $5 Nothing
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkCompositeType expr
                                                                                        return expr
                                                                                    }
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz              {% do
                                                                                        countOpenRecords <- ST.currentOpenRecords
                                                                                        unless (countOpenRecords > 0) $ do
                                                                                            ST.insertId $3 ST.Variable $5 Nothing
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkArrayType expr
                                                                                        TC.checkIntegerTypes $7
                                                                                        return expr
                                                                                    }
    | beginCompTypeId pointerVar id endCompTypeId TYPE                              {% do
                                                                                        countOpenRecords <- ST.currentOpenRecords
                                                                                        unless (countOpenRecords > 0) $ do
                                                                                            ST.insertId $3 ST.Variable $5 Nothing
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkPointerType expr
                                                                                        return expr
                                                                                    }
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz       {% do
                                                                                        countOpenRecords <- ST.currentOpenRecords
                                                                                        unless (countOpenRecords > 0) $ do
                                                                                            ST.insertId $3 ST.Variable $5 Nothing
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkPointerToArrayType expr
                                                                                        TC.checkIntegerTypes $7
                                                                                        return expr
                                                                                    }

CONST_DECLARATION :: { AST.Instruction }
    : const id type TYPE constValue EXPR                                            {% do
                                                                                        ST.insertId $3 ST.Constant $4 Nothing
                                                                                        let symbol = Tk.cleanedString $2
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.IdExpr symbol
                                                                                        checkAssignment $5 [expr] $6 True
                                                                                        TC.checkPrimitiveType expr
                                                                                        return $ AST.SimpleAssign expr $6
                                                                                    }
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                   {% do
                                                                                        ST.insertId $3 ST.Constant $5 Nothing
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        checkAssignment $6 [expr] $7 True
                                                                                        TC.checkCompositeType expr
                                                                                        return $ AST.SimpleAssign expr $7
                                                                                    }

ALIAS_DECLARATION :: { }
    : beginAlias id ALIAS_TYPE TYPE '.'                                             { }

ALIAS_TYPE :: { }
    : strongAlias                                                                   { }
    | weakAlias                                                                     { }

-- Instructions --

CODE_BLOCK :: { [AST.Instruction] }
    : OPEN_SCOPE INSTRUCTIONS CLOSE_SCOPE                                           { $2 }

INSTRUCTIONS :: { [AST.Instruction] }
    : {- empty -}                                                                   { [] }
    | INSTRUCTIONS INSTRUCTION                                                      { $2 : $1 }
    | INSTRUCTIONS comment                                                          { $1 }

INSTRUCTION :: { AST.Instruction }
    : EXPR ':=' EXPR '.'                                                            {% do
                                                                                        checkAssignment $2 [$1] $3 False
                                                                                        return $ AST.SimpleAssign $1 $3
                                                                                    }
    | EXPR ':==' EXPR '.'                                                           {% do
                                                                                        checkAssignment $2 [$1] $3 False
                                                                                        return $ AST.SimpleAssign $1 $3
                                                                                    }
    | EXPRLIST ':==' EXPR '.'                                                       {% do
                                                                                        let exprList = reverse $1
                                                                                        checkAssignment $2 exprList $3 False
                                                                                        return $ AST.MultAssign exprList $3
                                                                                    }
    | void ':=' FUNCTIONCALL '.'                                                    {% do
                                                                                        checkFunctionCallInstr $3
                                                                                        return $ AST.FuncCallInst $3
                                                                                    }
    | void ':==' FUNCTIONCALL '.'                                                   {% do
                                                                                        checkFunctionCallInstr $3
                                                                                        return $ AST.FuncCallInst $3
                                                                                    }
    | pass '.'                                                                      {% return AST.EmptyInst }
    | beginExit programName endExit '.'                                             {% return $ AST.ExitInst (Tk.cleanedString $3) }
    | read EXPR '.'                                                                 {% do
                                                                                        if AST.isValidLValue $2
                                                                                            then do
                                                                                                let toReadTp = AST.getType $2
                                                                                                unless (T.isPrimitiveType toReadTp || T.isStringType toReadTp) $ do
                                                                                                    let err = Err.InvalidExprType (show $ AST.getType $2) (Tk.position $1)
                                                                                                    ST.insertError $ Err.TE err
                                                                                                else do
                                                                                                    let err = Err.InvalidLValue (Tk.position $ AST.getToken $2)
                                                                                                    ST.insertError $ Err.TE err
                                                                                        return $ AST.Read $2
                                                                                    }
    | print EXPR '.'                                                                {% do

                                                                                        let toPrintTp = AST.getType $2
                                                                                        unless (T.isPrimitiveType toPrintTp || T.isStringType toPrintTp) $ do
                                                                                            let err = Err.InvalidExprType (show $ AST.getType $2) (Tk.position $1)
                                                                                            ST.insertError $ Err.TE err
                                                                                        return $ AST.Print $2
                                                                                    }
    | EXPR new '.'                                                                  {% do
                                                                                        let ptrType = AST.getType $1
                                                                                        case ptrType of
                                                                                            T.PointerT _ -> do checkValidLValue $1
                                                                                            _            -> do
                                                                                                let error = Err.UnexpectedType (show ptrType) (show Err.Pointer) (Tk.position $2)
                                                                                                ST.insertError $ Err.TE error
                                                                                        return $ AST.New $1
                                                                                    }
    | EXPR free '.'                                                                 {% do
                                                                                        let ptrType = AST.getType $1

                                                                                        case ptrType of
                                                                                            T.PointerT _ -> do checkValidLValue $1
                                                                                            _            -> do
                                                                                                let error = Err.UnexpectedType (show ptrType) (show Err.Pointer) (Tk.position $2)
                                                                                                ST.insertError $ Err.TE error

                                                                                        return $ AST.Free $1
                                                                                    }
    | continue '.'                                                                  {% do
                                                                                        openLoops <- ST.currentOpenLoops
                                                                                        when (openLoops == 0) $ do
                                                                                            ST.insertError $ Err.PE $ Err.NoLoop (Tk.position $1)
                                                                                        return $ AST.Continue
                                                                                    }
    | break '.'                                                                     {% do
                                                                                        openLoops <- ST.currentOpenLoops
                                                                                        when (openLoops == 0) $ do
                                                                                            ST.insertError $ Err.PE $ Err.NoLoop (Tk.position $1)
                                                                                        return $ AST.Break
                                                                                    }
    | returnOpen EXPRLIST returnClose                                               {% do
                                                                                        let returns = reverse $2
                                                                                        checkValidReturn $1 returns
                                                                                        return $ AST.Return returns
                                                                                    }
    | returnOpen returnClose                                                        {% do
                                                                                        checkValidReturn $1 []
                                                                                        return $ AST.Return []
                                                                                    }
    | IF '.'                                                                        {% return $ AST.If $1 }
    | SWITCHCASE '.'                                                                {% return $1 }
    | FOR '.'                                                                       {% return $1 }
    | WHILE '.'                                                                     {% return $1 }
    | DECLARATION                                                                   {% case $1 of
                                                                                        Nothing -> return AST.EmptyInst
                                                                                        Just inst -> return inst
                                                                                    }

IF :: { AST.IfInst }
    : if EXPR then CODE_BLOCK endif                                                 {% do
                                                                                        let exprType = AST.getType $2
                                                                                        unless (exprType `elem` [T.BoolT, T.TypeError]) $ do
                                                                                            let exprType = AST.getType $2
                                                                                                err = Err.UnexpectedType (show exprType) (show Err.Bool) (Tk.position $ AST.getToken $2)
                                                                                            ST.insertError $ Err.TE err

                                                                                        return $ AST.IfThen $2 (reverse $4)
                                                                                    }
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif                                 {% do

                                                                                        let exprType = AST.getType $2
                                                                                        unless (exprType `elem` [T.BoolT, T.TypeError]) $ do
                                                                                            let exprType = AST.getType $2
                                                                                                err = Err.UnexpectedType (show exprType) (show Err.Bool) (Tk.position $ AST.getToken $2)
                                                                                            ST.insertError $ Err.TE err
                                                                                        return $ AST.IfThenElse $2 (reverse $4) (reverse $6)
                                                                                    }

SWITCHCASE :: { AST.Instruction }
    : switch EXPR switchDec '.' CASES endSwitch                                     {% do
                                                                                        let exprType = AST.getType $2
                                                                                        unless (exprType `elem` [T.AtomT, T.TypeError]) $ do
                                                                                            let err = Err.UnexpectedType (show exprType) (show Err.Atom) (Tk.position $ AST.getToken $2)
                                                                                            ST.insertError $ Err.TE err
                                                                                        return $ AST.Switch $2 (reverse $5)
                                                                                    }

CASES :: { [AST.Case] }
    : CASE                                                                          { [$1] }
    | CASES CASE                                                                    { $2 : $1 }

CASE :: { AST.Case }
    : case atomLit '.' CODE_BLOCK                                                   {% do
                                                                                        atom <- ST.getAtomNumber $ Tk.cleanedString $2
                                                                                        return AST.Case atom (reverse $4)
                                                                                    }
    | case nothing '.' CODE_BLOCK                                                   {% return $ AST.Default (reverse $4) }

FOR :: { AST.Instruction }
    : OPEN_SCOPE OPEN_LOOP FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE CLOSE_LOOP       { let (id, lb, ub) = $3 in AST.For id lb ub (reverse $4) }

FOR_DEC :: { (AST.Id, AST.Expression, AST.Expression) }
    : for id type int '.' forLB EXPR forUB EXPR '.'                                 {% do

                                                                                        ST.insertId $2 ST.Variable ST.int Nothing
                                                                                        TC.checkIntegerType $7
                                                                                        TC.checkIntegerType $9
                                                                                        return (Tk.cleanedString $2, $7, $9)
                                                                                    }

WHILE :: { AST.Instruction }
    : while EXPR whileDec OPEN_LOOP CODE_BLOCK CLOSE_LOOP endWhile                  {% do
                                                                                        let exprType = AST.getType $2
                                                                                        unless (elem exprType [T.BoolT, T.TypeError]) $ do
                                                                                                let error = Err.UnexpectedType (show exprType) (show Err.Bool) (Tk.position $ AST.getToken $2)
                                                                                                ST.insertError $ Err.TE error
                                                                                        return $ AST.While $2 (reverse $5)
                                                                                    }

-- Expresions --
EXPR :: { TAC.Expression }
    : EXPR '+' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Sum astExpr1 astExpr2
                                                                                        TAC.generateCodeArithmeticBin astExpr $1 $3
                                                                                    }
    | EXPR '-' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Sub astExpr1 astExpr2
                                                                                        TAC.generateCodeArithmeticBin astExpr $1 $3
                                                                                    }
    | EXPR '*' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Prod astExpr1 astExpr2
                                                                                        TAC.generateCodeArithmeticBin astExpr $1 $3
                                                                                    }
    | EXPR '/' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Div astExpr1 astExpr2
                                                                                        TAC.generateCodeArithmeticBin astExpr $1 $3
                                                                                    }
    | EXPR '%' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Mod astExpr1 astExpr2
                                                                                        TAC.generateCodeArithmeticBin astExpr $1 $3
                                                                                    }
    | EXPR '=' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Eq astExpr1 astExpr2
                                                                                        TAC.generateCodeComparison astExpr $1 $3
                                                                                    }
    | EXPR '!=' EXPR                                                                {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Neq astExpr1 astExpr2
                                                                                        TAC.generateCodeComparison astExpr $1 $3
                                                                                    }
    | EXPR '<' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Lt astExpr1 astExpr2
                                                                                        TAC.generateCodeComparison astExpr $1 $3
                                                                                    }
    | EXPR '>' EXPR                                                                 {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Lt astExpr1 astExpr2
                                                                                        TAC.generateCodeComparison astExpr $1 $3
                                                                                    }
    | EXPR '<=' EXPR                                                                {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Leq astExpr1 astExpr2
                                                                                        TAC.generateCodeComparison astExpr $1 $3
                                                                                    }
    | EXPR '>=' EXPR                                                                {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $3
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Geq astExpr1 astExpr2
                                                                                        TAC.generateCodeComparison astExpr $1 $3
                                                                                    }
    | EXPR and GEN_LABEL EXPR                                                       {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $4
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.And astExpr1 astExpr2
                                                                                        TAC.generateCodeLogicalAnd astExpr $1 $4 $3
                                                                                    }
    | EXPR or GEN_LABEL EXPR                                                        {% do
                                                                                        let astExpr1 = TAC.getExpr $1
                                                                                            astExpr2 = TAC.getExpr $4
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.BinOp AST.Or astExpr1 astExpr2
                                                                                        TAC.generateCodeLogicalOr astExpr $1 $4 $3
                                                                                    }
    | not EXPR                                                                      {% do
                                                                                        let expr = TAC.getExpr $2
                                                                                        astExpr <- TC.buildAndCheckExpr $1 $ AST.UnOp AST.Not expr
                                                                                        TAC.generateCodeLogicalNot astExpr $2
                                                                                    }
    | EXPR '~'                                                                      {% do
                                                                                        let expr = TAC.getExpr $1
                                                                                        astExpr <- TC.buildAndCheckExpr $2 $ AST.UnOp AST.Neg Expr
                                                                                        TAC.generateCodeArithmeticUnary astExpr $1
                                                                                    }
    | deref EXPR                                                                    {% TC.buildAndCheckExpr $1 $ AST.UnOp AST.Deref $2 }
    | '[' EXPRLIST ']' EXPR                                                         {% TC.buildAndCheckExpr $3 $ AST.AccesIndex $4 (reverse $2) }
    | id '<-' EXPR                                                                  {% do
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.AccesField $3 (Tk.cleanedString $1)
                                                                                        case AST.getType $3 of
                                                                                            T.StructT _ _ -> return ()
                                                                                            T.TypeError -> return ()
                                                                                            _           -> do
                                                                                                let error = Err.InvalidExprType (show $ AST.getType $3) (Tk.position $2)
                                                                                                ST.insertError $ Err.TE error
                                                                                        return expr
                                                                                    }
    | EXPR '->' id                                                                  {% do
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.AccesField $1 (Tk.cleanedString $3)
                                                                                        case AST.getType $1 of
                                                                                                T.UnionT _ _ -> return ()
                                                                                                T.TypeError -> return ()
                                                                                                _           -> do
                                                                                                    let error = Err.InvalidExprType (show $ AST.getType $1) (Tk.position $2)
                                                                                                    ST.insertError $ Err.TE error
                                                                                        return expr
                                                                                    }
    | EXPR '?' id                                                                   {% TC.buildAndCheckExpr $2 $ AST.ActiveField $1 (Tk.cleanedString $3) }
    | '[(' naturalLit ']' EXPR                                                      {% TC.buildAndCheckExpr $3 $ AST.TupleIndex $4 ((read $ Tk.cleanedString $2) :: Int) }
    | EXPR cast TYPE                                                                {% TC.buildAndCheckExpr $2 $ AST.Cast $1 $3 }
    | '(' EXPR ')'                                                                  { $2 }
    | ARRAYLIT                                                                      { $1 }
    | TUPLELIT                                                                      { $1 }
    | FUNCTIONCALL                                                                  {% do
                                                                                        checkFunctionCallInstr $1
                                                                                        return $1
                                                                                    }
    | intLit                                                                        {% do
                                                                                        let expr = AST.IntLit ((read $ Tk.cleanedString $1) :: Int)
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TAC.generateCodeLiteral astExpr
                                                                                    }
    | floatLit                                                                      {% do
                                                                                        let expr = AST.FloatLit ((read $ Tk.cleanedString $1) :: Float)
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TAC.generateCodeLiteral astExpr
                                                                                    }
    | charLit                                                                       {% do
                                                                                        let expr =  AST.CharLit $ head $ Tk.cleanedString $1
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TAC.generateCodeLiteral astExpr
                                                                                    }
    | atomLit                                                                       {% do
                                                                                        atom <- ST.getAtomNumber $ Tk.cleanedString $1
                                                                                        let expr = AST.AtomLit atom
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TAC.generateCodeLiteral astExpr
                                                                                    }
    | stringLit                                                                     {% TC.buildAndCheckExpr $1 $ AST.StringLit $ Tk.cleanedString $1 }
    | true                                                                          {% do
                                                                                        let expr = AST.TrueLit
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TC.generateCodeLiteral astExpr
                                                                                    }
    | false                                                                         {% do
                                                                                        let expr = AST.FalseLit
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TC.generateCodeLiteral astExpr
                                                                                    }
    | null                                                                          {% do
                                                                                        let expr = AST.NullLit
                                                                                        astExpr <- TC.buildAndCheckExpr $1 expr
                                                                                        TC.generateCodeLiteral astExpr
                                                                                    }
    | id                                                                            {% do
                                                                                        let symbol = Tk.cleanedString $1
                                                                                        astExpr <- TC.buildAndCheckExpr $1 $ AST.IdExpr symbol
                                                                                        maybeEntry <- ST.lookupST symbol
                                                                                        maybeOffset <- case maybeEntry of
                                                                                            Just ST.SymbolInfo{ST.category=category, ST.symbolType=tp, ST.offset=mOffset} -> do
                                                                                                if category `elem` [ST.Variable, ST.Constant, ST.Parameter]
                                                                                                    then return mOffset
                                                                                                    else do
                                                                                                        let err = Err.UndeclaredName symbol (Tk.position $1)
                                                                                                        ST.insertError $ Err.PE err
                                                                                                        return Nohting
                                                                                            Nothing -> do
                                                                                                let err = Err.UndeclaredName symbol (Tk.position $1)
                                                                                                ST.insertError $ Err.PE err
                                                                                                return Nohting
                                                                                        let offset = maybe 0 id maybeOffset
                                                                                        TAC.generateCodeId astExpr offset
                                                                                    }

FUNCTIONCALL :: { AST.Expression }
    : id '((' procCallArgs EXPRLIST '))'                                            {% TC.buildAndCheckExpr $1 $ AST.FuncCall (Tk.cleanedString $1) (reverse $4) }
    | id '((' procCallArgs void '))'                                                {% TC.buildAndCheckExpr $1 $ AST.FuncCall (Tk.cleanedString $1) [] }
    | id '(('  '))'                                                                 {% TC.buildAndCheckExpr $1 $ AST.FuncCall (Tk.cleanedString $1) [] }

ARRAYLIT :: { AST.Expression }
    : '{{' EXPRLIST '}}'                                                            {% TC.buildAndCheckExpr $1 $ AST.ArrayLit $ reverse $2 }
    | '{{' '}}'                                                                     {% TC.buildAndCheckExpr $1 $ AST.ArrayLit [] }

TUPLELIT :: { AST.Expression }
    : '[[' EXPRLIST ']]'                                                            {% TC.buildAndCheckExpr $1 $ AST.TupleLit $ reverse $2 }
    | '[[' ']]'                                                                     {% TC.buildAndCheckExpr $1 $ AST.TupleLit [] }

EXPRLIST :: { [AST.Expression] }
    : EXPR                                                                          { [$1] }
    | EXPRLIST ',' EXPR                                                             { $3 : $1 }

GEN_LABEL :: { TAC.Label }
    : {- empty -}                                                                   {% TAC.generateLabel }

OPEN_SCOPE :: { Int }
    :  {- empty -}                                                                  {% do
                                                                                        ST.openScope
                                                                                        ST.currentScope
                                                                                    }

CLOSE_SCOPE :: { () }
    :  {- empty -}                                                                  {% ST.closeScope }

OPEN_LOOP :: { () }
    :  {- empty -}                                                                  {% ST.openLoop}

CLOSE_LOOP :: { () }
    :  {- empty -}                                                                  {% ST.closeLoop }


{
parseError :: [Tk.Token] -> ST.MonadParser a
parseError []     = do
    ST.insertError $ Err.PE   Err.SyntaxErrEOF
    fail "The scriptures do not follow the desired structure"
parseError (tk:_) = do
    ST.insertError $ Err.PE $ Err.SyntaxErr (Tk.cleanedString tk) (Tk.position tk)
    fail "The scriptures do not follow the desired structure"

checkFunctionCallInstr :: AST.Expression -> ST.MonadParser ()
checkFunctionCallInstr AST.Expression{AST.getExpr=(AST.FuncCall id args), AST.getToken=tk} = do
    entry <- ST.lookupFunction id $ length args
    case entry of
        Just _ -> return ()
        Nothing -> do
            let err = Err.UndeclaredFunction id (length args) (Tk.position tk)
            ST.insertError $ Err.PE err
checkFunctionCallInstr _ = return ()

checkAssignment :: Tk.Token -> [AST.Expression] -> AST.Expression -> Bool -> ST.MonadParser ()
checkAssignment tk lValues rValue isInit = do


    unless ((length lValues == 1) && isInit) $ do
        mapM_ (checkConstantReassignment tk) lValues

    mapM_ checkValidLValue lValues

    case lValues of
        [lValue] -> do
            let lType = AST.getType lValue
                rType = AST.getType rValue

            unless (T.checkAssignable lType rType) $ do
                ST.insertError $ Err.TE $ Err.UnexpectedType (show lType) (show rType) (Tk.position tk)
        _ -> do
                let rType = AST.getType rValue
                case rType of
                    T.TupleT rTypes  -> checkTypes rTypes
                    T.MultiReturnT rTypes -> checkTypes rTypes
                    T.TypeError -> return ()
                    _ -> ST.insertError $ Err.TE $ Err.InvalidExprType (show rType) (Tk.position tk)
    where checkTypes rTypes = do
            let lTypes = map AST.getType lValues
            if (length lTypes == length rTypes)
                then do
                    let matches = zipWith T.checkAssignable lTypes rTypes
                    case findIndex not matches of
                        Nothing -> return ()
                        Just pos -> do
                            let err = Err.UnexpectedType (show $ lTypes !! pos) (show $ rTypes !! pos) (Tk.position tk)
                            ST.insertError $ Err.TE err
                else ST.insertError $ Err.PE $ Err.MultiAssignmentLengthMissmatch (length lTypes) (length rTypes) (Tk.position tk)


checkConstantReassignment :: Tk.Token -> AST.Expression -> ST.MonadParser ()
checkConstantReassignment tk  AST.Expression{AST.getExpr=(AST.IdExpr id)} = do
    maybeSymbol <- ST.lookupST id
    case maybeSymbol of
        Just (ST.SymbolInfo{ST.category=ST.Constant}) ->
            ST.insertError $ Err.TE $ Err.ConstantReassignment (Tk.position $ tk)
        _ -> return ()
checkConstantReassignment _ _ = return ()

checkValidLValue :: AST.Expression -> ST.MonadParser ()
checkValidLValue maybeLVal = do

    unless (AST.isValidLValue maybeLVal) $ do
        let exprToken = AST.getToken maybeLVal
        ST.insertError $ Err.TE $ Err.InvalidLValue (Tk.position exprToken)

-- We are returning the right type for the current open function
checkValidReturn :: Tk.Token -> [AST.Expression] -> ST.MonadParser ()
checkValidReturn statement exprs = do
    (fn, params) <- ST.currentOpenFunction
    maybeEntry <- ST.lookupFunction fn params
    case maybeEntry of
        Just ST.SymbolInfo{
            ST.additional = (Just (ST.FunctionMetaData ST.FunctionInfo{ST.returns=returns}))
        } -> do
            let exprsTypes = map AST.getType exprs
            returnTypes <- mapM T.getTypeFromString returns
            if all T.notTypeError (returnTypes ++ exprsTypes)
                then do
                    if length returnTypes == length exprsTypes
                        then do
                            let matches = zipWith T.checkAssignable returnTypes exprsTypes
                            case findIndex not matches of
                                Nothing -> return ()
                                Just pos -> do
                                    let err = Err.UnexpectedType (show $ exprsTypes !! pos) (show $ returnTypes !! pos) (Tk.position statement)
                                    ST.insertError $ Err.TE err
                        else do
                            ST.insertError $ Err.PE $ Err.ReturnLengthMissmatch fn params (length returnTypes) (length exprsTypes) (Tk.position statement)
                else return ()
        _ -> return ()

-- We can only return primitive or pointers
checkValidReturnTypes :: Tk.Token -> [ST.Type] -> ST.MonadParser ()
checkValidReturnTypes tk types = do
    abstTypes <- mapM T.getTypeFromString types
    case findIndex (\t -> not $ T.isPrimitiveOrPointerType t) abstTypes of
        Nothing -> return ()
        Just pos ->
            ST.insertError $ Err.TE $ Err.InvalidExprType (show $ abstTypes !! pos) (Tk.position tk)
}
