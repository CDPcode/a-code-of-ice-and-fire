{
module Westeros.SouthOfTheWall.Parser (parse) where

import qualified Westeros.SouthOfTheWall.AST as AST
import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.TypeVer as T
import Data.Maybe (fromJust)
import Control.Monad.RWS ( MonadState(put, get), RWST, when, unless )

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
                                                                                            Nothing -> ST.insertError $ Err.PE (Err.UndefinedFunction name pos)
                                                                                            Just info -> if ST.defined $ ST.getFunctionMetaData info
                                                                                                then return ()
                                                                                                else ST.insertError $ Err.PE (Err.UndefinedFunction name pos)
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
    : FUNCTION_DEF FUNCTION_BODY CLOSE_SCOPE                                        { $5 }

FUNCTION_DEF :: { () }
    : id OPEN_SCOPE FUNCTION_PARAMETERS FUNCTION_RETURN                             {% do
                                                                                        ST.openFunction (Tk.cleanedString $1) (length $3)
                                                                                        -- PQC
                                                                                        -- En el preparser, la funcion
                                                                                        -- ST.updateFunctionInfo potencialmente crea
                                                                                        -- una entrada en la tabla de simbolos para el tipo
                                                                                        -- del retorno de la funcion si este es un retorno multivalor
                                                                                        when ($4 > 1) $ do
                                                                                            ST.genTypeSymbol
                                                                                            return ()
                                                                                    }


FUNCTION_PARAMETERS :: { }
    : beginFuncParams PARAMETER_LIST endFuncParams                                  { }

PARAMETER_LIST :: { }
    : void                                                                          { }
    | PARAMETERS                                                                    { }

PARAMETERS :: { }
    : PARAMETER                                                                     { }
    | PARAMETERS ',' PARAMETER                                                      { }

PARAMETER :: { () }
    : PARAMETER_TYPE id type TYPE                                                   {% ST.insertParam $2 $4 $1 }

PARAMETER_TYPE :: { ST.ParameterType }
    : valueArg                                                                      { ST.Value }
    | refArg                                                                        { ST.Reference }

FUNCTION_RETURN :: { Int }
    : beginReturnVals RETURN_TYPES endReturnVals                                    { $2 }

RETURN_TYPES :: { Int }
    : void                                                                          { 0 }
    | TYPES                                                                         { length $1 }

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
                                                                                        maybeInfo <- ST.lookupST alias
                                                                                        case maybeInfo of
                                                                                            Nothing -> do
                                                                                                let err = Err.IdNotFound alias
                                                                                                ST.insertError $ Err.TE err
                                                                                                return ST.tError
                                                                                            Just info -> do
                                                                                                case ST.additional info of
                                                                                                    Just (ST.AliasMetaData ST.ByName _) ->
                                                                                                        return alias
                                                                                                    Just (ST.AliasMetaData ST.ByStructure tp) ->
                                                                                                        return tp
                                                                                                    _ ->  do
                                                                                                        let err = Err.IdNotFound alias
                                                                                                        ST.insertError $ Err.TE err
                                                                                                        return ST.tError
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
    | beginStruct OPEN_SCOPE SIMPLE_DECLARATIONS CLOSE_SCOPE endStruct              {% ST.genTypeSymbol }
    | beginUnion OPEN_SCOPE SIMPLE_DECLARATIONS CLOSE_SCOPE endUnion                {% ST.genTypeSymbol }
    | beginTuple TUPLE_TYPES endTuple                                               {% ST.genTypeSymbol }

TUPLE_TYPES :: { }
    : {- empty -}                                                                   { }
    | TYPES                                                                         { }

-- Alias Declaration --
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
                                                                                        return $ Just $ AST.SimpleAssign $1 $3
                                                                                    }
    | SIMPLE_DECLARATION ':==' EXPR '.'                                             {% do
                                                                                        checkAssignment $2 [$1] $3 True
                                                                                        return $ Just $ AST.SimpleAssign $1 $3
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
                                                                                        ST.insertId $2 ST.Variable $4
                                                                                        let symbol = Tk.cleanedString $2
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.IdExpr symbol
                                                                                        TC.checkPrimitiveType expr
                                                                                        return expr
                                                                                    }

COMPOSITE_DECLARATION :: { AST.Expression }
    : beginCompTypeId var id endCompTypeId TYPE                                     {% do
                                                                                        ST.insertId $3 ST.Variable $5
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkRecordOrTupleType expr
                                                                                        return expr
                                                                                    }
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz              {% do
                                                                                        ST.insertId $3 ST.Variable $5
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkArrayType expr
                                                                                        TC.checkIntegerTypes $7
                                                                                        return expr
                                                                                    }
    | beginCompTypeId pointerVar id endCompTypeId TYPE                              {% do
                                                                                        ST.insertId $3 ST.Variable $5
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkPointerType expr
                                                                                        TC.checkPointerToRecordOrTupleType expr
                                                                                        return expr
                                                                                    }
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz       {% do
                                                                                        ST.insertId $3 ST.Variable $5
                                                                                        let symbol = Tk.cleanedString $3
                                                                                        expr <- TC.buildAndCheckExpr $3 $ AST.IdExpr symbol
                                                                                        TC.checkPointerToArrayType expr
                                                                                        TC.checkIntegerTypes $7
                                                                                        return expr
                                                                                    }

CONST_DECLARATION :: { AST.Instruction }
    : const id type TYPE constValue EXPR                                            {% do
                                                                                        ST.insertId $3 ST.Constant $4
                                                                                        let symbol = Tk.cleanedString $2
                                                                                        expr <- TC.buildAndCheckExpr $2 $ AST.IdExpr symbol
                                                                                        checkAssignment $5 [expr] $6 True
                                                                                        TC.checkPrimitiveType expr
                                                                                        return $ AST.SimpleAssign expr $6
                                                                                    }
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                   {% do
                                                                                        ST.insertId $3 ST.Constant $5
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
    | void ':=' EXPR '.'                                                            {% checkFunctionCall $2 $3 }
    | void ':==' EXPR '.'                                                           {% checkFunctionCall $2 $3 }
    | pass '.'                                                                      {% return AST.EmptyInst }
    | beginExit programName endExit '.'                                             {% return $ AST.ExitInst (Tk.cleanedString $3) }
    | read EXPR '.'                                                                 {% do
                                                                                        let toReadTp = AST.getType $2
                                                                                        unless (T.isPrimitiveType toPrintTp || T.isStringType toPrintTp) $ do
                                                                                            let err = Err.NonReadable (show toPrintTp) (Tk.position $1)
                                                                                            ST.insertError $ Err.TE err

                                                                                        return $ AST.Read $2

                                                                                    }
    | print EXPR '.'                                                                {% do
                                                                                        let toPrintTp = AST.getType $2
                                                                                        unless (T.isPrimitiveType toPrintTp || T.isStringType toPrintTp) $ do
                                                                                            let err = Err.NonPrintable (show toPrintTp) (Tk.position $1)
                                                                                            ST.insertError $ Err.TE err

                                                                                        return $ AST.Print $2
                                                                                    }
    | EXPR new '.'                                                                  {% do
                                                                                        let ptrType = AST.getType $1
                                                                                        case ptrType of
                                                                                            T.PointerT _ -> do
                                                                                                unless (AST.isValidLValue $1) $ do
                                                                                                    ST.insertError $ Err.TE $ Err.InvalidLValue (Tk.cleanedString $2) (Tk.position $2)
                                                                                            _            -> do
                                                                                                let error = Err.InvalidNew (show ptrType) (Tk.position $2)
                                                                                                ST.insertError $ Err.TE error

                                                                                        return $ AST.New $1
                                                                                    }
    | EXPR free '.'                                                                 {% do
                                                                                        let ptrType = AST.getType $1

                                                                                        case ptrType of
                                                                                            T.PointerT _ -> do
                                                                                                unless (AST.isValidLValue $1) $ do
                                                                                                    ST.insertError $ Err.TE $ Err.InvalidLValue (Tk.cleanedString $2) (Tk.position $2)
                                                                                            _            -> do
                                                                                                let error = Err.InvalidFree (show ptrType) (Tk.position $2)
                                                                                                ST.insertError $ Err.TE error

                                                                                        return $ AST.Free $1
                                                                                    }
    | continue '.'                                                                  {% do
                                                                                        openLoops <- ST.currentOpenLoops
                                                                                        if openLoops == 0
                                                                                            then ST.insertError $ Err.PE $ Err.NoLoop (Tk.position $1)
                                                                                            else return $ AST.Continue
                                                                                    }
    | break '.'                                                                     {% do
                                                                                        openLoops <- ST.currentOpenLoops
                                                                                        if openLoops == 0
                                                                                            then ST.insertError $ Err.PE $ Err.NoLoop (Tk.position $1)
                                                                                            else return $ AST.Break
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
                                                                                        unless ( elem exprType [T.BoolT, T.TypeError] ) $ do
                                                                                            let exprType = AST.getType $2
                                                                                                err    = Err.InvalidIfType (show exprType) (Tk.position $1)
                                                                                            ST.insertError $ Err.TE err

                                                                                        return $ AST.IfThen $2 (reverse $4)
                                                                                    }
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif                                 {% do

                                                                                        let exprType = AST.getType $2
                                                                                        unless ( elem exprType [T.BoolT, T.TypeError] ) $ do
                                                                                            let exprType = AST.getType $2
                                                                                                err    = Err.InvalidIfType (show exprType) (Tk.position $1)
                                                                                            ST.insertError $ Err.TE err

                                                                                        return $ AST.IfThenElse $2 (reverse $4) (reverse $6)
                                                                                    }

SWITCHCASE :: { AST.Instruction }
    : switch EXPR switchDec '.' CASES endSwitch                                     {% do
                                                                                        let switchExprType = AST.getType $2
                                                                                        unless ( elem exprType [T.AtomT, T.TypeError] ) $ do
                                                                                            let err = Err.WrongSwitchType (show switchExprType) (Tk.position $1)
                                                                                            ST.insertError $ Err.TE err
                                                                                        return $ AST.Switch $2 (reverse $5)
                                                                                    }

CASES :: { [AST.Case] }
    : CASE                                                                          { [$1] }
    | CASES CASE                                                                    { $2 : $1 }

CASE :: { AST.Case }
    : case atomLit '.' CODE_BLOCK                                                   { AST.Case (Tk.cleanedString $2) (reverse $4) }
    | case nothing '.' CODE_BLOCK                                                   { AST.Default (reverse $4) }

FOR :: { AST.Instruction }
    : OPEN_SCOPE OPEN_LOOP FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE CLOSE_LOOP       { let (id, lb, ub) = $2 in AST.For id lb ub (reverse $3) }

FOR_DEC :: { (AST.Id, AST.Expression, AST.Expression) }
    : for id type int '.' forLB EXPR forUB EXPR '.'                                 {% do

                                                                                        ST.insertId $2 ST.Variable ST.int

                                                                                        let lbType = AST.getType $7
                                                                                            ubType = AST.getType $9

                                                                                        unless (lbType == T.IntT && ubType == T.IntT) $ do
                                                                                            unless (lbType == T.TypeError || ubType == T.TypeError) $ do
                                                                                                let lb    = show lbType
                                                                                                    ub    = show ubType
                                                                                                    error = Err.WrongForBoundType lb ub (Tk.position $1)

                                                                                                ST.insertError $ Err.TE error

                                                                                        return (Tk.cleanedString $2, $7, $9)
                                                                                    }

-- TODO: Check for type error
WHILE :: { AST.Instruction }
    : while EXPR whileDec OPEN_LOOP CODE_BLOCK CLOSE_LOOP endWhile                  {% do
                                                                                        let exprType = AST.getType $2
                                                                                        unless (elem exprType [T.BoolT, T.TypeError]) $ do
                                                                                                let error = Err.InvalidWhileType (show exprType) (Tk.position $1)
                                                                                                ST.insertError $ Err.TE error

                                                                                        return $ AST.While $2 (reverse $4)
                                                                                    }

-- Expresions --

EXPR :: { AST.Expression }
    : EXPR '+' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Sum $1 $3 }
    | EXPR '-' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Sub $1 $3 }
    | EXPR '*' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Prod $1 $3 }
    | EXPR '/' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Div $1 $3 }
    | EXPR '%' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Mod $1 $3 }
    | EXPR '=' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Eq $1 $3 }
    | EXPR '!=' EXPR                                                                {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Neq $1 $3 }
    | EXPR '<' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Lt $1 $3 }
    | EXPR '>' EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Gt $1 $3 }
    | EXPR '<=' EXPR                                                                {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Leq $1 $3 }
    | EXPR '>=' EXPR                                                                {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Geq $1 $3 }
    | EXPR and EXPR                                                                 {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.And $1 $3 }
    | EXPR or EXPR                                                                  {% TC.buildAndCheckExpr $2 $ AST.BinOp AST.Or $1 $3 }
    | EXPR '~'                                                                      {% TC.buildAndCheckExpr $2 $ AST.UnOp AST.Neg $1 }
    | deref EXPR                                                                    {% TC.buildAndCheckExpr $1 $ AST.UnOp AST.Deref $2 }
    | '[' EXPRLIST ']' EXPR                                                         {% TC.buildAndCheckExpr $3 $ AST.AccesIndex $4 (reverse $2) }
    | id '<-' EXPR                                                                  {% TC.buildAndCheckExpr $2 $ AST.AccesField $3 (Tk.cleanedString $1) }
    | EXPR '->' id                                                                  {% TC.buildAndCheckExpr $2 $ AST.AccesField $1 (Tk.cleanedString $3) }
    | EXPR '?' id                                                                   {% TC.buildAndCheckExpr $2 $ AST.ActiveField $1 (Tk.cleanedString $3) }
    | '[(' naturalLit ']' EXPR                                                      {% TC.buildAndCheckExpr $3 $ AST.TupleIndex $4 ((read $ Tk.cleanedString $2) :: Int) }
    | EXPR cast TYPE                                                                {% TC.buildAndCheckExpr $2 $ AST.Cast $1 $3 }
    | '(' EXPR ')'                                                                  { $2 }
    | ARRAYLIT                                                                      { $1 }
    | TUPLELIT                                                                      { $1 }
    | FUNCTIONCALL                                                                  { $1 }
    | intLit                                                                        {% TC.buildAndCheckExpr $1 $ AST.IntLit ((read $ Tk.cleanedString $1) :: Int) }
    | floatLit                                                                      {% TC.buildAndCheckExpr $1 $ AST.FloatLit ((read $ Tk.cleanedString $1) :: Float) }
    | charLit                                                                       {% TC.buildAndCheckExpr $1 $ AST.CharLit $ head $ Tk.cleanedString $1 }
    | atomLit                                                                       {% TC.buildAndCheckExpr $1 $ AST.AtomLit $ Tk.cleanedString $1 }
    | stringLit                                                                     {% TC.buildAndCheckExpr $1 $ AST.StringLit $ Tk.cleanedString $1 }
    | true                                                                          {% TC.buildAndCheckExpr $1 $ AST.TrueLit }
    | false                                                                         {% TC.buildAndCheckExpr $1 $ AST.FalseLit }
    | null                                                                          {% TC.buildAndCheckExpr $1 $ AST.NullLit }
    | id                                                                            {% TC.buildAndCheckExpr $1 $ AST.IdExpr (Tk.cleanedString $1) }

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
parseError :: [Tk.Token] -> ST.MonadParser a -- OJO
parseError []     = do ST.insertError $ Err.PE Err.SyntaxErrEOF
                       fail "Parse error at EOF."
parseError (tk:_) = do ST.insertError $ Err.PE (Err.SyntaxErr tk)
                       fail $ "error: parse error with: \"" ++ Tk.cleanedString tk
                             ++ "\" at position " ++ show (Tk.position tk)
                             ++ "related to token: " ++ show (Tk.aToken tk)

checkFunctionCall :: Tk.Token -> AST.Expression -> ST.MonadParser AST.Instruction
checkFunctionCall tk expr = do
    unless (AST.isFunctionCall expr) $ do
        ST.insertError $ Err.PE $ Err.NonCallableExpression (Tk.position tk)
    return $ AST.FuncCallInst expr


checkAssignment :: Tk.Token -> [AST.Expression] -> AST.Expression -> Bool -> ST.MonadParser ()
checkAssignment tk lValues rValue isInit = do


    unless ((length lValues == 1) && isInit) $ do
        mapM_ checkConstantReassignment lValues

    mapM_ checkValidLValue lValues    

    case lValues of
        [lValue] -> do
            let lType = AST.getType lValue
                rType = AST.getType rValue
            
            unless (T.checkAssignable Type rType) $ do
                ST.insertError $ Err.TE $ Err.InvalidAssignment (show lType) (show rType) (Tk.position tk)
        _ -> do 
                let rType = AST.getType rValue
                case rType of
                    T.TupleT rTypes -> do
                        let lTypes = mapM AST.getType lVals
                        if (length lTypes == length rTypes)
                            then do 
                                zipWithM_ T.checkAssignable lTypes rTypes
                            else ST.insertError $ Err.TE $ Err.MultiAssignmentMissmatch (length lTypes) (length rTypes) (Tk.position tk)
                    TypeError -> do
                        return ()
                    _ -> ST.insertError $ Err.TE $ Err.InvalidMultiAssignment (show rType) (Tk.position tk) 
        
                        

checkConstantReassignment :: AST.Expression -> ST.MonadParser ()
checkConstantReassignment  AST.Expression{getExpr=(AST.IdExpr id), getToken=tk} = do
    let maybeSymbol = ST.lookupST id
    case maybeSymbol of
        Just (ST.SymbolInfo{category=ST.Constant}) ->
            ST.insertError $ Err.PE $ Err.ConstantReassignment (Tk.position $ tk)
        _ -> return ()
checkConstantReassignment AST.Expression{getExpr=(AST.AccesField expr id), getType=tp, getToken=tk} = do
    when T.isRecordType expr $ do
        symT <- get
        let scope = T.getScope tp
            maybeSymbol = ST.findSymbolInScope symT id scope
        case maybeSymbol of
            Just (ST.SymbolInfo{category=ST.Constant}) ->
                ST.insertError $ Err.PE $ Err.RecordFieldConstantReassignment id (Tk.position $ tk)
            _ -> return ()
checkConstantReassignment _ = return ()

checkValidLValue :: AST.Expression -> ST.MonadParser ()
checkValidLValue maybeLVal = do
    
    unless (AST.isValidLValue maybeLVal) $ do
        let exprToken = AST.getToken maybeLVal
        ST.insertError $ Err.TE $ Err.InvalidLValue (Tk.cleanedString exprToken) (Tk.position exprToken)

checkValidReturn :: Tk.Token -> [AST.Expression] -> ST.MonadParser ()
checkValidReturn statement exprs = do
    (fn, params) <- ST.currentOpenFunction
    case ST.lookupFunction fn params of
        Nothing -> return ()
        Just info -> do
            let fType = ST.symbolType info
            case fType of
                Nothing -> return ()
                Just tp -> do
                    let functionType = T.getTypeFromString tp
                    if functionType != T.TypeError
                        then
                            if T.checkAssignable functionType returnType
                                then return()
                                else ST.insertError $ Err.TE $ Err.InvalidReturnType (Tk.cleanedString statement) (Tk.position statement)
                            where returnType =  case exprs of
                                                    [expr] -> AST.getType expr
                                                    _ -> T.TupleT $ map AST.getType exprs 
                        else return ()         
}
