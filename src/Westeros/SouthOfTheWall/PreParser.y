{
module Westeros.SouthOfTheWall.PreParser (preParse) where

import qualified Westeros.SouthOfTheWall.AST            as AST
import qualified Westeros.SouthOfTheWall.Error          as Err
import qualified Westeros.SouthOfTheWall.Symtable       as ST
import qualified Westeros.SouthOfTheWall.Tokens         as Tk
import qualified Westeros.SouthOfTheWall.Types          as T
import qualified Westeros.SouthOfTheWall.TypeChecking   as TC

import Control.Monad.RWS (get, put)
import Data.List (find)
import Data.Maybe (fromJust)

}

%name                 preParse
%monad                { ST.MonadParser }
%tokentype            { Tk.Token }
%error                { parseError }

-- Token aliases definitions
%token

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
valueParam      { Tk.Token { Tk.aToken=Tk.TknValueParam } }
refParam        { Tk.Token { Tk.aToken=Tk.TknReferenceParam } }
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
paramNumber     { Tk.Token { Tk.aToken=Tk.TknParamNumber } }
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
PROGRAM :: {}
    : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                         {}
    | ALIASES HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                 {}

HEADER :: {}
    : programStart programName                                                      {}

CONTENTS :: {}
    : beginFuncDec FUNCTION_DECLARATIONS                                            {}

FUNCTION_DECLARATIONS :: {}
    : item globalDec FUNCTION_NAMES item main                                       {}

FUNCTION_NAMES :: { () }
    : {- empty -}                                                                   { () }
    | FUNCTION_NAMES item id paramNumber                                            {% do
                                                                                        let name  = Tk.cleanedString $3
                                                                                            params = read $ Tk.cleanedString $4 :: Int
                                                                                        function <- ST.lookupFunction name params
                                                                                        case function of
                                                                                            Nothing -> do
                                                                                                symT <- get
                                                                                                put $ ST.insertST symT $ ST.functionDecEntry name params
                                                                                            Just entry -> 
                                                                                                ST.insertError $ Err.PE (Err.RedeclareFunction name params (Tk.position $3))
                                                                                    }


GLOBAL :: {}
    : globalDec '{' DECLARATIONS '}'                                                {}

MAIN :: {}
    : main FUNCTION_BODY                                                            {}

ALIASES :: {}
    : aliasDec ALIAS_DECLARATIONS                                                   {}

ALIAS_DECLARATIONS :: {}
    : ALIAS_DECLARATION                                                             {}
    | ALIAS_DECLARATIONS ALIAS_DECLARATION                                          {}

-- Subrutines --
 
FUNCTIONS :: {}
    : {- empty -}                                                                   {}
    | FUNCTIONS FUNCTION                                                            {}

FUNCTION :: {} 
    : FUNCTION_DEF FUNCTION_BODY CLOSE_SCOPE                                        {}

FUNCTION_DEF :: { () }
    : id OPEN_SCOPE FUNCTION_PARAMETERS FUNCTION_RETURN                             {% do
                                                                                        symT <- get
                                                                                        let functionId = Tk.cleanedString $1
                                                                                            nParams = length $3
                                                                                        if ST.checkExisting symT functionId then do
                                                                                            decEntry <- ST.findFunctionDec functionId nParams
                                                                                            case decEntry of
                                                                                                Left entry -> do
                                                                                                    let params     = fst $ unzip $3
                                                                                                        paramTypes = snd $ unzip $3
                                                                                                    newEntry <- ST.updateFunctionInfo entry params paramTypes $4
                                                                                                    put $  ST.searchAndReplaceSymbol symT (functionId, entry) newEntry
                                                                                                Right defined -> do
                                                                                                    if defined
                                                                                                        then ST.insertError $ Err.PE (Err.RedefineFunction functionId nParams (Tk.position $1))
                                                                                                        else ST.insertError $ Err.PE (Err.UndeclaredFunction functionId nParams (Tk.position $1))
                                                                                        else ST.insertError $ Err.PE (Err.UndeclaredFunction functionId nParams (Tk.position $1))
                                                                                    }

FUNCTION_PARAMETERS :: { [(ST.Symbol, ST.Type)] }
    : beginFuncParams PARAMETER_LIST endFuncParams                                  { reverse $2 }

PARAMETER_LIST :: { [(ST.Symbol, ST.Type)] }
    : void                                                                          { [] }
    | PARAMETERS                                                                    { $1 }

PARAMETERS :: { [(ST.Symbol, ST.Type)] }
    : PARAMETER                                                                     { [$1] }
    | PARAMETERS ',' PARAMETER                                                      { $3 : $1}

PARAMETER :: { (ST.Symbol, ST.Type) }
    : PARAMETER_TYPE id type TYPE                                                   { (Tk.cleanedString $2, $4) }
    | beginCompTypeId PARAMETER_TYPE id endCompTypeId TYPE                          { (Tk.cleanedString $3, $5) }
    | beginCompTypeId PARAMETER_TYPE pointerVar id endCompTypeId TYPE               { (Tk.cleanedString $4, $6) }

PARAMETER_TYPE :: { ST.ParameterType }
    : valueParam                                                                    { ST.Value }
    | refParam                                                                      { ST.Reference }

FUNCTION_RETURN :: { [ST.Type] }
    : beginReturnVals RETURN_TYPES endReturnVals                                    { reverse $2 }

RETURN_TYPES  :: { [ST.Type] }
    : void                                                                          { [] }
    | TYPES                                                                         { $1 }

TYPES :: { [ST.Type] }
    : TYPE                                                                          { [$1] }
    | TYPES ',' TYPE                                                                { $3 : $1 }

FUNCTION_BODY :: {}
    : '{' INSTRUCTIONS '}'                                                          {}

-- Types ---
TYPE :: { ST.Type }
    : PRIMITIVE_TYPE                                                                { $1 }
    | COMPOSITE_TYPE                                                                { $1 }
    | id                                                                            { Tk.cleanedString $1 }

PRIMITIVE_TYPE :: { ST.Type }
    : int                                                                           { ST.int }
    | float                                                                         { ST.float }
    | char                                                                          { ST.char }
    | bool                                                                          { ST.bool }
    | atom                                                                          { ST.atom }

COMPOSITE_TYPE :: { ST.Type }
    : beginArray naturalLit TYPE endArray                                           {% do
                                                                                        name <- ST.genTypeSymbol
                                                                                        let dim = read $ Tk.cleanedString $2
                                                                                            info = ST.DopeVector $3 dim
                                                                                            typeInfo = ST.TypeInfo {ST.width = (dim+1) * 4, ST.align = 4}
                                                                                        ST.insertType name info typeInfo
                                                                                    }
    | string                                                                        {% do
                                                                                        name <- ST.genTypeSymbol
                                                                                        let info = ST.DopeVector ST.char 1
                                                                                            typeInfo = ST.TypeInfo {ST.width = 2 * 4, ST.align = 4}
                                                                                        ST.insertType name info typeInfo
                                                                                    }
    | pointerType TYPE                                                              {% do
                                                                                        name <- ST.genTypeSymbol
                                                                                        let info = ST.PointedType $2
                                                                                            typeInfo = ST.TypeInfo {ST.width = 4, ST.align = 4}
                                                                                        ST.insertType name info typeInfo
                                                                                    }
    | beginStruct OPEN_SCOPE SIMPLE_DECLARATIONS CLOSE_SCOPE endStruct              {% do
                                                                                        name <- ST.genTypeSymbol
                                                                                        let info = ST.StructScope $2
                                                                                        typeInfo <- ST.getTupleTypeInfo $ reverse $3
                                                                                        ST.insertType name info typeInfo
                                                                                    }

    | beginUnion OPEN_SCOPE SIMPLE_DECLARATIONS CLOSE_SCOPE endUnion                {% do
                                                                                        name <- ST.genTypeSymbol
                                                                                        let info = ST.UnionScope $2
                                                                                        typeInfo <- ST.getUnionTypeInfo $ reverse $3
                                                                                        ST.insertType name info typeInfo
                                                                                    }
    | beginTuple TUPLE_TYPES endTuple                                               {% do
                                                                                        name <- ST.genTypeSymbol
                                                                                        let info = ST.TupleTypes $2
                                                                                        typeInfo <- ST.getTupleTypeInfo $2
                                                                                        ST.insertType name info typeInfo
                                                                                    }
OPEN_SCOPE :: { ST.Scope }
    :  {- empty -}                                                                  {% do
                                                                                        ST.openScope
                                                                                        ST.currentScope
                                                                                    }

CLOSE_SCOPE :: { () }
    :  {- empty -}                                                                  {% ST.closeScope }

TUPLE_TYPES :: { [ST.Type] }
    : {- empty -}                                                                   { [] }
    | TYPES                                                                         { reverse $1 }

DECLARATIONS :: {}
    : {- empty -}                                                                   { }
    | DECLARATIONS DECLARATION                                                      { }
    | DECLARATIONS comment                                                          { }

DECLARATION :: {}
    : SIMPLE_DECLARATION '.'                                                        { }
    | SIMPLE_DECLARATION ':=' EXPR '.'                                              { }
    | SIMPLE_DECLARATION ':==' EXPR '.'                                             { }
    | CONST_DECLARATION '.'                                                         { }

SIMPLE_DECLARATIONS :: { [ST.Type] }
    : SIMPLE_DECLARATION                                                            { [$1] }
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                    { $3 : $1 }

SIMPLE_DECLARATION :: { ST.Type }
    : PRIMITIVE_DECLARATION                                                         { $1 }
    | COMPOSITE_DECLARATION                                                         { $1 }

PRIMITIVE_DECLARATION :: { ST.Type }
    : var id type TYPE                                                              { $4 }

COMPOSITE_DECLARATION :: { ST.Type }
    : beginCompTypeId var id endCompTypeId TYPE                                     { $5 }
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz              { $5 }
    | beginCompTypeId pointerVar id endCompTypeId TYPE                              { $5 }
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz       { $5 }

CONST_DECLARATION :: { ST.Type }
    : const id type TYPE constValue EXPR                                            { $4 }
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                   { $5 }

ALIAS_DECLARATION :: { () }
    : beginAlias id ALIAS_TYPE TYPE '.'                                             {% ST.insertAlias $2 (Tk.cleanedString $2) $3 $4 }

ALIAS_TYPE :: { ST.AliasType }
    : strongAlias                                                                   { ST.ByName }
    | weakAlias                                                                     { ST.ByStructure }

CODE_BLOCK :: {}
    : OPEN_SCOPE INSTRUCTIONS CLOSE_SCOPE                                           {}

-- Instructions --
INSTRUCTIONS :: {}
    : {- empty -}                                                                   {}
    | INSTRUCTIONS INSTRUCTION                                                      {}
    | INSTRUCTIONS comment                                                          {}

INSTRUCTION :: {}
    : EXPR ':=' EXPR '.'                                                            {}
    | void ':=' EXPR '.'                                                            {}
    | EXPRLIST ':==' EXPR '.'                                                       {}
    | void ':==' EXPR '.'                                                           {}
    | pass '.'                                                                      {}
    | beginExit programName endExit '.'                                             {}
    | read EXPR '.'                                                                 {}
    | print EXPR '.'                                                                {}
    | EXPR new '.'                                                                  {}
    | EXPR free '.'                                                                 {}
    | continue '.'                                                                  {}
    | break '.'                                                                     {}
    | returnOpen EXPRLIST returnClose                                               {}
    | returnOpen returnClose                                                        {}
    | IF '.'                                                                        {}
    | SWITCHCASE  '.'                                                               {}
    | FOR '.'                                                                       {}
    | WHILE                                                                         {}
    | DECLARATION                                                                   {}
    | FUNCTIONCALL                                                                  {}

IF :: {}
    : if EXPR then CODE_BLOCK endif                                                 {}
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif                                 {}

SWITCHCASE :: {}
    : switch EXPR switchDec '.' CASES endSwitch                                     {}

CASES :: {}
    : CASE                                                                          {}
    | CASES CASE                                                                    {}

CASE :: {}
    : case atomLit '.' CODE_BLOCK                                                   {}
    | case nothing '.' CODE_BLOCK                                                   {}

FOR :: {}
    : OPEN_SCOPE FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE                            {}

FOR_DEC :: {}                                                                       
    : for id type int '.' forLB EXPR forUB EXPR '.'                                 {}

WHILE :: {}
    : while EXPR whileDec CODE_BLOCK endWhile                                       {}

-- Expresions --

EXPR :: { }
    : EXPR '+' EXPR                                                                 { }
    | EXPR '-' EXPR                                                                 { }
    | EXPR '*' EXPR                                                                 { }
    | EXPR '/' EXPR                                                                 { }
    | EXPR '%' EXPR                                                                 { }
    | EXPR '=' EXPR                                                                 { }
    | EXPR '!=' EXPR                                                                { }
    | EXPR '<' EXPR                                                                 { }
    | EXPR '>' EXPR                                                                 { }
    | EXPR '<=' EXPR                                                                { }
    | EXPR '>=' EXPR                                                                { }
    | EXPR and EXPR                                                                 { }
    | EXPR or EXPR                                                                  { }
    | EXPR '~'                                                                      { }
    | not EXPR                                                                      { }
    | deref EXPR                                                                    { }
    | '[' EXPRLIST ']' EXPR                                                         { }
    | id '<-' EXPR                                                                  { }
    | EXPR '->' id                                                                  { }
    | EXPR '?' id                                                                   { }
    | '[(' naturalLit ']' EXPR                                                      { }
    | EXPR cast TYPE                                                                { }
    | '(' EXPR ')'                                                                  { }
    | ARRAYLIT                                                                      { }
    | TUPLELIT                                                                      { }
    | FUNCTIONCALL                                                                  { }
    | intLit                                                                        { }
    | floatLit                                                                      { }
    | charLit                                                                       { }
    | atomLit                                                                       { }
    | stringLit                                                                     { }
    | true                                                                          { }
    | false                                                                         { }
    | id                                                                            { }
    | null                                                                          { }

FUNCTIONCALL :: { }
    : id '((' procCallArgs EXPRLIST '))'                                            { }
    | id '((' procCallArgs void '))'                                                { }
    | id '(('  '))'                                                                 { }

ARRAYLIT :: { }
    : '{{' EXPRLIST '}}'                                                            { }
    | '{{' '}}'                                                                     { }

TUPLELIT :: { }
    : '[[' EXPRLIST ']]'                                                            { }
    | '[[' ']]'                                                                     { }

EXPRLIST :: { }
    : EXPR                                                                          { }
    | EXPRLIST ',' EXPR                                                             { }

{
parseError [] = error "Parse error at EOF."
parseError (tk:_) = error $ "error: parse error with: \"" ++ Tk.cleanedString tk
                             ++ "\" at position " ++ show (Tk.position tk)
                             ++ "related to token: " ++ show (Tk.aToken tk)
}
