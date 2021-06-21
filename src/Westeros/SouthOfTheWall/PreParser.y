{
module Westeros.SouthOfTheWall.PreParser (preParse) where

import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Error as Err

import Control.Monad.RWS
import Data.List (find)
import Data.Maybe (fromJust)

--    import qualified Westeros.SouthOfTheWall.AST as Ast
}

%name                 preParse
%monad                { ST.MonadParser }
%tokentype            { Tk.Token }
%error                { parseError }
-- TODO: %monad expr to properly handle errors

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
valueArg        { Tk.Token { Tk.aToken=Tk.TknValueArg } }
refArg          { Tk.Token { Tk.aToken=Tk.TknReferenceArg } }
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
argNumber       { Tk.Token { Tk.aToken=Tk.TknArgNumber } }
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
PROGRAM : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                                     {}
        | HEADER CONTENTS GLOBAL FUNCTIONS MAIN ALIASES                                             {}

HEADER : programStart programName                                                                   {}

CONTENTS : beginFuncDec FUNCTION_DECLARATIONS                                                       {}

FUNCTION_DECLARATIONS : item globalDec FUNCTION_NAMES item main                                     {}

FUNCTION_NAMES :: { () }
     : {- empty -}                                                                                  { () }
     | FUNCTION_NAMES item id argNumber                                                             {% ST.statefullSTupdate (ST.getFunctionDeclarationInfo $3 $4)}

GLOBAL : globalDec '{' DECLARATIONS '}'                                                             {}

MAIN : main FUNCTION_BODY                                                                           {}

ALIASES : aliasDec ALIAS_DECLARATIONS                                                               {}

ALIAS_DECLARATIONS: ALIAS_DECLARATION                                                               {}
                  | ALIAS_DECLARATIONS ALIAS_DECLARATION                                            {}

-- Subrutines --

FUNCTIONS : {- empty -}                                                                             {}
          | FUNCTIONS FUNCTION                                                                      {}

FUNCTION :: { () }
     : id FUNCTION_PARAMETERS FUNCTION_RETURN FUNCTION_BODY                                         {% do
                                                                                                         symT <- get

                                                                                                         let functionId = (Tk.cleanedString $1)

                                                                                                         -- name not in table
                                                                                                         when (not $ ST.checkExisting symT functionId) (fail "F")

                                                                                                         -- match found
                                                                                                         let entries         = fromJust $ ST.findSymbol symT functionId            -- bring all definitions for functionId name
                                                                                                             actualFunctions = filter (\e-> ST.category e == ST.Function ) entries -- filter those defined as functions

                                                                                                             check fEntry = (not $ ST.discriminant (ST.getFunctionMD fEntry)) && ST.nArgs (ST.getFunctionMD fEntry) == length $2
                                                                                                             matching     = find check actualFunctions

                                                                                                             -- choose the function entry that matches de requirements:
                                                                                                             --    + is a Function that hasn't been validated (.i.e: discriminant is false)
                                                                                                             --    + is a Function with the same number of arguments

                                                                                                         case matching of
                                                                                                              Nothing -> fail ("Not a function \"" ++ functionId ++ "\" declared, or defined but matching number of arguments")
                                                                                                              Just e  -> do

                                                                                                                   let newAdditional = (ST.getFunctionMD e) { ST.discriminant = True, ST.fParameters = $2 , ST.fReturn = $3 }
                                                                                                                       newSymT = ST.searchAndReplaceSymbol symT (functionId,e) (e { ST.additional = Just (ST.FunctionMD newAdditional) })
                                                                                                                   
                                                                                                                   put newSymT
                                                                                                    }





FUNCTION_PARAMETERS :: { [ST.Parameter] }
     : beginFuncParams PARAMETER_LIST endFuncParams                                                 { reverse $2 }

PARAMETER_LIST :: { [ST.Parameter] }
     : void                                                                                         { [] }
     | PARAMETERS                                                                                   { $1 }

PARAMETERS :: { [ST.Parameter] }
          : PARAMETER                                                                               { [$1] }
          | PARAMETERS ',' PARAMETER                                                                { $3 : $1 }

PARAMETER :: { ST.Parameter }
     : PARAMETER_TYPE id type TYPE                                                                  { ($1, Tk.cleanedString $2, $4) }

PARAMETER_TYPE :: { ST.PassType }
     : valueArg                                                                                     { ST.Value }
     | refArg                                                                                       { ST.Reference }

FUNCTION_RETURN :: { [ST.Type] }
     : beginReturnVals RETURN_TYPES endReturnVals                                                   { reverse $2 }

RETURN_TYPES  :: { [ST.Type] }
          : void                                                                                    { [] }
          | TYPES                                                                                   { $1 }

TYPES :: { [ST.Type] }
     : TYPE                                                                                         { [$1] }
     | TYPES ',' TYPE                                                                               { $3 : $1 }

FUNCTION_BODY : '{' INSTRUCTIONS '}'                                                                {}

-- Types ---

TYPE :: { ST.Type }
     : PRIMITIVE_TYPE                                                                               { $1 }
     | COMPOSITE_TYPE                                                                               { $1 }
     | id                                                                                           { ST.Als (Tk.cleanedString $1) }

PRIMITIVE_TYPE :: { ST.Type }
     : int                                                                                          { ST.Int }
     | float                                                                                        { ST.Float }
     | char                                                                                         { ST.Char }
     | bool                                                                                         { ST.Bool }
     | atom                                                                                         { ST.Atom }

COMPOSITE_TYPE :: { ST.Type }
     : beginArray naturalLit TYPE endArray                                                          { ST.Array (read (Tk.cleanedString $2) :: Int) $3 }
     | string                                                                                       { ST.Str }
     | pointerType TYPE                                                                             { ST.Ptr $2 }
     | beginStruct SIMPLE_DECLARATIONS endStruct                                                    { ST.Register (reverse $2) }
     | beginUnion SIMPLE_DECLARATIONS endUnion                                                      { ST.VRegister (reverse $2) }
     | beginTuple TUPLE_TYPES endTuple                                                              { ST.Tuple (reverse $2) }

TUPLE_TYPES :: { [ST.Type] }
          : {- empty -}                                                                             { [] }
          | TYPES                                                                                   { $1 }

-- Alias Declaration --

DECLARATIONS : {- empty -}                                                                          {}
             | DECLARATIONS DECLARATION                                                             {}
             | DECLARATIONS comment                                                                 {}

DECLARATION : SIMPLE_DECLARATION '.'                                                                {}
            | SIMPLE_DECLARATION ':=' EXPR '.'                                                      {}
            | SIMPLE_DECLARATION ':==' EXPR '.'                                                     {}
            | CONST_DECLARATION '.'                                                                 {}

SIMPLE_DECLARATIONS :: { [ST.Declaration] }
     : SIMPLE_DECLARATION                                                                           { [$1] }
     | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                                   { $3 : $1 }

SIMPLE_DECLARATION :: { ST.Declaration }
     : PRIMITIVE_DECLARATION                                                                        { $1 }
     | COMPOSITE_DECLARATION                                                                        { $1 }

PRIMITIVE_DECLARATION :: { ST.Declaration }
     : var id type TYPE                                                                             { (ST.Var, Tk.cleanedString $2, $4) }

COMPOSITE_DECLARATION :: { ST.Declaration }
     : beginCompTypeId var id endCompTypeId TYPE                                                    { (ST.Var, Tk.cleanedString $3, $5) }
     | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz                             { (ST.Var, Tk.cleanedString $3, $5) }
     | beginCompTypeId pointerVar id endCompTypeId TYPE                                             { (ST.PtrVar, Tk.cleanedString $3, $5) }
     | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz                      { (ST.PtrVar, Tk.cleanedString $3, $5) }

CONST_DECLARATION : const id type TYPE constValue EXPR                                              {}
                  | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                     {}

ALIAS_DECLARATION :: { () }
     : beginAlias id ALIAS_TYPE TYPE '.'                                                            {% ST.statefullSTupdate (ST.getAliasInfo $2 $3 $4) }

ALIAS_TYPE :: { ST.AliasType }
     : strongAlias                                                                                  { ST.Strong }
     | weakAlias                                                                                    { ST.Weak }

-- Instructions --

INSTRUCTIONS : {- empty -}                                                                          {}
             | INSTRUCTIONS INSTRUCTION                                                             {}
             | INSTRUCTIONS comment                                                                 {}

INSTRUCTION : EXPR ':=' EXPR '.'                                                                    {}
            | EXPRLIST ':==' EXPR '.'                                                               {}
            | void ':=' EXPR '.'                                                                    {}
            | void ':==' EXPR '.'                                                                   {}
            | pass '.'                                                                              {}
            | beginExit programName endExit '.'                                                     {}
            | read EXPR '.'                                                                         {}
            | print EXPR '.'                                                                        {}
            | EXPR new '.'                                                                          {}
            | EXPR free '.'                                                                         {}
            | continue '.'                                                                          {}
            | break '.'                                                                             {}
            | returnOpen EXPRLIST returnClose                                                       {}
            | returnOpen returnClose                                                                {}
            | IF '.'                                                                                {} -- ##
            | SWITCHCASE                                                                            {}
            | FOR '.'                                                                               {} -- ##
            | WHILE                                                                                 {}
            | DECLARATION                                                                           {}
            | FUNCTIONCALL                                                                          {}

IF : if EXPR then INSTRUCTIONS endif                                                                {}
   | if EXPR then INSTRUCTIONS else INSTRUCTIONS endif                                              {}

SWITCHCASE : switch EXPR switchDec CASES endSwitch                                                  {}

CASES : CASE                                                                                        {}
      | CASES CASE                                                                                  {}

CASE : case atomLit '.' INSTRUCTIONS                                                                {}
     | case nothing '.' INSTRUCTIONS                                                                {}

FOR : for id type int '.' forLB EXPR forUB EXPR '.' INSTRUCTIONS endFor                             {}

WHILE : while EXPR whileDec INSTRUCTIONS endWhile                                                   {}

-- Expresions --

EXPR : EXPR '+' EXPR                                                                                {}
     | EXPR '-' EXPR                                                                                {}
     | EXPR '*' EXPR                                                                                {}
     | EXPR '/' EXPR                                                                                {}
     | EXPR '%' EXPR                                                                                {}
     | EXPR '=' EXPR                                                                                {}
     | EXPR '!=' EXPR                                                                               {}
     | EXPR '<' EXPR                                                                                {}
     | EXPR '>' EXPR                                                                                {}
     | EXPR '<=' EXPR                                                                               {}
     | EXPR '>=' EXPR                                                                               {}
     | EXPR and EXPR                                                                                {}
     | EXPR or EXPR                                                                                 {}
     | EXPR '~'                                                                                     {}
     | deref EXPR                                                                                   {}
     | '[' EXPRLIST ']' EXPR                                                                        {}
     | id '<-' EXPR                                                                                 {}
     | EXPR '->' id                                                                                 {}
     | EXPR '?' id                                                                                  {}
     | '[(' naturalLit ']' EXPR                                                                     {}
     | EXPR cast TYPE                                                                               {}
     | '(' EXPR ')'                                                                                 {}
     | ARRAYLIT                                                                                     {}
     | TUPLELIT                                                                                     {}
     | FUNCTIONCALL                                                                                 {}
     | intLit                                                                                       {}
     | floatLit                                                                                     {}
     | charLit                                                                                      {}
     | atomLit                                                                                      {}
     | stringLit                                                                                    {}
     | true                                                                                         {}
     | false                                                                                        {}
     | id                                                                                           {}
     | null                                                                                         {}

FUNCTIONCALL : id '((' procCallArgs EXPRLIST '))'                                                   {}
             | id '((' procCallArgs void '))'                                                       {}
             | id '(('  '))'                                                                        {}

ARRAYLIT : '{{' EXPRLIST '}}'                                                                       {}
         | '{{' '}}'                                                                                {}

TUPLELIT : '[[' EXPRLIST ']]'                                                                       {}
         | '[[' ']]'                                                                                {}

EXPRLIST : EXPR                                                                                     {}
         | EXPRLIST ',' EXPR                                                                        {}

{

parseError :: [Tk.Token] -> ST.MonadParser a
parseError tks = fail (Err.displayErrorContext tks)

}