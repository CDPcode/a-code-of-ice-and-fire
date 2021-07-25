{
module Westeros.SouthOfTheWall.PreParser (preParse) where

import qualified Westeros.SouthOfTheWall.AST as Ast
import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk

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
PROGRAM :: {}
    : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                         {}
    | HEADER CONTENTS GLOBAL FUNCTIONS MAIN ALIASES                                 {}

HEADER :: {}
    : programStart programName                                                      {}

CONTENTS :: {}
    : beginFuncDec FUNCTION_DECLARATIONS                                            {}

FUNCTION_DECLARATIONS :: {}
    : item globalDec FUNCTION_NAMES item main                                       {}
 
FUNCTION_NAMES :: { () }
    : {- empty -}                                                                   { () }
    | FUNCTION_NAMES item id paramNumber                                            {% do 
                                                                                        symT <- get
                                                                                        let name  = Tk.cleanedString $3
                                                                                            params = read Tk.cleanedString $4 :: Int
                                                                                        function <- ST.lookupFunction name params
                                                                                        case function of
                                                                                            Nothing    -> put $ ST.insertST symT $ ST.functionDecEntry name params
                                                                                            Just entry -> ST.insertError $ Err.PE (Err.FRepeatedDeclarations name (Tk.position $3)) 
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

FUNCTION :: { () }
    : id FUNCTION_PARAMETERS FUNCTION_RETURN FUNCTION_BODY                          {% do
                                                                                        symT <- get
                                                                                        let functionId = Tk.cleanedString $1
                                                                                        let nParams = length $2
                                                                                        if ST.checkExisting symT functionId then do
                                                                                            decEntry <- findFunctionDeclaration functionId nParams
                                                                                            case decEntry of
                                                                                                Left entry -> do
                                                                                                    let newEntry = updateFunctionInfo entry $2 $3 
                                                                                                        newSymT = ST.searchAndReplaceSymbol symT (functionId, entry) newEntry
                                                                                                    put newSymT
                                                                                                Right defined -> do
                                                                                                    if defined 
                                                                                                        then ST.insertError $ Err.PE (Err.FRepeatedDefinitions functionId (Tk.position $1))
                                                                                                        else ST.insertError $ Err.PE (Err.InvalidNArgsDef functionId nParams (Tk.position $1))
                                                                                        else ST.insertError $ Err.PE (Err.FDefinitionWithoutDeclaration functionId (Tk.position $1))
                                                                                    }

FUNCTION_PARAMETERS :: { [ST.Symbol] }
    : beginFuncParams PARAMETER_LIST endFuncParams                                  { reverse $2 }

PARAMETER_LIST :: { [ST.Symbol] }
    : void                                                                          { [] }
    | PARAMETERS                                                                    { $1 }

PARAMETERS :: { [ST.Symbol]}
    : PARAMETER                                                                     { [$1] }
    | PARAMETERS ',' PARAMETER                                                      { $3 : $1 }

PARAMETER :: { ST.Symbol }
    : PARAMETER_TYPE id type TYPE                                                   { Tk.cleanedString $2 }

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
                                                                                        name <- genTypeSymbol
                                                                                        let info = ST.DopeVector $2 $3
                                                                                        ST.insertType name info
                                                                                    }
    | string                                                                        {% do
                                                                                        name <- genTypeSymbol
                                                                                        let info = ST.DopeVector 1 ST.char
                                                                                        ST.insertType name info
                                                                                    }
    | pointerType TYPE                                                              {% do
                                                                                        name <- genTypeSymbol
                                                                                        let info = ST.PointedType $2
                                                                                        ST.insertType name info
                                                                                    }
    | beginStruct OPEN_SCOPE SIMPLE_DECLARATIONS CLOSE_SCOPE endStruct              {% do
                                                                                        name <- genTypeSymbol
                                                                                        let info = ST.StructScope $2
                                                                                        ST.insertType name info 
                                                                                    }
    
    | beginUnion OPEN_SCOPE SIMPLE_DECLARATIONS CLOSE_SCOPE endUnion                {% do
                                                                                        name <- genTypeSymbol
                                                                                        let info = ST.UnionScope $2
                                                                                        ST.insertType name info
                                                                                    }
    | beginTuple TUPLE_TYPES endTuple                                               {% do 
                                                                                        name <- genTypeSymbol
                                                                                        let info = ST.TupleTypes $2
                                                                                        ST.insertType name info
                                                                                    }
OPEN_SCOPE :: { ST.Scope } 
    :  {- empty -}                                                                  { ST.currentScope } 

CLOSE_SCOPE :: { } 
    :  {- empty -}                                                                  { }

TUPLE_TYPES :: { [ST.Type] }
    : {- empty -}                                                                   { [] }
    | TYPES                                                                         { reverse $1 }

-- Alias Declaration --

DECLARATIONS :: {}
    : {- empty -}                                                                   {}
    | DECLARATIONS DECLARATION                                                      {}
    | DECLARATIONS comment                                                          {}

DECLARATION :: {}
    : SIMPLE_DECLARATION '.'                                                        {}
    | SIMPLE_DECLARATION ':=' EXPR '.'                                              {}
    | SIMPLE_DECLARATION ':==' EXPR '.'                                             {}
    | CONST_DECLARATION '.'                                                         {}

-- TODO: Las reglas de las declaraciones son las que llenan en la tabla los campos de los structs y unions
SIMPLE_DECLARATIONS :: { }
    : SIMPLE_DECLARATION                                                            { }
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                    { }

SIMPLE_DECLARATION :: {  }
    : PRIMITIVE_DECLARATION                                                         { }
    | COMPOSITE_DECLARATION                                                         { }

PRIMITIVE_DECLARATION :: { }
    : var id type TYPE                                                              { }

COMPOSITE_DECLARATION :: {  }
    : beginCompTypeId var id endCompTypeId TYPE                                     { }
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz              { }
    | beginCompTypeId pointerVar id endCompTypeId TYPE                              { }
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz       { }
--
CONST_DECLARATION :: {}
    : const id type TYPE constValue EXPR                                            {}
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                   {}

ALIAS_DECLARATION :: { () }
    : beginAlias id ALIAS_TYPE TYPE '.'                                             {% do 
                                                                                        symT <- get 
                                                                                        let name = Tk.cleanedString $2
                                                                                        case ST.findSymbol symT name of
                                                                                                Nothing -> put $ ST.insertST symT (createAliasEntry name $3 $4)
                                                                                                Just _  -> ST.insertError $ Err.PE (Err.RepeatedAliasName name (Tk.position $2))
                                                                                    }

ALIAS_TYPE :: { ST.AliasType }
    : strongAlias                                                                   { ST.ByName }
    | weakAlias                                                                     { ST.ByStructure }

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
    : if EXPR then INSTRUCTIONS endif                                               {}
    | if EXPR then INSTRUCTIONS else INSTRUCTIONS endif                             {}

SWITCHCASE :: {}
    : switch EXPR switchDec '.' CASES endSwitch                                     {}

CASES :: {}
    : CASE                                                                          {}
    | CASES CASE                                                                    {}

CASE :: {}
    : case atomLit '.' INSTRUCTIONS                                                 {}
    | case nothing '.' INSTRUCTIONS                                                 {}

FOR :: {}
    : for id type int '.' forLB EXPR forUB EXPR '.' INSTRUCTIONS endFor             {}

WHILE :: {}
    : while EXPR whileDec INSTRUCTIONS endWhile                                     {}

-- Expresions --

EXPR :: { Ast.Expression }
    : EXPR '+' EXPR                                                                 { }
    | EXPR '-' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Sub $1 $3 }
    | EXPR '*' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Prod $1 $3 }
    | EXPR '/' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Div $1 $3 }
    | EXPR '%' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Mod $1 $3 }
    | EXPR '=' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Eq $1 $3 }
    | EXPR '!=' EXPR                                                                { createExpression $2 $ Ast.BinOp Ast.Neq $1 $3 }
    | EXPR '<' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Lt $1 $3 }
    | EXPR '>' EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.Gt $1 $3 }
    | EXPR '<=' EXPR                                                                { createExpression $2 $ Ast.BinOp Ast.Leq $1 $3 }
    | EXPR '>=' EXPR                                                                { createExpression $2 $ Ast.BinOp Ast.Geq $1 $3 }
    | EXPR and EXPR                                                                 { createExpression $2 $ Ast.BinOp Ast.And $1 $3 }
    | EXPR or EXPR                                                                  { createExpression $2 $ Ast.BinOp Ast.Or $1 $3 }
    | EXPR '~'                                                                      { createExpression $2 $ Ast.UnOp Ast.Neg $1 }
    | deref EXPR                                                                    { createExpression $1 $ Ast.UnOp Ast.Deref $2 }
    | '[' EXPRLIST ']' EXPR                                                         { createExpression $3 $ Ast.AccesIndex $4 (reverse $2) }
    | id '<-' EXPR                                                                  { createExpression $2 $ Ast.AccesField $3 (Tk.cleanedString $1) }
    | EXPR '->' id                                                                  { createExpression $2 $ Ast.AccesField $1 (Tk.cleanedString $3) }
    | EXPR '?' id                                                                   { createExpression $2 $ Ast.ActiveField $1 (Tk.cleanedString $3) }
    | '[(' naturalLit ']' EXPR                                                      { createExpression $3 $ Ast.TupleIndex $4 ((read $ Tk.cleanedString $2) :: Int) }
    | EXPR cast TYPE                                                                { createExpression $2 $ Ast.Cast $1 $3 }
    | '(' EXPR ')'                                                                  { $2 }
    | ARRAYLIT                                                                      { $1 }
    | TUPLELIT                                                                      { $1 }
    | FUNCTIONCALL                                                                  { $1 }
    | intLit                                                                        { createExpression $1 $ Ast.IntLit ((read $ Tk.cleanedString $1) :: Int) }
    | floatLit                                                                      { createExpression $1 $ Ast.FloatLit ((read $ Tk.cleanedString $1) :: Float) }
    | charLit                                                                       { createExpression $1 $ Ast.CharLit $ head $ Tk.cleanedString $1 }
    | atomLit                                                                       { createExpression $1 $ Ast.AtomLit $ Tk.cleanedString $1 }
    | stringLit                                                                     { createExpression $1 $ Ast.StringLit $ Tk.cleanedString $1 }
    | true                                                                          { createExpression $1 $ Ast.TrueLit }
    | false                                                                         { createExpression $1 $ Ast.FalseLit }
    | id                                                                            { createExpression $1 $ Ast.IdExpr $ Tk.cleanedString $1 }
    | null                                                                          { createExpression $1 $ Ast.NullLit }

FUNCTIONCALL :: { Ast.Expression }
    : id '((' procCallArgs EXPRLIST '))'                                            { createExpression $2 $ Ast.FuncCall (Tk.cleanedString $1) (reverse $4) }
    | id '((' procCallArgs void '))'                                                { createExpression $2 $ Ast.FuncCall (Tk.cleanedString $1) [] }
    | id '(('  '))'                                                                 { createExpression $2 $ Ast.FuncCall (Tk.cleanedString $1) [] }

ARRAYLIT :: { Ast.Expression }
    : '{{' EXPRLIST '}}'                                                            { createExpression $1 $ Ast.ArrayLit $ reverse $2 }
    | '{{' '}}'                                                                     { createExpression $1 $ Ast.ArrayLit [] }

TUPLELIT :: { Ast.Expression }
    : '[[' EXPRLIST ']]'                                                            { createExpression $1 $ Ast.TupleLit $ reverse $2 }
    | '[[' ']]'                                                                     { createExpression $1 $ Ast.TupleLit [] }

EXPRLIST :: { [Ast.Expression] }
    : EXPR                                                                          { [$1] }
    | EXPRLIST ',' EXPR                                                             { $3 : $1 }

{
parseError [] = error "Parse error at EOF."
parseError (tk:_) = error $ "error: parse error with: \"" ++ Tk.cleanedString tk 
                             ++ "\" at position " ++ show (Tk.position tk) 
                             ++ "related to token: " ++ show (Tk.aToken tk)

createExpression :: Tk.Token -> Ast.Expr -> Ast.Expression
createExpression tk expr = Ast.Expression { Ast.getToken = tk, Ast.getExpr = expr, Ast.getType = Ast.AliasT "undefined" }


}