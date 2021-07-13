{
module Westeros.SouthOfTheWall.Parser (parse) where

import qualified Westeros.SouthOfTheWall.AST as Ast
import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Symtable as ST

import Data.Maybe (fromJust)
import Control.Monad.RWS ( MonadState(put, get), RWST, when )

}

%name                 parse
%tokentype            { Tk.Token }
%error                { parseError }
%monad                { ST.MonadParser }
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
PROGRAM :: { Ast.Program }
    : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                                         { Ast.Program $1 $2 $3 $4 $5 [] }
    | HEADER CONTENTS GLOBAL FUNCTIONS MAIN ALIASES                                                 { Ast.Program $1 $2 $3 $4 $5 $6 }

HEADER :: { Ast.Header }
    : programStart programName                                                                      { Tk.cleanedString $2 }

CONTENTS :: { Ast.FunctionNames }
    : beginFuncDec FUNCTION_DECLARATIONS                                                            { $2 }

FUNCTION_DECLARATIONS :: { Ast.FunctionNames }
    : item globalDec FUNCTION_NAMES item main                                                       { reverse $3 }

FUNCTION_NAMES :: { Ast.FunctionNames }
    : {- empty -}                                                                                   { [] }
    | FUNCTION_NAMES item id argNumber                                                              {% do
                                                                                                        let name = Tk.cleanedString $3
                                                                                                        let params = read (Tk.cleanedString $4) :: Int
                                                                                                        let pos = Tk.position $3
                                                                                                        
                                                                                                        function <- ST.lookupFunction name params
                                                                                                        case function of
                                                                                                            Nothing -> ST.insertError $ Err.PE (Err.UndefinedFunction name pos) 
                                                                                                            Just info -> case ST.discriminant $ ST.getFunctionMD info of
                                                                                                                True -> return ()
                                                                                                                False -> ST.insertError $ Err.PE (Err.UndefinedFunction name pos)
                                                                                                        return $ (name, params) : $1 }

GLOBAL :: { [Ast.Declaration] }
    : globalDec '{' DECLARATIONS '}'                                                                { reverse $3 }

MAIN :: { [Ast.Instruction] }
    : main FUNCTION_BODY                                                                            { $2 }

ALIASES :: { [Ast.AliasDeclaration] }
    : aliasDec ALIAS_DECLARATIONS                                                                   { reverse $2 }

ALIAS_DECLARATIONS :: { [Ast.AliasDeclaration] }
    : ALIAS_DECLARATION                                                                             { [$1] }
    | ALIAS_DECLARATIONS ALIAS_DECLARATION                                                          { $2 : $1 }

-- Subrutines --

FUNCTIONS :: { [Ast.FunctionDeclaration] }
    : {- empty -}                                                                                   { [] }
    | FUNCTIONS FUNCTION                                                                            { $2 : $1 }

FUNCTION :: { Ast.FunctionDeclaration }
    : id OPEN_SCOPE FUNCTION_PARAMETERS FUNCTION_RETURN FUNCTION_BODY CLOSE_SCOPE                   { Ast.FunctionDeclaration (Tk.cleanedString $1) $3 $4 $5 }

FUNCTION_PARAMETERS :: { [Ast.Parameter] }
    : beginFuncParams PARAMETER_LIST endFuncParams                                                  { $2 }

PARAMETER_LIST :: { [Ast.Parameter] }
    : void                                                                                          { [] }
    | PARAMETERS                                                                                    { reverse $1 }

PARAMETERS :: { [Ast.Parameter] }
    : PARAMETER                                                                                     { [$1] }
    | PARAMETERS ',' PARAMETER                                                                      { $3 : $1 }

PARAMETER :: { Ast.Parameter }
    : PARAMETER_TYPE id type TYPE                                                                   {% do
                                                                                                        sc <- ST.currentScope
                                                                                                        let name = Tk.cleanedString $2
                                                                                                        let entry = createParamEntry name sc $4 $1
                                                                                                        let pos = Tk.position $2
                                                                                                        symT <- get
                                                                                                        mInfo <- ST.lookup name
                                                                                                        case mInfo of
                                                                                                            Nothing -> put $ ST.insertST symT entry
                                                                                                            Just info ->
                                                                                                                if ST.scope info /= sc && ST.category info /= ST.Function && ST.category info /= ST.Alias
                                                                                                                    then put $ ST.insertST symT entry
                                                                                                                    else ST.insertError $ Err.PE (Err.RedeclaredParameter name pos)

                                                                                                        return $ Ast.Parameter $1 (Tk.cleanedString $2) $4
                                                                                                    }

PARAMETER_TYPE :: { Ast.ParamType }
    : valueArg                                                                                      { Ast.Value }
    | refArg                                                                                        { Ast.Ref }

FUNCTION_RETURN :: { [Ast.Type] }
    : beginReturnVals RETURN_TYPES endReturnVals                                                    { $2 }

RETURN_TYPES :: { [Ast.Type]}
    : void                                                                                          { [] }
    | TYPES                                                                                         { reverse $1 }

TYPES :: { [Ast.Type] }
    : TYPE                                                                                          { [$1] }
    | TYPES ',' TYPE                                                                                { $3 : $1 }

FUNCTION_BODY :: { [Ast.Instruction] }
    : '{' INSTRUCTIONS '}'                                                                          { reverse $2 }

TYPE :: { Ast.Type }
    : PRIMITIVE_TYPE                                                                                { $1 }
    | COMPOSITE_TYPE                                                                                { $1 }
    | id                                                                                            { Ast.AliasT (Tk.cleanedString $1) }

PRIMITIVE_TYPE :: { Ast.Type }
    : int                                                                                           { Ast.IntT }
    | float                                                                                         { Ast.FloatT }
    | char                                                                                          { Ast.CharT }
    | bool                                                                                          { Ast.BoolT }
    | atom                                                                                          { Ast.AtomT }

COMPOSITE_TYPE :: { Ast.Type }
    : beginArray naturalLit TYPE endArray                                                           { Ast.ArrayT $3 (read (Tk.cleanedString $2) :: Int) }
    | string                                                                                        { Ast.StringT }
    | pointerType TYPE                                                                              { Ast.PointerT $2 }
    | beginStruct SIMPLE_DECLARATIONS endStruct                                                     { Ast.StructT $2 }
    | beginUnion SIMPLE_DECLARATIONS endUnion                                                       { Ast.UnionT $2 }
    | beginTuple TUPLE_TYPES endTuple                                                               { Ast.TupleT $2 }

TUPLE_TYPES :: { [Ast.Type] }
    : {- empty -}                                                                                   { [] }
    | TYPES                                                                                         { reverse $1 }

-- Alias Declaration --

DECLARATIONS :: { [Ast.Declaration] }
    : {- empty -}                                                                                   { [] }
    | DECLARATIONS DECLARATION                                                                      { $2 : $1 }
    | DECLARATIONS comment                                                                          { $1 }

DECLARATION :: { Ast.Declaration }
    : SIMPLE_DECLARATION '.'                                                                        { Ast.VarDeclaration $1 Nothing }
    | SIMPLE_DECLARATION ':=' EXPR '.'                                                              { Ast.VarDeclaration $1 $ Just $3 }
    | SIMPLE_DECLARATION ':==' EXPR '.'                                                             { Ast.VarDeclaration $1 $ Just $3 }
    | CONST_DECLARATION '.'                                                                         { $1 }

SIMPLE_DECLARATIONS :: { [Ast.VariableDeclaration] }
    : SIMPLE_DECLARATION                                                                            { [$1] }
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                                    { $3 : $1 }

SIMPLE_DECLARATION :: { Ast.VariableDeclaration }
    : PRIMITIVE_DECLARATION                                                                         { $1 }
    | COMPOSITE_DECLARATION                                                                         { $1 }

PRIMITIVE_DECLARATION :: { Ast.VariableDeclaration }
    : var id type TYPE                                                                              {% maybeInsertVar $2 $4 Nothing }

COMPOSITE_DECLARATION :: { Ast.VariableDeclaration }
    : beginCompTypeId var id endCompTypeId TYPE                                                     {% maybeInsertVar $3 $5 Nothing }
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz                              {% maybeInsertVar $3 $5 (Just $7) }
    | beginCompTypeId pointerVar id endCompTypeId TYPE                                              {% maybeInsertVar $3 $5 Nothing }
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz                       {% maybeInsertVar $3 $5 (Just $7) }

CONST_DECLARATION :: { Ast.Declaration }
    : const id type TYPE constValue EXPR                                                            {% maybeInsertConst $2 $4 $6 }
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                                   {% maybeInsertConst $3 $5 $7 }

ALIAS_DECLARATION :: { Ast.AliasDeclaration }
    : beginAlias id ALIAS_TYPE TYPE '.'                                                             { Ast.AliasDec (Tk.cleanedString $2) $4 $3}

ALIAS_TYPE :: { Ast.AliasType }
    : strongAlias                                                                                   { Ast.StrongAlias }
    | weakAlias                                                                                     { Ast.WeakAlias }

-- Instructions --

CODE_BLOCK :: { [Ast.Instruction] }
    : OPEN_SCOPE INSTRUCTIONS CLOSE_SCOPE                                                           { $2 }

INSTRUCTIONS :: { [Ast.Instruction] }
    : {- empty -}                                                                                   { [] }
    | INSTRUCTIONS INSTRUCTION                                                                      { $2 : $1 }
    | INSTRUCTIONS comment                                                                          { $1 }

INSTRUCTION :: { Ast.Instruction }
    : EXPR ':=' EXPR '.'                                                                            { Ast.SimpleAssign $1 $3 }
    | EXPRLIST ':==' EXPR '.'                                                                       { Ast.MultAssign (reverse $1) $3 }
    | void ':=' EXPR '.'                                                                            { Ast.FuncCallInst $3 }
    | void ':==' EXPR '.'                                                                           { Ast.FuncCallInst $3 }
    | pass '.'                                                                                      { Ast.EmptyInst }
    | beginExit programName endExit '.'                                                             { Ast.ExitInst (Tk.cleanedString $3) }
    | read EXPR '.'                                                                                 { Ast.Read $2 }
    | print EXPR '.'                                                                                { Ast.Print $2}
    | EXPR new '.'                                                                                  { Ast.New $1 }
    | EXPR free '.'                                                                                 { Ast.Free $1 }
    | continue '.'                                                                                  { Ast.Continue }
    | break '.'                                                                                     { Ast.Break }
    | returnOpen EXPRLIST returnClose                                                               { Ast.Return (reverse $2) }
    | returnOpen returnClose                                                                        { Ast.Return [] }
    | IF '.'                                                                                        { Ast.If $1 }
    | SWITCHCASE '.'                                                                                { $1 }
    | FOR '.'                                                                                       { $1 }
    | WHILE '.'                                                                                     { $1 }
    | DECLARATION                                                                                   { Ast.DeclarationInst $1 }

IF :: { Ast.IfInst }
    : if EXPR then CODE_BLOCK endif                                                                 { Ast.IfThen $2 (reverse $4) }
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif                                                 { Ast.IfThenElse $2 (reverse $4) (reverse $6) }

SWITCHCASE :: { Ast.Instruction }
    : switch EXPR switchDec '.' CASES endSwitch                                                     { Ast.Switch $2 (reverse $5) }

CASES :: { [Ast.Case] }
    : CASE                                                                                          { [$1] }
    | CASES CASE                                                                                    { $2 : $1 }

CASE :: { Ast.Case }
    : case atomLit '.' CODE_BLOCK                                                                   { Ast.Case (Tk.cleanedString $2) (reverse $4) }
    | case nothing '.' CODE_BLOCK                                                                   { Ast.Default (reverse $4) }

FOR :: { Ast.Instruction }
    : OPEN_SCOPE FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE                                            { let (id, lb, ub) = $2 in Ast.For id lb ub (reverse $3) }

FOR_DEC :: { (Ast.Id, Ast.Expression, Ast.Expression) }
    : for id type int '.' forLB EXPR forUB EXPR '.'                                                {% do
                                                                                                        sc <- ST.currentScope

                                                                                                        let name  = Tk.cleanedString $2
                                                                                                            entry = createConstEntry name sc Ast.IntT
                                                                                                            pos   = Tk.position $2

                                                                                                        symT  <- get
                                                                                                        mInfo <- ST.lookup name

                                                                                                        case mInfo of
                                                                                                            Nothing -> put $ ST.insertST symT entry
                                                                                                            Just info ->
                                                                                                                if ST.scope info /= sc && ST.category info /= ST.Function && ST.category info /= ST.Alias
                                                                                                                    then put $ ST.insertST symT entry
                                                                                                                    else ST.insertError $ Err.PE (Err.RedeclaredName name pos)

                                                                                                        return (name, $7, $9)
                                                                                                    }

WHILE :: { Ast.Instruction }
    : while EXPR whileDec CODE_BLOCK endWhile                                                       { Ast.While $2 (reverse $4) }

-- Expresions --

EXPR :: { Ast.Expression }
    : EXPR '+' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Sum $1 $3 }
    | EXPR '-' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Sub $1 $3 }
    | EXPR '*' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Prod $1 $3 }
    | EXPR '/' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Div $1 $3 }
    | EXPR '%' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Mod $1 $3 }
    | EXPR '=' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Eq $1 $3 }
    | EXPR '!=' EXPR                                                                                { createExpression $2 $ Ast.BinOp Ast.Neq $1 $3 }
    | EXPR '<' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Lt $1 $3 }
    | EXPR '>' EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.Gt $1 $3 }
    | EXPR '<=' EXPR                                                                                { createExpression $2 $ Ast.BinOp Ast.Leq $1 $3 }
    | EXPR '>=' EXPR                                                                                { createExpression $2 $ Ast.BinOp Ast.Geq $1 $3 }
    | EXPR and EXPR                                                                                 { createExpression $2 $ Ast.BinOp Ast.And $1 $3 }
    | EXPR or EXPR                                                                                  { createExpression $2 $ Ast.BinOp Ast.Or $1 $3 }
    | EXPR '~'                                                                                      { createExpression $2 $ Ast.UnOp Ast.Neg $1 }
    | deref EXPR                                                                                    { createExpression $1 $ Ast.UnOp Ast.Deref $2 }
    | '[' EXPRLIST ']' EXPR                                                                         { createExpression $3 $ Ast.AccesIndex $4 (reverse $2) }
    | id '<-' EXPR                                                                                  { createExpression $2 $ Ast.AccesField $3 (Tk.cleanedString $1) }
    | EXPR '->' id                                                                                  { createExpression $2 $ Ast.AccesField $1 (Tk.cleanedString $3) }
    | EXPR '?' id                                                                                   { createExpression $2 $ Ast.ActiveField $1 (Tk.cleanedString $3) }
    | '[(' naturalLit ']' EXPR                                                                      { createExpression $3 $ Ast.TupleIndex $4 ((read $ Tk.cleanedString $2) :: Int) }
    | EXPR cast TYPE                                                                                { createExpression $2 $ Ast.Cast $1 $3 }
    | '(' EXPR ')'                                                                                  { $2 }
    | ARRAYLIT                                                                                      { $1 }
    | TUPLELIT                                                                                      { $1 }
    | FUNCTIONCALL                                                                                  { $1 }
    | intLit                                                                                        { createExpression $1 $ Ast.IntLit ((read $ Tk.cleanedString $1) :: Int) }
    | floatLit                                                                                      { createExpression $1 $ Ast.FloatLit ((read $ Tk.cleanedString $1) :: Float) }
    | charLit                                                                                       { createExpression $1 $ Ast.CharLit $ head $ Tk.cleanedString $1 }
    | atomLit                                                                                       { createExpression $1 $ Ast.AtomLit $ Tk.cleanedString $1 }
    | stringLit                                                                                     { createExpression $1 $ Ast.StringLit $ Tk.cleanedString $1 }
    | true                                                                                          { createExpression $1 $ Ast.TrueLit }
    | false                                                                                         { createExpression $1 $ Ast.FalseLit }
    | null                                                                                          { createExpression $1 $ Ast.NullLit }
    | id                                                                                            {% do
                                                                                                        let name = Tk.cleanedString $1
                                                                                                            pos  = Tk.position $1
                                                                                                            
                                                                                                        symT <- get
                                                                                                        mInfo <- ST.lookup name

                                                                                                        case mInfo of
                                                                                                            Nothing -> ST.insertError $ Err.PE (Err.UndefinedVar name pos)
                                                                                                            Just info ->
                                                                                                                case ST.category info of
                                                                                                                    ST.Constant  -> return ()
                                                                                                                    ST.Variable  -> return ()
                                                                                                                    ST.Parameter -> return ()
                                                                                                                    c -> ST.insertError $ Err.PE (Err.InvalidVar (show c) name pos)
                                                                                                        return $ createExpression $1 $ Ast.IdExpr $ Tk.cleanedString $1
                                                                                                    }

FUNCTIONCALL :: { Ast.Expression }
    : id '((' procCallArgs EXPRLIST '))'                                                            {% checkFunctionCall $2 $1 (reverse $4) }
    | id '((' procCallArgs void '))'                                                                {% checkFunctionCall $2 $1 [] }
    | id '(('  '))'                                                                                 {% checkFunctionCall $2 $1 [] }

ARRAYLIT :: { Ast.Expression }
    : '{{' EXPRLIST '}}'                                                                            { createExpression $1 $ Ast.ArrayLit $ reverse $2 }
    | '{{' '}}'                                                                                     { createExpression $1 $ Ast.ArrayLit [] }

TUPLELIT :: { Ast.Expression }
    : '[[' EXPRLIST ']]'                                                                            { createExpression $1 $ Ast.TupleLit $ reverse $2 }
    | '[[' ']]'                                                                                     { createExpression $1 $ Ast.TupleLit [] }

EXPRLIST :: { [Ast.Expression] }
    : EXPR                                                                                          { [$1] }
    | EXPRLIST ',' EXPR                                                                             { $3 : $1 }

OPEN_SCOPE :: { () }
    : {- empty -}                                                                                   {% ST.openScope }

CLOSE_SCOPE :: { () }
    : {- empty -}                                                                                   {% ST.closeScope }

{
parseError :: [Tk.Token] -> ST.MonadParser a -- OJO
parseError []     = do ST.insertError $ Err.PE Err.SyntaxErrEOF
                       fail "Parse error at EOF."
parseError (tk:_) = do ST.insertError $ Err.PE (Err.SyntaxErr tk)
                       fail $ "error: parse error with: \"" ++ Tk.cleanedString tk 
                             ++ "\" at position " ++ show (Tk.position tk) 
                             ++ "related to token: " ++ show (Tk.aToken tk)


-- ST.insertError $ Err.PE (Err.RedeclaredVar name pos)

createExpression :: Tk.Token -> Ast.Expr -> Ast.Expression
createExpression tk expr = Ast.Expression { Ast.getToken = tk, Ast.getExpr = expr, Ast.getType = Ast.AliasT "undefined" }

createVarEntry :: ST.Symbol -> Int -> Ast.Type -> ST.Entry
createVarEntry name scope tp = (name, info)
  where
    info = ST.SymbolInfo
        { ST.category = ST.Variable
        , ST.scope = scope
        , ST.tp = Just tp
        , ST.additional = Nothing
        }

createParamEntry :: ST.Symbol -> Int -> Ast.Type -> Ast.ParamType -> ST.Entry
createParamEntry name scope tp ptp = (name, info)
  where
    info = ST.SymbolInfo
        { ST.category = ST.Parameter
        , ST.scope = scope
        , ST.tp = Just tp
        , ST.additional = Just $ ST.PassType ptp
        }

createConstEntry :: ST.Symbol -> Int -> Ast.Type -> ST.Entry
createConstEntry sym scope tp = (sym, info)
  where
    info = ST.SymbolInfo
        { ST.category = ST.Constant
        , ST.scope = scope
        , ST.tp = Just tp
        , ST.additional = Nothing
        }


-- TODO: Stop ignoring size expressions
maybeInsertVar :: Tk.Token -> Ast.Type -> Maybe [Ast.Expression] -> ST.MonadParser Ast.VariableDeclaration
maybeInsertVar tk tp mSizes = do
    sc <- ST.currentScope
    let name = Tk.cleanedString tk
        pos = Tk.position tk
        entry = createVarEntry name sc tp

    symT  <- get
    mInfo <- ST.lookup name

    case mInfo of
        Nothing -> put $ ST.insertST symT entry
        Just info ->
            if ST.scope info /= sc && ST.category info /= ST.Function && ST.category info /= ST.Alias
                then put $ ST.insertST symT entry
                else ST.insertError $ Err.PE (Err.RedeclaredVar name pos)
    case mSizes of
        Nothing -> return $ Ast.SimpleVarDeclaration name tp
        Just sz -> return $ Ast.ArrayVarDeclaration name tp sz

maybeInsertConst :: Tk.Token -> Ast.Type -> Ast.Expression -> ST.MonadParser Ast.Declaration
maybeInsertConst tk tp expr = do
    sc <- ST.currentScope
    let name  = Tk.cleanedString tk
        pos   = Tk.position tk
        entry = createConstEntry name sc tp

    symT  <- get
    mInfo <- ST.lookup name

    case mInfo of
        Nothing -> put $ ST.insertST symT entry
        Just info ->
            if ST.scope info /= sc && ST.category info /= ST.Function && ST.category info /= ST.Alias
                then put $ ST.insertST symT entry
                else ST.insertError $ Err.PE (Err.RedeclaredConstant name pos)
    return $ Ast.ConstantDeclaration name tp expr


checkFunctionCall :: Tk.Token -> Tk.Token -> [Ast.Expression] -> ST.MonadParser Ast.Expression
checkFunctionCall tkPar tkId exprs = do
    let name   = Tk.cleanedString tkId
        pos    = Tk.position tkId
        params = length exprs

    mInfo <- ST.lookupFunction name params

    case mInfo of
        Nothing -> ST.insertError $ Err.PE (Err.UndefinedFunction name pos)
        Just info ->
            case ST.category info of
                ST.Function -> return ()
                c -> ST.insertError $ Err.PE (Err.ExpectedFunction (show c) name pos) 
    return $ createExpression tkPar $ Ast.FuncCall name exprs
}
