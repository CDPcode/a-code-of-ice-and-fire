{
module Westeros.SouthOfTheWall.Parser (parse) where

import qualified Westeros.SouthOfTheWall.AST as AST
import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.TypeVer as T
import Data.Maybe (fromJust)
import Control.Monad.RWS ( MonadState(put, get), RWST, when )

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
    : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                         { Ast.Program $3 (reverse $4) $5 }
    | ALIASES HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                 { Ast.Program $4 (reverse $5) $6 }

HEADER :: { }
    : programStart programName                                                      { Tk.cleanedString $2 }

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
                                                                                            Just info -> case ST.defined $ ST.getFunctionMetaData info of
                                                                                                True -> return ()
                                                                                                False -> ST.insertError $ Err.PE (Err.UndefinedFunction name pos)
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
    : id OPEN_SCOPE FUNCTION_PARAMETERS FUNCTION_RETURN FUNCTION_BODY CLOSE_SCOPE   { AST.FunctionDeclaration $5 }

FUNCTION_PARAMETERS :: { }
    : beginFuncParams PARAMETER_LIST endFuncParams                                  { }

PARAMETER_LIST :: { }
    : void                                                                          { }
    | PARAMETERS                                                                    { }

PARAMETERS :: { }
    : PARAMETER                                                                     { }
    | PARAMETERS ',' PARAMETER                                                      { }

PARAMETER :: { }
    : PARAMETER_TYPE id type TYPE                                                   { insertParam $2 $4 $1 }

PARAMETER_TYPE :: { ST.ParameterType }
    : valueArg                                                                      { ST.Value }
    | refArg                                                                        { ST.Ref }

FUNCTION_RETURN :: { [ST.Type] }
    : beginReturnVals RETURN_TYPES endReturnVals                                    { $2 }

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

TUPLE_TYPES :: { }
    : {- empty -}                                                                   { [] }
    | TYPES                                                                         { reverse $1 }

-- Alias Declaration --
DECLARATIONS :: { [AST.Instruction] }
    : {- empty -}                                                                   { [] }
    | DECLARATIONS DECLARATION                                                      { case $2 of
                                                                                        Nothing -> $1
                                                                                        Just AST.Instruction -> $2 : $1 }
    | DECLARATIONS comment                                                          { $1 }

DECLARATION :: { Maybe AST.Instruction }
    : SIMPLE_DECLARATION '.'                                                        { return Nothing }
    | SIMPLE_DECLARATION ':=' EXPR '.'                                              {  }
    | SIMPLE_DECLARATION ':==' EXPR '.'                                             {  }
    | CONST_DECLARATION '.'                                                         { return Nothing }

SIMPLE_DECLARATIONS :: { }
    : SIMPLE_DECLARATION                                                            {  }
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                    {  }

SIMPLE_DECLARATION :: {  }
    : PRIMITIVE_DECLARATION                                                         { }
    | COMPOSITE_DECLARATION                                                         { }

PRIMITIVE_DECLARATION :: {  }
    : var id type TYPE                                                              {
                                                                                    }   

COMPOSITE_DECLARATION :: {()}
    : beginCompTypeId var id endCompTypeId TYPE                                     { }
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz              { }
    | beginCompTypeId pointerVar id endCompTypeId TYPE                              { }
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz       { }

CONST_DECLARATION :: { }
    : const id type TYPE constValue EXPR                                            {% do 
                                                                                        let symbol = Tk.cleanedString $2
                                                                                            entry = lookupST symbol 
                                                                                        case entry of 
                                                                                            Nothing -> do 
                                                                                                -- TypeChecking missing
                                                                                                -- Need to check that type is Primitive
                                                                                                insertId $2 ST.Constant $4                                                                                            
                                                                                            Just info -> do
                                                                                                if scope info == ST.currentScope
                                                                                                    then ST.insertError $ Err.PE (Err.RedeclaredName symbol (Tk.position $2))
                                                                                                    else do
                                                                                                        -- TypeChecking missing
                                                                                                        insertId $2 ST.Constant $4 
                                                                                        -- return missing
                                                                                    }
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                   { % do 
                                                                                        let symbol = Tk.cleanedString $2
                                                                                            entry = lookupST symbol 
                                                                                        case entry of 
                                                                                            Nothing -> do 
                                                                                                -- TypeChecking missing
                                                                                                -- Need to check that type is Composed
                                                                                                insertId $2 ST.Constant $4                                                                                            
                                                                                            Just info -> do
                                                                                                if scope info == ST.currentScope
                                                                                                    then ST.insertError $ Err.PE (Err.RedeclaredName symbol (Tk.position $2))
                                                                                                    else do
                                                                                                        -- TypeChecking missing
                                                                                                        insertId $2 ST.Constant $4 
                                                                                        -- return missing
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
    : EXPR ':=' EXPR '.'                                                            { AST.SimpleAssign $1 $3 }
    | EXPRLIST ':==' EXPR '.'                                                       { AST.MultAssign (reverse $1) $3 }
    | void ':=' EXPR '.'                                                            { AST.FuncCallInst $3 }
    | void ':==' EXPR '.'                                                           { AST.FuncCallInst $3 }
    | pass '.'                                                                      { AST.EmptyInst }
    | beginExit programName endExit '.'                                             { AST.ExitInst (Tk.cleanedString $3) }
    | read EXPR '.'                                                                 { AST.Read $2 }
    | print EXPR '.'                                                                { AST.Print $2}
    | EXPR new '.'      {% do 
                            ptrType = AST.getType $1 

                            case ptrType of 
                                T.PointerT _ -> return ()
                                _            -> do 
                                    let error = Err.InvalidNew (show ptrType) $2  
                                    ST.insertError $ Err.TE error

                            return AST.Free $1 
 
                            AST.New $1 
                        }
    | EXPR free '.'     {% do  
                            ptrType = AST.getType $1 

                            case ptrType of 
                                T.PointerT _ -> return ()
                                _            -> do 
                                    let error = Err.InvalidFree (show ptrType) $2  
                                    ST.insertError $ Err.TE error

                            return AST.Free $1 
                        }
    | continue '.'                                                                  { AST.Continue }
    | break '.'                                                                     { AST.Break }
    | returnOpen EXPRLIST returnClose                                               { AST.Return (reverse $2) }
    | returnOpen returnClose                                                        { AST.Return [] }
    | IF '.'                                                                        { AST.If $1 }
    | SWITCHCASE '.'                                                                { $1 }
    | FOR '.'                                                                       { $1 }
    | WHILE '.'                                                                     { $1 }
    | DECLARATION                                                                   { AST.DeclarationInst $1 }

IF :: { AST.IfInst }
    : if EXPR then CODE_BLOCK endif                     {% do 
                                                            let exprType = AST.getType $2

                                                            when ( exprType /=T.BoolT ) $ do
                                                                let exprType = AST.getType $2
                                                                    error    = Err.InvalidIfTykpe (show exprType) (Tk.position $1)
                                                                ST.insertError $ Err.TE error
                                                          
                                                            AST.IfThen $2 (reverse $4) 
                                                        }
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif     {% do 

                                                            let exprType = AST.getType $2

                                                            when ( exprType /=T.BoolT ) $ do
                                                                let exprType = AST.getType $2
                                                                    error    = Err.InvalidIfTykpe (show exprType) (Tk.position $1)
                                                                ST.insertError $ Err.TE error

                                                            AST.IfThenElse $2 (reverse $4) (reverse $6) 
                                                        }

SWITCHCASE :: { AST.Instruction }
    : switch EXPR switchDec '.' CASES endSwitch     {% do 
                                                        let switchExprType = AST.getType $2 

                                                        when ( switchExprType /= T.AtomT ) $ do 

                                                            let error = Err.WrongSwitchType (show switchExprType) (Tk.position $1)
                                                            ST.insertError $ Err.PE error

                                                        return $ AST.Switch $2 (reverse $5) 
                                                    }

CASES :: { [AST.Case] }
    : CASE                                                                                          { [$1] }
    | CASES CASE                                                                                    { $2 : $1 }

CASE :: { AST.Case }
    : case atomLit '.' CODE_BLOCK                                                                   { AST.Case (Tk.cleanedString $2) (reverse $4) }
    | case nothing '.' CODE_BLOCK                                                                   { AST.Default (reverse $4) }

FOR :: { AST.Instruction }
    : OPEN_SCOPE FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE                                            { let (id, lb, ub) = $2 in AST.For id lb ub (reverse $3) }

FOR_DEC :: { (AST.Id, AST.Expression, AST.Expression) }
    : for id type int '.' forLB EXPR forUB EXPR '.'       {% do
                                  
                                                            ST.insertId $2 Variable ST.int
                                              
                                                            let lbType = AST.getType $7
                                                                ubType = AST.getType $9
                                              
                                                            when (lbType /= ubType || lbType != T.IntT || rbType != T.IntT) $ do
                                                               
                                                                let lb    = show lbType
                                                                    ub    = show ubType
                                                                    error = Err.MismatchingForBounds lb ub (Tk.position $1)
                                              
                                                                ST.insertError $ Err.TE error
                                              
                                                            return (Tk.cleanedString $2, $7, $9)
                                                          }


WHILE :: { AST.Instruction }
    : while EXPR whileDec CODE_BLOCK endWhile       {% do
                                                        let exprType = AST.getType $2 

                                                        when ( exprType /=T.BoolT ) $ do
                                                                let exprType = AST.getType $2
                                                                    error    = Err.InvalidWhileType (show exprType) (Tk.position $1)
                                                                ST.insertError $ Err.TE error
                                                                
                                                        return $ AST.While $2 (reverse $4) 
                                                    }

-- Expresions --

EXPR :: { AST.Expression }
    : EXPR '+' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Sub $1 $3 }
    | EXPR '-' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Sub $1 $3 }
    | EXPR '*' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Prod $1 $3 }
    | EXPR '/' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Div $1 $3 }
    | EXPR '%' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Mod $1 $3 }
    | EXPR '=' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Eq $1 $3 }
    | EXPR '!=' EXPR             {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Neq $1 $3 }
    | EXPR '<' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Lt $1 $3 }
    | EXPR '>' EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Gt $1 $3 }
    | EXPR '<=' EXPR             {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Leq $1 $3 }
    | EXPR '>=' EXPR             {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Geq $1 $3 }
    | EXPR and EXPR              {% T.buildAndCheckExpr $2 $ AST.BinOp AST.And $1 $3 }
    | EXPR or EXPR               {% T.buildAndCheckExpr $2 $ AST.BinOp AST.Or $1 $3 }
    | EXPR '~'                   {% T.buildAndCheckExpr $2 $ AST.UnOp AST.Neg $1 }
    | deref EXPR                 {% T.buildAndCheckExpr $1 $ AST.UnOp AST.Deref $2 }
    | '[' EXPRLIST ']' EXPR      {% T.buildAndCheckExpr $3 $ AST.AccesIndex $4 (reverse $2) }
    | id '<-' EXPR               {% T.buildAndCheckExpr $2 $ AST.AccesField $3 (Tk.cleanedString $1) }
    | EXPR '->' id               {% T.buildAndCheckExpr $2 $ AST.AccesField $1 (Tk.cleanedString $3) }
    | EXPR '?' id                {% T.buildAndCheckExpr $2 $ AST.ActiveField $1 (Tk.cleanedString $3) }
    | '[(' naturalLit ']' EXPR   {% T.buildAndCheckExpr $3 $ AST.TupleIndex $4 ((read $ Tk.cleanedString $2) :: Int) }
    | EXPR cast TYPE             {% T.buildAndCheckExpr $2 $ AST.Cast $1 $3 }
    | '(' EXPR ')'               { $2 }
    | ARRAYLIT                   { $1 }
    | TUPLELIT                   { $1 }
    | FUNCTIONCALL               { $1 }
    | intLit                     {% buildAndCheckExpr $1 $ AST.IntLit ((read $ Tk.cleanedString $1) :: Int) }
    | floatLit                   {% buildAndCheckExpr $1 $ AST.FloatLit ((read $ Tk.cleanedString $1) :: Float) }
    | charLit                    {% buildAndCheckExpr $1 $ AST.CharLit $ head $ Tk.cleanedString $1 }
    | atomLit                    {% buildAndCheckExpr $1 $ AST.AtomLit $ Tk.cleanedString $1 }
    | stringLit                  {% buildAndCheckExpr $1 $ AST.StringLit $ Tk.cleanedString $1 }
    | true                       {% buildAndCheckExpr $1 $ AST.TrueLit }
    | false                      {% buildAndCheckExpr $1 $ AST.FalseLit }
    | null                       {% buildAndCheckExpr $1 $ AST.NullLit }
    | id                         {% buildAndCheckExpr $1 $ AST.IdExpr $1 }

FUNCTIONCALL :: { AST.Expression }
    : id '((' procCallArgs EXPRLIST '))'   {% buildAndCheckExpr $1 $ FuncCall $1 (reverse $4) }
    | id '((' procCallArgs void '))'       {% buildAndCheckExpr $1 $ FuncCall $1 [] }
    | id '(('  '))'                        {% buildAndCheckExpr $1 $ FuncCall $1 [] }

ARRAYLIT :: { AST.Expression }
    : '{{' EXPRLIST '}}'                   {% buildAndCheckExpr $1 $ AST.ArrayLit $ reverse $2 }
    | '{{' '}}'                            {% buildAndCheckExpr $1 $ AST.ArrayLit [] }

TUPLELIT :: { AST.Expression }
    : '[[' EXPRLIST ']]'                   {% buildAndCheckExpr $1 $ AST.TupleLit $ reverse $2 }
    | '[[' ']]'                            {% buildAndCheckExpr $1 $ AST.TupleLit [] }

EXPRLIST :: { [AST.Expression] }
    : EXPR                                 { [$1] }
    | EXPRLIST ',' EXPR                    { $3 : $1 }


OPEN_SCOPE :: { Int }
    :  {- empty -}                                                                                  {% do
                                                                                                        ST.openScope
                                                                                                        ST.currentScope
                                                                                                    }

CLOSE_SCOPE :: { () }
    :  {- empty -}                                                                                  {% ST.closeScope }

{
parseError :: [Tk.Token] -> ST.MonadParser a -- OJO
parseError []     = do ST.insertError $ Err.PE Err.SyntaxErrEOF
                       fail "Parse error at EOF."
parseError (tk:_) = do ST.insertError $ Err.PE (Err.SyntaxErr tk)
                       fail $ "error: parse error with: \"" ++ Tk.cleanedString tk
                             ++ "\" at position " ++ show (Tk.position tk)
                             ++ "related to token: " ++ show (Tk.aToken tk)

checkFunctionCall :: Tk.Token -> Tk.Token -> [AST.Expression] -> ST.MonadParser AST.Expression
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
