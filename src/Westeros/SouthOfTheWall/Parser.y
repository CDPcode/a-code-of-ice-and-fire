{
module Westeros.SouthOfTheWall.Parser (parse) where

import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.Grammar as G
}

%name                parse 
%tokentype            { Tk.Token }
%error                { parseError }
-- TODO: %monad expr to properly handle errors

-- Token aliases definitions 
%token

    comment           { Tk.Token  { Tk.aToken=Tk.TknComment } }
    str               { Tk.Token  { Tk.aToken=Tk.TknString } } 
                                               
    -- Type tokens                             
    beginProgram      { Tk.Token  { Tk.aToken=Tk.TknProgramStart } }
    programName       { Tk.Token  { Tk.aToken=Tk.TknProgramName } }
    var               { Tk.Token  { Tk.aToken=Tk.TknVar } }
    const             { Tk.Token  { Tk.aToken=Tk.TknConst } }
    type              { Tk.Token  { Tk.aToken=Tk.TknType } }
    beginAlias        { Tk.Token  { Tk.aToken=Tk.TknBeginAlias } }
    strongAlias       { Tk.Token  { Tk.aToken=Tk.TknStrongAlias } }
    weakAlias         { Tk.Token  { Tk.aToken=Tk.TknWeakAlias } }

    -- Simple                                  
    int               { Tk.Token  { Tk.aToken=Tk.TknInt } }
    intLit            { Tk.Token  { Tk.aToken=Tk.TknIntLit } }
    float             { Tk.Token  { Tk.aToken=Tk.TknFloat } }
    floatLit          { Tk.Token  { Tk.aToken=Tk.TknFloatLit } }
    trilean           { Tk.Token  { Tk.aToken=Tk.TknTrilean } }
    true              { Tk.Token  { Tk.aToken=Tk.TknTrue } }
    neutral           { Tk.Token  { Tk.aToken=Tk.TknNeutral } }
    false             { Tk.Token  { Tk.aToken=Tk.TknFalse } }
    char              { Tk.Token  { Tk.aToken=Tk.TknChar } }
    charLit           { Tk.Token  { Tk.aToken=Tk.TknCharLit } }

    -- Compound                                
    beginCompType     { Tk.Token  { Tk.aToken=Tk.TknBeginCompType } }
    endCompType       { Tk.Token  { Tk.aToken=Tk.TknEndIDCompType } }
    struct            { Tk.Token  { Tk.aToken=Tk.TknStruct } }
    union             { Tk.Token  { Tk.aToken=Tk.TknUnion } }
    array             { Tk.Token  { Tk.aToken=Tk.TknArray } }
    stringLit         { Tk.Token  { Tk.aToken=Tk.TknStringLit } }
    pointer           { Tk.Token  { Tk.aToken=Tk.TknPointer } }
    arraySz           { Tk.Token  { Tk.aToken=Tk.TknArraySize } }
    stringSz          { Tk.Token  { Tk.aToken=Tk.TknStringSize } }
    arrayDecl         { Tk.Token  { Tk.aToken=Tk.TknArrayDecl } }
    stringDecl        { Tk.Token  { Tk.aToken=Tk.TknStringDecl } }
                                               
    -- Assignment tokens                       
    ':='              { Tk.Token  { Tk.aToken=Tk.TknAssign } }
    beginMultAssign   { Tk.Token  { Tk.aToken=Tk.TknBeginMultAssign } }
    endMultAssign     { Tk.Token  { Tk.aToken=Tk.TknEndMultAssign } }
    tupleAssign       { Tk.Token  { Tk.aToken=Tk.TknTupleAsign } }
                                               
    -- Operators tokens                        
    '+'               { Tk.Token  { Tk.aToken=Tk.TknPlus } }
    '-'               { Tk.Token  { Tk.aToken=Tk.TknMinus } }
    '*'               { Tk.Token  { Tk.aToken=Tk.TknMult } }
    '%'               { Tk.Token  { Tk.aToken=Tk.TknMod } }
    '~'               { Tk.Token  { Tk.aToken=Tk.TknNegate } }
    '='               { Tk.Token  { Tk.aToken=Tk.TknEqual } }
    '!='              { Tk.Token  { Tk.aToken=Tk.TknNotEqual } }
    '<'               { Tk.Token  { Tk.aToken=Tk.TknLessThan } }
    '>'               { Tk.Token  { Tk.aToken=Tk.TknGreaterThan } }
    '<='              { Tk.Token  { Tk.aToken=Tk.TknLessEqThan } }
    '>='              { Tk.Token  { Tk.aToken=Tk.TknGreaterEqThan } }
    beginExit         { Tk.Token  { Tk.aToken=Tk.TknBeginExit } }
    endExit           { Tk.Token  { Tk.aToken=Tk.TknEndExit } }
    not               { Tk.Token  { Tk.aToken=Tk.TknNot } }
    and               { Tk.Token  { Tk.aToken=Tk.TknAnd } }
    or                { Tk.Token  { Tk.aToken=Tk.TknOr } }

    -- Do we need these?                       
    '<=>'             { Tk.Token  { Tk.aToken=Tk.TknBoolEqual } }
    '<!=>'            { Tk.Token  { Tk.aToken=Tk.TknBoolNotEqual } }
                                               
    -- Composite Types Operators               
    structField       { Tk.Token  { Tk.aToken=Tk.TknStructField } }
    beginUnionQ       { Tk.Token  { Tk.aToken=Tk.TknBeginUnionQuestion } }
    unionQ            { Tk.Token  { Tk.aToken=Tk.TknUnionQuestion } }
    endUnionQ         { Tk.Token  { Tk.aToken=Tk.TknEndUnionQuestion } }
    unionField        { Tk.Token  { Tk.aToken=Tk.TknUnionField } }
    beginIdx          { Tk.Token  { Tk.aToken=Tk.TknBeginIndex } }
    tupleIdx          { Tk.Token  { Tk.aToken=Tk.TknBeginTupleIndex } }
    endIdx            { Tk.Token  { Tk.aToken=Tk.TknEndIndex } }
                                               
    -- IO tokens                               
    read              { Tk.Token  { Tk.aToken=Tk.TknRead } }
    print             { Tk.Token  { Tk.aToken=Tk.TknPrint } }
                                               
    -- Procedure Tokens                        
    beginF            { Tk.Token  { Tk.aToken=Tk.TknBeginFuncDecl } }
    listF             { Tk.Token  { Tk.aToken=Tk.TknListFunction } }
    fMain             { Tk.Token  { Tk.aToken=Tk.TknFirstMain } }
    sMain             { Tk.Token  { Tk.aToken=Tk.TknLastMain } }
    pass              { Tk.Token  { Tk.aToken=Tk.TknPass } } -- OJO
    fArgs             { Tk.Token  { Tk.aToken=Tk.TknFunctionArgs } }
    beginReturn       { Tk.Token  { Tk.aToken=Tk.TknBeginReturnVals } }
    endReturn         { Tk.Token  { Tk.aToken=Tk.TknEndReturnVals } }
    returnOpen        { Tk.Token  { Tk.aToken=Tk.TknReturnOpen } }
    returnClose       { Tk.Token  { Tk.aToken=Tk.TknReturnClose } }
    fCallOpen         { Tk.Token  { Tk.aToken=Tk.TknProcCallOpen } }
    fCallArgs         { Tk.Token  { Tk.aToken=Tk.TknProcCallArgs } }
    fCallClose        { Tk.Token  { Tk.aToken=Tk.TknProcCallClose } }
    val               { Tk.Token  { Tk.aToken=Tk.TknValueArg } }
    ref               { Tk.Token  { Tk.aToken=Tk.TknReferenceArg } }
                                               
    -- Blocks                                  
    '{'               { Tk.Token  { Tk.aToken=Tk.TknOpenBlock } }
    '}'               { Tk.Token  { Tk.aToken=Tk.TknCloseBlock } }
                                               
    void              { Tk.Token  { Tk.aToken=Tk.TknVoid } }
                                               
    -- Repetition Tokens                       
    for               { Tk.Token  { Tk.aToken=Tk.TknFor } }
    forLb             { Tk.Token  { Tk.aToken=Tk.TknForLB } }
    forUb             { Tk.Token  { Tk.aToken=Tk.TknForUB } }
    while             { Tk.Token  { Tk.aToken=Tk.TknWhile } }
    whileDec          { Tk.Token  { Tk.aToken=Tk.TknWhileDecoration } }
    continue          { Tk.Token  { Tk.aToken=Tk.TknContinue } }
    break             { Tk.Token  { Tk.aToken=Tk.TknBreak } } 
                                               
    -- Selection Tokens                        
    beginSel          { Tk.Token  { Tk.aToken=Tk.TknBeginSelection } }
    decSel            { Tk.Token  { Tk.aToken=Tk.TknSelectionDecorator } }
    trueBranch        { Tk.Token  { Tk.aToken=Tk.TknTrueBranch } }
    neutralBranch     { Tk.Token  { Tk.aToken=Tk.TknUnknownBranch } }
    falseBranch       { Tk.Token  { Tk.aToken=Tk.TknFalseBranch } } 
    endSel            { Tk.Token  { Tk.aToken=Tk.TknEndSelection } }
                                               
    -- Identifiers                             
    id                { Tk.Token  { Tk.aToken=Tk.TknID } }
    argCnt            { Tk.Token  { Tk.aToken=Tk.TknArgNumber } }
                                               
    -- Punctuation                             
    ','               { Tk.Token  { Tk.aToken=Tk.TknComma } }
    '.'               { Tk.Token  { Tk.aToken=Tk.TknDot } }
                                               
    -- Expressions                             
    '('               { Tk.Token  { Tk.aToken=Tk.TknOpenParenthesis } }
    ')'               { Tk.Token  { Tk.aToken=Tk.TknCloseParenthesis } }


-- Precedences and Associativities 

%left '+' '-'
%left '*' '%' -- '/'

%nonassoc '=' '!=' '<' '>' '<=' '>='
%left and or
%right not


%% -- Grammar

-- Program

-- A program is :
--a header
--a prologue
--a (possibly empty) list of function definitions
--      Each function definition holds:
--          a (possibly empty) list of instructions along with selection and repetition
--   an epilogue which is again, a function definition.
--  comments at any point

PROGRAM : HEADER PL F_DEF_LIST EL
    
HEADER : beginProgram programName beginF fMain F_DEC_LIST sMain {}

F_DEC_LIST : {- empty -}  { [] }
           | F_DEC_LIST  F_DEC { }

-- Function declaration
F_DEC : listF tknId argCnt { } 

-- Prologue
PL : {- empty -} {}

F_DEF_LIST : {- empty -} { [] } 
           | F_DEF_LIST F_DEF

-- Function definition: hereby I .. the honorable
F_DEF : fargs ref PARAMS beginReturn RETURN_T endReturn '{' INSTRUCTION_LIST '}' {}

-- Epilogue
EL :  {- empty -} {}
    
-- Parameters: either Nobody or a propper parameter list
PARAMS : void {}
       | PARAM_LIST {}

PARAM_LIST : PARAM {} -- ## no estoy seguro
           | PARAM_LIST ',' PARAM {}

PARAM : val NAMING {} 
      | ref NAMING {}  

-- <id> of House <declarador de tipo> 
NAMING :  id type TYPE {}

DECLARATION_LIST : DECLARATION { [] } 
                 | DECLARATION_LIST ',' DECLARATION {}

-- <lord|lady|knight> of House .. 
DECLARATION : var NAMING {} -- '.' 
            | const NAMING {}  -- '.' 

RETURN_T : void {}
         | TYPE_LIST {} 
         
TYPE_LIST : TYPE {}
          | TYPE_LIST ',' TYPE {}

TYPE : int {} 
     | float {}
     | char {}
     | trilean {}


-- Instructions --

INST_LIST : {- empty -} { [] } 
          | INST_LIST INST

INST : {-empty -} {} 


-- Expressions --

EXPR : intLit {G.IntLit (read (Tk.cleanedString $1)::Int) }  
     | floatLit {G.FloatLit (read (Tk.cleanedString $1)::Float) }
     | char { G.CharLit (head (Tk.cleanedString $1))}
     | true {} 
     | neutral {}
     | false {} 

     | EXPR '+' EXPR {}
     | EXPR '-' EXPR {}
     | EXPR '*' EXPR {}
     | EXPR '%' EXPR {}
     -- | EXPR '/' EXPR {} -- generate token
     | EXPR '=' EXPR {}
     | EXPR '!=' EXPR {}
     | EXPR '<' EXPR {}
     | EXPR '>' EXPR {}
     | EXPR '<=' EXPR {}
     | EXPR '>=' EXPR {}

     | not EXPR {}
     | EXPR and EXPR {}
     | EXPR or EXPR {}
--     beginExit -- es operadores ?
--     endExit   -- es operador ?




{
    -- Helper functions
    -- TODO:
    -- Error Function


parseError :: [Tk.Token] -> a
parseError _ = undefined
}