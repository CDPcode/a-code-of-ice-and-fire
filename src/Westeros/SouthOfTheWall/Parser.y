{
module Westeros.SouthOfTheWall.Parser (parse) where
    import qualified Westeros.SouthOfTheWall.Tokens as Tk
}

%name                parse 
%tokentype            { Tk.Token }
%error                { parseError }
-- TODO: %monad expr to properly handle errors

-- Token aliases definitions
token %

    comment           { Tk.Token  { Tk.aToken=TknComment } }
    str               { Tk.Token  { Tk.aToken=TknString } } 

    -- Type tokens
    beginProgram      { Tk.Token  { Tk.aToken=TknProgramStart } }
    programName       { Tk.Token  { Tk.aToken=TknProgramName } }
    var               { Tk.Token  { Tk.aToken=TknVar } }
    const             { Tk.Token  { Tk.aToken=TknConst } }
    type              { Tk.Token  { Tk.aToken=TknType } }
    beginAlias        { Tk.Token  { Tk.aToken=TknBeginAlias } }
    strongAlias       { Tk.Token  { Tk.aToken=TknStrongAlias } }
    weakAlias         { Tk.Token  { Tk.aToken=TknWeakAlias } }
    int               { Tk.Token  { Tk.aToken=TknInt } }
    intLit            { Tk.Token  { Tk.aToken=TknIntLit } }
    float             { Tk.Token  { Tk.aToken=TknFloat } }
    floatLit          { Tk.Token  { Tk.aToken=TknFloatLit } }
    trilean           { Tk.Token  { Tk.aToken=TknTrilean } }
    true              { Tk.Token  { Tk.aToken=TknTrue } }
    neutral           { Tk.Token  { Tk.aToken=TknNeutral } }
    false             { Tk.Token  { Tk.aToken=TknFalse } }
    char              { Tk.Token  { Tk.aToken=TknChar } }
    charLit           { Tk.Token  { Tk.aToken=TknCharLit } }
    beginCompType     { Tk.Token  { Tk.aToken=TknBeginCompType } }
    endCompType       { Tk.Token  { Tk.aToken=TknEndIDCompType } }
    struct            { Tk.Token  { Tk.aToken=TknStruct } }
    union             { Tk.Token  { Tk.aToken=TknUnion } }
    array             { Tk.Token  { Tk.aToken=TknArray } }
    stringLit         { Tk.Token  { Tk.aToken=TknStringLit } }
    pointer           { Tk.Token  { Tk.aToken=TknPointer } }
    arraySz           { Tk.Token  { Tk.aToken=TknArraySize } }
    stringSz          { Tk.Token  { Tk.aToken=TknStringSize } }
    arrayDecl         { Tk.Token  { Tk.aToken=TknArrayDecl } }
    stringDecl        { Tk.Token  { Tk.aToken=TknStringDecl } }

    -- Assignment tokens
    ':='              { Tk.Token  { Tk.aToken=TknAssign } }
    beginMultAssign   { Tk.Token  { Tk.aToken=TknBeginMultAssign } }
    endMultAssign     { Tk.Token  { Tk.aToken=TknEndMultAssign } }
    tupleAssign       { Tk.Token  { Tk.aToken=TknTupleAsign } }

    -- Operators tokens
    '+'               { Tk.Token  { Tk.aToken=TknPlus } }
    '-'               { Tk.Token  { Tk.aToken=TknMinus } }
    '*'               { Tk.Token  { Tk.aToken=TknMult } }
    '-'               { Tk.Token  { Tk.aToken=TknMod } }
    '~'               { Tk.Token  { Tk.aToken=TknNegate } }
    '='               { Tk.Token  { Tk.aToken=TknEqual } }
    '!='              { Tk.Token  { Tk.aToken=TknNotEqual } }
    '<'               { Tk.Token  { Tk.aToken=TknLessThan } }
    '>'               { Tk.Token  { Tk.aToken=TknGreaterThan } }
    '<='              { Tk.Token  { Tk.aToken=TknLessEqThan } }
    '>='              { Tk.Token  { Tk.aToken=TknGreaterEqThan } }
    beginExit         { Tk.Token  { Tk.aToken=TknBeginExit } }
    endExit           { Tk.Token  { Tk.aToken=TknEndExit } }
    not               { Tk.Token  { Tk.aToken=TknNot } }
    and               { Tk.Token  { Tk.aToken=TknAnd } }
    or                { Tk.Token  { Tk.aToken=TknOr } }
    -- Do we need these?
    '<=>'             { Tk.Token  { Tk.aToken=TknBoolEqual } }
    '<!=>'            { Tk.Token  { Tk.aToken=TknBoolNotEqual } }

    -- Composite Types Operators
    structField       { Tk.Token  { Tk.aToken=TknStructField } }
    beginUnionQ       { Tk.Token  { Tk.aToken=TknBeginUnionQuestion } }
    unionQ            { Tk.Token  { Tk.aToken=TknUnionQuestion } }
    endUnionQ         { Tk.Token  { Tk.aToken=TknEndUnionQuestion } }
    unionField        { Tk.Token  { Tk.aToken=TknUnionField } }
    beginIdx          { Tk.Token  { Tk.aToken=TknBeginIndex } }
    tupleIdx          { Tk.Token  { Tk.aToken=TknBeginTupleIndex } }
    endIdx            { Tk.Token  { Tk.aToken=TknEndIndex } }

    -- IO tokens
    read              { Tk.Token  { Tk.aToken=TknRead } }
    print             { Tk.Token  { Tk.aToken=TknPrint } }

    -- Procedure Tokens
    beginF            { Tk.Token  { Tk.aToken=TknBeginFuncDecl } }
    listF             { Tk.Token  { Tk.aToken=TknListFunction } }
    fMain             { Tk.Token  { Tk.aToken=TknFirstMain } }
    sMain             { Tk.Token  { Tk.aToken=TknLastMain } }
    pass              { Tk.Token  { Tk.aToken=TknPass } } -- OJO
    fArgs             { Tk.Token  { Tk.aToken=TknFunctionArgs } }
    beginReturn       { Tk.Token  { Tk.aToken=TknBeginReturnVals } }
    endReturn         { Tk.Token  { Tk.aToken=TknEndReturnVals } }
    returnOpen        { Tk.Token  { Tk.aToken=TknReturnOpen } }
    returnClose       { Tk.Token  { Tk.aToken=TknReturnClose } }
    fCallOpen         { Tk.Token  { Tk.aToken=TknProcCallOpen } }
    fCallArgs         { Tk.Token  { Tk.aToken=TknProcCallArgs } }
    fCallClose        { Tk.Token  { Tk.aToken=TknProcCallClose } }
    val               { Tk.Token  { Tk.aToken=TknValueArg } }
    ref               { Tk.Token  { Tk.aToken=TknReferenceArg } }

    -- Blocks
    '{'               { Tk.Token  { Tk.aToken=TknOpenBlock } }
    '}'               { Tk.Token  { Tk.aToken=TknCloseBlock } }

    void              { Tk.Token  { Tk.aToken=TknVoid } }

    -- Repetition Tokens
    for               { Tk.Token  { Tk.aToken=TknFor } }
    forLb             { Tk.Token  { Tk.aToken=TknForLB } }
    forUb             { Tk.Token  { Tk.aToken=TknForUB } }
    while             { Tk.Token  { Tk.aToken=TknWhile } }
    whileDec          { Tk.Token  { Tk.aToken=TknWhileDecoration } }
    continue          { Tk.Token  { Tk.aToken=TknContinue } }
    break             { Tk.Token  { Tk.aToken=TknBreak } } 

    -- Selection Tokens
    beginSel          { Tk.Token  { Tk.aToken=TknBeginSelection } }
    decSel            { Tk.Token  { Tk.aToken=TknSelectionDecorator } }
    trueBranch        { Tk.Token  { Tk.aToken=TknTrueBranch } }
    neutralBranch     { Tk.Token  { Tk.aToken=TknUnknownBranch } }
    falseBranch       { Tk.Token  { Tk.aToken=TknFalseBranch } } 
    endSel            { Tk.Token  { Tk.aToken=TknEndSelection } }

    -- Identifiers
    id                { Tk.Token  { Tk.aToken=TknID } }
    argCnt            { Tk.Token  { Tk.aToken=TknArgNumber } }

    -- Punctuation
    ','               { Tk.Token  { Tk.aToken=TknComma } }
    '.'               { Tk.Token  { Tk.aToken=TknDot } }
    
    -- Expressions
    '('               { Tk.Token  { Tk.aToken=TknOpenParenthesis } }
    ')'               { Tk.Token  { Tk.aToken=TknCloseParenthesis } }


-- Precedences

%% -- Grammar

{
    -- Helper functions

-- TODO:
-- Error Function
-- parseError :: [TokenPos] -> a
}