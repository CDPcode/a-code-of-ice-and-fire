{
module Westeros.SouthOfTheWall.Lexer (
    scanTokens,
    ) where

import Data.Char
import Westeros.SouthOfTheWall.Tokens
}

%wrapper "monadUserState"

-- Macros

$digits = [0-9]
@scapedchars = \\[nt\\\"\'] --"
@linebreaks = \n+\r?\r+\n?
@ws = $white+

-- regex go here

tokens :-
--          Whites
<0>         @ws                                                                                     ;

--          Comments
<0>         Suddenly\,                                                                              { pushToString `andBegin` comment }
<0>         In(@ws)the(@ws)midst(@ws)of                                                             { pushToString `andBegin` comment }
<0>         Therefore                                                                               { pushToString `andBegin` comment }
<comment>   \.                                                                                      { makeCommentToken `andBegin` 0 }
<comment>   .                                                                                       { pushToString }

--          String Literals
<0>         Maester(@ws)reading(@ws)\"                                                              { pushToString `andBegin` string }  --"
<string>    \"                                                                                      { makeStringToken `andBegin` 0 }    --"
<string>    @scapedchars                                                                            { pushScapedToString }
<string>    @linebreaks                                                                             { invalidBreak }
<string>    $printable                                                                              { pushToString }
<string>    .                                                                                       { invalidCharacter }

--          Program Start
<0>         A\ Song\ of\ Ice\ and\ Fire\:                                                           { makeToken TknProgramStart }
<0>         \-\-\ [A-Z][a-z]*([ ][A-Z][a-z]*)* \-\-                                                 { makeToken TknProgramName } 

--          Type Declaration
<0>         Lord                                                                                    { makeToken TknConst }
<0>         Lady                                                                                    { makeToken TknConst }
<0>         Knight                                                                                  { makeToken TknVar }
<0>         Wildling                                                                                { makeToken TknVarPointer }
<0>         of(@ws)House                                                                            { makeToken TknType }
<0>         hosts(@ws)a(@ws)feast(@ws)for                                                           { makeToken TknConstValue }
<0>         House                                                                                   { makeToken TknBeginAlias }
<0>         comes(@ws)from(@ws)the(@ws)old(@ws)lineage(@ws)of                                       { makeToken TknStrongAlias }
<0>         are(@ws)the(@ws)dogs(@ws)of                                                             { makeToken TknWeakAlias }

--          Data Types
<0>         Lanninteger                                                                             { makeToken TknInt }
<0>         Freyt                                                                                   { makeToken TknFloat }
<0>         Boolton                                                                                 { makeToken TknTrilean }
<0>         Starkhar                                                                                { makeToken TknChar }
<0>         Barathom                                                                                { makeToken TknAtom }
<0>         No(@ws)one                                                                              { makeToken TknVoid }

--          Literals 
<0>         True(@ws)Heir                                                                           { makeToken TknTrue }
<0>         Usurper                                                                                 { makeToken TknFalse }
<0>         $digits+(@ws)soldiers                                                                   { makeToken TknIntLit }
<0>         $digits+\.$digits+(@ws)descendants                                                      { makeToken TknFloatLit }
<0>         Hodor(@ws)\'(@scapedchars)\'                                                            { makeToken TknCharLit }
<0>         Hodor(@ws)\'$printable\'                                                                { makeToken TknCharLit }
<0>         Hodor(@ws)\'(@linebreaks)\'                                                             { invalidBreak }
<0>         Hodor(@ws)\'.\'                                                                         { invalidCharacter }
<0>         army(@ws)formation(@ws)of                                                               { makeToken TknBeginArrayLit }
<0>         aligned(@ws)together                                                                    { makeToken TknEndArrayLit }
<0>         Rickon                                                                                  { makeToken TknNull }

--          Composite Types
<0>         Former                                                                                  { makeToken TknBeginCompTypeId }
<0>         now                                                                                     { makeToken TknEndCompTypeId }
<0>         Lord(@ws)Commander(@ws)of                                                               { makeToken TknBeginArray }
<0>         bannermen                                                                               { makeToken TknEndArray }
<0>         Hand(@ws)of(@ws)the(@ws)King                                                            { makeToken TknString }
<0>         leading                                                                                 { makeToken TknBeginSizes }
<0>         to(@ws)their(@ws)deaths                                                                 { makeToken TknEndSizes }
<0>         King(@ws)of(@ws)whom                                                                    { makeToken TknBeginStruct }
<0>         have(@ws)bent(@ws)their(@ws)knees                                                       { makeToken TknEndStruct }
<0>         Faceless(@ws)Man(@ws)who(@ws)stole                                                      { makeToken TknBeginUnion }
<0>         their(@ws)faces                                                                         { makeToken TknEndUnion }
<0>         Spearwife(@ws)of                                                                        { makeToken TknPointerType }
<0>         White(@ws)Walker(@ws)possesing                                                          { makeToken TknBeginTuple }
<0>         wights                                                                                  { makeToken TknEndTuple }

--          Type conversion
<0>         adopted(@ws)by(@ws)House                                                                { makeToken TknCast }

--          Operators
<0>         takes                                                                                   { makeToken TknAssign }
<0>         fight(@ws)against                                                                       { makeToken TknTupleAssign }
<0>         joined(@ws)by                                                                           { makeToken TknPlus }
<0>         left(@ws)by                                                                             { makeToken TknMinus }
<0>         combined(@ws)forces(@ws)with                                                            { makeToken TknMult }
<0>         cut(@ws)into(@ws)pieces(@ws)by                                                          { makeToken TknDivide }
<0>         turncloak                                                                               { makeToken TknNegate }
<0>         stripped(@ws)of(@ws)his(@ws)dignity(@ws)by                                              { makeToken TknMod }
<0>         and                                                                                     { makeToken TknAnd }
<0>         or                                                                                      { makeToken TknOr }
<0>         similar(@ws)to(@ws)                                                                     { makeToken TknEqual }
<0>         different(@ws)from(@ws)                                                                 { makeToken TknNotEqual }
<0>         bested(@ws)by(@ws)                                                                      { makeToken TknLessThan }
<0>         defeating(@ws)                                                                          { makeToken TknGreaterThan }
<0>         almost(@ws)bested(@ws)by(@ws)                                                           { makeToken TknLessEqThan }
<0>         almost(@ws)defeating(@ws)                                                               { makeToken TknGreaterEqThan }

--          Composite Types Operators
<0>         subject(@ws)of                                                                          { makeToken TknStructField }
<0>         looking(@ws)in(@ws)the(@ws)mirror(@ws)at                                                { makeToken TknUnionQuery }
<0>         acting(@ws)as                                                                           { makeToken TknUnionField }
<0>         Soldier(@ws)acquainted(@ws)with                                                         { makeToken TknBeginIndex }
<0>         under(@ws)command(@ws)of                                                                { makeToken TknEndIndex }
<0>         Wight(@ws)[0-9]+                                                                        { makeToken TknTupleSelect }
<0>         marries(@ws)a                                                                           { makeToken TknPtr }
<0>         Spouse(@ws)of                                                                           { makeToken TknDereference } 
<0>         forsakes(@ws)marriage                                                                   { makeToken TknFree }

--          Exit Statement
<0>         The(@ws)book                                                                            { makeToken TknBeginExit }
<0>         has(@ws)reached(@ws)an(@ws)unexpected(@ws)end                                           { makeToken TknEndExit }

--          IO
<0>         A(@ws)raven(@ws)has(@ws)come(@ws)for                                                    { makeToken TknRead }
<0>         We(@ws)must(@ws)send(@ws)a(@ws)raven(@ws)with(@ws)everything(@ws)we(@ws)know(@ws)of     { makeToken TknPrint }

--          Empty Statement
<0>         The(@ws)three-eyed(@ws)raven(@ws)watches(@ws)from(@ws)afar                              { makeToken TknPass }

--          Procedures definition
<0>         Table\ of\ Contents\:                                                                   { makeToken TknBeginFuncDecl }
<0>         \-                                                                                      { makeToken TknFunctionItem }
<0>         Prologue                                                                                { makeToken TknGlobalDec }
<0>         Epilogue                                                                                { makeToken TknMain }
<0>         watches                                                                                 { makeToken TknBeginFunctionParams }
<0>         approach(@ws)from(@ws)a(@ws)distance(@ws);                                              { makeToken TknEndFunctionParams }
<0>         I(@ws)must(@ws)warn(@ws)you(@ws),                                                       { makeToken TknBeginReturnVals }
<0>         is(@ws)coming(@ws)\.                                                                    { makeToken TknEndReturnVals }
<0>         Dracarys                                                                                { makeToken TknReturnOpen }
<0>         !                                                                                       { makeToken TknReturnClose }
<0>         Valued                                                                                  { makeToken TknValueArg }
<0>         Honorable                                                                               { makeToken TknReferenceArg }

--          Blocks
<0>         Valar(@ws)Morghules(@ws)\.                                                              { makeToken TknOpenBlock }
<0>         Valar(@ws)Dohaeres(ws)\.                                                                { makeToken TknCloseBlock }

--          Procedures call
<0>         traveling                                                                               { makeToken TknProcCallOpen }
<0>         alongside                                                                               { makeToken TknProcCallArgs }
<0>         with(@ws)caution                                                                        { makeToken TknProcCallClose }

--          Determinate repetition
<0>         The(@ws)things(@ws)I(@ws)do(@ws)for                                                     { makeToken TknFor }
<0>         I(@ws)would(@ws)kill(@ws)from                                                           { makeToken TknForLB }
<0>         up(@ws)to                                                                               { makeToken TknForUB }
<0>         That,(@ws)and(@ws)much(@ws)more(@ws)I(@ws)would(@ws)do(ws)to(@ws)get(@ws)her(@ws)love   { makeToken TknEndFor }

--          Undeterminate repetition
<0>         While                                                                                   { makeToken TknWhile }
<0>         reigns(@ws)truly(@ws)upon(@ws)the(@ws)land                                              { makeToken TknWhileDecorator }
<0>         Only(@ws)for(@ws)as(@ws)long(@ws)as(@ws)the(@ws)sovereign(@ws)lives                     { makeToken TknWhileEnd}

--          Non-structured flow
<0>         What(@ws)is(@ws)dead(@ws)may(@ws)never(@ws)die                                          { makeToken TknContinue }
<0>         This(@ws)is(@ws)the(@ws)doom(@ws)of(@ws)Valyria                                         { makeToken TknBreak } 

--          Simple Selection
<0>         if                                                                                      { makeToken TknBeginSimpleSelection } 
<0>         may(@ws)be(@ws)the(@ws)True(@ws)King(@ws)of(@ws)the(@ws)Seven(@ws)Kingdoms,(@ws)then    { makeToken TknSimpleSelectionDecorator }
<0>         Otherwise                                                                               { makeToken TknElse } 
<0>         And(@ws)so(@ws)our(@ws)fate(@ws)rests(@ws)upon(@ws)this(@ws)decision                    { makeToken TknSimpleSelectionEnd }

--          Multiple Selection
<0>         You(@ws)will(@ws)be(@ws)betrayed(@ws)by                                                 { makeToken TknBeginMultipleSelection }
<0>         several(@ws)times                                                                       { makeToken TknMultipleSelectionDecorator }
<0>         Once(@ws)for                                                                            { makeToken TknBranch }
<0>         So(@ws)the(@ws)prophecy(@ws)says                                                        { makeToken TknEndMultipleSelection }

--          Identifiers
<0>         [A-Z]([\']?[a-z]+)+                                                                     { makeToken TknID }
<0>         M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{1,3})                                  { makeToken TknArgNumber }
<0>         M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{1,3})(IX|IV|V?I{0,3})                                  { makeToken TknArgNumber }
<0>         M{0,4}(CM|CD|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})                                  { makeToken TknArgNumber }
<0>         M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})                                  { makeToken TknArgNumber }

--          Appendix
<0>         Appendix:                                                                               { makeToken TknAliasDec }

--          Dot, Comma 
<0>         \,                                                                                      { makeToken TknComma }
<0>         \.                                                                                      { makeToken TknDot }

--          Expressions
<0>         \<\<                                                                                    { makeToken TknOpenParenthesis }
<0>         \>\>                                                                                    { makeToken TknCloseParenthesis }
<0>         «                                                                                       { makeToken TknOpenParenthesis }
<0>         »                                                                                       { makeToken TknCloseParenthesis }

<0>         [^\ \t\n\f\v\r]+                                                                        { invalidWord }
<0>         .                                                                                       { invalidCharacter }

-- Lexer and wrapper function definitions
{

data AlexUserState = LexerState {
        lexerString :: String,
        lexerTokens :: [Token],
        lexerErrors :: [Error]
    }

alexInitUserState :: AlexUserState
alexInitUserState = LexerState {
        lexerString = "",
        lexerTokens = [],
        lexerErrors = []
    }

alexEOF :: Alex AlexUserState
alexEOF = do
    ust <- getUserState
    case lexerString ust of 
        [] -> return ust
        _ -> do 
            addErrorToState $ Error "Unclosed string at EOF."
            getUserState

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

makeToken :: AbstractToken -> AlexAction AlexUserState
makeToken tk (AlexPn _ r c, _, _, str) len = do
    let str' = take len str
    addTokenToState Token {
        aToken = tk,
        cleanedString = str',
        capturedString = str',
        position = Position {row=r, col=c}
    }
    alexMonadScan

makeStringToken :: AlexAction AlexUserState
makeStringToken (AlexPn _ r c, _, _, _) _ = do
    ust <- getUserState
    let str' = reverse $ '\"' : lexerString ust
    addTokenToState Token {
        aToken = TknStringLit,
        cleanedString = str',
        capturedString = str',
        position = Position {row=r, col=c - (length str')}
    }
    cleanLexerString
    alexMonadScan

makeCommentToken :: AlexAction AlexUserState
makeCommentToken (AlexPn _ r c, _, _, _) _ = do
    ust <- getUserState
    let str' = reverse $ '.' : lexerString ust
    addTokenToState Token {
        aToken = TknComment,
        cleanedString = str',
        capturedString = str',
        position = Position {row=r, col=c - (length str')}
    }
    cleanLexerString
    alexMonadScan

invalidBreak :: AlexAction AlexUserState
invalidBreak (AlexPn _ r c, _, _, _) _ = do 
    addErrorToState $ Error $ "Invalid break at line " ++ show r ++ " column " ++ show c
    alexMonadScan

invalidCharacter, invalidWord :: AlexAction AlexUserState
invalidCharacter (AlexPn _ r c, _, _, _) _ = do 
    addErrorToState $ Error $ "Unexpected character at line " ++ show r ++ " column " ++ show c
    alexMonadScan

invalidWord (AlexPn _ r c, _, _, _) _ = do 
    addErrorToState $ Error $ "Unexpected word at line " ++ show r ++ " column " ++ show c
    alexMonadScan

pushToString :: AlexAction AlexUserState
pushToString (_, _, _, str) len = do 
    let str' = reverse $ take len str
    addStringToState str'
    alexMonadScan

mapScaped :: Char -> String 
mapScaped 'n' = "\n"
mapScaped 't' = "\t"
mapScaped '\'' = "\'"
mapScaped '\"' = "\""
mapScaped '\\' = "\\"

pushScapedToString :: AlexAction AlexUserState
pushScapedToString (_, _, _, str) len = do
    let str' = take len str
    addStringToState $ mapScaped $ last str'
    alexMonadScan
    
addTokenToState :: Token -> Alex ()
addTokenToState tk = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            lexerTokens = tk : lexerTokens ust 
        }
    }, ())

addErrorToState :: Error -> Alex ()
addErrorToState err = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            lexerErrors = err : lexerErrors ust 
        }
    }, ())

cleanLexerString :: Alex ()
cleanLexerString = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            lexerString = ""
        }
    }, ())

addStringToState :: String -> Alex ()
addStringToState str = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{ alex_ust = ust{ lexerString = str ++ lexerString ust } }, ())

scanTokens :: String -> Either [Error] [Token]
scanTokens str = 
    case runAlex str alexMonadScan of
        Left err -> Left [Error $ "Alex error: " ++ show err]
        Right ust -> 
            case lexerErrors ust of 
                [] -> Right $ reverse $ map postProcess $ lexerTokens ust
                _ -> Left $ reverse $ lexerErrors ust

postProcess :: Token -> Token 
postProcess (Token TknCharLit s _ p) = Token TknCharLit s (processCharLit s) p
postProcess (Token TknStringLit s _ p) = Token TknStringLit s (processStringLit s) p
postProcess (Token TknIntLit s _ p) = Token TknIntLit s (processIntLit s) p
postProcess (Token TknTupleSelect s _ p) = Token TknTupleSelect s (processIntLit s) p
postProcess tkn = tkn

processCharLit :: String -> String 
processCharLit str = 
    let str' = init $ tail $ dropWhile (/= '\'') str in
        if head str' == '\\' 
            then mapScaped $ last str'
            else str'

processStringLit :: String -> String 
processStringLit str = init $ tail $ dropWhile (/= '\"') str

processIntLit :: String -> String 
processIntLit = filter (\x -> isDigit x || x == '-')

processFloatLit :: String -> String 
processFloatLit = filter (\x -> isDigit x || x == '.' || x == '-')

}