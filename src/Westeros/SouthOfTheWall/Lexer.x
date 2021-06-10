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
<0>         \-\-\ ([A-Z][a-z]*\ )+\-\-                                                              { makeToken TknProgramName } 

--          Type Declaration
<0>         Lord                                                                                    { makeToken TknVar }
<0>         Lady                                                                                    { makeToken TknVar }
<0>         Knight                                                                                  { makeToken TknConst }
<0>         of(@ws)House                                                                            { makeToken TknType }
<0>         House                                                                                   { makeToken TknBeginAlias }
<0>         comes(@ws)from(@ws)the(@ws)old(@ws)lineage(@ws)of                                       { makeToken TknStrongAlias }
<0>         are(@ws)the(@ws)dogs(@ws)of                                                             { makeToken TknWeakAlias }

--          Data Types
<0>         Lanninteger                                                                             { makeToken TknInt }
<0>         Freyt                                                                                   { makeToken TknFloat }
<0>         Boolton                                                                                 { makeToken TknTrilean }
<0>         Starkhar                                                                                { makeToken TknChar }

--          Literals 
<0>         blood                                                                                   { makeToken TknTrue }
<0>         gold                                                                                    { makeToken TknNeutral }
<0>         love                                                                                    { makeToken TknFalse }
<0>         $digits+(@ws)soldiers                                                                   { makeToken TknIntLit }
<0>         $digits+\.$digits+(@ws)sons                                                             { makeToken TknFloatLit }
<0>         Hodor(@ws)\'(@scapedchars)\'                                                            { makeToken TknCharLit }
<0>         Hodor(@ws)\'$printable\'                                                                { makeToken TknCharLit }
<0>         Hodor(@ws)\'(@linebreaks)\'                                                             { invalidBreak }
<0>         Hodor(@ws)\'.\'                                                                         { invalidCharacter }

--          Composite Types
<0>         Former                                                                                  { makeToken TknBeginCompType }
<0>         now                                                                                     { makeToken TknEndIDCompType }
<0>         King(@ws)of                                                                             { makeToken TknStruct }
<0>         Faceless(@ws)Man(@ws)holding(@ws)faces(@ws)of                                           { makeToken TknUnion }
<0>         Lord(@ws)Commander(@ws)of                                                               { makeToken TknArray }
<0>         Hand(@ws)of(@ws)the(@ws)King                                                            { makeToken TknString }
<0>         Spearwife(@ws)of                                                                        { makeToken TknPointer }
<0>         White(@ws)Walker(@ws)with(@ws)deads(@ws)from(@ws)Houses                                 { makeToken TknTuple }
<0>         bannermen(@ws)holding                                                                   { makeToken TknArraySize }
<0>         ruling(@ws)over                                                                         { makeToken TknStringSize }
<0>         bannermen(@ws)holding:                                                                  { makeToken TknArrayDecl }
<0>         ruling(@ws)with(@ws)Grand                                                               { makeToken TknStringDecl }

--          Operators
<0>         takes                                                                                   { makeToken TknAssign }
<0>         take                                                                                    { makeToken TknBeginMultAssign }
<0>         respectively                                                                            { makeToken TknEndMultAssign }
<0>         fight(@ws)against                                                                       { makeToken TknTupleAsign }
<0>         without                                                                                 { makeToken TknMinus }
<0>         with                                                                                    { makeToken TknPlus }
<0>         times(@ws)the(@ws)power(@ws)of                                                          { makeToken TknMult }
<0>         divided(@ws)by(@ws)the(@ws)power(@ws)of                                                 { makeToken TknDivide }
<0>         picking(@ws)what(@ws)remains(@ws)of                                                     { makeToken TknMod }
<0>         bastard                                                                                 { makeToken TknNegate }
<0>         is(@ws)as(@ws)powerful(@ws)as                                                           { makeToken TknEqual }
<0>         not(@ws)merely(@ws)powerful(@ws)as                                                      { makeToken TknNotEqual }
<0>         is(@ws)weaker(@ws)than                                                                  { makeToken TknLessThan }
<0>         is(@ws)stronger(@ws)than                                                                { makeToken TknGreaterThan }
<0>         is(@ws)almost(@ws)as(@ws)weak(@ws)as                                                    { makeToken TknLessEqThan }
<0>         is(@ws)almost(@ws)as(@ws)strong(@ws)as                                                  { makeToken TknGreaterEqThan }
<0>         flayed                                                                                  { makeToken TknNot }
<0>         and                                                                                     { makeToken TknAnd }
<0>         or                                                                                      { makeToken TknOr }
<0>         equals                                                                                  { makeToken TknBoolEqual }
<0>         differentiates                                                                          { makeToken TknBoolNotEqual }

--          Composite Types Operators
<0>         subject(@ws)of                                                                          { makeToken TknStructField }
<0>         Is                                                                                      { makeToken TknBeginUnionQuestion }
<0>         using(@ws)the(@ws)face(@ws)of                                                           { makeToken TknUnionQuestion }
<0>         \?                                                                                      { makeToken TknEndUnionQuestion }
<0>         acting(@ws)as                                                                           { makeToken TknUnionField }
<0>         Soldier(@ws)acquainted(@ws)with                                                         { makeToken TknBeginIndex }
<0>         Wight(@ws)following                                                                     { makeToken TknBeginTupleIndex }
<0>         under(@ws)command(@ws)of                                                                { makeToken TknEndIndex }
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
<0>         \-                                                                                      { makeToken TknListFunction }
<0>         Prologue                                                                                { makeToken TknFirstMain }
<0>         Epilogue                                                                                { makeToken TknLastMain }
<0>         Hereby(@ws)I(@ws)introduce(@ws)the                                                      { makeToken TknFunctionParams }
<0>         I(@ws)must(@ws)warn(@ws)you                                                             { makeToken TknBeginReturnVals }
<0>         is(@ws)coming                                                                           { makeToken TknEndReturnVals }
<0>         Dracarys                                                                                { makeToken TknReturnOpen }
<0>         !                                                                                       { makeToken TknReturnClose }
<0>         Valued                                                                                  { makeToken TknValueArg }
<0>         Honorable                                                                               { makeToken TknReferenceArg }

--          Blocks
<0>         Valar(@ws)Morghules                                                                     { makeToken TknOpenBlock }
<0>         Valar(@ws)Dohaeres                                                                      { makeToken TknCloseBlock }

--          Procedures call
<0>         traveling                                                                               { makeToken TknProcCallOpen }
<0>         alongside                                                                               { makeToken TknProcCallArgs }
<0>         with(@ws)caution                                                                        { makeToken TknProcCallClose }

--          Unclassified
<0>         Nobody                                                                                  { makeToken TknVoid }

--          Determinate repetition
<0>         The(@ws)things(@ws)I(@ws)do(@ws)for                                                     { makeToken TknFor }
<0>         from                                                                                    { makeToken TknForLB }
<0>         until                                                                                   { makeToken TknForUB }

--          Undeterminate repetition
<0>         While                                                                                   { makeToken TknWhile }
<0>         flows(@ws)down(@ws)then(@ws)river                                                       { makeToken TknWhileDecoration }

--          Non-structured flow
<0>         What(@ws)is(@ws)dead(@ws)may(@ws)never(@ws)die                                          { makeToken TknContinue }
<0>         This(@ws)is(@ws)the(@ws)doom(@ws)of(@ws)Valyria                                         { makeToken TknBreak } 

--          Selection
<0>         You(@ws)will(@ws)be(@ws)betrayed(@ws)by                                                 { makeToken TknBeginSelection }
<0>         three(@ws)times                                                                         { makeToken TknSelectionDecorator }
<0>         Once(@ws)for(@ws)blood                                                                  { makeToken TknTrueBranch }
<0>         Once(@ws)for(@ws)love                                                                   { makeToken TknUnknownBranch }
<0>         Once(@ws)for(@ws)gold                                                                   { makeToken TknFalseBranch } 
<0>         So(@ws)the(@ws)prophecy(@ws)says                                                        { makeToken TknEndSelection }

--          Identifiers
<0>         [A-Z]([\']?[a-z]+)+                                                                     { makeToken TknID }
<0>         M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{1,3})                                  { makeToken TknArgNumber }
<0>         M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{1,3})(IX|IV|V?I{0,3})                                  { makeToken TknArgNumber }
<0>         M{0,4}(CM|CD|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})                                  { makeToken TknArgNumber }
<0>         M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})                                  { makeToken TknArgNumber }

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
postProcess (Token TknStringLit s _ p) = Token TknCharLit s (processStringLit s) p
postProcess (Token TknIntLit s _ p) = Token TknCharLit s (processIntLit s) p
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
processIntLit = filter isDigit 

processFloatLit :: String -> String 
processFloatLit = filter (\x -> isDigit x || x == '.')
}