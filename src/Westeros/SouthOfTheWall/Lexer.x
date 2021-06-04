{
module Westeros.SouthOfTheWall.Lexer (
    scanTokens,
    ) where
import Westeros.SouthOfTheWall.Tokens
}

%wrapper "monadUserState"

-- Macros

$digits = [0-9]
@scapedchars = \\[nt\\\"\\\'] --"
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
<0>         scroll(@ws)\"                                                                           { pushToString `andBegin` string }  --"
<string>    \"                                                                                      { makeStringToken `andBegin` 0 }    --"
<string>    @scapedchars                                                                            { pushToString }
<string>    @linebreaks                                                                             { invalidBreak }
<string>    $printable                                                                              { pushToString }
<string>    .                                                                                       { invalidCharacter }

--          Type Declaration
<0>         Lord                                                                                    { makeToken TknVar }
<0>         Lady                                                                                    { makeToken TknVar }
<0>         Knight                                                                                  { makeToken TknConst }
<0>         of(@ws)House                                                                            { makeToken TknType }
<0>         House                                                                                   { makeToken TknBeginAlias }
<0>         comes(@ws)from(@ws)the(@ws)old(@ws)lineage(@ws)of                                       { makeToken TknStrongAlias }
<0>         are(@ws)the(@ws)dogs(@ws)of                                                             { makeToken TknWeekAlias }

--          Data Types
<0>         Lanninteger                                                                             { makeToken TknInt }
<0>         $digits+(@ws)golden(@ws)dragons                                                         { makeToken TknIntLit }
<0>         Freyt                                                                                   { makeToken TknFloat }
<0>         $digits+\.$digits(@ws)drops(@ws)of(@ws)poison                                           { makeToken TknFloatLit }
<0>         Boolton                                                                                 { makeToken TknTrilean }
<0>         blood                                                                                   { makeToken TknTrue }
<0>         gold                                                                                    { makeToken TknNeutral }
<0>         love                                                                                    { makeToken TknFalse }
<0>         Starkhar                                                                                { makeToken TknChar }
<0>         rune(@ws)\'.\'                                                                          { makeToken TknCharLit }

--          Literals 
<0>         [\-]{0,1}$digits+(@ws)golden(@ws)coins                                               ;  
<0>         [\-]{0,1}$digits+\.$digits(@ws)drops(@ws)of(@ws)poison                            ; 

--TODO: definir apropiadamente los tipos compuestos
-- <0>         Former                                                                               { makeToken TknBeginCompType }
-- <0>         of                                                                                   { makeToken TknEndCompType }
-- <0>         now(@ws)King                                                                         { makeToken TknStruct }
-- <0>         ,                                                                                    { makeToken TknComma }
-- <0>         now(@ws)Faceless(@ws)Man(@ws)holding(@ws)faces(@ws)of:                               { makeToken TknUnion }
-- <0>         now(@ws)Lord(@ws)Commander                                                           { makeToken TknArray }

--          Operators
<0>         takes                                                                                   { makeToken TknAssign }
<0>         take                                                                                    { makeToken TknBeginMultAssign }
<0>         respectively                                                                            { makeToken TknEndMultAssign }
<0>         fight(@ws)against                                                                       { makeToken TknTupleAsign }
<0>         without                                                                                 { makeToken TknMinus }
<0>         with                                                                                    { makeToken TknPlus }
<0>         times(@ws)the(@ws)power(@ws)of                                                          { makeToken TknMult }
<0>         picking(@ws)what(@ws)remains(@ws)of                                                     { makeToken TknMod }
<0>         negated                                                                                 { makeToken TknNegate }
<0>         is(@ws)as(@ws)powerful(@ws)as                                                           { makeToken TknEqual }
<0>         not(@ws)merely(@ws)powerful(@ws)as                                                      { makeToken TknNotEqual }
<0>         is(@ws)weaker(@ws)than                                                                  { makeToken TknLessThan }
<0>         is(@ws)stronger(@ws)than                                                                { makeToken TknGreaterThan }
<0>         is(@ws)almost(@ws)as(@ws)weak(@ws)as                                                    { makeToken TknLessEqThan }
<0>         is(@ws)almost(@ws)as(@ws)strong(@ws)as                                                  { makeToken TknGreaterEqThan }

--          Exit Statement
<0>         The(@ws)book                                                                            { makeToken TknBeginExit }
<0>         has(@ws)reached(@ws)an(@ws)unexpected(@ws)end                                           { makeToken TknEndExit }

--          IO
<0>         A(@ws)raven@has(@ws)come(@ws)for                                                        { makeToken TknRead }
<0>         We(@ws)must(@ws)send(@ws)a(@ws)raven(@ws)with(@ws)everything(@ws)we(@ws)know(@ws)of     { makeToken TknPrint }

--          Empty Statement
<0>         The(@ws)three-eyed(@ws)raven(@ws)watches(@ws)from(@ws)afar                              { makeToken TknPass }

--          Procedures definition
<0>         Hereby(@ws)I(@ws)introduce(@ws)the(@ws)honorable                                        { makeToken TknFunctionArgs }
<0>         I(@ws)must(@ws)warn(@ws)you                                                             { makeToken TknBeginReturnVals }
<0>         is(@ws)coming                                                                           { makeToken TknEndReturnVals }
<0>         Dracarys                                                                                { makeToken TknReturnOpen }
<0>         !                                                                                       { makeToken TknReturnClose }

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

--          Dot, Comma missing

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
alexEOF = getUserState

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

makeToken :: AbstractToken -> AlexAction AlexUserState
makeToken tk (AlexPn _ r c, _, _, str) len = do
    let str' = take len str
    addTokenToState Token {
        token = tk,
        cleanedString = str',
        capturedString = str',
        position = Position {row=r, col=c}
    }
    alexMonadScan

makeStringToken :: Alex AlexUserState
makeStringToken (AlexPn _ r c, _, _, _) _ = do
    ust <- getUserState
    let str' = reverse $ '\"' : lexerString ust
    addTokenToState Token {
        token = TknString,
        cleanedString = str',
        capturedString = str',
        position = Position {row=r, col=c - (len str')}
    }
    cleanLexerString
    alexMonadScan

makeCommentToken :: Alex AlexUserState
makeCommentToken (AlexPn _ r c, _, _, _) _ = do
    ust <- getUserState
    let str' = reverse $ '.' : lexerString ust
    addTokenToState Token {
        token = TknComment,
        cleanedString = str',
        capturedString = str',
        position = Position {row=r, col=c - (len str')}
    }
    cleanLexerString
    alexMonadScan

invalidBreak :: Alex AlexUserState
invalidBreak (AlexPn _ r c, _, _, _) _ = do 
    addErrorToState $ Error "Invalid break..." -- needs definition of Error type
    alexMonadScan

invalidCharacter :: Alex AlexUserState
invalidBreak (AlexPn _ r c, _, _, _) _ = do 
    addErrorToState $ Error "Unexpected character..." -- needs definition of Error type
    alexMonadScan

pushToString :: Alex AlexUserState
pushToString (AlexPn _ r c, _, _, str) len = do 
    let str' = reverse $ take len str
    addStringToState str'
    alexMonadScan
    
addTokenToState :: Token -> Alex ()
addTokenToState tk = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            lexerTokens = tk : lexerTokens ust 
        }
    }, ())

addErrorToState :: Error -> Alex ()
addErrorToState err = undefined 
Alex $ \s@AlexState{alex_ust=ust}
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

}