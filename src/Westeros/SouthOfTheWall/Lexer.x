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
<0>         @ws                                                 ;

--          Comments
<0>         Suddenly\,                                          { pushToString `andBegin` comment }
<0>         In@wsthe@wsmidst@wsof                               { pushToString `andBegin` comment }
<0>         Therefore                                           { pushToString `andBegin` comment }
<comment>   \.                                                  { makeCommentToken `andBegin` 0 }
<comment>   .                                                   { pushToString }

--          String Literals
<0>         scroll@ws\"                                         { pushToString `andBegin` string }  --"
<string>    \"                                                  { makeStringToken `andBegin` 0 }    --"
<string>    @scapedchars                                        { pushToString }
<string>    @linebreaks                                         { invalidBreak }
<string>    $printable                                          { pushToString }
<string>    .                                                   { invalidCharacter }

--          Type Declaration
<0>         Lord                                                              { makeToken TknVar }
<0>         Lady                                                              { makeToken TknVar }
<0>         Knight                                                            { makeToken TknConst }
<0>         of@wsHouse                                                        { makeToken TknType }
<0>         House                                                             { makeToken TknBeginAlias }
<0>         comes@wsfrom@wsthe@wsold@wslineage@wsof                           { makeToken TknStrongAlias }
<0>         are@wsthe@wsdogs@wsof                                             { makeToken TknWeekAlias }

--          Data Types
<0>         Lanninteger                                                       { makeToken TknInt }
<0>         $digits+@wsgolden@wsdragons                                       { makeToken TknIntLit }
<0>         Freyt                                                             { makeToken TknFloat }
<0>         $digits+\.$digits@wsdrops@wsof@wspoison                           { makeToken TknFloatLit }
<0>         Boolton                                                           { makeToken TknTrilean }
<0>         blood                                                             { makeToken TknTrue }
<0>         gold                                                              { makeToken TknNeutral }
<0>         love                                                              { makeToken TknFalse }
<0>         Starkhar                                                          { makeToken TknChar }
<0>         rune@ws\'.\'                                                      { makeToken TknCharLit }

--          Literals 
<0>         [\-]{0,1}$digits+@ws+golden$white+coins                             ;  
<0>         [\-]{0,1}$digits+\.$digits$white+drops$white+of$white+poison        ; 

--TODO: definir apropiadamente los tipos compuestos
-- <0>         Former                                                            { makeToken TknBeginCompType }
-- <0>         of                                                                { makeToken TknEndCompType }
-- <0>         now@wsKing                                                        { makeToken TknStruct }
-- <0>         ,                                                                 { makeToken TknComma }
-- <0>         now@wsFaceless@wsMan@wsholding@wsfaces@wsof:                      { makeToken TknUnion }
-- <0>         now@wsLord@wsCommander                                            { makeToken TknArray }

--          Operators
<0>         takes                                                             { makeToken TknAssign }
<0>         take                                                              { makeToken TknBeginMultAssign }
<0>         respectively                                                      { makeToken TknEndMultAssign }
<0>         fight@wsagainst                                                   { makeToken TknTupleAsign }
<0>         without                                                           { makeToken TknMinus }
<0>         with                                                              { makeToken TknPlus }
<0>         times@wsthe@wspower@wsof                                          { makeToken TknMult }
<0>         picking@wswhat@wsremains@wsof                                     { makeToken TknMod }
<0>         negated                                                           { makeToken TknNegate }
<0>         is@wsas@wspowerful@wsas                                           { makeToken TknEqual }
<0>         not@wsmerely@wspowerful@wsas                                      { makeToken TknNotEqual }
<0>         is@wsweaker@wsthan                                                { makeToken TknLessThan }
<0>         is@wsstronger@wsthan                                              { makeToken TknGreaterThan }
<0>         is@wsalmost@wsas@wsweak@wsas                                      { makeToken TknLessEqThan }
<0>         is@wsalmost@wsas@wsstrong@wsas                                    { makeToken TknGreaterEqThan }

--          Exit Statement
<0>         The@wsbook                                                        { makeToken TknBeginExit }
<0>         has@wsreached@wsan@wsunexpected@wsend                             { makeToken TknEndExit }

--          IO
<0>         A@wsraven@has@wscome@wsfor                                        { makeToken TknRead }
<0>         We@wsmust@wssend@wsa@wsraven@wswith@wseverything@wswe@wsknow@wsof { makeToken TknPrint }

--          Empty Statement
<0>         The@wsthree-eyed@wsraven@wswatches@wsfrom@wsafar                  { makeToken TknPass }

--          Procedures definition
<0>         Hereby@wsI@wsintroduce@wsthe@wshonorable                          { makeToken TknFunctionArgs }
<0>         I@wsmust@wswarn@wsyou                                             { makeToken TknBeginReturnVals }
<0>         is@wscoming                                                       { makeToken TknEndReturnVals }
<0>         Dracarys                                                          { makeToken TknReturnOpen }
<0>         !                                                                 { makeToken TknReturnClose }

--          Procedures call
<0>         traveling                                                         { makeToken TknProcCallOpen }
<0>         alongside                                                         { makeToken TknProcCallArgs }
<0>         with@wscaution                                                    { makeToken TknProcCallClose }

--          Unclassified
<0>         Nobody                                                            { makeToken TknVoid }

--          Determinate repetition
<0>         The@wsthings@wsI@wsdo@wsfor                                       { makeToken TknFor }
<0>         from                                                              { makeToken TknForLB }
<0>         until                                                             { makeToken TknForUB }

--          Undeterminate repetition
<0>         While                                                            { makeToken TknWhile }
<0>         flows@wsdown@wsthen@wsriver                                      { makeToken TknWhileDecoration }

--          Non-structured flow
<0>         What@wsis@wsdead@wsmay@wsnever@wsdie                             { makeToken TknContinue }
<0>         This@wsis@wsthe@wsdoom@wsof@wsValyria                            { makeToken TknBreak } 

--          Selection
<0>         You@wswill@wsbe@wsbetrayed@wsby                                 { makeToken TknBeginSelction }
<0>         three@wstimes                                                   { makeToken TknSelectionDecorator }
<0>         Once@wsfor@wsblood                                              { makeToken TknTrueBranch }
<0>         Once@wsfor@wslove                                               { makeToken TknUnknownBranch }
<0>         Once@wsfor@wsgold                                               { makeToken TknFalseBranch } 
<0>         So@wsthe@wsprophecy@wssays                                      { makeToken TknEndSelection }

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
makeToken Tkn (AlexPn _ r c, _, _, str) len = do
    let str' = take len str
    addTokenToState Token {
        token = Tkn,
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
addTokenToState Tkn = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            lexerTokens = Tkn : lexerTokens ust 
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