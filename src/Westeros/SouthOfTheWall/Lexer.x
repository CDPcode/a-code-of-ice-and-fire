{
module Westeros.SouthOfTheWall.Lexer (
    scanTokens,
    ) where
import Westeros.SouthOfTheWall.Tokens
}

%wrapper "monadUserState"

-- macros go here

$digits = [0-9]
@scapedchars = \\[nt\\\"\\\'] --"
@linebreaks = \n+\r?\r+\n?
@ws = $white+

-- regex go here
tokens :-
<0>         @ws                                                 ;

--          Comments
<0>         Suddenly\,                                          { pushToString `andBegin` comment }
<0>         In$white+the$white+midst$white+of                   { pushToString `andBegin` comment }
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

--          Program Name
-- <0>         A@wsSong@wsof@wsIce@wsand@wsFire:@ws -- TODO: definir la sintaxis correcta de esta instrucción


<0>         Lord                                                              { makeToken TknVar }
<0>         Lady                                                              { makeToken TknVar }
<0>         Knight                                                            { makeToken TknConst }
<0>         of@wsHouse                                                        { makeToken TknType }
<0>         takes                                                             { makeToken TknAssign }
<0>         take                                                              { makeToken TknBeginMultAssign }
<0>         respectively                                                      { makeToken TknEndMultAssign }
<0>         fight@wsagainst                                                   { makeToken TknTupleAsign }
<0>         The@wsbook                                                        { makeToken TknBeginExit }
<0>         has@wsreached@wsan@wsunexpected@wsend                             { makeToken TknEndExit }
<0>         A@wsraven@has@wscome@wsfor                                        { makeToken TknRead }
<0>         We@wsmust@wssend@wsa@wsraven@wswith@wseverything@wswe@wsknow@wsof { makeToken TknPrint }
<0>         The@wsthree-eyed@wsraven@wswatches@wsfrom@wsafar                  { makeToken TknPass }
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

--TODO: definir apropiadamente los tipos compuestos
-- <0>         Former                                                            { makeToken TknBeginCompType }
-- <0>         of                                                                { makeToken TknEndCompType }
-- <0>         now@wsKing                                                        { makeToken TknStruct }
-- <0>         ,                                                                 { makeToken TknComma }
-- <0>         now@wsFaceless@wsMan@wsholding@wsfaces@wsof:                      { makeToken TknUnion }
-- <0>         now@wsLord@wsCommander                                            { makeToken TknArray }

<0>         House                                                             { makeToken TknBeginAlias }
<0>         comes@wsfrom@wsthe@wsold@wslineage@wsof                           { makeToken TknStrongAlias }
<0>         are@wsthe@wsdogs@wsof                                             { makeToken TknWeekAlias }
<0>         with                                                              { makeToken TknPlus }
<0>         without                                                           { makeToken TknMinus }
<0>         times@wsthe@wspower@wsof                                          { makeToken TknMult }
<0>         picking@wswhat@wsremains@wsof                                     { makeToken TknMod }
<0>         negated                                                           { makeToken TknNegate }
<0>         is@wsas@wspowerful@wsas                                           { makeToken TknEqual }
<0>         not@wsmerely@wspowerful@wsas                                      { makeToken TknNotEqual }
<0>         is@wsweaker@wsthan                                                { makeToken TknLessThan }
<0>         is@wsstronger@wsthan                                              { makeToken TknGreaterThan }
<0>         is@wsalmost@wsas@wsweak@wsas                                      { makeToken TknLessEqThan }
<0>         is@wsalmost@wsas@wsstrong@wsas                                    { makeToken TknGreaterEqThan }

{

alexEOF :: Alex AlexUserState
alexEOF = getUserState

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
        token = TkString,
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
        token = TkComment,
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
    
getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

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
    -> Right (s{
        alex_ust = ust{
            lexerString = str ++ lexerString ust
        }
    }, ())
    

}