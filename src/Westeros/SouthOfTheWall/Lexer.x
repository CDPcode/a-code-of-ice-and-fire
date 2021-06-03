{
module Westeros.SouthOfTheWall.Lexer where

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

<0>         $white+                                 ;
          Comments
<0>         Suddenly\,                              { pushToString `andBegin` comment }
<0>         In$white+the$white+midst$white+of       { pushToString `andBegin` comment }
<0>         Therefore                               { pushToString `andBegin` comment }
<comment>   \.                                      { makeCommentToken `andBegin` 0 }
<comment>   .                                       { pushToString }

--          String Literals
<0>         scroll$white+\"                         { pushToString `andBegin` string }  --"
<string>    \"                                      { makeStringToken `andBegin` 0 }    --"
<string>    @scapedchars                            { pushToString }
<string>    @linebreaks                             { invalidBreak }
<string>    $printable                              { pushToString }
<string>    .                                       { invalidCharacter }


-- Literals # Redefiniendo
--<0>     [\-]{0,1}$digits+@ws+golden$white+coins                      ;  
--<0>     [\-]{0,1}$digits+\.$digits$white+drops$white+of$white+poison ; 


-- Procedures

--  concerning definition
<0>     Hereby@ws I@ws introduce@ws the@ws honorable    ; -- {makeToken TKFunctionArgs }
<0>     of@ws House                                     ; -- {makeToken TKTypeDeclaration }
<0>     I@ws must@ws warn@ws you                        ; -- {makeToken TKPreReturn } -- Is the comma necessary?
<0>     is@ws coming                                    ; -- {makeToken TKPostReturn }
<0>     Dracarys                                        ; -- {makeToken TKReturnOpen }
<0>     !                                               ; -- {makeToken TKReturnClose } -- Is this necessary?

--  concerning call
<0>     traveling                                       ; -- {makeToken TKProcCallOpen }
<0>     alongside                                       ; -- {makeToken TKProcArgs }
<0>     with@ws caution                                 ; -- {makeToken TKProcCallClose }

-- Unclassified
<0>     Nobody                                          ; -- {makeToken TKVoid }

-- Determinate repetition

<0>     The@ws things@ws I@ws do@ws for                 ; -- {makeToken TKForLoop }
<0>     from                                            ; -- {makeToken TKForLB }
<0>     until                                           ; -- {makeToken TKForUB }

-- Undeterminate repetition

<0>     While                                           ; -- {makeToken TKWhileLoop }
<0>     flows@ws down@ws then@ws river                  ; -- {makeToken TKWhileDecoration }

-- Non-structured flow

<0>     What@ws is@ws dead@ws may@ws never@ws die       ; -- {makeToken TKContinue }
<0>     This@ws is@ws the@ws doom@ws of@ws Valyria      ; -- {makeToken TKBreak } 

-- Selection

<0>     Once@ws for@ws blood                            ; -- {makeToken TKTrueBranch }
<0>     Once@ws for@ws love                             ; -- {makeToken TKUnknownBranch }
<0>     Once@ws for@ws gold                             ; -- {makeToken TKFalseBranch } 
<0>     So@ws the@ws prophecy@ws says                   ; -- {makeToken TKSelectionTail }


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