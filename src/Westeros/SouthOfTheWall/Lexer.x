{
module Westeros.SouthOfTheWall.Lexer (
    --scanTokens,
    ) where

import Westeros.SouthOfTheWall.Tokens
}

%wrapper "monadUserState"

-- macros go here

$digits = [0-9]

-- regex go here
tokens :-

<0>     $white+                     ;


-- Literals

<0>     $digits+$white+golden$white+coins                           ;  
<0>     $digits+.$digits$white+drops$white+of$white+poison          ; 
<0>     blood                                                       ;  
<0>     gold                                                        ;  
<0>     love                                                        ;  

-- procedures


{

alexEOF :: Alex AlexUserState
alexEOF = undefined -- getUserState

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

{-
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
addErrorToState err = undefined 
Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            = err : lexerErrors ust 
        }
    }, ())

cleanLexerString :: Alex ()
cleanLexerString = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = ust{
            lexerString = ""
        }
    }, ())
-}

}