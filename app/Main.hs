module Main where

import Westeros.SouthOfTheWall.Lexer (scanTokens)
import Westeros.SouthOfTheWall.Parser (preParse)

import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk

import Control.Monad.RWS ( RWST(runRWST) )
import Data.Map
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs

    case head args of 
        "lex"   -> testLexer
        "parse" -> testParser
        _       -> putStrLn "Invalid option"

testLexer :: IO ()
testLexer = do
    str <- getContents  
    case scanTokens str of 
        Left errs -> mapM_ print errs
        Right tokens -> mapM_ print tokens

testParser :: IO ()
testParser = do 
    str <- getContents 
    case scanTokens str of 
        Left errs -> mapM_ print errs
        Right tokens -> do 
            (_,st,_) <- runRWST (preParse tokens) () ST.initialST 
            mapM_ print (toList $ ST.dict st)