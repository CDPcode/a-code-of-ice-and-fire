module Main where

import Westeros.SouthOfTheWall.AST          (prettyPrintProgram)
import Westeros.SouthOfTheWall.Lexer        (scanTokens)
import Westeros.SouthOfTheWall.Parser       (parse)
import Westeros.SouthOfTheWall.PreParser    (preParse)

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
        "preparse" -> testPreParser
        "parse" -> testParser
        _       -> putStrLn "Invalid option"

testLexer :: IO ()
testLexer = do
    str <- getContents
    case scanTokens str of
        Left errs -> mapM_ print errs
        Right tokens -> mapM_ print tokens

testPreParser :: IO ()
testPreParser = do
    str <- getContents
    case scanTokens str of
        Left errs -> mapM_ print errs
        Right tokens -> do
            (_,st,_) <- runRWST (preParse tokens) () ST.initialST
            print st

testParser :: IO ()
testParser = do
    str <- getContents
    case scanTokens str of
        Left errs -> mapM_ print errs
        Right tokens -> do
            mapM_ print tokens
            prettyPrintProgram $ parse tokens

