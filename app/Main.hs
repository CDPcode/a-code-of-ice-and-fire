module Main where

import Westeros.SouthOfTheWall.AST          (prettyPrintProgram)
import Westeros.SouthOfTheWall.Lexer        (scanTokens)
import Westeros.SouthOfTheWall.Parser       (parse)
import Westeros.SouthOfTheWall.PreParser    (preParse)

import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.Tokens as Tk

import Control.Monad.RWS ( RWST(runRWST), when )
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
    let(errors, tokens) = scanTokens str
    case errors of
        [] -> mapM_ print tokens
        _  -> mapM_ print errors

testPreParser :: IO ()
testPreParser = do
    str <- getContents
    let(errors, tokens) = scanTokens str
    when (null errors) (mapM_ print errors)
    (_, preSymbolTable, _) <- runRWST (preParse tokens) () ST.initialST
    print preSymbolTable

testParser :: IO ()
testParser = do
    str <- getContents
    let(errors, tokens) = scanTokens str
    when (null errors) (mapM_ print errors)
    (_, preSymbolTable, _) <- runRWST (preParse tokens) () ST.initialST
    (ast, finalSt, _) <- runRWST (parse tokens) () preSymbolTable{ST.scopeStack=[1,0]}
    prettyPrintProgram ast
    print finalSt

