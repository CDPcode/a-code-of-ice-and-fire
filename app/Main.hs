module Main where

import Westeros.SouthOfTheWall.AST          (prettyPrintProgram)
import Westeros.SouthOfTheWall.Lexer        (scanTokens)
import Westeros.SouthOfTheWall.Parser       (parse)
import Westeros.SouthOfTheWall.PreParser    (preParse)
import Westeros.SouthOfTheWall.PrettyPrint  (pretty)

import qualified Westeros.SouthOfTheWall.Symtable as ST

import Control.Monad.RWS ( RWST(runRWST), unless )
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
        [] -> mapM_ pretty tokens
        _  -> mapM_ print errors

testPreParser :: IO ()
testPreParser = do
    str <- getContents
    let (errors, tokens) = scanTokens str
    unless (null errors) (mapM_ print errors)
    (_, preSymbolTable, errs) <- runRWST (preParse tokens) () ST.initialST
    if null errs
        then pretty preSymbolTable
        else mapM_ pretty errs

testParser :: IO ()
testParser = do
    str <- getContents
    let(errors, tokens) = scanTokens str
    unless (null errors) (mapM_ print errors)
    (_, preSymbolTable, errs) <- runRWST (preParse tokens) () ST.initialST
    if null errs
        then do
            (ast, finalSt, errs') <- runRWST (parse tokens) () preSymbolTable{ ST.scopeStack=[1,0], ST.nextSymAlias = 0, ST.offsetStack = [0], ST.nextScope = 2 }
            if null errs'
                then do
                    prettyPrintProgram ast
                    pretty finalSt
            else mapM_ pretty errs'
        else mapM_ pretty errs

