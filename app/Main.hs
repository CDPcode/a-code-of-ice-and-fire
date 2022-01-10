module Main (main) where

import Westeros.SouthOfTheWall.AST          (prettyPrintProgram)
import Westeros.SouthOfTheWall.Lexer        (scanTokens)
import Westeros.SouthOfTheWall.Parser       (parse)
import Westeros.SouthOfTheWall.PreParser    (preParse)
import Westeros.SouthOfTheWall.PrettyPrint  (pretty)

import qualified Westeros.SouthOfTheWall.Symtable as ST
import qualified Westeros.SouthOfTheWall.TACGeneration as TACG
import qualified TACTypes.TAC as TAC

import Data.Foldable (toList)
import Control.Monad.RWS ( RWST(runRWST), unless )
import System.Environment (getArgs)
import qualified Westeros.SouthOfTheWall.TACGeneration as TACG

main :: IO ()
main = do
    args <- getArgs
    case head args of
        "lex"   -> testLexer
        "preparse" -> testPreParser
        "parse" -> testParser
        "tac"   -> testTAC
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
            prettyPrintProgram ast
            pretty finalSt
            mapM_ pretty errs'
        else mapM_ pretty errs

testTAC :: IO ()
testTAC = do
    str <- getContents
    let (errors, tokens) = scanTokens str
    unless (null errors) (mapM_ print errors)
    (_, preSymbolTable, errs) <- runRWST (preParse tokens) () ST.initialST
    if null errs
        then do
            (ast, finalSt, errs') <- runRWST (parse tokens) () preSymbolTable{ ST.scopeStack=[1,0], ST.nextSymAlias = 0, ST.offsetStack = [0], ST.nextScope = 2 }
            unless (null errs') $ do
                mapM_ pretty errs
            let tacSeq = ST.tacCode finalSt
                tacList = toList tacSeq
                globalTac = filter TACG.isGlobalCode tacList
                otherTac = filter (not . TACG.isGlobalCode) tacList
                tacProgram = TAC.TACProgram $ globalTac ++ otherTac
            print tacProgram
        else mapM_ pretty errs
