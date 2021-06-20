module Main where

import Lib
import Westeros.SouthOfTheWall.AST      (prettyPrintProgram)
import Westeros.SouthOfTheWall.Lexer    (scanTokens)
import Westeros.SouthOfTheWall.Parser   (parse)
import System.Environment               (getArgs)

main :: IO ()
main = do
    args <- getArgs

    case head args of
        "lex"   -> llex
        "parse" -> pparse
        _       -> putStrLn "Invalid option"

llex :: IO ()
llex = do
    str <- getContents
    case scanTokens str of
        Left errs -> mapM_ print errs
        Right tokens -> mapM_ print tokens

pparse :: IO ()
pparse = do
    str <- getContents
    case scanTokens str of
        Left errs -> mapM_ print errs
        Right tokens -> do
            mapM_ print tokens
            prettyPrintProgram $ parse tokens

