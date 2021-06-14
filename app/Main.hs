module Main where

import Lib
import Westeros.SouthOfTheWall.Lexer (scanTokens)
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs

    case head args of 
        "--lex"   -> lex
        "--parse" -> undefined
        _         -> putStrLn "Invalid option"

lex :: IO ()
lex = do
    str <- getContents  
    case scanTokens str of 
        Left errs -> mapM_ print errs
        Right tokens -> mapM_ print tokens

