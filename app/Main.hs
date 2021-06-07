module Main where

import Lib
import Westeros.SouthOfTheWall.Lexer (scanTokens)

main :: IO ()
main = do 
    str <- getContents  
    case scanTokens str of 
        Left errs -> mapM_ print errs
        Right tokens -> mapM_ print tokens

