module Main where

import Lib
import Westeros.SouthOfTheWall.Lexer (scanTokens)

main :: IO ()
main = do 
    str <- getContents  
    print $ scanTokens str

