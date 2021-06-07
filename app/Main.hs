module Main where

import Lib
import Westeros.SouthOfTheWall.Lexer (scanTokens)

main :: IO ()
main = getLine >>= print . scanTokens -- readFile >>= print . scanTokens
