module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token (..))


displayErrorContext :: [Tk.Token] -> String
displayErrorContext [] = "error: Parse error at EOF."
displayErrorContext (x:xs) = "error: parse error with: \"" ++ Tk.cleanedString x 
                             ++ "\" at position " ++ show (Tk.position x) 
                             ++ "related to token: " ++ show (Tk.aToken x) 