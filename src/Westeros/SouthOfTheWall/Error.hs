module Westeros.SouthOfTheWall.Error where

import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token (..))


displayErrorContext :: [Tk.Token] -> String
displayErrorContext [] = "You died"
displayErrorContext (x:xs) = "Problem with: \"" ++ Tk.cleanedString x 
                             ++ "\" \nat " ++ show (Tk.position x) 
                             ++ "\nrelated to token: " ++ show (Tk.aToken x) 