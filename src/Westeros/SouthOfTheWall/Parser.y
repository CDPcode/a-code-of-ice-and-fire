{
module Westeros.SouthOfTheWall.Parser (parse) where
    import qualified Westeros.SouthOfTheWall.Tokens as Tk
}

%name            parse 
%tokentype       { Tk.Token }
%error           { parseError }
-- TODO: %monad expr to properly handle errors

-- Token definitions
token %
    
-- Precedences

%% -- Grammar

{
    -- Helper functions

-- TODO:
-- Error Function
-- parseError :: [TokenPos] -> a
}