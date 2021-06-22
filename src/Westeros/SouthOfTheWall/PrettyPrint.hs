module Westeros.SouthOfTheWall.PrettyPrint where

import qualified Data.Map as M (toList) 

import Westeros.SouthOfTheWall.Symtable ( SymbolTable(dict, scopeStack, nextScope), SymbolInfo(category, scope, additional) )
import Data.List (intercalate)

instance Show SymbolInfo where
    show = prettySymbolInfo 1

prettySymbolInfo :: Int -> SymbolInfo -> String
prettySymbolInfo n si = preTb "Category: " ++ show (category si) ++ "\n"
                     ++ preTb "Scope : " ++ show ( scope si )  ++ "\n"
                     ++ preTb "extra: " ++ show ( additional si ) ++ "\n"
    where preTb = (replicate n '\t' ++)


instance Show SymbolTable where
    show st = "* Name info\n\n" ++ displayDict
              ++ "* Scope stack: " ++ show (scopeStack st)
              ++ "\n* Next scope: " ++ show (nextScope st)
        where
            displayDict  = foldl (\acc (b,c) -> acc ++ b ++ '\n' : splitInfo c) [] $ M.toList (dict st)
            splitInfo = intercalate bar . map show
            bar = "\n-------------------\n"