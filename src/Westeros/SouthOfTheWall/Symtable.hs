module Westeros.SouthOfTheWall.Symtable where

import qualified Data.Map.Strict as M

type Symbol = String

data Category
    = Variable
    | Constant
    | Function
    | Param
    | Field
    | Type
    | WeakAlias
    | StrongAlias
   deriving Show 
  

data SymbolInfo = SymbolTable 
    { category :: Category
    , scope :: Int
    , tp :: Maybe String -- pointer to a table entry
    -- , extra :: someType
    }

type Dict = M.Map Symbol SymbolInfo

data SymbolTable = SymbolTable 
    { dict :: Dict
    , scopeStack :: [Int]
    -- , additional :: someType 
    }



insertST :: SymbolTable -> String -> SymbolInfo -> SymbolTable
insertST = undefined
-- how to handle equal keys?

findSymbol :: SymbolTable -> String -> Maybe SymbolInfo
findSymbol = undefined