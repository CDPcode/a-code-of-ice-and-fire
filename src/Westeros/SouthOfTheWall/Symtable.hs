module Westeros.SouthOfTheWall.Symtable where

import qualified Data.Map.Strict as M
import qualified Westeros.SouthOfTheWall.Token as TK


type Symbol = String

data Category -- anything with a name
    = Type
    | Variable
    | Constant
    | Function
    | Param
    | Field
    | Alias
   deriving Show 
  
data AdditionalInfo 
    = ReturnTypes [String] -- For functions we save: name and return type(s).
    | PassType String        -- For parameters we save: name, type and either it is value or reference passed.
    | AliasType String       -- For aliases we save: name and the type it is a sinonym of.
    | PointedType String     -- For pointers we save: name and the pointed type
   deriving Show

data SymbolInfo = SymbolInfo 
    { category :: Category
    , scope :: Int
    , tp :: Maybe String                 -- pointer to a table entry (the type)
    , additional :: Maybe AdditionalInfo 
    }

data PassType = Value | Reference deriving Show

type Dict = M.Map Symbol [SymbolInfo]

data SymbolTable = SymbolTable 
    { dict :: Dict
    , scopeStack :: [Int]
    , nextScope :: Int    -- indicates the next scope to assign
    -- , additional :: someType 
    }


{- ST creation functions -}

getTypeInfo
    :: Tk.Token -- Typename
    -> (Symbol, SymbolInfo)
getTypeInfo = undefined

getStandardSymbolInfo 
    :: Tk.Token -- Variability
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> (Symbol, SymbolInfo)
getStandardSymbolInfo = undefined

getFunctionInfo
    :: Tk.Token   -- Id
    -> [Tk.Token] -- Return types
    -> (Symbol,SymbolInfo)
getFunctionInfo = undefined

getParamInfo
    :: Tk.Token -- Either value or reference
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> (Symbol, SymbolInfo)
getParamInfo = undefined

getFieldInfo
    :: Tk.Token -- Id
    -> Tk.Token -- Type
    -> (Symbol, SymbolInfo)
getFieldInfo = undefined

getAliasInfo 
    :: Tk.Token -- Id
    -> Tk.Token -- Pointed type
    -> Tk.Token -- Alias type
    -> (Symbol, SymbolInfo)
 getAliasInfo = undefined

insertDict :: Dict -> Symbol -> SymbolInfo -> Dict
insertDict = undefined
    
insertST :: SymbolTable -> Symbol -> SymbolInfo -> SymbolTable
insertST = undefined


{- ST lookup Functions -}

findSymbol :: SymbolTable -> String -> Maybe SymbolInfo
findSymbol = undefined