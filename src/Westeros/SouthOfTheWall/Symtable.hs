module Westeros.SouthOfTheWall.Symtable where

import qualified Data.Map.Strict as M
import qualified Westeros.SouthOfTheWall.Tokens as Tk
import Control.Monad.State  


type Symbol = String

-- Just a listing of known posisble errors. NOT a thing to use rn.
data ParserError
    = InvalidToken         -- given token does not fit context of grammar rule
    | InvalidVariability   -- given var/const does not fit meta-context for what is being defined
    | InvalidPrimitiveType -- given type for a primitive declaration is composite
    | InvalidCompositeType -- given type for a composite decalration is primitive


data Category -- anything with a name
    = Alias    
    | Constant
    | Field    
    | Function
    | Parameter
    | Type
    | Variable
   deriving Show 
  
data AdditionalInfo 
    = AliasMetaData (String,String) -- For aliases we save: name, type of alias and the type it is a sinonym of
    | PassType String               -- For parameters we save: name, type and either it is value or reference passed
    | ReturnTypes [String]          -- For functions we save: name and return type(s).

    -- Special cases for composite types

    | ArrayMetaData Int         -- For arrays we save: name and size
    | StringMetaData Int        -- For strings we save: name and size
    | PointerMetaData String    -- For pointers we save: name a
    | StructMetaData [String]   -- For structs we save: name and fields name
    | UnionMetaData [String]    -- For unions we save: name and fields names
    | TupleMetaData [String]    -- For tuples we save: name and types names
   deriving Show

data SymbolInfo = SymbolInfo 
    { category :: Category
    , scope :: Int
    , tp :: Maybe String                 -- pointer to a table entry (the type)
    , additional :: Maybe AdditionalInfo 
    }

type Entry = (Symbol,SymbolInfo)

data PassType = Value | Reference deriving Show

type Dict = M.Map Symbol [SymbolInfo]

data SymbolTable = SymbolTable 
    { dict :: Dict
    , scopeStack :: [Int]
    , nextScope :: Int    -- indicates the next scope to assign
    -- , additional :: someType 
    }


{- ST creation functions -}

toTypeSymbol :: Tk.Token -> Symbol
toTypeSymbol token = case Tk.aToken token of 
    Tk.TknInt          -> "int"
    Tk.TknChar         -> "char"
    Tk.TknFloat        -> "float"
    Tk.TknBool         -> "bool"
    Tk.TknAtom         -> "atom"
    
    Tk.TknPointerType  -> "pointer"
    Tk.TknString       -> "string"
    Tk.TknBeginArray   -> "array"
    Tk.TknBeginStruct  -> "struct"
    Tk.TknBeginUnion   -> "union"
    Tk.TknBeginTuple   -> "tuple"

    _                  -> error "Not a token Type" -- OJO : enhance error correction

getAliasInfo 
    :: Tk.Token -- Alias name
    -> Tk.Token -- Alias type
    -> Tk.Token -- Pointed Type
    -> Entry
getAliasInfo aliasName aliasType pointedType = undefined
    where
        symbol = Tk.cleanedString aliasName 
        info   = SymbolInfo { 
            category   = Alias, 
            scope      = pervasiveScope, 
            tp         = Nothing,
            additional = let md = (chooseAliasType aliasType, toTypeSymbol pointedType)
                         in Just $ AliasMetaData md
        }

        chooseAliasType at = case Tk.aToken at of 
                            Tk.TknWeakAlias   -> "weak'"
                            Tk.TknStrongAlias -> "strong"
                            _                 -> error "Invalid alias type"
                            -- OJO : enhance error management

getFunctionInfo
    :: Tk.Token   -- Id
    -> [Tk.Token] -- Return types
    -> Entry
getFunctionInfo id returnTypes = (symbol, info)
 where
    symbol = Tk.cleanedString id 
    info   = SymbolInfo { 
        category   = Function,
        scope      = functionScope,
        tp         = Nothing, 
        additional = Just $ ReturnTypes (map toTypeSymbol returnTypes)
    }

getParamInfo
    :: Tk.Token -- Either value or reference
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> Entry
getParamInfo passType id symType = (symbol, info)
    where 
        symbol = Tk.cleanedString id 
        info   = SymbolInfo { 
            category   = Parameter,
            scope      = defaultScope, -- adjust Scope at ST insertion
            tp         = Just (toTypeSymbol symType),
            additional = case Tk.aToken passType of 
                            Tk.TknValueArg     -> Just (PassType "value")
                            Tk.TknReferenceArg -> Just (PassType "reference")
                            _                  -> error "Invalid Variability" 
                            -- OJO : enhance error management
        }


{-
PROGRAM : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                                   { beginSymbolScan }
        | HEADER CONTENTS GLOBAL FUNCTIONS MAIN ALIASES                                           { beginSymbolScan }

-- concerning functions
FUNCTION : id FUNCTION_PARAMETERS FUNCTION_RETURN FUNCTION_BODY                                    { StatefullSTupdate (getFuncionInfo $1 $3) }

FUNCTION_RETURN : beginReturnVals RETURN_TYPES endReturnVals                                       { $2 } 

RETURN_TYPES : void                                                                                { $1 }
             | TYPES                                                                               { $1 }

TYPES : TYPE                                                                                       { [$1] }
      | TYPES ',' TYPE                                                                             { $2 : $1 } -- reverse

PARAMETER: PARAMETER_TYPE id TYPE                                                                  { StatefullSTupdate (getParamInfo $1 $2 $3) }  -- include current scope

PARAMETER_TYPE : valueArg                                                                          {$1}
               | refArg                                                                            {$1}

-- concerning aliases
ALIAS_DECLARATION : beginAlias id ALIAS_TYPE TYPE '.'                                              { StatefullSTupdate (getAliasInfo $2 $3 $4) }

ALIAS_TYPE : strongAlias                                                                           {$1} 
           | weakAlias                                                                             {$1} 


TYPE : PRIMITIVE_TYPE                                                                              {$1}
     | COMPOSITE_TYPE                                                                              {$1}
     | id                                                                                           

PRIMITIVE_TYPE : int                                                                               {$1}
               | float                                                                             {$1}
               | char                                                                              {$1}
               | bool                                                                              {$1}
               | atom                                                                              {$1}

COMPOSITE_TYPE : beginArray naturalLit TYPE endArray                                               {$1}
               | string                                                                            {$1}
               | pointerType TYPE                                                                  {$1}
               | beginStruct SIMPLE_DECLARATIONS endStruct                                         {$1}
               | beginUnion SIMPLE_DECLARATIONS endUnion                                           {$1}
               | beginTuple TUPLE_TYPES endTuple                                                   {$1}

-}

-- relevant get-* functions for preparser until here
-----------------------------------------------------------------------


getPrimitiveSymbolInfo 
    :: Tk.Token -- Variability
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> Entry
getPrimitiveSymbolInfo variability id symType = (symbol, info)
    where
        symbol = Tk.cleanedString id 
        info   = SymbolInfo { 
            category   = case Tk.aToken variability of 
                            Tk.TknVar   -> Variable
                            Tk.TknConst -> Constant
                            _           -> error "Invalid Variability", 
                            -- OJO : enhance error management
            scope      = defaultScope,
            tp         = Just (toTypeSymbol symType),
            additional = Nothing
        }

getCompositeSymbolInfo 
    :: Tk.Token -- Variability
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> Entry
getCompositeSymbolInfo variability id symType = undefined
    where
        symbol = Tk.cleanedString id 
        info   = SymbolInfo { 
            category   = case Tk.aToken variability of 
                            Tk.TknVar        -> Variable
                            Tk.TknVarPointer -> Variable
                            Tk.TknConst      -> Constant
                            _                -> error "Invalid Variability", 
                            -- OJO : enhance error management
            scope      = defaultScope, -- adjust scope at ST insertion
            tp         = Just (toTypeSymbol symType),
            additional = Nothing
        }

getFieldInfo
    :: Tk.Token -- Id
    -> Tk.Token -- Type
    -> Entry
getFieldInfo id symType = undefined
    where  
        symbol = Tk.cleanedString id
        info = SymbolInfo { 
            category   = Field,
            scope      = defaultScope, -- adjust scope at ST insertion
            tp         = Just (toTypeSymbol symType),
            additional = Nothing
        } 

getTypeInfo
    :: Tk.Token -- Typename
    -> Entry
getTypeInfo typename = (symbol, info)
    where
        symbol = Tk.cleanedString typename 
        info = SymbolInfo {                
            category   = Type,
            scope      = pervasiveScope,
            tp         = Nothing ,
            additional = Nothing 
        }

{-


PRIMITIVE_DECLARATION : var id type TYPE                                                           

COMPOSITE_DECLARATION : beginCompTypeId var id endCompTypeId TYPE                                 CT  
                      | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz          CT
                      | beginCompTypeId pointerVar id endCompTypeId TYPE                          CT
                      | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz   CT

CONST_DECLARATION : const id type TYPE constValue EXPR                                              
                  | beginCompTypeId const id endCompTypeId TYPE constValue EXPR                   CT


TYPE : PRIMITIVE_TYPE                                                                               
     | COMPOSITE_TYPE                                                                               
     | id                                                                                           

PRIMITIVE_TYPE : int                                                                                
               | float                                                                              
               | char                                                                               
               | bool                                                                               
               | atom                                                                               

COMPOSITE_TYPE : beginArray naturalLit TYPE endArray                                               EX - sz
               | string                                                                            EX - sz
               | pointerType TYPE                                                                  EX - tp
               | beginStruct SIMPLE_DECLARATIONS endStruct                                         EX - dec , tp
               | beginUnion SIMPLE_DECLARATIONS endUnion                                           EX - dec , tp
               | beginTuple TUPLE_TYPES endTuple                                                   EX - sz , tp

SIMPLE_DECLARATIONS : SIMPLE_DECLARATION                                                           { [$1] } 
                    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                                   { $2 : $1 }

SIMPLE_DECLARATIO : PRIMITIVE_DECLARATION                                                          { $1 }  
                   | COMPOSITE_DECLARATION                                                         { $1 }  

TUPLE_TYPES: {- empty -}                                                                           { [] }
           | TYPES                                                                                 { $1 } }

TYPES : TYPE                                                                                       { [$1] }
      | TYPES ',' TYPE                                                                             { $2 : $1 }  

-}

insertDict :: Dict -> Entry -> Dict
insertDict dict (k,v) = M.insertWith (++) k [v] dict
    

insertST :: SymbolTable -> Entry -> SymbolTable
insertST st entry = st { dict = insertDict (dict st) entry }


{- ST lookup Functions -}

findSymbol :: SymbolTable -> String -> Maybe SymbolInfo
findSymbol = undefined


{- Constants -}

-- Default type literals
tps = [ "int" , "char" , "float" , "bool" , "atom", "string", "array", "struct", "union", "pointer", "tuple" ]


pervasiveScope = 0
defaultScope   = maxBound :: Int
functionScope  = 1


initialTypes :: [SymbolInfo] 
initialTypes = map buildDefaultTypeSymbolInfo tps
    where
        buildDefaultTypeSymbolInfo typeSymbol = SymbolInfo { 
            category   = Type,
            scope      = pervasiveScope,
            tp         = Just typeSymbol,
            additional = Nothing
        }

-- Symbol table to begin with scanning
emptyST :: SymbolTable 
emptyST = st { dict = newDictionary }
    where  
        newDictionary  = foldl insertDict (dict st) initialEntries

        initialEntries = zip tps initialTypes

        st = SymbolTable { 
        dict       = M.empty :: M.Map Symbol [SymbolInfo],
        scopeStack = [],
        nextScope  = 1
    }


{- Statefull functions to be called on rules -}

-- Initial ST holding naming data for built-in types.
beginSymbolScan :: State SymbolTable () 
beginSymbolScan = void (put emptyST)


-- Tentative initial implementatino of statefull colletion of symbols.
statefullSTupdate :: Entry -> State SymbolTable ()
statefullSTupdate entry@(name,info) = do 
    st <- get 
    case category info of 
        Alias     -> put $ insertST st entry
        Function  -> put $ insertST ( st { nextScope = succ (nextScope st) } ) entry
        Parameter -> let updatedEntry = (name,info { scope = nextScope st })
                     in put $ insertST st updatedEntry 