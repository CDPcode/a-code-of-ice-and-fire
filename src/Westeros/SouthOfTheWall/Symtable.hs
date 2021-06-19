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
    | Param    
    | Type
    | Variable
   deriving Show 
  
data AdditionalInfo 
    = AliasType String        -- For aliases we save: name and the type it is a sinonym of.
    | PassType String         -- For parameters we save: name, type and either it is value or reference passed
    | ReturnTypes [String]    -- For functions we save: name and return type(s).

    -- Special cases for composite types
    | ArrayMetaData Int       -- For arrays we save: name and size
    | StringMetaData Int      -- For strings we save: name and size
    | PointerMetaData String  -- For pointers we save: name a
    | StructMetaData [String] -- For structs we save: name and fields name
    | UnionMetaData [String]  -- For unions we save: name and fields names
    | TupleMetaData [String]  -- For tuples we save: name and types names
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
    -> Tk.Token -- Pointed type
    -> (Symbol, SymbolInfo)
getAliasInfo aliasName pointedType = undefined
    where
        symbol = Tk.cleanedString aliasName 
        info   = SymbolInfo { 
            category   = Alias, 
            scope      = pervasiveScope, 
            tp         = Nothing,
            additional = Just $ AliasType (toTypeSymbol pointedType)
        }

getPrimitiveSymbolInfo 
    :: Tk.Token -- Variability
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> (Symbol, SymbolInfo)
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

{-

PROGRAM : HEADER CONTENTS GLOBAL FUNCTIONS MAIN                                                   { beginSymbolScan }
        | HEADER CONTENTS GLOBAL FUNCTIONS MAIN ALIASES                                           { beginSymbolScan }

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

getCompositeSymbolInfo 
    :: Tk.Token -- Variability
    -> Tk.Token -- Id
    -> Tk.Token -- Type
    -> (Symbol, SymbolInfo)
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
            scope      = defaultScope,
            tp         = Just (toTypeSymbol symType),
            additional = Nothing
        }

getFieldInfo
    :: Tk.Token -- Id
    -> Tk.Token -- Type
    -> (Symbol, SymbolInfo)
getFieldInfo id symType = undefined
    where  
        symbol = Tk.cleanedString id
        info = SymbolInfo { 
            category   = Field,
            scope      = defaultScope,
            tp         = Just (toTypeSymbol symType),
            additional = Nothing
        } 

getFunctionInfo
    :: Tk.Token   -- Id
    -> [Tk.Token] -- Return types
    -> (Symbol,SymbolInfo)
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
    -> (Symbol, SymbolInfo)
getParamInfo passType id symType = (symbol, info)
    where 
        symbol = Tk.cleanedString id 
        info   = SymbolInfo { 
            category   = Param,
            scope      = defaultScope,
            tp         = Just (toTypeSymbol symType),
            additional = case Tk.aToken passType of 
                            Tk.TknValueArg     -> Just (PassType "value")
                            Tk.TknReferenceArg -> Just (PassType "reference")
                            _                  -> error "Invalid Variability" 
                            -- OJO : enhance error management
        }

getTypeInfo
    :: Tk.Token -- Typename
    -> (Symbol, SymbolInfo)
getTypeInfo typename = (symbol, info)
    where
        symbol = Tk.cleanedString typename 
        info = SymbolInfo {                
            category   = Type,
            scope      = pervasiveScope,
            tp         = Nothing ,
            additional = Nothing 
        }

insertDict :: Dict -> (Symbol ,SymbolInfo) -> Dict
insertDict dict (k,v) = M.insertWith (++) k [v] dict
    

insertST :: SymbolTable -> (Symbol ,SymbolInfo) -> SymbolTable
insertST st (k,v) = case category v of 
    Variable -> st { dict = insertDict (dict st) (k,v) } 
    Constant -> st { dict = insertDict (dict st) (k,v) } 
    Function -> undefined
    Param    -> undefined
    Field    -> undefined
    Alias    -> undefined
    _        -> undefined
-- intended to be called in some parser rule like such: 
-- var id type TYPE   { insertST (getPrimitiveSymbolInfo $1 $2 $3) } 


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

beginSymbolScan :: State SymbolTable () 
beginSymbolScan = void (put emptyST)