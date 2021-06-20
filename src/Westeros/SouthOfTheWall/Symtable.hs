module Westeros.SouthOfTheWall.Symtable where

import qualified Westeros.SouthOfTheWall.Tokens as Tk

import qualified Data.Map.Strict as M
import Control.Monad.RWS ( MonadState(put, get), RWST )

import Data.List (intercalate)
import Data.Maybe (fromJust)


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
   deriving (Show,Eq)

data AdditionalInfo
    = AliasMD AliasType Type               -- For aliases we save: name and necessary info of the sinonymed type
    | FunctionMD FunctionInfo -- For functions we save: name , number of arguments, and return type(s). 
    | PassType String                      -- For parameters we save: name, type and either it is value or reference passed
   deriving (Show,Eq)

getFunctionMD :: SymbolInfo -> FunctionInfo
getFunctionMD SymbolInfo { additional = Just (FunctionMD e) } =  e
getFunctionMD _                             = error "getFunctionMD: Unpropper use"

data FunctionInfo = FunctionInfo
    { nArgs :: String -- OJO
    , fParameters :: [Parameter]
    , fReturn :: [Type]
    , discriminant :: Bool
    } deriving (Show,Eq)

completeFunctionEntry :: FunctionInfo -> Bool 
completeFunctionEntry fInfo = null (fParameters fInfo) && null (fReturn fInfo) 

--

data Type
    = Int
    | Char
    | Bool
    | Float
    | Atom
    | Array Int Type
    | Register [Declaration]
    | VRegister [Declaration]
    | Str
    | Ptr Type
    | Tuple [Type]
    | Als String
   deriving (Show,Eq)

type Id = String

data Variability = Var | Const | PtrVar deriving (Show,Eq)

type Declaration = (Variability, Id, Type)

data AliasType = Strong | Weak deriving (Show,Eq)

type Parameter = (PassType, Id, Type)

--

data SymbolInfo = SymbolInfo
    { category :: Category
    , scope :: Int
    , tp :: Maybe String                 -- pointer to a table entry (the type)
    , additional :: Maybe AdditionalInfo
} deriving Eq

instance Show SymbolInfo where
    show = prettySymbolInfo 1

prettySymbolInfo :: Int -> SymbolInfo -> String
prettySymbolInfo n si = preTb "Category: " ++ show (category si) ++ "\n"
                     ++ preTb "Scope : " ++ show ( scope si )  ++ "\n"
                     ++ preTb "extra: " ++ show ( additional si ) ++ "\n"
    where preTb = (replicate n '\t' ++)

type Entry = (Symbol,SymbolInfo)

data PassType = Value | Reference deriving (Show,Eq)

type Dict = M.Map Symbol [SymbolInfo]

data SymbolTable = SymbolTable
    { dict :: Dict
    , scopeStack :: [Int]
    , nextScope :: Int    -- indicates the next scope to assign
    -- , additional :: someType 
    }

instance Show SymbolTable where
    show st = "* Name info\n\n" ++ displayDict
              ++ "* Scope stack: " ++ show (scopeStack st)
              ++ "\n* Next scope: " ++ show (nextScope st)
        where
            displayDict  = foldl (\acc (b,c) -> acc ++ b ++ '\n' : splitInfo c) [] $ M.toList (dict st)
            splitInfo = intercalate bar . map show
            bar = "\n-------------------\n"

type MonadParser = RWST () [ParserError] SymbolTable IO


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

    Tk.TknVoid         -> "void"     -- represents "No type"

    _                  -> error ("Not a valid type token: " ++ show (Tk.aToken token)) -- OJO : enhance error correction


getAliasInfo
    :: Tk.Token  -- Alias name
    -> AliasType -- Alias type
    -> Type      -- Pointed Type
    -> Entry
getAliasInfo aliasName aliasType pointedType = (symbol, info)
    where
        symbol = Tk.cleanedString aliasName
        info   = SymbolInfo {
            category   = Alias,
            scope      = pervasiveScope,
            tp         = Nothing,
            additional = Just $ AliasMD aliasType pointedType
        }

getFunctionDeclarationInfo
    :: Tk.Token -- Id of function
    -> Tk.Token -- Number of arguments
    -> Entry
getFunctionDeclarationInfo id argNumber = (symbol, info)
    where
        symbol = Tk.cleanedString id
        info   = SymbolInfo {
            category   = Function,
            scope      = functionScope,
            tp         = Nothing,
            additional = Just $ FunctionMD (FunctionInfo {
                nArgs = Tk.cleanedString argNumber,
                fParameters =  [],
                fReturn = [],
                discriminant = False
            })
        }
{-
getFunctionDefinitionInfo
    :: String   -- Id of Function
    -> [Parameter] --  List of parameters
    -> [Type]      -- return types
    -> 
getFunctionDefinitionInfo id params types = undefined
-}

-- relevant get-* functions for preparser until here
-----------------------------------------------------------------------


getFunctionInfo
    :: Tk.Token   -- Id
    -> [Tk.Token] -- Return types
    -> Entry
getFunctionInfo id returnTypes = undefined

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

-- relevant get-* functions for parser until here
-----------------------------------------------------------------------

{- Dictionary and ST functions -}

insertDict :: Dict -> Entry -> Dict
insertDict dict (k,v) = M.insertWith (++) k [v] dict

insertST :: SymbolTable -> Entry -> SymbolTable
insertST st entry = st { dict = insertDict (dict st) entry }

searchAndReplaceSymbol :: SymbolTable -> Entry -> SymbolInfo -> SymbolTable
searchAndReplaceSymbol st entry@(name,info) newInfo = st { dict = newDict }
    where            
        newDict = M.insert name (fromJust newList) (dict st)
        newList = do 
           oldList <- findSymbol st name 
           return (searchAndReplace newInfo info oldList)
        

searchAndReplace :: Eq a => a -> a -> [a] -> [a]
searchAndReplace _   _   [] = [] 
searchAndReplace new old (x:xs) 
 | x == old  = new : xs
 | otherwise = x : searchAndReplace new old xs

{- ST lookup Functions -}

findDict :: Dict -> Symbol -> Maybe [SymbolInfo]
findDict = flip M.lookup

findSymbol :: SymbolTable -> Symbol -> Maybe [SymbolInfo]
findSymbol st = findDict (dict st)

checkExisting :: SymbolTable -> Symbol -> Bool
checkExisting st sym = case findSymbol st sym of
    Nothing -> False
    Just _  -> True

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
initialST :: SymbolTable
initialST = st { dict = newDictionary }
    where
        newDictionary  = foldl insertDict (dict st) initialEntries

        initialEntries = zip tps initialTypes

        st = SymbolTable {
        dict       = M.empty :: M.Map Symbol [SymbolInfo],
        scopeStack = [],
        nextScope  = 1
    }


{- Statefull functions to be called on rules -}

statefullSTupdate :: Entry -> MonadParser ()
statefullSTupdate entry@(name,info) = do
    st <- get
    case category info of
        Alias     -> case findSymbol st name of
                        Nothing -> put $ insertST st entry
                        Just _  -> fail "existing alias"  -- OJO
        Function  -> case findSymbol st name of
                        Nothing      -> put $ insertST ( st { nextScope = succ (nextScope st) } ) entry
                        Just entries -> do

                            let actualFunctions  = filter (\e -> category e == Function) entries 
                                functionsEntries = map getFunctionMD actualFunctions
                                functionsArgs    = map nArgs functionsEntries
                                currentArgs      = nArgs ( getFunctionMD info )

                            if currentArgs `notElem` functionsArgs then
                                put $ insertST ( st { nextScope = succ (nextScope st) } ) entry
                                else fail "A function with the same name and # of arguments exists " -- OJO

        _         -> undefined