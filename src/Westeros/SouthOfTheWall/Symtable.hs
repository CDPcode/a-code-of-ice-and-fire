module Westeros.SouthOfTheWall.Symtable where

import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.AST as Ast

import qualified Data.Map.Strict as M
import Control.Monad.RWS ( MonadState(put, get), RWST, when )

import Data.List (intercalate, find)
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
getFunctionMD _                             = error "getFunctionMD: Unpropper use. Report use"

data FunctionInfo = FunctionInfo
    { nArgs :: Int
    , fParameters :: [Parameter]
    , fReturn :: [Type]
    , discriminant :: Bool
    } deriving (Show,Eq)

completeFunctionEntry :: FunctionInfo -> Bool
completeFunctionEntry fInfo = null (fParameters fInfo) && null (fReturn fInfo)

data SymbolInfo = SymbolInfo
    { category :: Category
    , scope :: Int
    , tp :: Maybe Ast.Type               -- pointer to a table entry (the type)
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
                nArgs = read $ Tk.cleanedString argNumber :: Int,
                fParameters =  [],
                fReturn = [],
                discriminant = False
            })
        }

-- relevant get-* functions for preparser until here
-----------------------------------------------------------------------

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
        scopeStack = [0],
        nextScope  = 1
    }


{- Statefull functions to be called on rules -}

statefullSTupdate :: Entry -> MonadParser ()
statefullSTupdate entry@(name,info) = do
    st <- get
    case category info of
        Alias     -> case findSymbol st name of
                        Nothing -> put $ insertST st entry
                        Just _  -> let errMsg = "The name \""++name++"\" is an existing symbol"
                                   in fail errMsg
        Function  -> case findSymbol st name of
                        Nothing      -> put $ insertST ( st { nextScope = succ (nextScope st) } ) entry
                        Just entries -> do

                            let actualFunctions  = filter (\e -> category e == Function) entries
                                functionsEntries = map getFunctionMD actualFunctions
                                functionsArgs    = map nArgs functionsEntries
                                currentArgs      = nArgs ( getFunctionMD info )

                            if currentArgs `notElem` functionsArgs then
                                put $ insertST ( st { nextScope = succ (nextScope st) } ) entry
                                else let errMsg = "A function with the same name \""++name++"\" and # of arguments was already declared"
                                     in fail errMsg
        _         -> fail "Unexpected behaviour: Only Alias and Function categories are relevant in pre-parser"

openScope :: MonadParser ()
openScope = do
    symT <- get
    let newScope = nextScope symT
    let newStack = newScope : scopeStack symT
    put $ symT { scopeStack = newStack, nextScope = succ newScope }

closeScope :: MonadParser ()
closeScope = do
    symT <- get
    let (_:newStack) = scopeStack symT
    put $ symT { scopeStack = newStack }

findBest :: [SymbolInfo] -> [Int] -> Maybe SymbolInfo
findBest entries [] = Nothing
findBest entries (s:ss) = case filter (\e -> scope e == s) entries of
    [] -> findBest entries ss
    [a] -> Just a
    _ -> error "Somehow more than one id with same scope"

lookup :: Symbol -> MonadParser (Maybe SymbolInfo)
lookup sym = do
    symT <- get
    let stack = scopeStack symT
    let mBucket = findSymbol symT sym
    case mBucket of
        Nothing -> return Nothing
        Just bucket -> return $ findBest bucket stack
