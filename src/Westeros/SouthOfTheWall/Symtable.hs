module Westeros.SouthOfTheWall.Symtable where

import qualified Westeros.SouthOfTheWall.Error as Err

import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Data.Map.Strict as M

import Control.Monad.RWS ( MonadState(put, get), MonadWriter(tell), RWST, when )
import Data.List (intercalate, find)
import Data.Maybe (fromJust, isJust)

type Type = String

type Symbol = String

type Entry = (Symbol,SymbolInfo)

type Dictionary = M.Map Symbol [SymbolInfo]

type MonadParser = RWST () [Err.Error] SymbolTable IO

data AliasType = ByName | ByStructure deriving (Eq, Show)

data ParameterType = Value | Reference deriving (Eq, Show)

data Category 
    = Alias
    | Constant
    | Field
    | Function
    | Parameter
    | Type
    | Variable
   deriving (Show,Eq)

data SymbolInfo = SymbolInfo
    { category :: Category
    , scope :: Int
    , symbolType :: Maybe Type
    , additional :: Maybe AdditionalInfo
} deriving Eq

data AdditionalInfo
    = AliasMetaData AliasType Type
    | FunctionMetaData FunctionInfo              
    | ParameterType ParameterType                  
   deriving (Show,Eq)

data FunctionInfo = FunctionInfo
    { numberOfArgs :: Int
    , argumentTypes :: [Type]
    , returnTypes :: [Type]
    , defined :: Bool
    } deriving (Show,Eq)

data SymbolTable = SymbolTable
    { table :: Dictionary
    , scopeStack :: [Int]
    , nextScope :: Int    
    }

getFunctionMetaData :: SymbolInfo -> FunctionInfo
getFunctionMetaData SymbolInfo { additional = Just (FunctionMetaData e) } = e
getFunctionMetaData _ = error "getFunctionMetaData: Unpropper use. Report use"

{- Dictionary and ST functions -}

insertDictionary :: Dictionary -> Entry -> Dictionary
insertDictionary dict (s,si) = M.insertWith (++) s [si] dict

insertST :: SymbolTable -> Entry -> SymbolTable
insertST st entry = st { table = insertDictionary (table st) entry }

searchAndReplaceSymbol :: SymbolTable -> Entry -> SymbolInfo -> SymbolTable
searchAndReplaceSymbol st entry@(name,info) newInfo = st { table = newDictionary }
    where
        newDictionary = M.insert name (fromJust newList) (table st)
        newList = do
           oldList <- findSymbol st name
           return (searchAndReplace newInfo info oldList)

searchAndReplace :: Eq a => a -> a -> [a] -> [a]
searchAndReplace _   _   [] = []
searchAndReplace new old (x:xs)
 | x == old  = new : xs
 | otherwise = x : searchAndReplace new old xs

{- Filtering by scope -}

filterByScopeDictionary :: Dictionary -> Int -> [(Symbol,[SymbolInfo])]
filterByScopeDictionary dict referenceScope = filter (null . snd) defEntries
    where 
        defEntries = map filterEntries . M.toList $ dict 
        filterEntries (a,symInfList) = (a,filter (\symInfo -> 
                                        scope symInfo == referenceScope) symInfList)

-- ^ Assumes there are no repeated symbol names in any given scope.
filterByScopeDictionary' :: Dictionary -> Int -> [(Symbol,SymbolInfo)]
filterByScopeDictionary' dict refScope 
        = foldr (\(x,d) acc -> if isJust d then (x,fromJust d) : acc 
                                           else acc ) [] foundEntries
    where
        findEntries (a,xs) = (a, find (\e -> scope e == refScope) xs)
        foundEntries = map findEntries . M.toList $ dict
        
filterByScopeST :: SymbolTable -> Int -> [(Symbol,SymbolInfo)]
filterByScopeST = filterByScopeDictionary' . table

{- ST lookup Functions -}

findDictionary :: Dictionary -> Symbol -> Maybe [SymbolInfo]
findDictionary = flip M.lookup

findSymbol :: SymbolTable -> Symbol -> Maybe [SymbolInfo]
findSymbol st = findDictionary (table st)

checkExisting :: SymbolTable -> Symbol -> Bool
checkExisting st sym = case findSymbol st sym of
    Nothing -> False
    Just _  -> True


{- Statefull functions to be called on rules -}

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

lookupFunction :: Symbol -> Int -> MonadParser (Maybe SymbolInfo)
lookupFunction sym params = do
    symT <- get
    let mBucket = findSymbol symT sym
    case mBucket of
        Nothing -> return Nothing
        Just bucket -> return $ findFunction params bucket

findFunction :: Int -> [SymbolInfo] -> Maybe SymbolInfo
findFunction params [] = Nothing
findFunction params bucket = find (\e -> numberOfArgs (getFunctionMetaData e) == params) bucket

currentScope :: MonadParser Int
currentScope = do
    SymbolTable { scopeStack = (s:_)} <- get
    return s

insertError :: Err.Error -> MonadParser ()
insertError msg = tell [msg]


{- Constants -}

pervasiveScope, defaultScope, functionScope :: Int

pervasiveScope = 0 
defaultScope   = maxBound 
functionScope  = 1

{- Initial types -}

-- Symbol table to begin with scanning
initialST :: SymbolTable
initialST = SymbolTable {
        table       = M.empty :: M.Map Symbol [SymbolInfo],
        scopeStack = [0],
        nextScope  = 1
    }

typesSymbolInfo :: SymbolInfo
typesSymbolInfo = SymbolInfo { 
    category   = Type,
    scope      = pervasiveScope,
    symbolType = Nothing,
    additional = Nothing
}

initialTypes :: [String]
initialTypes = ["_int","_float","_char","_bool","_atom","_string","_array"] -- union, struct, tuple, pointer, array, alias

initializedST :: SymbolTable
initializedST = foldl insertST initialST entries 
    where entries = zip initialTypes (repeat typesSymbolInfo)