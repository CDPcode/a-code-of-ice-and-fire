module Westeros.SouthOfTheWall.Symtable where

import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Westeros.SouthOfTheWall.AST as Ast ( ParamType, AliasType, Type, Parameter )
import qualified Data.Map.Strict as M

import Control.Monad.RWS ( MonadState(put, get), MonadWriter(tell), RWST, when )
import Data.List (intercalate, find)
import Data.Maybe (fromJust)


type Symbol = String

type Entry = (Symbol,SymbolInfo)

data PassType = Value | Reference deriving (Show,Eq)

type Dict = M.Map Symbol [SymbolInfo]

type MonadParser = RWST () [String] SymbolTable IO

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
    , tp :: Maybe Ast.Type               
    , additional :: Maybe AdditionalInfo
} deriving Eq

data AdditionalInfo
    = AliasMD Ast.AliasType Ast.Type               
    | FunctionMD FunctionInfo              
    | PassType Ast.ParamType                   
   deriving (Show,Eq)

data FunctionInfo = FunctionInfo
    { nArgs :: Int
    , fParameters :: [Ast.Parameter]
    , fReturn :: [Ast.Type]
    , discriminant :: Bool
    } deriving (Show,Eq)

data SymbolTable = SymbolTable
    { dict :: Dict
    , scopeStack :: [Int]
    , nextScope :: Int    
    }




getFunctionMD :: SymbolInfo -> FunctionInfo
getFunctionMD SymbolInfo { additional = Just (FunctionMD e) } = e
getFunctionMD _                                               = error "getFunctionMD: Unpropper use. Report use"


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
findFunction params bucket = find (\e -> nArgs (getFunctionMD e) == params) bucket

currentScope :: MonadParser Int
currentScope = do
    SymbolTable { scopeStack = (s:_)} <- get
    return s

insertError :: String -> MonadParser ()
insertError msg = tell [msg]


{- Constants -}

pervasiveScope, defaultScope, functionScope :: Int

pervasiveScope = 0 
defaultScope   = maxBound 
functionScope  = 1

-- Symbol table to begin with scanning
initialST :: SymbolTable
initialST = SymbolTable {
        dict       = M.empty :: M.Map Symbol [SymbolInfo],
        scopeStack = [0],
        nextScope  = 1
    }