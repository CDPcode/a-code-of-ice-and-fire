module Westeros.SouthOfTheWall.Symtable where

import Data.Bifunctor       (second)
import Data.Foldable        (foldl')
import Control.Monad.RWS    ( MonadState(put, get), MonadWriter(tell), RWST, when )
import Data.List            (intercalate, find)
import Data.Maybe           (fromJust, isJust)

import qualified Data.Map.Strict as M
import qualified Westeros.SouthOfTheWall.Error as Err
import qualified Westeros.SouthOfTheWall.Tokens as Tk

type Type = String

type Symbol = String

type Scope = Int

type SymAlias = Int

type Entry = (Symbol, SymbolInfo)

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
    { category      :: Category
    , scope         :: Scope
    , symbolType    :: Maybe Type
    , additional    :: Maybe AdditionalInfo
} deriving Eq

data AdditionalInfo
    = AliasMetaData AliasType Type
    | DopeVector Type Int
    | PointedType Type
    | StructScope Scope
    | UnionScope Scope
    | TupleTypes [Type]
    | FunctionMetaData FunctionInfo
    | ParameterType ParameterType
   deriving (Show,Eq)

data FunctionInfo = FunctionInfo
    { numberOfParams    :: Int
    , parameters        :: [Symbol]
    , returnTypes       :: [Type]
    , defined           :: Bool
    } deriving (Show, Eq)

data SymbolTable = SymbolTable
    { table         :: Dictionary
    , scopeStack    :: [Scope]
    , nextScope     :: Scope
    , nextSymAlias  :: SymAlias
    }


{- Utility -}

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


{- ST Filtering by scope -}

filterByScopeDictionary :: Dictionary -> Scope -> Dictionary
filterByScopeDictionary dict referenceScope = M.fromList $ filter (not . null . snd) defEntries
  where
    filterEntries = filter (\symInfo -> scope symInfo == referenceScope)
    defEntries = map (second filterEntries) $ M.toList dict

filterByScopeST :: SymbolTable -> Int -> Dictionary
filterByScopeST = filterByScopeDictionary . table


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
        newStack = newScope : scopeStack symT
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

lookupST :: Symbol -> MonadParser (Maybe SymbolInfo)
lookupST sym = do
    symT <- get
    let stack = scopeStack symT
        mBucket = findSymbol symT sym
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
findFunction params bucket = find (\e -> category e == Function 
                                      && numberOfParams (getFunctionMetaData e) == params) bucket

findFunctionDec :: Symbol -> Int -> MonadParser (Either SymbolInfo Bool)
findFunctionDec sym params = do
    symT <- get
    function <- lookupFunction sym params
    case function of 
        Nothing -> return $ Right False
        Just entry -> do
            if not $ defined $ getFunctionMetaData entry 
                then return $ Left entry
                else return $ Right True 

updateFunctionInfo :: SymbolInfo -> [Type] -> [Type] -> MonadParser SymbolInfo
updateFunctionInfo info params returns = do 
    symT <- get
    let oldMetadata = getFunctionMetaData info
        newAdditional = Just $ FunctionMetaData oldMetadata { 
            numberOfParams = length params,
            parameters = params,
            returnTypes = returns,
            defined = True
        }
    
    case params of 
        []            -> return $ info {symbolType = Nothing, additional = newAdditional}  
        [singleType]  -> return $ info {symbolType = Just singleType, additional = newAdditional}  
        _            -> do 
            name <- genTypeSymbol
            let typeInfo = TupleTypes returns
            insertType name typeInfo
            return $ info {symbolType = Just name, additional = newAdditional}

currentScope :: MonadParser Int
currentScope = do
    SymbolTable { scopeStack = (s:_)} <- get
    return s

getNextSymAlias :: MonadParser Int
getNextSymAlias = do
    symT <- get
    let sa = nextSymAlias symT
    put $ symT { nextSymAlias = succ sa }
    return sa

insertError :: Err.Error -> MonadParser ()
insertError msg = tell [msg]


{- Constants -}

pervasiveScope, globalScope :: Scope
pervasiveScope = 0
globalScope  = 1


{- Initial types -}


initialTypes :: [Symbol]
initialTypes = ["_int","_float","_char","_bool","_atom","_string", "_pointer"] --array, union, struct, tuple, alias

initialST :: SymbolTable
initialST = foldl' insertST st entries
  where
    entries = zip initialTypes (repeat typesSymbolInfo)
    st = SymbolTable {
        table      = M.empty :: M.Map Symbol [SymbolInfo],
        scopeStack = [0],
        nextScope  = 1,
        nextSymAlias = 0
    }

typesSymbolInfo :: SymbolInfo
typesSymbolInfo = SymbolInfo {
    category   = Type,
    scope      = pervasiveScope,
    symbolType = Nothing,
    additional = Nothing
}

int :: String
int = "_int"
float :: String
float = "_float"
char :: String
char = "_char"
bool :: String
bool = "_bool"
atom :: String
atom = "_atom"
string :: String
string = "_string"
pointer :: String 
pointer = "_ptr"
array :: String 
array = "_array"
tError :: String 
tError = "_type_error"


initializedST :: SymbolTable
initializedST = foldl insertST initialST entries 
    where entries = zip initialTypes (repeat typesSymbolInfo)


-- Most generic insertion function
regularEntry:: Symbol
            -> Scope
            -> Category
            -> Maybe Type
            -> Maybe AdditionalInfo
            -> Entry
regularEntry name sc ctg tp add = (name, symInfo)
  where
    symInfo = SymbolInfo {
        category = ctg,
        scope = sc,
        symbolType = tp,
        additional = add
    }

idEntry :: Symbol -> Scope -> Category -> Type -> Entry
idEntry name sc ctg tp = regularEntry name sc ctg (Just tp) Nothing

typeEntry :: Symbol -> AdditionalInfo -> Entry
typeEntry name info = regularEntry name pervasiveScope Type Nothing (Just info)

aliasEntry :: Symbol -> AliasType -> Type -> Entry
aliasEntry name aliasType pointedType = 
    regularEntry name globalScope Alias Nothing (Just $ AliasMetaData aliasType pointedType)

functionDecEntry :: Symbol -> Int -> Entry
functionDecEntry name params =
    regularEntry name globalScope Function Nothing additional
    where
        additional = Just $ FunctionMetaData (
            FunctionInfo {
                numberOfParams = params,
                parameters =  [],
                returnTypes = [],
                defined = False
            })

-- ^ To insert entries for id's of things that are not a type.
insertId :: Tk.Token -> Category -> Type -> MonadParser ()
insertId tk ctg tp = do
    sc <- currentScope

    let name  = Tk.cleanedString tk
        entry = idEntry name sc ctg tp

    symT  <- get
    mInfo <- lookupST name

    case mInfo of
        Nothing -> put $ insertST symT entry
        Just info -> if checkNotRepeated info sc
                        then put $ insertST symT entry
                        else let pos = Tk.position tk
                             in insertError $ Err.PE (Err.RedeclaredVar name pos)

-- ^ To insert entries on ST for id's of types
insertType :: Symbol -> AdditionalInfo -> MonadParser Type
insertType name additional = do
    symT <- get
    if not $ checkExisting symT name
        then do
            let entry = typeEntry name additional
            put $ insertST symT entry
            return name
    else return name

genTypeSymbol :: MonadParser Type
genTypeSymbol = do
    next <- getNextSymAlias
    return $ "a_" ++ show next

checkNotRepeated :: SymbolInfo -> Scope -> Bool
checkNotRepeated symInf sc
    | cond      = True
    | otherwise = False
    where cond = scope symInf /= sc && category symInf `notElem` [Function,Alias]