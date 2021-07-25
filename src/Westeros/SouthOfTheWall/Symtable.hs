module Westeros.SouthOfTheWall.Symtable where

import qualified Westeros.SouthOfTheWall.Error as Err

import qualified Westeros.SouthOfTheWall.Tokens as Tk
import qualified Data.Map.Strict as M

import Control.Monad.RWS ( MonadState(put, get), MonadWriter(tell), RWST, when )
import Data.List (intercalate, find)
import Data.Maybe (fromJust, isJust)

type Type = String

type Symbol = String

type Scope = Int

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
    | DopeVector Type Int 
    | PointedType String
    | NestedScope Int
    | TupleTypes [Type]

    | FunctionMetaData FunctionInfo              
    | ParameterType ParameterType                  
   deriving (Show,Eq)

data FunctionInfo = FunctionInfo
    { numberOfParams :: Int
    , parameters :: [Symbol]
    , returnTypes :: [Type]
    , defined :: Bool
    } deriving (Show,Eq)

data SymbolTable = SymbolTable
    { table :: Dictionary
    , scopeStack :: [Int]
    , nextScope :: Int    
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
findFunction params bucket = find (\e -> numberOfParams (getFunctionMetaData e) == params) bucket

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
        table      = M.empty :: M.Map Symbol [SymbolInfo],
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


initialTypes :: [String]
initialTypes = ["_int","_float","_char","_bool","_atom","_string", "_pointer"] --array, union, struct, tuple, alias

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

-- ^ Insertion function for symbols with simple types.
idEntry :: Symbol -> Scope -> Category -> Type -> Entry
idEntry name sc ctg tp = regularEntry name sc ctg (Just tp) Nothing 

typeEntry :: Symbol -> Scope -> Category -> AdditionalInfo -> Entry
typeEntry name sc ctg add = regularEntry name sc ctg Nothing (Just add)


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
-- ^ Special cases:
-- ^    + Array
-- ^    + Structs
-- ^    + Unions
-- ^    + Tuples (?)
-- ^    + Aliases (?)
insertType :: Symbol -> Bool -> AdditionalInfo -> MonadParser Type
insertType name isNested additional = do
    symT <- get

    let entry = typeEntry name pervasiveScope Type additional

    if not $ checkExisting symT name 
        then do
            put $ insertST symT entry 
            return name
        else error "TODO"


-- ^ This is for inserting structs/unions
insertNestedType :: Scope -> Bool -> MonadParser Type
insertNestedType sc isStruct = do

    let definedScope = NestedScope (succ sc)
        tpName        
            | isStruct  = getAnonymousType sc "_struct_"
            | otherwise = getAnonymousType sc "_union_"
        tpEntry = typeEntry tpName pervasiveScope Type definedScope

    symT  <- get
    put $ insertST symT tpEntry

    return tpName

getAnonymousType sc preName = preName ++ show (succ sc)

getArrayType tp dim = array ++ '_' : tp ++ '_' : show dim


checkNotRepeated :: SymbolInfo -> Scope -> Bool
checkNotRepeated symInf sc 
    | cond      = True
    | otherwise = False
    where cond = scope symInf /= sc && category symInf `notElem` [Function,Alias]


{-
struct { 
    int a;
    int b; 
    struct { 
        bool a;
        bool b;
    } my_struct_2; 
} my_struct_1;


|   "_struct0" -> category = Type,
|                 scope = a,
|                 type = Nothing,
|                 additional = Just (NestedScope $ succ a)
|
|   "my_struct_1" -> category = Var,
|                    scope = a,
|                    type = "_struct0",
|                    additional = Nothing
|   
|   "a" -> category = Var,        
|          scope = succ a,
|          type = "_int",
|          additional = Nothing
|   
|   "b" -> category = Var,        
|          scope = succ a,
|          type = "_int",
|          additional = Nothing
| 
|
| | "_struct1" -> category = Type,
| |               scope = succ a ,
| |               type = Nothing,
| |               additional = Just (NestedScope $ succ $ succ a)
| |
| | "my_struct_2" -> category = Var,
| |                  scope = succ a,
| |                  type = "_struct1",
| |                  additional = Nothing
| | 
| | "a" -> category = Var,        
| |        scope = succ $ succ a,
| |        type = "_bool",
| |        additional = Nothing
| | 
| | "b" -> category = Var,        
| |        scope = succ $ succ a,
| |        type = "_bool",
| |        additional = Nothing

nested types saving:

create an entry in ST for the indexed type (["_struct","_union"]"$i"), with NestedScope = succ currentScope
create an entry in ST with name, scope (=currentScope), saved nested type and additionalInfo if applies

for each of the ("name","type") pairs:
    if type is Struct -> call nested types saving.
    else -> save it as usual, with adjusted scope

-}