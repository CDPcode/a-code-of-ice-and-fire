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

type Offset = Int

type Width = Int

type Alignment = Int

data TypeInfo = TypeInfo
    { width :: Width
    , align :: Alignment
    } deriving (Show, Eq)

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
    , offset        :: Maybe Offset         -- Only for variables/constants
    , typeInfo      :: Maybe TypeInfo       -- Only for types
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
    , offsetStack   :: [Offset]
    , nextSymAlias  :: SymAlias
    }


{- Utility -}

getFunctionMetaData :: SymbolInfo -> FunctionInfo
getFunctionMetaData SymbolInfo { additional = Just (FunctionMetaData e) } = e
getFunctionMetaData _ = error "getFunctionMetaData: Unpropper use. Report use"

getAliasMetaData :: SymbolInfo -> (AliasType, Type)
getAliasMetaData SymbolInfo { additional = Just (AliasMetaData aType tp) } = (aType, tp)
getAliasMetaData _ = error "getFunctionMetaData: Unpropper use. Report use"


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

findSymbolInScope :: SymbolTable -> Symbol -> Scope -> Maybe SymbolInfo
findSymbolInScope st sym sc = wrap $ filterScope <$> findSymbol st sym
  where
    filterScope = filter (\info -> scope info == sc)
    wrap (Just [x]) = Just x
    wrap _  = Nothing


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
            let additionalInfo = TupleTypes returns
            tInfo <- getTupleTypeInfo returns
            insertType name additionalInfo tInfo
            return $ info {symbolType = Just name, additional = newAdditional}

currentScope :: MonadParser Int
currentScope = do
    SymbolTable { scopeStack = (s:_) } <- get
    return s

currentOffset :: MonadParser Int
currentOffset = do
    SymbolTable { offsetStack = (o:_) } <- get
    return o

setCurrentOffset :: Offset -> MonadParser ()
setCurrentOffset newOffset = do
    symT <- get
    let (_:rest) = offsetStack symT
        newOffsetStack = newOffset : rest

    put $ symT { offsetStack = newOffsetStack }

updateOffset :: TypeInfo -> MonadParser ()
updateOffset tInfo = do
    offs <- currentOffset
    let alignedOffset = getAlignedOffset offs (align tInfo)
    setCurrentOffset $ alignedOffset + width tInfo

getNextSymAlias :: MonadParser Int
getNextSymAlias = do
    symT <- get
    let sa = nextSymAlias symT
    put $ symT { nextSymAlias = succ sa }
    return sa

genTypeSymbol :: MonadParser Type
genTypeSymbol = do
    next <- getNextSymAlias
    return $ "a_" ++ show next

insertError :: Err.Error -> MonadParser ()
insertError msg = tell [msg]

{- Offsets, widths and alignments -}

getAlignedOffset :: Offset -> Alignment -> Offset
getAlignedOffset offs alignment = ceilDiv * alignment
  where
    ceilDiv = (offs + alignment - 1) `div` alignment

getTypeInfo :: Symbol -> MonadParser TypeInfo
getTypeInfo id = do
    mInfo <- lookupST id
    case mInfo of
        Nothing -> fail $ "Somehow type with id " ++ id ++ " has not been inserted in symbols table"
        Just info ->
            case category info of
                Alias -> getTypeInfo $ snd $ getAliasMetaData info
                Type ->
                    case typeInfo info of
                        Nothing -> fail $ "Somehow type with id " ++ id ++ " doesn't have width and alignment"
                        Just tInfo -> return tInfo
                _ -> fail $ "Wrong category for id " ++ id ++ " expected type or alias."

getTupleTypeInfo :: [Type] -> MonadParser TypeInfo
getTupleTypeInfo tps = do
    tInfos <- mapM getTypeInfo tps
    let aligns = map align tInfos
        a = foldr max 0 aligns
        w = foldl' sumTypeInfo 0 tInfos
    return $ TypeInfo {width = w, align = a}
  where
    sumTypeInfo acc info = getAlignedOffset acc (align info) + width info

getUnionTypeInfo :: [Type] -> MonadParser TypeInfo
getUnionTypeInfo tps = do
    tInfos <- mapM getTypeInfo tps
    let widths = map width tInfos
        aligns = map align tInfos
        a = foldr max 0 aligns
        w = foldl' max 0 widths
    return $ TypeInfo {width = w, align = a}


{- Constants -}

pervasiveScope, globalScope :: Scope
pervasiveScope = 0
globalScope  = 1

{- Initial types -}

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
tError :: String
tError = "_type_error"

initialTypes :: [Symbol]
initialTypes = [int, float, char, bool, atom] --array, union, struct, tuple, alias

initialTypesInfo :: [TypeInfo]
initialTypesInfo = zipWith TypeInfo initWidths initWidths
  where
    initWidths = [4, 8, 4, 1, 4]

initialST :: SymbolTable
initialST = foldl' insertST st entries
  where
    entries = zip initialTypes infos
    infos = map typesSymbolInfo initialTypesInfo
    st = SymbolTable
        { table           = M.empty
        , scopeStack      = [0,1]
        , nextScope       = 2
        , offsetStack     = [0]
        , nextSymAlias    = 0
        }

typesSymbolInfo :: TypeInfo -> SymbolInfo
typesSymbolInfo info = SymbolInfo
    { category   = Type
    , scope      = pervasiveScope
    , symbolType = Nothing
    , additional = Nothing
    , offset     = Nothing
    , typeInfo   = Just info
    }

{- Entries generation functions -}

regularEntry:: Symbol
            -> Scope
            -> Category
            -> Maybe Type
            -> Maybe AdditionalInfo
            -> Maybe Offset
            -> Maybe TypeInfo
            -> Entry
regularEntry name sc ctg tp add mOffset mInfo = (name, symInfo)
  where
    symInfo = SymbolInfo
        { category      = ctg
        , scope         = sc
        , symbolType    = tp
        , additional    = add
        , offset        = mOffset
        , typeInfo      = mInfo
        }

idEntry :: Symbol -> Scope -> Category -> Type -> Offset -> Entry
idEntry name sc ctg tp offs = regularEntry name sc ctg (Just tp) Nothing (Just offs) Nothing

paramEntry :: Symbol -> Scope -> Type -> AdditionalInfo -> Entry
paramEntry name sc tp info = regularEntry name sc Parameter (Just tp) (Just info) Nothing Nothing

typeEntry :: Symbol -> AdditionalInfo -> TypeInfo -> Entry
typeEntry name info tInfo = regularEntry name globalScope Type Nothing (Just info) Nothing (Just tInfo)

aliasEntry :: Symbol -> AliasType -> Type -> Entry
aliasEntry name aliasType pointedType =
    regularEntry name globalScope Alias Nothing (Just $ AliasMetaData aliasType pointedType) Nothing Nothing

functionDecEntry :: Symbol -> Int -> Entry
functionDecEntry name params =
    regularEntry name globalScope Function Nothing info Nothing Nothing
    where
        info = Just $ FunctionMetaData (
            FunctionInfo {
                numberOfParams = params,
                parameters =  [],
                returnTypes = [],
                defined = False
            })

{- Insertion Function -}

insertId :: Tk.Token -> Category -> Type -> MonadParser ()
insertId tk ctg tp = do
    sc <- currentScope
    currOffset <- currentOffset
    tInfo <- getTypeInfo tp

    let offs = getAlignedOffset currOffset (align tInfo)
        name  = Tk.cleanedString tk
        entry = idEntry name sc ctg tp offs
        newOffset = offs + width tInfo

    symT  <- get
    mInfo <- lookupST name

    case mInfo of
        Nothing -> put $ insertST symT entry
        Just info ->
            if checkNotRepeated info sc
            then do
                put $ insertST symT entry
                setCurrentOffset newOffset
            else insertError $ Err.PE (Err.RedeclaredVar name $ Tk.position tk)

insertParam :: Tk.Token -> Type -> ParameterType -> MonadParser ()
insertParam tk tp paramType = do
    
    sc <- currentScope
    let name  = Tk.cleanedString tk
        entry = paramEntry name sc tp $ ParameterType paramType
    
    symT  <- get
    mInfo <- lookupST name
    case mInfo of
        Nothing -> put $ insertST symT entry
        Just info ->
            if checkNotRepeated info sc
            then do
                put $ insertST symT entry
            else insertError $ Err.PE (Err.RedeclaredParameter name $ Tk.position tk)

insertType :: Symbol -> AdditionalInfo -> TypeInfo -> MonadParser Type
insertType name add tInfo = do
    symT <- get
    if not $ checkExisting symT name
        then do
            let entry = typeEntry name add tInfo
            put $ insertST symT entry
            return name
    else return name

insertAlias :: Tk.Token -> Type -> AliasType -> Type -> MonadParser ()
insertAlias tk name aliasType tp = do
    symT <- get
    if checkExisting symT name
    then insertError $ Err.PE (Err.RepeatedAliasName name (Tk.position tk))
    else do
        case findSymbolInScope symT tp globalScope of
            Nothing -> insertError $ Err.PE (Err.UndefinedType name (Tk.position tk))
            Just info -> do
                let realType = case category info of
                        Alias ->
                            case getAliasMetaData info of
                                (ByStructure, rType) -> rType
                                _ -> tp
                        _ -> tp
                let entry = aliasEntry name aliasType realType
                put $ insertST symT entry
                
checkNotRepeated :: SymbolInfo -> Scope -> Bool
checkNotRepeated symInf sc = scope symInf /= sc && category symInf `notElem` [Function, Alias]
