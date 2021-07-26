{-# LANGUAGE OverloadedStrings #-}

module Westeros.SouthOfTheWall.PrettyPrint where

import Data.Function ((&))
import Data.List (intercalate, intersperse)
import Data.Text (pack)
import Rainbow
    ( fore,
      blue,
      brightCyan,
      brightBlue,
      brightRed,
      green,
      red,
      chunksToByteStrings,
      toByteStringsColors256,
      chunk,
      Chunk)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M (toList)

import Westeros.SouthOfTheWall.Symtable ( SymbolTable(..), SymbolInfo(..) )
import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token(..), Position(..))
import Westeros.SouthOfTheWall.Error



-- ^ Interface for pretty things
class Pretty a where
    pretty :: a -> IO ()

-- ^ Get a lazy bytestring from a chunk generator
--      Chunk generator == output of colored formatted text
-- OJO: possible bottleneck
chunksToLazyBS :: (a -> [Chunk]) -> a -> BS.ByteString
chunksToLazyBS chunker source = printable
    where
        printable = BS.concat $ map BS.fromStrict strict
        strict    = chunksToByteStrings toByteStringsColors256 (chunker source)

-- ^ Eases producing chunks from things other than text
chunkFromStr :: String -> Chunk
chunkFromStr = chunk . pack


{- Pretty printing for Tokens -}

instance Pretty Tk.Token where
    pretty = BS.putStrLn . chunksToLazyBS tokenChunks

tokenChunks :: Tk.Token -> [Chunk]
tokenChunks token = [ chunk "-Token "
                    , chunkFromStr (show $ Tk.aToken token) & fore green
                    , chunkFromStr $ "\n\tContents " ++ show (Tk.cleanedString  token)
                    ] ++ positionChunks (Tk.position token)


{- Pretty printing for ST -}

instance Pretty SymbolTable where
    pretty = BS.putStrLn . chunksToLazyBS symTableChunk


symTableChunk :: SymbolTable -> [Chunk]
symTableChunk symT = 
    chunk "* Name info\n" : concatMap foo asList
    ++ [ chunkFromStr $ "\n* Scope stack :" ++ show (scopeStack symT)
       , chunkFromStr $ "\n* Next Scope " ++ show (nextScope symT) 
       , chunkFromStr $ "\n* Offset Stack " ++ show (offsetStack symT)
       , chunkFromStr $ "\n* Next symbol alias " ++ show (nextSymAlias symT)
       ]
    where
        asList = M.toList (table symT)
        foo (a,b) = (chunkFromStr ('\n':a) & fore green) : preProcess b :: [Chunk]
            where  -- OJO: possible bottleneck
                preProcess = intercalate [chunkFromStr bar] . map symbolInfoChunk
                bar = "\n\t-------------------"


symbolInfoChunk :: SymbolInfo -> [Chunk]
symbolInfoChunk si = [ chunk "\n\tCategory: "
                     , chunkFromStr (show (category si)) & fore blue
                     , chunkFromStr $ "\n\tScope: " ++ show (scope si)
                     ] ++ tp (symbolType si)  
                     ++ addnl (additional si)
                     ++ offst (offset si)
                     ++ tInfo (typeInfo si)
    where
        tp (Just t) = [ chunkFromStr $ "\n\t Type: " ++ show t ]   
        tp Nothing  = [] 

        addnl (Just t) = [ chunkFromStr $ "\n\t Additional: " ++ show t ]
        addnl Nothing  = []

        offst (Just t) = [ chunkFromStr $ "\n\t Offset" ++ show t ]
        offst Nothing  = []

        tInfo (Just t) = [ chunkFromStr $ "\n\t Type info: " ++ show t]
        tInfo Nothing  = [] 


{- Pretty printing for generic errors -}

instance Pretty Error where
    pretty (PE parserError) = pretty parserError
    pretty (TE typeError)   = pretty typeError


{- Pretty printing for parse errors -}

instance Pretty ParserError where
    pretty = BS.putStrLn . chunksToLazyBS
                    ( (parseErrorHead :) . parseErrorChunks )


parseErrorChunks :: ParserError -> [Chunk]
parseErrorChunks (FRepeatedDeclarations fName pos) =
    [ chunk "Redeclared function "
    , chunkFromStr fName & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (FRepeatedDefinitions fName pos) =
    [ chunk "Redefined function "
    , chunkFromStr fName
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (InvalidNArgsDef fName nArgs pos) =
    [ chunk "Undeclared function "
    , chunkFromStr fName & fore brightBlue
    , chunkFromStr (" with " ++ show nArgs ++ " parameters.")
    ] ++ positionChunks pos
parseErrorChunks (FDefinitionWithoutDeclaration fName pos) =
    [ chunk "Function "
    , chunkFromStr fName & fore brightBlue
    , chunk " defined, but not declared."
    ] ++ positionChunks pos
parseErrorChunks (RepeatedAliasName aName pos) =
    [ chunk "Redeclared symbol "
    , chunkFromStr aName & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (UndefinedType name pos) = 
    [ chunk "Undefined type "
    , chunkFromStr name & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos

parseErrorChunks (UndefinedFunction fName pos) =
    [ chunk "Undefined function "
    , chunkFromStr fName & fore brightBlue
    , chunk "."
    ]  ++ positionChunks pos
parseErrorChunks (RedeclaredParameter parName pos) =
    [ chunk "Redeclared parameter "
    , chunkFromStr parName & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (RedeclaredName name pos) =
    [ chunk "Redeclared name: "
    , chunkFromStr name & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (UndefinedVar name pos) =
    [ chunk "Undefined variable: "
    , chunkFromStr name & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (InvalidVar cat unexpectedSym pos) =
    [ chunkFromStr ("Expected variable, found " ++ show cat ++ " ")
    , chunkFromStr unexpectedSym & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (RedeclaredVar name pos) =
    [ chunk "Redeclared variable: "
    , chunkFromStr name & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (RedeclaredConstant name pos) =
    [ chunk "Redeclared constant: "
    , chunkFromStr name & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (ExpectedFunction cat name pos) =
    [ chunkFromStr ("Expected function, foound " ++ cat ++ " ")
    , chunkFromStr name & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
parseErrorChunks (NonCallableExpression pos) = 
    chunk "Non callable expression found" : positionChunks pos 
parseErrorChunks (SyntaxErr tk) =
    [ chunk "Syntax error related to: "
    , chunkFromStr (show (Tk.aToken tk)) & fore brightBlue
    ] ++ positionChunks (Tk.position tk)
parseErrorChunks SyntaxErrEOF =
    [ chunk "Syntax error at End Of File"]

parseErrorHead :: Chunk
parseErrorHead = chunk "Parser Error: " & fore red



{- Pretty printing for type errors -}

instance Pretty TypeError where
    pretty = BS.putStrLn . chunksToLazyBS ( (typeErrorHead :) . typeErrorChunks)


typeErrorChunks :: TypeError -> [Chunk]
typeErrorChunks (HeterogeneusArrayType pos) = 
    chunk "Found heterogeneus array." : positionChunks pos
typeErrorChunks (InvalidIndexType errorTp exprString pos) = 
    [ chunk "Invalid Index type "
    , chunkFromStr (truncateType errorTp) & fore brightBlue 
    , chunk " for expression " 
    , chunkFromStr exprString & fore brightCyan
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InconsistentTypesBinOp op (a,b) actualTypes pos) = 
    [ chunk "Inconsistent types for binary operation "
    , chunkFromStr op & fore brightCyan, chunk " .Found " 
    , chunkFromStr (truncateType a) & fore brightBlue, chunk " and " 
    , chunkFromStr (truncateType b) & fore brightCyan
    , chunk " but both types must be one of " 
    , chunkFromStr (unwords actualTypes) 
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidTypesBinOp op (a,b) actualTypes pos) = 
    [ chunk "Invalid type for binary operation "
    , chunkFromStr op & fore brightCyan, chunk " .Found "
    , chunkFromStr (truncateType a) & fore brightBlue, chunk " and "
    , chunkFromStr (truncateType b) & fore brightBlue
    , chunk " but expected one of " 
    , chunkFromStr (unwords actualTypes)
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidTypeUnOp op errType actualTypes pos) = 
    [ chunk "Invalid type for unary operation " 
    , chunkFromStr op & fore brightCyan, chunk " .Found "
    , chunkFromStr (truncateType errType) & fore brightBlue
    , chunk " but expected one of ", chunkFromStr (unwords actualTypes)
    , chunk "." 
    ] ++ positionChunks pos
typeErrorChunks (InvalidDereference tp pos) = 
    [ chunk " Expected a pointer but found "
    , chunkFromStr (truncateType tp) & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (RecordFieldNotFound name sc pos) = 
    [ chunk "Not a field "
    , chunkFromStr name & fore brightBlue
    , chunk " defined whithin scope "
    , chunkFromStr (show sc) & fore brightRed 
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (RepeatedRecordField name sc pos) = 
    [ chunk " Existing record field "
    , chunkFromStr name & fore brightBlue 
    , chunk " defined whithin scope "
    , chunkFromStr (show sc)
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (UnTypedRecordField name pos) = 
    [ chunk " Record field "
    , chunkFromStr name & fore brightBlue 
    , chunk " has no type."
    ] ++ positionChunks pos
typeErrorChunks (NotARecordType typeName pos) = 
    [ chunkFromStr typeName & fore brightBlue 
    , chunk " is not a record type."
    ] ++ positionChunks pos
typeErrorChunks (NotAnUnion typeName pos) = 
    [ chunkFromStr typeName & fore brightBlue 
    , chunk " is not an Union type."
    ] ++ positionChunks pos
typeErrorChunks (NotATupleType err pos) = 
    [ chunkFromStr (truncateType err) & fore brightBlue 
    , chunk " is not a propper tuple type."
    ] ++ positionChunks pos
typeErrorChunks (IdNotFound name) = 
    [ chunk " Id "
    , chunkFromStr name & fore brightBlue
    , chunk " not found."
    ] 
typeErrorChunks (UnTypedId name) = 
    [ chunk " Id ", chunkFromStr name & fore brightBlue 
    , chunk " has no type."
    ]
typeErrorChunks (NotAFunction name) = 
    [ chunkFromStr name & fore brightBlue 
    , chunk " is not a function."
    ]
typeErrorChunks (FunctionWithoutMD name) = 
    [ chunk " Function "
    , chunkFromStr name & fore brightBlue 
    , chunk " has no metadata."
    ]
typeErrorChunks (NonCasteableTypes source dest pos) = 
    [ chunk "Cannot cast "
    , chunkFromStr (truncateType source) & fore brightBlue 
    , chunk " and "
    , chunkFromStr (truncateType dest) & fore brightBlue
    , chunk "." 
    ] ++ positionChunks pos
typeErrorChunks (InvalidWhileType typeName pos) = 
    [ chunk "Found "
    , chunkFromStr (truncateType typeName) & fore brightBlue 
    , chunk " but expected Int for while expression." 
    ] ++ positionChunks pos
typeErrorChunks (WrongForBoundType lb ub pos) = 
    [ chunk "Expected Int as bounds but found "
    , chunkFromStr (truncateType lb) & fore brightBlue
    , chunk " and " 
    , chunkFromStr (truncateType ub) & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (WrongSwitchType exprType pos) = 
    [ chunk "Switch expression must be Atom but found "
    , chunkFromStr (truncateType exprType) 
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidIfType exprType pos) = 
    [ chunk "If must be Bool but found "
    , chunkFromStr (truncateType exprType)
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidNew ptrType pos) = 
    [ chunk "Expected pointer type but found "
    , chunkFromStr (truncateType ptrType) & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidFree ptrType pos) = 
    [ chunk "Expected pointer type but found "
    , chunkFromStr (truncateType ptrType) & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (NonReadable tp pos) = 
    [ chunkFromStr (truncateType tp) & fore brightBlue 
    , chunk " is not a readable type."
    ] ++ positionChunks pos
typeErrorChunks (NonPrintable tp pos) = 
    [ chunkFromStr (truncateType tp) & fore brightBlue 
    , chunk " is not a printable type."
    ] ++ positionChunks pos
typeErrorHead  :: Chunk 
typeErrorHead = chunk "Type Error: " & fore red

positionChunks :: Tk.Position -> [Chunk]
positionChunks Tk.Position{ Tk.row = r , Tk.col = c } =
    [ chunk "\n"
    , chunk "--> " & fore brightBlue
    , chunkFromStr (show r) & fore brightRed
    , chunk ":"
    , chunkFromStr (show c) & fore brightRed
    ]

truncateType :: String -> String 
truncateType tp 
    | length tp < 10 = tp 
    | otherwise      = pfx ++ trail
    where 
        pfx = take sz tp
        sz  = 10 
        trail = "..."


{-
:set -XOverloadedStrings

import Data.Function ((&))

Rainbow
putChunkLn $ "Some blue text" & fore blue
color256 :: Word8 -> Radiant

import Data.Text
pack :: String -> Text

import qualified Data.ByteString as BS

toByteStringsColors0 :: Chunk -> [ByteString] -> [ByteString]

toByteStringsColors8 :: Chunk -> [ByteString] -> [ByteString]

toByteStringsColors256 :: Chunk -> [ByteString] -> [ByteString]

EXAMPLE

myChunks :: [Chunk String]
myChunks = [ chunk "Roses" & fore red, chunk "\n",
             chunk "Violets" & fore blue, chunk "\n" ]

myPrintedChunks :: IO ()
myPrintedChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256 $ myChunks

-}
