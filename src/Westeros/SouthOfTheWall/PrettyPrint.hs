{-# LANGUAGE OverloadedStrings #-}

module Westeros.SouthOfTheWall.PrettyPrint where

import Data.Function ((&))
import Data.List (intercalate, intersperse)
import Data.Text (pack)
import Rainbow
    ( fore,
      blue,
      green,
      red,
      chunksToByteStrings,
      toByteStringsColors256,
      chunk,
      Chunk )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M (toList) 

import Westeros.SouthOfTheWall.Symtable ( SymbolTable(dict, scopeStack, nextScope), SymbolInfo(category, scope, additional) )
import Westeros.SouthOfTheWall.Tokens (Token(..), Position(..))
import Westeros.SouthOfTheWall.Error (ParserError(..))



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

instance Pretty Token where
    pretty = BS.putStrLn . chunksToLazyBS tokenChunks 

tokenChunks :: Token -> [Chunk]
tokenChunks token = [ chunk "-Token "
                    , chunkFromStr (show $ aToken token) & fore green
                    , chunkFromStr $ "\n\tContents " 
                        ++ show (cleanedString  token) 
                        ++ "\n\tAt row: "
                    , chunkFromStr (show $ row $ position token) & fore red
                    , chunk " column: "
                    , chunkFromStr (show $ col $ position token) & fore red
                    ]


{- Pretty printing for ST -}

instance Pretty SymbolTable where
    pretty = BS.putStrLn . chunksToLazyBS symTableChunk 


symTableChunk :: SymbolTable -> [Chunk]
symTableChunk symT = chunk "* Name info\n" 
                     : concatMap foo asList 
                     ++ [ chunkFromStr ("\n* Scope stack :" ++ show (scopeStack symT)
                          ++ "\n* Next Scope " ++ show (nextScope symT) )
                        ]
    where 
        asList = M.toList (dict symT)
        foo (a,b) = (chunkFromStr ('\n':a) & fore green) : preProcess b :: [Chunk]
            where  -- OJO: possible bottleneck
                preProcess = intercalate [chunkFromStr bar] . map symbolInfoChunk 
                bar = "\n\t-------------------"


symbolInfoChunk :: SymbolInfo -> [Chunk]
symbolInfoChunk si = [ chunk "\n\tCategory: " 
                     , chunkFromStr (show (category si)) & fore blue
                     , chunkFromStr $ "\n\tScope: " ++ show (scope si)
                     ]


{- Pretty printing for errors -}

instance Pretty ParserError where
    -- ^ PreParser related
    pretty (FRepeatedDeclarations fName) = undefined
    -- ^ "A function with the same name \""++name++"\" and # of arguments was already declared"
    pretty (InvalidNArgsDef fName nArgs) = undefined
    -- ^ ("No function \"" ++ functionId ++ "\" with "++ show (length $2) ++ " arguments was declared") -- InvalidNArgsDef _ _
    pretty (FDefinitionWithoutDeclaration fName) = undefined
    -- ^ "Function "++functionId++" defined, but not declared"
    pretty (RepeatedAliasName aName) = undefined
    -- ^ "The name \""++name++"\" is an existing symbol"
    pretty (FRepeatedDefinitions fName) = undefined
    -- ^ ST.insertError ("Function \"" ++ functionId ++ "\" was already defined") -- FRepeatedDefinitions _

    
    pretty (UndefinedFunction name parameters) = undefined
    -- ^ "error: undefined function " ++ name ++ "with " ++ (show params) ++ " parameters."
    pretty (RedeclaredParemeter parName position) = undefined
    -- ^Error: redeclared parameter " ++ name ++ " at position " ++ show (Tk.position $2)
    pretty (RedeclaredName name position) = undefined
    -- "Error: redeclared name " ++ name ++ " at position " ++ show (Tk.position $2)
    pretty (UndefinedVar name position) = undefined
    -- "Error: undefined variable " ++ name ++ " at postition " ++ show pos
    pretty (InvalidVar category unexpectedSym position) = undefined
    -- "Error: expected variable, found " ++ show c ++ " " ++ name ++ " at position " ++ show pos
    pretty (RedeclaredVar name position) = undefined
    -- "Error: redeclared variable " ++ name ++ " at position " ++ show pos
    pretty (RedeclaredConstant name position) = undefined
    -- "Error: redeclared constant " ++ name ++ " at position " ++ show pos
    pretty (ExpectedFunction category name position) = undefined
    -- "Error: expected function, found " ++ show c ++ " " ++ name ++ " at position " ++ show pos
    -- ^ Parser related

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