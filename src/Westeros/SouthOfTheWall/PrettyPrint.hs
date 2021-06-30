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

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M (toList) 

import Westeros.SouthOfTheWall.Symtable ( SymbolTable(dict, scopeStack, nextScope), SymbolInfo(category, scope, additional) )
import Westeros.SouthOfTheWall.Tokens (Token(..), Position(..))


chunkFromStr :: String -> Chunk 
chunkFromStr = chunk . pack

{- Pretty printing for Tokens -}

prettyToken :: Token -> IO ()
prettyToken tk = BS.putStrLn (BS.concat tkBs)
    where tkBs = chunksToByteStrings toByteStringsColors256 (tokenChunks tk)

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

prettyST :: SymbolTable -> IO ()
prettyST symT = BS.putStrLn (BS.concat tkBs)
    where tkBs = chunksToByteStrings toByteStringsColors256 (symTableChunk symT)

symTableChunk :: SymbolTable -> [Chunk]
symTableChunk symT = chunk "* Name info\n" 
                     : concatMap foo asList 
                     ++ [ chunkFromStr ("\n* Scope stack :" ++ show (scopeStack symT)
                          ++ "\n* Next Scope " ++ show (nextScope symT) )
                        ]
    where 
        asList = M.toList (dict symT)
        foo (a,b) = (chunkFromStr ('\n':a) & fore green) : preProcess b :: [Chunk]
            where  -- OJO
                preProcess = intercalate [chunkFromStr bar] . map symbolInfoChunk 
                bar = "\n\t-------------------"


symbolInfoChunk :: SymbolInfo -> [Chunk]
symbolInfoChunk si = [ chunk "\n\tCategory: " 
                     , chunkFromStr (show (category si)) & fore blue
                     , chunkFromStr $ "\n\tScope: " ++ show (scope si)
                     ]

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