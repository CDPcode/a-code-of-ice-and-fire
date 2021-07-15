{-# LANGUAGE OverloadedStrings #-}

module Westeros.SouthOfTheWall.PrettyPrint where

import Data.Function ((&))
import Data.List (intercalate, intersperse)
import Data.Text (pack)
import Rainbow
    ( fore,
      blue,
      brightBlue,
      brightRed,
      green,
      red,
      chunksToByteStrings,
      toByteStringsColors256,
      chunk,
      Chunk )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M (toList) 

import Westeros.SouthOfTheWall.Symtable ( SymbolTable(table, scopeStack, nextScope), SymbolInfo(category, scope, additional) )
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
symTableChunk symT = chunk "* Name info\n" 
                     : concatMap foo asList 
                     ++ [ chunkFromStr ("\n* Scope stack :" ++ show (scopeStack symT)
                          ++ "\n* Next Scope " ++ show (nextScope symT) )
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
                     ]

{- Pretty printing for generic errors -}

instance Pretty Error where
    pretty (PE parserError) = pretty parserError
    pretty (TE typeError)   = pretty typeError
    

{- Pretty printing for parse errors -}

instance Pretty ParserError where
    pretty pe =  BS.putStrLn . chunksToLazyBS 
                    ( (parseErrorHead :) . errorChunks ) $ pe


errorChunks :: ParserError -> [Chunk]
errorChunks (FRepeatedDeclarations fName pos) = 
    [ chunk "Redeclared function "
    , chunkFromStr fName & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
errorChunks (FRepeatedDefinitions fName pos) = 
    [ chunk "Redefined function "
    , chunkFromStr fName 
    , chunk "."
    ] ++ positionChunks pos
errorChunks (InvalidNArgsDef fName nArgs pos) = 
    [ chunk "Undeclared function "
    , chunkFromStr fName & fore brightBlue
    , chunkFromStr (" with " ++ show nArgs ++ " parameters.") 
    ] ++ positionChunks pos
errorChunks (FDefinitionWithoutDeclaration fName pos) = 
    [ chunk "Function "
    , chunkFromStr fName & fore brightBlue 
    , chunk " defined, but not declared."
    ] ++ positionChunks pos
errorChunks (RepeatedAliasName aName pos) =
    [ chunk "Redeclared symbol "
    , chunkFromStr aName & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos

errorChunks (UndefinedFunction fName pos) = 
    [ chunk "Undefined function "
    , chunkFromStr fName & fore brightBlue 
    , chunk "."
    ]  ++ positionChunks pos
errorChunks (RedeclaredParameter parName pos) = 
    [ chunk "Redeclared parameter "
    , chunkFromStr parName & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
errorChunks (RedeclaredName name pos) = 
    [ chunk "Redeclared name: "
    , chunkFromStr name & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
errorChunks (UndefinedVar name pos) = 
    [ chunk "Undefined variable: "
    , chunkFromStr name & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
errorChunks (InvalidVar category unexpectedSym pos) = 
    [ chunkFromStr ("Expected variable, found " ++ show category ++ " ") 
    , chunkFromStr unexpectedSym & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
errorChunks (RedeclaredVar name pos) = 
    [ chunk "Redeclared variable: "
    , chunkFromStr name & fore brightBlue 
    , chunk "."
    ] ++ positionChunks pos
errorChunks (RedeclaredConstant name pos) = 
    [ chunk "Redeclared constant: "
    , chunkFromStr name & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
errorChunks (ExpectedFunction category name pos) = 
    [ chunkFromStr ("Expected function, foound " ++ category ++ " ")
    , chunkFromStr name & fore brightBlue 
    , chunk "."
    ]
errorChunks (SyntaxErr tk) =
    [ chunk "Syntax error related to: "
    , chunkFromStr (show (Tk.aToken tk)) & fore brightBlue 
    ] ++ positionChunks (Tk.position tk)
errorChunks SyntaxErrEOF = 
    [ chunk "Syntax error at End Of File"]

parseErrorHead :: Chunk
parseErrorHead = chunk "Parser Error: " & fore red

positionChunks :: Tk.Position -> [Chunk]
positionChunks Tk.Position{ Tk.row = r , Tk.col = c } =
    [ chunk "\n"
    , chunk "--> " & fore brightBlue
    , chunkFromStr (show r) & fore brightRed 
    , chunk ":"
    , chunkFromStr (show c) & fore brightRed 
    ]
                                        

{- Pretty printing for type errors -}

instance Pretty TypeError where
    pretty te = undefined

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