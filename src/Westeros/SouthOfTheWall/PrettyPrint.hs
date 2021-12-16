{-# LANGUAGE OverloadedStrings #-}

module Westeros.SouthOfTheWall.PrettyPrint (
      Pretty (..)
    ) where

import Data.List(intercalate)
import Data.Function ((&))
import Data.Text (pack)
import Rainbow
    ( fore
    , blue
    , brightCyan
    , brightBlue
    , brightRed
    , green
    , red
    , chunksToByteStrings
    , toByteStringsColors256
    , chunk
    , Chunk)

import Westeros.SouthOfTheWall.Error
    ( Error(..)
    , ParserError(..)
    , TypeError(..) )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M (toList)

import Westeros.SouthOfTheWall.Symtable ( SymbolTable(..), SymbolInfo(..), TypeInfo(..) )
import qualified Westeros.SouthOfTheWall.Tokens as Tk (Token(..), Position(..))



-- ^ Interface for pretty things
class Pretty a where
    pretty :: a -> IO ()

-- ^ Get a lazy bytestring from a chunk generator
--      Chunk generator == output of colored formatted text
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

        offst (Just t) = [ chunkFromStr $ "\n\t Offset: " ++ show t ]
        offst Nothing  = []

        tInfo (Just t) = [ chunkFromStr $ "\n\t Width: " ++ show (width t)
                         , chunkFromStr $ "\n\t Alignment: " ++ show (align t)]

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
parseErrorChunks (RedeclareFunction fName args pos) =
    [ chunk "Chapter name "
    , chunkFromStr (fName ++ " " ++ show args) & fore brightBlue
    , chunk " redeclared."
    ] ++ positionChunks pos
parseErrorChunks (RedefineFunction fName args pos) =
    [ chunk "Chapter "
    , chunkFromStr (fName ++ " " ++ show args) & fore brightBlue
    , chunk " rewritten."
    ] ++ positionChunks pos
parseErrorChunks (UndeclaredFunction fName nArgs pos) =
    [ chunk "Chapter "
    , chunkFromStr (fName ++ " " ++ show nArgs) & fore brightBlue
    , chunk " not announced."
    ] ++ positionChunks pos
parseErrorChunks (UndefinedFunction fName nArgs pos) =
    [ chunk "Chapter "
    , chunkFromStr (fName ++ " " ++ show nArgs) & fore brightBlue
    , chunk " announced but not written."
    ] ++ positionChunks pos
parseErrorChunks (RedeclaredName name pos) =
    [ chunk "Name "
    , chunkFromStr name & fore brightBlue
    , chunk " is already taken."
    ] ++ positionChunks pos
parseErrorChunks (UndeclaredName name pos) =
    [ chunk "Name "
    , chunkFromStr name & fore brightBlue
    , chunk " has not been introducted before."
    ] ++ positionChunks pos
parseErrorChunks (NonCallableExpression pos) =
    chunk "You are trying to refer to an article that hasn't been written." : positionChunks pos
parseErrorChunks (NoLoop pos) =
    chunk "Forbidden statement at this point of the story." : positionChunks pos
parseErrorChunks (MultiAssignmentLengthMissmatch nArgs nVars pos) =
    [ chunk "In your story, "
    , chunkFromStr (show nArgs) & fore brightBlue
    , chunk " characters take "
    , chunkFromStr (show nVars) & fore brightBlue
    , chunk " fights."
    ] ++ positionChunks pos
parseErrorChunks (ReturnLengthMissmatch str nArgs nRet nVars pos) =
    [ chunk "In your story, chapter "
    , chunkFromStr str & fore brightBlue
    , chunk " "
    , chunkFromStr (show nArgs) & fore brightBlue
    , chunk " is expected to end with "
    , chunkFromStr (show nRet) & fore brightBlue
    , chunk " characters coming, but you only provided "
    , chunkFromStr (show nVars) & fore brightBlue
    , chunk " ."
    ] ++ positionChunks pos
parseErrorChunks (IndexOutOfBounds nArgs nVars pos) =
    [ chunk "You are trying to tell the story of the soldier "
    , chunkFromStr (show nArgs) & fore brightBlue
    , chunk " of an army with "
    , chunkFromStr (show nVars) & fore brightBlue
    , chunk " soldiers"
    ] ++ positionChunks pos
parseErrorChunks (SyntaxErr str pos) =
    [ chunk "The scriptures do not follow the desired structure in the word "
    , chunkFromStr str & fore brightBlue
    ] ++ positionChunks pos
parseErrorChunks SyntaxErrEOF =
    [ chunk "The scriptures do not follow the desired structure before ending"]

parseErrorHead :: Chunk
parseErrorHead = chunk "Your scriptures contain some semantic error: " & fore red



{- Pretty printing for type errors -}

instance Pretty TypeError where
    pretty = BS.putStrLn . chunksToLazyBS ( (typeErrorHead :) . typeErrorChunks)

typeErrorChunks :: TypeError -> [Chunk]
typeErrorChunks (HeterogeneusArrayType pos) =
    chunk "An army can only be formed by soldiers of one house." : positionChunks pos
typeErrorChunks (UnexpectedType tp1 tp2 pos) =
    [ chunk "Unexpected mention of a member from "
    , chunkFromStr tp1 & fore brightBlue
    , chunk ". You should've mentioned a member from "
    , chunkFromStr tp2 & fore brightCyan
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidExprType tp pos) =
    [ chunk "Invalid mention of a member from "
    , chunkFromStr tp & fore brightBlue
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidLValue pos) =
    chunk "There is a character that is not allowed to fight agains anyone from another family" : positionChunks pos
typeErrorChunks (IncompatibleTypes tp1 tp2 pos) =
    [ chunk "A member from "
    , chunkFromStr tp1 & fore brightBlue
    , chunk "can't be put in the same sentence with someone from "
    , chunkFromStr tp2 & fore brightCyan
    , chunk "."
    ] ++ positionChunks pos
typeErrorChunks (InvalidField f pos) =
    [ chunk "The name  "
    , chunkFromStr f & fore brightBlue
    , chunk " is not related to the person at "
    ] ++ positionChunks pos
typeErrorChunks (ConstantReassignment pos) =
    chunk "There is a character that cannot fight again." : positionChunks pos
typeErrorChunks (NonCasteableTypes tp1 tp2 pos) =
    [ chunk "You cannot marry someone from "
    , chunkFromStr tp1 & fore brightBlue
    , chunk " to a member of the family "
    , chunkFromStr tp2 & fore brightCyan
    , chunk "."
    ] ++ positionChunks pos

typeErrorHead  :: Chunk
typeErrorHead = chunk "Your scriptures contain inconsistencies in characters: " & fore red

positionChunks :: Tk.Position -> [Chunk]
positionChunks Tk.Position{ Tk.row = r , Tk.col = c } =
    [ chunk "\n At position --> Line "
    , chunkFromStr (show r) & fore brightRed
    , chunk ": Column "
    , chunkFromStr (show c) & fore brightRed
    , chunk "\n"
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
