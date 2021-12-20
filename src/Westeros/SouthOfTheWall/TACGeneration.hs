module Westeros.SouthOfTheWall.TACGeneration (
      Expression (..)
    , Address (..)
    , Instruction (..)
    , CodeBlock (..)
    , backpatch
    , generateCode
    , getNextTemp
    , getNextFloat
    , getNextLabel
    ) where


import Control.Monad.RWS                (get, put)
import Data.Sequence                    (Seq, (|>))
import TACTypes.TAC                     (TACCode)
import Westeros.SouthOfTheWall.Symtable (MonadParser)

import qualified Data.Sequence                      as Seq
import qualified TACTypes.TAC                       as TAC
import qualified Westeros.SouthOfTheWall.AST        as AST
import qualified Westeros.SouthOfTheWall.Symtable   as ST

type Label = String

data Address
    = Temp String
    | Memory String

data Expression = Expression
    { getExpr       :: AST.Expression
    , getTrueList   :: [Int]
    , getFalseList  :: [Int]
    , getAddress    :: Address
    }

data Instruction = Instruction
    { getInstruction    :: AST.Instruction
    , getNextList       :: [Int]
    , getBreakList      :: [Int]
    , getContinueList   :: [Int]
    }

data CodeBlock = CodeBlock
    { getInstructions       :: [AST.Instruction]
    , getBlockNextList      :: [Int]
    , getBlockBreakList     :: [Int]
    , getBlockContinueList  :: [Int]
    }

backpatch :: [Int] -> Label -> MonadParser ()
backpatch list label = do
    st <- get
    put st { ST.tacCode = foldr backpatch' (ST.tacCode st) list }
  where
    updateJump :: TACCode -> TACCode
    updateJump tac@TAC.TACCode { TAC.tacOperation = TAC.Goto } = tac { TAC.tacLValue = Just $ TAC.Label label }
    updateJump tac@TAC.TACCode { TAC.tacOperation = TAC.Goif } = tac { TAC.tacLValue = Just $ TAC.Label label }
    updateJump tac = tac
    backpatch' :: Int -> Seq TACCode -> Seq TACCode
    backpatch' n s = Seq.adjust' updateJump n s

generateCode :: TACCode -> MonadParser ()
generateCode tac = do
    st <- get
    put st { ST.tacCode = ST.tacCode st |> tac }

getNextTemp :: MonadParser String
getNextTemp = do
    st <- get
    let n = ST.nextTemp st
        temp = "t" ++ show n
    put st { ST.nextTemp = n+1 }
    return temp

getNextFloat :: MonadParser String
getNextFloat = do
    st <- get
    let n = ST.nextTemp st
        float = "f" ++ show n
    put st { ST.nextTemp = n+1 }
    return float

getNextLabel :: MonadParser String
getNextLabel = do
    st <- get
    let n = ST.nextLabel st
        label = "L" ++ show n
    put st { ST.nextLabel = n+1 }
    return label

