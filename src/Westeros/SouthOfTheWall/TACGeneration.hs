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
    , generateLabel
    , generateCodeArithmeticBin
    , generateCodeArithmeticUnary
    , generateCodeComparison
    , generateCodeLogicalAnd
    , generateCodeLogicalOr
    , generateCodeLogicalNot
    ) where


import Control.Monad.RWS                (get, put, gets)
import Data.Sequence                    (Seq, (|>))
import TACTypes.TAC                     (TACCode)
import Westeros.SouthOfTheWall.Symtable (MonadParser)

import qualified Data.Sequence                      as Seq
import qualified TACTypes.TAC                       as TAC
import qualified Westeros.SouthOfTheWall.AST        as AST
import qualified Westeros.SouthOfTheWall.Types      as T
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

getNextInstruction :: MonadParser Int
getNextInstruction = gets (Seq.length . ST.tacCode)

generateLabel :: MonadParser Label
generateLabel = do
    label <- getNextLabel
    generateCode $ TAC.TACCode TAC.MetaLabel (Just $ TAC.Label label) Nothing Nothing
    return label

getTempFromAddress :: Address -> MonadParser String
getTempFromAddress (Temp temp) = return temp
getTempFromAddress (Memory offset) = do
    temp <- getNextTemp
    generateCode $ TAC.TACCode TAC.RDeref (Just $ TAC.Id temp) (Just $ TAC.Id TAC.base) (Just $ TAC.Id offset)
    return temp

getFloatFromAddress :: Address -> MonadParser String
getFloatFromAddress (Temp temp) = return temp
getFloatFromAddress (Memory offset) = do
    temp <- getNextFloat
    generateCode $ TAC.TACCode TAC.RDeref (Just $ TAC.Id temp) (Just $ TAC.Id TAC.base) (Just $ TAC.Id offset)
    return temp

generateCodeArithmeticBin :: AST.Expression -> Expression -> Expression -> MonadParser Expression
generateCodeArithmeticBin astExpr exp1 exp2 = do
    let t = AST.getType astExpr
        op = case AST.getExpr astExpr of
            (AST.BinOp AST.Sum _ _)     -> TAC.Add
            (AST.BinOp AST.Sub _ _)     -> TAC.Sub
            (AST.BinOp AST.Prod _ _)    -> TAC.Mult
            (AST.BinOp AST.Div _ _)     -> TAC.Div
            (AST.BinOp AST.Mod _ _)     -> TAC.Mod
            _ -> error "Called 'generateCodeArithmeticBin' with a non aritmetic expression"
    temp <- case t of
        T.FloatT -> getNextFloat
        T.IntT   -> getNextTemp
        _        -> return "error"
    t1 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress exp1
        T.IntT   -> getTempFromAddress $ getAddress exp1
        _        -> return "error"
    t2 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress exp2
        T.IntT   -> getTempFromAddress $ getAddress exp2
        _        -> return "error"
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = op
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id t1
        , TAC.tacRValue2    = Just $ TAC.Id t2
        }
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = []
        , getFalseList  = []
        , getAddress    = Temp temp
        }

generateCodeArithmeticUnary :: AST.Expression -> Expression -> MonadParser Expression
generateCodeArithmeticUnary astExpr expr = do
    let t = AST.getType astExpr
        op = case AST.getExpr astExpr of
            (AST.UnOp AST.Neg _)     -> TAC.Minus
            _ -> error "Called 'generateCodeArithmeticBin' with a non aritmetic expression"
    temp <- case t of
        T.FloatT -> getNextFloat
        T.IntT   -> getNextTemp
        _        -> return "error"
    t1 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress expr
        T.IntT   -> getTempFromAddress $ getAddress expr
        _        -> return "error"
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = op
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id t1
        , TAC.tacRValue2    = Nothing
        }
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = []
        , getFalseList  = []
        , getAddress    = Temp temp
        }


generateCodeComparison :: AST.Expression -> Expression -> Expression -> MonadParser Expression
generateCodeComparison astExpr exp1 exp2 = do
    let t = AST.getType $ getExpr exp1
        op = case AST.getExpr astExpr of
            (AST.BinOp AST.Eq _ _)      -> TAC.Eq
            (AST.BinOp AST.Neq _ _)     -> TAC.Neq
            (AST.BinOp AST.Lt _ _)      -> TAC.Lt
            (AST.BinOp AST.Leq _ _)     -> TAC.Leq
            (AST.BinOp AST.Gt _ _)      -> TAC.Gt
            (AST.BinOp AST.Geq _ _)     -> TAC.Geq
            _ -> error "Called 'generateCodeComparison' with a non comparative expression"
    temp <- getNextTemp
    t1 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress exp1
        T.IntT   -> getTempFromAddress $ getAddress exp1
        T.BoolT  -> getTempFromAddress $ getAddress exp1
        T.CharT  -> getTempFromAddress $ getAddress exp1
        _        -> return "error"
    t2 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress exp2
        T.IntT   -> getTempFromAddress $ getAddress exp2
        T.BoolT  -> getTempFromAddress $ getAddress exp2
        T.CharT  -> getTempFromAddress $ getAddress exp2
        _        -> return "error"
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = op
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id t1
        , TAC.tacRValue2    = Just $ TAC.Id t2
        }
    trueInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goif
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Just $ TAC.Id temp
        , TAC.tacRValue2    = Nothing
        }
    falseInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = [trueInst]
        , getFalseList  = [falseInst]
        , getAddress    = Temp temp
        }

generateCodeLogicalAnd :: AST.Expression -> Expression -> Expression -> Label -> MonadParser Expression
generateCodeLogicalAnd astExpr exp1 exp2 label = do

    temp <- getNextTemp
    t1 <- getTempFromAddress $ getAddress exp1
    t2 <- getTempFromAddress $ getAddress exp2

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.And
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id t1
        , TAC.tacRValue2    = Just $ TAC.Id t2
        }

    backpatch (getTrueList exp1) label
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = getTrueList exp2
        , getFalseList  = getFalseList exp1 ++ getFalseList exp2
        , getAddress    = Temp temp
        }

generateCodeLogicalOr :: AST.Expression -> Expression -> Expression -> Label -> MonadParser Expression
generateCodeLogicalOr astExpr exp1 exp2 label = do

    temp <- getNextTemp
    t1 <- getTempFromAddress $ getAddress exp1
    t2 <- getTempFromAddress $ getAddress exp2

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Or
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id t1
        , TAC.tacRValue2    = Just $ TAC.Id t2
        }

    backpatch (getFalseList exp1) label
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = getTrueList exp1 ++ getTrueList exp2
        , getFalseList  = getFalseList exp2
        , getAddress    = Temp temp
        }

generateCodeLogicalNot:: AST.Expression -> Expression -> MonadParser Expression
generateCodeLogicalNot astExpr expr = do

    temp <- getNextTemp
    t1 <- getTempFromAddress $ getAddress expr

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Neg
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id t1
        , TAC.tacRValue2    = Nothing
        }

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = getFalseList expr
        , getFalseList  = getTrueList expr
        , getAddress    = Temp temp
        }