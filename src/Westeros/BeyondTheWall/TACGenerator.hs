module Westeros.BeyondTheWall.TACGenerator where 

import Control.Monad.State  (State, get, put)

import qualified TACTypes.TAC                       as TAC
import qualified Westeros.SouthOfTheWall.AST        as AST
import qualified Westeros.SouthOfTheWall.Symtable   as ST

data TACState = TACState 
    { nextTemp :: Int 
    , nextLabel :: Int
    }

type TACMonad = State TACState

getNextTemp :: TACMonad TAC.Operand
getNextTemp = do 
    s <- get
    let temp = 'T' : show (nextTemp s)
    put s { nextTemp = nextTemp s + 1 }
    return $ TAC.Id temp

getNextLabel :: TACMonad TAC.Operand
getNextLabel = do 
    s <- get
    let label = 'L' : show (nextLabel s)
    put s { nextLabel = nextLabel s + 1 }
    return $ TAC.Label label

generateTAC :: ST.SymbolTable -> AST.Program -> TACMonad TAC.TACProgram
generateTAC st (AST.Program global funcs mainFunc) = do 
    globalTAC <- mapM instructionToTAC global
    mainTAC <- mapM instructionToTAC mainFunc
    funcsTAC <- mapM functionstToTAC funcs
    return $ globalTAC ++ mainTAC ++ funcsTAC

expressionRvalTAC :: ST.SymbolTable -> AST.Expr -> TACMonad (TAC.TACProgram, TAC.Operand)
expressionRvalTAC _ (AST.IntLit n) = return ([], TAC.Constant (TAC.Int n))
expressionRvalTAC _ (AST.CharLit c) = return ([], TAC.Constant (TAC.Char c))
expressionRvalTAC _ (AST.FloatLit f) = return ([], TAC.Constant (TAC.Float f))
expressionRvalTAC _ (AST.StringLit s) = undefined
expressionRvalTAC _ (AST.AtomLit a) = undefined
expressionRvalTAC _ (AST.TrueLit) = return ([], TAC.Constant (TAC.Bool True))
expressionRvalTAC _ (AST.FalseLit) = return ([], TAC.Constant (TAC.Bool False))
expressionRvalTAC _ (AST.NullLit) = undefined
expressionRvalTAC _ (AST.ArrayLit a) = undefined
expressionRvalTAC _ (AST.TupleLit t) = undefined
expressionRvalTAC _ (AST.FuncCall f args) = undefined
expressionRvalTAC st (AST.BinOp op expl expr) = do 
    (lcode, lval) <- expressionRvalTAC st $ AST.getExpr expl
    (rcode, rval) <- expressionRvalTAC st $ AST.getExpr expr
    temp <- getNextTemp
    let opTAC = case op of 
        AST.Sum     -> TAC.Add
        AST.Sub     -> TAC.Sub
        AST.Mult    -> TAC.Prod
        AST.Div     -> TAC.Div
        AST.Mod     -> TAC.Mod
        AST.Eq      -> TAC.Eq
        AST.Neq     -> TAC.Neq
        AST.Lt      -> TAC.Lt
        AST.Gt      -> TAC.Gt
        AST.Leq     -> TAC.Leq
        AST.Geq     -> TAC.Geq
        AST.And     -> TAC.And
        AST.Or      -> TAC.Or
    let code = TACCode opTAC (Just temp) (Just lval) (Just rval)
    return (lcode ++ rcode ++ [code], temp) 
expressionRvalTAC _ (AST.UnOp op expr) = do 
    (code, val) <- expressionRvalTAC st $ AST.getExpr expr
    temp <- getNextTemp
    let (opTAC, operand) = case op
        AST.Neg -> (TAC.Minus, Nothing)
        AST.Not -> (TAC.Neg, Nothing)
        AST.Deref -> (TAC.RDeref, Just 0)
    let newCode = TACCode opTAC temp val operand
    return (code ++ [newCode], temp)

