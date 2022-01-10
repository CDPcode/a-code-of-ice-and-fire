module Westeros.SouthOfTheWall.TACGeneration (
      Expression (..)
    , Address (..)
    , Instruction (..)
    , CodeBlock (..)
    , Case (..)
    , CaseInit (..)
    , Label
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
    , generateCodeLiteral
    , generateCodeId
    , generateCodeStructAccess
    , generateCodeUnionAccess
    , generateCodeUnionQuery
    , generateCodeArrayAccess
    , generateCodeTupleAccess
    , generateCodeDeref
    , generateCodeIf
    , generateCodeIfElse
    , generateCodeSingleJump
    , generateCodeWhile
    , generateCodeFor
    , generateCodeForInit
    , generateCodeRead
    , generateCodePrint
    , generateCodeNew
    , generateCodeFree
    , generateCodeContinue
    , generateCodeBreak
    , generateCodeCaseInit
    , generateCodeCase
    , generateCodeSwitch
    , generateCodeOpenFunction
    , generateCodeCloseFunction
    , generateCodeAssign
    , generateCodePrintString
    , patchFunction
    ,generateCodeAllocArray,generateCodeParamArray,generateCodeArraySize,generateCodeFunctionCall,generateCodeReturn) where


import Control.Monad.RWS                (get, put, gets)
import Data.Maybe                       (fromMaybe, mapMaybe, fromJust)
import Data.Sequence                    (Seq, (|>))
import Westeros.SouthOfTheWall.Symtable (MonadParser)
import qualified Data.Map                           as M
import qualified Data.Sequence                      as Seq
import qualified TACTypes.TAC                       as TAC
import qualified Westeros.SouthOfTheWall.AST        as AST
import qualified Westeros.SouthOfTheWall.Types      as T
import qualified Westeros.SouthOfTheWall.Symtable   as ST
import Data.Bifunctor (second)

type Label = String

data Address
    = Temp String
    | Memory String
    | Heap String

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

data CaseInit = CaseInit
    { getAtomId :: Maybe Int
    , getCaseLabel :: Label
    , getCompInst :: Maybe Int
    , getJumpInst :: Maybe Int
    }

data Case = Case
    { getAstCaseExpr    :: AST.Case
    , getCaseInit       :: CaseInit
    , getCaseCodeBlock  :: CodeBlock
    , getCaseJumpInst   :: Int
    }

backpatch :: [Int] -> String -> MonadParser ()
backpatch list op = do
    st <- get
    put st { ST.tacCode = foldr backpatch' (ST.tacCode st) list }
  where
    updateCode :: TAC.TACCode -> TAC.TACCode
    updateCode tac@TAC.TACCode { TAC.tacOperation = TAC.Goto } = tac { TAC.tacLValue  = Just $ TAC.Label op }
    updateCode tac@TAC.TACCode { TAC.tacOperation = TAC.Goif } = tac { TAC.tacLValue  = Just $ TAC.Label op }
    updateCode tac@TAC.TACCode { TAC.tacOperation = TAC.Neq  } = tac { TAC.tacRValue1 = Just $ TAC.Id    op }
    updateCode tac = tac
    backpatch' :: Int -> Seq TAC.TACCode -> Seq TAC.TACCode
    backpatch' n s = Seq.adjust' updateCode n s

generateCode :: TAC.TACCode -> MonadParser ()
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

-- TODO: Consider chars when loading to temp
getTempFromAddress :: Bool -> Address -> MonadParser String
getTempFromAddress _ (Temp temp) = return temp
getTempFromAddress isByte (Memory offset) = do
    temp <- getNextTemp
    let operation = if isByte then TAC.RDerefb else TAC.RDeref
    generateCode $ TAC.TACCode operation (Just $ TAC.Id temp) (Just $ TAC.Id TAC.base) (Just $ TAC.Id offset)
    return temp
getTempFromAddress isByte (Heap offset) = do
    temp <- getNextTemp
    let operation = if isByte then TAC.RDerefb else TAC.RDeref
    generateCode $ TAC.TACCode operation (Just $ TAC.Id temp) (Just $ TAC.Id offset) (Just $ TAC.Constant $ TAC.Int 0)
    return temp

getFloatFromAddress :: Address -> MonadParser String
getFloatFromAddress (Temp temp) = return temp
getFloatFromAddress (Memory offset) = do
    temp <- getNextFloat
    generateCode $ TAC.TACCode TAC.RDeref (Just $ TAC.Id temp) (Just $ TAC.Id TAC.base) (Just $ TAC.Id offset)
    return temp
getFloatFromAddress (Heap offset) = do
    temp <- getNextFloat
    generateCode $ TAC.TACCode TAC.RDeref (Just $ TAC.Id temp) (Just $ TAC.Id offset) (Just $ TAC.Constant $ TAC.Int 0)
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
        T.IntT   -> getTempFromAddress False $ getAddress exp1
        _        -> return "error"
    t2 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress exp2
        T.IntT   -> getTempFromAddress False $ getAddress exp2
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
        T.IntT   -> getTempFromAddress False $ getAddress expr
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
        T.IntT   -> getTempFromAddress False $ getAddress exp1
        T.BoolT  -> getTempFromAddress False $ getAddress exp1
        T.CharT  -> getTempFromAddress True $ getAddress exp1
        _        -> return "error"
    t2 <- case t of
        T.FloatT -> getFloatFromAddress $ getAddress exp2
        T.IntT   -> getTempFromAddress False $ getAddress exp2
        T.BoolT  -> getTempFromAddress False $ getAddress exp2
        T.CharT  -> getTempFromAddress True $ getAddress exp2
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

    trueLabel <- generateLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Bool True
        , TAC.tacRValue2    = Nothing
        }
    trueInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }


    falseLabel <- generateLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Bool False
        , TAC.tacRValue2    = Nothing
        }
    falseInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }

    backpatch (getTrueList exp1) label
    backpatch (getTrueList exp2) trueLabel
    backpatch (getFalseList exp1 ++ getFalseList exp2) falseLabel
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = [trueInst]
        , getFalseList  = [falseInst]
        , getAddress    = Temp temp
        }

generateCodeLogicalOr :: AST.Expression -> Expression -> Expression -> Label -> MonadParser Expression
generateCodeLogicalOr astExpr exp1 exp2 label = do

    temp <- getNextTemp

    trueLabel <- generateLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Bool True
        , TAC.tacRValue2    = Nothing
        }
    trueInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }


    falseLabel <- generateLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Bool False
        , TAC.tacRValue2    = Nothing
        }
    falseInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }

    backpatch (getFalseList exp1) label
    backpatch (getFalseList exp2) falseLabel
    backpatch (getTrueList exp1 ++ getTrueList exp2) trueLabel
    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = [trueInst]
        , getFalseList  = [falseInst]
        , getAddress    = Temp temp
        }


generateCodeLogicalNot:: AST.Expression -> Expression -> MonadParser Expression
generateCodeLogicalNot astExpr expr = do

    temp <- getNextTemp

    trueLabel <- generateLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Bool True
        , TAC.tacRValue2    = Nothing
        }
    trueInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }


    falseLabel <- generateLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Bool False
        , TAC.tacRValue2    = Nothing
        }
    falseInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }

    backpatch (getTrueList expr) falseLabel
    backpatch (getFalseList expr) trueLabel

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = [trueInst]
        , getFalseList  = [falseInst]
        , getAddress    = Temp temp
        }

generateCodeDeref :: AST.Expression -> Expression -> MonadParser Expression
generateCodeDeref astExpr expr = do

    (trueList, falseList) <- case AST.getType astExpr of
        T.BoolT -> do
            temp <- getTempFromAddress False $ getAddress expr
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
            return ([trueInst], [falseInst])
        _ -> return ([], [])

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = trueList
        , getFalseList  = falseList
        , getAddress    = getAddress expr
        }

-- TODO: be careful with assign with chars
generateCodeLiteral :: AST.Expression -> MonadParser Expression
generateCodeLiteral astExpr = do
    temp <- case AST.getType astExpr of
        T.FloatT -> getNextFloat
        _        -> getNextTemp

    let constant = case AST.getExpr astExpr of
            AST.IntLit n    -> TAC.Constant $ TAC.Int n
            AST.FloatLit f  -> TAC.Constant $ TAC.Float f
            AST.CharLit c   -> TAC.Constant $ TAC.Char c
            AST.TrueLit     -> TAC.Constant $ TAC.Bool True
            AST.FalseLit    -> TAC.Constant $ TAC.Bool False
            AST.AtomLit n   -> TAC.Constant $ TAC.Int n
            AST.NullLit     -> TAC.Constant $ TAC.Int 0
            _               -> error "Function 'generateCodeLiteral' called without a literal"

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just constant
        , TAC.tacRValue2    = Nothing
        }

    trueList <- case AST.getExpr astExpr of
        AST.TrueLit -> do
            trueInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return [trueInst]
        _ -> return []

    falseList <- case AST.getExpr astExpr of
        AST.TrueLit -> do
            falseList <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return [falseList]
        _ -> return []

    return $ Expression
        { getExpr      = astExpr
        , getTrueList  = trueList
        , getFalseList = falseList
        , getAddress   = Temp temp
        }

generateCodeId :: AST.Expression -> Int -> MonadParser Expression
generateCodeId astExpr@AST.Expression{AST.getExpr=AST.IdExpr sym} offset = do

    temp <- getNextTemp
    maybeInfo <- ST.lookupST sym
    address <- case maybeInfo of
        Just info -> do
            case ST.additional info of
                Just (ST.ParameterType ST.Reference) -> do
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation  = TAC.Add
                        , TAC.tacLValue     = Just $ TAC.Id temp
                        , TAC.tacRValue1    = Just $ TAC.Id TAC.base
                        , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int offset
                        }
                    return $ Heap temp
                _ -> do
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation  = TAC.Assign
                        , TAC.tacLValue     = Just $ TAC.Id temp
                        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Int offset
                        , TAC.tacRValue2    = Nothing
                        }
                    return $ Memory temp
        _ -> do
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Assign
                , TAC.tacLValue     = Just $ TAC.Id temp
                , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Int offset
                , TAC.tacRValue2    = Nothing
                }
            return $ Memory temp

    (trueList, falseList) <- case AST.getType astExpr of
        T.BoolT -> do
            temp' <- getTempFromAddress False address
            trueInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goif
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Just $ TAC.Id temp'
                , TAC.tacRValue2    = Nothing
                }
            falseInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return ([trueInst], [falseInst])
        _ -> return ([], [])

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = trueList
        , getFalseList  = falseList
        , getAddress    = address
        }
generateCodeId _ _ = error "Function 'generateCodeId' called without an Id"

generateCodeStructAccess :: AST.Expression -> String -> Expression -> MonadParser Expression
generateCodeStructAccess astExpr sym structExpr = do
    let sc = case AST.getType $ getExpr structExpr of
            T.StructT n _ -> n
            _             -> 0

    mInfo <- ST.lookupInScopeST sym sc
    offset <- case mInfo of
        Just ST.SymbolInfo { ST.offset = Just o } -> return o
        _ -> return 0

    temp <- getNextTemp

    let (r0, address) = case getAddress structExpr of
            Temp t   -> (t, Temp temp)
            Memory t -> (t, Memory temp)
            Heap t   -> (t, Heap temp)

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Add
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id r0
        , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int offset
        }

    (trueList, falseList) <- case AST.getType astExpr of
        T.BoolT -> do
            temp' <- getTempFromAddress False address
            trueInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goif
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Just $ TAC.Id temp'
                , TAC.tacRValue2    = Nothing
                }
            falseInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return ([trueInst], [falseInst])
        _ -> return ([], [])

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = trueList
        , getFalseList  = falseList
        , getAddress    = address
        }

generateCodeUnionAccess :: AST.Expression -> Expression -> MonadParser Expression
generateCodeUnionAccess astExpr unionExpr = do
    temp <- getNextTemp

    let (r0, address) = case getAddress unionExpr of
            Temp t   -> (t, Temp temp)
            Memory t -> (t, Memory temp)
            Heap t   -> (t, Heap temp)

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Add
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id r0
        , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int 4
        }

    (trueList, falseList) <- case AST.getType astExpr of
        T.BoolT -> do
            temp' <- getTempFromAddress False address
            trueInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goif
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Just $ TAC.Id temp'
                , TAC.tacRValue2    = Nothing
                }
            falseInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return ([trueInst], [falseInst])
        _ -> return ([], [])

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = trueList
        , getFalseList  = falseList
        , getAddress    = address
        }

generateCodeUnionQuery :: AST.Expression -> String -> Expression -> MonadParser Expression
generateCodeUnionQuery astExpr sym unionExpr = do
    let idx = case AST.getType astExpr of
            T.UnionT _ ids -> length $ takeWhile (\p -> fst p /= sym) ids
            _              -> 0

    temp <- getNextTemp

    temp' <- getTempFromAddress False $ getAddress unionExpr

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Eq
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id temp'
        , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int idx
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

generateCodeArrayAccess :: AST.Expression -> Expression -> [Expression] -> MonadParser Expression
generateCodeArrayAccess astExpr arrayExpr indexList = do

    temp <- getNextTemp

    let (r0, address) = case getAddress arrayExpr of
            Temp t   -> (t, Temp temp)
            Memory t -> (t, Memory temp)
            Heap t   -> (t, Heap temp)

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Add
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id r0
        , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int 4
        }

    mWidth <- T.getTypeWidth $ AST.getTypeStr astExpr
    let width = case mWidth of
            Just w -> w
            Nothing -> error "This should not be happening"

    indexTemp <- generateIndex address Nothing indexList width
    arrayStart <- getTempFromAddress False $ getAddress arrayExpr
    resultTemp <- getNextTemp
    let resultAddress = case getAddress arrayExpr of
            Temp _   -> Temp resultTemp
            Memory _ -> Memory resultTemp
            Heap _   -> Heap resultTemp

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Add
        , TAC.tacLValue     = Just $ TAC.Id resultTemp
        , TAC.tacRValue1    = Just $ TAC.Id arrayStart
        , TAC.tacRValue2    = Just $ TAC.Id indexTemp
        }

    (trueList, falseList) <- case AST.getType astExpr of
        T.BoolT -> do
            temp' <- getTempFromAddress False address
            trueInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goif
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Just $ TAC.Id temp'
                , TAC.tacRValue2    = Nothing
                }
            falseInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return ([trueInst], [falseInst])
        _ -> return ([], [])

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = trueList
        , getFalseList  = falseList
        , getAddress    = resultAddress
        }

  where
    generateIndex :: Address -> Maybe String -> [Expression] -> Int -> MonadParser String
    generateIndex _ previousTemp [indx]  width = do

        indexTemp <- getTempFromAddress False $ getAddress indx

        temp <- case previousTemp of
            Nothing -> return indexTemp
            Just t  -> do
                temp <- getNextTemp
                generateCode $ TAC.TACCode
                    { TAC.tacOperation  = TAC.Add
                    , TAC.tacLValue     = Just $ TAC.Id temp
                    , TAC.tacRValue1    = Just $ TAC.Id t
                    , TAC.tacRValue2    = Just $ TAC.Id indexTemp
                    }
                return temp

        result <- getNextTemp
        generateCode $ TAC.TACCode
            { TAC.tacOperation  = TAC.Mult
            , TAC.tacLValue     = Just $ TAC.Id result
            , TAC.tacRValue1    = Just $ TAC.Id temp
            , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int width
            }
        return result

    generateIndex dimAddress previousTemp (indx:indxs) width = do

        newDimTemp <- getNextTemp
        let (dimTemp, newDimAddress) = case dimAddress of
                Temp t   -> (t, Temp newDimTemp)
                Memory t -> (t, Memory newDimTemp)
                Heap t   -> (t, Heap newDimTemp)

        generateCode $ TAC.TACCode
            { TAC.tacOperation  = TAC.Add
            , TAC.tacLValue     = Just $ TAC.Id newDimTemp
            , TAC.tacRValue1    = Just $ TAC.Id dimTemp
            , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int 4
            }

        dimSize <- getTempFromAddress False newDimAddress
        indexTemp <- getTempFromAddress False $ getAddress indx

        temp <- case previousTemp of
            Nothing -> return indexTemp
            Just t  -> do
                temp <- getNextTemp
                generateCode $ TAC.TACCode
                    { TAC.tacOperation  = TAC.Add
                    , TAC.tacLValue     = Just $ TAC.Id temp
                    , TAC.tacRValue1    = Just $ TAC.Id t
                    , TAC.tacRValue2    = Just $ TAC.Id indexTemp
                    }
                return temp

        result <- getNextTemp
        generateCode $ TAC.TACCode
            { TAC.tacOperation  = TAC.Mult
            , TAC.tacLValue     = Just $ TAC.Id result
            , TAC.tacRValue1    = Just $ TAC.Id temp
            , TAC.tacRValue2    = Just $ TAC.Id dimSize
            }

        generateIndex newDimAddress (Just result) indxs width

    generateIndex _ _ _ _ = return "error"

generateCodeTupleAccess :: AST.Expression -> Expression -> Int -> MonadParser Expression
generateCodeTupleAccess astExpr tupleExpr index = do

    indexTemp <- getIndexOffset (AST.getTypeStr astExpr) index

    let (r0, address) = case getAddress tupleExpr of
            Temp t   -> (t, Temp indexTemp)
            Memory t -> (t, Memory indexTemp)
            Heap t   -> (t, Heap indexTemp)

    temp <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Add
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id indexTemp
        , TAC.tacRValue2    = Just $ TAC.Id r0
        }

    (trueList, falseList) <- case AST.getType astExpr of
        T.BoolT -> do
            temp' <- getTempFromAddress False address
            trueInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goif
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Just $ TAC.Id temp'
                , TAC.tacRValue2    = Nothing
                }
            falseInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation  = TAC.Goto
                , TAC.tacLValue     = Just $ TAC.Label "_"
                , TAC.tacRValue1    = Nothing
                , TAC.tacRValue2    = Nothing
                }
            return ([trueInst], [falseInst])
        _ -> return ([], [])

    return $ Expression
        { getExpr       = astExpr
        , getTrueList   = trueList
        , getFalseList  = falseList
        , getAddress    = address
        }

  where
    getIndexOffset :: String -> Int -> MonadParser String
    getIndexOffset tp idx = do

        typeEntry <- ST.lookupST tp
        case typeEntry of
            Just ST.SymbolInfo{ ST.additional = Just (ST.TupleTypes tps) } -> do
                offset <- computeOffset (take idx tps)
                temp <- getNextTemp
                generateCode $ TAC.TACCode
                    { TAC.tacOperation  = TAC.Assign
                    , TAC.tacLValue     = Just $ TAC.Id temp
                    , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Int offset
                    , TAC.tacRValue2    = Nothing
                    }
                return temp
            _ -> return "error"
    computeOffset :: [String] -> MonadParser Int
    computeOffset [] = return 0
    computeOffset (tp:tps) = do
        mWidth <- T.getTypeWidth tp
        rest <- computeOffset tps
        case mWidth of
            Just w -> return $ w + rest
            Nothing -> error "This should not be happening"


generateCodeIf :: AST.Instruction -> Expression -> CodeBlock -> String -> MonadParser Instruction
generateCodeIf astInst boolExpr codeBlock label = do

    backpatch (getTrueList boolExpr) label

    return $ Instruction
        {   getInstruction   = astInst
        ,   getNextList      = getFalseList boolExpr ++ getBlockNextList codeBlock
        ,   getBreakList     = getBlockBreakList codeBlock
        ,   getContinueList  = getBlockContinueList codeBlock
        }

generateCodeSingleJump :: MonadParser Int
generateCodeSingleJump = do
    nextInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }
    return nextInst

generateCodeIfElse :: AST.Instruction -> Expression -> CodeBlock -> CodeBlock -> String -> String -> Int -> MonadParser Instruction
generateCodeIfElse astInst boolExpr codeBlockThen codeBlockElse thenLabel elseLabel jumpInstr = do

    backpatch (getTrueList boolExpr) thenLabel
    backpatch (getFalseList boolExpr) elseLabel

    return $ Instruction
        {   getInstruction   = astInst
        ,   getNextList      = getBlockNextList codeBlockThen ++ [jumpInstr] ++ getBlockNextList codeBlockElse
        ,   getBreakList     = getBlockBreakList codeBlockThen ++ getBlockBreakList codeBlockElse
        ,   getContinueList  = getBlockContinueList codeBlockThen ++ getBlockContinueList codeBlockElse
        }

generateCodeWhile :: AST.Instruction -> Expression -> CodeBlock -> String -> String -> MonadParser Instruction
generateCodeWhile astInst boolExpr codeBlock beginLabel blockLabel = do

    backpatch (getBlockNextList codeBlock) beginLabel
    backpatch (getTrueList boolExpr) blockLabel
    backpatch (getBlockContinueList codeBlock) beginLabel

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goto
        , TAC.tacLValue     = Just $ TAC.Label beginLabel
        , TAC.tacRValue1    = Nothing
        , TAC.tacRValue2    = Nothing
        }

    return $ Instruction
        {   getInstruction   = astInst
        ,   getNextList      = getFalseList boolExpr ++ getBlockBreakList codeBlock
        ,   getBreakList     = []
        ,   getContinueList  = []
        }


generateCodeForInit :: Expression -> Expression -> Int -> MonadParser (String, Int)
generateCodeForInit lbExpr ubExpr offset = do

    t0 <- getTempFromAddress False $ getAddress lbExpr

    t1 <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id t1
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Int offset
        , TAC.tacRValue2    = Nothing
        }

    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.LDeref
        ,   TAC.tacLValue     = Just $ TAC.Id TAC.base
        ,   TAC.tacRValue1    = Just $ TAC.Id t1
        ,   TAC.tacRValue2    = Just $ TAC.Id t0
        }

    t2 <- getTempFromAddress False $ getAddress ubExpr

    blockLabel <- generateLabel

    t3 <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.RDeref
        , TAC.tacLValue     = Just $ TAC.Id t3
        , TAC.tacRValue1    = Just $ TAC.Id TAC.base
        , TAC.tacRValue2    = Just $ TAC.Id t1
        }

    cond <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Geq
        , TAC.tacLValue     = Just $ TAC.Id cond
        , TAC.tacRValue1    = Just $ TAC.Id t3
        , TAC.tacRValue2    = Just $ TAC.Id t2
        }

    endCycle <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Goif
        , TAC.tacLValue     = Just $ TAC.Label "_"
        , TAC.tacRValue1    = Just $ TAC.Id cond
        , TAC.tacRValue2    = Nothing
        }

    return (blockLabel, endCycle)

generateCodeFor :: AST.Instruction -> CodeBlock -> String -> Int -> Int -> MonadParser Instruction
generateCodeFor astInst codeBlock blockLabel iterOffset endInst = do

    incLabel <- generateLabel
    backpatch(getBlockNextList codeBlock) incLabel
    backpatch(getBlockContinueList codeBlock) incLabel

    t0 <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Assign
        , TAC.tacLValue     = Just $ TAC.Id t0
        , TAC.tacRValue1    = Just $ TAC.Constant $ TAC.Int iterOffset
        , TAC.tacRValue2    = Nothing
        }

    t1 <- getNextTemp
    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.RDeref
        ,   TAC.tacLValue     = Just $ TAC.Id t1
        ,   TAC.tacRValue1    = Just $ TAC.Id TAC.base
        ,   TAC.tacRValue2    = Just $ TAC.Id t0
        }

    t2 <- getNextTemp
    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.Add
        ,   TAC.tacLValue     = Just $ TAC.Id t2
        ,   TAC.tacRValue1    = Just $ TAC.Id t1
        ,   TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int 1
        }

    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.LDeref
        ,   TAC.tacLValue     = Just $ TAC.Id TAC.base
        ,   TAC.tacRValue1    = Just $ TAC.Id t0
        ,   TAC.tacRValue2    = Just $ TAC.Id t2
        }

    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.Goto
        ,   TAC.tacLValue     = Just $ TAC.Label blockLabel
        ,   TAC.tacRValue1    = Nothing
        ,   TAC.tacRValue2    = Nothing
        }

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = endInst : getBlockBreakList codeBlock
        ,  getBreakList     = []
        ,  getContinueList  = []
        }

generateCodeRead :: AST.Instruction -> Expression -> MonadParser Instruction
generateCodeRead astInst expr = do

    t0 <- case AST.getType (getExpr expr) of
        T.ArrayT T.CharT 1 -> getTempFromAddress False $ getAddress expr
        _ -> getNextTemp

    op <- case AST.getType (getExpr expr) of
        T.IntT -> return TAC.Readi
        T.FloatT -> return TAC.Readf
        T.CharT -> return TAC.Readc
        _ -> return TAC.Read

    generateCode $ TAC.TACCode
        {   TAC.tacOperation = op
        ,   TAC.tacLValue    = Just $ TAC.Id t0
        ,   TAC.tacRValue1   = Nothing
        ,   TAC.tacRValue2   = Nothing
       }

    case AST.getType (getExpr expr) of
        T.ArrayT T.CharT 1 -> return ()
        T.CharT -> storeFromTemp True t0 $ getAddress expr
        _ -> storeFromTemp False t0 $ getAddress expr

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = []
        ,  getBreakList     = []
        ,  getContinueList  = []
        }

generateCodePrint :: AST.Instruction -> Expression -> MonadParser Instruction
generateCodePrint astInst expr = do

    t0 <- case AST.getType (getExpr expr) of
        T.FloatT -> getFloatFromAddress $ getAddress expr
        T.CharT -> getTempFromAddress True $ getAddress expr
        _ -> getTempFromAddress False $ getAddress expr

    op <- case AST.getType (getExpr expr) of
        T.IntT -> return TAC.Printi
        T.FloatT -> return TAC.Printf
        T.CharT -> return TAC.Printc
        _ -> return TAC.Print

    generateCode $ TAC.TACCode
        {   TAC.tacOperation = op
        ,   TAC.tacLValue    = Just $ TAC.Id t0
        ,   TAC.tacRValue1   = Nothing
        ,   TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = []
        ,  getBreakList     = []
        ,  getContinueList  = []
        }

generateCodeNew :: AST.Instruction -> Expression -> MonadParser Instruction
generateCodeNew astInst expr = do

    mWidth <- T.getTypeWidth $ AST.getTypeStr (getExpr expr)

    let width = fromMaybe 0 mWidth

    t0 <- getNextTemp
    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.Assign
        ,   TAC.tacLValue    = Just $ TAC.Id t0
        ,   TAC.tacRValue1   = Just $ TAC.Constant $ TAC.Int width
        ,   TAC.tacRValue2   = Nothing
        }

    t1 <- getNextTemp
    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.Malloc
        ,   TAC.tacLValue    = Just $ TAC.Id t1
        ,   TAC.tacRValue1   = Just $ TAC.Id t0
        ,   TAC.tacRValue2   = Nothing
        }

    storeFromTemp False t1 $ getAddress expr

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = []
        ,  getBreakList     = []
        ,  getContinueList  = []
        }

-- TODO: Make sure this is properly done
generateCodeFree :: AST.Instruction -> Expression -> MonadParser Instruction
generateCodeFree astInst expr = do

    t0 <- getTempFromAddress False $ getAddress expr

    generateCode $ TAC.TACCode
        {   TAC.tacOperation  = TAC.Free
        ,   TAC.tacLValue    = Just $ TAC.Id t0
        ,   TAC.tacRValue1   = Nothing
        ,   TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = []
        ,  getBreakList     = []
        ,  getContinueList  = []
        }


generateCodeContinue :: AST.Instruction -> MonadParser Instruction
generateCodeContinue astInst = do

    nextInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Goto
        , TAC.tacLValue    = Just $ TAC.Label "_"
        , TAC.tacRValue1   = Nothing
        , TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = []
        ,  getBreakList     = []
        ,  getContinueList  = [nextInst]
    }

generateCodeBreak :: AST.Instruction -> MonadParser Instruction
generateCodeBreak astInst = do

    nextInst <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Goto
        , TAC.tacLValue    = Just $ TAC.Label "_"
        , TAC.tacRValue1   = Nothing
        , TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        {  getInstruction   = astInst
        ,  getNextList      = []
        ,  getBreakList     = [nextInst]
        ,  getContinueList  = []
    }

storeFromTemp :: Bool -> String -> Address -> MonadParser ()
storeFromTemp isByte temp (Temp t0) = do
    let operation = if isByte then TAC.Assignb else TAC.Assign
    generateCode $ TAC.TACCode
        { TAC.tacOperation = operation
        , TAC.tacLValue = Just $ TAC.Id t0
        , TAC.tacRValue1 = Just $ TAC.Id temp
        , TAC.tacRValue2 = Nothing
        }
storeFromTemp isByte temp (Memory t0) = do
    let operation = if isByte then TAC.LDerefb else TAC.LDeref
    generateCode $ TAC.TACCode
        { TAC.tacOperation = operation
        , TAC.tacLValue = Just $ TAC.Id t0
        , TAC.tacRValue1 = Just $ TAC.Id TAC.base
        , TAC.tacRValue2 = Just $ TAC.Id temp
        }
storeFromTemp isByte temp (Heap t0) = do
    let operation = if isByte then TAC.LDerefb else TAC.LDeref
    generateCode $ TAC.TACCode
        { TAC.tacOperation = operation
        , TAC.tacLValue = Just $ TAC.Id t0
        , TAC.tacRValue1 = Just $ TAC.Constant $ TAC.Int 0
        , TAC.tacRValue2 = Just $ TAC.Id temp
        }

generateCodeCaseInit :: Maybe Int -> MonadParser CaseInit
generateCodeCaseInit maybeAtom = do

    caseLabel <- generateLabel
    case maybeAtom of
        Nothing -> return $ CaseInit
            { getAtomId     = Nothing
            , getCaseLabel  = caseLabel
            , getCompInst   = Nothing
            , getJumpInst   = Nothing
            }
        Just atom -> do
            compInst <- getNextInstruction
            t0 <- getNextTemp
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.Neq
                , TAC.tacLValue    = Just $ TAC.Id t0
                , TAC.tacRValue1   = Just $ TAC.Id "_"
                , TAC.tacRValue2   = Just $ TAC.Constant $ TAC.Int atom
                }
            jumpInst <- getNextInstruction
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.Goif
                , TAC.tacLValue    = Just $ TAC.Label "_"
                , TAC.tacRValue1   = Just $ TAC.Id t0
                , TAC.tacRValue2   = Nothing
                }
            return $ CaseInit
                { getAtomId     = Just atom
                , getCaseLabel  = caseLabel
                , getCompInst   = Just compInst
                , getJumpInst   = Just jumpInst
                }

generateCodeCase :: AST.Case -> CaseInit -> CodeBlock -> Label -> Int -> MonadParser Case
generateCodeCase astCaseExpr caseInit codeBlock label jumpInst = do

    backpatch (getBlockNextList codeBlock) label

    return $ Case
        { getAstCaseExpr = astCaseExpr
        , getCaseInit = caseInit
        , getCaseCodeBlock = codeBlock
        , getCaseJumpInst = jumpInst
        }

generateCodeSwitch :: AST.Instruction -> Expression -> [Case] -> MonadParser Instruction
generateCodeSwitch astInst expr cases = do

    t0 <- getTempFromAddress False $ getAddress expr

    backpatchCompInst cases t0
    backpatchJumpInsts cases
    let nextList = map getCaseJumpInst cases
        continueList = concatMap (getBlockContinueList . getCaseCodeBlock) cases
        breakList = concatMap (getBlockBreakList . getCaseCodeBlock) cases

    return $ Instruction
        { getInstruction   = astInst
        , getNextList      = nextList
        , getBreakList     = breakList
        , getContinueList  = continueList
        }

    where
        backpatchCompInst :: [Case] -> String -> MonadParser ()
        backpatchCompInst list t0 = do
            let insts = mapMaybe (getCompInst . getCaseInit) list
            backpatch insts t0
        backpatchJumpInsts :: [Case] -> MonadParser ()
        backpatchJumpInsts list = do
            let insts = mapMaybe (getJumpInst . getCaseInit) list
                labels = map (getCaseLabel . getCaseInit) list
            backpatchJumpInsts' insts (tail labels)
        backpatchJumpInsts' :: [Int] -> [String] -> MonadParser ()
        backpatchJumpInsts' (inst:insts) (label:labels) = do
            backpatch [inst] label
            backpatchJumpInsts' insts labels
        backpatchJumpInsts' _ _ = return ()

generateCodeOpenFunction :: MonadParser Int
generateCodeOpenFunction = do

    n <- getNextInstruction
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.MetaBeginFunc
        , TAC.tacLValue    = Just $ TAC.Id "_"
        , TAC.tacRValue1   = Just $ TAC.Constant $ TAC.Int 0
        , TAC.tacRValue2   = Nothing
        }
    return n

generateCodeCloseFunction :: MonadParser ()
generateCodeCloseFunction = do

    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.MetaEndFunc
        , TAC.tacLValue    = Just $ TAC.Constant $ TAC.Int 0
        , TAC.tacRValue1   = Nothing
        , TAC.tacRValue2   = Nothing
        }

patchFunction :: Int -> MonadParser ()
patchFunction inst = do

    symT <- get
    (function, params) <- ST.currentOpenFunction
    let offset = ST.maxOffset symT
        name = function ++ '_' : show params
    put symT { ST.tacCode = backpatch' name offset inst (ST.tacCode symT) }
    where
        updateCode :: String -> Int -> TAC.TACCode -> TAC.TACCode
        updateCode name offset tac@TAC.TACCode { TAC.tacOperation = TAC.MetaBeginFunc } =
            tac { TAC.tacLValue   = Just $ TAC.Id name
                , TAC.tacRValue2  = Just $ TAC.Constant $ TAC.Int offset
            }
        updateCode _ _ tac = tac
        backpatch' :: String -> Int -> Int -> Seq TAC.TACCode -> Seq TAC.TACCode
        backpatch' name offset n s = Seq.adjust' (updateCode name offset) n s

generateCodeArraySize :: Address -> MonadParser String
generateCodeArraySize address = do

    temp <- getNextTemp
    let (r0, sizeAddress) = case address of
            Temp t   -> (t, Temp temp)
            Memory t -> (t, Memory temp)
            Heap t   -> (t, Heap temp)

    generateCode $ TAC.TACCode
        { TAC.tacOperation  = TAC.Add
        , TAC.tacLValue     = Just $ TAC.Id temp
        , TAC.tacRValue1    = Just $ TAC.Id r0
        , TAC.tacRValue2    = Just $ TAC.Constant $ TAC.Int 4
        }

    -- t0 now contains de size of the array
    getTempFromAddress False sizeAddress

generateCodeAllocArray :: Address -> MonadParser ()
generateCodeAllocArray address = do

    -- t0 now contains de size of the array
    t0 <- generateCodeArraySize address

    t1 <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Assign
        , TAC.tacLValue    = Just $ TAC.Id t1
        , TAC.tacRValue1   = Just $ TAC.Id TAC.stack
        , TAC.tacRValue2   = Nothing
        }

    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Add
        , TAC.tacLValue    = Just $ TAC.Id TAC.stack
        , TAC.tacRValue1   = Just $ TAC.Id TAC.stack
        , TAC.tacRValue2   = Just $ TAC.Id t0
        }

    storeFromTemp False t1 address


generateCodeParamArray :: String -> MonadParser()
generateCodeParamArray arrayId = do

    entry <- ST.lookupST arrayId
    maybeOffset <- case entry of
        Just ST.SymbolInfo{ST.category=ST.Parameter, ST.offset=mOffset} -> return mOffset
        _ -> return Nothing

    offset <- case maybeOffset of
        Just o -> return o
        _ -> return 0

    t0 <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Assign
        , TAC.tacLValue    = Just $ TAC.Id t0
        , TAC.tacRValue1   = Just $ TAC.Constant $ TAC.Int offset
        , TAC.tacRValue2   = Nothing
        }

    prevArrayAddress <- getTempFromAddress False $ Memory t0
    generateCodeAllocArray $ Memory t0
    newArrayAddress <- getTempFromAddress False $ Memory t0

    size <- generateCodeArraySize $ Memory t0

    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.MemCopy
        , TAC.tacLValue    = Just $ TAC.Id prevArrayAddress
        , TAC.tacRValue1   = Just $ TAC.Id newArrayAddress
        , TAC.tacRValue2   = Just $ TAC.Id size
        }

generateCodeFunctionCall :: AST.Expression -> [Expression] -> MonadParser Expression
generateCodeFunctionCall astExpr@AST.Expression{AST.getExpr = AST.FuncCall symbol _, AST.getType = tp} exprs = do

    temp <- getNextTemp
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Assign
        , TAC.tacLValue    = Just $ TAC.Id temp
        , TAC.tacRValue1   = Just $ TAC.Constant $ TAC.Int 0
        , TAC.tacRValue2   = Nothing
        }

    label <- generateLabel
    let expr = last exprs
    backpatch (getTrueList expr ++ getFalseList expr) label

    result <- getNextTemp
    case tp of
        T.TypeError ->
            return $ Expression
                { getExpr       = astExpr
                , getTrueList   = []
                , getFalseList  = []
                , getAddress    = Temp result
                }
        _ -> do
            symbolInfo <- ST.lookupFunction symbol $ length exprs
            let fInfo = ST.getFunctionMetaData $ fromJust symbolInfo
                scope = ST.fScope fInfo
            symT <- get
            let params = map (second head) $ M.toList $ ST.filterByScopeST symT scope
            generateCodeParams params exprs
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.Call
                , TAC.tacLValue    = Just $ TAC.Id result
                , TAC.tacRValue1   = Just $ TAC.Id symbol
                , TAC.tacRValue2   = Nothing
                }

            (trueList, falseList) <- case AST.getType astExpr of
                T.BoolT -> do
                    trueInst <- getNextInstruction
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation  = TAC.Goif
                        , TAC.tacLValue     = Just $ TAC.Label "_"
                        , TAC.tacRValue1    = Just $ TAC.Id result
                        , TAC.tacRValue2    = Nothing
                        }
                    falseInst <- getNextInstruction
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation  = TAC.Goto
                        , TAC.tacLValue     = Just $ TAC.Label "_"
                        , TAC.tacRValue1    = Nothing
                        , TAC.tacRValue2    = Nothing
                        }
                    return ([trueInst], [falseInst])
                _ -> return ([], [])

            return $ Expression
                { getExpr       = astExpr
                , getTrueList   = trueList
                , getFalseList  = falseList
                , getAddress    = Temp result
                }
    where
        generateCodeParams :: [(String, ST.SymbolInfo)] -> [Expression] -> MonadParser ()
        generateCodeParams [] _ = return ()
        generateCodeParams _ [] = return ()
        generateCodeParams ((_, info) : params) (expr : tacExprs) = do
            offset <- case ST.offset info of
                Just o -> return o
                _ -> return 0
            case ST.additional info of
                Just (ST.ParameterType ST.Reference) -> do
                    t0 <- getNextTemp
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation = TAC.Param
                        , TAC.tacLValue    = Just $ TAC.Id t0
                        , TAC.tacRValue1   = Just $ TAC.Constant $ TAC.Int offset
                        , TAC.tacRValue2   = Nothing
                        }
                    t1 <- case getAddress expr of
                        Heap t -> return t
                        Memory t -> do
                            t1 <- getNextTemp
                            generateCode $ TAC.TACCode
                                {TAC.tacOperation = TAC.Add
                                , TAC.tacLValue    = Just $ TAC.Id t1
                                , TAC.tacRValue1   = Just $ TAC.Id t
                                , TAC.tacRValue2   = Just $ TAC.Id TAC.base
                                }
                            return t1
                        Temp _ -> return "error: wrong address"
                    storeFromTemp False t1 $ Heap t0
                _ -> do
                    let stType = ST.symbolType info
                    mWidth <- case stType of
                        Just t -> T.getTypeWidth t
                        Nothing -> return $ Just 0
                    t0 <- getNextTemp
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation = TAC.Param
                        , TAC.tacLValue    = Just $ TAC.Id t0
                        , TAC.tacRValue1   = Just $ TAC.Constant $ TAC.Int offset
                        , TAC.tacRValue2   = Nothing
                        }
                    if T.isPrimitiveOrPointerType $ AST.getType $ getExpr expr
                        then do
                            t1 <- getTempFromAddress False $ getAddress expr
                            storeFromTemp False t1 $ Heap t0
                        else do
                            t1 <- case getAddress expr of
                                Heap t -> return t
                                Memory t -> do
                                    t2 <- getNextTemp
                                    generateCode $ TAC.TACCode
                                        { TAC.tacOperation = TAC.Add
                                        , TAC.tacLValue    = Just $ TAC.Id t2
                                        , TAC.tacRValue1   = Just $ TAC.Id t
                                        , TAC.tacRValue2   = Just $ TAC.Id TAC.base
                                        }
                                    return t2
                                Temp _ -> return "error: wrong address"

                            generateCode $ TAC.TACCode
                                { TAC.tacOperation = TAC.MemCopy
                                , TAC.tacLValue    = Just $ TAC.Id t0
                                , TAC.tacRValue1   = Just $ TAC.Id t1
                                , TAC.tacRValue2   = Just $ TAC.Constant $ TAC.Int $ fromMaybe 0 mWidth
                                }
            generateCodeParams params tacExprs
generateCodeFunctionCall _ _ = error "generateCodeFunctionCall: invalid function call"

generateCodeAssign :: AST.Instruction -> Expression -> Expression -> String -> MonadParser Instruction
generateCodeAssign astInst lExpr rExpr lLabel = do
    backpatch (getTrueList lExpr ++ getFalseList lExpr) lLabel
    rLabel <- generateLabel
    backpatch (getTrueList rExpr ++ getFalseList rExpr) rLabel

    dstAddress <- case getAddress lExpr of
        -- TODO: Check if this is right
        Temp t   -> return t
        Memory t -> do
            temp <- getNextTemp
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.Add
                , TAC.tacLValue = Just $ TAC.Id temp
                , TAC.tacRValue1 = Just $ TAC.Id TAC.base
                , TAC.tacRValue2 = Just $ TAC.Id t
                }
            return temp
        Heap t   -> return t

    case AST.getType (getExpr rExpr) of
        T.FloatT -> do
            t0 <- getFloatFromAddress (getAddress rExpr)
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.LDeref
                , TAC.tacLValue = Just $ TAC.Id dstAddress
                , TAC.tacRValue1 = Just $ TAC.Constant $ TAC.Int 0
                , TAC.tacRValue2 = Just $ TAC.Id t0
                }
        T.IntT -> do
            t0 <- getTempFromAddress False (getAddress rExpr)
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.LDeref
                , TAC.tacLValue = Just $ TAC.Id dstAddress
                , TAC.tacRValue1 = Just $ TAC.Constant $ TAC.Int 0
                , TAC.tacRValue2 = Just $ TAC.Id t0
                }
        T.AtomT -> do
            t0 <- getTempFromAddress False (getAddress rExpr)
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.LDeref
                , TAC.tacLValue = Just $ TAC.Id dstAddress
                , TAC.tacRValue1 = Just $ TAC.Constant $ TAC.Int 0
                , TAC.tacRValue2 = Just $ TAC.Id t0
                }
        T.BoolT -> do
            t0 <- getTempFromAddress False (getAddress rExpr)
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.LDeref
                , TAC.tacLValue = Just $ TAC.Id dstAddress
                , TAC.tacRValue1 = Just $ TAC.Constant $ TAC.Int 0
                , TAC.tacRValue2 = Just $ TAC.Id t0
                }
        T.CharT -> do
            t0 <- getTempFromAddress True (getAddress rExpr)
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.LDerefb
                , TAC.tacLValue = Just $ TAC.Id dstAddress
                , TAC.tacRValue1 = Just $ TAC.Constant $ TAC.Int 0
                , TAC.tacRValue2 = Just $ TAC.Id t0
                }
        _ -> do
            srcAddress <- case getAddress rExpr of
                Temp t -> return t
                Memory t -> do
                    temp <- getNextTemp
                    generateCode $ TAC.TACCode
                        { TAC.tacOperation = TAC.Add
                        , TAC.tacLValue = Just $ TAC.Id temp
                        , TAC.tacRValue1 = Just $ TAC.Id TAC.base
                        , TAC.tacRValue2 = Just $ TAC.Id t
                        }
                    return temp
                Heap t   -> return t
            mWidth <- T.getTypeWidth $ AST.getTypeStr $ getExpr rExpr
            let width = fromMaybe 0 mWidth
            generateCode $ TAC.TACCode
                { TAC.tacOperation = TAC.MemCopy
                , TAC.tacLValue = Just $ TAC.Id dstAddress
                , TAC.tacRValue1 = Just $ TAC.Id srcAddress
                , TAC.tacRValue2 = Just $ TAC.Constant $ TAC.Int width
                }

    return $ Instruction
        { getInstruction  = astInst
        , getNextList     = []
        , getBreakList    = []
        , getContinueList = []
        }

generateCodePrintString :: AST.Instruction -> String -> MonadParser Instruction
generateCodePrintString astInst str = do
    label <- getNextLabel
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.MetaStaticStr
        , TAC.tacLValue    = Just $ TAC.Label label
        , TAC.tacRValue1   = Just $ TAC.Constant $ TAC.String str
        , TAC.tacRValue2   = Nothing
        }
    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Print
        , TAC.tacLValue    = Just $ TAC.Label label
        , TAC.tacRValue1   = Nothing
        , TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        { getInstruction  = astInst
        , getNextList     = []
        , getBreakList    = []
        , getContinueList = []
        }

generateCodeReturn :: AST.Instruction -> [Expression] -> MonadParser Instruction
generateCodeReturn astInst [] = do

    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Return
        , TAC.tacLValue    = Nothing
        , TAC.tacRValue1   = Nothing
        , TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        { getInstruction  = astInst
        , getNextList     = []
        , getBreakList    = []
        , getContinueList = []
        }

generateCodeReturn astInst [expr] = do

    t0 <- case AST.getType $ getExpr expr of
        T.CharT -> getTempFromAddress True (getAddress expr)
        _       -> getTempFromAddress False (getAddress expr)

    generateCode $ TAC.TACCode
        { TAC.tacOperation = TAC.Return
        , TAC.tacLValue    = Just $ TAC.Id t0
        , TAC.tacRValue1   = Nothing
        , TAC.tacRValue2   = Nothing
        }

    return $ Instruction
        { getInstruction  = astInst
        , getNextList     = []
        , getBreakList    = []
        , getContinueList = []
        }

generateCodeReturn _ _ = error "Multivalue return not implemented"
