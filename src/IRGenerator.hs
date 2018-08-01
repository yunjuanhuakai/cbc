module IRGenerator where

import qualified Ast                           as A
import           IR
import           IState
import           Helper

import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.State.Strict     ( StateT(..)
                                                , evalStateT
                                                , get
                                                , put
                                                )

addStmt :: Stmt -> Cb ()
addStmt s = do
  i <- get
  put $ i { stmts = s : stmts i }

cjump :: Expr -> Stmt -> Stmt -> Cb ()
cjump cond thenLabel elseLabel = addStmt $ CJump cond thenLabel elseLabel

jump :: Stmt -> Cb ()
jump = addStmt . Jump

label :: Stmt -> Cb ()
label = addStmt

labelGen :: Cb Stmt
labelGen = do
  i <- get
  let res = Label $ show $ labelCount i
  put $ i { labelCount = labelCount i + 1 }
  return res

pushBrack :: Stmt -> Cb ()
pushBrack label = do
  i <- get
  put $ i { breakStack = label : breakStack i }

pushContinue :: Stmt -> Cb ()
pushContinue label = do
  i <- get
  put $ i { continueStack = label : continueStack i }

popBrack :: Cb Stmt
popBrack = do
  i <- get
  put $ i { breakStack = tail $ breakStack i }
  return $ head $ breakStack i

popContinue :: Cb Stmt
popContinue = do
  i <- get
  put $ i { continueStack = tail $ continueStack i }
  return $ head $ continueStack i

topBrack :: Cb Stmt
topBrack = head . breakStack <$> get

topContinue :: Cb Stmt
topContinue = head . continueStack <$> get

transformCase :: Expr -> A.Stmt -> Cb ()
transformCase cv (A.Case _ expr body) = do
  thenLabel <- labelGen

  tv        <- transformExpr' expr
  cjump (Bin EQ_ cv tv) <$> pure thenLabel <*> topBrack
  label thenLabel
  transformStmt body

transformStmt :: A.Stmt -> Cb ()
transformStmt (A.Switch _ cond cases default_) = do
  endLabel <- labelGen
  cv       <- transformExpr' cond

  pushBrack endLabel
  forM_ cases (transformCase cv)
  mapM_ transformStmt default_
  popBrack
  label endLabel

transformStmt A.Case{}                       = fail "出现位置错误的case"
transformStmt (A.Default _ body            ) = transformStmt body

transformStmt (A.If _ cond thenBody Nothing) = do
  thenLabel <- labelGen
  endLabel  <- labelGen

  cjump <$> transformExpr' cond <*> pure thenLabel <*> pure endLabel
  addStmt thenLabel
  transformStmt thenBody
  addStmt endLabel

transformStmt (A.If _ cond thenBody (Just elseBody)) = do
  thenLabel <- labelGen
  elseLabel <- labelGen
  endLabel  <- labelGen

  cjump <$> transformExpr' cond <*> pure thenLabel <*> pure elseLabel
  label thenLabel
  transformStmt thenBody
  jump endLabel
  label elseLabel
  transformStmt elseBody
  label endLabel

transformStmt (A.While _ cond body) = do
  begLabel  <- labelGen
  bodyLabel <- labelGen
  endLabel  <- labelGen

  label begLabel
  cjump <$> transformExpr cond <*> pure bodyLabel <*> pure endLabel
  label bodyLabel

  pushContinue begLabel
  pushBrack endLabel

  transformStmt body
  
  popBrack
  popContinue

  jump begLabel
  label endLabel

transformStmt (A.DoWhile _ body cond) = do
  begLabel  <- labelGen
  bodyLabel <- labelGen
  endLabel  <- labelGen

  pushContinue begLabel
  pushBrack endLabel

  label begLabel
  label bodyLabel
  transformStmt body

  popBrack
  popContinue

  cjump <$> transformExpr cond <*> pure bodyLabel <*> pure endLabel
  label endLabel

transformStmt (A.For _ init cond next body) = undefined

transformStmt (A.Break    _) = topBrack >>= jump
transformStmt (A.Continue _) = topContinue >>= jump

transformExpr' :: A.Expr -> Cb Expr
transformExpr' e = do
  i <- get
  put $ i { level = level i + 1 }
  res <- transformExpr e
  put $ i { level = level i - 1 }
  return res

transformExpr :: A.Expr -> Cb Expr
transformExpr = undefined
