{-# LANGUAGE MultiWayIf #-}
module IRGenerator where

import qualified Ast                           as A
import           IR
import           IState
import           Helper

import           Data.Maybe
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

return_ :: Maybe Expr -> Cb ()
return_ e = addStmt $ Return e

expression :: Expr -> Cb ()
expression e = addStmt $ Expression e

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
  cjump (Bin EQ_ I8 cv tv) <$> pure thenLabel <*> topBrack
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

transformStmt (A.For _ init cond next body) = do
  begLabel  <- labelGen
  bodyLabel <- labelGen
  endLabel  <- labelGen

  mapM_ transformExpr' init
  label begLabel
  if isJust cond
    then
      cjump
      <$> transformExpr' (fromJust cond)
      <*> pure bodyLabel
      <*> pure endLabel
    else pure $ jump bodyLabel
  label bodyLabel

  pushContinue begLabel
  pushBrack endLabel

  transformStmt body

  popBrack
  popContinue

  mapM_ transformExpr' next
  jump begLabel
  label endLabel

transformStmt (A.Goto  _ s ) = jump $ Label s
transformStmt (A.Label _ s ) = label $ Label s
transformStmt (A.Break    _) = topBrack >>= jump
transformStmt (A.Continue _) = topContinue >>= jump
transformStmt (A.Return _ maybeExpr) =
  mapM transformExpr' maybeExpr >>= return_
transformStmt (A.Expression _ expr) = transformExpr' expr >>= expression

transformExpr' :: A.Expr -> Cb Expr
transformExpr' e = do
  i <- get
  put $ i { level = level i + 1 }
  res <- transformExpr e
  put $ i { level = level i - 1 }
  return res

queryType' :: A.Expr -> Cb Type
queryType' e = do
  t <- queryType e
  pure $ transformType t

queryTypeTwo' :: A.Expr -> A.Expr -> Cb Type
queryTypeTwo' le re = do
  t <- queryTypeTwo le re
  pure $ transformType t

transformBin :: A.Expr -> String -> A.Expr -> Cb Expr
transformBin lae op rae = do
  lt <- queryType lae
  rt <- queryType rae

  le <- transformExpr' lae
  re <- transformExpr' rae
  if | A.isPtr lt && A.isPtr rt && op == "-" -> 
       pure $ Bin DIV UI64 (Bin SUB UI64 le re) (I $ A.sizeof lt)
     | A.isPtr lt -> 
       pure $ Bin (transformOp op) UI64 le (Bin MUL UI64 re $ I (A.sizeof lt))
     | A.isPtr rt -> 
       pure $ Bin (transformOp op) UI64 (Bin MUL UI64 le $ I (A.sizeof rt)) re
     | otherwise -> 
       Bin (transformOp op) <$> queryTypeTwo' lae rae <*> pure le <*> pure re

transformExpr :: A.Expr -> Cb Expr
transformExpr (A.Unary _ "+" expr) = transformExpr' expr
transformExpr (A.Unary _ "-" expr) = Uni UMINUS <$> queryType' expr <*> transformExpr' expr
transformExpr (A.Binary _ op le re) = transformBin le op re