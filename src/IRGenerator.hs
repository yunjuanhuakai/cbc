{-# LANGUAGE MultiWayIf #-}
module IRGenerator where

import qualified Ast                           as A
import           IR
import           IState
import           Helper
import           Type.Check

import           Data.Char
import qualified Data.Vector                   as V
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.State.Strict     ( StateT(..)
                                                , evalStateT
                                                , get
                                                , put
                                                )


transformDecl :: Int -> A.Declaration -> Cb IR
transformDecl = undefined

-----------------------------------------------------------------------

queryType :: A.Expr -> Cb A.Type
queryType e = do
  ist <- get
  case Map.lookup e (exprTypes ist) of
    Just t  -> pure t
    Nothing -> computeType e

queryTypeTwo :: A.Expr -> A.Expr -> Cb A.Type
queryTypeTwo le re = do
  lt <- queryType le
  rt <- queryType re
  pormotTwo lt rt

offset' :: A.Expr -> Cb Int
offset' (A.Member _ name expr) = do
  t <- queryType expr
  case A.offset t name of
    Just n -> pure n
    Nothing -> fail "错误的成员变量偏移量计算"
offset' _ = fail "无法计算偏移量"

tmpVar :: Type -> Cb Expr
tmpVar t = do
  ist <- get
  let id = declCount ist + 1
  pure $ Var ("#tmp" ++ show id) t

addStmt :: Stmt -> Cb ()
addStmt s = do
  i <- get
  let cur = curIR i
  let irMap = ir i
  let ir' = irMap Map.! cur
  let nIr = ir' { stmts = V.snoc (stmts ir') s }
  put $ i { ir = Map.insert cur nIr irMap}

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

assign :: Expr -> Expr -> Cb ()
assign le re = addStmt $ Assign le re

labelGen :: String -> Cb Stmt
labelGen str = do
  i <- get
  let res = Label (str ++ show (labelCount i))
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
  thenLabel <- labelGen "then"

  tv        <- transformExpr' expr
  lab       <- topBrack
  cjump (Bin EQ_ I8 cv tv) thenLabel lab
  label thenLabel
  transformStmt body

transformStmt :: A.Stmt -> Cb ()
transformStmt (A.Switch _ cond cases default_) = do
  endLabel <- labelGen "end"
  cv       <- transformExpr' cond

  pushBrack endLabel
  forM_ cases (transformCase cv)
  mapM_ transformStmt default_
  popBrack
  label endLabel

transformStmt A.Case{}                       = fail "出现位置错误的case"
transformStmt (A.Default _ body            ) = transformStmt body

transformStmt (A.If _ cond thenBody Nothing) = do
  thenLabel <- labelGen "then"
  endLabel  <- labelGen "end"

  e <- transformExpr' cond
  cjump e thenLabel endLabel
  addStmt thenLabel
  transformStmt thenBody
  addStmt endLabel

transformStmt (A.If _ cond thenBody (Just elseBody)) = do
  thenLabel <- labelGen "then"
  elseLabel <- labelGen "else"
  endLabel  <- labelGen "end"

  e <- transformExpr' cond
  cjump e thenLabel elseLabel
  label thenLabel
  transformStmt thenBody
  jump endLabel
  label elseLabel
  transformStmt elseBody
  label endLabel

transformStmt (A.While _ cond body) = do
  begLabel  <- labelGen "whildBeg"
  bodyLabel <- labelGen "whileBody"
  endLabel  <- labelGen "whildEnd"

  label begLabel
  e <- transformExpr' cond
  cjump e bodyLabel endLabel
  label bodyLabel

  pushContinue begLabel
  pushBrack endLabel

  transformStmt body

  popBrack
  popContinue

  jump begLabel
  label endLabel

transformStmt (A.DoWhile _ body cond) = do
  begLabel  <- labelGen "beg"
  bodyLabel <- labelGen "body"
  endLabel  <- labelGen "end"

  pushContinue begLabel
  pushBrack endLabel

  label begLabel
  label bodyLabel
  transformStmt body

  popBrack
  popContinue

  e <- transformExpr' cond
  cjump e bodyLabel endLabel
  label endLabel

transformStmt (A.For _ init cond next body) = do
  begLabel  <- labelGen "forBeg"
  bodyLabel <- labelGen "forBody"
  endLabel  <- labelGen "forEnd"

  mapM_ transformExpr' init
  label begLabel
  if isJust cond
    then do 
      e <- transformExpr' (fromJust cond)
      cjump e bodyLabel endLabel
    else jump bodyLabel
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
transformStmt (A.Block _ _ stmts) = forM_ (reverse stmts) transformStmt 

transformExpr' :: A.Expr -> Cb Expr
transformExpr' e = do
  i <- get
  put $ i { level = level i + 1 }
  res <- transformExpr e
  ni <- get
  put $ ni { level = level ni - 1 }
  return res

queryType' :: A.Expr -> Cb Type
queryType' e = do
  t <- queryType e
  pure $ transformType t

queryTypeTwo' :: A.Expr -> A.Expr -> Cb Type
queryTypeTwo' le re = do
  t <- queryTypeTwo le re
  pure $ transformType t

isStatement :: Cb Bool
isStatement = fmap (\ist -> level ist == 1) get

transformBin :: Expr -> A.Type -> String -> Expr -> A.Type -> Cb Expr
transformBin le lt op re rt = do
  lve <- toLv le $ transformType lt
  rve <- toLv re $ transformType rt
  if | A.isPtr lt && A.isPtr rt && op == "-" -> do
        v <- tmpVar UI64
        assign v (Bin SUB UI64 lve rve)
        pure $ Bin DIV UI64 v (I $ A.sizeof lt)
      | A.isPtr lt -> do
        v <- tmpVar UI64
        assign v (Bin MUL UI64 rve $ I (A.sizeof lt))
        pure $ Bin (transformOp op) UI64 lve v
      | A.isPtr rt -> do
        v <- tmpVar UI64
        assign v (Bin MUL UI64 lve $ I (A.sizeof rt))
        pure $ Bin (transformOp op) UI64 v rve
      | otherwise -> do
        t <- pormotTwo lt rt
        pure $ Bin (transformOp op) (transformType t) lve rve

toLv :: Expr -> Type -> Cb Expr
toLv e@Var{} _ = pure e
toLv e@Mem{} _ = pure e
toLv e t = do
  tmp <- tmpVar t
  assign tmp e
  pure tmp

transformOpAssign :: A.Expr -> String -> A.Expr -> Cb Expr
transformOpAssign lae op rae = do
  lt  <- queryType lae
  rt  <- queryType rae

  le <- transformExpr' lae
  re <- transformExpr' rae

  a <- tmpVar UI64
  tmp <- tmpVar $ transformType lt
  assign a (Addr le)
  assign tmp le
  bin <- transformBin tmp lt op re rt
  assign (Mem a) bin
  pure bin

transformExpr :: A.Expr -> Cb Expr
transformExpr (A.Unary _ "+" expr) = transformExpr' expr
transformExpr (A.Unary _ "-" expr) =
  Uni UMINUS <$> queryType' expr <*> transformExpr' expr
transformExpr (  A.Binary _ op le re ) = do
  lt <- queryType le
  rt <- queryType re

  lre <- transformExpr' le
  rre <- transformExpr' re
  transformBin lre lt op rre rt
transformExpr e@(A.Member _ name expr) = do
  v   <- toLv <$> transformExpr' expr
  ptr <- tmpVar UI64
  Mem <$> (Bin ADD UI64 ptr <$> fmap I (offset' e))
transformExpr e@(A.PtrMember _ name expr) =
  Mem <$> (Bin ADD UI64 <$> transformExpr' expr <*> fmap I (offset' e))
transformExpr (A.Assign _ le re) = do
  b   <- isStatement
  lhs <- transformExpr' le
  rhs <- transformExpr' re
  if b
    then do assign lhs rhs; pure lhs
    else do
      -- lhs 在赋值表达式为子表达式的情况下，使用两次可能会导致副作用从而影响语义
      -- 因此引入临时变量，将最终结果记录下来
      -- cont(lhs = rhs) -> tmp = rhs; lhs = tmp; cont(tmp)
      tmp <- queryType re >>= tmpVar . transformType
      assign tmp rhs
      assign lhs tmp
      pure tmp
transformExpr (A.OpAssign _ op le re) = transformOpAssign le op re
transformExpr (A.Suffix _ op expr   ) = do
  b <- isStatement
  e <- transformExpr' expr
  if b
    then transformOpAssign expr
                           (if op == "++" then "+" else "-")
                           (A.IntLiteral A.NoFC 1)
    else do
      -- a 存 expr 的引用，tmp 记录 expr 的结果
      -- cont(expr++) -> a = &expr; tmp = *a; *a = *a + 1; cont(tmp)
      t   <- queryType expr
      a   <- tmpVar UI64
      tmp <- tmpVar $ transformType t
      assign a       (Addr e)
      assign tmp     (Mem a)
      assign (Mem a) (Bin ADD UI64 (Mem a) (I 1))
      pure tmp
transformExpr (A.Prefix _ op expr) = transformOpAssign
  expr
  (if op == "++" then "+" else "-")
  (A.IntLiteral A.NoFC 1)
transformExpr (A.Cast _ t expr) = do
  ist <- get
  put $ ist { exprTypes = Map.insert expr t $ exprTypes ist }
  transformExpr' expr
transformExpr (A.Cond _ cond then_ else_) = do
  thenLabel <- labelGen "condThen"
  elseLabel <- labelGen "condElse"
  endLabel  <- labelGen "condEnd"

  tmp       <- queryType then_ >>= tmpVar . transformType
  e <- transformExpr' cond 
  cjump e thenLabel elseLabel
  label thenLabel
  transformExpr' then_ >>= assign tmp
  jump endLabel
  label elseLabel
  transformExpr' else_ >>= assign tmp
  label endLabel
  pure tmp
transformExpr (A.Address     _ expr) = Addr <$> transformExpr' expr
transformExpr (A.Dereference _ expr) = Mem <$> transformExpr' expr
transformExpr (A.Arrayref _ e ie   ) = do
  array <- transformExpr' e
  index <- transformExpr' ie

  t     <- A.elemType <$> queryType e
  tmp   <- tmpVar UI64
  assign tmp $ Bin ADD UI64 array index
  pure $ Mem tmp
transformExpr (A.Decl _ name id) = do
  ist <- get
  undefined
transformExpr (A.Seq _ exprs   ) = last <$> mapM transformExpr' exprs
transformExpr (A.Funcall _ params fun) =
  Call <$> transformExpr' fun <*> mapM transformExpr' params
transformExpr (A.IntLiteral    _ i) = pure $ I i
transformExpr (A.FloatLiteral  _ f) = pure $ F f
transformExpr (A.StringLiteral _ s) = pure $ Str s
transformExpr (A.CharLiteral   _ c) = pure $ I $ digitToInt c
transformExpr (A.BoolLiteral   _ b) = pure $ I $ if b then 1 else 0
