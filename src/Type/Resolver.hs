-- 替换CbUnknown，同时调用Type.Check进行检测

module Type.Resolver where

import qualified Data.Map.Strict               as Map

import           Ast
import           Helper
import           IState
import           Type.Check
import           Control.Monad
import           Control.Monad.Trans.State.Strict
                                                ( get
                                                , put
                                                )

checkRcursive' :: Declaration -> Cb Declaration
checkRcursive' decl = do
  res <- checkRcursive $ declToType decl
  if res then pure decl else fail "存在循环定义的结构体"

unit :: Unit -> Cb Unit
unit (Unit decls) = Unit <$> mapM decl decls

param :: Param -> Cb Param
param (Param t name) = Param <$> type' t <*> pure name

decl :: Declaration -> Cb Declaration
decl (Variable fc t name init) =
  Variable fc <$> type' t <*> pure name <*> mapM exprAndCheck init >>= checkVar
decl (Function fc rt name params body) =
  Function fc
    <$> type' rt
    <*> pure name
    <*> mapM param params
    <*> stmt body
    >>= checkFunction
decl (UndefineFunction fc rt name params) =
  UndefineFunction fc <$> type' rt <*> pure name <*> mapM param params
decl (Struct fc name params) =
  Struct fc name <$> mapM param params >>= checkRcursive'
decl (Union fc name params) =
  Union fc name <$> mapM param params >>= checkRcursive'
decl (Typedef fc t name) = Typedef fc <$> type' t <*> pure name

stmt :: Stmt -> Cb Stmt
stmt (If fc expr then_ else_) =
  If fc <$> (exprAndCheck expr >>= checkCond) <*> stmt then_ <*> mapM stmt else_
stmt (Switch fc expr cases default_) =
  Switch fc <$> exprAndCheck expr <*> mapM stmt cases <*> mapM stmt default_
stmt (Case fc expr body) =
  Case fc <$> (exprAndCheck >=> checkCase) expr <*> stmt body
stmt (For fc init cond next body) =
  For fc
    <$> mapM exprAndCheck init
    <*> mapM (exprAndCheck >=> checkCond) cond
    <*> mapM exprAndCheck next
    <*> stmt body
stmt (While fc cond body) =
  While fc <$> (exprAndCheck cond >>= checkCond) <*> stmt body
stmt (DoWhile fc body cond) =
  DoWhile fc <$> stmt body <*> (exprAndCheck cond >>= checkCond)
stmt (Block fc decls stmts) = Block fc <$> mapM decl decls <*> mapM stmt stmts
stmt (Return     fc expr  ) = Return fc <$> mapM exprAndCheck expr
stmt (Expression fc expr  ) = Expression fc <$> exprAndCheck expr
stmt s                      = pure s

exprAndCheck :: Expr -> Cb Expr
exprAndCheck expr = do
  res <- expr' expr
  t   <- computeType res
  cst <- get
  put $ cst { exprTypes = Map.insert res t $ exprTypes cst }
  pure res

expr' :: Expr -> Cb Expr
expr' (Funcall fc params fun) =
  Funcall fc <$> mapM exprAndCheck params <*> exprAndCheck fun
expr' (SizeofType fc t) = SizeofType fc <$> type' t
expr' (SizeofExpr fc e) = SizeofExpr fc <$> exprAndCheck e
expr' (Assign fc lv rv) = Assign fc <$> exprAndCheck lv <*> exprAndCheck rv
expr' (OpAssign fc s lv rv) =
  OpAssign fc s <$> exprAndCheck lv <*> exprAndCheck rv
expr' (Binary fc op l r ) = Binary fc op <$> exprAndCheck l <*> exprAndCheck r
expr' (Unary  fc op expr) = Unary fc op <$> exprAndCheck expr
expr' (Prefix fc op expr) = Prefix fc op <$> exprAndCheck expr
expr' (Suffix fc op expr) = Suffix fc op <$> exprAndCheck expr
expr' (Cast   fc t  e   ) = Cast fc <$> type' t <*> pure e
expr' (Cond fc cond then_ else_) =
  Cond fc <$> exprAndCheck cond <*> exprAndCheck then_ <*> exprAndCheck else_
expr' (Address     fc expr   ) = Address fc <$> exprAndCheck expr
expr' (Dereference fc expr   ) = Dereference fc <$> exprAndCheck expr
expr' (Member    fc name expr) = Member fc name <$> exprAndCheck expr
expr' (PtrMember fc name expr) = PtrMember fc name <$> exprAndCheck expr
expr' (Arrayref fc ve ie) = Arrayref fc <$> exprAndCheck ve <*> exprAndCheck ie
expr' (Decl fc name id) = do
  ist <- get
  h  <- findHandlerById id
  t  <- type' $ handlerType h
  put $ ist { handlers = Map.insert id (h { handlerType = t }) $ handlers ist }
  pure $ Decl fc name id
expr' e = pure e

type' :: Type -> Cb Type
type' (CbUnknown name) = do
  cst <- get
  case Map.lookup name (types cst) of
    Just t  -> pure t
    Nothing -> fail $ "未知的类型名" ++ name
type' t = pure t
