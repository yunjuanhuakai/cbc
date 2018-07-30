-- 替换CbUnknown，同时调用Type.Check进行检测

module Type.Resolver where

import qualified Data.Map.Strict               as Map

import           Ast
import           Helper
import           IState
import           Type.Check
import           Control.Monad.Trans.State.Strict
                                                ( get
                                                , put
                                                , evalState
                                                )

checkRcursive' :: Declaration -> Cb Declaration
checkRcursive' decl = do
  let res = evalState (checkRcursive $ declToType decl) Map.empty
  if res then pure decl else fail "存在循环定义的结构体"

unit :: Unit -> Cb Unit
unit (Unit imps decls) = Unit imps <$> mapM decl decls

param :: Param -> Cb Param
param (Param t name init) =
  Param <$> type' t <*> pure name <*> mapM exprAndCheck' init

decl :: Declaration -> Cb Declaration
decl (Variable fc t name init) =
  Variable fc <$> type' t <*> pure name <*> mapM exprAndCheck' init
decl (Function fc rt name params body) =
  Function fc
    <$> type' rt
    <*> pure name
    <*> mapM param params
    <*> stmt body
decl (UndefineFunction fc rt name params) =
  UndefineFunction fc <$> type' rt <*> pure name <*> mapM param params
decl (Struct fc name params) =
  Struct fc name <$> mapM param params >>= checkRcursive'
decl (Union fc name params) =
  Union fc name <$> mapM param params >>= checkRcursive'
decl (Typedef fc t name) = Typedef fc <$> type' t <*> pure name

checkCond :: Expr -> Cb Expr
checkCond e = do
  (e, t) <- exprAndCheck e
  if isBool t
    then pure e
    else fail "作为条件的表达式不是bool类型的"

checkCase :: Expr -> Cb Expr
checkCase e = do
  (e, t) <- exprAndCheck e
  if pormot t CbULong 
    then pure e
    else fail "作为 case 语句的匹配值不符合为整数的限制"

stmt :: Stmt -> Cb Stmt
stmt (If fc expr then_ else_) = If fc <$> checkCond expr <*> stmt then_ <*> mapM stmt else_
stmt (Switch fc expr stmts) =
  Switch fc <$> exprAndCheck' expr <*> mapM stmt stmts
stmt (Case fc expr body) = Case fc <$> checkCase expr <*> stmt body
stmt (For fc init cond next body) =
  For fc
    <$> stmt init
    <*> mapM checkCond cond
    <*> stmt next
    <*> stmt body
stmt (While fc cond body) =
  While fc <$> checkCond cond <*> stmt body
stmt (DoWhile fc body cond) =
  DoWhile fc <$> stmt body <*> checkCond cond
stmt (Block fc decls stmts) =
  Block fc <$> mapM decl decls <*> mapM stmt stmts
stmt (Return     fc expr) = Return fc <$> mapM exprAndCheck' expr
stmt (Expression fc expr) = Expression fc <$> exprAndCheck' expr
stmt s                    = pure s

exprAndCheck :: Expr -> Cb (Expr, Type)
exprAndCheck expr = do
  ist <- get
  e   <- expr' expr
  case computeType e of
    Right t   -> pure (e, t)
    Left  err -> fail err

exprAndCheck' :: Expr -> Cb Expr
exprAndCheck' expr = fst <$> exprAndCheck expr

expr' :: Expr -> Cb Expr
expr' (Funcall fc params fun) =
  Funcall fc <$> mapM exprAndCheck' params <*> exprAndCheck' fun
expr' (SizeofType fc t) = SizeofType fc <$> type' t
expr' (SizeofExpr fc e) = SizeofExpr fc <$> exprAndCheck' e
expr' (Assign fc lv rv) = Assign fc <$> exprAndCheck' lv <*> exprAndCheck' rv
expr' (OpAssign fc s lv rv) =
  OpAssign fc s <$> exprAndCheck' lv <*> exprAndCheck' rv
expr' (Binary fc op l r ) = Binary fc op <$> exprAndCheck' l <*> exprAndCheck' r
expr' (Unary  fc op expr) = Unary fc op <$> exprAndCheck' expr
expr' (Prefix fc op expr) = Prefix fc op <$> exprAndCheck' expr
expr' (Suffix fc op expr) = Suffix fc op <$> exprAndCheck' expr
expr' (Cast   fc t  e   ) = Cast fc <$> type' t <*> pure e
expr' (Cond fc cond then_ else_) =
  Cond fc <$> exprAndCheck' cond <*> exprAndCheck' then_ <*> exprAndCheck' else_
expr' (Address     fc expr   ) = Address fc <$> exprAndCheck' expr
expr' (Dereference fc expr   ) = Dereference fc <$> exprAndCheck' expr
expr' (Member    fc name expr) = Member fc name <$> exprAndCheck' expr
expr' (PtrMember fc name expr) = PtrMember fc name <$> exprAndCheck' expr
expr' (Arrayref fc ve ie) = Arrayref fc <$> exprAndCheck' ve <*> exprAndCheck' ie
expr' (Varable   fc name t   ) = Varable fc name <$> type' t
expr' e                        = pure e

type' :: Type -> Cb Type
type' (CbUnknown name) = do
  ist <- get
  case Map.lookup name (types ist) of
    Just t  -> pure t
    Nothing -> fail $ "未知的类型名" ++ name
type' t = pure t
