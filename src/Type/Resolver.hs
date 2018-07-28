-- 替换CbUnknown，同时调用Type.Check进行检测

module Type.Resolver where

import qualified Data.Map.Strict               as Map

import           Ast
import           Helper
import           IState
import           Type.Check
import           Control.Monad.Trans.State.Strict
                                                ( get
                                                , evalState
                                                )

checkRcursive' :: Declaration -> Cb Declaration
checkRcursive' decl = do
  let res = evalState (checkRcursive $ declToType decl) Map.empty
  if res then pure decl else fail "存在循环定义的结构体"

unit :: Unit -> Cb Unit
unit (Unit imps decls) = Unit imps <$> mapM decl decls

param :: Param -> Cb Param
param (Param t name init) = Param <$> type' t <*> pure name <*> mapM expr' init

decl :: Declaration -> Cb Declaration
decl (Variable fc t name init) =
  Variable fc <$> type' t <*> pure name <*> mapM expr' init
decl (Function fc rt name params body) =
  Function fc <$> type' rt <*> pure name <*> mapM param params <*> stmt body
decl (UndefineFunction fc rt name params) =
  UndefineFunction fc <$> type' rt <*> pure name <*> mapM param params
decl (Struct fc name params) =
  Struct fc name <$> mapM param params >>= checkRcursive'
decl (Union fc name params) =
  Union fc name <$> mapM param params >>= checkRcursive'
decl (Typedef fc t name) = Typedef fc <$> type' t <*> pure name

stmt :: Stmt -> Cb Stmt
stmt (If fc expr then_ else_) =
  If fc <$> expr' expr <*> stmt then_ <*> mapM stmt else_
stmt (Switch fc expr stmts) = Switch fc <$> expr' expr <*> mapM stmt stmts
stmt (Case   fc expr body ) = Case fc <$> expr' expr <*> stmt body
stmt (For fc init cond next body) =
  For fc <$> stmt init <*> mapM expr' cond <*> stmt next <*> stmt body
stmt (While   fc cond  body ) = While fc <$> expr' cond <*> stmt body
stmt (DoWhile fc body  cond ) = DoWhile fc <$> stmt body <*> expr' cond
stmt (Block   fc decls stmts) = Block fc <$> mapM decl decls <*> mapM stmt stmts
stmt (Return     fc expr    ) = Return fc <$> mapM expr' expr
stmt (Expression fc expr    ) = Expression fc <$> expr' expr
stmt s                        = pure s

expr' :: Expr -> Cb Expr
expr' (Funcall fc params fun) = Funcall fc <$> mapM expr' params <*> expr' fun
expr' (SizeofType fc t      ) = SizeofType fc <$> type' t
expr' (SizeofExpr fc e      ) = SizeofExpr fc <$> expr' e
expr' (Assign fc lv rv      ) = Assign fc <$> expr' lv <*> expr' rv
expr' (OpAssign fc s  lv rv ) = OpAssign fc s <$> expr' lv <*> expr' rv
expr' (Binary   fc op l  r  ) = Binary fc op <$> expr' l <*> expr' r
expr' (Unary  fc op expr    ) = Unary fc op <$> expr' expr
expr' (Prefix fc op expr    ) = Prefix fc op <$> expr' expr
expr' (Suffix fc op expr    ) = Suffix fc op <$> expr' expr
expr' (Cast   fc t  e       ) = Cast fc <$> type' t <*> pure e
expr' (Cond fc cond then_ else_) =
  Cond fc <$> expr' cond <*> expr' then_ <*> expr' else_
expr' (Address     fc expr   ) = Address fc <$> expr' expr
expr' (Dereference fc expr   ) = Dereference fc <$> expr' expr
expr' (Member    fc name expr) = Member fc name <$> expr' expr
expr' (PtrMember fc name expr) = PtrMember fc name <$> expr' expr
expr' (Arrayref  fc ve   ie  ) = Arrayref fc <$> expr' ve <*> expr' ie
expr' e                        = pure e

type' :: Type -> Cb Type
type' (CbUnknown name) = do
  ist <- get
  case Map.lookup name (types ist) of
    Just t  -> pure t
    Nothing -> fail $ "未知的类型名" ++ name
type' t = pure t
