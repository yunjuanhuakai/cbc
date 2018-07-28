-- 类型计算

module Type.Compute where
import qualified Data.Map.Strict               as Map
import           Control.Monad.State.Strict     ( get
                                                , put
                                                )
import           Data.Functor
import           Ast
import           Helper
import           IState

putType :: Expr -> Type -> Cb ()
putType e t = do
  ist <- get
  put $ ist { exprType = Map.insert e t (exprType ist) }

getType :: Expr -> Cb Type
getType e = do
  ist <- get
  pure $ exprType ist Map.! e

putType' :: Expr -> Expr -> Cb ()
putType' oe e = do
  t <- getType oe
  putType e t

computeType :: Expr -> Cb ()
computeType e@(Funcall fc params fun) = do
  computeType fun
  funType <- getType fun
  case funType of
    CbFunction rt pats -> putType e rt
    _                  -> fail "对非函数表达式进行调用"
computeType e@(SizeofType fc t ) = putType e CbLong
computeType e@(SizeofExpr fc e_) = putType e CbLong
computeType e@(Assign fc lv rv ) = do
  computeType lv
  putType' lv e
computeType e@(OpAssign fc op lv rv) = do
  computeType lv
  putType' lv e
computeType e@(Binary fc op l r        ) = pure ()
computeType e@(Unary  fc op expr       ) = pure ()
computeType e@(Prefix fc op expr       ) = pure ()
computeType e@(Suffix fc op expr       ) = pure ()
computeType e@(Cast   fc t  e_         ) = pure ()
computeType e@(Cond fc cond then_ else_) = pure ()
computeType e@(Address     fc expr     ) = pure ()
computeType e@(Dereference fc expr     ) = pure ()
computeType e@(Member    fc name expr  ) = pure ()
computeType e@(PtrMember fc name expr  ) = pure ()
computeType e@(Arrayref  fc ve   ie    ) = pure ()
computeType e                            = pure ()
