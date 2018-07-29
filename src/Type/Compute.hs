-- 类型表达式类型

module Type.Compute where
import           Control.Monad.State.Strict     ( get
                                                , put
                                                )
import           Data.Functor
import           Ast
import           Helper
import           IState
import           Type.Check

checkCall :: Type -> [Type] -> Bool
checkCall (CbFunction fc pats) params = all (uncurry pormot) (zip params pats)

pormot' :: Type -> Type -> Cb Type
pormot' source target = 
  if pormot source target 
    then pure target
    else fail "类型不匹配"

-- 计算并检测表达式类型
computeType :: Expr -> Cb Type
computeType (Funcall fc params fun) = do
  funType    <- computeType fun
  paramTypes <- mapM computeType params
  case funType of
    t@(CbFunction rt pats) ->
      if checkCall t paramTypes then pure rt else fail "函数调用参数类型不匹配"
    _ -> fail "对非函数表达式进行调用"
computeType (SizeofType fc t     ) = pure CbLong
computeType (SizeofExpr fc e_    ) = pure CbLong
computeType (Assign fc lv rv     ) = do
  lt <- computeType lv
  rt <- computeType rv
  pormot' rt lt 
computeType (OpAssign fc op lv rv) = do
  rt <- computeType $ Binary NoFC op lv rv
  lt <- computeType lv
  pormot' rt lt
computeType (Binary   fc op l  r ) = do
  lt <- computeType l
  rt <- computeType r
  case checkTypeBin lt op rt of
    Left  err -> fail err
    Right t   -> pure t
computeType (Unary  fc op expr       ) = computeType expr
computeType (Prefix fc op expr       ) = computeType expr
computeType (Suffix fc op expr       ) = computeType expr
computeType (Cast   fc t  e_         ) = pure t
computeType (Cond fc cond then_ else_) = computeType then_
computeType (Address fc expr         ) = do
  t <- computeType expr
  case t of
    CbFunction{} -> pure t -- 函数类型会进行隐式指针类型转换
    _            -> pure $ CbPtr t
computeType (Dereference fc expr) = do
  t <- computeType expr
  case t of
    CbFunction{} -> pure t
    CbPtr t      -> pure t
    _            -> fail "对非指针对象解引用"
computeType (Member fc name expr) = do
  t <- computeType expr
  case findMem t name of
    Just (Param t _ _) -> pure t
    Nothing            -> fail $ "无法获取 " ++ name ++ " 成员的类型"
computeType (PtrMember fc name expr) = do
  t <- computeType expr
  case findPtrMem t name of
    Just (Param t _ _) -> pure t
    Nothing            -> fail $ "无法获取 " ++ name ++ " 成员的类型"
computeType (Arrayref fc ve   ie   ) = computeType ve
computeType (Varable  fc name type_) = pure type_
computeType IntLiteral{}             = pure $ CbConst CbInt
computeType FloatLiteral{}           = pure $ CbConst CbFloat
computeType StringLiteral{}          = pure $ CbConst $ CbPtr CbChar
computeType CharLiteral{}            = pure $ CbConst CbChar
computeType BoolLiteral{}            = pure $ CbConst CbBool
