{-# LANGUAGE MultiWayIf #-}

-- 类型表达式类型并检测有效性

module Type.Compute where
import           Control.Monad.State.Strict     ( get
                                                , put
                                                )
import           Data.Functor
import           Ast
import           Helper
import           IState

computeTypeBinByPOrE :: Type -> Type -> Either String Type
computeTypeBinByPOrE CbPtr{} CbPtr{} = Left "两指针之间不能发生运算"
computeTypeBinByPOrE (CbPtr t) it =
  if isInteger it then Right t else Left "对指针加了一个非整数"
computeTypeBinByPOrE it (CbPtr t) =
  if isInteger it then Right t else Left "对指针加了一个非整数"
computeTypeBinByPOrE lt rt = case typeTable !! typeIndex lt !! typeIndex rt of
  Just t  -> Right t
  Nothing -> fail "无法提升的类型"

computeTypeBin :: Type -> String -> Type -> Either String Type
computeTypeBin lt op rt = if
  | op `elem` ["+", "-"]             -> undefined
  | op `elem` ["*", "/"]             -> undefined
  | op `elem` ["<", "<=", ">", ">="] -> undefined
  | op `elem` ["%", "&", "|", "^", ">>", "<<"] -> undefined
  | op `elem` ["==", "!="]           -> undefined
  | op `elem` ["&&", "||"]           -> undefined
  | otherwise                        -> undefined

computeType :: Expr -> Cb Type
computeType (Funcall fc params fun) = do
  funType <- computeType fun
  case funType of
    CbFunction rt pats -> pure rt
    _                  -> fail "对非函数表达式进行调用"
computeType (SizeofType fc t     ) = pure CbLong
computeType (SizeofExpr fc e_    ) = pure CbLong
computeType (Assign fc lv rv     ) = computeType lv
computeType (OpAssign fc op lv rv) = computeType lv
computeType (Binary   fc op l  r ) = do
  lt <- computeType l
  rt <- computeType r
  case computeTypeBin lt op rt of
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
    CbFunction{} -> pure t
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
  case findMem t name of
    Just (Param t _ _) -> pure t
    Nothing            -> fail $ "无法获取 " ++ name ++ " 成员的类型"
computeType (Arrayref fc ve   ie   ) = computeType ve
computeType (Varable  fc name type_) = pure type_
computeType IntLiteral{}             = pure $ CbConst CbInt
computeType FloatLiteral{}           = pure $ CbConst CbFloat
computeType StringLiteral{}          = pure $ CbConst $ CbPtr CbChar
computeType CharLiteral{}            = pure $ CbConst CbChar
computeType BoolLiteral{}            = pure $ CbConst CbBool
