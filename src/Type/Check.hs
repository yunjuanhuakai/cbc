{-# LANGUAGE MultiWayIf #-}

-- 类型定义的正确性以及静态类型检测
module Type.Check where

import           Ast
-- import           Helper
import qualified Data.Map.Strict               as Map
import           Control.Monad.Trans.State.Strict
                                                ( State(..)
                                                , get
                                                , put
                                                , evalState
                                                )
import           Control.Monad

----------------------- rcursive ----------------------------------

data Flag = Checking | Checked
type CheckState = Map.Map Type Flag

checkRcursiveParams :: Type -> [Type] -> State CheckState Bool
checkRcursiveParams t ts = do
    s <- get
    case Map.lookup t s of
        Just Checking -> pure False
        Just Checked  -> pure True
        Nothing       -> do
            put $ Map.insert t Checking s
            res <- foldM
                (\b t -> do
                    res <- checkRcursive t
                    pure $ b && res
                )
                True
                ts
            s <- get
            put $ Map.insert t Checked s
            pure res

checkRcursive :: Type -> State CheckState Bool
checkRcursive t@(CbStruct name params) =
    checkRcursiveParams t (fmap paramType params)
checkRcursive t@(CbUnion name params) =
    checkRcursiveParams t (fmap paramType params)
checkRcursive (CbArray t _) = checkRcursive t
checkRcursive (CbConst t  ) = checkRcursive t
checkRcursive t             = do
    s <- get
    put $ Map.insert t Checked s
    pure True

------------------------ check expr ----------------------------------

checkTypeBinByArithmetic :: Type -> Type -> Either String Type
checkTypeBinByArithmetic lt rt = if
  | isBool lt || isBool rt -> Left "无法对bool类型运用这个运算"
  | otherwise -> case typeTable !! typeIndex lt !! typeIndex rt of
    Just t  -> Right t
    Nothing -> Left "无法提升的类型"

checkTypeBinByPOrE :: Type -> Type -> Either String Type
checkTypeBinByPOrE lt rt = if
  | isPtr lt && isPtr rt -> Left "两指针之间无法运行该运算"
  | isPtr lt -> if isInteger rt then Right lt else Left "对指针类型加减必须为整形"
  | isPtr rt -> if isInteger lt then Right rt else Left "对指针类型加减必须为整形"
  | otherwise -> checkTypeBinByArithmetic lt rt

checkTypeBin :: Type -> String -> Type -> Either String Type
checkTypeBin lt op rt = if
  | op `elem` ["+", "-"] -> checkTypeBinByPOrE lt rt
  | op `elem` ["*", "/"] -> checkTypeBinByArithmetic lt rt
  | op `elem` ["<", "<=", ">", ">="] -> const CbBool <$> checkTypeBinByArithmetic lt rt
  | op `elem` ["%", "&", "|", "^", ">>", "<<"] ->  if
    | isInteger lt && isInteger rt -> checkTypeBinByArithmetic lt rt
    | otherwise -> Left "无法对非整形数字运用这个运算符"
  | op `elem` ["==", "!="] -> case typeTable !! typeIndex lt !! typeIndex rt of
    Just t  -> Right t
    Nothing -> Left "无法提升的类型"
  | op `elem` ["&&", "||"] -> if
    | isBool lt && isBool rt -> Right CbBool
    | otherwise -> Left "逻辑运算符两侧须为bool类型"
  | otherwise -> Left "未知的操作符"

checkTypeUnary :: String -> Type -> Either String Type
checkTypeUnary op t = if
    | op `elem` ["+", "-"] -> if isSigned t
        then pure t
        else Left "无法对非有符号数字调用该操作符"
    | op == "~" -> if isInteger t then pure t else Left "无法对非整数类型调用该操作符"
    | op == "!" -> if isBool t then pure t else Left "无法对非布尔类型调用该操作符"
    | op `elem` ["++", "--"] -> if isNumber t || isPtr t
        then pure t
        else Left "仅可对指针或数字类型调用该操作符"
    | otherwise -> Left "未知的操作符"

checkTypeAssign :: Type -> Type -> Either String Type
checkTypeAssign (CbConst _) _  = Left "对 const 类型的表达式赋值"
checkTypeAssign lt          rt = pormot' rt lt

checkCall :: Type -> [Type] -> Bool
checkCall (CbFunction fc pats) params = all (uncurry pormot) (zip params pats)

pormot' :: Type -> Type -> Either String Type
pormot' source target =
  if pormot source target then pure target else Left "类型不匹配"

maybeToEither :: Maybe a -> Either String a
maybeToEither (Just a) = pure a
maybeToEither Nothing  = Left "类型不匹配"

pormotTwo :: Type -> Type -> Either String Type
pormotTwo t1 t2 = maybeToEither $ typeTable !! typeIndex t1 !! typeIndex t2

-- 计算并检测表达式类型
computeType :: Expr -> Either String Type
computeType (Funcall fc params fun) = do
  funType    <- computeType fun
  paramTypes <- mapM computeType params
  case funType of
    t@(CbFunction rt pats) ->
      if checkCall t paramTypes then pure rt else Left "函数调用参数类型不匹配"
    _ -> Left "对非函数表达式进行调用"
computeType (SizeofType fc t ) = pure CbLong
computeType (SizeofExpr fc e_) = pure CbLong
computeType (Assign fc lv rv ) = if isLValue lv
  then do
    lt <- computeType lv
    rt <- computeType rv
    checkTypeAssign lt rt
  else Left "对非左值做赋值操作"
computeType (OpAssign fc op lv rv) = if isLValue lv
  then do
    rt <- computeType $ Binary NoFC op lv rv
    lt <- computeType lv
    checkTypeAssign lt rt
  else Left "对非左值做赋值操作"
computeType (Binary fc op l r) = do
  lt <- computeType l
  rt <- computeType r
  checkTypeBin lt op rt
computeType (Unary  fc op expr       ) = computeType expr >>= checkTypeUnary op
computeType (Prefix fc op expr       ) = computeType expr >>= checkTypeUnary op
computeType (Suffix fc op expr       ) = computeType expr >>= checkTypeUnary op
computeType (Cast   fc t  e_         ) = pure t
computeType (Cond fc cond then_ else_) = do
  condType <- computeType cond
  thenType <- computeType then_
  elseType <- computeType else_
  if isBool condType
    then pormotTwo thenType elseType
    else Left "作为条件的表达式并非bool类型"
computeType (Address fc expr) = if isLValue expr
  then do
    t <- computeType expr
    case t of
      CbFunction{} -> pure t -- 函数类型会进行隐式指针类型转换
      _            -> pure $ CbPtr t
  else Left "对非左值做取址操作"
computeType (Dereference fc expr) = do
  t <- computeType expr
  case t of
    CbFunction{} -> pure t
    CbPtr t      -> pure t
    _            -> Left "对非指针对象解引用"
computeType (Member fc name expr) = do
  t <- computeType expr
  case findMem t name of
    Just (Param t _ _) -> pure t
    Nothing            -> Left $ "无法获取 " ++ name ++ " 成员的类型"
computeType (PtrMember fc name expr) = do
  t <- computeType expr
  case findPtrMem t name of
    Just (Param t _ _) -> pure t
    Nothing            -> Left $ "无法获取 " ++ name ++ " 成员的类型"
computeType (Arrayref fc ve ie) = do
  vt <- computeType ve
  it <- computeType ie
  if isPtr vt && isInteger it
    then maybeToEither $ mem vt
    else Left "对非指针类型的值解引用"
computeType (Varable fc name type_) = pure type_
computeType IntLiteral{}            = pure $ CbConst CbInt
computeType FloatLiteral{}          = pure $ CbConst CbFloat
computeType StringLiteral{}         = pure $ CbConst $ CbPtr CbChar
computeType CharLiteral{}           = pure $ CbConst CbChar
computeType BoolLiteral{}           = pure $ CbConst CbBool
