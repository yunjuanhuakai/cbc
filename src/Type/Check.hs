{-# LANGUAGE MultiWayIf #-}

-- 类型定义的正确性以及静态类型检测
module Type.Check where

import           Ast
import           Type.State
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Functor
import           Control.Monad.Trans.State.Strict
                                                ( State(..)
                                                , get
                                                , put
                                                , evalState
                                                )
import           Control.Monad

---------------------- check other -------------------------------

checkCond :: Expr -> CbCheck Expr
checkCond e = do
  t <- fmap (\s -> exprTypes s Map.! e) get
  if isBool t then pure e else fail "作为条件的表达式不是bool类型的"

checkCase :: Expr -> CbCheck Expr
checkCase e = do
  t <- fmap (\s -> exprTypes s Map.! e) get
  if pormot t CbULong then pure e else fail "作为 case 语句的匹配值不符合为整数的限制"


checkVar :: Declaration -> CbCheck Declaration
checkVar d@(Variable fc t name init) = if isJust init
  then do
    vt <- fmap (\s -> exprTypes s Map.! fromJust init) get
    if pormot vt t then pure d else fail "初始化语句与变量类型不符"
  else pure d

checkReturn :: Type -> [Stmt] -> CbCheck ()
checkReturn CbVoid []                  = pure ()
checkReturn t      []                  = fail "没有找到必要的返回值类型"
checkReturn t      [Return _ Nothing ] = pure ()
checkReturn t      [Return _ (Just e)] = computeType e
  >>= \vt -> if pormot vt t then pure () else fail "函数return语句的类型与返回值不符"
checkReturn t (stmt : stmts) = checkReturn t stmts

checkFunction :: Declaration -> CbCheck Declaration
checkFunction d@(Function fc rt name params body) = case body of
  (Block _ _ stmts) -> checkReturn rt stmts $> d
  _                 -> fail "函数定义的结构异常"


----------------------- rcursive ----------------------------------

checkRcursiveParams :: Type -> [Type] -> CbCheck Bool
checkRcursiveParams t ts = do
    cst <- get
    let s = rcursive cst
    case Map.lookup t s of
        Just Checking -> pure False
        Just Checked  -> pure True
        Nothing       -> do
          put $ cst { rcursive = Map.insert t Checking s }
          res <- foldM
              (\b t -> do
                  res <- checkRcursive t
                  pure $ b && res
              )
              True
              ts
          cst <- get
          put $ cst { rcursive = Map.insert t Checking $ rcursive cst }
          pure res

checkRcursive :: Type -> CbCheck Bool
checkRcursive t@(CbStruct name params) =
    checkRcursiveParams t (fmap paramType params)
checkRcursive t@(CbUnion name params) =
    checkRcursiveParams t (fmap paramType params)
checkRcursive (CbArray t _) = checkRcursive t
checkRcursive (CbConst t  ) = checkRcursive t
checkRcursive t             = do
    cst <- get
    put $ cst { rcursive = Map.insert t Checking $ rcursive cst }
    pure True

------------------------ check expr ----------------------------------

checkTypeBinByArithmetic :: Type -> Type -> CbCheck Type
checkTypeBinByArithmetic lt rt = if
  | isBool lt || isBool rt -> fail "无法对bool类型运用这个运算"
  | otherwise -> case typeTable !! typeIndex lt !! typeIndex rt of
    Just t  -> pure t
    Nothing -> fail "无法提升的类型"

checkTypeBinByPOrE :: Type -> Type -> CbCheck Type
checkTypeBinByPOrE lt rt = if
  | isPtr lt && isPtr rt -> fail "两指针之间无法运行该运算"
  | isPtr lt -> if isInteger rt then pure lt else fail "对指针类型加减必须为整形"
  | isPtr rt -> if isInteger lt then pure rt else fail "对指针类型加减必须为整形"
  | otherwise -> checkTypeBinByArithmetic lt rt

checkTypeBin :: Type -> String -> Type -> CbCheck Type
checkTypeBin lt op rt = if
  | op `elem` ["+", "-"] -> checkTypeBinByPOrE lt rt
  | op `elem` ["*", "/"] -> checkTypeBinByArithmetic lt rt
  | op `elem` ["<", "<=", ">", ">="] -> const CbBool <$> checkTypeBinByArithmetic lt rt
  | op `elem` ["%", "&", "|", "^", ">>", "<<"] ->  if
    | isInteger lt && isInteger rt -> checkTypeBinByArithmetic lt rt
    | otherwise -> fail $ "无法对非整形数字运用 " ++ op ++ " 运算符"
  | op `elem` ["==", "!="] -> case typeTable !! typeIndex lt !! typeIndex rt of
    Just t  -> pure CbBool
    Nothing -> fail "无法提升的类型"
  | op `elem` ["and", "or"] -> if
    | isBool lt && isBool rt -> pure CbBool
    | otherwise -> fail "逻辑运算符两侧须为bool类型"
  | otherwise -> fail "未知的操作符"

checkTypeUnary :: String -> Type -> CbCheck Type
checkTypeUnary op t = if
    | op `elem` ["+", "-"] -> if isSigned t
        then pure t
        else fail "无法对非有符号数字调用该操作符"
    | op == "~" -> if isInteger t then pure t else fail "无法对非整数类型调用该操作符"
    | op == "!" -> if isBool t then pure t else fail "无法对非布尔类型调用该操作符"
    | op `elem` ["++", "--"] -> if isNumber t || isPtr t
        then pure t
        else fail "仅可对指针或数字类型调用该操作符"
    | otherwise -> fail "未知的操作符"

checkTypeAssign :: Type -> Type -> CbCheck Type
checkTypeAssign (CbConst _) _  = fail "对 const 类型的表达式赋值"
checkTypeAssign lt          rt = pormot' rt lt

checkCall :: Type -> [Type] -> Bool
checkCall (CbFunction fc pats) params =
  length pats == length params && all (uncurry pormot) (zip params pats)

pormot' :: Type -> Type -> CbCheck Type
pormot' source target =
  if pormot source target then pure target else fail "类型不匹配"

maybeToEither :: Maybe a -> CbCheck a
maybeToEither (Just a) = pure a
maybeToEither Nothing  = fail "类型不匹配"

pormotTwo :: Type -> Type -> CbCheck Type
pormotTwo t1 t2 = maybeToEither $ typeTable !! typeIndex t1 !! typeIndex t2

-- 计算并检测表达式类型
computeType :: Expr -> CbCheck Type
computeType (Funcall fc params fun) = do
  funType    <- computeType fun
  paramTypes <- mapM computeType params
  case funType of
    t@(CbFunction rt pats) ->
      if checkCall t paramTypes then pure rt else fail "函数调用参数类型不匹配"
    _ -> fail "对非函数表达式进行调用"
computeType (SizeofType fc t ) = pure CbLong
computeType (SizeofExpr fc e_) = pure CbLong
computeType (Assign fc lv rv ) = if isLValue lv
  then do
    lt <- computeType lv
    rt <- computeType rv
    checkTypeAssign lt rt
  else fail "对非左值做赋值操作"
computeType (OpAssign fc op lv rv) = if isLValue lv
  then do
    rt <- computeType $ Binary NoFC op lv rv
    lt <- computeType lv
    checkTypeAssign lt rt
  else fail "对非左值做赋值操作"
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
    else fail "作为条件的表达式并非bool类型"
computeType (Address fc expr) = if isLValue expr
  then do
    t <- computeType expr
    case t of
      CbFunction{} -> pure t -- 函数类型会进行隐式指针类型转换
      _            -> pure $ CbPtr t
  else fail "对非左值做取址操作"
computeType (Dereference fc expr) = do
  t <- computeType expr
  case t of
    CbFunction{} -> pure t
    CbPtr t      -> pure t
    _            -> fail "对非指针对象解引用"
computeType (Member fc name expr) = do
  t <- computeType expr
  case findMem t name of
    Just (Param t _ ) -> pure t
    Nothing            -> fail $ "无法获取 " ++ name ++ " 成员的类型"
computeType (PtrMember fc name expr) = do
  t <- computeType expr
  case findPtrMem t name of
    Just (Param t _ ) -> pure t
    Nothing            -> fail $ "无法获取 " ++ name ++ " 成员的类型"
computeType (Arrayref fc ve ie) = do
  vt <- computeType ve
  it <- computeType ie
  if isPtr vt && isInteger it
    then maybeToEither $ mem vt
    else fail "对非指针类型的值解引用"
computeType (Decl fc name type_) = pure type_
computeType (Seq fc exprs)       = last <$> mapM computeType exprs
computeType IntLiteral{}         = pure $ CbConst CbInt
computeType FloatLiteral{}       = pure $ CbConst CbFloat
computeType StringLiteral{}      = pure $ CbConst $ CbPtr CbChar
computeType CharLiteral{}        = pure $ CbConst CbChar
computeType BoolLiteral{}        = pure $ CbConst CbBool
