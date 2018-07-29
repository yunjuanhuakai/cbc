{-# LANGUAGE MultiWayIf #-}

-- 类型定义的正确性以及静态类型检测
module Type.Check where

import           Ast
import           Helper
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

checkExpr :: Expr -> Type -> Cb ()
checkExpr (Funcall fc params fun) t = undefined
checkExpr (Assign fc lv rv     ) t = undefined
checkExpr (OpAssign fc op lv rv) t = undefined
checkExpr (Unary  fc op expr       ) t = checkExpr expr t
checkExpr (Suffix fc op expr       ) t = checkExpr expr t

------------------------ check type ----------------------------------

checkTypeBinByArithmetic :: Type -> Type -> Either String Type
checkTypeBinByArithmetic lt rt = if
  | isBool lt || isBool rt -> Left "无法对bool类型运用这个运算"
  | otherwise -> case typeTable !! typeIndex lt !! typeIndex rt of
    Just t  -> Right t
    Nothing -> fail "无法提升的类型"

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
    Nothing -> fail "无法提升的类型"
  | op `elem` ["&&", "||"] -> if
    | isBool lt && isBool rt -> Right CbBool
    | otherwise -> fail "逻辑运算符两侧须为bool类型"
  | otherwise -> undefined