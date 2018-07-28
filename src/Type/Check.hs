-- 类型定义的正确性、表达式有效性检测以及静态类型检测
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
