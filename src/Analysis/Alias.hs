{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Analysis.Alias where

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import qualified Data.Graph.Inductive          as G
import           Data.Maybe
import           Control.Monad.Trans.State
import           IR
import           Analysis.Base

type AilasMap = LMap Expr (LSet Expr)

worklistAilas :: FlowF AilasMap -> Analysis (NMap AilasMap)
worklistAilas f = do
    nodes  <- G.dfs' . stmtCfg <$> get
    lnodes <- G.labNodes . stmtCfg <$> get
    let stmts = fmap nodeVal $ filter isNode $ fmap snd lnodes
    worklistIter nodes (\/) (init stmts) suc pre f
  where
    init stmts =
        LMap $ foldl (\b a -> M.insert a bottom b) M.empty $ lvalues' stmts

ailas :: Analysis (NMap AilasMap)
ailas = worklistAilas $ aliasF impl where impl = undefined

aliasF :: NMap Stmt -> FlowF AilasMap
aliasF m n = aliasF' (m M.! n)

-- 只有对于赋值，有三种特殊情况
-- 变量地址赋值指针，别名信息变为指向单一内存块
-- 指针位移|数组索引赋值指针，别名信息改变为指向整个另一个整个指针对象
-- 对取址操作赋值，当取址的兑现为指针时，使被赋值的对象的别名信息变为等号右侧的别名信息
-- 其余赋值语句别名信息简单等同于右侧别名覆盖左侧
-- 其余语句不会改变别名信息
aliasF' :: Stmt -> AilasMap -> AilasMap
aliasF' (Assign l r) m
    | isPtr l && isAddr r = LMap $ M.insert l (LSet $ S.singleton r) (nmap m)
    | isPtr l && isBin r = let (Bin _ _ lv rv) = r in 
        if isPtr lv then replace m l lv else replace m l rv
    | isMem l && isPtr (var l) = aliasF' (Assign (var l) r) m
    | otherwise = replace m l r
aliasF' _ m = m

(!?) :: AilasMap -> Expr -> Maybe (LSet Expr)
FMap     !? l = Nothing
(LMap m) !? l = m M.!? l

push :: AilasMap -> Expr -> Expr -> AilasMap
push m k v =
    let vs  = nset $ fromJust (m !? k)
        nvs = LSet $ S.insert v vs
        nm  = M.insert k nvs (nmap m)
    in  LMap nm

replace :: AilasMap -> Expr -> Expr -> AilasMap
replace m l1 l2 =
    let sl1 = m !? l1
        sl2 = m !? l2
    in  case (sl1, sl2) of
            (Just s1, Just s2) -> LMap $ M.insert l1 s2 (nmap m)
            _                  -> m
