{-# LANGUAGE OverloadedLists, MultiWayIf, TupleSections #-}

module Analysis.Base where

import           IR
import           Helper
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Graph.Inductive          as G
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans            ( lift )
import           Control.Monad.ST
import           Control.Monad                  ( guard
                                                , mapM
                                                , void
                                                , when
                                                , foldM
                                                , forM
                                                )
import           Control.Applicative
import           GHC.Generics                   ( Generic )

-- 将AState中的cfg转换成ssa形式
-- TODO 对于赋值左侧的指针操作暂时不知道怎么处理，先往下走走看，实在不行把对指针操作当一个整体
ssa :: Analysis ()
ssa = do
    gens  <- M.filter ((> 1) . S.size) <$> genBlocks
    idoms <- idoms
    df    <- domFront idoms
    let inPhis = M.foldlWithKey'
            (\res expr gens -> M.insert expr (dfPlus df gens) res)
            M.empty
            gens

    undefined
  where
    isLab Label{} = True
    isLab _       = False

    insertIrHand ir phis
        | isLab $ V.head ir = V.cons (V.head ir) $ phis V.++ V.tail ir
        | otherwise         = phis V.++ ir

    rename = undefined

-- iDom为直接支配节点树，idoms为给定节点作为支配节点的map
idoms :: Analysis (NMap (S.Set G.Node))
idoms = do
    iDom <- iDom <$> get
    pure $ foldl' (\res node -> M.insert node (iter node iDom) res)
                  M.empty
                  (M.keys iDom)
  where
    iter node iDom = case M.lookup node iDom of
        Just n  -> S.insert n $ iter n iDom
        Nothing -> S.empty

genBlocks :: Analysis (M.Map Expr (S.Set G.Node))
genBlocks = do
    genMap <- genMap <$> get
    pure $ foldl'
        (\res node -> foldl'
            (\res' expr -> case M.lookup expr res of
                Just s  -> M.insert expr (S.insert node s) res'
                Nothing -> M.insert expr (S.singleton node) res'
            )
            res
            (nset $ genMap M.! node)
        )
        M.empty
        (M.keys genMap)

domFront :: NMap (S.Set G.Node) -> Analysis (NMap (S.Set G.Node))
domFront idom = foldM
    (\df x -> do
        let idoms = idom M.! x
        local <- S.filter (`notElem` idoms) . S.fromList <$> suc x -- 当前必经节点后继节点的必经节点不是这个节点的
        let
            dfs = foldl'
                (\b z -> b `S.union` fromMaybe S.empty (df M.!? z))
                S.empty
                idoms
        let ups = S.filter (`notElem` idoms) dfs
        pure $ M.insert x (local `S.union` ups) df
    )
    M.empty
    (reverse $ M.keys idom)

-- DF(S) = \/DF(x)
-- DF+(S) = lim DF^i(S) -> DF^1(S) = DF(S) and DF^i(S) = DF(S \/ DF^i(S))
dfPlus :: NMap (S.Set G.Node) -> S.Set G.Node -> S.Set G.Node
dfPlus df s = impl $ dfSet df s
  where
    impl dfp =
        let ndfp = dfSet df $ s `S.union` dfp
        in  if dfp == ndfp then dfp else impl ndfp

dfSet :: NMap (S.Set G.Node) -> S.Set G.Node -> S.Set G.Node
dfSet df = foldl' (\r a -> r `S.union` fromMaybe S.empty (df M.!? a)) S.empty

toBlock :: Stmts -> V.Vector Stmts
toBlock ir = foldr impl V.empty zipIR
  where
    zipIR = V.zip ir (V.tail ir)

    isLabel Label{} = True
    isLabel _       = False

    nextLeader Return{} = True
    nextLeader CJump{}  = True
    nextLeader Jump{}   = True
    nextLeader _        = False

    pushLast vec v = V.snoc (V.init vec) (V.snoc (V.last vec) v)

    impl (f, s) v | nextLeader f = V.snoc (pushLast v f) V.empty
                  | isLabel s    = V.snoc v V.empty
                  | otherwise    = pushLast v f

data FCNode a = FCNode { nodeVal :: a }
              | FCStart
              | FCEnd
              deriving (Show, Eq, Ord)

isNode :: FCNode a -> Bool
isNode (FCNode _) = True
isNode _          = False

toBlocks :: Analysis [(G.Node, IR)]
toBlocks = do
    nodes <- G.labNodes . cfg <$> get
    pure $ G.mapSnd nodeVal <$> filter (isNode . snd) nodes

assignInBlock :: Expr -> FCNode IR -> Bool
assignInBlock le FCEnd       = False
assignInBlock le FCStart     = False
assignInBlock le (FCNode ir) = match $ stmts ir
  where
    match ir
        | V.null ir = False
        | otherwise = case assignLv (V.head ir) of
            Just lv -> (lv == le) || match (V.tail ir)
            _       -> match (V.tail ir)

type NMap a = M.Map G.Node a

class Lattices a where
    top, bottom :: a
    (\/), (/\) :: a -> a -> a

type FlowF a = G.Node -> a -> a

data LSet a = FSet | LSet { nset :: S.Set a }
    deriving (Eq, Ord, Show)

data LMap k v = FMap | LMap { nmap :: M.Map k v }
    deriving (Eq, Ord, Show)

instance (Ord a) => Lattices (LSet a) where
    top    = FSet
    bottom = LSet S.empty

    FSet \/ _ = FSet
    _    \/ FSet = FSet
    LSet ls \/ LSet rs = LSet $ ls `S.union` rs

    FSet /\ e = e
    e /\ FSet = e
    LSet ls /\  LSet rs = LSet $ ls `S.intersection` rs

instance (Ord k, Lattices v) => Lattices (LMap k v) where
    top    = FMap
    bottom = LMap M.empty

    FMap \/ _ = FMap
    _    \/ FMap = FMap
    LMap ls \/ LMap rs = LMap $ foldl' (\m (k,v) ->
        case M.lookup k rs of
            Just rv -> M.insert k (v \/ rv) m
            Nothing -> M.insert k v m
        ) M.empty $ M.toList ls

    FMap /\ e = e
    e /\ FMap = e
    LMap ls /\ LMap rs = LMap $ foldl' (\m (k,v) ->
        case M.lookup k rs of
            Just rv -> M.insert k (v /\ rv) m
            Nothing -> m
        ) M.empty $ M.toList ls

-- 正向分析需要从start开始，根据当前节点的前置节点来计算lattices
worklistForward :: (Lattices a, Eq a) => FlowF a -> Analysis (NMap a)
worklistForward f = do
    nodes <- reverse . G.dfs' . cfg <$> get
    worklistIter nodes (/\) top pre suc f

-- 反向分析需要从end开始，根据当前节点的后置节点来计算lattices
worklistBackwark :: (Lattices a, Eq a) => FlowF a -> Analysis (NMap a)
worklistBackwark f = do
    nodes <- G.dfs' . cfg <$> get
    worklistIter nodes (\/) bottom suc pre f


worklistIter
    :: (Lattices a, Eq a)
    => [G.Node] -- 遍历的集合列表
    -> (a -> a -> a) -- 数据流聚合函数
    -> a -- 初始化格
    -> (G.Node -> Analysis [G.Node]) -- 流入节点
    -> (G.Node -> Analysis [G.Node]) -- 流出节点
    -> FlowF a -- 流函数
    -> Analysis (NMap a)
worklistIter nodes aggreF init in' out f = impl nodes $ M.fromList $ fmap
    (, init)
    nodes
  where
    totaleffect node res =
        foldl' (\p b -> f b (res M.! b) `aggreF` p) init <$> in' node
    impl []       res = pure res
    impl (b : bs) res = do
        total <- totaleffect b res
        if total == res M.! b
            then impl bs res
            else do
                ns <- out b
                impl (bs ++ filter (`notElem` bs) ns) (M.insert b total res)

liveOutF :: (NMap (LSet Expr), NMap (LSet Expr)) -> FlowF (LSet Expr)
liveOutF (genMap, prsvMap) = \node liveOut ->
    let gen  = getEs genMap node
        prsv = getEs prsvMap node
    in  gen \/ (liveOut /\ prsv)
    where getEs lvset node = fromMaybe bottom $ M.lookup node lvset

varSet :: Analysis ()
varSet = do
    ast               <- get
    (genMap, prsvMap) <-
        foldl'
                (\(gv, pv) (node, ir) ->
                    ( M.insert node (LSet $ genLvs ir) gv
                    , M.insert node (LSet $ prsv ir) pv
                    )
                )
                (M.empty, M.empty)
            <$> toBlocks
    put $ ast { genMap = genMap, prsvMap = prsvMap }

liveOut :: Analysis (NMap (LSet Expr))
liveOut = do
    ast <- get
    worklistForward $ liveOutF (genMap ast, prsvMap ast)

genFlowChart :: V.Vector IR -> G.Gr (FCNode IR) ()
genFlowChart blocks = G.run_ G.empty $ do
    G.insMapNodeM FCStart
    G.insMapNodeM FCEnd
    V.imapM_ impl blocks
    G.insMapEdgeM (FCStart, FCNode $ V.head blocks, ())
  where
    impl i block
        = let block' = FCNode block
          in
              do
                  G.insMapNodeM block'
                  stmt <- V.lastM $ stmts block
                  case stmt of
                      CJump _ then_ else_ -> do
                          G.insMapEdgeM (block', findBlock then_, ())
                          G.insMapEdgeM (block', findBlock else_, ())
                      Jump label -> G.insMapEdgeM (block', findBlock label, ())
                      Return _ -> G.insMapEdgeM (block', FCEnd, ())
                      _ -> G.insMapEdgeM
                          ( block'
                          , if length blocks == i
                              then FCEnd
                              else FCNode $ blocks V.! (i + 1)
                          , ()
                          )
    findBlock label =
        FCNode $ fromJust $ V.find ((label ==) . V.head . stmts) blocks

data AState = AState
    {
      cfg :: G.Gr (FCNode IR) () -- 以基本块为单位的控制流图
    , stmtCfg :: G.Gr (FCNode Stmt) () -- 以语句为单位的控制流图
    , iDom :: NMap G.Node
    , genMap :: NMap (LSet Expr)
    , prsvMap :: NMap (LSet Expr)
    , varRname :: M.Map Expr (S.Set Expr) -- ssa重命名变量集合
    }
    deriving (Show, Eq)

type Analysis = StateT AState (MaybeT Cb)

stmtCfgInit :: Analysis ()
stmtCfgInit = do
    ist <- get
    put ist { stmtCfg = impl (cfg ist) }
  where
    impl g = G.run_ G.empty $ do
        mapM_ insIr (snd <$> G.labNodes g)
        mapM_ (insEdges g) $ G.edges g

    insEdges g (f, t) = insEdge (fromJust $ G.lab g f) (fromJust $ G.lab g t)

    insEdge (FCNode from) (FCNode to) = G.insMapEdgeM
        (FCNode $ V.last $ stmts from, FCNode $ V.head $ stmts to, ())
    insEdge FCStart (FCNode to) =
        G.insMapEdgeM (FCStart, FCNode $ V.head $ stmts to, ())
    insEdge (FCNode from) FCEnd =
        G.insMapEdgeM (FCNode $ V.last $ stmts from, FCEnd, ())
    insEdge _ _ = G.insMapEdgeM (FCStart, FCEnd, ())

    insIr (FCNode ir) = G.insMapNodesM $ fmap FCNode (V.toList (stmts ir))
    insIr FCEnd       = G.insMapNodesM [FCEnd]
    insIr FCStart     = G.insMapNodesM [FCStart]

suc :: G.Node -> Analysis [G.Node]
suc node = G.suc . cfg <$> get <*> pure node

pre :: G.Node -> Analysis [G.Node]
pre node = G.pre . cfg <$> get <*> pure node

backEdge :: Analysis [(G.Node, G.Node)]
backEdge = do
    g <- cfg <$> get
    pure $ filter (G.hasEdge g) (G.iDom g 1)

natLoop :: (G.Node, G.Node) -> Analysis [G.Node]
natLoop (m, n) = impl m [n]
  where
    impl p loop
        | p `elem` loop = pure loop
        | otherwise = do
            ns <- pre p
            r  <- foldM
                (\l n -> do
                    r <- impl n l
                    pure (n : l)
                )
                loop
                ns
            pure (p : r)
