{-# LANGUAGE OverloadedLists, MultiWayIf, TupleSections #-}

-- 控制流是这样的
module CFG where

import           IR
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

toBlock :: IR -> V.Vector IR
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

data FCNode = FCBlock { block :: IR }
            | FCStart
            | FCEnd
            deriving (Show, Eq, Ord)

isBlock :: FCNode -> Bool
isBlock (FCBlock _) = True
isBlock _           = False

toBlocks :: CFG [(G.Node, IR)]
toBlocks = do
    nodes <- G.labNodes <$> lift ask
    pure $ G.mapSnd block <$> filter (isBlock . snd) nodes

assignInBlock :: Expr -> FCNode -> Bool
assignInBlock le FCEnd        = False
assignInBlock le FCStart      = False
assignInBlock le (FCBlock ir) = match ir
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

data NSet a = FSet | NSet { nset :: S.Set a }
    deriving (Eq, Ord, Show)

instance (Ord a) => Lattices (NSet a) where
    top    = FSet
    bottom = NSet S.empty

    FSet \/ _ = FSet
    _    \/ FSet = FSet
    NSet ls \/ NSet rs = NSet $ ls `S.union` rs

    FSet /\ e = e
    e /\ FSet = e
    NSet ls /\  NSet rs = NSet $ ls `S.intersection` rs

-- 正向分析需要从start开始，根据当前节点的前置节点来计算lattices
worklistForward :: (Lattices a, Eq a) => FlowF a -> CFG (NMap a)
worklistForward f = do
    nodes <- reverse . G.dfs' <$> lift ask
    worklistIter nodes (/\) top pre suc f

-- 反向分析需要从end开始，根据当前节点的后置节点来计算lattices
worklistBackwark :: (Lattices a, Eq a) => FlowF a -> CFG (NMap a)
worklistBackwark f = do
    nodes <- G.dfs' <$> lift ask
    worklistIter nodes (/\) top suc pre f

worklistIter
    :: (Lattices a, Eq a)
    => [G.Node] -- 遍历的集合列表   
    -> (a -> a -> a) -- 数据流聚合函数
    -> a -- 初始化格
    -> (G.Node -> CFG [G.Node]) -- 节点的影响节点
    -> (G.Node -> CFG [G.Node]) -- 受影响节点
    -> FlowF a -- 流函数
    -> CFG (NMap a)
worklistIter nodes aggreF init from to f = impl nodes $ M.fromList $ fmap
    (, init)
    nodes
  where
    totaleffect node res =
        foldl' (\p b -> f b (res M.! b) `aggreF` p) init <$> from node
    impl []       res = pure res
    impl (b : bs) res = do
        total <- totaleffect b res
        if total == res M.! b
            then impl bs res
            else do
                ns <- to b
                impl (nub $ ns ++ bs) (M.insert b total res)

liveOutF :: (NMap (NSet Expr), NMap (NSet Expr)) -> FlowF (NSet Expr)
liveOutF (genMap, prsvMap) = \node liveOut ->
    let gen  = getEs genMap node
        prsv = getEs prsvMap node
    in  gen \/ (liveOut /\ prsv)
    where getEs lvset node = fromMaybe bottom $ M.lookup node lvset

varSet :: CFG (NMap (NSet Expr), NMap (NSet Expr))
varSet =
    foldl'
            (\(gv, pv) (node, ir) ->
                ( M.insert node (NSet $ genLvs ir) gv
                , M.insert node (NSet $ prsv ir) pv
                )
            )
            (M.empty, M.empty)
        <$> toBlocks

liveOut :: CFG (NMap (NSet Expr))
liveOut = do
    vs <- varSet
    worklistForward $ liveOutF vs

genFlowChart :: V.Vector IR -> G.Gr FCNode ()
genFlowChart blocks = G.run_ G.empty $ do
    G.insMapNodeM FCStart
    G.insMapNodeM FCEnd
    V.imapM_ impl blocks
    G.insMapEdgeM (FCStart, FCBlock $ V.head blocks, ())
  where
    impl i block
        = let block' = FCBlock block
          in
              do
                  G.insMapNodeM block'
                  stmt <- V.lastM block
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
                              else FCBlock $ blocks V.! (i + 1)
                          , ()
                          )
    findBlock label = FCBlock $ fromJust $ V.find ((label ==) . V.head) blocks

type CFG = MaybeT (Reader (G.Gr FCNode ()))

suc :: G.Node -> CFG [G.Node]
suc node = G.suc <$> lift ask <*> pure node

pre :: G.Node -> CFG [G.Node]
pre node = G.pre <$> lift ask <*> pure node

backEdge :: CFG [(G.Node, G.Node)]
backEdge = do
    g <- lift ask
    pure $ filter (G.hasEdge g) (G.iDom g 1)

natLoop :: (G.Node, G.Node) -> CFG [G.Node]
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


