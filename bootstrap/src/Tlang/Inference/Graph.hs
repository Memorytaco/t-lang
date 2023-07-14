{-| * Graphic representation and inference for MLF type system

   This module contains all main methods for inference of MLF type system.

   ** Supporting Module

   We use fgl as underlying data structure to operate on graphic representation of
   MLF grpahic types. Please see `Tlang.Inference.Graph.Operation` for lowlevel
   operations of MLF constraint.
-}

module Tlang.Inference.Graph
  ( GEdge (..)
  , GConstraint (..)
  , GFlag (..)
  , Perm (..)
  , GNode (..)
  , GNodeLabel (..)
  , NodeArity (..)


  , (~=~)
  , expandat
  , runExpansion
  , interiorC, interiorS, interiorI
  , permission
  )
where

import Control.Monad
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..), modify, gets)
import Control.Monad.RWS (RWST (..))
import Data.Functor ((<&>))
import Data.Bifunctor (first, second, bimap)
import Data.List (sortBy, union, nub, intersect)
import Data.Graph.Inductive hiding (edges, nodes)
import Data.Maybe (fromJust)

import Tlang.Graph.Type
import Tlang.Graph.Operation

-- TODO: add a set of test cases
-- here is one: forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c
-- | first order term unification, operates on function nodes, return the proper unified node
(~=~)
  :: ( MonadState (Gr (GNode (GNodeLabel lit label name)) (GEdge name), [Node]) m, MonadFail m
     , Eq name, Eq lit, Ord label
     )
  => Node -> Node -> m Node
(~=~) n1 n2
  | n1 == n2 = return n1
  | otherwise = do
  (l1, l2) <- (,) <$> lof n1 <*> lof n2
  case (l1, l2) of
    -- internal error
    (GNode, _) -> fail "unexpected G node"
    (_, GNode) -> fail "unexpected G node"
    -- not supported
    (GType NodeAbs, _) -> fail "unsupported Abs node"
    (_, GType NodeAbs) -> fail "unsupported Abs node"
    -- variable
    (GType NodeBot, _) -> n1 ==> n2 >> n1 >== n2 >> n1 ==& n2
    (_, GType NodeBot) -> n2 ==> n1 >> n2 >== n2 >> n2 ==& n1
    (GType NodeSum, GType NodeSum) -> do
      f1s <- fmap snd <$> fields n1
      f2s <- fmap snd <$> fields n2
      if length f1s == length f2s then return () else fail "Mismatched arity of Sum node"
      n1 ==> n2 >> n1 ==& n2 >> unifyMulti (zip f1s f2s) >> return n2
    (GType NodeRec, GType NodeRec) -> do
      f1s <- fmap snd <$> fields n1
      f2s <- fmap snd <$> fields n2
      if length f1s == length f2s then return () else fail "Mismatched arity of Rec node"
      n1 ==> n2 >> n1 ==& n2 >> unifyMulti (zip f1s f2s) >> return n2
    (GType NodeTup, GType NodeTup) -> do
      ts1 <- subS n1
      ts2 <- subS n2
      if length ts1 == length ts2 then return () else fail "Mismatched arity of Tuple node"
      n1 ==> n2 >> n1 ==& n2 >> unifyMulti (zip ts1 ts2) >> return n2
    (GType NodeApp, GType NodeApp) -> do
      ts1 <- subS n1
      ts2 <- subS n2
      if length ts1 == length ts2 then return () else fail "Mismatched arity of Application node"
      n1 ==> n2 >> n1 ==& n2 >> unifyMulti (zip ts1 ts2) >> return n2
    (GType (NodeHas tag1), GType (NodeHas tag2)) -> do
      if tag1 == tag2 then return () else fail "Mismatched subfield"  -- the tags are same after this
      ts1 <- subS n1
      ts2 <- subS n2
      case (ts1, ts2) of
        -- label only, redirect and rebind
        ([], []) -> n1 ==> n2 >> n1 ==& n2
        -- unify and then redirect
        ([sn1], [sn2]) -> n1 ==> n2 >> n1 ==& n2 >> sn1 ~=~ sn2
        -- mismatched arity
        _ -> fail "Mismatched arity of label"
    (_, _) ->
      if l1 == l2
      then n1 ==> n2 >> n1 ==& n2
      else fail "Incompatible node"
  where
    -- | label of a node
    lof node = gets (context . fst) <*> pure node <&> lab'
    -- | out structure edge of a node
    subS node = do
      nodes <- gets (sOut . fst) <*> return node
      return . fmap fst $ sortBy (\(_, a) (_, b) -> compare a b) nodes
    -- | fields of a type, in its label order
    fields node = do
      nodes <- subS node
      sortBy (\(a, _) (b, _) -> compare a b) <$> forM nodes \flabel -> do
        tagn <- lof flabel
        case tagn of
          GType (NodeHas tag) -> return (tag, flabel)
          _ -> fail "Expect Field node, but get something other than NodeHas"
    -- | redirect all incoming structure edges of node1 to node2, return the second node
    (==>)
      :: (MonadState (Gr (GNode (GNodeLabel lit label name)) (GEdge name), [Node]) m, Eq name)
      => Node -> Node -> m Node
    (==>) src dst = gets (sEdgeIn . fst) <*> pure src >>= mapM_ \case
        e@(ancestor, _, GSub i) -> do
          modify (first $ delAllLEdge e)
          modify (first $ insEdge (ancestor, dst, GSub i))
        _ -> return ()
      >> return dst
    -- | maintain a grafted bottom node list such that m is bottom node, and <<m>> belongs to Tu term grah
    -- see p94, Figure 7.3.2, 1.c.Bn2 in Graphical types and constraints
    (>==) a b = modify (second $ union [b] . filter (/= a)) >> pure b
    -- | unify sequence pair of nodes, take additional care of shared nodes
    unifyMulti ((a,b):xs) = do
      to <- a ~=~ b
      let from = if to == a then b else a
          subst i = if i == from then to else i
      unifyMulti $ bimap subst subst <$> xs
    unifyMulti [] = return ()
    -- | return direct ancestors of node n, if it is partially grafted node.
    -- if this is used during unification, '==>' should be used first.
    partial n = do
      (g, ns) <- get
      if or $ sReach g <$> ns <*> [n]
      then return . nub $ fst <$> sIn g n
      else return []
    -- | get a node's binder
    binderOf n = do
      g <- gets fst
      case nub $ (\(_, b, e) -> [b | isBindEdge e]) =<< out g n of
        [a] -> return (a :: Node)
        _ -> fail "Property doesn't hold, every node should have exactly one binder"
    -- | get a node's bound flag
    flagOf n = do
      g <- gets fst
      case nub $ (\(_, _, e) -> [e | isBindEdge e]) =<< out g n of
        [GBind flag _ _] -> return flag
        _ -> fail "Property doesn't hold, every node should have exactly one binder"
    -- | least common binder of two nodes
    lcb (a :: Node) b = do
      g <- gets $ elfilter isBindEdge . fst
      let n1s = reverse $ dfs [a] g
          n2s = reverse $ dfs [b] g
      case filter (uncurry (==)) $ zip n1s n2s of
        [] -> fail $ "No least common binder for " <> show a <> " and " <> show b
        ls -> return . fst $ last ls
    -- | rebind (common binder, most resticted flag) of `a` and `b`
    (==&) (a :: Node) (b :: Node) = do
      -- get binders
      (b1, b2) <- (,) <$> binderOf a <*> binderOf b
      -- get flags
      (f1, f2) <- (,) <$> flagOf a <*> flagOf b
      -- direct ancestor of partial grafted node
      ns :: [Node] <- partial b
      -- compute least common binder
      cb <- foldM lcb b1 $ b2: ns
      -- let's rebind the edge!
      -- delete original edge first
      gets fst >>= \g -> case bEdgeOut g b of
        [e] -> modify (first $ delLEdge e)
        _ -> fail "Property doesn't hold, every node should have exactly one binder"
      count <- gets fst <&> (+1) . length . flip bEdgeIn cb
      modify (first $ insEdge (b, cb, GBind (min f1 f2) count Nothing))
      return b

{- | ** utility for type inference
--
--  used to solve graphic constraint
-}

runExpansion
  :: (Eq lit, Eq name, Eq label, MonadFail m)
  => Gr (GNode (GNodeLabel lit label name)) (GEdge name)
  -> (Node, Node) -> Node
  -> m ((Node, [(Node, Node)]), Gr (GNode (GNodeLabel lit label name)) (GEdge name))
runExpansion r a b = do
  (res, stat, ()) <- runRWST (expandat a b) r r
  return (res, stat)

-- | expand a type scheme under a gen node
expandat
  :: forall m lit label name
  .  ( MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m
     , MonadState (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m
     , Eq lit, Eq name, Eq label, MonadFail m)
  => (Node, Node) -> Node -> m (Node, [(Node, Node)])
expandat (rootG, scheme) target = do
  preAssert
  binds <- dupScheme
  return (fromJust $ lookup scheme binds, binds)
  where
    directS g = nub . fmap fst . filter (isStructureEdge . snd) . lsuc g
    dupScheme :: m [(Node, Node)]
    dupScheme = do
      labof <- lab'' <$> (ask :: m (Gr (GNode (GNodeLabel lit label name)) (GEdge name)))
      oNodes :: [Node] <- do
        n1 <- interiorS rootG
        nds <- union n1 <$> frontierS rootG
        intersect <$> interiorI scheme <*> return nds
      nNodes <- gets $ newNodes (length oNodes)
      let nBinds = zip oNodes nNodes
          oBinds = zip nNodes oNodes
      -- insert new generated nodes
      modify $ insNodes (second labof <$> oBinds)
      -- insert edges between copied nodes
      do fullEdges <- asks $ -- get all edges between copied nodes
           filter (\(from, to, isConstraintEdge -> l) -> from `elem` oNodes && to `elem` oNodes && not l) . labEdges
         modify . insEdges $ (\(from, to, l) -> let toNew = fromJust . flip lookup nBinds in (toNew from, toNew to, l)) <$> fullEdges
      -- change frontierS nodes into bottom nodes
      do fS <- intersect <$> frontierS rootG <*> pure oNodes
         modify . insNodes $ (, GType NodeBot) . fromJust . flip lookup nBinds <$> fS
         modify . insEdges $ fS >>= -- add unify constraint between two nodes
           (\o -> let n = fromJust $ lookup o nBinds in [(o, n, GOperate CUnify), (n, o, GOperate CUnify)])
      -- add binding edges to new rootG
      let nRoot = fromJust $ lookup scheme nBinds
      do nx <- gets $ length . flip bEdgeOut target
         modify $ insEdge (nRoot, target, GBind Flexible (nx + 1) Nothing)
      -- add MLF expansion
      do oEdges <- asks $ filter (\(from, _, _) -> from `elem` oNodes) . flip bEdgeIn rootG
         modify . insEdges $ fmap (\(from, _, l) -> (fromJust $ lookup from nBinds, nRoot, l)) oEdges
      return nBinds
    preAssert :: m ()
    preAssert = do
      labof <- asks lab''
      directOf <- asks directS
      -- type scheme should be a direct child of node G (rootG)
      if scheme `elem` directOf rootG then return () else fail "Invalid type scheme to be expanded"
      -- the target node should be a node G again
      if labof target == GNode then return () else fail "Invalid expansion root"

-- | get **constraint interior node** including it self.
interiorC
  :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
  => Node -> m [Node]
interiorC root = go [root] []
  where
    go [] visited = return visited
    go roots visited = do
      next <- asks with
      let nexts = nub $ next =<< roots
          rets = roots `union` visited
      go (filter (`notElem` rets) nexts) rets
      where
        with g = nub . fmap fst . filter (isBindEdge . snd) . lpre g

-- | get **structural interior node**
interiorS
  :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
  => Node -> m [Node]
interiorS root = do
  cs <- interiorC root
  ss <- interiorI root
  return (nub $ intersect cs ss)

-- | get reachable structure node
interiorI
  :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
  => Node -> m [Node]
interiorI root = go [root] []
  where
    go [] xs = return xs
    go roots visited = do
      next <- asks with
      let nexts = nub $ next =<< roots
          rets = roots `union` visited
      go (filter (`notElem` rets) nexts) rets
      where
        with g = nub . fmap fst . filter (isStructureEdge . snd) . lsuc g

-- | get **structural frontier node**
frontierS
  :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
  => Node -> m [Node]
frontierS root = do
  interiors <- interiorS root
  next <- asks with
  return . filter (`notElem` interiors) . nub $ next =<< interiors
  where
    with g = nub . fmap fst . filter (isStructureEdge . snd) . lsuc g

permission :: MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m
           => Node -> Node -> m Perm
permission _root _n = do
  void ask
  undefined

