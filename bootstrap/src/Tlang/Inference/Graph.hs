{-| * Graphic representation and inference for MLF type system

   This module contains all main methods for inference of MLF type system.

   ** Supporting Module

   We use fgl as underlying data structure to operate on graphic representation of
   MLF grpahic types. Please see `Tlang.Inference.Graph.Operation` for lowlevel
   operations of MLF constraint.
-}

module Tlang.Inference.Graph
  ( 
  )
where

-- import Tlang.Graph.Type

-- TODO: add a set of test cases
-- here is one: forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c
-- | first order term unification, operates on function nodes, return the proper unified node

{- | ** utility for type inference
--
--  used to solve graphic constraint
-}

-- | expand a type scheme under a gen node
-- expandat
--   :: forall m lit label name
--   .  ( MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m
--      , MonadState (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m
--      , Eq lit, Eq name, Eq label, MonadFail m)
--   => (Node, Node) -> Node -> m (Node, [(Node, Node)])
-- expandat (rootG, scheme) target = do
--   preAssert
--   binds <- dupScheme
--   return (fromJust $ lookup scheme binds, binds)
--   where
--     directS g = nub . fmap fst . filter (isStructureEdge . snd) . lsuc g
--     dupScheme :: m [(Node, Node)]
--     dupScheme = do
--       labof <- lab'' <$> (ask :: m (Gr (GNode (GNodeLabel lit label name)) (GEdge name)))
--       oNodes :: [Node] <- do
--         n1 <- interiorS rootG
--         nds <- union n1 <$> frontierS rootG
--         intersect <$> interiorI scheme <*> return nds
--       nNodes <- gets $ newNodes (length oNodes)
--       let nBinds = zip oNodes nNodes
--           oBinds = zip nNodes oNodes
--       -- insert new generated nodes
--       modify $ insNodes (second labof <$> oBinds)
--       -- insert edges between copied nodes
--       do fullEdges <- asks $ -- get all edges between copied nodes
--            filter (\(from, to, isConstraintEdge -> l) -> from `elem` oNodes && to `elem` oNodes && not l) . labEdges
--          modify . insEdges $ (\(from, to, l) -> let toNew = fromJust . flip lookup nBinds in (toNew from, toNew to, l)) <$> fullEdges
--       -- change frontierS nodes into bottom nodes
--       do fS <- intersect <$> frontierS rootG <*> pure oNodes
--          modify . insNodes $ (, GType NodeBot) . fromJust . flip lookup nBinds <$> fS
--          modify . insEdges $ fS >>= -- add unify constraint between two nodes
--            (\o -> let n = fromJust $ lookup o nBinds in [(o, n, GOperate CUnify), (n, o, GOperate CUnify)])
--       -- add binding edges to new rootG
--       let nRoot = fromJust $ lookup scheme nBinds
--       do nx <- gets $ length . flip bEdgeOut target
--          modify $ insEdge (nRoot, target, GBind Flexible (nx + 1) Nothing)
--       -- add MLF expansion
--       do oEdges <- asks $ filter (\(from, _, _) -> from `elem` oNodes) . flip bEdgeIn rootG
--          modify . insEdges $ fmap (\(from, _, l) -> (fromJust $ lookup from nBinds, nRoot, l)) oEdges
--       return nBinds
--     preAssert :: m ()
--     preAssert = do
--       labof <- asks lab''
--       directOf <- asks directS
--       -- type scheme should be a direct child of node G (rootG)
--       if scheme `elem` directOf rootG then return () else fail "Invalid type scheme to be expanded"
--       -- the target node should be a node G again
--       if labof target == GNode then return () else fail "Invalid expansion root"

-- -- | get **constraint interior node** including it self.
-- interiorC
--   :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
--   => Node -> m [Node]
-- interiorC root = go [root] []
--   where
--     go [] visited = return visited
--     go roots visited = do
--       next <- asks with
--       let nexts = nub $ next =<< roots
--           rets = roots `union` visited
--       go (filter (`notElem` rets) nexts) rets
--       where
--         with g = nub . fmap fst . filter (isBindEdge . snd) . lpre g

-- -- | get **structural interior node**
-- interiorS
--   :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
--   => Node -> m [Node]
-- interiorS root = do
--   cs <- interiorC root
--   ss <- interiorI root
--   return (nub $ intersect cs ss)

-- -- | get reachable structure node
-- interiorI
--   :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
--   => Node -> m [Node]
-- interiorI root = go [root] []
--   where
--     go [] xs = return xs
--     go roots visited = do
--       next <- asks with
--       let nexts = nub $ next =<< roots
--           rets = roots `union` visited
--       go (filter (`notElem` rets) nexts) rets
--       where
--         with g = nub . fmap fst . filter (isStructureEdge . snd) . lsuc g

-- -- | get **structural frontier node**
-- frontierS
--   :: (MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m)
--   => Node -> m [Node]
-- frontierS root = do
--   interiors <- interiorS root
--   next <- asks with
--   return . filter (`notElem` interiors) . nub $ next =<< interiors
--   where
--     with g = nub . fmap fst . filter (isStructureEdge . snd) . lsuc g

-- permission :: MonadReader (Gr (GNode (GNodeLabel lit label name)) (GEdge name)) m
--            => Node -> Node -> m Perm
-- permission _root _n = do
--   void ask
--   undefined

