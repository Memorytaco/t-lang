module Tlang.Inference.Graph
  ( GraphRel (..)
  , GEdge (..)
  , GConstraint (..)
  , GFlag (..)
  , Perm (..)
  , GNode (..)
  , GNodeLabel (..)
  , NodeArity (..)

  , arityOf
  , runToGraph
  , toGraph
  , simplify
  , runRestore
  , restore
  , augGraph

  , (~=~)
  , expandat
  , interiorC, interiorS, interiorI
  , permission
  )
where

import Tlang.AST

import Control.Monad
import Control.Monad.Reader (MonadReader (..), runReaderT, asks)
import Control.Monad.State (MonadState (..), modify, runStateT, gets)
import Control.Monad.Identity (Identity)
import Control.Monad.RWS (RWST (..))
import Data.Functor ((<&>), ($>))
import Data.Functor.Foldable
import Data.Bifunctor (first, second, bimap)
import Data.List (sortBy, find, union, deleteBy, nub)
import Data.Graph.Inductive hiding (edges, nodes)
import qualified Data.Graph.Inductive as I
import Data.Coerce (coerce)

import Tlang.Inference.Graph.Type
import Tlang.Inference.Graph.Operator

arityOf :: NodeArity -> [Variance]
arityOf = coerce

runToGraph
  :: (MonadFail m, Eq name, Traversable f, Traversable c)
  => [(name, Node)]
  -> Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)
  -> Type label name lit (Bound name) c f rep
  -> m (Node, Gr (GNode (GNodeLabel lit label rep name)) (GEdge name))
runToGraph r s t = do
  (root, graph, ()) <- runRWST (toGraph t) r s
  return (root, graph)

-- | transform syntactic type into graphic type, and check its validity
toGraph :: ( MonadFail m, MonadReader [(name, Node)] m
           , MonadState (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m
           , Eq name, Traversable f, Traversable c
           )
        => Type label name lit (Bound name) c f rep
        -> m Node
toGraph = cata go
  where
    newNode node = do
      nid <- gets $ head . newNodes 1
      modify $ insNode (nid, GType node)
      return nid
    newEdge label n1 n2 = modify $ insEdge (n1, n2, label)
    -- count new binding edge, and introduce a new index
    newBindSeq node = do
      graph <- get
      let counts = inn graph node >>= \(_, _, edge) ->
            case edge of
              GSub _ -> pure 0
              _ -> pure 1
      return (sum counts + 1)
    go TypBotF = newNode NodeBot
    go TypUniF = newNode NodeUni
    go (TypRepF rep) = newNode (NodeRep rep)
    go (TypLitF lit) = newNode (NodeLit lit)
    go (TypRefF name) = do
      res'maybe <- asks $ lookup name
      case res'maybe of
        Nothing -> newNode (NodeRef name)
        Just node -> return node
    go (TypTupF nds) = do
      tupNode <- newNode NodeTup
      nodes <- sequence nds
      forM_ (zip nodes $ GSub <$> [1..]) \(node, edge) -> newEdge edge tupNode node
      return tupNode
    go (TypRecF lns) = do
      redNode <- newNode NodeRec
      lbs <- mapM sequence lns
      forM_ (zip lbs $ GSub <$> [1..]) \((label, node), edge) -> do
        lNode <- newNode $ NodeHas label
        newEdge edge redNode lNode
        newEdge (GSub 1) lNode node
      return redNode
    go (TypSumF lns) = do
      sumNode <- newNode NodeSum
      lbs <- mapM (mapM sequence) lns
      forM_ (zip lbs $ GSub <$> [1..]) \((label, node'maybe), edge) -> do
        lNode <- newNode $ NodeHas label
        newEdge edge sumNode lNode
        mapM (newEdge (GSub 1) lNode) node'maybe
      return sumNode
    go (TypPieF _ _) = fail "type constraint is not supported"
    go (TypAppF ma mb ms) = do
      appNode <- newNode NodeApp
      na <- ma
      nb <- mb
      ns <- sequence ms
      forM_ (zip (na:nb:ns) $ GSub <$> [1..]) \(node, edge) -> newEdge edge appNode node
      return appNode
    go (TypEquF _ _) = fail "equi-recursive type is not supported"
    go (TypAllF mbound mr) = do
      bound <- sequence mbound
      case bound of
        name :> node -> do
          root <- local ((name, node):) mr
          ix <- newBindSeq root
          newEdge (GBind Flexible ix (Just name)) node root
          return root
        name :~ node -> do
          root <- local ((name, node):) mr
          ix <- newBindSeq root
          newEdge (GBind Rigid ix (Just name)) node root
          return root
    go (TypAbsF mbound mr) = do
      absNode <- newNode NodeAbs
      pair@(name, node) <- case mbound of
        name :> mt -> (name,) <$> mt
        name :~ mt -> (name,) <$> mt
      rnode <- local (pair:) mr
      newEdge (GBind Explicit 1 (Just name)) node absNode
      newEdge (GSub 1) absNode node
      newEdge (GSub 2) absNode rnode
      return absNode
    go (TypLiftF _) = fail "type annotation is not supported"

-- | simplify, forall (a <> g). a == g
-- FIXME: complete the ruleset
simplify r g =
  case cleanable g of
    [] -> g
    nds -> simplify r $ delNodes nds g
  where
    cleanable lg =
      let total = I.nodes lg
       in filter (\n -> n /= r && null (sOut lg n <> sIn lg n)) total

runRestore
  :: (Show label, MonadFail m)
  => Node
  -> Gr (GNode (GNodeLabel lit label rep Symbol)) (GEdge Symbol)
  -> ([(Node, Symbol)], Int)
  -> m (Type label Symbol lit (Bound Symbol) Identity Identity rep, ([(Node, Symbol)], Int))
runRestore node g s = do
  (typ, stat, ()) <- runRWST (restore (Symbol . show) node) g s
  return (typ, stat)

-- | convert a graphic type back to normal representation
-- it satisfies:
-- t == restore . toGraph t
-- FIXME: when somewhere occurs node like forall a.a, it will enter dead loop.
restore
  :: ( MonadReader (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m
     , MonadState ([(Node, name)], Int) m, Show label
     , MonadFail m
     )
  => (Node -> name) -> Node -> m (Type label name lit (Bound name) c f rep)
restore schema root = do
  -- we check out whether this node has been bound somewhere
  localNames <- gets fst
  case lookup root localNames of
    Just name -> return (TypRef name)  -- if so, return the bounded name and it is done.
    Nothing -> do -- otherwise, generate sub structure
      label <- ask >>= \g -> return . lab' $ context g root
      -- we construct local symbol first, mutate local name binding
      bounds <- getInBounds root
      oldBounds <- gets fst
      modify (first ((fmap snd <$> bounds) <>))
      -- we construct type body (aka. monotype) second
      body <- case label of
        GType NodeUni -> return TypUni
        GType NodeBot -> return TypBot
        GType (NodeRep rep) -> return (TypRep rep)
        GType (NodeLit lit) -> return (TypLit lit)
        GType (NodeRef name) -> return (TypRef name)
        GType NodeSum -> do
          labelNodes <- getStructure root >>= \subs -> mapM nodeLabel subs
          pairs <- forM labelNodes \(node, nlabel) ->
            case nlabel of
              GType (NodeHas label) -> do
                typ <- getLabelMaybe node
                return (label, typ)
              _ -> fail "Expect a Label node, but have none!"
          return (TypSum pairs)
        GType NodeRec -> do
          labelNodes <- getStructure root >>= mapM nodeLabel
          pairs <- forM labelNodes \(node, nlabel) ->
            case nlabel of
              GType (NodeHas label) -> do
                typ <- getLabel node
                return (label, typ)
              _ -> fail "Expect a Label node, but have none!"
          return (TypRec pairs)
        GType NodeTup -> do
          subtypes <- getStructure root >>= mapM (restore schema)
          return $ TypTup subtypes
        GType NodeApp -> do
          t1:t2:subtypes <- getStructure root >>= mapM (restore schema)
          return $ TypApp t1 t2 subtypes
        GType NodeAbs -> error "not supported now"
        GType (NodeHas tag) -> fail $ "Unexpected label node of " <> show tag
        GNode -> fail "Unexpected G node"
      -- we generalize type with local name binding, third
      bindings <- forM bounds \(node, (perm, name)) -> do
        -- remove bound ot the node, so recursive type variable is not allowed.
        modify (first $ filter (\(n, _) -> n /= node))
        typ <- restore schema node
        case perm of
          Rigid -> return (name :~ typ)
          Flexible -> return (name :> typ)
          Explicit -> return (name :> typ)
      let generalize = foldr (flip (.)) id $ TypAll <$> bindings
      modify (first $ const oldBounds) -- restore the local binding
      return $ generalize body  -- return the final type
  where
    -- | generate new name using index and one schema
    newName schema = do
      ix <- gets snd
      modify (fmap (+1))
      return (schema ix)
    -- | return information of bound node of a binder
    -- (Node, (Perm, name))
    getInBounds node = do
      inwards <- asks inn
      let -- filterBind :: Monad m => LEdge (GEdge name) -> m [((Node, Node), (Perm, name), Int)]
          filterBind (a, b, GBind perm ix Nothing) = do
            name <- newName schema
            return [((a,b), (perm, name), ix)]
          filterBind (a, b, GBind perm ix (Just name)) = return [((a,b), (perm, name), ix)]
          filterBind _ = return []
      bounds <- foldM (\ls nbound -> (ls <>) <$> filterBind nbound) [] $ inwards node
      return $ (\(fst -> from, info, _) -> (from, info)) <$> sortBy (\(_, _, i1) (_, _, i2)-> compare i1 i2) bounds
    -- | return out structure nodes, in sub order
    -- [Node]
    getStructure node = do
      outwards <- asks out
      let filterStructure :: Monad m => LEdge (GEdge name) -> m [((Node, Node), Int)]
          filterStructure (a, b, GSub ix) = return [((a,b), ix)]
          filterStructure _ = return []
      edges <- foldM (\ls nbound -> (ls <>) <$> filterStructure nbound) [] $ outwards node
      return $ (\((_, to), _) -> to) <$> sortBy (\(_, i1) (_, i2) -> compare i1 i2) edges
    -- | fetch label type
    getLabel node = do
      nodes <- getStructure node
      case nodes of
        [sub] -> restore schema sub
        _ -> fail $ "Require arity 1 of label but it is " <> show (length nodes)
    getLabelMaybe node = do
      nodes <- getStructure node
      case nodes of
        [] -> return Nothing
        [sub] -> Just <$> restore schema sub
        _ -> fail $ "Require arity 1 or 0 of label but it is " <> show (length nodes)
    nodeLabel node = ask >>= \g -> return (node, lab' $ context g node)

-- | add redundant binding edges to graphic type
-- FIXME: add constructor node and remove application node
-- TODO: add concepts of arity and variance of node
augGraph :: (MonadState (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m, MonadFail m)
         => Node -> m ()
augGraph root = do
  g <- get
  let subs = fst <$> sOut g root
      base = length . filter (\(_,_,e) -> isBindEdge e) $ inn g root
  valids <- nub . join <$> mapM checkEdge subs
  forM_ (zip valids [base..(base + length valids)]) \(n, i) -> do
    modify (insEdge (n, root, GBind Flexible i Nothing))
  augGraph `mapM` nub subs
  return ()
  where
    checkEdge r = do
      g <- get
      case filter (\(_,_, e) -> isBindEdge e) $ out g r of
        [] -> return [r]
        [_] -> return []
        _ -> fail $ "Invalid binding edge for node " <> show r

-- TODO: add a set of test cases
-- here is one: forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c
-- | first order term unification, operates on function nodes, return the proper unified node
(~=~)
  :: ( MonadState (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name), [Node]) m, MonadFail m
     , Eq name, Eq rep, Eq lit, Ord label
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
      :: (MonadState (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name), [Node]) m, Eq name)
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

{- | # utility for type inference
--  used to solve graphic constraint
-}

-- | expand a type scheme under a gen node
expandat = undefined

interiorC, interiorS
  :: (MonadReader (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m)
  => Node -> m [Node]
interiorC root = do
  go root
  undefined
  where
    go root = do
      g <- ask
      let nodes = fmap fst . filter (isBindEdge . snd) $ lpre g root
      undefined

interiorS = undefined

interiorI = undefined

permission :: MonadReader (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m
           => Node -> Node -> m Perm
permission _root _n = do
  void ask
  undefined
