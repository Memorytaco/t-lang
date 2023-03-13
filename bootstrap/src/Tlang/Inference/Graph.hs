module Tlang.Inference.Graph

where

import Tlang.AST

import Data.Graph.Inductive hiding (edges, nodes)
import qualified Data.Graph.Inductive as I
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State (MonadState (..), modify, runStateT)
import Control.Monad
import Control.Monad.RWS (RWST (..))
import Data.Functor.Foldable
import Data.Bifunctor (first, second, bimap)
import Data.List (sortBy, find, union, deleteBy, nub)
import Control.Monad.Identity (Identity)
import Data.Coerce (coerce)

-- graphviz
import Data.GraphViz hiding (DotGraph)
import Data.GraphViz.Attributes.Complete (Label (StrLabel))
import Data.Text.Lazy (pack)
import Data.GraphViz.Commands.IO (writeDotFile)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)

-- < play only, delete it later
import Tlang.AST (typOperator)
import qualified Tlang.Parser.Type as PT
import Data.Text (Text)
import qualified Data.Text.IO as Text
-- play only, delete it later >

data GraphRel
  = EqualRel    -- exact same structure
  | SimilarRel  -- similar relation
  | GraftRel    -- graft instance relation
  | MergeRel    -- merge instance relation
  | RaiseRel    -- raise instance relation
  | WeakRel     -- weaken instance relation
  deriving (Ord, Show, Eq)

-- | label for edge
data GEdge a
  = GSub Int
  -- ^ structural edge, with number attached. orders matter.
  | GBind GFlag Int (Maybe a)
  -- ^ binding edge, with number attached and also the origin name. orders and names matter.
  | GOperate GConstraint
  -- ^ operation edge that indicates one action. **now it serves as constraint edge only.**
  deriving (Show, Eq, Ord)

-- | Constraint edge
data GConstraint
  = CUnify        -- ^ unify constraint edge
  | CInstance Int -- ^ instance constraint edge
  deriving (Show, Eq, Ord)

-- | Used both for instance relation and permission
-- if `Lock` is taken as relation, then it is treated the same as `Rigid`
-- if `Explicit` is taken as permission, then it is treated the same as `Flexible`
data GFlag
  = Rigid     -- ^ permission r, Rigid
  | Explicit  -- ^ permission f, Flexible
  | Flexible  -- ^ permission f, Flexible
  deriving (Show, Eq, Ord)

-- | permission related to `GFlag`
data Perm
  = PRed    -- ^ mixed with rigid and flexible, lowest permission
  | POrange -- ^ first flexible and then rigid, middle permission
  | PGreen  -- ^ all time be the flexible, largest permission
  | PInert  -- ^ nodes have no bounded sub node
  | PMono   -- ^ momorphic nodes, have no polymorphic nodes
  deriving (Show, Eq, Ord)

-- | label for node
data GNode typ
  = GNode     -- ^ scheme node, the **G** node, with sort *scheme*
  | GType typ -- ^ type node, with sort *type*
  deriving (Show, Eq, Ord, Functor)

-- | actual label for Type, doesn't support predicate constraint
data GNodeLabel lit label rep name
  = NodeUni -- unit type node, arity 0
  | NodeBot -- special bottom node, arity 0
  | NodeRep rep -- representation node, arity 0
  | NodeLit lit -- literal type node, arity 0
  | NodeRef name  -- type ref node, used to hold concrete type, arity 0
  | NodeSum -- has multiple labels, and each label may have one type
  | NodeRec -- has multiple lables, and each label must have one type
  | NodeTup -- has as many sub nodes (graphs) as user want, preserve the order
  | NodeHas label -- type label node, depends on its parent, can have one or no sub graph, arity 1
  | NodeApp -- application node, minimal arity 2
  | NodeAbs -- abstraction node, arity 2
  | NodeCons name NodeArity
  deriving (Show, Eq)

-- | mark arity info of constructor
newtype NodeArity = NodeArity [Variance] deriving (Show, Eq)
arityOf :: NodeArity -> [Variance]
arityOf = coerce

instance (Labellable name, Show label) => Labellable (GNodeLabel lit label rep name) where
  toLabelValue NodeUni = StrLabel "()"
  toLabelValue NodeBot = StrLabel "⊥"
  toLabelValue (NodeRep _) = StrLabel "&"
  toLabelValue (NodeLit _) = StrLabel "$"
  toLabelValue (NodeRef name) = toLabelValue name
  toLabelValue NodeSum = StrLabel "<>"
  toLabelValue NodeRec = StrLabel "{}"
  toLabelValue NodeTup = StrLabel "*"
  toLabelValue (NodeHas label) = StrLabel (pack $ "$" <> show label)
  toLabelValue (NodeAbs) = StrLabel "λ"
  toLabelValue (NodeApp) = StrLabel "@"

instance Labellable typ => Labellable (GNode typ) where
  toLabelValue GNode = StrLabel ".G."
  toLabelValue (GType typ) = toLabelValue typ

instance Labellable Symbol where
  toLabelValue (Symbol name) = toLabelValue name
  toLabelValue (Op name) = toLabelValue name

instance Show a => Labellable (GEdge a) where
  toLabelValue (GSub i) = toLabelValue i
  toLabelValue (GBind perm ix (Just name)) = StrLabel . pack $ show ix <> ":" <> show name
  toLabelValue (GBind perm ix Nothing)  = StrLabel . pack $ show ix
  toLabelValue (GOperate CUnify) = StrLabel "**Unify**"
  toLabelValue (GOperate (CInstance i)) = StrLabel . pack $ show i <> " |>"

runToGraph
  :: (MonadFail m, Eq name, Traversable f, Traversable c)
  => [(name, Node)]
  -> Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)
  -> Type label name lit (Bound name) c f rep
  -> m (Node, Gr (GNode (GNodeLabel lit label rep name)) (GEdge name))
runToGraph r s t = do
  (root, graph, ()) <- runRWST (toGraph t) r s
  return (root, graph)

toGraph :: ( MonadFail m, MonadReader [(name, Node)] m
           , MonadState (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m
           , Eq name, Traversable f, Traversable c
           )
        => Type label name lit (Bound name) c f rep
        -> m Node
toGraph = cata go
  where
    newNode node = do
      nid <- head . newNodes 1 <$> get
      modify $ insNode (nid, GType node)
      return nid
    newEdge label n1 n2 = modify $ insEdge (n1, n2, label)
    -- count new binding edge, and introduce a new index
    newBindSeq node = do
      graph <- get
      let counts = (inn graph node) >>= \(_, _, edge) ->
            case edge of
              GSub _ -> pure 0
              _ -> pure 1
      return (sum counts + 1)
    go TypBotF = newNode NodeBot
    go TypUniF = newNode NodeUni
    go (TypRepF rep) = newNode (NodeRep rep)
    go (TypLitF lit) = newNode (NodeLit lit)
    go (TypRefF name) = do
      res'maybe <- lookup name <$> ask
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
      lbs <- mapM (sequence . fmap sequence) lns
      forM_ (zip lbs $ GSub <$> [1..]) \((label, node'maybe), edge) -> do
        lNode <- newNode $ NodeHas label
        newEdge edge sumNode lNode
        sequence $ newEdge (GSub 1) lNode <$> node'maybe
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
  localNames <- fst <$> get
  case lookup root localNames of
    Just name -> return (TypRef name)  -- if so, return the bounded name and it is done.
    Nothing -> do -- otherwise, generate sub structure
      label <- ask >>= \g -> return . lab' $ context g root
      -- we construct local symbol first, mutate local name binding
      bounds <- getInBounds root
      oldBounds <- fst <$> get
      modify (first ((fmap snd <$> bounds) <>))
      -- we construct type body (aka. monotype) second
      body <- case label of
        GType NodeUni -> return TypUni
        GType NodeBot -> return TypBot
        GType (NodeRep rep) -> return (TypRep rep)
        GType (NodeLit lit) -> return (TypLit lit)
        GType (NodeRef name) -> return (TypRef name)
        GType NodeSum -> do
          labelNodes <- getStructure root >>= \subs -> sequence $ nodeLabel <$> subs
          pairs <- forM labelNodes \(node, nlabel) ->
            case nlabel of
              GType (NodeHas label) -> do
                typ <- getLabelMaybe node
                return (label, typ)
              _ -> fail "Expect a Label node, but have none!"
          return (TypSum pairs)
        GType NodeRec -> do
          labelNodes <- getStructure root >>= \subs -> sequence $ nodeLabel <$> subs
          pairs <- forM labelNodes \(node, nlabel) ->
            case nlabel of
              GType (NodeHas label) -> do
                typ <- getLabel node
                return (label, typ)
              _ -> fail "Expect a Label node, but have none!"
          return (TypRec pairs)
        GType NodeTup -> do
          subtypes <- getStructure root >>= \subNodes -> sequence $ restore schema <$> subNodes
          return $ TypTup subtypes
        GType NodeApp -> do
          t1:t2:subtypes <- getStructure root >>= \subNodes -> sequence $ restore schema <$> subNodes
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
      ix <- snd <$> get
      modify (fmap (+1))
      return (schema ix)
    -- | return information of bound node of a binder
    -- (Node, (Perm, name))
    getInBounds node = do
      inwards <- inn <$> ask
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
      outwards <- out <$> ask
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
  valids <- nub . join <$> sequence (checkEdge <$> subs)
  forM_ (zip valids [base..(base + length valids)]) \(n, i) -> do
    modify (insEdge (n, root, GBind Flexible i Nothing))
  sequence $ augGraph <$> nub subs
  return ()
  where
    checkEdge r = do
      g <- get
      case filter (\(_,_, e) -> isBindEdge e) $ out g r of
        [] -> return [r]
        [_] -> return []
        _ -> fail $ "Invalid binding edge for node " <> show r

isStructureEdge, isBindEdge :: GEdge a -> Bool
isStructureEdge (GSub _) = True
isStructureEdge _ = False
isBindEdge (GBind _ _ _) = True
isBindEdge _ = False

-- | out structure edge of a node
sEdgeOut, sEdgeIn, bEdgeOut, bEdgeIn :: (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) -> Node -> [LEdge (GEdge name)]
sEdgeOut g node = filter (\(_,_,e) -> isStructureEdge e) $ out g node
sEdgeIn g node = filter (\(_,_,e) -> isStructureEdge e) $ inn g node
bEdgeOut g node = filter (\(_,_,e) -> isBindEdge e) $ out g node
bEdgeIn g node = filter (\(_,_,e) -> isBindEdge e) $ inn g node

-- | successor and presuccsor structure nodes of the node
sOut, sIn
  :: (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) -> Node -> [(Node, Int)]
sOut g node = foldl (\ls (n, e) -> case e of GSub i -> (n, i):ls; _ -> ls) [] $ lsuc g node  -- all successor of the node
sIn g node = foldl (\ls (n, e) -> case e of GSub i -> (n, i):ls; _ -> ls) [] $ lpre g node  -- all presuccessor of the node

-- | return True if there is a path from n1 to n2
sReach
  :: (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) -> Node -> Node -> Bool
sReach g n1 n2 = elem n2 . dfs [n1] $ elfilter isStructureEdge g

-- TODO: add a set of test cases
-- here is one: forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c
-- | first order term unification, operates on function nodes, return the proper unified node
(==?)
  :: ( MonadState (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name), [Node]) m, MonadFail m
     , Eq name, Eq rep, Eq lit, Ord label
     )
  => Node -> Node -> m Node
(==?) n1 n2
  | n1 == n2 = return n1
  | otherwise = do
  (l1, l2) <- (,) <$> lof n1 <*> lof n2
  case (l1, l2) of
    -- internal error
    (GNode, _) -> fail $ "unexpected G node"
    (_, GNode) -> fail $ "unexpected G node"
    -- not supported
    (GType NodeAbs, _) -> fail $ "unsupported Abs node"
    (_, GType NodeAbs) -> fail $ "unsupported Abs node"
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
        ([sn1], [sn2]) -> n1 ==> n2 >> n1 ==& n2 >> sn1 ==? sn2
        -- mismatched arity
        _ -> fail "Mismatched arity of label"
    (_, _) ->
      case l1 == l2 of
        True -> n1 ==> n2 >> n1 ==& n2
        False -> fail $ "Incompatible node"
  where
    -- label of a node
    lof node = fmap lab' $ context . fst <$> get <*> return node
    -- out structure edge of a node
    subS node = do
      nodes <- sOut . fst <$> get <*> return node
      return . fmap fst $ sortBy (\(_, a) (_, b) -> compare a b) nodes
    -- fields of a type, in its label order
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
    (==>) n1 n2 = sEdgeIn . fst <$> get <*> pure n1 >>= mapM_ \case
        (from, to, edge@(GSub _)) -> do
          modify (first $ delAllLEdge (from, to, edge))
          modify (first $ insEdge (from, n2, edge))
        _ -> return ()
      >> return n2
    -- | maintain a grafted bottom node list such that m is bottom node, and <<m>> belongs to Tu term grah
    -- see p94, Figure 7.3.2, 1.c.Bn2 in Graphical types and constraints
    (>==) n1 n2 = modify (second $ union [n2] . filter (/= n1)) >> pure n2
    -- | unify sequence pair of nodes, take additional care of shared nodes
    unifyMulti ((n1,n2):xs) = do
      to <- n1 ==? n2
      let from = if to == n1 then n2 else n1
          subst i = if i == from then to else i
      unifyMulti $ bimap subst subst <$> xs
    unifyMulti [] = return ()
    -- | return direct ancestors of node n, if it is partially grafted node.
    -- if this is used during unification, '==>' should be used first.
    partial n = do
      (g, ns) <- get
      case foldl (||) False $ sReach g <$> ns <*> [n] of
        False -> return []
        True -> return . nub $ fst <$> sIn g n
    -- | get a node's binder
    binderOf n = do
      g <- fst <$> get
      case nub . join $ (\(_, b, e) -> if isBindEdge e then [b] else []) <$> out g n of
        [a] -> return (a :: Node)
        _ -> fail "Property doesn't hold, every node should have exactly one binder"
    -- | get a node's bound flag
    flagOf n = do
      g <- fst <$> get
      case nub . join $ (\(_, b, e) -> if isBindEdge e then [e] else []) <$> out g n of
        [GBind flag _ _] -> return flag
        _ -> fail "Property doesn't hold, every node should have exactly one binder"
    -- | least common binder of two nodes
    lcb (n1 :: Node) n2 = do
      g <- elfilter isBindEdge . fst <$> get
      let n1s = reverse $ dfs [n1] g
          n2s = reverse $ dfs [n2] g
      case filter (uncurry (==)) $ zip n1s n2s of
        [] -> fail $ "No least common binder for " <> show n1 <> " and " <> show n2
        ls -> return . fst $ last ls
    -- | rebind (common binder, most resticted flag) of `n1` and `n2`
    (==&) (n1 :: Node) (n2 :: Node) = do
      -- binders
      (b1, b2) <- (,) <$> binderOf n1 <*> binderOf n2
      -- flags
      (f1, f2) <- (,) <$> flagOf n1 <*> flagOf n2
      -- direct ancestor of partial grafted node
      ns :: [Node] <- partial n2
      -- compute least common binder
      cb <- foldM lcb b1 $ b2: ns
      -- let's rebind the edge!
      -- delete original edge first
      fst <$> get >>= \g -> case bEdgeOut g n2 of
        [e] -> modify (first $ delLEdge e)
        _ -> fail "Property doesn't hold, every node should have exactly one binder"
      count <- fst <$> get >>= \g -> return . (+1) . length $ bEdgeIn g cb
      modify (first $ insEdge (n2, cb, GBind (min f1 f2) count Nothing))
      return n2

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
permission root n = do
  undefined

play :: Text -> IO ()
play str = do
  res <- PT.play typOperator str
  case res of
    Right t -> do
       (root, g) <- runToGraph [] empty t
       (tn, _) <- runRestore root g ([], 0)
       Text.putStrLn str
       putStrLn "original: "
       putStrLn $ show t
       putStrLn "Restored: "
       putStrLn $ show tn
    Left err -> putStrLn err

saveGraph :: (Show label, Show name, Labellable name, Ord name)
          => (Node, Gr (GNode (GNodeLabel lit label rep name)) (GEdge name))
          -> FilePath -> IO ()
saveGraph (root, g) name = do
  dot <- runDotGraph g root
  void $ runGraphviz dot Png (name <> ".png")
  writeDotFile (name <> ".dot") dot

runDotGraph :: (Show label, Show name, Labellable name, Ord name, Monad m)
            => Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)
            -> Node -> m (DotGraph Node)
runDotGraph g root = do
  mDot <- runReaderT (dotGraph root) g
  return $ digraph' mDot

dotGraph
  :: forall m lit label rep name
  . (Ord name, Show name, Labellable name, Show label, MonadReader (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m)
  => Node -> m (Dot Node)
dotGraph root = do
  nodes <- labNodes <$> ask
  let noded = foldl (>>) mempty $ aNode <$> nodes
  structures <- recEdgeS root
  bindings <- recEdgeB root
  return do
    edgeAttrs [rank SameRank, ordering OutEdges]
    structures
    bindings
    noded
  where
    aNode (n, label) = node n [toLabel label]
    aEdge (from, to, label) = edge from to $
      case label of
        GBind perm _ _ ->
          case perm of
            Rigid -> [style dashed]
            Flexible -> [style dotted]
            Explicit -> [style dotted]
          <> [toLabel label]
        _ -> [toLabel label]
    recEdgeS :: Node -> m (Dot Node)
    recEdgeS r = do
      g <- ask
      let edges = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ sEdgeOut g r
          subs = nub $ (\(_, to, _) -> to) <$> edges
      if subs == []
         then return mempty
         else do
           sNext <- sequence $ recEdgeS <$> subs
           return do foldl (>>) mempty (aEdge <$> edges)
                     foldl (>>) mempty sNext
    recEdgeB :: Node -> m (Dot Node)
    recEdgeB r = do
      g <- ask
      let edges = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ sEdgeOut g r
          bedges = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ filter (\(_,_,e) -> not $ isStructureEdge e) $ inn g r
          subs = nub $ (\(_, to, _) -> to) <$> edges
      bNext <- if edges == []
         then return mempty
         else fmap (foldl (>>) mempty) . sequence $ recEdgeB <$> subs
      return do foldl (>>) mempty (aEdge <$> bedges)
                bNext

-- | a way to visualize graphic type
viewType :: FilePath -> Text -> IO ()
viewType path str = do
  res <- PT.play typOperator str
  case res of
    Right t -> do
       (root, g) <- runToGraph [] empty t
       saveGraph (root, g) path
       ((), ng) <- runStateT (augGraph root) g
       saveGraph (root, ng) (path <> ".aug")
       (tn, _) <- runRestore root g ([], 0)
       Text.putStrLn str
       putStrLn "original: "
       putStrLn $ show t
       putStrLn "Restored: "
       putStrLn $ show tn
       prettyPrint g
    Left err -> putStrLn err

-- | a temporary function used to play unify in ghci REPL
testUnify :: FilePath -> Text -> IO ()
testUnify path str = do
  res <- PT.play typOperator str
  case res of
    Right t -> do
       (root, g) <- runToGraph [] empty t
       saveGraph (root, g) path
       putStrLn "the graph:"
       prettyPrint g
       n1 <- getLine >>= return . read @Int
       n2 <- getLine >>= return . read @Int
       ((), ng) <- runStateT (augGraph root) g
       (_, (simplify root -> gu, _)) <- runStateT (n1 ==? n2) (ng,[])
       saveGraph (root, ng) (path <> ".aug")
       saveGraph (root, simplify root gu) (path <> ".gu")
       (tn, _) <- runRestore root gu ([], 0)
       Text.putStrLn str
       putStrLn "original: "
       putStrLn $ show t
       putStrLn "Restored: "
       putStrLn $ show tn
    Left err -> putStrLn err

