module Tlang.Inference.Graph

where

import Tlang.AST

import Data.Graph.Inductive
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), modify)
import Control.Monad
import Control.Monad.RWS (RWST (..))
import Data.Functor.Foldable
import Data.Bifunctor (first)

data GraphRel
  = EqualRel
  | GraftRel
  | MergeRel
  | RaiseRel
  | WeakRel
  deriving (Ord, Show, Eq)

class GraphType t where
  instof :: GraphRel -> t -> t -> Bool

-- | label for edge
data GEdge a
  = GSub Int -- a direct link node, with number attached
  | GBind Perm Int (Maybe a) -- binding edge with number attached and also the origin name, it may or may not be useful.
  deriving (Show, Eq, Ord)

data Perm
  = Rigid
  | Flexible
  | Explicit
  deriving (Show, Eq, Ord)

-- | label for node
data GNode a
  = GRoot -- the empty node
  | GNode a -- node labeled by `a`
  deriving (Show, Eq, Ord, Functor)

-- | actual label for Type, doesn't support predicate constraint
data GNodeLabel lit label rep name
  = NodeRef name  -- type ref node, used to hold concrete type
  | NodeSum -- has multiple labels, and each label may have one type
  | NodeRec -- has multiple lables, and each label must have one type
  | NodeTup -- has as many sub nodes (graphs) as user want, preserve the order
  | NodeHas label -- type label node, depends on its parent, can have one or no sub graph
  | NodeRep rep -- representation node
  | NodeApp -- application node, minimal arity 2
  | NodeAbs -- abstraction node, arity 2
  | NodeUni -- unit type node
  | NodeLit lit -- literal type node
  | NodeBot -- special bottom node
  deriving (Show, Eq)

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
      modify $ insNode (nid, GNode node)
      return nid
    newEdge label n1 n2 = modify $ insEdge (n1, n2, label)
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
        name :> mr -> (name,) <$> mr
        name :~ mr -> (name,) <$> mr
      rnode <- local (pair:) mr
      newEdge (GBind Explicit 1 (Just name)) node absNode
      newEdge (GSub 1) absNode node
      newEdge (GSub 2) absNode rnode
      return absNode
    go (TypLiftF _) = fail "type annotation is not supported"

instance GraphType (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name))

-- | node label for term
data G
