{-# LANGUAGE AllowAmbiguousTypes #-}
{- | Graph related definition
--
-- We use definitions here as core structure for graph and
-- use `Graph.Extension` to assign meaning to graph.
--
-- This module contains common operations on graph and also
-- some simple algorithms like dfs.
--
-- TODO: create a compact representation of graph
-}
module Graph.Core
  (
  -- ** Core structure
    Link (..)
  , Hole (..)

  -- ** Core graph
  , CoreG
  , HasOrderGraph
  , HasOrderNode
  , HasOrderEdge
  , HasEqGraph
  , HasEqNode
  , HasEqEdge

  -- ** original constructor
  , Algebra.Graph (..)

  -- ** helpers
  , hole, link, link', lFrom, lTo, linkFrom, linkTo
  , isHole, isLink, isLinkOf, getLink, filterLink
  , maybeHole, maybeLink
  , edge, edges
  , (-<<), (>>-)
  , (-++), (++-)
  , connect, overlay, overlays, vertex, vertices
  , hasVertex
  , Algebra.induce
  , induceLink, replaceLink

  -- ** algorithms
  , reachable, linkable, Algebra.transpose, dfs, isHoleOf, compress, linkList
  )
where

import qualified Algebra.Graph.Labelled as Algebra
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad ((<=<))

import Language.Generic ((:<:), inj, prj)
import Data.Maybe (maybeToList)
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Lens ( (^..), _1 )

-- | algebraic edge for graph
newtype Link e = Link (e (Link e))
deriving instance (Show (e (Link e))) => Show (Link e)
deriving instance (Eq (e (Link e))) => Eq (Link e)
deriving instance (Ord (e (Link e))) => Ord (Link e)

-- | generic node for graph
data Hole e a = Hole (e (Hole e a)) a deriving (Functor, Foldable, Traversable)
deriving instance (Show (e (Hole e a)), Show a) => Show (Hole e a)
deriving instance (Eq (e (Hole e a)), Eq a) => Eq (Hole e a)
deriving instance (Ord (e (Hole e a)), Ord a) => Ord (Hole e a)

-- ** core structure and method for graph unification

-- | TODO: use this implementation as default algebraic graph
data Graph e a
  = Empty
  | Vertex a
  | Connect e (Graph e a) (Graph e a)
  deriving (Show, Eq, Ord, Functor)
deriveBifunctor ''Graph
makeBaseFunctor ''Graph

-- | core structure for graphic representation of syntactic type
type CoreG nodes edges info = Algebra.Graph (Set.Set (Link edges)) (Hole nodes info)

type HasOrderGraph nodes edges info = (HasOrderNode nodes info, HasOrderEdge edges)
type HasOrderNode nodes info = (Ord info, Ord (nodes (Hole nodes info)))
type HasOrderEdge edges = Ord (edges (Link edges))

type HasEqGraph nodes edges info = (HasEqNode nodes info, Eq (edges (Link edges)))
type HasEqNode nodes info = (Eq info, Eq (nodes (Hole nodes info)))
type HasEqEdge edges = Eq (edges (Link edges))

-- | construct new link
link :: e :<: edges => e (Link edges) -> Set.Set (Link edges)
link = Set.singleton . Link . inj
link' :: e :<: edges => e (Link edges) -> Link edges
link' = Link . inj

-- | construct new node
hole :: node :<: nodes => node (Hole nodes info) -> info -> Hole nodes info
hole v = Hole (inj v)
{-# INLINE hole #-}

infixl 5 -<<, >>-, -++, ++-

-- | helpers for constructing arbitrary edges and nodes

(-<<) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Set.Set (Link edges))
(-<<) a e = (a, link e)
(>>-) :: (Hole nodes info, Set.Set (Link edges)) -> Hole nodes info -> CoreG nodes edges info
(>>-) (a, e) = Algebra.edge e a

(-++) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Set.Set (Link edges))
(-++) a e = (a, link e)
(++-) :: (Ord (edges (Link edges))) => (Hole nodes info, Set.Set (Link edges)) -> Hole nodes info -> CoreG nodes edges info
(++-) (a, e) b = overlay (Algebra.edge e a b) (Algebra.edge e b a)

-- | re-export of algebra edge
edge :: (edge :<: edges)
     => edge (Link edges)
     -> Hole nodes info -> Hole nodes info
     -> CoreG nodes edges info
edge e = Algebra.edge (link e)
{-# INLINE edge #-}

-- | re-export of algebra edges
edges :: (edge :<: edges, HasOrderEdge edges)
      => [(edge (Link edges), Hole nodes info, Hole nodes info)]
      -> CoreG nodes edges info
edges es = Algebra.edges $ es >>= \(e, a, b) -> return (link e, a, b)
{-# INLINE edges #-}

-- ** general graph operation, multiplication and addition

connect :: forall edge edges nodes info. (edge :<: edges)
        => edge (Link edges)
        -> CoreG nodes edges info
        -> CoreG nodes edges info
        -> CoreG nodes edges info
connect e = Algebra.connect (link e)

overlay :: HasOrderEdge edges => CoreG nodes edges info -> CoreG nodes edges info -> CoreG nodes edges info
overlay = Algebra.overlay
{-# INLINE overlay #-}
overlays :: HasOrderEdge edges => [CoreG nodes edges info] -> CoreG nodes edges info
overlays = Algebra.overlays
{-# INLINE overlays #-}

vertex :: node :<: nodes => node (Hole nodes info) -> info -> CoreG nodes edges info
vertex = fmap Algebra.Vertex . hole
{-# INLINE vertex #-}

vertices :: HasOrderEdge edges => [Hole nodes info] -> CoreG nodes edges info
vertices = Algebra.vertices
{-# INLINE vertices #-}

-- | query adjacent edges and nodes
linkFrom, linkTo :: (HasOrderGraph ns es info)
                 => (Hole ns info -> Bool) -> CoreG ns es info -> [(Link es, Hole ns info)]
linkFrom p g = List.sort $ List.nub $ [(e, a)|(es, a) <- (Algebra.outputs <=< maybeToList) $ Algebra.context p g, e <- Set.toList es]
linkTo p g = List.sort $ List.nub $ [(e, a)|(es, a) <- (Algebra.inputs <=< maybeToList) $ Algebra.context p g, e <- Set.toList es]

lFrom, lTo :: (e :<: es, HasOrderGraph ns es info)
           => (Hole ns info -> Bool) -> CoreG ns es info -> [(e (Link es), Hole ns info)]
lFrom p g = [(v, n)| (Link e, n) <- linkFrom p g, Just v <- [prj e]]
lTo p g = [(v, n)| (Link e, n) <- linkTo p g, Just v <- [prj e]]

-- edgeList :: CoreG ns es info -> [(Set.Set (Link es), Hole ns info, Hole ns info)]
-- edgeList = undefined

linkList :: (HasOrderGraph ns es info) => CoreG ns es info -> [(Link es, Hole ns info, Hole ns info)]
linkList gr = List.sort $ [(e, a, b) | (es, a, b) <- Algebra.edgeList gr, e <- Set.toList es]

-- | compress a graph, O(order(gr))
compress :: HasOrderGraph ns es info => CoreG ns es info -> CoreG ns es info
compress gr =
  let res = [ (e, a, b) | (es, a, b) <- Algebra.edgeList gr, e <- Set.toList es]
  in overlays [ gr' | es <- groupFst (sortFst res), gr' <- pure . overlays $ merge <$> groupSnd (sortSnd es)]
  where
    sortFst = List.sortBy (\(_, a, _) (_, b, _) -> compare a b)
    groupFst = List.groupBy (\(_, a, _) (_, b, _) -> a == b)
    sortSnd = List.sortBy (\(_, _, a) (_, _, b) -> compare a b)
    groupSnd = List.groupBy (\(_, _, a) (_, _, b) -> a == b)
    merge [] = Algebra.Empty
    merge ((e, a, b):xs)
      = Algebra.Connect (Set.fromList . (e:) $ xs ^.. traverse . _1)
        (Algebra.Vertex a) (Algebra.Vertex b)

-- | act like `maybe` function but test against `Hole`
maybeHole
  :: (node :<: nodes)
  => (Hole nodes info -> a)
  -> (Hole nodes info -> node (Hole nodes info) -> info -> a)
  -> Hole nodes info
  -> a
maybeHole f g node@(Hole (prj -> node'maybe) info)
  = maybe (f node) (`g'` info) node'maybe
  where g' = g node

-- | general predicate for Hole
isHole :: (node :<: nodes) => Hole nodes info -> (node (Hole nodes info) -> info -> Bool) -> Bool
isHole node p = maybeHole (const False) (const p) node
{-# INLINE isHole #-}

isHoleOf :: forall node nodes info. (node :<: nodes) => Hole nodes info -> Bool
isHoleOf = maybeHole @node (const False) (\ _ _ _ -> True)

-- | act like `maybe` function but test against `Link`
maybeLink :: edge :<: edges => (Link edges -> a) -> (edge (Link edges) -> a) -> Link edges -> a
maybeLink f g l@(Link (prj -> e'Maybe)) = maybe (f l) g e'Maybe

-- | general predicate for Link
isLink :: (edge :<: edges) => (edge (Link edges) -> Bool) -> Link edges -> Bool
isLink predicate (Link (prj -> e)) = maybe False predicate e

-- | use type level marker to check edge kind
isLinkOf :: forall edge edges. (edge :<: edges) => Link edges -> Bool
isLinkOf = isLink @edge (const True)
{-# INLINE isLinkOf #-}

-- | get links between two nodes, there may not exist links
getLink :: (HasOrderEdge edges, HasEqNode nodes info)
        => Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> Set.Set (Link edges)
getLink = Algebra.edgeLabel
{-# INLINE getLink #-}

-- | filter out unmatched links between two nodes
filterLink
  :: HasOrderGraph nodes edges info
  => (Link edges -> Bool)
  -> Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
filterLink predicate n1 n2 g = Algebra.replaceEdge (Set.filter predicate $ getLink n1 n2 g) n1 n2 g

-- | keep checked link
induceLink :: (Link edges -> Bool)
           -> CoreG nodes edges info -> CoreG nodes edges info
induceLink p = Algebra.emap (Set.filter p)
{-# INLINE induceLink #-}

-- | replace existed links between two nodes into others and it doesn't create
-- links if there are no links between them.
--
-- You can also use it as "removeLink".
--
-- @@@
-- -- remove all links from "a" to "b" in grpah "gr"
-- replaceLink (const Nothing) a b gr
-- @@@
replaceLink
  :: HasOrderGraph nodes es info
  => (Link es -> Maybe (Link es))
  -> Hole nodes info -> Hole nodes info -> CoreG nodes es info -> CoreG nodes es info
replaceLink f a b gr = Algebra.replaceEdge (Set.fromList $ (>>= maybeToList . f) $ Set.toList $ getLink a b gr) a b gr

-- | predicates
hasVertex :: (Hole nodes info -> Bool) -> CoreG nodes edges info -> Bool
hasVertex p = Algebra.foldg False p (const (||))

-- ** algorithms

-- | get a collection of reachable nodes, with a predicate to select valid edge
reachable :: HasOrderGraph ns es info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Set.Set (Hole ns info)
reachable p g n = Set.fromList $ dfs p g n
{-# INLINE reachable #-}

-- | get a collection of reachable nodes, with a predicate to select valid routing edge
dfs :: HasOrderGraph ns es info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> [Hole ns info]
dfs p gr start = reverse $ go [n | (e, n) <- linkFrom (== start) gr, p e] [start]
  where
    go [] visited = visited
    go (n:ns) visited =
      if n `elem` visited
      then go ns visited
      else go (ns <> [node | (e, node) <- linkFrom (== n) gr, p e])
              (n:visited)

-- | test whether one node is reachable via specified edges
linkable :: HasOrderGraph ns es info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Hole ns info -> Bool
linkable p g from to = Set.member to $ reachable p g from
