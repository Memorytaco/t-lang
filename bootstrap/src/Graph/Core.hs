{-# LANGUAGE AllowAmbiguousTypes #-}
{- | Graph related definition
--
-- We use definitions here as core structure for graph and
-- use `Graph.Extension` to assign meaning to graph.
--
-- This module contains common operations on graph and also
-- some simple algorithms like dfs.
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
  , maybeHole
  , edge, edges
  , (-<<), (>>-)
  , (-++), (++-)
  , connect, overlay, overlays, vertex, vertices
  , hasVertex
  , Algebra.induce
  , induceLink, replaceLink

  -- ** algorithms
  , reachable, linkable, Algebra.transpose, dfs
  )
where

import qualified Algebra.Graph.AdjacencyMap as AdjacencyMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AdjacencyMap
import qualified Algebra.Graph.Labelled as Algebra
import Data.Set (Set, singleton, toList, fromList)
import qualified Data.Set as Set (filter, member)
import Data.List (sort)

import Language.Generic ((:<:), inj, prj)
import Data.Maybe (maybeToList)

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

-- | core structure for graphic representation of syntactic type
type CoreG nodes edges info = Algebra.Graph (Set (Link edges)) (Hole nodes info)

type HasOrderGraph nodes edges info = (HasOrderNode nodes info, HasOrderEdge edges)
type HasOrderNode nodes info = (Ord info, Ord (nodes (Hole nodes info)))
type HasOrderEdge edges = Ord (edges (Link edges))

type HasEqGraph nodes edges info = (HasEqNode nodes info, Eq (edges (Link edges)))
type HasEqNode nodes info = (Eq info, Eq (nodes (Hole nodes info)))
type HasEqEdge edges = Eq (edges (Link edges))

-- | construct new link
link :: e :<: edges => e (Link edges) -> Set (Link edges)
link = singleton . Link . inj
link' :: e :<: edges => e (Link edges) -> Link edges
link' = Link . inj

-- | construct new node
hole :: node :<: nodes => node (Hole nodes info) -> info -> Hole nodes info
hole v = Hole (inj v)
{-# INLINE hole #-}

infixl 5 -<<, >>-, -++, ++-

-- | helpers for constructing arbitrary edges and nodes

(-<<) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Set (Link edges))
(-<<) a e = (a, link e)
(>>-) :: (Hole nodes info, Set (Link edges)) -> Hole nodes info -> CoreG nodes edges info
(>>-) (a, e) = Algebra.edge e a

(-++) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Set (Link edges))
(-++) a e = (a, link e)
(++-) :: (Ord (edges (Link edges))) => (Hole nodes info, Set (Link edges)) -> Hole nodes info -> CoreG nodes edges info
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
linkFrom p g = sort [(e, b)|(es, a, b) <- Algebra.edgeList g, p a, e <- toList es]
linkTo p g = sort [(e, a)|(es, a, b) <- Algebra.edgeList g, p b, e <- toList es]

lFrom, lTo :: (e :<: es, HasOrderGraph ns es info)
           => (Hole ns info -> Bool) -> CoreG ns es info -> [(e (Link es), Hole ns info)]
lFrom p g = [(v, n)| (Link e, n) <- linkFrom p g, Just v <- [prj e]]
lTo p g = [(v, n)| (Link e, n) <- linkTo p g, Just v <- [prj e]]

-- | act like `maybe` function but test against `Hole`
maybeHole
  :: (node :<: nodes)
  => (Hole nodes info -> a)
  -> (Hole nodes info -> node (Hole nodes info) -> info -> a)
  -> Hole nodes info
  -> a
maybeHole f g node@(Hole (prj -> node'maybe) info) =
  case node'maybe of
    Just tag -> g node tag info
    Nothing -> f node

-- | general predicate
isHole :: (node :<: nodes) => Hole nodes info -> (node (Hole nodes info) -> info -> Bool) -> Bool
isHole node p = maybeHole (const False) (const p) node
{-# INLINE isHole #-}

isLink :: (edge :<: edges) => Link edges -> (edge (Link edges) -> Bool) -> Bool
isLink (Link (prj -> e)) predicate = maybe False predicate e

-- | use type level marker to check edge kind
isLinkOf :: forall edge edges. (edge :<: edges) => Link edges -> Bool
isLinkOf e = isLink @edge e (const True)
{-# INLINE isLinkOf #-}

-- | get links between two nodes, there may not exist links
getLink :: (HasOrderEdge edges, HasEqNode nodes info)
        => Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> Set (Link edges)
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
replaceLink f a b gr = Algebra.replaceEdge (fromList $ (>>= maybeToList . f) $ toList $ getLink a b gr) a b gr

-- | predicates
hasVertex :: (Hole nodes info -> Bool) -> CoreG nodes edges info -> Bool
hasVertex p = Algebra.foldg False p (const (||))

-- ** algorithms

-- | get a collection of reachable nodes, with a predicate to select valid edge
reachable :: HasOrderNode ns info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Set (Hole ns info)
reachable p g n = fromList $ dfs p g n
{-# INLINE reachable #-}

-- | get a collection of reachable nodes, with a predicate to select valid edge
dfs :: HasOrderNode ns info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> [Hole ns info]
dfs p g n = flip AdjacencyMap.dfs [n] . Algebra.foldg AdjacencyMap.empty AdjacencyMap.vertex (const AdjacencyMap.connect)
          $ Algebra.emap (Set.filter p) g

-- | test whether one node is reachable via specified edges
linkable :: HasOrderNode ns info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Hole ns info -> Bool
linkable p g from to = Set.member to $ reachable p g from
