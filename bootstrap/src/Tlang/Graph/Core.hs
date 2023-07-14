module Tlang.Graph.Core
  (
  -- ** Core structure
    Link (..)
  , Hole (..)

  -- ** Core graph
  , CoreG

  -- ** original constructor
  , Graph (..)

  -- ** helpers
  , hole, hole', link, link', lFrom, lTo, linkFrom, linkTo
  , isHole, isLink, getLink, filterLink
  , matchHole
  , (-<), (>-), edge, edges
  , (-<<), (>>-)
  , connect, overlay, overlays, vertex, vertices
  , hasVertex

  -- ** algorithms
  , reachable, linkable, transpose, dfs
  )
where

import qualified Algebra.Graph.AdjacencyMap as AdjacencyMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AdjacencyMap
import Algebra.Graph.Labelled (Graph (..), Context (..), context, edgeLabel, edgeList, replaceEdge, transpose, emap, foldg)
import qualified Algebra.Graph.Labelled as Algebra (edge, connect, overlay, vertices, overlays, edges)
import Data.Functor ((<&>))
import Data.Set (Set, singleton, toList, fromList)
import qualified Data.Set as Set (filter, member)
import Data.List (sort)

import Tlang.Generic ((:<:) (..))

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
type CoreG nodes edges info = Graph (Set (Link edges)) (Hole nodes info)

-- | construct new link
link :: sub :<: sup => sub (Link sup) -> Set (Link sup)
link = singleton . Link . inj
link' :: sub :<: sup => sub (Link sup) -> Link sup
link' = Link . inj
-- | construct new node
hole :: sub :<: sup => info -> sub (Hole sup info) -> Hole sup info
hole info v = Hole (inj v) info
hole' :: sub :<: sup => sub (Hole sup info) -> info -> Hole sup info
hole' v info = hole info v
{-# INLINE hole' #-}

infixl 5 -<, >-, -<<, >>-

-- | helpers for constructing arbitrary edges and nodes
(-<) :: (node :<: nodes, edge :<: edges)
     => (info, node (Hole nodes info))
     -> edge (Link edges) -> (Hole nodes info, Set (Link edges))
(-<) a = (uncurry hole a,) . link
(>-) :: (node :<: nodes)
     => (Hole nodes info, Set (Link edges))
     -> (info, node (Hole nodes info)) -> CoreG nodes edges info
(>-) (a, e) = Algebra.edge e a . uncurry hole

(-<<) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Set (Link edges))
(-<<) a e = (a, link e)
(>>-) :: (Hole nodes info, Set (Link edges)) -> Hole nodes info -> CoreG nodes edges info
(>>-) (a, e) = Algebra.edge e a

-- | reexport of algebra edge
edge :: forall edge a b edges nodes info. (a :<: nodes, b :<: nodes, edge :<: edges)
     => edge (Link edges)
     -> (info, a (Hole nodes info)) -> (info, b (Hole nodes info))
     -> CoreG nodes edges info
edge e a b = Algebra.edge (link e) (uncurry hole a) (uncurry hole b)
-- | reexport of algebra edges
edges :: forall edge edges nodes info. (edge :<: edges, Ord (edges (Link edges)))
      => [(edge (Link edges), Hole nodes info, Hole nodes info)]
      -> CoreG nodes edges info
edges es = Algebra.edges $ es >>= \(e, a, b) -> return (link e, a, b)

-- ** general graph operation, multiplication and addition

connect :: forall edge edges nodes info. (edge :<: edges)
        => edge (Link edges)
        -> CoreG nodes edges info
        -> CoreG nodes edges info
        -> CoreG nodes edges info
connect e = Algebra.connect (link e)

overlay :: Ord (edges (Link edges)) => CoreG nodes edges info -> CoreG nodes edges info -> CoreG nodes edges info
overlay = Algebra.overlay
{-# INLINE overlay #-}
overlays :: Ord (edges (Link edges)) => [CoreG nodes edges info] -> CoreG nodes edges info
overlays = Algebra.overlays
{-# INLINE overlays #-}

vertex :: forall node nodes info edges. node :<: nodes => (info, node (Hole nodes info)) -> CoreG nodes edges info
vertex = Vertex . uncurry hole
{-# INLINE vertex #-}

vertices :: Ord (edges (Link edges)) => [Hole nodes info] -> CoreG nodes edges info
vertices = Algebra.vertices
{-# INLINE vertices #-}

-- | query adjacent edges and nodes
linkFrom, linkTo :: (Ord (es (Link es)), Ord info, Ord (ns (Hole ns info)))
                 => (Hole ns info -> Bool) -> CoreG ns es info -> [(Link es, Hole ns info)]
linkFrom p g = sort $ edgeList g >>= \(es, a, b) -> if p a then toList es <&> (,b) else []
linkTo p g = sort $ edgeList g >>= \(es, a, b) -> if p b then toList es <&> (,a) else []

lFrom, lTo :: forall e es ns info. (e :<: es, Ord (es (Link es)), Ord info, Ord (ns (Hole ns info)))
           => (Hole ns info -> Bool) -> CoreG ns es info -> [(e (Link es), Hole ns info)]
lFrom p g = linkFrom p g >>= \(Link e, n) ->
  case prj @e e of
    Just v -> pure (v, n)
    Nothing -> mempty
lTo p g = linkTo p g >>= \(Link e, n) ->
  case prj @e e of
    Just v -> pure (v, n)
    Nothing -> mempty

matchHole
  :: forall node nodes info a. (node :<: nodes)
  => (Hole nodes info -> a)
  -> (node (Hole nodes info) -> info -> a)
  -> Hole nodes info
  -> a
matchHole f g node@(Hole (prj -> node'maybe) info) =
  case node'maybe of
    Just tag -> g tag info
    Nothing -> f node

-- | general predicate
isHole :: forall node nodes info. (node :<: nodes)
       => Hole nodes info
       -> (info -> node (Hole nodes info) -> Bool) -> Bool
isHole (Hole (prj -> node) info) predicate = maybe False (predicate info) node

isLink :: forall edge edges. (edge :<: edges)
       => Link edges
       -> (edge (Link edges) -> Bool) -> Bool
isLink (Link (prj -> e)) predicate = maybe False predicate e

-- | get links between two nodes, there may not exist links
getLink :: (Ord (edges (Link edges)), Eq info, Eq (nodes (Hole nodes info)))
        => Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> Set (Link edges)
getLink = edgeLabel
{-# INLINE getLink #-}

-- | filter out unmatched links between two nodes
filterLink :: (Ord info, Ord (nodes (Hole nodes info)), Ord (edges (Link edges)))
           => (Link edges -> Bool)
           -> Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
filterLink predicate n1 n2 g = replaceEdge (Set.filter predicate $ getLink n1 n2 g) n1 n2 g

-- | predicates
hasVertex :: (Hole nodes info -> Bool) -> CoreG nodes edges info -> Bool
hasVertex p = foldg False p (const (||))

-- ** algorithms

-- | get a collection of reachable nodes, with a predicate to select valid edge
reachable :: (Ord info, Ord (ns (Hole ns info))) => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Set (Hole ns info)
reachable p g n = fromList $ dfs p g n
{-# INLINE reachable #-}

-- | get a collection of reachable nodes, with a predicate to select valid edge
dfs :: (Ord info, Ord (ns (Hole ns info))) => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> [Hole ns info]
dfs p g n = flip AdjacencyMap.dfs [n] . foldg AdjacencyMap.empty AdjacencyMap.vertex (const AdjacencyMap.connect)
          $ emap (Set.filter p) g

-- | test whether one node is reachable via specified edges
linkable :: (Ord info, Ord (ns (Hole ns info))) => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Hole ns info -> Bool
linkable p g from to = Set.member to $ reachable p g from
