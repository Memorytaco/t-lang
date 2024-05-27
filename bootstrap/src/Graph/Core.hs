{-# LANGUAGE AllowAmbiguousTypes, PartialTypeSignatures #-}
{- | Graph related definition
--
-- We use definitions here as core structure for graph and
-- use `Graph.Extension` to assign meaning to graph.
--
-- This module contains common operations on graph and also
-- some simple algorithms like dfs.
--
-- TODO: create a compact representation of graph, refine definition of functions.
-- TODO: each function here should have a clear meaning and well defined property.
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

  -- ** rexport graph constructor
  , Graph (..)
  , GraphF (..)

  -- ** helpers
  , hole, link, lFrom, lFroms, lTo, lTos, linkFrom, linkTo
  , isHole, isLink, isLinkOf, filterLink
  , tryHole, tryLink
  , isHoleOf, areHolesOf
  , fromEdge, fromEdges, toEdges
  , (-<<), (>>-)
  , (-++), (++-)
  , connect, overlay, overlays, vertex, fromVertices, toVertices
  , hasVertex
  , induce
  , einduce
  , induceLink, replaceLink

  -- ** algorithms
  , reachable, linkable, transpose, dfs, bfs, compress
  , splitHole
  )
where

import Language.Generic.Subsume ( (:<:), inj, prj, (:~:), split )

import qualified Graph.Data as Algebra
import Graph.Data (Graph (..), GraphF (..), induce, einduce, transpose, order, outFrom, inTo, overlay, overlays, fromVertices)
import qualified Graph.Algorithm as Algorithm
import Language.Generic ((:+:))

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

-- | core structure for graphic representation of syntactic type
type CoreG nodes edges info = Algebra.Graph (Link edges) (Hole nodes info)

type HasOrderGraph nodes edges info = (HasOrderNode nodes info, HasOrderEdge edges)
type HasOrderNode nodes info = (Ord info, Ord (nodes (Hole nodes info)))
type HasOrderEdge edges = Ord (edges (Link edges))

type HasEqGraph nodes edges info = (HasEqNode nodes info, HasEqEdge edges)
type HasEqNode nodes info = (Eq info, Eq (nodes (Hole nodes info)))
type HasEqEdge edges = Eq (edges (Link edges))

-- | construct new link
link :: e :<: edges => e (Link edges) -> Link edges
link = Link . inj

-- | construct new node
hole :: node :<: nodes => node (Hole nodes info) -> info -> Hole nodes info
hole = Hole . inj
{-# INLINE hole #-}

infixl 5 -<<, >>-, -++, ++-

-- | helpers for constructing arbitrary edges and nodes

-- | directed edge link.
(-<<) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Link edges)
(-<<) a e = (a, link e)
(>>-) :: (Hole nodes info, Link edges) -> Hole nodes info -> CoreG nodes edges info
(>>-) (a, e) = Algebra.fromEdge e a

-- | undirected edge link.
(-++) :: edge :<: edges => Hole nodes info -> edge (Link edges) -> (Hole nodes info, Link edges)
(-++) a e = (a, link e)
(++-) :: (Hole nodes info, Link edges) -> Hole nodes info -> CoreG nodes edges info
(++-) (a, e) b = overlay (Algebra.fromEdge e a b) (Algebra.fromEdge e b a)

-- | re-export of algebra edge
fromEdge :: edge :<: edges => edge (Link edges) -> Hole nodes info -> Hole nodes info -> CoreG nodes edges info
fromEdge = Algebra.fromEdge . link

-- | get edges from graph with order and no duplication.
toEdges :: HasOrderGraph nodes edges info => CoreG nodes edges info -> [(Link edges, Hole nodes info, Hole nodes info)]
toEdges = order . Algebra.toEdges

-- | re-export of algebra edges
fromEdges :: edge :<: edges => [(edge (Link edges), Hole nodes info, Hole nodes info)] -> CoreG nodes edges info
fromEdges es = Algebra.fromEdges $ es >>= \(e, a, b) -> return (link e, a, b)

-- | get nodes from graph with order and no duplication.
toVertices :: HasOrderGraph nodes edges info => CoreG nodes edges info -> [Hole nodes info]
toVertices = order . Algebra.toVertices

-- ** general graph operation, multiplication and addition

connect :: forall edge edges nodes info. edge :<: edges
        => edge (Link edges)
        -> CoreG nodes edges info
        -> CoreG nodes edges info
        -> CoreG nodes edges info
connect e = Algebra.connect (link e)

vertex :: node :<: nodes => node (Hole nodes info) -> info -> CoreG nodes edges info
vertex tag info = Algebra.Vertex (hole tag info)

-- | query out edges and nodes, with ascendent order.
linkFrom :: HasOrderGraph ns es info
         => (Hole ns info -> Bool) -> CoreG ns es info -> [(Hole ns info, (Link es, Hole ns info))]
linkFrom p = order . outFrom p

-- | query in edges and nodes, with ascendent order.
linkTo :: HasOrderGraph ns es info
       => (Hole ns info -> Bool) -> CoreG ns es info -> [((Hole ns info, Link es), Hole ns info)]
linkTo p = order . inTo p

-- | query out edges and nodes, with ascendent order. Filter on edge label with type.
lFrom :: (e :<: es, Ord (e (Link es)), HasOrderGraph ns es info)
      => Hole ns info -> CoreG ns es info -> [(e (Link es), Hole ns info)]
lFrom n g = order [(e, b) | (_, (Link tag, b)) <- linkFrom (== n) g, Just e <- [prj tag]]

-- | query out edges and nodes, with ascendent order. Filter on edge label with type.
lFroms :: (e :<: es, Ord (e (Link es)), HasOrderGraph ns es info)
      => [Hole ns info] -> CoreG ns es info -> [(e (Link es), Hole ns info)]
lFroms ns g = order [(e, b) | (_, (Link tag, b)) <- linkFrom (`elem` ns) g, Just e <- [prj tag]]

-- | query in edges and nodes, with ascendent order. Filter on edge label with type.
lTo :: (e :<: es, Ord (e (Link es)), HasOrderGraph ns es info)
    => Hole ns info -> CoreG ns es info -> [(Hole ns info, e (Link es))]
lTo n g = order [(a, e) | ((a, Link tag), _) <- linkTo (== n) g, Just e <- [prj tag]]


lTos :: (e :<: es, Ord (e (Link es)), HasOrderGraph ns es info)
    => [Hole ns info] -> CoreG ns es info -> [(Hole ns info, e (Link es))]
lTos ns g = order [(a, e) | ((a, Link tag), _) <- linkTo (`elem` ns) g, Just e <- [prj tag]]

-- | compress a graph, O(size(gr))
--
-- TODO: complete algorithm
compress :: HasOrderGraph ns es info => CoreG ns es info -> CoreG ns es info
compress gr =
  let edges = toEdges gr
      vertices = toVertices gr
  in overlay (Algebra.fromEdges edges) (fromVertices vertices)

-- | act like `maybe` function but test against `Hole`
tryHole
  :: (node :<: nodes)
  => (Hole nodes info -> a)
  -> (Hole nodes info -> node (Hole nodes info) -> info -> a)
  -> Hole nodes info
  -> a
tryHole fls tru node@(Hole (prj -> node'maybe) info)
  = maybe (fls node) (`g'` info) node'maybe
  where g' = tru node

-- | `split` applied to Hole structure
splitHole
  :: ((fls :+: tru) :~: nodes)
  => (Hole nodes info -> info -> fls (Hole nodes info) -> a)
  -> (Hole nodes info -> info -> tru (Hole nodes info) -> a)
  -> Hole nodes info -> a
splitHole fls tru n@(Hole tag info)
  = split (fls n info) (tru n info) tag

-- | general predicate for Hole
isHole :: node :<: nodes => Hole nodes info -> (node (Hole nodes info) -> info -> Bool) -> Bool
isHole node p = tryHole (const False) (const p) node
{-# INLINE isHole #-}

isHoleOf :: forall node nodes info. (node :<: nodes) => Hole nodes info -> Bool
isHoleOf = tryHole @node (const False) (\ _ _ _ -> True)

areHolesOf :: forall tru fls nodes info. (fls :+: tru) :~: nodes => Hole nodes info -> Bool
areHolesOf = splitHole (\_ _ (_ :: fls _) -> False) (\_ _ (_ :: tru _) -> True)

-- | act like `maybe` function but test against `Link`
tryLink :: edge :<: edges => (Link edges -> a) -> (edge (Link edges) -> a) -> Link edges -> a
tryLink fls tru l@(Link (prj -> e)) = maybe (fls l) tru e

-- | general predicate for Link
isLink :: (edge :<: edges) => (edge (Link edges) -> Bool) -> Link edges -> Bool
isLink p (Link (prj -> e)) = maybe False p e

-- | use type level marker to check edge kind
isLinkOf :: forall edge edges. (edge :<: edges) => Link edges -> Bool
isLinkOf = isLink @edge (const True)
{-# INLINE isLinkOf #-}

-- | filter out unmatched links between two nodes
filterLink
  :: HasOrderGraph nodes edges info
  => (Link edges -> Bool)
  -> Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
filterLink predicate n1 n2 = einduce \e a b ->
  (if n1 == a && n2 == b then (if predicate e then Just (e, a, b) else Nothing) else Just (e, a, b))

-- | keep checked link
induceLink :: HasEqGraph nodes edges info => (Link edges -> Bool)
           -> CoreG nodes edges info -> CoreG nodes edges info
induceLink p = einduce \e a b -> if p e then Just (e, a, b) else Nothing
{-# INLINE induceLink #-}

-- | replace existed links between two nodes into others and it doesn't create
-- links if there are no links between them.
--
-- You can also use it as "removeLink".
--
-- @
-- -- remove all links from "a" to "b" in grpah "gr"
-- replaceLink (const Nothing) a b gr
-- @
replaceLink
  :: HasEqGraph nodes es info
  => (Link es -> Maybe (Link es))
  -> Hole nodes info -> Hole nodes info -> CoreG nodes es info -> CoreG nodes es info
replaceLink f m n = einduce \e a b -> if m == a && n == b then (,a,b) <$> f e else Just (e, a, b)

-- | predicates
hasVertex :: (Hole nodes info -> Bool) -> CoreG nodes edges info -> Bool
hasVertex = any

reachable :: HasEqGraph ns es info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> [Hole ns info]
reachable = dfs
{-# INLINE reachable #-}

-- ** algorithms

-- | depth first search, with a predicate to select valid routing edge
dfs :: Eq a => (t -> Bool) -> Graph t a -> a -> [a]
dfs p gr start = Algorithm.dfs (const [start]) (\_g (os, _) -> [a | (_, (e, a)) <- os, p e]) gr

-- | breadth first search, with a predicate to select valid routing edge
bfs :: Eq a => (t -> Bool) -> a -> Graph t a -> [a]
bfs p start = Algorithm.bfs (const [start]) (\_g (os, _) -> [a | (_, (e, a)) <- os, p e])

-- | test whether one node is reachable via specified edges
linkable :: HasOrderGraph ns es info => (Link es -> Bool) -> CoreG ns es info -> Hole ns info -> Hole ns info -> Bool
linkable p g from to = elem to $ dfs p g from
