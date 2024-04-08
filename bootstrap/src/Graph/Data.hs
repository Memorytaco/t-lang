module Graph.Data
  ( -- ** data structure
    Graph (..)  -- ^ Directed Grpah
  , UGraph (..) -- ^ Undirected Graph
  , GraphF (..)

  , Many (..)
  , MGraph (..) -- ^ MultiEdged Graph

    -- ** implementation optimization and property
  , depth, size
  , prune
  , collapse
  , balance

    -- ** A helpful definition of 'order' to help us sort out necessary information
  , order

    -- ** connects
  , overlays
  , connects

    -- ** transformation between graphs
  , toUndirected

    -- ** methods
  , toVertices
  , fromVertices
  , toEdges
  , fromEdges

  , outFrom
  , inTo

  , removeEdge
  , einduce
  )
where

import Data.Bifunctor.TH (deriveBifunctor)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable
import Control.Comonad.Cofree ( Cofree(..) )
import Data.List (nub, (\\))
import qualified Data.Foldable as Fold (Foldable (toList))
import qualified Data.Set as Set (toAscList, fromList)
import Utility.Operator ((<!>))

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary)

-- ** core structure and method for graph unification

-- | TODO: use this implementation as default algebraic graph
data Graph e a
  = None
  | Vertex a
  | Overlay (Graph e a) (Graph e a)
  | Connect e (Graph e a) (Graph e a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
deriveBifunctor ''Graph

-- | base structure for ''Graph
makeBaseFunctor ''Graph

instance Applicative (Graph e) where
  pure = Vertex
  None <*> _ = None
  (Vertex f) <*> g = f <$> g
  (Overlay fa fb) <*> g = Overlay (fa <*> g) (fb <*> g)
  (Connect e fa fb) <*> g = Connect e (fa <*> g) (fb <*> g)

instance Monad (Graph e) where
  None >>= _ = None
  (Vertex a) >>= f = f a
  (Overlay ma mb) >>= f = Overlay (ma >>= f) (mb >>= f)
  (Connect e ma mb) >>= f = Connect e (ma >>= f) (mb >>= f)

-- For generating property check, instance of Arbitrary
instance (Arbitrary e, Arbitrary a) => Arbitrary (Graph e a) where
  arbitrary = QC.sized gen
    where gen 0 = return None
          gen n = QC.oneof
            [ Vertex <$> rn (n-1)
            , Overlay <$> rn (n `div` 2) <*> rn (n `div` 2)
            , Connect <$> rn (n-1) <*> rn (n `div` 2) <*> rn (n `div` 2)
            ]
            where rn s = QC.resize s QC.arbitrary

-- | one implementation for edge label
newtype Many e = Many (Cofree Maybe e)
  deriving (Show, Eq, Ord, Functor, Applicative, Monad, Foldable, Traversable)

instance Eq e => Semigroup (Many e) where
  -- complexity O(n)
  (Many (a :< Nothing)) <> (Many b) = Many (a :< Just b)
  (Many (a :< (Just as))) <> b =
    case Many as <> b of
      Many x -> Many (a :< Just x)

-- | undirected graph
newtype UGraph a = UGraph (Graph () a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

-- convert a directed graph to undirected graph
toUndirected :: Graph e a -> UGraph a
toUndirected = UGraph . cata alg
  where
    alg NoneF = None
    alg (VertexF a) = Vertex a
    alg (OverlayF a b) = Overlay a b
    alg (ConnectF _ a b) = Connect () a b

-- | Multiple edge graph
newtype MGraph e a = MGraph (Graph (Many e) a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | strong order of the list with no duplication and is sorted.
--
-- it has complexity of O(n*log(n))
order :: Ord a => [a] -> [a]
order = Set.toAscList . Set.fromList

-- | calculate depth of graph implementation with complexity of O(size(tree))
--
-- e.g.
--
-- @
-- depth None == 0
-- depth (Vertex 2) == 0
-- depth (Overlay (Vertex 0) None) == 1
-- depth (Connect () (Vertex 0) None) == 1
-- @
--
-- It is a metric for measuring implementation performance.
depth :: (Ord n, Num n) => Graph e a -> n
depth = cata alg
  where alg NoneF = 0
        alg (VertexF _) = 0
        alg (OverlayF a b)
          | a > b = a + 1
          | otherwise = b + 1
        alg (ConnectF _ a b)
          | a > b = a + 1
          | otherwise = b + 1

-- | number of vertices. A metric for measuring implementation performance.
size :: Num n => Graph e a -> n
size = fromInteger . toInteger . length

-- | remove redundant empty vertex, O(depth(g))
--
-- This operation helps reduce size of structure and is irreversible.
-- It destructs underlying structure and make it smaller.
prune :: Graph e a -> Graph e a
prune = cata go
  where go NoneF = None
        go (VertexF a) = Vertex a
        go (OverlayF None a) = a
        go (OverlayF a None) = a
        go (OverlayF a b) = Overlay a b
        go (ConnectF _ None a) = a
        go (ConnectF _ a None) = a
        go (ConnectF e a b) = Connect e a b

-- | TODO: Optimization
overlays :: [Graph e a] -> Graph e a
overlays = foldr Overlay None

-- | TODO optimise usage of memory
balance :: (Eq a, Eq e) => Graph e a -> Graph e a
balance = cata alg
  where alg NoneF = None
        alg (VertexF a) = Vertex a
        alg (OverlayF a b)
          | a == b = a
          | otherwise = Overlay a b
        alg (ConnectF e a b) = Connect e a b

-- | TODO: Optimization
connects :: [(e, Graph e a, Graph e a)] -> Graph e a
connects = foldr (Overlay . \(e, a, b) -> Connect e a b) None

-- | collect vertices from graph with redundancy.
-- if you need properties holding from these vertices
-- please apply relevant operator to the list.
--
-- e.g. Let's say we want to have unique list of vertices and
-- we can do:
--
-- @
-- myToVertices :: Eq a => Graph e a -> [a]
-- myToVertices = nub . toVertices
-- @
toVertices :: Graph e a -> [a]
toVertices = Fold.toList

-- | well balanced vertex group but not ordered.
fromVertices :: [a] -> Graph e a
fromVertices = ana coalg
  where coalg [] = NoneF
        coalg [a] = VertexF a
        coalg (pick -> (a, b)) = OverlayF a b
        pick = flip splitAt <!> (`div` 2) . length

-- | collect all edges from graph, using In-order and with redundancy.
--
-- if you need properties holding from these vertices
-- please apply relevant operator to the list.
--
-- e.g. Let's say we want to have unique list of edges and
-- we can do:
--
-- @
-- mytoEdges :: (Eq a, Eq e) => Graph e a -> [(e, a, a)]
-- mytoEdges = nub . toEdges
-- @
toEdges :: Graph e a -> [(e, a, a)]
toEdges = snd . cata go
  where
    go NoneF = ([], [])
    go (VertexF a) = ([a], [])
    go (OverlayF (ns1, es1) (ns2, es2)) = (ns1 <> ns2, es1 <> es2)
    go (ConnectF e (ns1, es1) (ns2, es2)) = (ns1 <> ns2, [(e, a, b)| a <- ns1, b <- ns2] <> es1 <> es2)

-- | TODO: optimization
fromEdges :: [(e, a, a)] -> Graph e a
fromEdges = overlays . fmap \(e, a, b) -> Connect e (Vertex a) (Vertex b)

-- | Pick up a single node (or a collection of nodes), and gets edges out of it.
--
-- With duplication and no order.
outFrom :: (a -> Bool) -> Graph e a -> [(e, a)]
outFrom is = snd . cata go
  where go NoneF = ([], [])
        go (VertexF a) = ([a], [])
        go (OverlayF (as, es1) (bs, es2)) = (as <> bs, es1 <> es2)
        go (ConnectF e (as, es1) (bs, es2)) =
          (as <> bs, es1 <> es2 <> if any is as then (e,) <$> bs else [])

-- | Pick up a single node (or a collection of nodes), and gets edges into it.
--
-- With duplication and no order.
inTo :: (a -> Bool) -> Graph e a -> [(a, e)]
inTo is = fst . cata go
  where go NoneF = ([], [])
        go (VertexF a) = ([], [a])
        go (OverlayF (es1, as) (es2, bs)) = (es1 <> es2, as <> bs)
        go (ConnectF e (es1, as) (es2, bs)) =
          (es1 <> es2 <> if any is bs then (,e) <$> bs else [], as <> bs)

-- | removes edges between two vertices and this is expensive. @removeEdge@ can actually
-- be applied to any edges between arbitrary nodes if there is any.
--
-- The algorithm is from this paper "https://dl.acm.org/doi/10.1145/3122955.3122956".
removeEdge :: Eq a => (e -> (a, a) -> Bool) -> Graph e a -> Graph e a
removeEdge pick = snd . para go
  where go NoneF = ([], None)
        go (VertexF a) = ([a], Vertex a)
        go (OverlayF (_, (as, a)) (_, (bs, b))) = (nub (as <> bs), Overlay a b)
        go (ConnectF e (a', (as, a)) (b', (bs, b))) =
            ( nub (as <> bs)
            , if null removes then Connect e a b else
              Connect e a (tos /\ b') `Overlay`
              Connect e (froms /\ a') b
            )
          where removes = [(x, y)| x <- as, y <- bs, pick e (x, y)]
                (froms, tos) = unzip removes
                ns /\ g = g >>= \n -> if n `elem` ns then None else return n

-- | remove all edges and collapse a graph to simple vertices (with redundancy).
--
-- if we have @let g = Connect 0 (Vertex 1) $ Connect 1 (Vertex 3) (Vertex 2)@
-- and then we have
--
-- @
-- sort (toVertices $ collapse g) == [1,2,3]
-- toEdges (collapse g) == []
-- @
collapse :: Graph e a -> Graph f a
collapse = cata alg
  where alg NoneF = None
        alg (VertexF a) = Vertex a
        alg (OverlayF None b)  = b
        alg (OverlayF a None) = a
        alg (OverlayF a b) = Overlay a b
        alg (ConnectF _ None b) = b
        alg (ConnectF _ a None) = a
        alg (ConnectF _ a b) = Overlay a b

-- | `einduce` doesn't maintain underlying structure and can perform almost any graph transform.
--
-- `einduce` is expensive.
--
-- you can roughly take the following equation in mind although it is not necessarily same
-- as actual implementation.
--
-- @emap@ is a conditional edge transformation and can be used as 'replaceEdge' and @f@ is
-- user provided operator to pick up which edge is to be replaced or removed based on
-- its application context.
--
-- @
-- einduce f = removeEdge f . emap f
-- @
--
-- let's say we have @let g = Connect 0 (pure 1) $ Connect 0 (pure 2) (pure 3)@, we have:
--
-- @
-- sort (toEdges $ einduce (\e (a, b) -> if e == 0 then Just (e, a, b) else Nothing) g) = [(0, 1, 2), (0, 1, 3)]
-- @
einduce :: (Eq a, Eq e) => (e -> (a, a) -> Maybe (e, a, a)) -> Graph e a -> Graph e a
einduce mapEdge = combine . cata go
  -- we maintain two copies of graph, one with graphs removing edges and one
  -- with graphs adding edges.
  where combine ((_, r), g) = prune (Overlay r g)
        go NoneF = (([], None), None) -- ((vertices, collects), graph)
        go (VertexF a) = (([a], None), Vertex a)
        go (OverlayF ((as, m), a) ((bs, n), b)) = ((nub $ as <> bs, Overlay m n), Overlay a b)
        go (ConnectF e ((as, m), a) ((bs, n), b)) =
          ((nub $ as <> bs, fromEdges adds `Overlay` Overlay m n),) $
          removeEdge (\e' (a', b') -> (e', a', b') `elem` removes) (Connect e a b)
          where newEdges = nub [z| x <- as, y <- bs, z <- Fold.toList $ mapEdge e (x, y)]
                oldEdges = nub [(e, x, y)| x <- as, y <- bs]
                removes = oldEdges \\ newEdges
                adds = newEdges \\ oldEdges
