module Graph.Data
  ( Graph (..)
  , UGraph (..)

  , toUndirected
  , depth
  )
where

import Data.Bifunctor.TH (deriveBifunctor)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable

-- ** core structure and method for graph unification

-- | TODO: use this implementation as default algebraic graph
data Graph e a
  = None
  | Vertex a
  | Overlay (Graph e a) (Graph e a)
  | Connect e (Graph e a) (Graph e a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
deriveBifunctor ''Graph
makeBaseFunctor ''Graph


instance Applicative (Graph e) where
  pure = Vertex
  None <*> _ = None
  (Vertex f) <*> g = f <$> g
  (Overlay fa fb) <*> g = Overlay (fa <*> g) (fb <*> g)
  (Connect e fa fb) <*> g = Connect e (fa <*> g) (fb <*> g)

-- enable auto shrink to remove unnecessary nodes
instance Monad (Graph e) where
  None >>= _ = None
  (Vertex a) >>= f = f a
  (Overlay ma mb) >>= f = Overlay (ma >>= f) (mb >>= f)
  (Connect e ma mb) >>= f = Connect e (ma >>= f) (mb >>= f)

-- undirected graph
newtype UGraph a = UGraph (Graph () a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

-- convert a directed graph to undirected graph
toUndirected :: Graph e a -> UGraph a
toUndirected = UGraph . cata go
  where
    go NoneF = None
    go (VertexF a) = Vertex a
    go (OverlayF a b) = Overlay a b
    go (ConnectF _ a b) = Connect () a b

-- | calculate depth of graph implementation
depth :: Graph e a -> Integer
depth = cata go
  where go NoneF = 0
        go (VertexF _) = 0
        go (OverlayF a b)
          | a > b = a + 1
          | otherwise = b + 1
        go (ConnectF _ a b)
          | a > b = a + 1
          | otherwise = b + 1

vertices :: [a] -> Graph e a
vertices = overlays . fmap Vertex

overlays :: [Graph e a] -> Graph e a
overlays = error "TODO"