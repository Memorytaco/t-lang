-- | This module has not been tested, be careful.
--
-- This module contains common graph algorithms.
--
-- Algorithms are designed in general defaults to
-- directed graph with edge labels and nodes. It is user's
-- responsibility to decide how to treat those labels
-- of graph, since one can easily drop the labels or transformed
-- them into simple forms and then give or not the properties
-- of graph.
--
-- TODO: Add test case
module Graph.Algorithm
  (
    -- | * Depth first traversal algorithms
    dfsM, dfs
    -- | * Breadth first traversal algorithms
  , bfsM, bfs

    -- | * TopSort algorithm
  , spanForestM, spanForest
  , topSortM, topSort

    -- | * Least matched sequence
  , lms

  , RouteT, RouteStartT
  , Route, RouteStart
  )
where

import Graph.Data ( Graph, outFrom, inTo )
import Recursion.Morphism ( dyna )
import Data.Maybe ( fromMaybe )
import Control.Comonad.Cofree ( Cofree(..) )
import Data.Functor.Foldable ( ListF(..) )
import Control.Monad.Identity (Identity (..))
import Graph.Tree (Forest (..), Tree (..), postorder)

-- | `RouteT` is a iterator, it generates next route to go.
--
-- It can access the whole graph and pick up next nodes with
-- degree of "current" node. It can also remember history information
-- by using a 'Monad' underlying.
--
-- Effects of duplications in the returned list is determined by algorithm
-- using it.
type RouteT m e a = Graph e a -> ([(a, (e, a))], [((a, e), a)]) -> m [a]
-- | `RouteStartT` generates nodes where to start and with an order.
--
-- It can computes none or one or many nodes to start and in a list
-- order.
--
-- Effects of duplications in the returned list is determined by algorithm
-- using it.
type RouteStartT m e a = Graph e a -> m [a]

type Route e a = Graph e a -> ([(a, (e, a))], [((a, e), a)]) -> [a]
type RouteStart e a = Graph e a -> [a]

-- | depth first traversal algorithm. with monad support.
--
-- Duplications in both `RouteStartT` and `RouteT` are resulting in no
-- effect. The first unique one is picked up and duplications are ignored.
dfsM :: (Monad m, Eq a) => RouteStartT m e a -> RouteT m e a -> Graph e a -> m [a]
dfsM startWith next g = do
  starts <- startWith g
  reverse <$> loop starts []
  where loop [] routes = return routes
        loop (a:as) routes = do
          if a `notElem` routes
          then do
            nexts <- next g (outFrom (== a) g, inTo (== a) g)
            -- difference between dfs and bfs
            loop (nexts <> as) (a:routes)
          else loop as routes

-- | breadth first traversal algorithm. with monad support.
--
-- Duplications in both `RouteStartT` and `RouteT` are resulting in no
-- effect. The first unique one is picked up and duplications are ignored.
bfsM :: (Monad m, Eq a) => RouteStartT m e a -> RouteT m e a -> Graph e a -> m [a]
bfsM startWith next g = do
  starts <- startWith g
  reverse <$> loop starts []
  where loop [] routes = return routes
        loop (a:as) routes = do
          if a `notElem` routes
          then do
            nexts <- next g (outFrom (== a) g, inTo (== a) g)
            -- difference between dfs and bfs
            loop (as <> nexts) (a:routes)
          else loop as routes

-- | spanForest generation algorithm. with monad support.
--
-- Duplications in both `RouteStartT` and `RouteT` are resulting in no
-- effect. The first unique one is picked up and duplications are ignored.
spanForestM :: (Monad m, Eq a) => RouteStartT m e a -> RouteT m e a -> Graph e a -> m (Forest a)
spanForestM startWith next g = do
  starts <- startWith g
  snd <$> loop starts []
  where loop [] routes = return (routes, Forest [])
        loop (a:as) routes = do
          if a `elem` routes
          then loop as routes
          else do
            nexts <- next g (outFrom (== a) g, inTo (== a) g)
            (r1, Forest f1) <- loop nexts (a:routes)
            (r2, Forest f2) <- loop as r1
            return (r2, Forest $ Tree a f1:f2)

-- | topSort algorithm. with monad support. It does topsort starting
-- with `RouteStartT` and usually it will simply be `return . toVertices`
-- to select full graph. Or It can also choose carefully a subgraph
-- using `RouteStartT` and `RouteT` to do topsort within a range.
--
-- Duplications in both `RouteStartT` and `RouteT` are resulting in no
-- effect. The first unique one is picked up and duplications are ignored.
--
-- https://stackoverflow.com/questions/21675925/topological-sort-in-haskell
topSortM :: (Monad m, Eq a) => RouteStartT m e a -> RouteT m e a -> Graph e a -> m [a]
topSortM graphWith next g = do
  Forest trees <- spanForestM graphWith next g
  return $ reverse (trees >>= postorder)

-- | depth first traversal algorithm
dfs :: Eq a => RouteStart e a -> Route e a -> Graph e a -> [a]
dfs startWith next = runIdentity . dfsM (return . startWith) (fmap return . next)

-- | breadth first traversal algorithm
bfs :: Eq a => RouteStart e a -> Route e a -> Graph e a -> [a]
bfs startWith next = runIdentity . bfsM (return . startWith) (fmap return . next)

-- | spanForest algorithm using depth first search algorithm
spanForest :: Eq a => RouteStart e a -> Route e a -> Graph e a -> Forest a
spanForest startWith next = runIdentity . spanForestM (return . startWith) (fmap return . next)

-- | topSort algorithm using depth first search algorithm
topSort :: Eq a => RouteStart e a -> Route e a -> Graph e a -> [a]
topSort graphWith next = runIdentity . topSortM (return . graphWith) (fmap return . next)

-- | TODO: optimise its memory usage
-- least matched sequence using brute force matching method
lms :: Eq a => [a] -> [a] -> [a]
lms as bs = dyna alg coalg (as, bs)
  where
    -- we split sequences here
    coalg (_, []) = Nil
    coalg ([], tail -> xs)
      | null as = Nil
      | null xs = Nil
      | otherwise = Cons (as, xs) (tail as, xs)
    coalg (hs, xs) = Cons (hs, xs) (tail hs, xs)
    alg :: Eq a => ListF ([a], [a]) (Cofree (ListF ([a], [a])) [a]) -> [a]
    alg Nil = []
    alg (Cons (xs, ys) (n :< _)) =
      fromMaybe n $ mapM (\ (a, b) ->
        if a == b then Just a else Nothing) (zip xs ys)
