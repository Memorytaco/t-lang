module Graph.Tree
  ( Tree (..)
  , TreeF (..)
  , Forest (..)

  , postorder
  )
where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Tree a = Tree a [Tree a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
makeBaseFunctor ''Tree

newtype Forest a = Forest [Tree a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

postorder :: Tree a -> [a]
postorder = cata alg
  where alg (TreeF a ms) = concat ms <> [a]
