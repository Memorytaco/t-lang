{-# LANGUAGE ExplicitNamespaces #-}
module Language.Generic
  (
  -- ** Definitions
    Alg (..)
  , (:@:)

  -- ** Useful functor
  , X (..)
  , type (|:) (..)
  , type (|:$) (..)

  , strip, stripBase
  , fromX, toX

  -- ** term level open recursion
  , Recursion (..)
  , Recursion2 (..)

  -- ** Re-Export of foldable functor
  , module FunctorFoldable
  , module Subsume
  , module Data

  )
where

import Prelude hiding (Either (..))
import Data.Functor.Foldable as FunctorFoldable

import Language.Generic.Subsume as Subsume
import Language.Generic.Data as Data

import GHC.Generics (Generic (..))

-- | a specialised Fix and its relevant FixF, Fixpoint
newtype X f = X (f (X f)) deriving (Generic)
deriving instance (Eq (f (X f))) => Eq (X f)
deriving instance (Ord (f (X f))) => Ord (X f)
instance (Show (f (X f))) => Show (X f) where
  show (X v) = "(" <> show v <> ")"

type instance Base (X f) = f
instance Functor f => Recursive (X f) where
  project (X v) = v
instance Functor f => Corecursive (X f) where
  embed = X

-- | strip annotation
strip :: Functor f => f |: a -> X f
strip (_ :| f) = X $ strip <$> f

stripBase :: Corecursive c => (Base c |: a) -> c
stripBase = fromX . strip

fromX :: Corecursive t => X (Base t) -> t
fromX = refix

toX :: Recursive t => t -> X (Base t)
toX = refix

-- | general algebra operation
class Alg f a | a -> f where
  alg :: f a -> a
instance (Alg l a, Alg r a) => Alg (l :+: r) a where
  alg (Inl v) = alg v
  alg (Inr v) = alg v

type (:@:) a op = Alg op a

-- | open recursion, a template
newtype Recursion m a b = Recursion ((a -> m b) -> (a -> m b))
-- | open recursion, a template, for arity 2
newtype Recursion2 m a b r = Recursion2 ((a -> b -> m r) -> (a -> b -> m r))

