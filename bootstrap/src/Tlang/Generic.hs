module Tlang.Generic
  ( (:+:) (..)
  , (:+:$) (..)
  , (:<:) (..)
  , Alg (..)
  , (:@:)
  )
where

import Data.Functor.Foldable (Recursive)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Tlang.TH (fixQ)
import Data.Kind (Type)

-- | * generic functor sum type
-- a way using functor making extended data, please refer https://doi.org/10.1017/S0956796808006758 
-- for more information

infixr 1 :+:
data (f :+: g) a
  = Inl (f a)
  | Inr (g a)
  deriving (Functor, Foldable, Traversable, Eq)
makeBaseFunctor $ fixQ [d| instance (Traversable g, Traversable f) => Recursive ((f :+: g) a) |]
deriving instance (Ord (f a), (Ord (g a))) => Ord ((f :+: g) a)

instance (Show (f a), Show (g a)) => Show ((f :+: g) a) where
  show (Inl v) = show v
  show (Inr v) = show v

-- | * Generic definition to save typing, a type indexed method
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just
instance {-# OVERLAPS #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl a) = Just a
  prj (Inr _) = Nothing
instance {-# OVERLAPS #-} (Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj (Inl _) = Nothing
  prj (Inr a) = prj a

-- | general algebra operation
class Alg f a | a -> f where
  alg :: f a -> a
instance (Alg l a, Alg r a) => Alg (l :+: r) a where
  alg (Inl v) = alg v
  alg (Inr v) = alg v

type (:@:) a op = Alg op a

