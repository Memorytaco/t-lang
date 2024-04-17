module Language.Generic.Data
  ( (:+:) (..)
  , (:++:) (..)
  , type (|:) (..)
  , type (|:$) (..)
  )
where

import Data.Functor.Foldable (Recursive)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.TH (fixQ)
import Prettyprinter ( Pretty(pretty) )
import Data.Bifunctor.TH (deriveBifunctor)

infixr 3 :+:

-- | generic functor sum type
--
-- a way using functor making extended data, please refer https://doi.org/10.1017/S0956796808006758
-- for more information
data (f :+: g) a
  = Inl (f a)
  | Inr (g a)
  deriving (Functor, Foldable, Traversable, Eq, Ord)

instance (Show (f a), Show (g a)) => Show ((f :+: g) a) where
  show (Inl v) = show v
  show (Inr v) = show v

instance (Pretty (f a), Pretty (g a)) => Pretty ((f :+: g) a) where
  pretty (Inl v) = pretty v
  pretty (Inr v) = pretty v

-- | generic bifunctor sum type
data (f :++: g) b a
  = Inll (f b a)
  | Inrr (g b a)
  deriving (Functor, Foldable, Traversable, Eq, Ord)
deriveBifunctor ''(:++:)

instance (Show (f b a), Show (g b a)) => Show ((f :++: g) b a) where
  show (Inll v) = show v
  show (Inrr v) = show v

instance (Pretty (f b a), Pretty (g b a)) => Pretty ((f :++: g) b a) where
  pretty (Inll v) = pretty v
  pretty (Inrr v) = pretty v

-- | a specialised Cofree and its relevant CofreeF
infixl 2 |: , :|
data f |: a = a :| (f (f |: a)) deriving (Functor, Foldable, Traversable)
makeBaseFunctor $ fixQ [d| instance (Functor f) => Recursive (f |: a) |]
deriving instance (Eq (f (f |: a)), Eq a) => Eq (f |: a)
deriving instance (Ord (f (f |: a)), Ord a) => Ord (f |: a)
deriving instance (Show (f (f |: a)), Show a) => Show (f |: a)

