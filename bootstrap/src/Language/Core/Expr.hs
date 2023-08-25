{-# LANGUAGE QuantifiedConstraints #-}
module Language.Core.Expr
  ( Expr (..)
  , ExprF (..)
  , type (@:) (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)

import Tlang.TH (fixQ)

-- | type annotation with full power of the type system
data typ @: term = term :@ typ deriving (Show, Eq, Functor, Traversable, Foldable)
$(deriveBifunctor ''(@:))

-- | a `Free` like structure for defining `Expr`
--
-- `Val` is simply a syntactic constant to `Expr` (e.g. a name reference, a variable)
data Expr f a
  = Val a
  | Expr (f (Expr f a))
  deriving (Functor)

deriving instance (Show (f (Expr f a)), Show a) => Show (Expr f a)
deriving instance (Eq (f (Expr f a)), Eq a) => Eq (Expr f a)

instance Functor f => Applicative (Expr f) where
  pure = Val
  (Val f) <*> (Val a) = Val (f a)
  f <*> Expr fv = Expr ((f <*>) <$> fv)
  (Expr fv) <*> a = Expr ((<*> a) <$> fv)

instance Functor f => Monad (Expr f) where
  Val a >>= f = f a
  Expr fma >>= f = Expr ((>>= f) <$> fma)

makeBaseFunctor $ fixQ [d|
  instance (Functor f) => Recursive (Expr f a)
  |]
deriving instance (Show a, Show r, forall x. Show x => Show (f x)) => Show (ExprF f a r)
