{-# LANGUAGE AllowAmbiguousTypes #-}
module Tlang.Transform.Pass
  (
    -- ** defined transform for `Expr`
    TransformCata (..)
  , PassCata
  , transformCata

    -- ** companion type family
  , Provide
  )
where

import Tlang.AST.Expr (Expr (..))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))

type Provide :: (Type -> Type) -> (Type -> Type) -> Type
             -> pass -> (Type -> Type)
             -> Constraint
type family Provide m t a pass f :: Constraint

-- | use this in __pass__ parameter in `Provide` family to define
-- a simple pass on `Expr`
data PassCata (a :: any)

-- | simple transformation pass with cata scheme
class TransformCata pass f a | pass f -> a where
  transformCata_
    :: (Provide m t a (PassCata pass) f, Monad m)
    => Proxy pass -> f (m (Expr t a)) -> m (Expr t a)

-- | please use this if you need one pass
transformCata :: forall pass t f a m. (TransformCata pass f a, Provide m t a (PassCata pass) f, Monad m)
              => f (m (Expr t a)) -> m (Expr t a)
transformCata = transformCata_ (Proxy @pass)

