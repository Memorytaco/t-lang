{-# LANGUAGE AllowAmbiguousTypes #-}
module Tlang.Transform.Class
  (
    -- ** defined transform for `Expr`
    TransformSimp (..)
  , PassSimp
  , transformSimp

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
data PassSimp (a :: any)

-- | simple transformation pass with cata scheme
class TransformSimp pass f a | pass f -> a where
  transformSimp_
    :: (Provide m t a (PassSimp pass) f, Monad m)
    => Proxy pass -> f (m (Expr t a)) -> m (Expr t a)

-- | please use this if you need one pass
transformSimp :: forall pass t f a m. (TransformSimp pass f a, Provide m t a (PassSimp pass) f, Monad m)
              => f (m (Expr t a)) -> m (Expr t a)
transformSimp = transformSimp_ (Proxy @pass)

