module Tlang.Helper.AST.Binder
  ( HasBinder (..)
  )
where

import Tlang.AST.Type
import Tlang.Generic
import qualified Data.Kind as K (Type)

class HasBinder a where
  type BinderOf a :: K.Type -> K.Type
  type BodyOf a   :: K.Type
  getBinder :: forall binder. (binder :<: BinderOf a) => a -> Maybe (binder a, BodyOf a)

instance HasBinder (Type name cons bind inj rep) where
  type BinderOf (Type name cons bind inj rep) = bind
  type BodyOf (Type name cons bind inj rep) = Type name cons bind inj rep
  getBinder (TypLet bnd body) = (, body) <$> prj bnd
  getBinder _ = Nothing
