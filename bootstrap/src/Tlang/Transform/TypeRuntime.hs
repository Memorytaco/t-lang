{- | This module is used to transform syntactic type into runtime structure


    TODO: This module should have similar structure to `Tlang.Transform.TypeGraph` module
-}
module Tlang.Transform.TypeRuntime
  (
    FoldRuntimeRep (..)
  , FoldBinderRuntimeRep (..)
  , ConstrainRuntime
  )
where

import Data.Kind (Type, Constraint)

type ConstrainRuntime :: (Type -> Type) -> (Type -> Type) -> Type -> Constraint
type family ConstrainRuntime f m a

-- | a special case to fold with context
class FoldBinderRuntimeRep f a | f -> a where
  foldBinderRuntimeRep :: ConstrainRuntime f m a => f (m a) -> m a -> m a

-- | allow to have multiple representation
class FoldRuntimeRep f a | f -> a where
  foldRuntimeRep :: ConstrainRuntime f m a => f (m a) -> m a
