{-# LANGUAGE AllowAmbiguousTypes #-}
-- | common setting for language constraint
module Language.Constraint.Setting
  ( ConstraintSetting

  , HasConstraintError
  , ConstraintError (..)
  , Error


  , throwTrivial
  , handleTrivial
  , throwMsg
  , throwContext
  , withContext
  , handleContext

  , whenMessage

  )
where

import Language.Setting (HasGraph)
import Graph.Extension.GraphicType
import Language.Generic.Subsume ((:>+:))
import Graph.Core (HasOrderGraph)
import Capability.Error
import Data.Kind (Type)
import Data.Bifunctor.TH (deriveBifunctor)

-- | basic constraint environment
type ConstraintSetting m nodes edges info =
  ( HasGraph nodes edges info m
  , HasOrderGraph nodes edges info
  , edges :>+: '[T Sub, Pht O]
  , nodes :>+: '[T NodeBot, R []]
  )

-- | Error setting for constraint
type HasConstraintError m c a =
  ( HasThrow (Error ConstraintError a) (ConstraintError c a) m
  , HasCatch (Error ConstraintError a) (ConstraintError c a) m
  )

-- | utility type for defining constraint error handling mechanism
data Error :: k -> Type -> Type

data ConstraintError c a
  = -- | error with context
    ConstraintError c [ConstraintError c a]
  | -- | arbitrary message
    ConstraintMessage String
  | -- | arbitrary sub error
    ConstraintTrivial a
  deriving (Functor, Foldable, Traversable)

deriveBifunctor ''ConstraintError

throwTrivial :: forall e m c a. HasConstraintError m c e => e -> m a
throwTrivial = throw @(Error ConstraintError e) . ConstraintTrivial

handleTrivial :: forall e m c a. HasConstraintError m c e => m a -> (e -> m a) -> m a
handleTrivial m f = catch @(Error ConstraintError e) m \case
  ConstraintTrivial e -> f e
  e -> throw @(Error ConstraintError e) e

throwMsg :: forall e m c a. HasConstraintError m c e => String -> m a
throwMsg = throw @(Error ConstraintError e) . ConstraintMessage

throwContext :: forall e m c a. HasConstraintError m c e => c -> [ConstraintError c e] -> m a
throwContext c = throw @(Error ConstraintError e) . ConstraintError c

withContext :: forall e m c a. HasConstraintError m c e => c -> m a -> m a
withContext c m = catch @(Error ConstraintError e) m (throw @(Error ConstraintError e) . ConstraintError c . (:[]))

handleContext :: forall e m c a. HasConstraintError m c e => m a -> (c -> [ConstraintError c e] -> m a) -> m a
handleContext m1 f = catch @(Error ConstraintError e) m1 \case
  ConstraintError c es -> f c es
  e -> throw @(Error ConstraintError e) e

whenMessage :: forall e m c a. HasConstraintError m c e => Bool -> String -> m a -> m a
whenMessage bool msg m = if bool then m else throwMsg @e msg

