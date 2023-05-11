module Tlang.Graph.Core
  ( Link (..)
  , Hole (..)
  , Uno (..)

  , CoreG
  , (:~~:) (..)
  , GraphConstraint
  )
where

import Data.GraphViz
import Data.Graph.Inductive
import Control.Monad.State (MonadState)
import Data.Kind (Constraint, Type)

import Tlang.Generic ((:<:), (:+:) (..))

-- | generic edge for graph
data Link e a = Link (e (Link e a)) a deriving (Functor, Foldable, Traversable)
deriving instance (Show (e (Link e a)), Show a) => Show (Link e a)
deriving instance (Eq (e (Link e a)), Eq a) => Eq (Link e a)

-- | generic node for graph
data Hole e a = Hole (e (Hole e a)) a deriving (Functor, Foldable, Traversable)
deriving instance (Show (e (Hole e a)), Show a) => Show (Hole e a)
deriving instance (Eq (e (Hole e a)), Eq a) => Eq (Hole e a)

-- | hold constant entity
data Uno c a = Uno c deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Labellable (e (Hole e a))) => Labellable (Hole e a) where
  toLabelValue (Hole v _) = toLabelValue v
instance (Labellable (e (Link e a))) => Labellable (Link e a) where
  toLabelValue (Link v _) = toLabelValue v
instance (Labellable c) => Labellable (Uno c a) where
  toLabelValue (Uno v) = toLabelValue v

-- | core structure for graph unification
type CoreG nodes edges info = Gr (Hole nodes info) (Link edges info)

type family GraphConstraint (n :: Type -> Type) (node :: Type -> Type) (edge :: Type -> Type) (info :: Type) :: Constraint

class node :~~: info | node -> info where
  gunify :: ( MonadState (CoreG ns es info) m
            , GraphConstraint node ns es info -- ^ allow user to specify context
            , node :<: ns, Eq (Gr (Hole ns info) (Link es info))
            )
         => (node (Hole e info), Node, info) -> Node -> m Node

type instance GraphConstraint (a :+: b) n e i = (GraphConstraint a n e i, GraphConstraint b n e i, a :<: n, b :<: n)
instance (f :~~: a, g :~~: a) => (f :+: g) :~~: a where
  gunify (Inr v, n, a) = gunify (v, n, a)
  gunify (Inl v, n, a) = gunify (v, n, a)
