{- | * This module adds and removes sugar to syntactic types
--
--  TODO: import more strategies and algorithms here.
--
--  Type sugars are grouped by rules and rule consists of
--  cases.
--
--  Please refer to "docs/sugar_rules.md" for more info.
---}
module Driver.Transform.Desugar
  (
    driveTypeGraphSugar
  , addBindingToType
  )
where

import Control.Monad.IO.Class
import Control.Monad.State (StateT (..))
import Capability.Sink ( HasSink )
import Capability.Source ( MonadState(..), HasSource )
import Capability.State (HasState)

import Graph.Core ( CoreG, Hole, HasOrderGraph )
import Language.Setting (GraphState)
import Transform.Desugar (runSugarRule, treeSugarRule, case01, case10)
import Language.Core (Name)
import Language.Generic
import Graph.Extension.GraphicType

type M nodes edges info m = (StateT (CoreG nodes edges info) m)
newtype TypeGraphSugar nodes edges info m a = TypeGraphSugar
  { runTypeGraphSugar :: M nodes edges info m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO)
    deriving ( HasState GraphState (CoreG nodes edges info)
             , HasSink GraphState (CoreG nodes edges info)
             , HasSource GraphState (CoreG nodes edges info)
             )
      via MonadState (M nodes edges info m)

driveTypeGraphSugar :: TypeGraphSugar nodes edges info m a -> CoreG nodes edges info -> m (a, CoreG nodes edges info)
driveTypeGraphSugar m = runStateT (runTypeGraphSugar m)

-- | Graphic type constraint requires every node should have exactly one __binder__, which this function serves.
addBindingToType
  :: (HasOrderGraph nodes edges info, nodes :>+: '[T NodeApp, T NodeTup], edges :>+: '[T (Binding Name), T Sub], Monad m)
  => CoreG nodes edges info -> Hole nodes info -> m (Bool, CoreG nodes edges info)
addBindingToType gr g = driveTypeGraphSugar (runSugarRule (treeSugarRule rules) g) gr
  where rules = 
          [ case01 @Name, case10 @Name
          ]
