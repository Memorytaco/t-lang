{-# LANGUAGE NoMonomorphismRestriction #-}
-- | ** unification driver code
module Driver.Unification
  (
    -- ** unify with exception
    driveUnifier
  , unify
  , GraphUnifyError (..)
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT (..))

import Data.Text (Text)

import Graph.Core (CoreG, HasOrderGraph, Hole)
import Language.Constraint.Unification
import Language.Core (Label, Name)
import Language.Generic ((:>+:))
import Language.Setting (GraphState, HasGraphShow)

import Capability.Error (HasCatch, HasThrow, MonadError (..))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..))
import Graph.Extension.GraphicType

-- | unification monad
newtype GraphUnify ns es m a = GraphUnify
  { runGraphUnify ::
      StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving (HasThrow GraphUnifyError (GraphUnifyError (Hole ns Int)), HasCatch GraphUnifyError (GraphUnifyError (Hole ns Int)))
        via MonadError (StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m))
    deriving (HasState GraphState (CoreG ns es Int), HasSource GraphState (CoreG ns es Int), HasSink GraphState (CoreG ns es Int))
        via MonadState (StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m))

driveUnifier
  :: Unifier (GraphUnify ns es m) ns Int -> CoreG ns es Int -> Hole ns Int -> Hole ns Int
  -> m (Either (GraphUnifyError (Hole ns Int)) (Hole ns Int, CoreG ns es Int))
driveUnifier u g a b = runExceptT $ runStateT (runGraphUnify $ runUnifier u a b) g

unify
  :: ( Monad m, HasOrderGraph ns es Int, HasGraphShow ns es Int
     , ns :>+: '[T NodeTup, T NodeSum, T NodeRec, T (NodeHas Label), T (NodeRef Name), T NodeArr, NodePht]
     , ns :>+: '[T (NodeLit Integer), T (NodeLit Text), T NodeApp, T NodeBot, R [], G]
     , es :>+: '[T (Binding Name), T Sub, Pht O]
     )
  => CoreG ns es Int
  -> Hole ns Int -> Hole ns Int
  -> m (Either (GraphUnifyError (Hole ns Int)) (Hole ns Int, CoreG ns es Int))
unify = driveUnifier . hook1 . foldCase $ Case
  [

  -- bottom variable node
    case1, case2

  -- tuple
  , case10

  , case20, case21, case25 @Label
  , case30, case31
  
  -- type literal node
  , case40 @Integer , case40 @Text

  -- phantom node
  , caseNodePht
  -- type name node
  , case60 @Name
  ]


