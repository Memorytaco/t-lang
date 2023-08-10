{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- | ** unification driver code
-}
module Driver.Unification
  (
    -- ** unify with exception
    runUnify
  , runDefaultUnify
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT (..))

import Data.Text (Text)

import Tlang.Unification.Graph
import Tlang.Graph.Core ( CoreG, Hole )
import Language.Core (Label, Name)

import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..))
import Capability.Error (HasThrow, HasCatch, MonadError (..))

-- | unification monad
newtype GraphUnify ns es m a = GraphUnify
  { runGraphUnify ::
      StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving (HasThrow "failure" (GraphUnifyError (Hole ns Int)), HasCatch "failure" (GraphUnifyError (Hole ns Int)))
        via MonadError (StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m))
    deriving (HasState "graph" (CoreG ns es Int), HasSource "graph" (CoreG ns es Int), HasSink "graph" (CoreG ns es Int))
        via MonadState (StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m))

runUnify
  :: Unifier (GraphUnify ns es m) ns Int -> CoreG ns es Int -> Hole ns Int -> Hole ns Int
  -> m (Either (GraphUnifyError (Hole ns Int)) (Hole ns Int, CoreG ns es Int))
runUnify u g a b = runExceptT $ runStateT (runGraphUnify $ runUnifier u a b) g

runDefaultUnify = runUnify . hook1 . joinCaseT $ CaseT
  [ case1, case2
  , case10
  , case20, case21, case25 @Label
  , case30
  , case40 @Integer
  , case40 @Text
  , case50
  , case60 @Name
  ]


