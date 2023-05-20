{- | ** unification driver code
-}
module Driver.Unification
  (
    -- ** unify with exception
    runUnify
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT (..))

import Tlang.Unification.Type
import Tlang.Graph.Core
import Tlang.Generic ((:<:))

import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..))
import Capability.Error (HasThrow, MonadError (..))

-- | unification monad
newtype TypeUnify ns es m a = TypeUnify
  { runTypeUnify ::
      StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving (HasThrow "failure" (GraphUnifyError (Hole ns Int)))
        via MonadError (StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m))
    deriving (HasState "graph" (CoreG ns es Int), HasSource "graph" (CoreG ns es Int), HasSink "graph" (CoreG ns es Int))
        via MonadState (StateT (CoreG ns es Int) (ExceptT (GraphUnifyError (Hole ns Int)) m))

runUnify
  :: ( GraphConstraint ns ns es Int (TypeUnify ns es m)
     , Monad m, Ord (es (Link es)), Eq (ns (Hole ns Int))
     , ns :~~: Int, ns :<: ns
     )
  => CoreG ns es Int -> Hole ns Int -> Hole ns Int
  -> m (Either (GraphUnifyError (Hole ns Int)) (Hole ns Int, CoreG ns es Int))
runUnify g a b = runExceptT $ runStateT (runTypeUnify $ a ~=~ b) g
