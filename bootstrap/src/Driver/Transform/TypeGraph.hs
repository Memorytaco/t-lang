module Driver.Transform.TypeGraph
  ( runToGraph
  )
where

import Transform.TypeGraph (toGraph, ConstrainGraph, FoldBinderTypeGraph, FoldTypeGraph)
import Language.Core (Type)
import Graph.Extension.GraphicType
import Graph.Core
import Language.Generic ((:<:))

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Capability.Reader (HasReader, MonadReader (..), Rename (..), Pos (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)
import Capability.State (HasState, MonadState (..))

-- | shofthand for monad
type M name nodes edges m
  = ReaderT ( [(name, (Hole nodes Int, CoreG nodes edges Int))]
            , [(name, (Hole nodes Int, CoreG nodes edges Int))])
      (StateT Int m)

newtype TypeToGraphM name nodes edges m a = TypeToGraphM
  { runTypeToGraphM :: M name nodes edges m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    -- local name at position 2: (_, here)
    deriving ( HasSource "local" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             , HasReader "local" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             ) via Rename 2 (Pos 2 () (MonadReader (M name nodes edges m)))
    -- global name at position 1: (here, _)
    deriving ( HasSource "global" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             , HasReader "global" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             ) via Rename 1 (Pos 1 () (MonadReader (M name nodes edges m)))
    -- assign a unique token to each node which helps keep graph structure steady
    deriving (HasState "node" Int, HasSource "node" Int, HasSink "node" Int)
          via MonadState (M name nodes edges m)

-- | actual driver to start the engine, and it allows monad transform
runToGraph :: ( ConstrainGraph bind nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph rep nodes edges Int (TypeToGraphM name nodes edges m)
              , FoldBinderTypeGraph bind Int, FoldTypeGraph rep Int
              , Ord (edges (Link edges))
              , Functor bind, Functor rep
              , T NodeBot :<: nodes
              , T NodeApp :<: nodes, T Sub :<: edges
              , Show name, Eq name
              , MonadFail m
              )
           => ([(name, (Hole nodes Int, CoreG nodes edges Int))], [(name, (Hole nodes Int, CoreG nodes edges Int))])
           -> Int -> Type bind rep name name
           -> m ((Hole nodes Int, CoreG nodes edges Int), Int)
runToGraph r seed t = runStateT (runReaderT (runTypeToGraphM $ toGraph t) r) seed
