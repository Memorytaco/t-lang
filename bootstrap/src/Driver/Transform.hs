module Driver.Transform
  ( PlayG
  , TypeToGraphM (..)
  , runToGraph
  )
where

import Tlang.Transform.TypeGraph (toGraph, ConstrainGraph, FoldBinderTypeGraph, FoldTypeGraph)
import Tlang.Unification.Type
import Tlang.AST (Name, Type, Label)
import Tlang.Graph.Extension.Type
import Tlang.Graph.Core
import Tlang.Generic ((:+:), (:<:))

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Capability.Reader (HasReader, MonadReader (..), Rename (..), Pos (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)
import Capability.State (HasState, MonadState (..))

import Data.Text (Text)

type PlayG
  = CoreG (   Uno NodeBot :+: Uno (NodeLit Integer) :+: Uno (NodeLit Text)
          :+: Uno NodeTup :+: Uno NodeSum :+: Uno NodeRec :+: Uno (NodeRef Name)
          :+: Uno NodeApp :+: Uno (NodeHas Label)
          )
          (Uno Sub :+: Uno (Bind Name)) Int

-- | shofthand for monad
type T name nodes edges m
  = ReaderT ([(name, (Hole nodes Int, CoreG nodes edges Int))], [(name, (Hole nodes Int, CoreG nodes edges Int))])
      (StateT Int m)

newtype TypeToGraphM name nodes edges m a = TypeToGraphM
  { runTypeToGraphM :: T name nodes edges m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    -- local name at position 2: (_, here)
    deriving ( HasSource "local" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             , HasReader "local" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             ) via Rename 2 (Pos 2 () (MonadReader (T name nodes edges m)))
    -- global name at position 1: (here, _)
    deriving ( HasSource "global" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             , HasReader "global" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             ) via Rename 1 (Pos 1 () (MonadReader (T name nodes edges m)))
    -- assign a unique token to each node which helps keep graph structure steady
    deriving (HasState "node" Int, HasSource "node" Int, HasSink "node" Int)
          via MonadState (T name nodes edges m)

-- | actual driver to start the engine, and it allows monad transform
runToGraph :: ( ConstrainGraph bind nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph rep nodes edges Int (TypeToGraphM name nodes edges m)
              , FoldBinderTypeGraph bind Int, FoldTypeGraph rep Int
              , Ord (edges (Link edges))
              , Functor bind, Functor rep
              , Uno NodeBot :<: nodes
              , Uno NodeApp :<: nodes, Uno Sub :<: edges
              , Show name, Eq name
              , MonadFail m
              )
           => ([(name, (Hole nodes Int, CoreG nodes edges Int))], [(name, (Hole nodes Int, CoreG nodes edges Int))])
           -> Int -> Type bind rep name name
           -> m ((Hole nodes Int, CoreG nodes edges Int), Int)
runToGraph r seed t = runStateT (runReaderT (runTypeToGraphM $ toGraph t) r) seed

