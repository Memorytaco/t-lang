module Driver.Transform
  ( PlayG
  , runToGraph
  )
where

import Tlang.Transform.TypeGraph (toGraph, ConstrainGraph, InjGraph, BinderGraph, LiteralGraph, RuntimeRepGraph)
import Tlang.Unification.Type
import Tlang.AST (Name, Type, Label)
import Tlang.Graph.Extension.Type
import Tlang.Graph.Core
import Tlang.Generic ((:+:), (:<:))

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Capability.Reader (HasReader, MonadReader (..))
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

newtype TypeToGraphM name nodes edges m a = TypeToGraphM
  { runTypeToGraphM ::
      ReaderT [(name, (Hole nodes Int, CoreG nodes edges Int))] (StateT Int m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving ( HasSource "variable" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             , HasReader "variable" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             ) via MonadReader (ReaderT [(name, (Hole nodes Int, CoreG nodes edges Int))] (StateT Int m))
    deriving (HasState "node" Int, HasSource "node" Int, HasSink "node" Int)
          via MonadState (ReaderT [(name, (Hole nodes Int, CoreG nodes edges Int))] (StateT Int m))

runToGraph :: ( ConstrainGraph prm nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph bind nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph inj nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph rep nodes edges Int (TypeToGraphM name nodes edges m)
              , InjGraph inj Int, LiteralGraph prm Int, BinderGraph bind Int, RuntimeRepGraph rep Int
              , Monad m, Ord (edges (Link edges))
              , Functor inj, Functor bind, Functor prm, Functor rep
              , Uno (NodeRef name) :<: nodes, Uno NodeBot :<: nodes
              , Uno NodeApp :<: nodes, Uno Sub :<: edges, Eq name
              )
           => [(name, (Hole nodes Int, CoreG nodes edges Int))]
           -> Int -> Type rep prm bind inj name
           -> m ((Hole nodes Int, CoreG nodes edges Int), Int)
runToGraph r seed t = runStateT (runReaderT (runTypeToGraphM $ toGraph t) r) seed

