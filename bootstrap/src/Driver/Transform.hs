module Driver.Transform
  ( PlayG
  , PlayGP
  , runToGraph
  )
where

import Tlang.Transform.TypeGraph (toGraph, ConstrainGraph, InjGraph, BinderGraph, LiteralGraph)
import Tlang.Unification.Type
import Tlang.AST (Symbol, Type, Label, Bound, StandardRepType)
import Tlang.Graph.Extension.Type
import Tlang.Graph.Core
import Tlang.Generic ((:+:), (:<:))

import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Capability.Reader (HasReader, MonadReader (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)
import Capability.State (HasState, MonadState (..))

import Data.Text (Text)

type PlayG r
  = CoreG (   Uno NodeBot :+: Uno (NodeLit Integer) :+: Uno (NodeLit Text)
          :+: Uno NodeTup :+: Uno NodeSum :+: Uno NodeRec :+: Uno (NodeRef Symbol)
          :+: Uno NodeApp :+: Uno (NodeHas Label) :+: Uno (NodeRep r)
          )
          (Uno Sub :+: Uno (Bind Symbol)) Int

-- | Graph for handling type generated fro parser
type PlayGP = PlayG (StandardRepType Symbol Label (Bound Symbol) Identity)

newtype TypeToGraphM name nodes edges m a = TypeToGraphM
  { runTypeToGraphM ::
      ReaderT [(name, (Hole nodes Int, CoreG nodes edges Int))] (StateT Int m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving ( HasSource "variable" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             , HasReader "variable" [(name, (Hole nodes Int, CoreG nodes edges Int))]
             ) via MonadReader (ReaderT [(name, (Hole nodes Int, CoreG nodes edges Int))] (StateT Int m))
    deriving (HasState "node" Int, HasSource "node" Int, HasSink "node" Int)
          via MonadState (ReaderT [(name, (Hole nodes Int, CoreG nodes edges Int))] (StateT Int m))

runToGraph :: ( ConstrainGraph cons nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph bind nodes edges Int (TypeToGraphM name nodes edges m)
              , ConstrainGraph inj nodes edges Int (TypeToGraphM name nodes edges m)
              , InjGraph inj Int, LiteralGraph cons Int, BinderGraph bind Int
              , Monad m, Ord (edges (Link edges))
              , Traversable inj, Traversable bind, Traversable cons
              , Uno (NodeRep rep) :<: nodes, Uno (NodeRef name) :<: nodes, Uno NodeBot :<: nodes
              , Uno NodeApp :<: nodes, Uno Sub :<: edges, Eq name
              )
           => [(name, (Hole nodes Int, CoreG nodes edges Int))]
           -> Int -> Type name cons bind inj rep
           -> m ((Hole nodes Int, CoreG nodes edges Int), Int)
runToGraph r seed t = runStateT (runReaderT (runTypeToGraphM $ toGraph t) r) seed

