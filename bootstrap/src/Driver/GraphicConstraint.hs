module Driver.GraphicConstraint
  (
  
  -- ** monad for graphic constraint generator
    GCGen
  
  -- ** common driver
  , driveGCGen

  -- ** expression constraint generator
  , genExprGC
  )
where

import Language.Constraint.Graphic
import Language.Generic ((:>+:), type (|:) (..))
import Language.Core (ExprF (..), Expr (..))

import Graph.Core (Hole (..), HasOrderEdge)
import Graph.Extension.GraphicType


import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..))
import Capability.Reader (HasReader, MonadReader (..))
import Capability.Error (HasThrow, HasCatch, MonadError (..))

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT (..))
import Control.Monad.Reader (ReaderT (..))

type GCGen' name ns m = ReaderT (BindingTable name ns) (StateT Int (ExceptT ConstraintGenErr m))

newtype GCGen name ns m a = GCGen
  { runGCGen :: GCGen' name ns m a
  } deriving newtype (Functor, Applicative, Monad)
    deriving (HasThrow ConstraintGenErr ConstraintGenErr, HasCatch ConstraintGenErr ConstraintGenErr)
      via MonadError (GCGen' name ns m)
    deriving (HasState "NodeCreator" Int, HasSink "NodeCreator" Int, HasSource "NodeCreator" Int)
      via MonadState (GCGen' name ns m)
    deriving (HasReader "binding" (BindingTable name ns), HasSource "binding" (BindingTable name ns))
      via MonadReader (GCGen' name ns m)

driveGCGen :: GCGen name ns m a
  -> BindingTable name ns
  -> Int
  -> m (Either ConstraintGenErr (a, Int))
driveGCGen m r i = runExceptT (runStateT (runReaderT (runGCGen m) r) i)

-- | generate staged graphic constraint for expression
genExprGC ::
  ( target ~ (ExprF f name |: Hole nodes Int)
  , ConstraintGen f target Int
  , ConstrainGraphic f target (GCGen name nodes m) nodes edges Int
  , edges
      :>+: '[ T Sub
            , T (Binding name)
            , T Unify
            , T Instance
            , Pht NDOrderLink
            , Pht Sub
            ]
  , nodes :>+: '[T NodeBot, G, NDOrder]
  , Traversable f
  , HasOrderEdge edges
  , Show name
  , Eq name
  , Monad m
  ) => Expr f name -> BindingTable name nodes -> Int
  -> m (Either ConstraintGenErr (StageConstraint nodes edges Int target, Int))
genExprGC e = driveGCGen (genConstraint e)