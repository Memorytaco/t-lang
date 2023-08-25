module Driver.GraphicConstraint
  (

  -- ** monad for graphic constraint generator
    GCGen

  -- ** monad for graphic constraint solver
  , GCSolver

  -- ** common driver
  , driveGCGen
  , driveGCSolver

  -- ** expression constraint generator
  , genGC

  -- ** actual runner for solving graphic constraint
  , solve

  -- ** infer
  , preInfer
  )
where

import Language.Constraint.Graphic
import Language.Generic ((:>+:), type (|:) (..))
import Language.Core (ExprF (..), Expr (..))

import Graph.Core (Hole (..), HasOrderEdge, HasOrderGraph)
import Graph.Extension.GraphicType


import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..))
import Capability.Reader (HasReader, MonadReader (..))
import Capability.Error (HasThrow, HasCatch, MonadError (..), Ctor (..), Rename (..))

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT (..))
import Control.Monad.Reader (ReaderT (..))
import GHC.Generics (Generic)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- ** Driver for constraint generation
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type GCGen' name ns m = ReaderT (BindingTable name ns) (StateT Int (ExceptT (ConstraintGenErr name) m))
newtype GCGen name ns m a = GCGen
  { runGCGen :: GCGen' name ns m a
  } deriving newtype (Functor, Applicative, Monad)
    deriving (HasThrow ConstraintGenErr (ConstraintGenErr name), HasCatch ConstraintGenErr (ConstraintGenErr name))
      via MonadError (GCGen' name ns m)
    deriving (HasState "NodeCreator" Int, HasSink "NodeCreator" Int, HasSource "NodeCreator" Int)
      via MonadState (GCGen' name ns m)
    deriving (HasReader "binding" (BindingTable name ns), HasSource "binding" (BindingTable name ns))
      via MonadReader (GCGen' name ns m)

-- | general driver for constraint generation monad
driveGCGen :: GCGen name ns m a
  -> BindingTable name ns
  -> Int
  -> m (Either (ConstraintGenErr name) (a, Int))
driveGCGen m r i = runExceptT (runStateT (runReaderT (runGCGen m) r) i)

-- | generate stage graphic constraint of expression
genGC :: forall nodes edges name m f target.
  ( target ~ (ExprF f name |: Hole nodes Int)
  , ConstraintGen f target Int
  , ConstrainGraphic f target (GCGen name nodes m) nodes edges Int
  , edges :>+: '[T Sub, T (Binding name), T Unify, T Instance, Pht NDOrderLink, Pht Sub]
  , nodes :>+: '[T NodeBot, G, NDOrder]
  , Traversable f
  , HasOrderEdge edges
  , Eq name
  , Monad m
  ) => Expr f name -> BindingTable name nodes -> Int
  -> m (Either (ConstraintGenErr name) (StageConstraint nodes edges Int target, Int))
genGC e = driveGCGen (genConstraint e)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- ** Driver for constraint solver
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


-- | top level error structure to hold every sub error
data SolverErr name err
  = ConstraintSolvErr (ConstraintSolvErr err)
  | ConstraintGenErr (ConstraintGenErr name)
   deriving (Show, Eq, Ord, Generic)

type GCSolver' name err nodes edges m
  = ReaderT (Unifier err nodes edges Int (GCSolver name err nodes edges m))
      (StateT Int (ExceptT (SolverErr name err) m))
newtype GCSolver name err nodes edges m a = GCSolver
  { runGCSolver ::  GCSolver' name err nodes edges m a
  } deriving newtype (Functor, Applicative, Monad)
    deriving (HasState "NodeCreator" Int, HasSink "NodeCreator" Int, HasSource "NodeCreator" Int)
      via MonadState (GCSolver' name err nodes edges m)

    deriving (HasThrow SolverErr (SolverErr name err), HasCatch SolverErr (SolverErr name err))
      via MonadError (GCSolver' name err nodes edges m)
    deriving (HasThrow ConstraintSolvErr (ConstraintSolvErr err), HasCatch ConstraintSolvErr (ConstraintSolvErr err))
      via Rename "ConstraintSolvErr" (Ctor "ConstraintSolvErr" SolverErr (MonadError (GCSolver' name err nodes edges m)))
    deriving (HasThrow ConstraintGenErr (ConstraintGenErr name), HasCatch ConstraintGenErr (ConstraintGenErr name))
      via Rename "ConstraintGenErr" (Ctor "ConstraintGenErr" SolverErr (MonadError (GCSolver' name err nodes edges m)))

    deriving ( HasReader "Unifier" (Unifier err nodes edges Int (GCSolver name err nodes edges m))
             , HasSource "Unifier" (Unifier err nodes edges Int (GCSolver name err nodes edges m)))
      via MonadReader (GCSolver' name err nodes edges m)

-- | general driver for solver monad
driveGCSolver
  :: GCSolver name err nodes edges m a
  -> Unifier err nodes edges Int (GCSolver name err nodes edges m)
  -> Int
  -> m (Either (SolverErr name err) (a, Int))
driveGCSolver solver unify i = runExceptT (runStateT (runReaderT (runGCSolver solver) unify) i)

-- | solve a stage constraint
solve
  :: forall name err nodes edges m a
  . ( edges :>+: '[Pht Sub, Pht NDOrderLink, T (Binding name), T Unify, T Instance, T Sub]
    , nodes :>+: '[NDOrder, G, T NodeBot]
    , HasOrderGraph nodes edges Int
    , Monad m
    )
  => StageConstraint nodes edges Int a
  -> Unifier err nodes edges Int (GCSolver name err nodes edges m)
  -> Int
  -> m (Either (SolverErr name err) (StageConstraint nodes edges Int a, Int))
solve c = driveGCSolver (solveConstraint @name c)

-- | directly generate presolution from expression
preInfer
  :: forall nodes edges err name f m target
  .  ( target ~ (ExprF f name |: Hole nodes Int)
     , ConstraintGen f target Int
     , ConstrainGraphic f target (GCGen name nodes m) nodes edges Int
     , edges :>+: '[Pht Sub, Pht NDOrderLink, T (Binding name), T Unify, T Instance, T Sub]
     , nodes :>+: '[NDOrder, G, T NodeBot]
     , HasOrderGraph nodes edges Int, Monad m
     , Traversable f, Eq name
     )
  => Expr f name -> BindingTable name nodes -> Int
  -> Unifier err nodes edges Int (GCSolver name err nodes edges m)
  -> m (Either (SolverErr name err) (StageConstraint nodes edges Int target, Int))
preInfer e env i unify = do
  genGC e env i >>= \case
    Right (constraint, i') ->
      solve constraint unify i'
    Left err -> return (Left $ ConstraintGenErr err)