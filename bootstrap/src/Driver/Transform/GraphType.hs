{- | * Driver module, which converts graphic representation of type back to its syntactic representation
-}
module Driver.Transform.GraphType
  (
  -- ** a configuration used to maintain a context
    GraphTypeConf (..)

  -- ** actual runner
  , runGraphType
  )
where

import Tlang.Transform.GraphType
import Tlang.AST (Type)
import Tlang.Graph.Core

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Capability.Reader (HasReader, MonadReader (..), Field (..))
import Capability.State (HasState, modify, gets, MonadState (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)

import GHC.Generics (Generic)
import Data.String (IsString (..))

data GraphTypeConf m nodes edges name = GraphTypeConf
  { graph :: CoreG nodes edges Int  -- ^ a graph to follow paths
  , scheme :: Maybe name -> m name  -- ^ a name scheme
  , local :: [(Hole nodes Int, name)] -- ^ local type name binding
  } deriving Generic

-- | shofthand for monad
type M self nodes edges name m
  = ReaderT (GraphTypeConf self nodes edges name)
      (StateT (String, Int) m)

newtype GraphToTypeM nodes edges name m a = GraphToTypeM
  { runGraphToTypeM :: M (GraphToTypeM nodes edges name m) nodes edges name m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    deriving ( HasSource "local" [(Hole nodes Int, name)], HasReader "local" [(Hole nodes Int, name)])
      via Field "local" () (MonadReader (M (GraphToTypeM nodes edges name m) nodes edges name m))
    deriving ( HasSource "scheme" (Maybe name -> GraphToTypeM nodes edges name m name)
             , HasReader "scheme" (Maybe name -> GraphToTypeM nodes edges name m name)
             ) via Field "scheme" () (MonadReader (M (GraphToTypeM nodes edges name m) nodes edges name m))
    deriving ( HasSource "graph" (CoreG nodes edges Int), HasReader "graph" (CoreG nodes edges Int))
      via Field "graph" () (MonadReader (M (GraphToTypeM nodes edges name m) nodes edges name m))
    deriving (HasSource "name info" (String, Int), HasSink "name info" (String, Int), HasState "name info" (String, Int))
      via MonadState (M (GraphToTypeM nodes edges name m) nodes edges name m)

-- | actual runner to transform a graphic type into its syntatic form
runGraphType :: ( GraphTypeConstrain nodes (GraphToTypeM nodes edges name m) nodes edges Int bind rep name name
                , UnfoldGraphType nodes Int, Monad m, IsString name, Show name)
             => CoreG nodes edges Int -> [(Hole nodes Int, name)] -> (String, Int)
             -> Hole nodes Int -> m (Type bind rep name name, (String, Int))
runGraphType g bindings stat root =
  let reader = runReaderT (runGraphToTypeM $ toSyntacticType root) (GraphTypeConf g defaultScheme bindings)
   in runStateT reader stat

-- | If the original name is provided, we return it unmodified and remember this name for future use
-- , otherwise we generate a new name from integer and hint
--
-- Anyway, we define name scheme here
defaultScheme :: (IsString name, Show name, HasState "name info" (String, Int) m) => Maybe name -> m name
defaultScheme Nothing = modify @"name info" (fmap (+1)) >> gets @"name info" (\(hint, i) -> fromString $ hint <> show i)
defaultScheme (Just name) = modify @"name info" (\(_, i) -> (show name, i)) >> return name
