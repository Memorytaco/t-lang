{- | * Driver module, which converts graphic representation of type back to its syntactic representation
-}
module Driver.Transform.GraphType
  (
  -- ** a configuration used to maintain a context
    GraphTypeConf (..)

  -- ** actual runner
  , runGraphType
  , syntacticType
  )
where

import Transform.GraphType
import Language.Core (Type, Label)
import Language.Core.Extension
import Language.Generic ((:>+:))
import Language.Core.Constraint
import Language.Setting
import Graph.Extension.GraphicType
import Graph.Core

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Except (ExceptT (..), runExceptT)

import Capability.Reader (HasReader, MonadReader (..), Field (..), Rename (..))
import Capability.State (HasState, modify, gets, MonadState (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)
import Capability.Error (HasThrow, HasCatch, MonadError (..))

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Text (Text)

data GraphTypeConf m nodes edges name = GraphTypeConf
  { graph :: CoreG nodes edges Int  -- ^ a graph to follow paths
  , scheme :: Maybe name -> m name  -- ^ a name scheme
  , local :: [(Hole nodes Int, name)] -- ^ local type name binding
  } deriving Generic

-- | shofthand for monad
type GraphToType' nodes edges name m
  = ReaderT (GraphTypeConf (GraphToType nodes edges name m) nodes edges name)
      (StateT (String, Int) (ExceptT (GraphToTypeErr (Hole nodes Int)) m))

newtype GraphToType nodes edges name m a = GraphToType
  { runGraphToType :: GraphToType' nodes edges name m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    deriving ( HasSource "local" [(Hole nodes Int, name)], HasReader "local" [(Hole nodes Int, name)])
      via Field "local" () (MonadReader (GraphToType' nodes edges name m))
    deriving ( HasSource "scheme" (Maybe name -> GraphToType nodes edges name m name)
             , HasReader "scheme" (Maybe name -> GraphToType nodes edges name m name)
             ) via Field "scheme" () (MonadReader (GraphToType'  nodes edges name m))
    deriving ( HasSource GraphReader (CoreG nodes edges Int), HasReader GraphReader (CoreG nodes edges Int))
      via Rename "graph" (Field "graph" () (MonadReader (GraphToType' nodes edges name m)))
    deriving (HasSource "name info" (String, Int), HasSink "name info" (String, Int), HasState "name info" (String, Int))
      via MonadState (GraphToType' nodes edges name m)
    deriving (HasThrow GraphToTypeErr (GraphToTypeErr (Hole nodes Int)), HasCatch GraphToTypeErr (GraphToTypeErr (Hole nodes Int)))
      via MonadError (GraphToType' nodes edges name m)

-- | actual runner to transform a graphic type into its syntatic form
runGraphType :: (Monad m, IsString name, Show name)
             => CoreG nodes edges Int -> [(Hole nodes Int, name)] -> (String, Int)
             -> Conversion nodes edges Int bind rep name (GraphToType nodes edges name m) name
             -> Hole nodes Int
             -> m (Either (GraphToTypeErr (Hole nodes Int)) (Type bind rep name name, (String, Int)))
runGraphType gr bindings stat c root =
  let reader = runReaderT (runGraphToType $ runConversion c root) (GraphTypeConf gr defaultScheme bindings)
   in runExceptT $ runStateT reader stat

syntacticType
  :: ( HasEqNode nodes Int
     , nodes :>+: '[T (NodeRef name), T NodeTup, T NodeArr, T (NodeLit Integer), T (NodeLit Text)]
     , nodes :>+: '[T (NodeHas Label), T NodeRec, T NodeSum, T NodeApp, NodePht, T NodeBot]
     , edges :>+: '[T Sub, T (Binding name)]
     , rep :>+: '[Tuple, Literal Integer, Literal Text, Record Label, Variant Label]
     , bind :>+: '[Forall (Prefix name)]
     , Functor rep, Functor bind
     , HasOrderGraph nodes edges Int, Ord name, IsString name
     , Monad m
     )
  => Conversion nodes edges Int bind rep name (GraphToType nodes edges name m) name
syntacticType = treeConversion
  [ special02 -- handle bottom
  , literal01
  , literal02
  , literal03
  , literal04 @Integer
  , literal04 @Text
  , literal05 @Label
  , literal06 @Label
  , structure01
  , special01
  ]

-- | If the original name is provided, we return it unmodified and remember this name for future use
-- , otherwise we generate a new name from integer and hint
--
-- Anyway, we define name scheme here
defaultScheme :: (IsString name, Show name, HasState "name info" (String, Int) m) => Maybe name -> m name
defaultScheme Nothing = modify @"name info" (fmap (+1)) >> gets @"name info" (\(hint, i) -> fromString $ hint <> show i)
defaultScheme (Just name) = modify @"name info" (\(_, i) -> (show name, i)) >> return name
