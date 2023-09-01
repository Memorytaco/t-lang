module Driver.Transform.TypeGraph
  ( toGraphicType
  , buildTypeContext
  , ToGraphicTypeErr (..)
  )
where

import Transform.TypeGraph (toGraph, ConstrainGraph, FoldBinderTypeGraph, FoldTypeGraph, TypeContext, ToGraphicTypeErr (..), TypeContextTable)
import Language.Core (Type, type (+>) (Free))
import Graph.Extension.GraphicType
import Graph.Core ( CoreG, Hole, Link )
import Language.Generic ((:<:))
import Language.Setting ( NodeCreator )

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Except (ExceptT (..) , runExceptT)
import Capability.Reader (HasReader, MonadReader (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)
import Capability.State (HasState, MonadState (..))
import Data.Bifunctor (first)
import Capability.Error ( HasCatch, HasThrow, MonadError(..) )

-- | shofthand for monad
type M name nodes edges m
  = ReaderT (TypeContextTable name nodes edges Int)
      (StateT Int (ExceptT (ToGraphicTypeErr name) m))

newtype TypeToGraphM name nodes edges m a = TypeToGraphM
  { runTypeToGraphM :: M name nodes edges m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    deriving ( HasSource TypeContext (TypeContextTable name nodes edges Int)
             , HasReader TypeContext (TypeContextTable name nodes edges Int)
             ) via (MonadReader (M name nodes edges m))
    -- assign a unique token to each node which helps keep graph structure steady
    deriving (HasState NodeCreator Int, HasSource NodeCreator Int, HasSink NodeCreator Int)
          via MonadState (M name nodes edges m)
    deriving (HasThrow ToGraphicTypeErr (ToGraphicTypeErr name), HasCatch ToGraphicTypeErr (ToGraphicTypeErr name))
      via MonadError (M name nodes edges m)

-- | actual driver to start the engine, and it allows monad transform
toGraphicType
  :: ( ConstrainGraph bind nodes edges Int (TypeToGraphM name nodes edges m)
     , ConstrainGraph rep nodes edges Int (TypeToGraphM name nodes edges m)
     , FoldBinderTypeGraph bind Int, FoldTypeGraph rep Int
     , Ord (edges (Link edges))
     , Functor bind, Functor rep
     , T NodeBot :<: nodes
     , T NodeApp :<: nodes, T Sub :<: edges
     , Eq name, Monad m
     )
  => TypeContextTable name nodes edges Int
  -> Int -> Type bind rep name name
  -> m (Either (ToGraphicTypeErr name) ((Hole nodes Int, CoreG nodes edges Int), Int))
toGraphicType r seed t = runExceptT $ runStateT (runReaderT (runTypeToGraphM $ toGraph t) r) seed

buildTypeContext
  :: [(name, (Hole nodes Int, CoreG nodes edges Int))]
  -> TypeContextTable name nodes edges Int
buildTypeContext = fmap (first Free)