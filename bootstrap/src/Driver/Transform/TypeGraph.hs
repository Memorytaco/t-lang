module Driver.Transform.TypeGraph
  ( toGraphicType
  , buildTypeContext
  , ToGraphicTypeErr (..)
  , toGraphicTypeMock
  , toGraphicTypeConstraintMock
  , mockGlobal
  , mockGlobalConstraint
  )
where

import Transform.TypeGraph
  (toGraph, ConstrainGraph, FoldBinderTypeGraph, FoldTypeGraph, TypeContext, ToGraphicTypeErr (..), TypeContextTable, ConstrainBinderGraph)
import Language.Core (Type, type (+>) (Free))
import Graph.Extension.GraphicType
import Graph.Core (CoreG, Hole, HasOrderEdge, Graph (..), overlays, (-<<), (>>-))
import Language.Generic ((:<:), (:>+:))
import Language.Setting (NodeCreator, node)

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Except (ExceptT (..) , runExceptT)
import Capability.Reader (HasReader, MonadReader (..))
import Capability.Source (HasSource)
import Capability.Sink (HasSink)
import Capability.State (HasState, MonadState (..))
import Data.Bifunctor (first)
import Data.String (IsString (..))
import Capability.Error ( HasCatch, HasThrow, MonadError(..) )

-- | shorthand for monad
type M name nodes edges m
  = ReaderT (TypeContextTable name nodes edges Int)
      (StateT Int (ExceptT (ToGraphicTypeErr name) m))

newtype ToGraphicType name nodes edges m a = TypeToGraphM
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
  :: ( ConstrainBinderGraph bind name nodes edges Int (ToGraphicType name nodes edges m)
     , ConstrainGraph rep nodes edges Int (ToGraphicType name nodes edges m)
     , FoldBinderTypeGraph bind name Int, FoldTypeGraph rep Int
     , HasOrderEdge edges
     , Functor (bind name), Functor rep
     , T NodeBot :<: nodes
     , T NodeApp :<: nodes, T Sub :<: edges
     , Eq name, Monad m
     )
  => (name -> ToGraphicType name nodes edges m (Maybe (Hole nodes Int, CoreG nodes edges Int)))
  -> TypeContextTable name nodes edges Int
  -> Int -> Type bind rep name name
  -> m (Either (ToGraphicTypeErr name) ((Hole nodes Int, CoreG nodes edges Int), Int))
toGraphicType lookupGlobal r seed t = runExceptT $ runStateT (runReaderT (runTypeToGraphM $ toGraph lookupGlobal t) r) seed

-- | `toGraphicType` with mocked global type definition
toGraphicTypeConstraintMock
  :: ( ConstrainBinderGraph bind name nodes edges Int (ToGraphicType name nodes edges m)
     , ConstrainGraph rep nodes edges Int (ToGraphicType name nodes edges m)
     , FoldBinderTypeGraph bind name Int, FoldTypeGraph rep Int
     , HasOrderEdge edges
     , Functor (bind name), Functor rep
     , nodes :>+: '[T NodeBot, T NodeApp, T NodeArr, T (NodeRef name), G]
     , edges :>+: '[T Sub, T (Binding name)]
     , Eq name, IsString name, Monad m
     )
  => TypeContextTable name nodes edges Int
  -> Int -> Type bind rep name name
  -> m (Either (ToGraphicTypeErr name) ((Hole nodes Int, CoreG nodes edges Int), Int))
toGraphicTypeConstraintMock = toGraphicType mockGlobalConstraint

-- | transform syntactic type into graphic type in an virtual environment.
toGraphicTypeMock
  :: ( ConstrainBinderGraph bind name nodes edges Int (ToGraphicType name nodes edges m)
     , ConstrainGraph rep nodes edges Int (ToGraphicType name nodes edges m)
     , FoldBinderTypeGraph bind name Int, FoldTypeGraph rep Int
     , HasOrderEdge edges
     , Functor (bind name), Functor rep
     , nodes :>+: '[T NodeBot, T NodeApp, T NodeArr, T (NodeRef name)]
     , T Sub :<: edges
     , Eq name, IsString name, Monad m
     )
  => TypeContextTable name nodes edges Int
  -> Int -> Type bind rep name name
  -> m (Either (ToGraphicTypeErr name) ((Hole nodes Int, CoreG nodes edges Int), Int))
toGraphicTypeMock = toGraphicType mockGlobal

buildTypeContext
  :: [(name, (Hole nodes Int, CoreG nodes edges Int))]
  -> TypeContextTable name nodes edges Int
buildTypeContext = fmap (first Free)

mockGlobalConstraint
  :: forall name nodes edges m
  . ( IsString name, Monad m
    , nodes :>+: '[T NodeArr, T (NodeRef name), G]
    , edges :>+: '[T Sub, T (Binding name)]
    , Eq name
    )
  => name -> ToGraphicType name nodes edges m (Maybe (Hole nodes Int, CoreG nodes edges Int))
mockGlobalConstraint name = do
  g <- node (G 1)
  r <- if fromString "->" == name
  then node (T NodeArr)
  else node (T $ NodeRef mempty name)
  return . Just . (g, ) $ overlays
    [ r -<< T (Binding Flexible $ Nothing @name) >>- g
    , g -<< T (Sub 1) >>- r
    ]

-- | TODO: Refine this utility for helping testing.
mockGlobal
  :: forall name nodes edges m
  . ( IsString name, Monad m
    , nodes :>+: '[T NodeArr, T (NodeRef name)]
    , Eq name
    )
  => name -> ToGraphicType name nodes edges m (Maybe (Hole nodes Int, CoreG nodes edges Int))
mockGlobal name = do
  r <- if fromString "->" == name
    then node (T NodeArr)
    else node (T $ NodeRef mempty name)
  return $ Just (r, Vertex r)
