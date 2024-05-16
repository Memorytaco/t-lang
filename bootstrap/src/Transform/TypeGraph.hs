{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Transformation module for graphic type
--
--  This module transform syntactic type into relevent graphic representation
module Transform.TypeGraph
  (
  -- ** transform type into graph
    toGraph

  -- ** three helpers for handling different structure, it is extendable
  , FoldBinderTypeGraph (..)
  , FoldTypeGraph (..)
  -- ** companion type family for extending environment
  , ConstrainGraph
  , ConstrainBinderGraph

  , TypeContext
  , HasTypeContext
  , TypeContextTable

  , HasToGraphicTypeErr
  , ToGraphicTypeErr (..)
  )
where

-- ** for graph
import Graph.Core
import Graph.Extension.GraphicType
import Language.Core.Extension
import Language.Generic ((:+:) (..), (:<:), (:++:))
import Language.DataLayout (Rep (..))
import Language.Setting (HasNodeCreator, node)

import Capability.Reader (HasReader, asks, local)
import Control.Monad (forM, foldM)

import Data.Functor.Foldable (cata)
import Data.Functor ((<&>))

-- ** for type
import qualified Language.Core as Type
import Language.Core (type (+>) (..), Prefix (..))

-- ** for signature
import Data.Kind (Constraint, Type)
import Capability.Error
import Language.Generic.Data ((:++:)(..))

-- ** definition for handler

type ConstrainGraph :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
type family ConstrainGraph source nodes edges info m :: Constraint
type ConstrainBinderGraph :: (Type -> Type -> Type) -> Type -> (Type -> Type) -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
type family ConstrainBinderGraph source name nodes edges info m :: Constraint

-- | for handling binder structure
class FoldBinderTypeGraph binder name info | binder name -> info where
  foldBinderTypeGraph
    :: ConstrainBinderGraph binder name nodes edges info m
    => binder name (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)

-- | for handling representation, type constructor
class FoldTypeGraph f info | f -> info where
  foldTypeGraph
    :: ConstrainGraph f nodes edges info m
    => f (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)

-- ** definition for `:+:`
type instance ConstrainBinderGraph (f :++: g) name nodes edges info m
  = (ConstrainBinderGraph f name nodes edges info m, ConstrainBinderGraph g name nodes edges info m)
type instance ConstrainGraph (f :+: g) nodes edges info m
  = (ConstrainGraph f nodes edges info m, ConstrainGraph g nodes edges info m)

instance (FoldBinderTypeGraph f name info, FoldBinderTypeGraph g name info) => FoldBinderTypeGraph (f :++: g) name info where
  foldBinderTypeGraph (Inll v) = foldBinderTypeGraph v
  foldBinderTypeGraph (Inrr v) = foldBinderTypeGraph v
instance (FoldTypeGraph f info, FoldTypeGraph g info) => FoldTypeGraph (f :+: g) info where
  foldTypeGraph (Inl v) = foldTypeGraph v
  foldTypeGraph (Inr v) = foldTypeGraph v

-- ** general method for the algorithm

data TypeContext
type HasTypeContext name nodes edges info m =
  (HasReader TypeContext (TypeContextTable name nodes edges info) m)
type TypeContextTable name nodes edges info
  = [(name +> name, (Hole nodes info, CoreG nodes edges info))]

lookupLocalName
  :: (Eq name, HasTypeContext name nodes edges info m)
  => name -> m (Maybe (Hole nodes info, CoreG nodes edges info))
lookupLocalName name
  = asks @TypeContext (lookup $ Bind name)

-- | with free name, we first lookup it in a predefined environment and if
-- failed, we invoke a dynamic function to fetch its name.
lookupFreeName
  :: (Eq name, HasTypeContext name nodes edges info m, HasToGraphicTypeErr name m)
  => (name -> m (Maybe (Hole nodes info, CoreG nodes edges info)))
  -> name -> m (Hole nodes info, CoreG nodes edges info)
lookupFreeName lookupGlobal name
  = lookupLocalName name >>= \case
    Just a -> return a
    Nothing -> asks @TypeContext (lookup $ Free name)
      >>= maybe (lookupGlobal name) (return . Just)
      >>= maybe (failToGraphicTypeErr (TGMissBind name)) return

-- | internal use only, it is useful when you want to add local type binding to type context
appendContext
  :: HasTypeContext name nodes edges info m
  => TypeContextTable name nodes edges info
  -> m a -> m a
appendContext tbl = local @TypeContext (tbl <>)

-- | error used for transforming type into graphic type
newtype ToGraphicTypeErr name
  -- | variables that is locally or globally bound
  = TGMissBind name
  deriving (Show, Eq, Ord)

type HasToGraphicTypeErr name m
  = ( HasThrow ToGraphicTypeErr (ToGraphicTypeErr name) m
    , HasCatch ToGraphicTypeErr (ToGraphicTypeErr name) m
    )

failToGraphicTypeErr :: HasToGraphicTypeErr name m => ToGraphicTypeErr name -> m a
failToGraphicTypeErr = throw @ToGraphicTypeErr

-- | move bound site from `from` to `to`
moveBindingEdge
  :: forall name nodes edges info
   . (T (Binding name) :<: edges, HasOrderGraph nodes edges info)
  => Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
moveBindingEdge from to = einduce $ tryLink @(T (Binding name)) (\e a b -> Just (e,a,b))
  -- if `b` is just `from`, then replace it with `to`. `einduce` will automatically
  -- do edge deletion so edge (a --> from) doesn't exist any more.
  \e a b -> if b == from then Just (link e, a, to) else Just (link e, a, b)

-- ** the algorithm for transforming syntactic type into graphic representation

type instance ConstrainGraph (Type.Type bind f name) nodes edges info m =
  ( FoldTypeGraph f info, ConstrainGraph f nodes edges info m
  , FoldBinderTypeGraph bind name info, ConstrainBinderGraph bind name nodes edges info m
  , HasTypeContext name nodes edges info m
  , HasNodeCreator m
  , T NodeBot :<: nodes, T NodeApp :<: nodes, T Sub :<: edges
  , Ord (edges (Link edges))
  , HasToGraphicTypeErr name m
  , Eq name, Functor f, Functor (bind name)
  )
instance FoldTypeGraph (Type.Type bind f name) Int where
  foldTypeGraph = cata go
    where
      go Type.TypPhtF = node (T NodeBot) <&> \v -> (v, Vertex v)
      go (Type.TypVarF v) = v
      go (Type.TypConF m ms) = do
        ns <- sequence $ m: ms
        let len = toInteger $ length ns
        top <- node (T $ NodeApp len)
        gs <- forM (zip [1..len] ns) \(i, (sub, subg)) ->
          return $ (top -<< T (Sub i) >>- sub) <> subg
        return (top, overlays gs)
      go (Type.TypBndF binder) = foldBinderTypeGraph binder
      go (Type.TypeF v) = foldTypeGraph v


-- | transform a syntactic type into its graphic representation
-- we need to prepare global environment before we start this transformation
toGraph
  :: ( HasNodeCreator m
     , HasTypeContext name nodes edges Int m
     , FoldBinderTypeGraph bind name Int, ConstrainBinderGraph bind name nodes edges Int m
     , FoldTypeGraph f Int, ConstrainGraph f nodes edges Int m
     , Eq name, Ord (edges (Link edges))
     , Functor (bind name), Functor f
     , T NodeBot :<: nodes, T NodeApp :<: nodes, T Sub :<: edges
     , HasToGraphicTypeErr name m
     )
  => (name -> m (Maybe (Hole nodes Int, CoreG nodes edges Int)))
  -> Type.Type bind f name name -> m (Hole nodes Int, CoreG nodes edges Int)
toGraph lookupGlobal = foldTypeGraph . fmap (lookupFreeName lookupGlobal)

-- ** instances

-- *** handle binders

type instance ConstrainBinderGraph Prefix name nodes edges info m
  = ( HasOrderGraph nodes edges info
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord name, Show name
    , NodePht :<: nodes
    , HasNodeCreator m
    , HasTypeContext name nodes edges info m
    )
type instance ConstrainBinderGraph (Forall Prefix) name nodes edges info m
  = ConstrainBinderGraph Prefix name nodes edges info m
-- | handle `Forall` binder
instance FoldBinderTypeGraph (Forall Prefix) name Int where
  foldBinderTypeGraph (Forall bound mbody) = do
    (name, mbound, flag) <- case bound of
      name :~ mbound -> return (name, mbound, Rigid)
      name :> mbound -> return (name, mbound, Flexible)
    -- convert bounds first
    binding@(var, bbody) <- mbound
    -- convert type body
    (root, body) <- appendContext [(Bind name, binding)] mbody
    -- check whether the body is merely bounded type nodes, if so there
    -- may exist a loop via binding edge, and a phantom node needs to be put here
    -- to remove the self binding loop.
    --
    -- to handle this situation: @forall a. a@
    if root == var then do
      pht <- node NodePht
      -- A phantom node delegates one loop binding node to avoid
      -- infinite self binding and thus can avoid loop in
      -- implementation.
      return . (pht,) . overlays $
        [ moveBindingEdge @name root pht body
        , bbody, var -<< T (Binding flag (Just name)) >>- pht
        , pht -<< T (Sub 1) >>- root
        ]
    else return . (root,) . overlays $
      [ body, bbody
      , var -<< T (Binding flag (Just name)) >>- root
      ]

type instance ConstrainBinderGraph (Scope Prefix) name nodes edges info m = ConstrainBinderGraph Prefix name nodes edges info m
-- | handle `Scope` binder
instance FoldBinderTypeGraph (Scope Prefix) name Int where
  foldBinderTypeGraph (Scope _v _mt) = error "Not implemented, Type system doesn't support type level lambda"

-- *** handle literals

type instance ConstrainGraph Tuple nodes edges info m
  = ( T Sub :<: edges, T NodeTup :<: nodes
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , Functor m, HasNodeCreator m
    )
-- | handle `Tuple`
instance FoldTypeGraph Tuple Int where
  foldTypeGraph (Tuple ms) = do
    gs <- sequence ms
    let len = toInteger $ length gs
    root <- node (T $ NodeTup len)
    g <- foldM (\g (i, (a, g')) -> return $ overlay (g <> g') $ root -<< T (Sub i) >>- a) None $ zip [1..len] gs
    return (root, g)


type instance ConstrainGraph (Record label) nodes edges info m
  = ( T (NodeHas label) :<: nodes, T NodeRec :<: nodes, T Sub :<: edges
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , HasNodeCreator m
    )
-- | handle `Record`
instance FoldTypeGraph (Record label) Int where
  foldTypeGraph (Record ms) = do
    gs <- forM ms sequence
    let len = toInteger $ length gs
    root <- node (T $ NodeRec len)
    g <- (\f -> foldM f None $ zip [1..len] gs) \g (i, (l, (a, g'))) -> do
      label <- node (T $ NodeHas True l)
      return $ overlays [root -<< T (Sub i) >>- label, label -<< T (Sub 1) >>- a, g', g]
    return (root, g)


type instance ConstrainGraph (Variant label) nodes edges info m
  = ( T (NodeHas label) :<: nodes
    , T NodeSum :<: nodes, T Sub :<: edges
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , HasNodeCreator m
    )
-- | handle `Variant`
instance FoldTypeGraph (Variant label) Int where
  foldTypeGraph (Variant ms) = do
    gs <- mapM (mapM sequence) ms
    let len = toInteger $ length gs
    root <- node (T $ NodeSum len)
    g <- (\f -> foldM f None $ zip [1..len] gs) \g (i, (l, val)) -> do
      label <- node (T $ NodeHas True l)
      return . overlays $
        [ overlays [root -<< T (Sub i) >>- label, g]
        , maybe None (\(a, g') -> overlays [label -<< T (Sub 1) >>- a, g']) val
        ]
    return (root, g)

-- | handle `Literal`
type instance ConstrainGraph (Literal lit) nodes edges info m
  = ( T (NodeLit lit) :<: nodes, HasNodeCreator m)
instance FoldTypeGraph (Literal lit) Int where
  foldTypeGraph (Literal lit) = node (T $ NodeLit lit) <&> \v -> (v, Vertex v)

-- *** handle Injectors

type instance ConstrainGraph Identity nodes edges info m = ()
-- | `Identity` injector does nothing
instance FoldTypeGraph Identity Int where
  foldTypeGraph (Identity m) = m

-- *** handle RuntimeRep

type instance ConstrainGraph Rep nodes edges info m = ()
instance FoldTypeGraph Rep Int where
  foldTypeGraph (Rep _) = error "TODO: implement"
