{-# LANGUAGE AllowAmbiguousTypes #-}
{- | Transformation module for graphic type

    This module transform syntactic type into relevent graphic representation

-}
module Transform.TypeGraph
  (
  -- ** transform type into graph
    toGraph

  -- ** three helpers for handling different structure, it is extendable
  , FoldBinderTypeGraph (..)
  , FoldTypeGraph (..)
  -- ** companion type family for extending environment
  , ConstrainGraph

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
import Language.Generic ((:+:) (..), (:<:))
import Tlang.Rep (Rep (..))
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

-- ** definition for handler

type ConstrainGraph :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
type family ConstrainGraph source nodes edges info m :: Constraint

-- | for handling binder structure
class FoldBinderTypeGraph binder info | binder -> info where
  foldBinderTypeGraph
    :: ConstrainGraph binder nodes edges info m
    => binder (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)
    -> m (Hole nodes info, CoreG nodes edges info)

-- | for handling representation, type constructor
class FoldTypeGraph f info | f -> info where
  foldTypeGraph
    :: ConstrainGraph f nodes edges info m
    => f (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)

-- ** definition for `:+:`
type instance ConstrainGraph (f :+: g) nodes edges info m
  = (ConstrainGraph f nodes edges info m, ConstrainGraph g nodes edges info m)

instance (FoldBinderTypeGraph f info, FoldBinderTypeGraph g info) => FoldBinderTypeGraph (f :+: g) info where
  foldBinderTypeGraph (Inl v) = foldBinderTypeGraph v
  foldBinderTypeGraph (Inr v) = foldBinderTypeGraph v
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
  :: (Eq name, HasTypeContext name nodes edges info m, HasToGraphicTypeErr name m)
  => name -> m (Hole nodes info, CoreG nodes edges info)
lookupLocalName name
  = asks @TypeContext (lookup $ Bind name)
  >>= maybe (failToGraphicTypeErr (TGMissLocalBind name)) return

lookupGlobalName
  :: (Eq name, HasTypeContext name nodes edges info m, HasToGraphicTypeErr name m)
  => name -> m (Hole nodes info, CoreG nodes edges info)
lookupGlobalName name
  = asks @TypeContext (lookup $ Free name)
  >>= maybe (failToGraphicTypeErr (TGMissGlobalBind name)) return

-- | internal use only, it is useful when you want to add local type binding to type context
appendContext
  :: HasTypeContext name nodes edges info m
  => TypeContextTable name nodes edges info
  -> m a -> m a
appendContext tbl = local @TypeContext (tbl <>)

-- | error used for transforming type into graphic type
data ToGraphicTypeErr name
  = TGMissGlobalBind name
  | TGMissLocalBind name
  deriving (Show, Eq, Ord)

type HasToGraphicTypeErr name m
  = ( HasThrow ToGraphicTypeErr (ToGraphicTypeErr name) m
    , HasCatch ToGraphicTypeErr (ToGraphicTypeErr name) m
    )

failToGraphicTypeErr :: HasToGraphicTypeErr name m => ToGraphicTypeErr name -> m a
failToGraphicTypeErr = throw @ToGraphicTypeErr

-- | increase binding index by 1, the lower binding index, the outer the binder
-- e.g. forall a b. anything
-- Index are:  1 2
-- a has 1, b has 2
--
-- another one:
--    forall a b c d e f. anything
--           1 2 3 4 5 6
shiftBindingEdge
  :: forall name nodes edges info
   . (T (Binding name) :<: edges, HasOrderGraph nodes edges info)
  => Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
shiftBindingEdge root g = foldl shiftup g $ lTo @(T (Binding name)) (== root) g
  where shiftup gr (e@(T (Binding flag i name)), n) = overlays
          [ n -<< T (Binding flag (i+1) name) >>- root
          , filterLink (/= link' e) n root gr
          ]
{-# INLINE shiftBindingEdge #-}

-- | move bound site from `from` to `to`
moveBindingEdge
  :: forall name nodes edges info
   . ( T (Binding name) :<: edges, HasOrderGraph nodes edges info)
  => Hole nodes info -> Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
moveBindingEdge from to g = foldl move g $ lTo @(T (Binding name)) (== from) g
  where move gr (e@(T (Binding flag i name)), n) = overlays
          [ n -<< T (Binding flag i name) >>- to
          , filterLink (/= link' e) n from gr
          ]
{-# INLINE moveBindingEdge #-}

-- ** the algorithm for transforming syntactic type into graphic representation

type instance ConstrainGraph (Type.Type bind f name) nodes edges info m =
  ( FoldTypeGraph f info, ConstrainGraph f nodes edges info m
  , FoldBinderTypeGraph bind info, ConstrainGraph bind nodes edges info m
  , HasTypeContext name nodes edges info m
  , HasNodeCreator m
  , T NodeBot :<: nodes, T NodeApp :<: nodes, T Sub :<: edges
  , Ord (edges (Link edges))
  , HasToGraphicTypeErr name m
  , Eq name, Functor f, Functor bind
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
        gs <- forM (zip [1..len] ns) \(i, (sub, subg)) -> return $ Connect (link . T $ Sub i) (Vertex top) (Vertex sub) <> subg
        return (top, overlays gs)
      go (Type.TypBndF binder fv) = foldBinderTypeGraph binder . foldTypeGraph $ fv <&> \case
        Bind name -> lookupLocalName name
        Free m -> m
      go (Type.TypeF v) = foldTypeGraph v


-- | transform a syntactic type into its graphic representation
-- we need to prepare global environment before we start this transformation
toGraph
  :: ( HasNodeCreator m
     , HasTypeContext name nodes edges Int m
     , FoldBinderTypeGraph bind Int, ConstrainGraph bind nodes edges Int m
     , FoldTypeGraph f Int, ConstrainGraph f nodes edges Int m
     , Eq name
     , Ord (edges (Link edges))
     , Functor bind, Functor f
     , T NodeBot :<: nodes, T NodeApp :<: nodes, T Sub :<: edges
     , HasToGraphicTypeErr name m
     )
  => Type.Type bind f name name -> m (Hole nodes Int, CoreG nodes edges Int)
toGraph = foldTypeGraph . fmap lookupGlobalName

-- ** instances

-- *** handle binders

type instance ConstrainGraph (Prefix name) nodes edges info m
  = ( HasOrderGraph nodes edges info
    , T (Binding name) :<: edges, T Sub :<: edges
    , NodePht :<: nodes
    , HasNodeCreator m
    , HasTypeContext name nodes edges info m
    )
-- | handle binder
instance FoldBinderTypeGraph (Prefix name) Int where
  foldBinderTypeGraph bounds mbody = do
    -- fetch relevant binding name, its bounded type and permission
    (name, mbound, flag) <- case bounds of
      name :~ mbound -> return (name, mbound, Rigid)
      name :> mbound -> return (name, mbound, Flexible)
    -- convert bounds first
    a@(var, bbody) <- mbound
    -- convert type body
    (root, body) <- appendContext [(Bind name, a)] mbody
    -- check whether the body is merely bounded type nodes, if so there
    -- may exist a loop via binding edge, and a phantom node needs to be put here
    -- to remove the self binding loop
    if root == var then do
      pht <- node NodePht
      -- TODO: explain why we need to move all binding edges from old root to the phantom node
      return (pht, overlays [ moveBindingEdge @name root pht $ shiftBindingEdge @name root body
                            , bbody, var -<< T (Binding flag 1 (Just name)) >>- pht
                            , pht -<< T (Sub 1) >>- root])
    else return (root, overlays [shiftBindingEdge @name root body, bbody, var -<< T (Binding flag 1 (Just name)) >>- root])

type instance ConstrainGraph (Forall f) nodes edges info m = ConstrainGraph f nodes edges info m
-- | handle `Forall` binder
instance FoldBinderTypeGraph f Int => FoldBinderTypeGraph (Forall f) Int where
  foldBinderTypeGraph (Forall v) = foldBinderTypeGraph v
type instance ConstrainGraph (Scope f) nodes edges info m = ConstrainGraph f nodes edges info m
-- | handle `Scope` binder
instance FoldBinderTypeGraph f Int => FoldBinderTypeGraph (Scope f) Int where
  foldBinderTypeGraph (Scope v) = foldBinderTypeGraph v

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
    g <- foldM (\g (i, (a, g')) -> return $ overlay (g <> g') $ root -<< T (Sub i) >>- a) Empty $ zip [1..len] gs
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
    g <- (\f -> foldM f Empty $ zip [1..len] gs) \g (i, (l, (a, g'))) -> do
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
    g <- (\f -> foldM f Empty $ zip [1..len] gs) \g (i, (l, val)) -> do
      label <- node (T $ NodeHas True l)
      return . overlays $
        [ overlays [root -<< T (Sub i) >>- label, g]
        , maybe Empty (\(a, g') -> overlays [label -<< T (Sub 1) >>- a, g']) val
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
