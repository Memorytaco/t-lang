{- | Transformation module for graphic type

    This module transform syntactic type into relevent graphic representation

-}
module Tlang.Transform.TypeGraph
  (
  -- ** transform type into graph
    toGraph

  -- ** three helpers for handling different structure, it is extendable
  , FoldBinderTypeGraph (..)
  , FoldTypeGraph (..)
  -- ** companion type family for extending environment
  , ConstrainGraph
  )
where

-- ** for graph
import Tlang.Unification.Type
import Tlang.Graph.Core
import Tlang.Graph.Extension.Type
import Tlang.Extension
import Tlang.Generic ((:+:) (..), (:<:))
import Tlang.Rep (Rep (..))

import Capability.State (HasState, get, modify)
import Capability.Reader (HasReader, asks, local)
import Control.Monad (forM, foldM)
import Control.Monad.Identity (Identity (..))

import Data.Functor.Foldable (cata)
import Data.Functor ((<&>))
import Data.Text (Text)

-- ** for type
import qualified Tlang.AST.Type as Type
import Tlang.AST.Type (type (>|) (..))
import Tlang.AST.Type (Bound (..))

-- ** for signature
import Data.Kind (Constraint, Type)

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

node :: HasState "node" Int m => m Int
node = modify @"node" (+1) >> get @"node"

-- ** the algorithm for transforming syntactic type into graphic representation

type instance ConstrainGraph (Type.Type bind f name) nodes edges info m =
  ( FoldTypeGraph f info, ConstrainGraph f nodes edges info m
  , FoldBinderTypeGraph bind info, ConstrainGraph bind nodes edges info m
  , HasReader "local" [(name, (Hole nodes info, CoreG nodes edges info))] m
  , HasState "node" Int m
  , Uno NodeBot :<: nodes, Uno NodeApp :<: nodes, Uno Sub :<: edges
  , Ord (edges (Link edges))
  , MonadFail m
  , Show name, Eq name, Functor f, Functor bind
  )
instance FoldTypeGraph (Type.Type bind f name) Int where
  foldTypeGraph = cata go
    where
      go Type.TypPhtF = node <&> hole' (Uno NodeBot) <&> \v -> (v, Vertex v)
      go (Type.TypVarF v) = v
      go (Type.TypConF m ms) = do
        ns <- sequence $ m: ms
        let len = toInteger $ length ns
        top <- node <&> hole' (Uno $ NodeApp len)
        gs <- forM (zip [1..len] ns) \(i, (sub, subg)) -> return $ Connect (link . Uno $ Sub i) (Vertex top) (Vertex sub) <> subg
        return (top, overlays gs)
      go (Type.TypBndF binder fv) = foldBinderTypeGraph binder . foldTypeGraph $ fv >>= return . \case
        New name -> asks @"local" (lookup name) >>= \case
          Just v -> return v
          Nothing -> fail $ "Unable to find local binding of " <> show name
        Inc m -> m
      go (Type.TypeF v) = foldTypeGraph v


-- | transform a syntactic type into its graphic representation
-- we need to prepare global environment before we start this transformation
toGraph
  :: ( HasState "node" Int m
     , HasReader "global" [(name, (Hole nodes Int, CoreG nodes edges Int))] m
     , HasReader "local" [(name, (Hole nodes Int, CoreG nodes edges Int))] m
     , FoldBinderTypeGraph bind Int, ConstrainGraph bind nodes edges Int m
     , FoldTypeGraph f Int, ConstrainGraph f nodes edges Int m
     , Show name, Eq name
     , Ord (edges (Link edges))
     , Functor bind, Functor f
     , Uno NodeBot :<: nodes, Uno NodeApp :<: nodes, Uno Sub :<: edges
     , MonadFail m
     )
  => Type.Type bind f name name -> m (Hole nodes Int, CoreG nodes edges Int)
toGraph = foldTypeGraph . fmap \name -> asks @"global" (lookup name) >>= \case
  Just v -> return v
  Nothing -> fail $ "Unable to find name " <> show name

-- ** instances

-- *** handle binders

type instance ConstrainGraph (Bound name) nodes edges info m
  = ( Eq (edges (Link edges))
    , Ord (nodes (Hole nodes info)), Ord (edges (Link edges))
    , Uno (Bind name) :<: edges, Uno Sub :<: edges
    , NodePht :<: nodes
    , HasState "node" Int m
    , HasReader "local" [(name, (Hole nodes info, CoreG nodes edges info))] m
    )
-- | handle binder
instance FoldBinderTypeGraph (Bound name) Int where
  foldBinderTypeGraph (name :~ mbound) mbody = do
    a@(var, _) <- mbound
    (root, body) <- local @"local" ((name, a):) mbody
    -- check whether the body is merely bounded type nodes, if so there
    -- is a loop via binding edge, and it needs to add a phantom node here
    -- to remove the self binding loop
    if root == var
    then do
      pht <- hole' NodePht <$> node
      return (pht, overlays [body, var -<< Uno (Bind Flexible 1 (Just name)) >>- pht, pht -<< Uno (Sub 1) >>- root])
    else do
      let shiftLink g (e@(Uno (Bind flag i name')), n) =
            return . overlay (n -<< Uno (Bind flag (i+1) name') >>- root) $ filterLink (/= link' e) n root g
      g' <- foldM shiftLink body $ lTo @(Uno (Bind name)) (== root) body
      return (root, overlay g' $ var -<< Uno (Bind Flexible 1 (Just name)) >>- root)

  foldBinderTypeGraph (name :> mbound) mbody = do
    a@(var, _) <- mbound
    (root, body) <- local @"local" ((name, a):) mbody
    if root == var
    then do
      pht <- hole' NodePht <$> node
      return (pht, overlays [body, var -<< Uno (Bind Flexible 1 (Just name)) >>- pht, pht -<< Uno (Sub 1) >>- root])
    else do
      let shiftLink g (e@(Uno (Bind flag i name')), n) =
            return . overlay (n -<< Uno (Bind flag (i+1) name') >>- root) $ filterLink (/= link' e) n root g
      g' <- foldM shiftLink body $ lTo @(Uno (Bind name)) (== root) body
      return (root, overlay g' $ var -<< Uno (Bind Flexible 1 (Just name)) >>- root)
    -- if hasVertex (== var) body
    -- then do
    --   let shiftLink g (e@(Uno (Bind flag i name')), n) =
    --         return . overlay (n -<< Uno (Bind flag (i+1) name') >>- root) $ filterLink (/= link' e) n root g
    --   g' <- foldM shiftLink body $ lTo @(Uno (Bind name)) (== root) body
    --   return (root, overlay g' $ var -<< Uno (Bind Rigid 1 (Just name)) >>- root)
    -- else return (root, body)

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
  = ( Uno Sub :<: edges, Uno NodeTup :<: nodes
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , Functor m, HasState "node" Int m
    )
-- | handle `Tuple`
instance FoldTypeGraph Tuple Int where
  foldTypeGraph (Tuple ms) = do
    gs <- sequence ms
    let len = toInteger $ length gs
    root <- node <&> hole' (Uno $ NodeTup len)
    g <- foldM (\g (i, (a, g')) -> return $ overlay (g <> g') $ root -<< Uno (Sub i) >>- a) Empty $ zip [1..len] gs
    return (root, g)


type instance ConstrainGraph (Record label) nodes edges info m
  = ( Uno (NodeHas label) :<: nodes, Uno NodeRec :<: nodes, Uno Sub :<: edges
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , HasState "node" Int m
    )
-- | handle `Record`
instance FoldTypeGraph (Record label) Int where
  foldTypeGraph (Record ms) = do
    gs <- forM ms sequence
    let len = toInteger $ length gs
    root <- node <&> hole' (Uno $ NodeRec len)
    g <- (\f -> foldM f Empty $ zip [1..len] gs) \g (i, (l, (a, g'))) -> do
      label <- node <&> hole' (Uno $ NodeHas True l)
      return $ overlays [root -<< Uno (Sub i) >>- label, label -<< Uno (Sub 1) >>- a, g', g]
    return (root, g)


type instance ConstrainGraph (Variant label) nodes edges info m
  = ( Uno (NodeHas label) :<: nodes, Uno NodeRec :<: nodes, Uno Sub :<: edges
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , HasState "node" Int m
    )
-- | handle `Variant`
instance FoldTypeGraph (Variant label) Int where
  foldTypeGraph (Variant ms) = do
    gs <- mapM (mapM sequence) ms
    let len = toInteger $ length gs
    root <- node <&> hole' (Uno $ NodeRec len)
    g <- (\f -> foldM f Empty $ zip [1..len] gs) \g (i, (l, val)) -> do
      label <- node <&> hole' (Uno $ NodeHas True l)
      return . overlays $
        [ overlays [root -<< Uno (Sub i) >>- label, g]
        , maybe Empty (\(a, g') -> overlays [label -<< Uno (Sub 1) >>- a, g']) val
        ]
    return (root, g)

-- | handle `LiteralText'
type instance ConstrainGraph LiteralText nodes edges info m
  = ( Uno (NodeLit Text) :<: nodes, HasState "node" Int m)
instance FoldTypeGraph LiteralText Int where
  foldTypeGraph (LiteralText (getLiteral -> lit)) = node <&> hole' (Uno $ NodeLit lit) <&> \v -> (v, Vertex v)

-- | handle `LiteralNatural'
type instance ConstrainGraph LiteralNatural nodes edges info m
  = ( Uno (NodeLit Integer) :<: nodes, HasState "node" Int m)
instance FoldTypeGraph LiteralNatural Int where
  foldTypeGraph (LiteralNatural (getLiteral -> lit)) = node <&> hole' (Uno $ NodeLit lit) <&> \v -> (v, Vertex v)


-- *** handle Injectors

type instance ConstrainGraph Identity nodes edges info m = ()
-- | `Identity` injector does nothing
instance FoldTypeGraph Identity Int where
  foldTypeGraph (Identity m) = m

-- *** handle RuntimeRep

type instance ConstrainGraph Rep nodes edges info m = ()
instance FoldTypeGraph Rep Int where
  foldTypeGraph (Rep _) = error "TODO: implement"
