{- | Transformation module for graphic type

    This module transform syntactic type into relevent graphic representation

-}
module Tlang.Transform.TypeGraph
  (
  -- ** transform type into graph
    toGraph

  -- ** three helpers for handling different structure, it is extendable
  , BinderGraph (..)
  , LiteralGraph (..)
  , InjGraph (..)
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

import Capability.State (HasState, get, modify)
import Capability.Reader (HasReader, ask, local)
import Control.Monad (forM, foldM)
import Control.Monad.Identity (Identity (..))

import Data.Functor.Foldable (cata)
import Data.Functor ((<&>))
import Data.Text (Text)

-- ** for type
import qualified Tlang.AST.Type as Type
import Tlang.AST.Type (Bound (..))

-- ** for signature
import Data.Kind (Constraint, Type)

-- ** definition for handler

type ConstrainGraph :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
type family ConstrainGraph source nodes edges info m :: Constraint

-- | for handling binder structure
class BinderGraph binder info | binder -> info where
  handleBinder
    :: ConstrainGraph binder nodes edges info m
    => binder (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)
    -> m (Hole nodes info, CoreG nodes edges info)

-- | for handling literal, type constructor
class LiteralGraph literal info | literal -> info where
  handleLiteral
    :: ConstrainGraph literal nodes edges info m
    => literal (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)

-- | for handling type extension
class InjGraph injector info | injector -> info where
  handleInj
    :: ConstrainGraph injector nodes edges info m
    => injector (m (Hole nodes info, CoreG nodes edges info))
    -> m (Hole nodes info, CoreG nodes edges info)

-- ** definition for `:+:`
type instance ConstrainGraph (f :+: g) nodes edges info m
  = (ConstrainGraph f nodes edges info m, ConstrainGraph g nodes edges info m)

instance (BinderGraph f info, BinderGraph g info) => BinderGraph (f :+: g) info where
  handleBinder (Inl v) = handleBinder v
  handleBinder (Inr v) = handleBinder v
instance (LiteralGraph f info, LiteralGraph g info) => LiteralGraph (f :+: g) info where
  handleLiteral (Inl v) = handleLiteral v
  handleLiteral (Inr v) = handleLiteral v
instance (InjGraph f info, InjGraph g info) => InjGraph (f :+: g) info where
  handleInj (Inl v) = handleInj v
  handleInj (Inr v) = handleInj v

-- ** general method for the algorithm

node :: HasState "node" Int m => m Int
node = modify @"node" (+1) >> get @"node"

localName :: (Eq name, HasReader "variable" [(name, (Hole nodes info, CoreG nodes edges info))] m)
          => name -> m (Maybe (Hole nodes info, CoreG nodes edges info))
localName name = ask @"variable" >>= return . lookup name

-- ** the algorithm for transforming syntactic type into graphic representation

-- | transform a syntactic type into its graphic representation
-- TODO: we need to handle type alias for global type names
toGraph
  :: ( HasState "node" Int m, BinderGraph bind Int, LiteralGraph cons Int, InjGraph inj Int
     , HasReader "variable" [(name, (Hole nodes Int, CoreG nodes edges Int))] m
     , ConstrainGraph cons nodes edges Int m, ConstrainGraph bind nodes edges Int m, ConstrainGraph inj nodes edges Int m
     , Eq name, Ord (edges (Link edges)), Traversable inj, Traversable bind, Traversable cons
     -- nodes constraint
     , Uno (NodeRep rep) :<: nodes, Uno (NodeRef name) :<: nodes, Uno NodeBot :<: nodes
     , Uno NodeApp :<: nodes
     -- edges constraint
     , Uno Sub :<: edges
     )
  => Type.Type name cons bind inj rep -> m (Hole nodes Int, CoreG nodes edges Int)
toGraph = cata go
  where
    go Type.TypPhtF = node <&> hole' (Uno NodeBot) <&> \v -> (v, Vertex v)
    go (Type.TypRepF r) = node <&> hole' (Uno $ NodeRep r) <&> \v -> (v, Vertex v)
    go (Type.TypRefF name) = localName name >>= \case
      Just a -> return a
      -- TOOD: handle type alias, allow a context for nominal type
      Nothing -> node <&> hole' (Uno $ NodeRef False name) <&> \v -> (v, Vertex v)
    go (Type.TypLitF v) = handleLiteral v
    go (Type.TypConF m ms) = do
      ns <- sequence $ m: ms
      let len = toInteger $ length ns
      top <- node <&> hole' (Uno $ NodeApp len)
      gs <- forM (zip [1..len] ns) \(i, (sub, subg)) -> return $ Connect (link . Uno $ Sub i) (Vertex top) (Vertex sub) <> subg
      return (top, overlays gs)
    go (Type.TypLetF binder v) = handleBinder binder v
    go (Type.TypInjF v) = handleInj v

-- ** instances

-- *** handle binders

type instance ConstrainGraph (Bound name) nodes edges info m
  = ( Eq (edges (Link edges))
    , Ord (nodes (Hole nodes info)), Ord (edges (Link edges))
    , Uno (Bind name) :<: edges
    , HasReader "variable" [(name, (Hole nodes info, CoreG nodes edges info))] m
    )
-- | handle binder
instance BinderGraph (Bound name) Int where
  handleBinder (name :~ mbound) mbody = do
    a@(var, _) <- mbound
    (root, body) <- local @"variable" ((name, a):) mbody
    if hasVertex (== var) body
    then do
      let shiftLink g (e@(Uno (Bind flag i name')), n) =
            return . overlay (n -<< Uno (Bind flag (i+1) name') >>- root) $ filterLink (/= link' e) n root g
      g' <- foldM shiftLink body $ lTo @(Uno (Bind name)) (== root) body
      return (root, overlay g' $ var -<< Uno (Bind Flexible 1 (Just name)) >>- root)
    else return (root, body)
  handleBinder (name :> mbound) mbody = do
    a@(var, _) <- mbound
    (root, body) <- local @"variable" ((name, a):) mbody
    if hasVertex (== var) body
    then do
      let shiftLink g (e@(Uno (Bind flag i name')), n) =
            return . overlay (n -<< Uno (Bind flag (i+1) name') >>- root) $ filterLink (/= link' e) n root g
      g' <- foldM shiftLink body $ lTo @(Uno (Bind name)) (== root) body
      return (root, overlay g' $ var -<< Uno (Bind Rigid 1 (Just name)) >>- root)
    else return (root, body)

type instance ConstrainGraph (Forall f) nodes edges info m = ConstrainGraph f nodes edges info m
-- | handle `Forall` binder
instance BinderGraph f Int => BinderGraph (Forall f) Int where
  handleBinder (Forall v) = handleBinder v
type instance ConstrainGraph (Scope f) nodes edges info m = ConstrainGraph f nodes edges info m
-- | handle `Scope` binder
instance BinderGraph f Int => BinderGraph (Scope f) Int where
  handleBinder (Scope v) = handleBinder v

-- *** handle literals

type instance ConstrainGraph Tuple nodes edges info m
  = ( Uno Sub :<: edges, Uno NodeTup :<: nodes
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes info))
    , Functor m, HasState "node" Int m
    )
-- | handle `Tuple`
instance LiteralGraph Tuple Int where
  handleLiteral (Tuple ms) = do
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
instance LiteralGraph (Record label) Int where
  handleLiteral (Record ms) = do
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
instance LiteralGraph (Variant label) Int where
  handleLiteral (Variant ms) = do
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
instance LiteralGraph LiteralText Int where
  handleLiteral (LiteralText (getLiteral -> lit)) = node <&> hole' (Uno $ NodeLit lit) <&> \v -> (v, Vertex v)

-- | handle `LiteralNatural'
type instance ConstrainGraph LiteralNatural nodes edges info m
  = ( Uno (NodeLit Integer) :<: nodes, HasState "node" Int m)
instance LiteralGraph LiteralNatural Int where
  handleLiteral (LiteralNatural (getLiteral -> lit)) = node <&> hole' (Uno $ NodeLit lit) <&> \v -> (v, Vertex v)


-- *** handle Injectors

type instance ConstrainGraph Identity nodes edges info m = ()
-- | `Identity` injector does nothing
instance InjGraph Identity Int where
  handleInj (Identity m) = m
