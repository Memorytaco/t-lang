{-# LANGUAGE QuantifiedConstraints, AllowAmbiguousTypes #-}
-- TODO: Complete this module
module Transform.Desugar
  ( pruneForallType

  , foldSugarRule
  , runSugarRule

  , case01
  , case10
  )
where

import Graph.Core
import Graph.Extension.GraphicType

import Language.Core (Type (..), TypeF (..))
import Language.Core.Extension (Forall (..))
import Language.Core.Constraint
import Language.Generic
import Language.Setting

import Data.Function (fix)
import Control.Monad (foldM, forM_)

-- FIXME: Add syntactic sugar rule
pruneForallType
  :: forall name bind rep a
   . ( Functor rep, Functor (bind name)
     , Forall Prefix :<<: bind
     , forall x. Eq x => Eq (bind name x), forall x. Eq x => Eq (rep x)
     , Eq name, Eq a
     )
  => Type bind rep name a -> Type bind rep name a
pruneForallType = cata go
  where
    go TypPhtF = TypPht
    go (TypVarF a) = TypVar a
    go (TypConF a as) = TypCon a as
    go (TypBndF bnd) =
      case prjj @(Forall Prefix) bnd of
        Just (Forall binding a) -> case binding of
          (_name :~ _typ) -> pruneForallType @name a
          (_name :> _typ) -> pruneForallType @name a
        Nothing -> TypBnd bnd
    go (TypeF f) = Type f


-- | Structure to hold a desugar rule
data SugarRule nodes edges info m a
  = HasGraph nodes edges info m
  => SugarRule (Recursion m (Hole nodes info) a)

-- | structure link from, sort by sub edge
sFrom :: (HasGraph nodes edges info m, T Sub :<: edges, HasOrderGraph nodes edges info)
      => Hole nodes info -> m [(T Sub (Link edges), Hole nodes info)]
sFrom = getsGraph . lFrom @(T Sub)

hasBinding
  :: forall name nodes edges info. (Ord name, T (Binding name) :<: edges, HasOrderGraph nodes edges info)
  => Hole nodes info -> CoreG nodes edges info -> Bool
hasBinding root = not . null . lFrom @(T (Binding name)) root

addBinding :: forall name nodes edges info. (T (Binding name) :<: edges)
           => (Hole nodes info, Flag) -> Hole nodes info -> CoreG nodes edges info -> CoreG nodes edges info
addBinding (from, flag) to gr = overlay gr (from -<< T (Binding flag $ Nothing @name) >>- to)

-- | combine a list of desugar rule into one.
--
-- Returned `Bool` indicates whether the rule has been applied to the node.
foldSugarRule :: HasGraph nodes edges info m => [SugarRule nodes edges info m Bool] -> SugarRule nodes edges info m Bool
foldSugarRule = foldr foldT (SugarRule $ Recursion \_ _ -> return False)
  where
    foldT (SugarRule (Recursion f)) (SugarRule (Recursion g)) = SugarRule $ Recursion \sugar n ->
      f sugar n >>= \case
      True -> return True
      False -> g sugar n

-- | driver for sugar rule, extract rules and apply it to node in a monad.
runSugarRule :: SugarRule nodes edges info m a -> Hole nodes info -> m a
runSugarRule (SugarRule (Recursion f)) = fix f

-- | RULE: NodeApp
--
-- TODO: consider arity and variance in 'NodeApp'
case01
  :: forall name nodes edges info m
  . ( edges :>+: '[T Sub, T (Binding name)]
    , nodes :>+: '[T NodeApp]
    , Ord name
    , HasGraph nodes edges info m
    , HasOrderGraph nodes edges info
    )
 => SugarRule nodes edges info m Bool
case01 = SugarRule $ Recursion \sugar ->
  tryHole (const $ return False) \root (T (NodeApp _)) _ -> do
    subs <- fmap snd <$> sFrom root
    gr <- getGraph
    let foldT gr' n =
          if hasBinding @name n gr'
          then return gr'
          else return $ addBinding @name (n, Flexible) root gr'
    foldM foldT gr subs >>= putGraph >> forM_ subs sugar
    return True

-- | RULE: NodeTup
case10
  :: forall name nodes edges info m
  . ( edges :>+: '[T Sub, T (Binding name)]
    , nodes :>+: '[T NodeTup]
    , Ord name
    , HasGraph nodes edges info m
    , HasOrderGraph nodes edges info
    )
 => SugarRule nodes edges info m Bool
case10 = SugarRule $ Recursion \sugar ->
  tryHole (const $ return False) \root (T (NodeTup _)) _ -> do
    subs <- fmap snd <$> sFrom root
    forM_ subs \n -> do
      getsGraph (hasBinding @name n) >>= \case
        True -> return ()
        False -> modifyGraph (addBinding @name (n, Flexible) root)
      sugar n
    return True
