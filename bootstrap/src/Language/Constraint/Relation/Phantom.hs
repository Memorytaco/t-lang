module Language.Constraint.Relation.Phantom
   ( phtGuard
  , phtShrink
  , phtRaise
  )
where

import Control.Monad (unless)
import Graph.Core
import Graph.Data (removeEdge)
import Graph.Extension.GraphicType
import Language.Core (Name)
import Language.Generic.Subsume
import Language.Setting

type Setting m ns es info =
  ( HasOrderGraph ns es info
  , NodePht :<: ns
  , HasGraph ns es info m
  , T Sub :<: es
  , T (Binding Name) :<: es
  )

-- | make sure we have a correct phantom node structure to a phantom node.
phtGuard :: (Setting m ns es info) => Hole ns info -> m (Hole ns info)
phtGuard pht
  -- make sure it is a phantom node
  | isHoleOf @NodePht pht = do
    r <- getsGraph (lFrom @(T Sub) pht) >>= \case
      [snd -> r] -> return r
      _ -> error "NodePht node should have exactly one subnode but it is not"
    getsGraph (lFrom @(T (Binding Name)) r) >>= \case
      -- if the structure is ok, we return the phantom node
      -- otherwise we correct it and return right node
      [snd -> rbn] -> if rbn == pht then return pht else do
        -- remove structure edge: pht -> r
        modifyGraph $ removeEdge \e from to -> from == pht && to == r && isLinkOf @(T Sub) e
        -- replace pht with r, since the loop condition doesn't hold anymore
        modifyGraph $ fmap \n -> if n == pht then r else n
        return r
      _ -> error "node should have exactly one binding edge but it is not"
  -- simply return the node if it is not a phantom node
  | otherwise = return pht


-- | remove a phantom node to its origin node and this can be reverted back with `phtRaise`.
phtShrink :: (Setting m ns es info) => Hole ns info -> m (Hole ns info)
phtShrink pht
  -- make sure it is a phantom node
  | isHoleOf @NodePht pht = do
    r <- getsGraph (lFrom @(T Sub) pht) >>= \case
      [snd -> r] -> return r
      _ -> error "NodePht node should have exactly one subnode but it is not"
    getsGraph (lFrom @(T (Binding Name)) r) >>= \case
      [snd -> a] -> unless (a == pht) do error "NodePht's representation is not bound to itself"
      _ -> error "NodePht's representation is not bound to itself"
    -- remove structure edge: pht -> r
    modifyGraph $ removeEdge \e from to -> from == pht && to == r && isLinkOf @(T Sub) e
    -- replace pht with r, since the loop condition doesn't hold anymore
    modifyGraph $ fmap \n -> if n == pht then r else n
    return r
  -- simply return the node if it is not a phantom node
  | otherwise = return pht

-- | using existing phantom node to break binding loop if there is one.
--
-- if the loop doesn't exist, it is not raising.
phtRaise :: (Setting m ns es info) => Hole ns info -> Hole ns info -> m (Hole ns info)
phtRaise pht a
  -- simply return the node if it a phantom node
  | isHoleOf @NodePht a = return a
  -- make sure it is not a phantom node
  | otherwise = do
    bs <- getsGraph (fmap snd . lFrom @(T (Binding Name)) a)
    if a `elem` bs
      then do
        modifyGraph (<> (pht -<< T (Sub 1) >>- a))
        modifyGraph $ einduce \e from to ->
          if to == a then Just (e, from, pht) else Just (e, from, to)
        return pht
      else return a

