module Tlang.Graph.Dot
  ( dotGraph
  , runDotGraph
  )
where

{- this module handles visualizing graphic type based on dot.
-}

import Tlang.AST
import Tlang.Graph.Type
import Tlang.Graph.Operation

import Data.Graph.Inductive
import Data.GraphViz hiding (DotGraph)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)
import Control.Monad.Reader
import Data.List (nub, sortBy)

runDotGraph :: (Show label, Show name, Labellable name, Ord name, Monad m)
            => Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)
            -> Node -> m (DotGraph Node)
runDotGraph g root = do
  mDot <- runReaderT (dotGraph root) g
  return $ digraph' mDot

dotGraph
  :: forall m lit label rep name
  . (Ord name, Show name, Labellable name, Show label, MonadReader (Gr (GNode (GNodeLabel lit label rep name)) (GEdge name)) m)
  => Node -> m (Dot Node)
dotGraph root = do
  nodes <- asks labNodes
  let noded = foldl (>>) mempty $ aNode <$> nodes
  structures <- recEdgeS root
  bindings <- recEdgeB root
  return do
    edgeAttrs [rank SameRank, ordering OutEdges]
    structures
    bindings
    noded
  where
    aNode (n, label) = node n [toLabel label]
    aEdge (from, to, label) = edge from to $
      case label of
        GBind perm _ _ ->
          case perm of
            Rigid -> [style dashed]
            Flexible -> [style dotted]
            Explicit -> [style dotted]
          <> [toLabel label]
        _ -> [toLabel label]
    recEdgeS :: Node -> m (Dot Node)
    recEdgeS r = do
      g <- ask
      let edges = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ sEdgeOut g r
          subs = nub $ (\(_, to, _) -> to) <$> edges
      if null subs
         then return mempty
         else do
           sNext <- mapM recEdgeS subs
           return do foldl (>>) mempty (aEdge <$> edges)
                     foldl (>>) mempty sNext
    recEdgeB :: Node -> m (Dot Node)
    recEdgeB r = do
      g <- ask
      let edges = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ sEdgeOut g r
          bedges = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ filter (\(_,_,e) -> not $ isStructureEdge e) $ inn g r
          subs = nub $ (\(_, to, _) -> to) <$> edges
      bNext <- if null edges
         then return mempty
         else foldl (>>) mempty <$> mapM recEdgeB subs
      return do foldl (>>) mempty (aEdge <$> bedges)
                bNext

