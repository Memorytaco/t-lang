module Tlang.Graph.Operation
  ( sOut, sIn
  , isStructureEdge, isBindEdge, isConstraintEdge
  , sEdgeOut, sEdgeIn, bEdgeOut, bEdgeIn
  , sReach
  , lab''
  )
where

import Tlang.Graph.Type
import Data.Graph.Inductive
import Capability
import Tlang.Generic ((:<:), prj)

-- | successor and presuccsor structure nodes of the node
sOut, sIn
  :: Gr (GNode (GNodeLabel lit label rep name)) (GEdge name) -> Node -> [(Node, Int)]
sOut g node = foldl (\ls (n, e) -> case e of GSub i -> (n, i):ls; _ -> ls) [] $ lsuc g node  -- all successor of the node
sIn g node = foldl (\ls (n, e) -> case e of GSub i -> (n, i):ls; _ -> ls) [] $ lpre g node  -- all presuccessor of the node

-- | return True if there is a path from n1 to n2
sReach
  :: Gr (GNode (GNodeLabel lit label rep name)) (GEdge name) -> Node -> Node -> Bool
sReach g n1 n2 = elem n2 . dfs [n1] $ elfilter isStructureEdge g

-- | out structure edge of a node
sEdgeOut, sEdgeIn, bEdgeOut, bEdgeIn :: Gr (GNode (GNodeLabel lit label rep name)) (GEdge name) -> Node -> [LEdge (GEdge name)]
sEdgeOut g node = filter (\(_,_,e) -> isStructureEdge e) $ out g node
sEdgeIn g node = filter (\(_,_,e) -> isStructureEdge e) $ inn g node
bEdgeOut g node = filter (\(_,_,e) -> isBindEdge e) $ out g node
bEdgeIn g node = filter (\(_,_,e) -> isBindEdge e) $ inn g node

isStructureEdge, isBindEdge, isConstraintEdge :: GEdge a -> Bool
isStructureEdge (GSub _) = True
isStructureEdge _ = False
isBindEdge (GBind {}) = True
isBindEdge _ = False
isConstraintEdge (GOperate _) = True
isConstraintEdge _ = False

lab'' :: Graph gr => gr a b -> Node -> a
lab'' g = lab' . context g
