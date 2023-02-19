module Tlang.Inference.Graph

where

data GraphRel
  = EqualRel
  | GraftRel
  | MergeRel
  | RaiseRel
  | WeakRel
  deriving (Ord, Show, Eq)

class GraphType t where
  instof :: GraphRel -> t -> t -> Bool

-- | label for edge
data GEdge
-- | label for node
data GNode

data G
