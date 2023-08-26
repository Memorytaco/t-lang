module Driver.Transform
  (

  -- ** default definition for Graphic Type
    SurfaceG
  , SurfaceGNodes
  , SurfaceGEdges

  , UnifyGNodes
  , UnifyGEdges
  , UnifyG

  -- ** re-export subfunctionality
  , module GraphType
  , module TypeGraph
  )
where

import Driver.Transform.GraphType as GraphType
import Driver.Transform.TypeGraph as TypeGraph

import Language.Core (Name, Label)
import Language.Generic ((:+:))
import Graph.Extension.GraphicType
import Graph.Core

import Data.Text (Text)


type SurfaceGNodes
  = T NodeBot :+: T (NodeLit Integer) :+: T (NodeLit Text)
    :+: T NodeTup :+: T NodeSum :+: T NodeRec :+: T (NodeRef Name)
    :+: T NodeApp :+: T (NodeHas Label) :+: T NodeArr
    :+: NodePht
type SurfaceGEdges = T Sub :+: T (Binding Name)

type SurfaceG = CoreG SurfaceGNodes SurfaceGEdges Int

type UnifyGNodes
  = SurfaceGNodes
    :+: NDOrder
    :+: Histo :+: G

type UnifyGEdges
  = SurfaceGEdges
    :+: Pht O :+: Pht Sub :+: Pht NDOrderLink
    :+: T Unify :+: T Instance

type UnifyG = CoreG UnifyGNodes UnifyGEdges Int
