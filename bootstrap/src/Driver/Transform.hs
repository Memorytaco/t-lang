module Driver.Transform
  (

  -- ** default definition for Graphic Type
    PlayG
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

type PlayG
  = CoreG (   T NodeBot :+: T (NodeLit Integer) :+: T (NodeLit Text)
          :+: T NodeTup :+: T NodeSum :+: T NodeRec :+: T (NodeRef Name)
          :+: T NodeApp :+: T (NodeHas Label)
          :+: NodePht
          )
          (T Sub :+: T (Binding Name)) Int

type UnifyG
  = CoreG (   T NodeBot :+: T (NodeLit Integer) :+: T (NodeLit Text)
          :+: T NodeTup :+: T NodeSum :+: T NodeRec :+: T (NodeRef Name)
          :+: T NodeApp :+: T (NodeHas Label)
          :+: NodePht :+: Histo :+: G
          )
          (T Sub :+: T (Binding Name) :+: Pht O) Int

