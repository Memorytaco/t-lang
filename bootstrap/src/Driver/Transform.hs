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

import Tlang.AST (Name, Label)
import Tlang.Generic ((:+:))
import Tlang.Graph.Extension.Type
import Tlang.Graph.Core

import Data.Text (Text)

type PlayG
  = CoreG (   T NodeBot :+: T (NodeLit Integer) :+: T (NodeLit Text)
          :+: T NodeTup :+: T NodeSum :+: T NodeRec :+: T (NodeRef Name)
          :+: T NodeApp :+: T (NodeHas Label)
          :+: NodePht
          )
          (T Sub :+: T (Bind Name)) Int

type UnifyG
  = CoreG (   T NodeBot :+: T (NodeLit Integer) :+: T (NodeLit Text)
          :+: T NodeTup :+: T NodeSum :+: T NodeRec :+: T (NodeRef Name)
          :+: T NodeApp :+: T (NodeHas Label)
          :+: NodePht :+: Histo :+: G
          )
          (T Sub :+: T (Bind Name) :+: Pht O) Int

