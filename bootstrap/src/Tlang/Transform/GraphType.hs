{- | * Convert graphic representation of type back to its syntactic representation
-}

module Tlang.Transform.GraphType

where

import Tlang.AST
import Tlang.Graph.Core

import Capability.Reader (HasReader)
import Data.Functor.Identity (Identity)

import qualified Data.Kind as Data (Type, Constraint)

type GraphTypeConstrain :: (Data.Type -> Data.Type) -> (Data.Type -> Data.Type)
                        -> (Data.Type -> Data.Type) -> (Data.Type -> Data.Type) -> Data.Type
                        -> (Data.Type -> Data.Type) -> (Data.Type -> Data.Type) -> Data.Type -> Data.Type
                        -> Data.Constraint
type family GraphTypeConstrain node m nodes edges info bind rep name a

class UnfoldGraphType node info | node -> info where
  unfoldGraphType :: (HasReader "graph" (CoreG nodes edges info) m, GraphTypeConstrain node m nodes edges info bind rep name a)
                  => (Hole nodes info -> m (Type bind rep name a))
                  -> node (Hole nodes info) -> info -> m (Type bind rep name a)

-- toSyntacticType 



type instance GraphTypeConstrain Identity m nodes edges info bind rep name a = ()
instance UnfoldGraphType Identity Int where
  unfoldGraphType restore _ _ = do
    undefined

