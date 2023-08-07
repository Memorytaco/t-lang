{- | reexport submodules
-}


module Tlang.Rep
  (
    -- ** runtime representation
    Rep (..)
  , _Rep

    -- ** re-export
  , module Class
  , module Prim
  , module DataRep
  )
where

import Tlang.Rep.Class as Class
import Tlang.Rep.Primitive as Prim
import Tlang.Rep.DataRep as DataRep

import Control.Lens

newtype Rep a = Rep { __Rep :: DataRep (PrimitiveT SeqT) a}
  deriving newtype (Show, Eq, Ord, Functor)

makeLenses ''Rep
