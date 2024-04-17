{- | reexport submodules
-}


module Language.DataLayout
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

import Language.DataLayout.Class as Class
import Language.DataLayout.Primitive as Prim
import Language.DataLayout.DataRep as DataRep

import Control.Lens
import Prettyprinter (Pretty)

newtype Rep a = Rep { __Rep :: DataRep (PrimitiveT SeqT) a}
  deriving newtype (Show, Eq, Ord, Functor, Pretty)

makeLenses ''Rep
