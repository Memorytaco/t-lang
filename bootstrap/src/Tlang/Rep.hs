{- | reexport submodules
-}


module Tlang.Rep
  (
    -- ** runtime representation
    Rep (..)

    -- ** re-export
  , module Class
  , module Prim
  , module Concrete
  )
where

import Tlang.Rep.Class as Class
import Tlang.Rep.Primitive as Prim
import Tlang.Rep.Concrete as Concrete

newtype Rep a = Rep (DataRep (PrimitiveT SeqT) a)
  deriving newtype (Show, Eq, Ord, Functor)
