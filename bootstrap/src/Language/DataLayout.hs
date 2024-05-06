{- | * Language runtime and datalayout
--
-- This module defines everything we need to
-- wrap language program into a low a level
-- middle language, which is closing
-- to C programming language but is specialised to
-- data layout.
--
-- This acts like a translation layer between
-- abstraction code and real machine code. It aims
-- to handle some common translation idioms which
-- may repeat itself so many times that we decide
-- to put a marker to it so we can deal with
-- it in a uniform way.
-}

module Language.DataLayout
  ( -- ** runtime representation
    Rep (..)
  , _Rep

    -- ** re-export
  , module Class
  , module Primitive
  , module DataRep
  )
where

import Language.DataLayout.Class as Class
import Language.DataLayout.Primitive as Primitive
import Language.DataLayout.DataRep as DataRep

import Control.Lens
import Prettyprinter (Pretty)

-- | runtime representation of types in language.
--
-- Its semantic is basically interpreted by parameter `a` in `Rep a`.
--
-- e.g. If we take `a` as something like high level type
-- system (let's say T), then `Rep T` can mean "it is in the
-- middle of representing type system T". And `Rep T` can also
-- be part of `T`, where "runtime representation" is comming from.
--
-- If T is something real, like assembly language type and we can
-- say we are in the middle of translating representation to
-- real thing running on computer.
--
-- This type is originally designed to target at LLVM type
-- system, so it should not be more complex than a
-- direct mapping to translate it into real LLVM type.
newtype Rep a = Rep { __Rep :: DataRep (PrimitiveT SeqT) a}
  deriving newtype (Show, Eq, Ord, Functor, Pretty)

makeLenses ''Rep
