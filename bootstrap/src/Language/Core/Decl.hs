{-| * Language Declaration

  This module provides detailed definition
  of toplevel item.

  It may include:

  1. operator declaration
  2. type alias
  3. data type
  4. value definition
  5. FFI definition

-}

module Language.Core.Decl
  (
  -- ** Core structure for module declaration
    Decl (..)
  , Decls (..)

  -- ** Methods for interacting with `Decl` directly
  , declare
  , declOf
  )
where

import Tlang.Generic ((:<:), inj, prj)

-- ** Core declaration structure

-- | including data definition and operations
newtype Decl decls info = Decl { getDecl :: decls info } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype Decls decls info = Decls { getDecls :: [Decl decls info] } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | declare a structure
declare :: decl :<: decls => decl info -> Decl decls info
declare = Decl . inj
{-# INLINE declare #-}

-- | get inner structure
declOf :: forall decl decls info. decl :<: decls
       => Decl decls info -> Maybe (decl info)
declOf = prj @decl . getDecl
{-# INLINE declOf #-}
