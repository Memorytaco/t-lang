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
  , DeclStore (..)

  -- ** Methods for interacting with `Decl` directly
  , declare
  , isDeclOf
  , isDeclStoreOf
  )
where

import Language.Generic ((:<:), inj, prj)
import Data.Maybe (catMaybes)

-- ** Core declaration structure

-- | including data definition and operations
newtype Decl i a =
  Decl
    { unDecl :: i a
    } deriving (Show, Eq, Functor, Foldable, Traversable)

newtype DeclStore i a =
  DeclStore
    { unDeclStore :: [Decl i a]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | declare a structure
declare :: ix :<: set => ix a -> Decl set a
declare = Decl . inj
{-# INLINE declare #-}

-- | ask if the item has correct index type and return if so, otherwise
-- return Nothing.
isDeclOf :: forall ix set a. ix :<: set => Decl set a -> Maybe (ix a)
isDeclOf = prj @ix . unDecl
{-# INLINE isDeclOf #-}

isDeclStoreOf :: forall ix set a. ix :<: set => DeclStore set a -> [ix a]
isDeclStoreOf store = catMaybes (isDeclOf @ix <$> unDeclStore store)
