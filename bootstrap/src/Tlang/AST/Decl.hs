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

module Tlang.AST.Decl
  (
  -- ** Core structure for module declaration
    Decl (..)
  , Decls (..)

  -- ** Methods for interacting with `Decl` directly
  , declare
  , declOf

  -- ** Open methods for useful info from `Decl`
  , Query (..)
  , HasInfo (..)
  )
where

import Tlang.Generic ((:<:) (..))

-- ** Core declaration structure

-- | including data definition and operations
data Decl decls info = Decl (decls info) deriving (Show, Functor, Foldable, Traversable)
newtype Decls decls info = Decls [Decl decls info] deriving (Show, Functor, Foldable, Traversable)

-- ** plain method

-- | allow direct query of structure
class Query decl where
  query :: decl :<: decls => (info -> Bool) -> Decl decls info -> Maybe (decl info)
  queryAll :: decl :<: decls => (info -> Bool) -> Decls decls info -> [decl info]
  queryAll info (Decls decls) =
    case sequence $ query info <$> decls of
      Just ls -> ls
      Nothing -> []

-- | allow fetching inner information
class HasInfo decl where
  getInfo :: decl info -> info

-- | declare a structure
declare :: decl :<: decls => decl info -> Decl decls info
declare = Decl . inj
-- | get inner structure
declOf :: forall decl decls info. decl :<: decls => Decl decls info -> Maybe (decl info)
declOf (Decl a) = prj a
