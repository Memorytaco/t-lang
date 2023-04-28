{-
  * extension for declaration

  all main extensions will prefixed
  by `User`, but it could contain
  sub extensions also.
-}

module Tlang.Extension.Decl
  (
  -- ** user data type definition
    UserData (..)
  , UserEnum (..)
  , UserStruct (..)
  , UserCoerce (..)
  , UserPhantom (..)
  , injData

  -- ** type alias definition
  , UserType (..)

  -- ** foreign function interface
  , UserFFI (..)
  , FFItem (..)

  -- ** value binding
  , UserValue (..)

  -- ** parser item, to modify language syntax, like operator precedence
  , UserItem (..)
  , ItemSpace (..)
  )
where

import Tlang.AST.Operator
import Tlang.Generic ((:<:) (..))
import Tlang.AST.Class.Decl
import Tlang.AST.Decl

-- ** extensions for data type definition

-- | Core definition for data, extended with declaration on type.
data UserData vars ext typ info = UserData vars info (ext typ) deriving (Show, Eq, Functor, Foldable, Traversable)

injData :: ext :<: exts => vars -> info -> ext typ -> UserData vars exts typ info
injData vars info = UserData vars info . inj

-- | a default definition for use in AST parsing
data UserEnum field typ
  = UserEnum field [typ]
  | UserEnums (UserEnum field typ) [UserEnum field typ]
  deriving (Show, Eq, Functor, Foldable, Traversable)
-- | a default definition for use in AST parsing
data UserStruct field typ
  = UserStruct field typ
  | UserStructs (UserStruct field typ) [UserStruct field typ]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | coercible definition of data type
data UserCoerce typ
  = UserCoerce typ
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | phantom data def
data UserPhantom typ
  = UserPhantom
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | type alias
data UserType typ vars info = UserType typ vars info deriving (Show, Eq, Functor, Foldable, Traversable)

-- ** extensions for FFI

-- *** external symbol

-- | external defined symbol, its semantic depends
-- on its attributes and type signature.
data UserFFI typ info
  = UserFFI (Maybe FFItem) typ info
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- *** attributes used to define external symbol

-- | customised attribute
data FFItem
  = FFItemF String [FFItem]     -- ^ custom value, with optional arguments
  | FFItemS String              -- ^ string value
  | FFItemI Integer             -- ^ Integer value
  | FFItemA [FFItem]            -- ^ sequence items, take a list form
  | FFItemR [(String, FFItem)]  -- ^ associated value, take a record form
  deriving (Show, Eq)

-- ** extensions for value definition

-- | user value definition
data UserValue val typ info
  = UserValue val typ info
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- ** extensions for parser rule (user defined operator for now)

-- | lexical item definition, definition of lexical operator (For Now, more to come in future)
-- TODO: figure out a way to manipulate lexemes and rules
data UserItem a
  = UserItem ItemSpace [Operator String] a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | lexical item namespace, to group lexical items together
newtype ItemSpace = ItemSpace String deriving (Show, Eq)


-- ** Definition of `Decl` related type class instance

-- *** `DeclInfo` related
instance DeclInfo (UserData vars ext typ) where
  getInfo (UserData _ info _) = info
instance DeclInfo (UserType typ vars) where
  getInfo (UserType _ _ info) = info
instance DeclInfo (UserFFI typ) where
  getInfo (UserFFI _ _ info) = info
instance DeclInfo (UserValue val typ) where
  getInfo (UserValue _ _ info) = info
instance DeclInfo UserItem where
  getInfo (UserItem _ _ info) = info

-- *** `Query` related
instance DeclInfo decl => Query decl where
  query f (Decl cls) = prj cls >>= \decl -> if f (getInfo decl) then Just decl else Nothing
