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

-- ** extensions for data type definition

-- | Core definition for data, extended with declaration on type.
data UserData ext typ info = UserData info (ext typ) deriving (Show, Eq, Functor, Foldable, Traversable)

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
data UserCoerce field typ
  = UserCoerce typ
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | phantom data def
data UserPhantom typ
  = UserPhantom
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | type alias
data UserType typ info = UserType typ info deriving (Show, Eq, Functor, Foldable, Traversable)

-- ** extensions for FFI

-- *** external symbol

-- | external defined symbol, its semantic depends
-- on its attributes and type signature.
data UserFFI typ info
  = UserFFI FFItem typ info
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- *** attributes used to define external symbol

-- | customised attribute
data FFItem
  = FFItem String [FFItem]      -- ^ symbol value, with optional arguments
  | FFItemS String              -- ^ string value
  | FFItemI Integer             -- ^ Integer value
  | FFItemA [FFItem]            -- ^ sequence items, take a list form
  | FFItemR [(String, FFItem)]  -- ^ associated value, take a record form
  deriving (Show, Eq)

-- ** extensions for value definition

-- | user value definition
data UserValue typ term info
  = UserValue (term typ) info
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- ** extensions for parser rule (user defined operator for now)

-- | lexical item definition, definition of lexical operator (For Now, more to come in future)
-- TODO: figure out a way to manipulate lexemes and rules
data UserItem a
  = UserItem ItemSpace [Operator String] a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | lexical item namespace, to group lexical items together
newtype ItemSpace = NameSpace String deriving (Show, Eq)

