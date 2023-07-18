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
  , UserDataDef (..)
  , UserEnum (..)
  , UserStruct (..)
  , UserCoerce (..)
  , UserPhantom (..)

  -- ** type alias definition
  , UserType (..)

  -- ** foreign function interface
  , FFI (..)

  -- ** value binding
  , UserValue (..)

  -- ** parser item, to modify language syntax, like operator precedence
  , UserOperator (..)

  -- ** a container for holding things
  , Item (..)
  )
where

import Tlang.AST.Operator
import Tlang.AST.Attribute
import Tlang.AST.Class.Decl


-- | boring container
data Item item a = Item item a
  deriving (Show, Eq, Ord, Functor)

-- ** extensions for data type definition

-- | Core definition for data, extended with declaration on type.
data UserData vars def info = UserData info vars def deriving (Show, Eq, Functor, Foldable, Traversable)
newtype UserDataDef ext a = UserDataDef (ext a) deriving (Show, Eq, Functor, Foldable, Traversable)

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

-- | external FFI symbol is defined by a name and a type.
--
-- It interpretation or semantic is determined by its attributes.
data FFI typ name = FFI Attr typ name
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- *** attributes used to define external symbol

-- ** extensions for value definition

-- | user value definition
data UserValue val typ info
  = UserValue val typ info
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- ** extensions for parser rule (user defined operator for now)

-- | User defined operator, with name as operator name
data UserOperator name
  = UserOperator (OperatorSpace (Operator name))
  deriving (Show, Eq, Functor)

-- ** Definition of `Decl` related type class instance

-- *** `DeclInfo` related
instance DeclInfo (UserData vars def) where
  getInfo (UserData info _ _) = info
instance DeclInfo (UserType typ vars) where
  getInfo (UserType _ _ info) = info
instance DeclInfo (UserValue val typ) where
  getInfo (UserValue _ _ info) = info
instance DeclInfo (Item a) where
  getInfo (Item _ info) = info
