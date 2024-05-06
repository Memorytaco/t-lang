{-
  * extension for declaration

  all main extensions will prefixed
  by `User`, but it could contain
  sub extensions also.
-}

module Language.Core.Extension.Decl
  (
  -- ** user data type definition
    DataType (..)
  , DataEnum (..)
  , DataStruct (..)
  , DataBody (..)
  , DataNone (..)

  -- ** type alias definition
  , AliasType (..)

  -- ** foreign function interface
  , FFI (..)

  -- ** value binding
  , UserValue (..)

  -- ** parser item, to modify language syntax, like operator precedence
  , UserOperator (..)

  -- ** a container for holding things
  , Item (..)
  , DataPrefix (..)

  -- ** re-export
  , Identity (..)
  )
where

import Language.Core.Operator
import Language.Core.Macro
import Language.Core.Constraint (Prefixes (..))
import Language.Core.Class.Decl

import Data.Functor.Identity (Identity (..))
import Data.Bifunctor (first)

-- | boring container
data Item item a = Item item a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype DataPrefix typ name
  = DataPrefix (Prefixes name typ)
  deriving Show via Prefixes name typ
instance Functor (DataPrefix typ) where
  fmap f (DataPrefix v) = DataPrefix (first f v)

-- ** extensions for data type definition

-- | for sum type
data DataEnum field typ
  = DataEnum
    { getDataEnumHead :: (field, [typ])
    , getDataEnumTail :: [(field, [typ])]
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | for record type
data DataStruct field typ
  = DataStruct
    { getDataStructHead :: (field, typ)
    , getDataStructTail :: [(field, typ)]
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Core definition for data
data DataType env xt typ name
  = DataType
    { getDataTypeEnv :: env typ name
    , getDataTypeArity :: Integer
    , getDataType :: xt typ
    , getDataTypeName :: name
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

data DataNone a = DataNone deriving (Show, Eq, Functor, Foldable, Traversable)

newtype DataBody x typ = DataBody (x typ) deriving (Show, Eq, Functor, Foldable, Traversable)

-- | type alias
data AliasType env typ name
  = AliasType
    { getAliasTypeEnv :: env typ name
    , getAliasTypeArity :: Integer
    , getAliasType :: typ
    , getAliasTypeName :: name
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

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
instance DeclInfo (UserValue val typ) where
  getInfo (UserValue _ _ info) = info
instance DeclInfo (Item a) where
  getInfo (Item _ info) = info
