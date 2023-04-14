{-| * Language Declaration

  This module provides detailed definition
  of toplevel item.

  It may include:

  1. operator declaration
  2. type alias declaration
  3. data declaration
  4. toplevel value definition
  5. external value declaration

-}

module Tlang.AST.Decl
  ( UserDataMark
  , UserDataMarkFamily
  , UserDataAST
  , UserData (..)
  , DataEnum (..), DataStruct (..)
  , BoundVars (..)
  , TypNative (..)

  , UserForeign (..)
  , Attr (..)
  , AttrParam (..)

  , UserType (..)

  , LexiNameSpace (..)
  , UserLexi (..)

  , UserBind (..)

  , Decl (..)
  , DeclKind (..)
  )
where

import Tlang.AST.Operator
import Tlang.AST.Type (Bound)
import Tlang.Rep (PrimitiveT (..), SeqT (..))
import Data.Kind (Type)

-- | ** data constructor definition

-- | extensiable type family for data declaration
type family UserDataMarkFamily (mark :: UserDataMark a) :: Type -> Type -> Type

data UserDataMark a
  = DataSum a -- ^ coproduct variant, used in type family
  | DataProduct a -- ^ product variant, used in type family
  | DataSupply a -- ^ primitive external type definition
  | DataVar a -- ^ type variables of data declaration

-- | Core definition for data definition
data UserData mark name typ
  = UserStruct name
    ((UserDataMarkFamily ('DataVar mark)) name typ)
    ((UserDataMarkFamily ('DataProduct mark)) name typ)
  | UserEnum name
    ((UserDataMarkFamily ('DataVar mark)) name typ)
    ((UserDataMarkFamily ('DataSum mark)) name typ)
  | UserData name
    ((UserDataMarkFamily ('DataVar mark)) name typ)
    ((UserDataMarkFamily ('DataSupply mark)) name typ)

-- | data constructor used for AST
data UserDataAST
-- | relevant type instance
type instance UserDataMarkFamily ('DataSum UserDataAST) = DataEnum
type instance UserDataMarkFamily ('DataProduct UserDataAST) = DataStruct
type instance UserDataMarkFamily ('DataSupply UserDataAST) = TypNative
type instance UserDataMarkFamily ('DataVar UserDataAST) = BoundVars

newtype TypNative name typ = TypNative (PrimitiveT SeqT typ) deriving (Show, Eq)
newtype BoundVars name typ = BoundVars [Bound name typ]

-- | a default definition for use in AST parsing
data DataEnum name typ
  = DataEnum name
  | DataCons name typ
  | DataEnums [DataEnum name typ]
  deriving (Show, Eq)
-- | a default definition for use in AST parsing
data DataStruct name typ
  = DataStruct name typ
  | DataStructs [DataStruct name typ]
  deriving (Show, Eq)

-- | ** type definition
data UserType name typ
  = UserType name typ
  deriving (Show, Eq)

-- | ** external symbol

-- | external defined symbol, its semantic depends
-- on its attributes and type signature.
data UserForeign name typ
  = UserForeign Attr typ name
  deriving (Show, Eq)

-- | ** attribute denotation

-- | customised attribute
data Attr
  = Attr String [AttrParam]
  deriving (Show, Eq)
data AttrParam
  = AttrI Int -- Integer value
  | AttrF Float -- float value
  | AttrS String  -- string value
  | AttrA [AttrParam] -- list value
  | AttrR [(String, AttrParam)] -- record value
  deriving (Show, Eq)

-- | ** value definition

-- | user value definition
data UserBind name term
  = UserBind name term
  deriving (Show, Eq)

-- | ** lexical item definition

-- | lexical namespace, to group lexical items together
newtype LexiNameSpace = LexiNameSpace String deriving (Show, Eq)

-- | definition of lexical operator (For Now, more to come in future) TODO: figure out a way to manipulate lexemes and rules
data UserLexi
  = UserLexi LexiNameSpace [Operator String]
  deriving (Show, Eq)

-- | All possible declarations
data DeclKind
  = DeclData  -- data definition
  | DeclLexi  -- lexical declaration
  | DeclType  -- type definition
  | DeclBind  -- value definition
  | DeclFore  -- external value declaration
  deriving (Show, Eq)

data Decl mark name typ term
  = DData (UserData mark name typ)
  | DLexi UserLexi
  | DType (UserType name typ)
  | DBind (UserBind name term)
  | DFore (UserForeign name typ)
