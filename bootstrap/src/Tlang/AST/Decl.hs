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
  ( UserValue (..)
  , Attr
  )
where


-- | ** external symbol

-- | external defined symbol, its semantic depends
-- on its attributes and type signature.
data UserValue typ name attr
  = UserValue typ name attr
  deriving (Show, Eq)

-- | ** data constructor definition

-- | extensiable type family for data declaration
type family UserDataMark (mark :: * ) :: * -> * -> *

-- | coproduct variant, used in type family
data DataSum
-- | product variant, used in type family
data DataProduct
-- | Core definition for data definition
data UserData mk name typ sup
  = UserStruct name ((UserDataMark (mk DataProduct)) name typ)
  | UserEnum name ((UserDataMark (mk DataSum)) name typ)
  | UserData name sup

-- | data constructor used for AST
data UserDataAST k
-- | relevant type instance
type instance UserDataMark (UserDataAST DataSum) = DataEnum
type instance UserDataMark (UserDataAST DataProduct) = DataStruct

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

-- | customised attribute
data Attr

-- | All possible declaration geners
data DeclKind
  = DeclData  -- data definition
  | DeclLexi  -- lexical declaration
  | DeclType  -- type definition
  | DeclBind  -- value definition
  | DeclExtl  -- external value declaration
  deriving (Show, Eq)
