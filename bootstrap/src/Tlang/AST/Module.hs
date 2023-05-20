module Tlang.AST.Module
  (

  -- ** module definition

  -- | module itself is quite simple now, it contains a list of
  -- imports and declarations and module name. Nothing more and nothing
  -- less. query operation is implemented by `Decl` type class. So
  -- we can abstrace `Module` away from concrete `Decl` definition.
    Module (..)
  , ModuleName (..)
  , Frag (..)

  -- | import statement
  , Use (..)

  -- | name alias
  , NameAlias (..)
  )
where

import Tlang.AST.Decl

-- | module path fragment
newtype Frag
  = Frag String
  deriving (Show, Eq, Ord)

-- | a `ModuleName` is composed by multiple `Frag`.
-- Language takes a style of unix file path. Everything fits in
-- unix file path is supported by `ModuleName` though it is limited
-- in some language context.
data ModuleName = ModuleName [Frag] Frag deriving (Show, Eq, Ord)

data Module decls info
  = Module
    { mmName :: ModuleName        -- ^ Module name
    , mmUses :: [Use info]        -- ^ Module imports, including lexical items
    , mmDecl :: Decls decls info  -- ^ Module declarations
    } deriving (Show, Functor)

data NameAlias name
  = NameAlias name (Maybe name)
  deriving (Eq, Functor)

instance Show name => Show (NameAlias name) where
  show (NameAlias name (Just alias)) = show name <> " as " <> show alias
  show (NameAlias name Nothing) = show name

-- | A use statement to import symbol name.
-- Use (origin name, current name) [symbol list]
data Use info = Use (NameAlias ModuleName) [NameAlias info] deriving (Show, Eq, Functor)

