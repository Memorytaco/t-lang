module Tlang.AST.Module
  (
  -- ** module definition

  -- | module itself is quite simple now, it contains a list of
  -- imports and declarations and module name. Nothing more and nothing
  -- less. query operation is implemented by `Decl` type class. So
  -- we can abstrace `Module` away from concrete `Decl` definition.
    Module (..)
  , ModuleName (..)

  -- | import statement
  , Use (..)
  )
where

import Tlang.AST.Decl
import Tlang.AST.Name (Name (..), Alias (..))

-- | a `ModuleName` is composed by multiple `Frag`.
-- Language takes a style of unix file path. Everything fits in
-- unix file path is supported by `ModuleName` though it is limited
-- in some language context.
data ModuleName = ModuleName [Name] Name deriving (Show, Eq, Ord)

data Module decls info
  = Module
    { mmName :: ModuleName        -- ^ Module name
    , mmUses :: [Use info]        -- ^ Module imports, including lexical items
    , mmDecl :: Decls decls info  -- ^ Module declarations
    } deriving (Show, Functor)

-- | A use statement to import symbol name.
-- Use (origin name, current name) [symbol list]
data Use a = Use (Alias ModuleName) [Alias a] deriving (Show, Eq, Functor)

