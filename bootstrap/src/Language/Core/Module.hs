module Language.Core.Module
  (
  -- ** module definition

  -- | module itself is quite simple now, it contains a list of
  -- imports and declarations and module name. Nothing more and nothing
  -- less. query operation is implemented by `Decl` type class. So
  -- we can abstrace `Module` away from concrete `Decl` definition.
    Module (..)
  , moduleHeader
  , moduleImports
  , moduleDecls

  , ModuleName (..)
  , fuseModuleName

  -- | import statement
  , Use (..)
  )
where

import Language.Core.Decl
import Language.Core.Name (Name (..), Alias (..))
import Data.List (intersperse)
import Control.Lens

-- | a `ModuleName` is composed by multiple `Frag`.
-- Language takes a style of unix file path. Everything fits in
-- unix file path is supported by `ModuleName` though it is limited
-- in some language context.
data ModuleName = ModuleName [Name] Name deriving (Show, Eq, Ord)

-- | merge a `ModuleName` into `Name`
fuseModuleName :: ModuleName -> Name
fuseModuleName (ModuleName ls l) = mconcat $ intersperse "/" (ls <> [l])

data Module decls info
  = Module
    { _moduleHeader :: ModuleName       -- ^ Module name
    , _moduleImports :: [Use info]      -- ^ Module imports, including lexical items
    , _moduleDecls :: Decls decls info  -- ^ Module declarations
    } deriving (Show, Eq, Functor)

-- | A use statement to import symbol name.
-- Use (origin name, current name) [symbol list]
data Use a = Use (Alias ModuleName) [Alias a] deriving (Show, Eq, Functor)

makeLenses ''Module

