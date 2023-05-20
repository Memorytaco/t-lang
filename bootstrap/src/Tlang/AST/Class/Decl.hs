module Tlang.AST.Class.Decl
  (
  -- ** Open methods for useful info from `Decl`
    Query (..)
  , DeclInfo (..)
  )
where

import Tlang.AST.Decl
import Tlang.Generic ((:<:))
import Control.Monad (mapM)
import Data.Maybe (fromMaybe)

-- ** plain method

-- | allow direct query of structure
class Query decl where
  query :: decl :<: decls => (info -> Bool) -> Decl decls info -> Maybe (decl info)
  queryAll :: decl :<: decls => (info -> Bool) -> Decls decls info -> [decl info]
  queryAll info (Decls decls) = fromMaybe [] $ mapM (query info) decls

-- | allow fetching inner information
class DeclInfo decl where
  getInfo :: decl info -> info

