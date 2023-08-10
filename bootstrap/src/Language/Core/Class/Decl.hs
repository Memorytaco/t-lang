{- | ** methods for interacting with AST structure
-}
module Language.Core.Class.Decl
  (
    -- ** Open methods for useful info from `Decl`
    Query (..)
  , DeclInfo (..)
  , queryAll
  )
where

import Language.Core.Decl
import Tlang.Generic ((:<:), prj)

-- ** plain method

-- | allow direct query of structure
class Query decl where
  query :: decl :<: decls => (info -> Bool) -> Decl decls info -> Maybe (decl info)

queryAll :: (Query decl, decl :<: decls) => (info -> Bool) -> Decls decls info -> [decl info]
queryAll p (Decls decls) = query p <$> decls >>= \case Just a -> [a]; Nothing -> []

-- | allow fetching inner information
class DeclInfo decl where
  getInfo :: decl info -> info

-- *** `Query` related
instance DeclInfo decl => Query decl where
  query f (Decl cls) = prj cls >>= \decl -> if f (getInfo decl) then Just decl else Nothing
