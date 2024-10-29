{- | play with graphic type and get information from data
-}

module Driver.Graph
  ( -- parseTypeGrpah
  )
where

import Language.Core (builtinStore, Name)

import Data.Text (Text)

import Graph.Core (induceLink, isLinkOf)
import Graph.Extension.GraphicType

import Compiler.SourceParsing (getGraphicType)

dumpGraphType :: Maybe Integer -> Text -> IO ()
dumpGraphType suffix typ = do
  ((root, g), _) <- getGraphicType mempty 0 builtinStore "stdin" typ
  let nameS = "graph-s" <> maybe "" show suffix <> ".dot"
      nameB = "graph-b" <> maybe "" show suffix <> ".dot"
  writeFile nameS $ exportViaShow root (induceLink (isLinkOf @(T Sub)) g)
  writeFile nameB $ exportViaShow root (induceLink (isLinkOf @(T (Binding Name))) g)

-- | TODO
exportViaShow :: a
exportViaShow = undefined

export_temp :: Monad m => m a
export_temp = do
  return undefined
