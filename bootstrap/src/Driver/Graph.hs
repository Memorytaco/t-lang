{- | play with graphic type and get information from data
-}

module Driver.Graph
  ( parseTypeGrpah
  )
where

import Driver.Parser ( driveParser, surfaceType )
import Text.Megaparsec
import Data.Void (Void)

import Language.Core ( builtinStore, Name)

import Data.Text (Text)
import Algebra.Graph.Export.Dot (exportViaShow)

import Driver.Transform
import Graph.Core (induceLink, isLinkOf)
import Graph.Extension.GraphicType

-- | save graph as png file
-- saveGraph :: (Show label, Show name, Labellable name, Ord name)
--           => (Node, Gr (GNode (GNodeLabel lit label name)) (GEdge name))
--           -> FilePath -> IO ()
-- saveGraph (root, g) name = do
--   dot <- runDotGraph g root
--   void $ runGraphviz dot Png (name <> ".png")
--   writeDotFile (name <> ".dot") dot

parseTypeGrpah :: Text -> IO SurfaceG
parseTypeGrpah typ = do
   (res, _) <- driveParser builtinStore (surfaceType eof) "stdin" typ
   case res of
     Right t -> do
        g <- toGraphicTypeMock mempty 0 t >>= \case
          Left err -> fail $ show err
          Right ((_, g :: SurfaceG), _) -> return g
        dumpTypegraph Nothing g
        return g
     Left (err :: ParseErrorBundle Text Void) -> putStrLn (errorBundlePretty err) >> fail "see previous message"


dumpTypegraph :: Maybe Integer -> SurfaceG -> IO ()
dumpTypegraph suffix g = do
  let nameS = "graph-s" <> maybe "" show suffix <> ".dot"
      nameB = "graph-b" <> maybe "" show suffix <> ".dot"
  writeFile nameS $ exportViaShow (induceLink (isLinkOf @(T Sub)) g)
  writeFile nameB $ exportViaShow (induceLink (isLinkOf @(T (Binding Name))) g)