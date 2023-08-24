{- | play with graphic type and get information from data
-}

module Driver.Graph
  ( parseTypeGrpah
  )
where

import Language.Parser (pratt, Power (..))
import Driver.Parser (driveParser, TypeLang)
import Text.Megaparsec
import Data.Void (Void)

import Language.Core ( builtinStore, TypSurface )

import Data.Text (Text)
import Algebra.Graph.Export.Dot (exportViaShow)

import Driver.Transform

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
   res <- driveParser builtinStore (pratt @(TypeLang Void _) @TypSurface eof Go) "stdin" typ
   case res of
     (Right t, _) -> do
        ((_, g :: SurfaceG), _) <- runToGraph mempty 0 t
        writeFile "graph.dot" $ exportViaShow g
        return g
     (Left (err :: ParseErrorBundle Text Void), _) -> putStrLn (errorBundlePretty err) >> error "see previous message"
