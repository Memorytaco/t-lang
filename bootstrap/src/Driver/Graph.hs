{- | play with graphic type and get information from data
-}

module Driver.Graph
  (
    saveGraph
  , parseTypeGrpah
  )
where

import Tlang.Parser (WithType, pratt)
import Driver.Parser (runParser)
import Text.Megaparsec hiding (runParser)
import Data.Void (Void)

import Tlang.AST
import Tlang.Inference.Graph
import Tlang.Graph.Dot

import Control.Monad.State
import Data.Text (Text)
import Data.GraphViz
import Data.Graph.Inductive
import Data.GraphViz.Commands.IO (writeDotFile)

import Algebra.Graph.Export.Dot (exportViaShow)

import Driver.Transform

-- | save graph as png file
saveGraph :: (Show label, Show name, Labellable name, Ord name)
          => (Node, Gr (GNode (GNodeLabel lit label rep name)) (GEdge name))
          -> FilePath -> IO ()
saveGraph (root, g) name = do
  dot <- runDotGraph g root
  void $ runGraphviz dot Png (name <> ".png")
  writeDotFile (name <> ".dot") dot

parseTypeGrpah :: Text -> IO PlayGP
parseTypeGrpah typ = do
   res <- runParser (typOperator, []) (pratt @(WithType _) eof (-100)) "stdin" typ
   case res of
     (Right t, _) -> do
        ((_, g :: PlayGP), _) <- runToGraph 0 t
        writeFile "graph.dot" $ exportViaShow g
        return g
     (Left (err :: ParseErrorBundle Text Void), _) -> putStrLn (errorBundlePretty err) >> error "see previous message"
