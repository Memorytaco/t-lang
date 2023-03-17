module Driver.Graph
  ( play
  , saveGraph
  , viewType
  , testUnify
  )
where

{- play with graphic type and get information from data
-}

import qualified Tlang.Parser.Type as PT
import Tlang.AST
import Tlang.Inference.Graph
import Tlang.Inference.Graph.Dot

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.GraphViz
import Data.Graph.Inductive
import Data.GraphViz.Commands.IO (writeDotFile)

-- | save graph as png file
saveGraph :: (Show label, Show name, Labellable name, Ord name)
          => (Node, Gr (GNode (GNodeLabel lit label rep name)) (GEdge name))
          -> FilePath -> IO ()
saveGraph (root, g) name = do
  dot <- runDotGraph g root
  void $ runGraphviz dot Png (name <> ".png")
  writeDotFile (name <> ".dot") dot

-- | a way to visualize graphic type
viewType :: FilePath -> Text -> IO ()
viewType path str = do
  res <- PT.play typOperator str
  case res of
    Right t -> do
       (root, g) <- runToGraph [] empty t
       saveGraph (root, g) path
       ((), ng) <- runStateT (augGraph root) g
       saveGraph (root, ng) (path <> ".aug")
       (tn, _) <- runRestore root g ([], 0)
       Text.putStrLn str
       putStrLn "original: "
       print t
       putStrLn "Restored: "
       print tn
       prettyPrint g
    Left err -> putStrLn err

-- | a temporary function used to play unify in ghci REPL
testUnify :: FilePath -> Text -> IO ()
testUnify path str = do
  res <- PT.play typOperator str
  case res of
    Right t -> do
       (root, g) <- runToGraph [] empty t
       saveGraph (root, g) path
       putStrLn "the graph:"
       prettyPrint g
       n1 <- getLine >>= return . read @Int
       n2 <- getLine >>= return . read @Int
       ((), ng) <- runStateT (augGraph root) g
       (_, (simplify root -> gu, _)) <- runStateT (n1 ~=~ n2) (ng,[])
       saveGraph (root, ng) (path <> ".aug")
       saveGraph (root, simplify root gu) (path <> ".gu")
       (tn, _) <- runRestore root gu ([], 0)
       Text.putStrLn str
       putStrLn "original: "
       print t
       putStrLn "Restored: "
       print tn
    Left err -> putStrLn err

play :: Text -> IO ()
play str = do
  res <- PT.play typOperator str
  case res of
    Right t -> do
       (root, g) <- runToGraph [] empty t
       (tn, _) <- runRestore root g ([], 0)
       Text.putStrLn str
       putStrLn "original: "
       print t
       putStrLn "Restored: "
       print tn
    Left err -> putStrLn err

