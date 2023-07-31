{-| * User interface module
-}

module EvalLoop.Loop
  ( repl'loop
  , shell
  )
where

import Data.Text (Text, pack)
import qualified Data.Text.IO as Text (putStrLn)
import System.Console.Haskeline
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Catch (MonadMask)
import Text.Megaparsec
import Control.Monad.State (MonadState (..), StateT (..), modify, gets)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor (($>))

import Language.Core
  ( query, Decls (..)
  , moduleHeader, moduleDecls, fuseModuleName
  )
import Language.Core.Extension.Decl (Item (..), UserOperator (..))

import qualified Data.Map as Map

import EvalLoop.Read
import EvalLoop.Config

import Control.Lens hiding (op)

data LoopControl state
  = LoopControl
    { _loopEnding :: Bool
    , _loopPrompt :: String
    , _loopState :: state
    } deriving (Show, Eq)

makeLenses ''LoopControl

shell :: IO ()
shell = do
  stat <- newEvalState
  let loop = runStateT repl'loop (LoopControl False "loop > " stat)
  runInputT defaultSettings loop $> ()

repl'loop :: (MonadIO m, MonadMask m) => StateT (LoopControl EvalState) (InputT m) ()
repl'loop = do
  lControl :: LoopControl EvalState <- get
  line'maybe <- lift $ getInputLine $ mconcat [show (lControl ^. loopState . linesNumber), " ", lControl ^. loopPrompt]
  case line'maybe of
    Nothing ->
      if lControl ^. loopEnding
      then lift $ outputStrLn "done."
      else do
        lift $ outputStrLn "Prese ^D again to exit"
        modify (loopEnding .~ True)
        repl'loop
    Just txt -> do
      pRes'either <- driveInterface (lControl ^. loopState) (pack txt)
      case pRes'either of
        Left bundle'err -> lift . outputStrLn $ errorBundlePretty bundle'err
        Right res -> do
          case res of
            ReadCommand cmd ->
              case cmd of
                RDefineGlobal decl -> do
                  case query @(Item (UserOperator Text)) (const True) decl of
                    Just (Item (UserOperator op) _) ->
                      modify $ loopState . evalStore . operators %~ (op:)
                    Nothing -> return ()
                  modify (loopState . evalStore . thisModule . moduleDecls %~ (Decls . (decl:) . getDecls))
                  lift . outputStrLn $ show decl
                RListSource (Just name) -> do
                  sourceStore <- gets (^. (loopState . evalStore . stageStore . parserStage . parsedFiles))
                  case Map.lookup name sourceStore of
                    Nothing -> lift $ outputStrLn $ "source for module " <> show name <> " is not available"
                    Just content -> liftIO $ Text.putStrLn content
                RListSource Nothing -> do
                  pairs <- gets (^. (loopState . evalStore . stageStore . parserStage . parsedSource))
                  let outputs = pairs & traverse %~ (\(path, m) -> show (fuseModuleName $ m ^. moduleHeader) <> ": " <> path)
                  lift $ forM_ outputs outputStrLn
                RListModules -> do
                  mods <- gets (^. (loopState . evalStore . stageStore . parserStage . parsedSource)) <&> fmap snd
                  liftIO $ forM_ (mods & traverse %~ (^. moduleHeader)) (putStrLn . show . fuseModuleName)
                RListDecls -> do
                  lift $ mapM_ (outputStrLn . show) $ getDecls $ lControl ^. loopState . evalStore . thisModule . moduleDecls
            ReadExpr e -> lift . outputStrLn $ show e
            ReadSource path content m -> do
              modify ( loopState . evalStore . stageStore . parserStage %~
                        (parsedFiles %~ Map.insert (fuseModuleName $ m ^. moduleHeader) content)
                      . (parsedSource %~ ((path, m):))
                     )
              lift . outputStrLn $ "load module " <> show (fuseModuleName $ m ^. moduleHeader) <> " from source \"" <> path <> "\""
            ReadNothing -> return ()
      modify (loopEnding .~ False)
      modify (loopState . linesNumber %~ (+1))
      repl'loop

