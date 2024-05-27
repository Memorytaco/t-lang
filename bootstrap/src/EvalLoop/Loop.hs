{- | * 
-}

module EvalLoop.Loop
  ( repl'loop
  , shell
  , UseLoop
  )
where

import Data.Text (Text, pack)
import qualified Data.Text.IO as Text (putStrLn)
import System.Console.Haskeline
import Control.Monad (forM_, (<=<))
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Text.Megaparsec
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor (($>))

import Language.Core
  ( query, Decls (..)
  , moduleHeader, moduleDecls, fuseModuleName
  )
import Language.Core.Extension.Decl (Item (..), UserOperator (..))

import qualified Data.Map as Map

import EvalLoop.Read
import EvalLoop.Store

import Control.Lens hiding (op)
import Capability.Source (HasSource, MonadState (..), Field (..), Rename (..))
import Capability.Sink (HasSink)
import Capability.State ( get, modify, gets, HasState )
import Control.Monad.IO.Class ( MonadIO(..) )
import GHC.Generics (Generic)
import Compiler.SourceParsing
import Capability.Reader (HasReader, ReadState (..))
import LLVM.Context (withContext)
import LLVM (withModuleFromAST, moduleLLVMAssembly)
import qualified Data.ByteString as ByteString
import Compiler.CodeGen (genExpr)
import Driver.Compiler.CodeGen.LLVM (runWithDefaultMain)
import Compiler.TypeChecking (tcExprToSyntacticType)
import Transform.Desugar (pruneForallType)
import Prettyprinter (pretty)

-- | store runtime information for repl
data LoopControl state
  = LoopControl
    { _loopEnding :: Bool
    , _loopPrompt :: String
    , _loopState :: state
    } deriving (Show, Eq, Generic)

makeLenses ''LoopControl

shell :: IO ()
shell = do
  stat <- newEvalState
  let loop = driveLoopT (LoopControl False "loop > " stat) repl'loop
  runInputT defaultSettings loop $> ()

data UseLoop
type LoopT' m = StateT (LoopControl EvalState) (InputT m)
type UseStageStoreField'UseLoop m =
  Rename "_stageStore" (Field "_stageStore" "_evalStore" (Field "_evalStore" "_loopState" (Field "_loopState" UseLoop m)))

newtype LoopT m a
  = LoopT
  { runLoopT :: LoopT' m a
  } deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadMask, MonadFail)
    deriving (HasSource UseLoop (LoopControl EvalState), HasSink UseLoop (LoopControl EvalState))
        via MonadState (LoopT' m)
    deriving (HasState UseLoop (LoopControl EvalState))
        via MonadState (LoopT' m)
    deriving (HasReader UseLoop (LoopControl EvalState))
        via ReadState (LoopT m)
    deriving (HasSource UseCompilerStore StageStore, HasSink UseCompilerStore StageStore)
        via (UseStageStoreField'UseLoop (LoopT m))
    deriving (HasState UseCompilerStore StageStore)
        via (UseStageStoreField'UseLoop (LoopT m))
    deriving (HasReader UseCompilerStore StageStore)
        via (UseStageStoreField'UseLoop (LoopT m))


driveLoopT :: LoopControl EvalState -> LoopT m a -> InputT m (a, LoopControl EvalState)
driveLoopT s m = runStateT (runLoopT m) s

-- | allow operation in `InputT` be executed in `LoopT`
liftInput :: Monad m => InputT m a -> LoopT m a
liftInput m = LoopT $ lift m

repl'loop :: (MonadIO m, MonadMask m, MonadFail m) => LoopT m ()
repl'loop = do
  lControl :: LoopControl EvalState <- get @UseLoop
  line'maybe <- liftInput $ getInputLine $ mconcat [show (lControl ^. loopState . linesNumber), " ", lControl ^. loopPrompt]
  case line'maybe of
    Nothing ->
      if lControl ^. loopEnding
      then liftInput $ outputStrLn "done."
      else do
        liftInput $ outputStrLn "Prese ^D again to exit"
        modify @UseLoop (loopEnding .~ True)
        repl'loop
    Just txt -> do
      pRes'either <- driveInterface (lControl ^. loopState) (pack txt)
      case pRes'either of
        Left bundle'err -> liftInput . outputStrLn $ errorBundlePretty bundle'err
        Right res -> do
          case res of
            ReadCommand cmd ->
              case cmd of
                RDefineGlobal decl -> do
                  case query @(Item (UserOperator Text)) (const True) decl of
                    Just (Item (UserOperator op) _) ->
                      modify @UseLoop $ loopState . evalStore . operators %~ (op:)
                    Nothing -> return ()
                  modify @UseLoop (loopState . evalStore . thisModule . moduleDecls %~ (Decls . (decl:) . getDecls))
                  liftInput . outputStrLn $ show decl
                RListSource (Just name) -> do
                  sourceStore <- gets @UseLoop (^. (loopState . evalStore . stageStore . stageSourceParsing . spFiles))
                  case Map.lookup name sourceStore of
                    Nothing -> liftInput $ outputStrLn $ "source for module " <> show name <> " is not available"
                    Just content -> liftIO $ Text.putStrLn content
                RListSource Nothing -> do
                  pairs <- gets @UseLoop (^. (loopState . evalStore . stageStore . stageSourceParsing . spSources))
                  let outputs = pairs & traverse %~ (\(path, m) -> show (fuseModuleName $ m ^. moduleHeader) <> ": " <> path)
                  liftInput $ forM_ outputs outputStrLn
                RListModules -> do
                  mods <- gets @UseLoop (^. (loopState . evalStore . stageStore . stageSourceParsing . spSources)) <&> fmap snd
                  liftIO $ forM_ (mods & traverse %~ (^. moduleHeader)) (print . fuseModuleName)
                RListDecls -> do
                  liftInput $ mapM_ (outputStrLn . show) $ getDecls $ lControl ^. loopState . evalStore . thisModule . moduleDecls
                RLoadSource path -> loadModuleFromFile path >>= \case
                  Right m -> liftInput $ outputStrLn $ "load module "<> show (fuseModuleName $ m ^. moduleHeader) <> " from source \"" <> path <> "\""
                  Left s -> liftInput $ outputStrLn s
                RShowModule name -> lookupSurfaceModule name >>= liftInput . \case
                  Just m ->  outputStrLn $ prettyShowSurfaceModule m
                  Nothing -> outputStrLn $ "Can't not find module: '" <> show name <> "'"
                RQueryTypeOf withSugar e ->
                  tcExprToSyntacticType [] 0 ("a", 0) e >>= \case
                    Left err -> liftInput $ outputStrLn $ show err
                    Right (t, _) -> liftInput do outputStrLn $ show $ pretty t
                RLoadObject _ -> undefined
                RLoadShared _ -> undefined
                RDumpBitcode e -> do
                  -- TODO: fix genExpr
                  m <- runWithDefaultMain "repl" undefined -- (genExpr e <&> snd)
                  liftIO $ withContext \ctx -> withModuleFromAST ctx m (ByteString.putStr <=< moduleLLVMAssembly)
            ReadExpr e -> liftInput $ outputStrLn $ show $ pretty e
            ReadNothing -> return ()
      modify @UseLoop (loopEnding .~ False)
      modify @UseLoop (loopState . linesNumber %~ (+1))
      repl'loop

