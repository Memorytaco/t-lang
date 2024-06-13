{- | * 
-}

module EvalLoop.Loop
  ( evaDriver
  )
where


import System.Console.Isocline

import Data.Text (Text, pack, dropWhileEnd, stripPrefix, unpack)
import qualified Data.Text.IO as Text (putStrLn)
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Text.Megaparsec
import Control.Monad
import Control.Monad.State (StateT (..))
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
import Data.Char (isSpace)

-- | evaluation control. store runtime information for repl
data EvaControl state
  = EvaControl
    { _loopEnding :: Bool
    , _loopPrompt :: String
    , _loopState :: state
    } deriving (Show, Eq, Generic)

makeLenses ''EvaControl

type History = String

evaDriver :: Maybe History -> IO ()
evaDriver history'opt = do
  case history'opt of
    Just history -> setHistory history 200
    Nothing -> return ()
  styleDef "ic-prompt" "ansi-maroon"
  void $ enableAutoTab True

  stat <- newEvalState
  driveEva (EvaControl False "eva" stat) evaFeed $> ()

data UserEva
type EvaT m = StateT (EvaControl EvalState) m
type UseStageStoreField'UseLoop m =
  Rename "_stageStore" (Field "_stageStore" "_evalStore" (Field "_evalStore" "_loopState" (Field "_loopState" UserEva m)))

newtype Eva m a = Eva
  { emitEva :: EvaT m a
  } deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadMask, MonadFail)
    deriving (HasSource UserEva (EvaControl EvalState), HasSink UserEva (EvaControl EvalState))
        via MonadState (EvaT m)
    deriving (HasState UserEva (EvaControl EvalState))
        via MonadState (EvaT m)
    deriving (HasReader UserEva (EvaControl EvalState))
        via ReadState (Eva m)
    deriving (HasSource UseCompilerStore StageStore, HasSink UseCompilerStore StageStore)
        via (UseStageStoreField'UseLoop (Eva m))
    deriving (HasState UseCompilerStore StageStore)
        via (UseStageStoreField'UseLoop (Eva m))
    deriving (HasReader UseCompilerStore StageStore)
        via (UseStageStoreField'UseLoop (Eva m))

driveEva :: EvaControl EvalState -> Eva m a -> m (a, EvaControl EvalState)
driveEva s m = runStateT (emitEva m) s

completer :: CompletionEnv -> String -> IO ()
completer env input
  | dropWhileEnd isSpace (pack input) == ":load" = completeFileName env input Nothing [".", ".."] []
  | otherwise = addCompletions env (commands input) >> wordCompleter keywords env input
  where
    commands prefix =
      let tp = pack prefix in
      [ (":def", "top level definition")
      , (":load", "load a source file")
      , (":list", "list current items")
      , (":source", "list attached source file of a module")
      , (":showm", "show a module")
      , (":run", "run an expression")
      , (":type", "show type of an expression")
      , (":dump", "dump internal representation")] >>= \(w, h) ->
        case stripPrefix tp (pack w) of
          Just txt -> [Completion (unpack txt) w h]
          Nothing -> []
    keywords = ["let", "module", "data", "in", "use"]

styler :: String -> Fmt
styler = undefined

evaFeed :: (MonadIO m, MonadMask m, MonadFail m) => Eva m ()
evaFeed = do
  lControl :: EvaControl EvalState <- get @UserEva
  line'maybe <- liftIO do
    let prompt = mconcat [show (lControl ^. loopState . linesNumber), " ", lControl ^. loopPrompt]
    readlineExMaybe prompt (Just completer) Nothing
  case line'maybe of
    Nothing ->
      if lControl ^. loopEnding
      then return ()
      else do
        liftIO $ putStrLn "Press ^D again to exit"
        modify @UserEva (loopEnding .~ True)
        evaFeed
    Just txt -> do
      pRes'either <- driveInterface (lControl ^. loopState) (pack txt)
      case pRes'either of
        Left bundle'err -> liftIO $ print $ errorBundlePretty bundle'err
        Right res -> do
          case res of
            ReadCommand cmd ->
              case cmd of
                RDefineGlobal decl -> do
                  case query @(Item (UserOperator Text)) (const True) decl of
                    Just (Item (UserOperator op) _) ->
                      modify @UserEva $ loopState . evalStore . operators %~ (op:)
                    Nothing -> return ()
                  modify @UserEva (loopState . evalStore . thisModule . moduleDecls %~ (Decls . (decl:) . getDecls))
                  liftIO $ print decl
                RListSource (Just name) -> do
                  sourceStore <- gets @UserEva (^. (loopState . evalStore . stageStore . stageSourceParsing . spFiles))
                  case Map.lookup name sourceStore of
                    Nothing -> liftIO . putStrLn $ "source for module " <> show name <> " is not available"
                    Just content -> liftIO $ Text.putStrLn content
                RListSource Nothing -> do
                  pairs <- gets @UserEva (^. (loopState . evalStore . stageStore . stageSourceParsing . spSources))
                  let outputs = pairs & traverse %~ (\(path, m) ->
                        show (fuseModuleName $ m ^. moduleHeader) <> ": " <> path)
                  liftIO $ forM_ outputs print
                RListModules -> do
                  mods <- gets @UserEva (^. (loopState . evalStore . stageStore . stageSourceParsing . spSources)) <&> fmap snd
                  liftIO $ forM_ (mods & traverse %~ (^. moduleHeader)) (print . fuseModuleName)
                RListDecls -> liftIO do
                  mapM_ print $ getDecls $ lControl ^. loopState . evalStore . thisModule . moduleDecls
                RLoadSource path -> loadModuleFromFile path >>= \case
                  Right m -> liftIO . putStrLn $ "load module "<> show (fuseModuleName $ m ^. moduleHeader) <> " from source \"" <> path <> "\""
                  Left s -> liftIO $ putStrLn s
                RShowModule name -> lookupSurfaceModule name >>= liftIO . \case
                  Just m ->  putStrLn $ prettyShowSurfaceModule m
                  Nothing -> putStrLn $ "Can't not find module: '" <> show name <> "'"
                RQueryTypeOf withSugar e ->
                  tcExprToSyntacticType [] 0 ("a", 0) e >>= \case
                    Left err -> liftIO $ print err
                    Right (t, _) -> liftIO do print $ pretty t
                RLoadObject _ -> undefined
                RLoadShared _ -> undefined
                RDumpBitcode e -> do
                  -- TODO: fix genExpr
                  m <- runWithDefaultMain "repl" undefined -- (genExpr e <&> snd)
                  liftIO $ withContext \ctx -> withModuleFromAST ctx m (ByteString.putStr <=< moduleLLVMAssembly)
            ReadExpr e -> liftIO . print $ pretty e
            ReadNothing -> return ()
      modify @UserEva (loopEnding .~ False)
      modify @UserEva (loopState . linesNumber %~ (+1))
      evaFeed

