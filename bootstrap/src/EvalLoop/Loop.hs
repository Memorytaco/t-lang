{- | * 
-}

module EvalLoop.Loop
  ( evaDriver
  )
where


import System.Console.Haskeline

import EvalLoop.Handler.InputHandler

import Effectful
import Effectful.State.Dynamic
import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic (EffectHandler, interpret, localSeqUnlift)

import Data.Text (Text, pack, dropWhileEnd, stripPrefix, unpack)
import qualified Data.Text as Text (reverse)
import qualified Data.Text.IO as Text (putStrLn)
import Text.Megaparsec hiding (State)
import Control.Monad
import Data.Functor (($>))

import Language.Core.Decl
    ( DeclStore(..), isDeclOf )
import Language.Core.Module
    ( moduleHeader, moduleDecls, fuseModuleName )
import Language.Core.Extension.Decl (Item (..), UserOperator (..))

import qualified Data.Map as Map

import EvalLoop.Read
import EvalLoop.Store

import Control.Lens hiding (op)
import GHC.Generics (Generic)
import Compiler.SourceParsing
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
  let settings = Settings {historyFile=history'opt, complete=completer, autoAddHistory=True}
  stat <- newEvalState

  runEff . handleHaskeline settings . evalStateLocal (EvaControl False "eva> " stat) $ evaFeed

compilerStoreHandler :: State (EvaControl EvalState) :> es => EffectHandler (State StageStore) es
compilerStoreHandler env = \case
  Get -> get @(EvaControl EvalState) <&> (^. loopState . evalStore . stageStore)
  Put s -> modify (loopState . evalStore . stageStore .~ s)
  State update -> state \s ->
      let (a, val) = update $ s ^. loopState . evalStore . stageStore
       in (a, loopState . evalStore . stageStore .~ val $ s)
  StateM updateM -> stateM \s -> do
      (a, val) <- localSeqUnlift env \unlift -> unlift . updateM $ s ^. loopState . evalStore . stageStore
      return (a, loopState . evalStore . stageStore .~ val $ s)

accessCompilerStoreHandler :: State (EvaControl EvalState) :> es => EffectHandler (Reader StageStore) es
accessCompilerStoreHandler env = \case
  Ask -> get <&> (^. loopState . evalStore . stageStore)
  Local f m -> do
    val <- get
    modify (loopState . evalStore . stageStore %~ f)
    res <- localSeqUnlift env \unlift -> unlift m
    put val $> res

completer :: MonadIO m => CompletionFunc m
completer arg@(left, _right)
  | Text.reverse (dropWhileEnd isSpace (pack left)) == ":load" = completeFilename ("", "")
  | otherwise = completeWordWithPrev Nothing [' '] commands arg
  where
    commands _ prefix = return
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
          Just txt -> [Completion (unpack txt) h False]
          Nothing -> []
    _keywords :: [String] = ["let", "module", "data", "in", "use"]

evaFeed :: (IOE :> es, Haskeline :> es, State (EvaControl EvalState) :> es) => Eff es ()
evaFeed = interpret accessCompilerStoreHandler $ interpret compilerStoreHandler do
  lControl :: EvaControl EvalState <- get
  line'maybe <- do
    let prompt = mconcat [show (lControl ^. loopState . linesNumber), " ", lControl ^. loopPrompt]
    getUserInput prompt
  case line'maybe of
    Nothing ->
      if lControl ^. loopEnding
      then return ()
      else do
        putStrLine "Press ^D again to exit"
        modify (loopEnding .~ True)
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
                  case isDeclOf @(Item (UserOperator Text)) decl of
                    Just (Item (UserOperator op) _) ->
                      modify $ loopState . evalStore . operators %~ (op:)
                    Nothing -> return ()
                  modify (loopState . evalStore . thisModule . moduleDecls %~ (DeclStore . (decl:) . unDeclStore))
                  liftIO $ print decl
                RListSource (Just name) -> do
                  sourceStore <- gets (^. (loopState . evalStore . stageStore . stageSourceParsing . spFiles))
                  case Map.lookup name sourceStore of
                    Nothing -> liftIO . putStrLn $ "source for module " <> show name <> " is not available"
                    Just content -> liftIO $ Text.putStrLn content
                RListSource Nothing -> do
                  pairs <- gets (^. (loopState . evalStore . stageStore . stageSourceParsing . spSources))
                  let outputs = pairs & traverse %~ (\(path, m) ->
                        show (fuseModuleName $ m ^. moduleHeader) <> ": " <> path)
                  liftIO $ forM_ outputs print
                RListModules -> do
                  mods <- gets (fmap snd . (^. (loopState . evalStore . stageStore . stageSourceParsing . spSources)))
                  liftIO $ forM_ (mods & traverse %~ (^. moduleHeader)) (print . fuseModuleName)
                RListDecls -> liftIO do
                  mapM_ print $ unDeclStore $ lControl ^. loopState . evalStore . thisModule . moduleDecls
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
      modify (loopEnding .~ False)
      modify (loopState . linesNumber %~ (+1))
      evaFeed

