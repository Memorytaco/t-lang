module EvalLoop.Read
 ( InterfaceT (..)
 , ReadError (..)
 , ReadCommand (..)
 , ReadResult (..)
 , interface
 , driveInterface
 )
where

import Control.Monad
import Control.Applicative (Alternative)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Except
import Data.Text as Text (Text, unpack, dropWhileEnd, dropWhile)
import Data.Char (isSpace)
import qualified Data.Text.IO as Text
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.List (intercalate)
import Control.Lens

import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.Context (withContext)
import qualified Data.ByteString as B

import Language.Core
  ( Name (..), builtinStore, ModuleName (..), Decls (..)
  , TypSurface, ExprSurface, DeclSurface, ModuleSurface
  , moduleHeader, moduleImports, moduleDecls
  )
import Language.Parser (module', declaration, reserved)

import EvalLoop.Config
import Tlang.Emit (genExpr)

import EvalLoop.Util.Parser

import Driver.Parser
import Driver.CodeGen

newtype InterfaceT m a = InterfaceT
  { runInterfaceT :: ParsecT ReadError Text (ReaderT EvalState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
    deriving newtype (MonadReader EvalState)
    deriving newtype (MonadPlus, Alternative, MonadParsec ReadError Text)

data ReadError
  = UnknownCommand String
  | SubParseError
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ReadError where
  showErrorComponent = show

data ReadCommand
    -- | list available modules
  = RListModules
    -- | show defined decls in repl
  | RListDecls
    -- | list current loaded source file names
    -- or print out associated source file of a module
  | RListSources (Maybe Name)
    -- | show information of a module
  | RShowModule Text
    -- | declare something in repl
  | RDefineGlobal DeclSurface
    -- | load a object file from a path, which will be used in jit compilation
  | RLoadObject FilePath
    -- | load a dynamic object file from a path
  | RLoadShared FilePath

data ReadResult
  = ReadExpr (ExprSurface TypSurface)
  | ReadCommand ReadCommand
  -- | load source code from a file
  | ReadSource FilePath Text ModuleSurface
  | ReadNothing

stripSpace :: Text -> Text
stripSpace = Text.dropWhile isSpace . Text.dropWhileEnd isSpace
ppModuleName :: ModuleName -> [Char]
ppModuleName (ModuleName ls l) = intercalate "/" (fmap show $ ls <> [l])

interface :: forall m. MonadIO m => InterfaceT m ReadResult
interface = do
  command'maybe <- optional $ char ':' *> some letterChar <* many spaceChar
  ops <- asks (^. (evalStore . operators))
  mods :: [ModuleSurface] <- asks (^. (evalStore . stageStore . parserStage . parsedSource)) <&> fmap snd
  case command'maybe of
    Just "def" ->
      getInput >>= parseDecl ops eof "stdin" >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        (Right res, _) -> return . ReadCommand $ RDefineGlobal res
    Just "load" -> do
      file :: String <- unpack . stripSpace <$> getInput
      let parseFile = module' mods (declaration @(PredefDeclLang _) (void $ reserved ";;"))
      content <- liftIO $ Text.readFile file
      driveParser builtinStore parseFile file content >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        (Right (def, _), _) -> return $ ReadSource file content def
    Just "decls" -> return $ ReadCommand RListDecls
    Just "mods" -> return $ ReadCommand RListModules
    Just "showm" -> do
      modName <- unpack . stripSpace <$> getInput
      case lookup modName $ (\m -> (ppModuleName $ m ^. moduleHeader, m)) <$> mods of
        Just m -> liftIO do
          putStrLn $ "module> " <> modName <> ":"
          putStrLn "==== uses:"
          forM_ (m ^. moduleImports) $ putStrLn . show
          putStrLn "==== items:"
          forM_ (getDecls $ m ^. moduleDecls) $ putStrLn . show
          return ReadNothing
        Nothing -> do
          liftIO . putStrLn $ "Can't find module \"" <> modName <> "\""
          return ReadNothing
    Just "run" -> do undefined
    Just "gen" ->
      getInput >>= parseSurfaceExpr "stdin" ops >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
          customFailure SubParseError
        (Right res, _) -> do
          lmodule' <- runModule "repl" emptyIRBuilder $ runCodeGen emptyState emptyData (withEntryTop . fmap snd $ genExpr res)
          liftIO $ withContext \ctx -> withModuleFromAST ctx lmodule' \rmodule -> do
            moduleLLVMAssembly rmodule >>= B.putStr
          return ReadNothing
    Just cmd -> customFailure $ UnknownCommand cmd
    Nothing -> do
      resinput <- getInput
      case resinput of
        "" -> return ReadNothing
        _ -> parseSurfaceExpr "stdin" ops resinput >>= \case
            (Left err, _) -> do
              liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
              customFailure SubParseError
            (Right res, _) -> return $ ReadExpr res

driveInterface
  :: MonadIO m
  => EvalState -> Text -> m (Either (ParseErrorBundle Text ReadError) ReadResult)
driveInterface r txt = runReaderT (runParserT (runInterfaceT interface) "stdin" txt) r
