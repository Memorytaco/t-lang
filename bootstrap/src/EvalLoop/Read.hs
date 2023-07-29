module EvalLoop.Read
 ( ShellParser (..)
 , ReadError (..)
 , ReadCommand (..)
 , ReadResult (..)
 , toplevel
 , runToplevel
 )
where

import Control.Monad
import Control.Applicative (Alternative)
import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Except
import Data.Text as Text (Text, unpack, dropWhileEnd, dropWhile)
import Data.Char (isSpace)
import qualified Data.Text.IO as Text
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.List (intercalate)

import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.Context (withContext)
import qualified Data.ByteString as B

import Language.Core (builtinStore, ModuleName (..), Module (..), Decls (..), TypSurface, ExprSurface, DeclSurface)
import Language.Parser (module', declaration, reserved)

import EvalLoop.Config
import Tlang.Emit (genExpr)

import EvalLoop.Util.Parser

import Driver.Parser
import Driver.CodeGen

newtype ShellParser m a = ShellParser
  { runShellParser :: ParsecT ReadError Text (RWST ShellConfig () (ShellState PredefDeclExtVal) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
    deriving newtype (MonadState (ShellState PredefDeclExtVal), MonadReader ShellConfig)
    deriving newtype (MonadPlus, Alternative, MonadParsec ReadError Text)

data ReadError
  = UnknownCommand String
  | SubParseError
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ReadError where
  showErrorComponent = show

data ReadCommand
    -- | load source code from a file
  = RLoadSource FilePath
    -- | list available modules
  | RListModules
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
  | ReadNothing

stripSpace :: Text -> Text
stripSpace = Text.dropWhile isSpace . Text.dropWhileEnd isSpace
ppModuleName :: ModuleName -> [Char]
ppModuleName (ModuleName ls l) = intercalate "/" (fmap show $ ls <> [l])

toplevel :: forall m. MonadIO m => ShellParser m ReadResult
toplevel = do
  command'maybe <- optional $ char ':' *> some letterChar <* many spaceChar
  ops <- gets operators
  mods <- gets modules
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
        (Right (def, _), _) -> do
          liftIO . putStrLn $ show def
          modify \s -> s { modules = def: modules s}
          return ReadNothing
    Just "mods" -> do
      liftIO $ forM_ (mmName <$> mods) \modName -> putStrLn $ ppModuleName modName
      return ReadNothing
    Just "showm" -> do
      modName <- unpack . stripSpace <$> getInput
      case lookup modName $ (\m -> (ppModuleName $ mmName m, m)) <$> mods of
        Just m -> liftIO do
          putStrLn $ "module> " <> modName <> ":"
          putStrLn "==== uses:"
          forM_ (mmUses m) \u -> putStrLn $ show u
          putStrLn "==== items:"
          forM_ (getDecls $ mmDecl m) \d -> putStrLn $ show d
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
          module' <- runModule "repl" emptyIRBuilder $ runCodeGen emptyState emptyData (withEntryTop . fmap snd $ genExpr res)
          liftIO $ withContext \ctx -> withModuleFromAST ctx module' \rmodule -> do
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

runToplevel
  :: MonadIO m
  => ShellConfig
  -> ShellState PredefDeclExtVal
  -> Text
  -> m ( Either (ParseErrorBundle Text ReadError) ReadResult
       , ShellState PredefDeclExtVal
       , ()
       )
runToplevel r s txt = runRWST (runParserT (runShellParser toplevel) "stdin" txt) r s
