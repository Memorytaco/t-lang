module EvalLoop.Parser
 ( ShellParser (..)
 , ShellError (..)
 , ShellRes (..)
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

import Language.Core (builtinStore, ModuleName (..), Module (..), Decls (..), TypSurface, ExprSurface)
import Language.Parser (module', declaration, reserved)

import EvalLoop.Config
import Tlang.Emit (genExpr)

import EvalLoop.Util.ExprParser

import Driver.Parser
import Driver.CodeGen

newtype ShellParser m a = ShellParser
  { getShellParser :: ParsecT ShellError Text (RWST ShellConfig () (ShellState PredefDeclExtVal) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
    deriving newtype (MonadState (ShellState PredefDeclExtVal), MonadReader ShellConfig)
    deriving newtype (MonadPlus, Alternative, MonadParsec ShellError Text)

data ShellError
  = UnknownCommand String
  | SubParseError
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ShellError where
  showErrorComponent = show

data ShellRes decl expr txt
  = LangDef decl txt
  | LangExpr expr txt
  | LangNone

stripSpace :: Text -> Text
stripSpace = Text.dropWhile isSpace . Text.dropWhileEnd isSpace
ppModuleName :: ModuleName -> [Char]
ppModuleName (ModuleName ls l) = intercalate "/" (fmap show $ ls <> [l])

toplevel :: forall m. MonadIO m => ShellParser m (ShellRes PredefDeclVal (ExprSurface TypSurface) Text)
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
        (Right res, _) -> LangDef res <$> getInput
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
          return LangNone
    Just "mods" -> do
      liftIO $ forM_ (mmName <$> mods) \modName -> putStrLn $ ppModuleName modName
      return LangNone
    Just "showm" -> do
      modName <- unpack . stripSpace <$> getInput
      case lookup modName $ (\m -> (ppModuleName $ mmName m, m)) <$> mods of
        Just m -> liftIO do
          putStrLn $ "module> " <> modName <> ":"
          putStrLn "==== uses:"
          forM_ (mmUses m) \u -> putStrLn $ show u
          putStrLn "==== items:"
          forM_ (getDecls $ mmDecl m) \d -> putStrLn $ show d
          return LangNone
        Nothing -> do
          liftIO . putStrLn $ "Can't find module \"" <> modName <> "\""
          return LangNone
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
          return LangNone
    Just cmd -> customFailure $ UnknownCommand cmd
    Nothing -> do
      resinput <- getInput
      case resinput of
        "" -> return LangNone
        _ -> parseSurfaceExpr "stdin" ops resinput >>= \case
            (Left err, _) -> do
              liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
              customFailure SubParseError
            (Right res, _) -> LangExpr res <$> getInput

runToplevel
  :: MonadIO m
  => ShellConfig
  -> ShellState PredefDeclExtVal
  -> Text
  -> m ( Either (ParseErrorBundle Text ShellError) (ShellRes PredefDeclVal (ExprSurface TypSurface) Text)
       , ShellState PredefDeclExtVal
       , ()
       )
runToplevel r s txt = runRWST (runParserT (getShellParser toplevel) "stdin" txt) r s
