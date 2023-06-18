module Interface.Parser
 ( ShellParser (..)
 , ShellError (..)
 , ShellRes (..)
 , toplevel
 , runToplevel
 )
where

import Control.Monad
import Control.Applicative (Alternative)
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Except
import Data.Text (Text)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Data.Void (Void)

import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.Context (withContext)
import qualified Data.ByteString as B

import Interface.Config
import Tlang.Parser (runDSL)
import Tlang.Emit (genExpr)

import Driver.Parser
import Driver.CodeGen

newtype ShellParser m a = ShellParser
  { getShellParser :: ParsecT ShellError Text (RWST ShellConfig () ShellState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
    deriving newtype (MonadState ShellState, MonadReader ShellConfig)
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

toplevel :: forall m. MonadIO m => ShellParser m (ShellRes PredefDeclVal PredefExprVal Text)
toplevel = do
  command'maybe <- optional $ char ':' *> some letterChar <* many spaceChar
  ops <- gets operators
  case command'maybe of
    Just "def" ->
      getInput >>= parseDecl ops eof "stdin" >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        (Right res, _) -> LangDef res <$> getInput
    Just "gen" ->
      getInput >>= driveParser ops (runDSL @(PredefExprLang _) @PredefExprVal eof) "stdin" >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
          customFailure SubParseError
        (Right res, _) -> do
          module' <- runModule "repl" emptyIRBuilder $ runCodeGen emptyState emptyData (withEntryTop . fmap snd $ genExpr res)
          liftIO $ withContext \ctx -> withModuleFromAST ctx module' \rmodule -> do
            moduleLLVMAssembly rmodule >>= B.putStr
          return LangNone
          -- LangDef res <$> getInput
    Just cmd -> customFailure $ UnknownCommand cmd
    Nothing -> do
      resinput <- getInput
      case resinput of
        "" -> return LangNone
        _ -> do
          driveParser ops (runDSL @(PredefExprLang _) @PredefExprVal eof) "stdin" resinput >>= \case
            (Left err, _) -> do
              liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
              customFailure SubParseError
            (Right res, _) -> LangExpr res <$> getInput

runToplevel
  :: MonadIO m
  => ShellConfig
  -> ShellState
  -> Text
  -> m (Either (ParseErrorBundle Text ShellError) (ShellRes PredefDeclVal PredefExprVal Text), ShellState, ())
runToplevel r s txt = runRWST (runParserT (getShellParser toplevel) "stdin" txt) r s
