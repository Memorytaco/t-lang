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

import Interface.Config
import Tlang.AST (Decl, Symbol)
import Tlang.Parser (pratt, DeclareExtension, ParseExprType, WithExpr, ParseType)

import Driver.Parser

newtype ShellParser m a = ShellParser 
  { getShellParser :: ParsecT ShellError Text (RWST ShellConfig () ShellState m) a
  } deriving (Functor, Applicative, Monad, MonadState ShellState, MonadReader ShellConfig, MonadIO, MonadFail)
    deriving newtype (Alternative, MonadPlus)

deriving instance MonadParsec ShellError Text (ShellParser m)

data ShellError
  = UnknownCommand String
  | SubParseError
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ShellError where
  showErrorComponent = show

data ShellRes txt
  = LangDef (Decl (DeclareExtension ParseType) Symbol) txt
  | LangExpr ParseExprType txt
  | LangNone

toplevel :: forall m. MonadIO m => ShellParser m (ShellRes Text)
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
    Just cmd -> customFailure $ UnknownCommand cmd
    Nothing -> do
      resinput <- getInput
      case resinput of
        "" -> return LangNone
        _ -> do
          runParser ops (pratt @(WithExpr _) eof (-100)) "stdin" resinput >>= \case
            (Left err, _) -> do
              liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
              customFailure SubParseError
            (Right res, _) -> LangExpr res <$> getInput

runToplevel
  :: MonadIO m
  => ShellConfig
  -> ShellState
  -> Text
  -> m (Either (ParseErrorBundle Text ShellError) (ShellRes Text), ShellState, ())
runToplevel r s txt = runRWST (runParserT (getShellParser toplevel) "stdin" txt) r s
