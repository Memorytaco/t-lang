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
import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Except
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import Interface.Config
import Tlang.AST (None)
import qualified Tlang.Parser.Decl as Decl
import qualified Tlang.Parser.Expr as Expr

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

data ShellRes txt c f
  = LangDef (Decl.ParseDeclType c f) txt
  | LangExpr (Expr.ParseExprType c f) txt

toplevel :: MonadIO m => ShellParser m (ShellRes Text None Identity)
toplevel = do
  command'maybe <- optional $ char ':' *> some letterChar <* many spaceChar
  ops <- gets operators
  case command'maybe of
    Just "def" -> do
      res'either <- getInput >>= Decl.play id ops 
      case res'either of
        Left err -> do
          liftIO $ putStrLn err
          customFailure SubParseError
        Right res -> LangDef res <$> getInput
    Just cmd -> customFailure $ UnknownCommand cmd
    Nothing -> do
      res'either <- getInput >>= Expr.play ops
      case res'either of
        Left err -> do
          liftIO $ putStrLn err
          customFailure SubParseError
        Right res -> LangExpr res <$> getInput

runToplevel
  :: MonadIO m
  => ShellConfig
  -> ShellState
  -> Text
  -> m (Either (ParseErrorBundle Text ShellError) (ShellRes Text None Identity), ShellState, ())
runToplevel r s txt = runRWST (runParserT (getShellParser toplevel) "stdin" txt) r s
