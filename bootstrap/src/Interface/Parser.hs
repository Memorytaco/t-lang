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
import Text.Megaparsec
import Text.Megaparsec.Char

import Interface.Config
import Tlang.AST (Decl, Symbol)
import qualified Tlang.Parser.Decl as Decl
import qualified Tlang.Parser.Expr as Expr
import qualified Tlang.Parser.Type as Type

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
  = LangDef (Decl (Decl.DeclareExtension Type.ParseType) Symbol) txt
  | LangExpr Expr.ParseExprType txt
  | LangNone

toplevel :: MonadIO m => ShellParser m (ShellRes Text)
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
      resinput <- getInput
      case resinput of
        "" -> return LangNone
        _ -> do
          res'either <- Expr.play ops resinput
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
  -> m (Either (ParseErrorBundle Text ShellError) (ShellRes Text), ShellState, ())
runToplevel r s txt = runRWST (runParserT (getShellParser toplevel) "stdin" txt) r s
