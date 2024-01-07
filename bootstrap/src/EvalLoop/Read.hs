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
import Data.Text as Text (Text, unpack, dropWhileEnd, dropWhile)
import Data.Char (isSpace)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char ( char, letterChar, spaceChar)
import Control.Lens
import Compiler.SourceParsing (getSurfaceExpr, getSurfaceDeclEof)
import Data.Functor (($>))

import Language.Core
  ( Name (..)
  , TypSurface, ExprSurface, DeclSurface
  )
import Language.Parser (reserved, reservedOp)

import EvalLoop.Store
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (MonadIO (..))

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
  | RListSource (Maybe Name)
    -- | show information of a module
  | RShowModule Name
    -- | declare something in repl
  | RDefineGlobal DeclSurface
    -- | query type of an expression, and display with or without sugar
    --
    -- True means add sugar
  | RQueryTypeOf Bool (ExprSurface TypSurface)
    -- | load a object file from a path, which will be used in jit compilation
  | RLoadObject FilePath
    -- | load a dynamic object file from a path
  | RLoadShared FilePath
    -- | load source file from path
  | RLoadSource FilePath
    -- | dump llvm bit code for an expression
  | RDumpBitcode (ExprSurface TypSurface)

data ReadResult
  = ReadExpr (ExprSurface TypSurface)
  | ReadCommand ReadCommand
  | ReadNothing

stripSpace :: Text -> Text
stripSpace = Text.dropWhile isSpace . Text.dropWhileEnd isSpace

-- | TODO: after pinning parser error, we delay error handling using SubParseError
interface :: forall m. MonadIO m => InterfaceT m ReadResult
interface = do
  command'maybe <- optional $ char ':' *> some letterChar <* many spaceChar
  ops <- asks (^. (evalStore . operators))
  let nextExpr cc = getInput >>= getSurfaceExpr ops "stdin" >>= cc
  case command'maybe of
    Just "def" ->
      getInput >>= getSurfaceDeclEof ops "stdin" >>= \case
        Left err -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        Right res -> return . ReadCommand $ RDefineGlobal res
    Just "load" -> getInput <&> ReadCommand . RLoadSource . unpack . stripSpace 
    Just "list" -> reserved "module" $> ReadCommand RListModules
               <|> reserved "source" $> ReadCommand (RListSource Nothing)
               <|> return (ReadCommand RListDecls)
    Just "source" -> getInput <&> Name . stripSpace >>= \case
      "" -> do
        liftIO $ putStrLn "expect a module name"
        return ReadNothing
      name -> return . ReadCommand $ RListSource (Just name)
    Just "showm" -> getInput <&> ReadCommand . RShowModule . Name . stripSpace
    Just "run" -> do undefined
    Just "type" -> do
      (isNothing -> withSugar) <- optional (reservedOp "!")
      nextExpr \case
        Left err -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        Right e -> return . ReadCommand $ RQueryTypeOf withSugar e
    Just "dump" ->
      getInput >>= getSurfaceExpr ops "stdin" >>= \case
        Left err -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        Right res -> return $ ReadCommand $ RDumpBitcode res
    Just cmd -> customFailure $ UnknownCommand cmd
    Nothing -> do
      resinput <- getInput
      case resinput of
        "" -> return ReadNothing
        _ -> getSurfaceExpr ops "stdin" resinput >>= \case
            Left err -> do
              liftIO . putStrLn $ errorBundlePretty err
              customFailure SubParseError
            Right res -> return $ ReadExpr res

driveInterface
  :: MonadIO m
  => EvalState -> Text -> m (Either (ParseErrorBundle Text ReadError) ReadResult)
driveInterface r txt = runReaderT (runParserT (runInterfaceT interface) "line" txt) r
