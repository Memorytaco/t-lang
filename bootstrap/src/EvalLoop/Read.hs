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
import Text.Megaparsec.Char ( char, letterChar, spaceChar )
import Data.Void (Void)
import Control.Lens

import Data.Functor (($>))

import Language.Core
  ( Name (..), builtinStore, Decls (..)
  , TypSurface, ExprSurface, DeclSurface, ModuleSurface
  , moduleHeader, moduleImports, moduleDecls, fuseModuleName
  )
import Language.Parser (reserved)

import EvalLoop.Store
import EvalLoop.Util.Parser

import Driver.Parser

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
  | RShowModule Text
    -- | declare something in repl
  | RDefineGlobal DeclSurface
    -- | load a object file from a path, which will be used in jit compilation
  | RLoadObject FilePath
    -- | load a dynamic object file from a path
  | RLoadShared FilePath
    -- | dump llvm bit code for an expression
  | RDumpBitcode (ExprSurface TypSurface)

data ReadResult
  = ReadExpr (ExprSurface TypSurface)
  | ReadCommand ReadCommand
  -- | load source code from a file
  | ReadSource FilePath Text ModuleSurface
  | ReadNothing

stripSpace :: Text -> Text
stripSpace = Text.dropWhile isSpace . Text.dropWhileEnd isSpace

-- | TODO: after fixing parser error, we delay error handling using SubParseError
interface :: forall m. MonadIO m => InterfaceT m ReadResult
interface = do
  command'maybe <- optional $ char ':' *> some letterChar <* many spaceChar
  ops <- asks (^. (evalStore . operators))
  mods :: [ModuleSurface] <- asks (^. (evalStore . stageStore . stageSourceParsing . spSources)) <&> fmap snd
  case command'maybe of
    Just "def" ->
      getInput >>= driveParser ops (surfaceDecl eof) "stdin" >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        (Right res, _) -> return . ReadCommand $ RDefineGlobal res
    Just "load" -> do
      file :: String <- unpack . stripSpace <$> getInput
      let parseFile = surfaceModule mods (void $ reserved ";;") eof
      content <- liftIO $ Text.readFile file
      driveParser builtinStore parseFile file content >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty err
          customFailure SubParseError
        (Right def, _) -> return $ ReadSource file content def
    Just "list" -> reserved "module" $> ReadCommand RListModules
               <|> reserved "source" $> ReadCommand (RListSource Nothing)
               <|> return (ReadCommand RListDecls)
    Just "source" -> do
      name <- Name . stripSpace <$> getInput
      if name == Name ""
         then do liftIO $ putStrLn "expect a module name"
                 return ReadNothing
         else  return . ReadCommand $ RListSource (Just name)
    Just "showm" -> do
      name <- Name . stripSpace <$> getInput
      case lookup name $ (\m -> (fuseModuleName $ m ^. moduleHeader, m)) <$> mods of
        Just m -> liftIO do
          putStrLn $ "module> " <> show name <> ":"
          putStrLn "==== uses:"
          forM_ (m ^. moduleImports) print
          putStrLn "==== items:"
          forM_ (getDecls $ m ^. moduleDecls) print
          return ReadNothing
        Nothing -> do
          liftIO . putStrLn $ "Can't find module \"" <> show name <> "\""
          return ReadNothing
    Just "run" -> do undefined
    Just "dump" ->
      getInput >>= parseSurfaceExpr "stdin" ops >>= \case
        (Left err, _) -> do
          liftIO . putStrLn $ errorBundlePretty (err :: ParseErrorBundle Text Void)
          customFailure SubParseError
        (Right res, _) -> return $ ReadCommand $ RDumpBitcode res
          -- lmodule' <- runModule "repl" emptyIRBuilder $ runCodeGen emptyState emptyData (withEntryTop . fmap snd $ genExpr res)
          -- liftIO $ withContext \ctx -> withModuleFromAST ctx lmodule' \rmodule -> do
          --   moduleLLVMAssembly rmodule >>= B.putStr
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
