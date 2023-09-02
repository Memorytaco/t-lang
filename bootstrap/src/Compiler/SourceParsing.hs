{-# LANGUAGE RankNTypes #-}
{- | source parsing stage
--
-- this module provides functionality of parsing and also some common
-- utilities for dealing with raw surface language.
-}
module Compiler.SourceParsing
  (

  -- ** general utility
    prettyShowSurfaceModule

  -- ** parsing modules
  , loadModuleFromFile
  , loadModuleFromText

  -- ** parsing items from text
  , getSurfaceExpr
  , getSurfaceType
  , mapSurfaceDecl

  , getSurfaceDecl
  , getSurfaceDeclEof

  -- ** querying item
  , lookupSurfaceModule

  -- ** re-export builtin parser
  , driveParser -- ^ driver
  , surfaceExpr -- ^ expression
  , surfaceDecl -- ^ declaration
  , surfaceType -- ^ type
  )
where

import Language.Core ( ModuleSurface, TypSurface, ExprSurface, DeclSurface, OperatorStore, builtinStore, fuseModuleName, Module (..), Name, moduleHeader, Decls (getDecls))
import Compiler.Store ( HasCompilerStore, UseCompilerStore, stageSourceParsing, spSources, spFiles, AccessCompilerStore )
import Data.Text (Text)
import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Driver.Parser (driveParser, surfaceDecl, surfaceType, surfaceExpr, surfaceModule)

import Language.Parser.Lexer ( reservedOp )
import Text.Megaparsec (eof, ParseErrorBundle, MonadParsec (lookAhead), errorBundlePretty)
import Data.Void (Void)
import Data.Functor ( ($>) )
import Control.Lens ( (<&>), (%~), _2, (^..), (^.) )
import Capability.State ( modify, gets )
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Capability.Reader (asks)

import Control.Monad.Except (runExceptT, MonadTrans (..), MonadError (throwError))

-- ** general methods for surface module

-- | show basic information of a module
prettyShowSurfaceModule :: ModuleSurface -> String
prettyShowSurfaceModule (Module header impts decls) = intercalate "\n" (headerDoc <> imptsDoc <> declsDoc)
  where
    headerDoc = [show $ fuseModuleName header <> " :"]
    imptsDoc = "imports:":"":(impts <&> show)
    declsDoc = "definitions:":"":(getDecls decls <&> show)

-- ** actions for source file

loadModuleFromFile
  :: (HasCompilerStore m, MonadIO m) => FilePath -> m (Either String ModuleSurface)
loadModuleFromFile path = liftIO (Text.readFile path) >>= loadModuleFromText path
{-# INLINE loadModuleFromFile #-}

-- | load a module from a source file.
--
-- It updates compiler store.
--
-- TOOD: consider outdated files
loadModuleFromText
  :: (HasCompilerStore m) => String -> Text -> m (Either String ModuleSurface)
loadModuleFromText path content = runExceptT do
  sources <- lift $ gets @UseCompilerStore (^.. (stageSourceParsing . spSources . traverse . _2))
  (m'either, _) <- driveParser builtinStore (surfaceModule sources (reservedOp ";;" $> ()) eof) path content
  m <- case m'either of
    Right m -> return m
    Left err -> throwError $ errorBundlePretty err
  lift do
    modify @UseCompilerStore ((stageSourceParsing . spSources) %~ ((path, m):))
    modify @UseCompilerStore ((stageSourceParsing . spFiles) %~ Map.insert (fuseModuleName $ _moduleHeader m) content)
  return m

getSurfaceExpr
  :: Monad m
  => OperatorStore -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) (ExprSurface TypSurface))
getSurfaceExpr ops prompt content =
  driveParser ops (surfaceExpr eof) prompt content <&> fst

getSurfaceType :: Monad m
  => OperatorStore -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) TypSurface)
getSurfaceType ops prompt content =
  driveParser ops (surfaceType eof) prompt content
  <&> fst

-- | consume input till `end` and parse declaration.
getSurfaceDecl :: Monad m
  => OperatorStore -> (forall e f. MonadParsec e Text f => f ()) -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) DeclSurface, OperatorStore)
getSurfaceDecl ops end = driveParser ops (surfaceDecl (lookAhead end) <* end)

-- | map surfaceDecl parser to something else. It is recommended to use parser combinator
-- to get reasonable results.
mapSurfaceDecl :: Monad m
  => OperatorStore
  -> (forall e f. MonadParsec e Text f => f ())
  -> (forall e f. MonadParsec e Text f => f DeclSurface -> f res)
  -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) res, OperatorStore)
mapSurfaceDecl ops end f = driveParser ops (f $ surfaceDecl (lookAhead end) <* end)

-- | comsume whole inputs to parse declaration and abandon modified operator store.
getSurfaceDeclEof :: Monad m
  => OperatorStore -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) DeclSurface)
getSurfaceDeclEof ops prompt content = driveParser ops (surfaceDecl eof) prompt content <&> fst

-- ** methods available for store

-- | lookup module with its canonical name
lookupSurfaceModule :: AccessCompilerStore m => Name -> m (Maybe ModuleSurface)
lookupSurfaceModule name =
  asks @UseCompilerStore (^. (stageSourceParsing . spSources))
  <&> lookup name . (traverse %~ \(_, m) -> (fuseModuleName (m ^. moduleHeader), m))