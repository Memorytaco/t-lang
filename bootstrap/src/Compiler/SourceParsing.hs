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
  -- *** parsing language type and expression
  , getSurfaceExpr
  , getSurfaceType
  , mapSurfaceDecl

  -- *** parsing declaration item
  , getSurfaceDecl
  , getSurfaceDeclEof

  -- *** parsing graph type
  , getGraphicType 

  -- ** querying item
  , lookupSurfaceModule

  -- *** item conversion
  , querySyntacticType
  , queryGraphicType

  -- ** re-export builtin parser
  , driveParser -- ^ driver
  , surfaceExpr -- ^ expression
  , surfaceDecl -- ^ declaration
  , surfaceType -- ^ type
  )
where

import Language.Core
  ( ModuleSurface, TypSurface, ExprSurface, DeclSurface
  , OperatorStore, builtinStore, fuseModuleName, Module (..), Name, msName, DeclStore (unDeclStore)
  , GraphicNodesSurface, GraphicEdgesSurface, GraphicTypeSurface, ConstraintEdgesSurface, ConstraintNodesSurface
  , CoreStage (..)
  )
import Compiler.Store ( HasCompilerStore, stageSourceParsing, spSources, spFiles, AccessCompilerStore )
import Data.Text (Text)
import Data.List (intercalate)
import Driver.Parser (driveParser, surfaceDecl, surfaceType, surfaceExpr, surfaceModule)

import Effectful
import Effectful.State.Dynamic
import Effectful.Reader.Dynamic
import Effectful.Error.Dynamic

import Language.Parser.Lexer ( reservedOp )
import Text.Megaparsec (eof, ParseErrorBundle, MonadParsec (lookAhead), errorBundlePretty)
import Data.Void (Void)
import Data.Functor ( ($>) )
import Control.Lens ( (<&>), (%~), _2, (^..), (^.) )
import qualified Data.Map as Map
import qualified Data.Text.IO as Text

import Driver.Transform.TypeGraph (toGraphicTypeMock, ToGraphicTypeErr)
import Transform.TypeGraph (TypeContextTable)
import Graph.Core (Hole, CoreG)
import Driver.Transform.GraphType (runGraphType, syntacticType)
import Transform.GraphType (GraphToTypeErr)

-- ** general methods for surface module

-- | show basic information of a module
prettyShowSurfaceModule :: ModuleSurface SParsing -> String
prettyShowSurfaceModule (ModuleSource header impts decls path) = intercalate "\n" (headerDoc <> imptsDoc <> declsDoc)
  where
    headerDoc = [show $ fuseModuleName header <> " :"]
    imptsDoc = "imports:":"":(impts <&> show)
    declsDoc = "definitions:":"":(unDeclStore decls <&> show)

-- ** actions for source file

loadModuleFromFile
  :: (HasCompilerStore m, IOE :> m) => FilePath -> Eff m (Either String (ModuleSurface SParsing))
loadModuleFromFile path = liftIO (Text.readFile path) >>= loadModuleFromText path
{-# INLINE loadModuleFromFile #-}

-- | load a module from a source file.
--
-- It updates compiler store.
--
-- TOOD: consider outdated files
loadModuleFromText
  :: (HasCompilerStore m) => String -> Text -> Eff m (Either String (ModuleSurface SParsing))
loadModuleFromText path content = runErrorNoCallStack do
  sources <- gets (^.. (stageSourceParsing . spSources . traverse . _2))
  (m'either, _) <- driveParser builtinStore (surfaceModule sources (reservedOp ";;" $> ()) eof) path content
  m <- case m'either of
    Right m -> return m
    Left err -> throwError $ errorBundlePretty err
  modify ((stageSourceParsing . spSources) %~ ((path, m):))
  modify ((stageSourceParsing . spFiles) %~ Map.insert (fuseModuleName $ m ^. msName) content)
  return m

-- | try fetching expression ast from text.
getSurfaceExpr
  :: Monad m
  => OperatorStore -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) (ExprSurface TypSurface))
getSurfaceExpr ops prompt content =
  driveParser ops (surfaceExpr eof) prompt content <&> fst

-- | try fetching type ast from text.
getSurfaceType :: Monad m
  => OperatorStore -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) TypSurface)
getSurfaceType ops prompt content =
  driveParser ops (surfaceType eof) prompt content
  <&> fst

-- | FIXME: refine this interface
getGraphicType
  :: MonadFail m
  => TypeContextTable Name GraphicNodesSurface GraphicEdgesSurface Int -> Int
  -> OperatorStore -> String -> Text -> m ((Hole GraphicNodesSurface Int, GraphicTypeSurface), Int)
getGraphicType env counter ops prompt content = getSurfaceType ops prompt content >>= \case
  -- TODO: replace mock typing environment
  Right typ -> toGraphicTypeMock env counter typ >>= \case
    Right res -> return res
    Left err -> fail $ show err
  Left bundle -> fail $ errorBundlePretty bundle

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
lookupSurfaceModule :: AccessCompilerStore m => Name -> Eff m (Maybe (ModuleSurface SParsing))
lookupSurfaceModule name =
  asks (^. (stageSourceParsing . spSources))
  <&> lookup name . (traverse %~ \(_, m) -> (fuseModuleName (m ^. msName), m))

-- *** syntactic type and graphic type conversion

-- | convert graphic type into syntactic type
querySyntacticType
  :: MonadFail m
  => (Hole ConstraintNodesSurface Int, CoreG ConstraintNodesSurface ConstraintEdgesSurface Int)
  -> [(Hole ConstraintNodesSurface Int, Name)] -> (String, Int)
  -> m (TypSurface, (String, Int))
querySyntacticType (r, gr) context hint = runGraphType gr context hint syntacticType r >>= \case
  Left err -> fail $ show err
  Right a -> return a

-- | convert syntactic type into graphic type.
--
-- TODO: Add real environment
queryGraphicType :: (MonadFail m, nodes ~ ConstraintNodesSurface, edges ~ ConstraintEdgesSurface)
                 => TypeContextTable Name nodes edges Int -> Int -> TypSurface
                 -> m ((Hole nodes Int, CoreG nodes edges Int), Int)
queryGraphicType ctx seed typ = toGraphicTypeMock ctx seed typ >>= \case
  Left err -> fail $ show err
  Right a -> return a
