module Compiler.SourceParsing
  (

  -- ** parsing modules
    loadModuleFromFile
  , loadModuleFromText

  -- ** parsing items from text
  , getSurfaceExpr
  , getSurfaceType

  , getSurfaceDecl
  , getSurfaceDecls

  -- ** re-export builtin parser
  , driveParser -- ^ driver
  , surfaceExpr -- ^ expression
  , surfaceDecl -- ^ declaration
  , surfaceType -- ^ type
  )
where

import Language.Core ( ModuleSurface, TypSurface, ExprSurface, DeclSurface, OperatorStore, builtinStore, fuseModuleName, Module (_moduleHeader))
import Compiler.Store ( HasCompilerStore, UseCompilerStore, stageSourceParsing, spSources, spFiles )
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Driver.Parser (driveParser, surfaceDecl, surfaceType, surfaceExpr, surfaceModule)

import Language.Parser.Lexer ( reservedOp )
import Text.Megaparsec (eof, ParseErrorBundle, MonadParsec (lookAhead), sepBy1, (<|>))
import Data.Void (Void)
import Data.Functor ( ($>) )
import Control.Lens ( (<&>), (%~), (^.) )
import Capability.State ( modify, gets )
import qualified Data.Map as Map
import qualified Data.Text.IO as Text

loadModuleFromFile
  :: (HasCompilerStore m, MonadFail m, MonadIO m) => FilePath -> m ModuleSurface
loadModuleFromFile path = liftIO (Text.readFile path) >>= loadModuleFromText path
{-# INLINE loadModuleFromFile #-}

-- | TOOD: consider outdated files
loadModuleFromText
  :: (HasCompilerStore m, MonadFail m) => String -> Text -> m ModuleSurface
loadModuleFromText path content = do
  sources <- gets @UseCompilerStore (^. (stageSourceParsing . spSources))
  (m'either, _) <- driveParser builtinStore (surfaceModule (snd <$> sources) (reservedOp ";;" $> ()) eof) path content
  m <- case m'either of
    Right m -> return m
    Left _ -> fail "" -- TOOD: complete error message
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

-- | declaration may modify operator environment.
getSurfaceDecl :: Monad m
  => OperatorStore -> String -> Text
  -> m ( Either (ParseErrorBundle Text Void) DeclSurface
       , OperatorStore
       )
getSurfaceDecl ops = driveParser ops (surfaceDecl eof)

getSurfaceDecls :: Monad m
  => OperatorStore -> String -> Text
  -> m ( Either (ParseErrorBundle Text Void) [DeclSurface]
       , OperatorStore
       )
getSurfaceDecls ops = driveParser ops
  $ surfaceDecl (lookAhead $ reservedOp ";;" $> () <|> eof) `sepBy1` reservedOp ";;"
