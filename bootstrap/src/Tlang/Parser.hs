module Tlang.Parser
  ( parseSource
  )
where

{-

As the file name suggests.

This module includes
- TypedName, UnTypedName. we have UnTypedName when building up AST, and then we resolve it to TypedName.
- TypeAnno. it is used to represent user type annotation and it also serves as initial type to literal value.

-}

import Tlang.AST
import Tlang.Parser.Module (parseModule, moduleHeader)

import Prelude hiding (readFile)
import Text.Megaparsec
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Char8 (readFile)
import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Except (MonadError, liftEither)
import Data.Bifunctor (first)
import Data.Graph (graphFromEdges, bcc, reverseTopSort)
import Data.List (find)
import Data.Void (Void)
import Tlang.Parser.Decl (declaration)
import Tlang.Parser.Decl (DeclareExtension)
import Tlang.Parser.Type (ParseType)

parseSource
  :: (MonadIO m, MonadError String m)
  => [FilePath] -> m [Module (DeclareExtension ParseType) Symbol]
parseSource paths = do
  edges <- forM paths \path -> do
    source <- liftIO $ decodeUtf8 <$> readFile path
    (keys', key) <- runParserT (moduleHeader @Void) path source >>= liftEither . first errorBundlePretty
    return (source, key, fmap (\(Use (NameAlias k _) _) -> k) keys')
  let (graph, getNode, getVertex) = graphFromEdges edges
      bforest = bcc graph
  -- if bforest == [] then return () else fail "Cyclic dependency of module detected"
  let over bs i = do
        let (source, key, keys) = getNode i
            lookupModule p = find \(Module s _ _) -> p == s
        case lookupModule key bs of
          Just _ -> return bs
          Nothing -> do
            (m, _) <- parseModule @Void (show key) (typOperator, []) bs declaration source >>= liftEither . first errorBundlePretty . fst
            return $ m:bs
  foldM over [] $ reverseTopSort graph
