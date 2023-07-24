module Language.Parser
  (
    -- ** reexport of module
    module Module
  , module Pattern
  , module Expr
  , module Decl
  , module Type
  , module Class
  , module Lexer
  )
where

import Language.Parser.Module as Module
import Language.Parser.Pattern as Pattern
import Language.Parser.Expr as Expr
import Language.Parser.Decl as Decl
import Language.Parser.Type as Type
import Language.Parser.Class as Class
import Language.Parser.Lexer as Lexer

-- parseSource
--   :: (MonadIO m, MonadError String m)
--   => [FilePath] -> m [Module (DeclareExtension ParseType) Symbol]
-- parseSource paths = do
--   edges <- forM paths \path -> do
--     source <- liftIO $ decodeUtf8 <$> readFile path
--     (keys', key) <- runParserT (moduleHeader @Void) path source >>= liftEither . first errorBundlePretty
--     return (source, key, fmap (\(Use (NameAlias k _) _) -> k) keys')
--   let (graph, getNode, getVertex) = graphFromEdges edges
--       bforest = bcc graph
--   -- if bforest == [] then return () else fail "Cyclic dependency of module detected"
--   let over bs i = do
--         let (source, key, keys) = getNode i
--             lookupModule p = find \(Module s _ _) -> p == s
--         case lookupModule key bs of
--           Just _ -> return bs
--           Nothing -> do
--             (m, _) <- parseModule @Void (show key) (typOperator, []) bs declaration source >>= liftEither . first errorBundlePretty . fst
--             return $ m:bs
--   foldM over [] $ reverseTopSort graph
