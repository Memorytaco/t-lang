module Tlang.Parser
  ( playParser
  , parseSource
  )
where

{-

As the file name suggests.

This module includes
- TypedName, UnTypedName. we have UnTypedName when building up AST, and then we resolve it to TypedName.
- TypeAnno. it is used to represent user type annotation and it also serves as initial type to literal value.

-}

import Tlang.AST
import Tlang.Parser.Module (runModule, ParsedModule, buildGraph)

import Prelude hiding (readFile)
import Text.Megaparsec
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Char8 (readFile)
import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Except (MonadError, liftEither)
import Data.Bifunctor (first)
import Data.Graph (graphFromEdges, bcc, reverseTopSort)
import Data.List (find)
import Data.Void (Void)

playParser :: forall e a. (ShowErrorComponent e, Show a) => ParsecT e Text IO a -> Text -> IO ()
playParser p txt = runParserT p "stdin" txt >>= putStrLn . either errorBundlePretty show

parseSource :: (MonadFail m, MonadIO m, MonadError String m) => [FilePath] -> m [ParsedModule]
parseSource paths = do
  edges <- forM paths \path -> do
    source <- liftIO $ decodeUtf8 <$> readFile path
    (keys', key) <- runParserT (buildGraph @Void) path source >>= liftEither . first errorBundlePretty
    return (source, key, fmap (\(Use (k, _) _) -> k) keys')
  let (graph, getNode, getVertex) = graphFromEdges edges
      bforest = bcc graph
  -- if bforest == [] then return () else fail "Cyclic dependency of module detected"
  let over bs i = do
        let (source, key, keys) = getNode i
            lookupModule p = find \(Module _ s _ _) -> p == s
        case lookupModule key bs of
          Just _ -> return bs
          Nothing -> do
            (m, _) <- runModule @Void (show key) (typOperator, []) bs source >>= liftEither . first errorBundlePretty . fst
            return $ m:bs
  foldM over [] $ reverseTopSort graph

-- onExprNameF :: (name1 -> name2) -> Base (Expr op name1) (Expr op name2) -> (Expr op name2)
-- onExprNameF f (ExLitF name v) = ExLit (f name) v
-- onExprNameF f (ExRefF name) = ExRef (f name)
-- onExprNameF f (ExCallF name v1 v2) = ExCall (f name) v1 v2
-- onExprNameF f (ExBindF name a v) = ExBind (f name) a v
-- onExprNameF f (ExOpF op name v1 v2) = ExOp op (f name) v1 v2
-- onExprNameF f (ExOpUniF op name v) = ExOpUni op (f name) v

-- mapExprNameF :: (name -> a) -> (a -> a -> a) -> Base (Expr op name) a -> a
-- mapExprNameF f _ (ExLitF n _) = f n
-- mapExprNameF f _ (ExRefF n) = f n
-- mapExprNameF f o (ExCallF n a1 a2) = f n `o` (a1 `o` a2)
-- mapExprNameF f o (ExBindF n _ a) = f n `o` a
-- mapExprNameF f o (ExOpF _ n a1 a2) = f n `o` (a1 `o` a2)
-- mapExprNameF f o (ExOpUniF _ n a) = f n `o` a

-- parseToplevel :: String -> Either ParseError [Definition (UnTypedName Op)]
-- parseToplevel = parseTop builtinOperator
