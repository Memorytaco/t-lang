module Tlang.Test

where


import Tlang.Inference.Type (unify, localKind, runTyping, cookExpr, cookPattern, NormalType, clean, bindCheck)
import Tlang.Inference.Kind (NormalKind)
import qualified Tlang.Parser.Type as PT
import qualified Tlang.Parser.Expr as PE
import qualified Tlang.Parser.Pattern as PP
import Tlang.AST (typOperator, Symbol (..), (:@) (..), Kind (..), Operator)

import Data.Text (Text, unpack)
import Data.Bifunctor (first)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)

defaultEnv :: [NormalKind :@ Symbol]
defaultEnv =
  [ Op "->" :@ (KindType ::> KindType ::> KindType)
  , Symbol "maybe" :@ (KindType ::> KindType)
  , Symbol "i8" :@ KindType
  , Symbol "str" :@ KindType
  , Symbol "g1" :@ (KindType ::> KindType ::> KindType ::> KindType)
  ]

playUnify :: [NormalKind :@ Symbol] -> Text -> Text -> IO ()
playUnify env l r = do
  el <- first (l,) <$> PT.play typOperator l
  er <- first (r,) <$> PT.play typOperator r
  case (,) <$> el <*> er of
    Left (txt, err) -> do
      putStrLn "In Text:"
      putStrLn "===="
      putStrLn (unpack txt)
      putStrLn "====\n"
      putStrLn err
    Right (rt1, rt2) -> do
      putStrLn $ "left: " <> show rt1
      putStrLn $ "right: " <> show rt2
      (t1, t2) <- (,) <$> runReaderT (localKind rt1) (env <> defaultEnv)
                      <*> runReaderT (localKind rt2) (env <> defaultEnv)
      runReaderT (unify t1 t2) [] >>= print . fmap clean
      -- show <$> runReaderT (unify t1 t2) [] >>= putStrLn

playInfer
  :: MonadFail m
  => [NormalKind :@ Symbol]
  -> ([Operator String], [Operator String])
  -> Text
  -> m (PE.ParseExpr (NormalType NormalKind))
playInfer env ops txt = do
  res'either <- PE.play ops txt
  expr <- case res'either of
    Left err -> fail err
    Right expr -> runReaderT (cookExpr expr) (defaultEnv <> env)
  (e, _, _) <- runTyping (Nothing, expr) mempty mempty
  return e

playBind
  :: MonadFail m
  => ([Operator String], [Operator String])
  -> Text
  -> m [Symbol]
playBind ops txt = do
  res'either <- PP.play ops txt
  pat <- case res'either of
    Left err -> fail err
    Right pat -> runReaderT (cookPattern pat) defaultEnv
  names'either <- runReaderT (runExceptT $ bindCheck pat) mempty
  case names'either of
    Left err -> fail $ show err
    Right names -> return names
