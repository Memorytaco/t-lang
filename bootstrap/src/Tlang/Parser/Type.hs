{- | * Parser for type expression
    TODO: add parser for type literal
-}

module Tlang.Parser.Type
  ( Parser
  , ExpressionToken
  , ParseType

  , getParser
  , liftParser
  , dataVariant
  , record

  , unParser
  , play -- | for development only
  )
where

import Tlang.Parser.Pratt
import Tlang.Parser.Lexer

import Tlang.AST

import Data.Text (Text, pack)
import Data.List (find)
import Data.Bifunctor (first)
import Data.Functor (($>), (<&>))
import Text.Megaparsec hiding (Label)
import Control.Monad.Reader (ReaderT (..), ask, runReaderT, MonadReader)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Data.Void (Void)
import Control.Monad.Identity (Identity)
import Tlang.Generic (inj)
import Tlang.Helper.AST.Type (getTypeLit)
import Tlang.Extension.Type (Tuple (..), Forall (..), Variant (..), Record (..), Scope (..))

newtype Parser e m a = Parser
  { unWrapParser :: ReaderT [Operator String] (ParsecT e Text m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader [Operator String])
    deriving newtype (Alternative, MonadFail, MonadPlus)
deriving newtype instance ShowErrorComponent e => MonadParsec e Text (Parser e m)

type ParseType = TypeAST Identity
type ExpressionToken = OperatorClass String ParseType

instance ShowErrorComponent e => OperatorParser ExpressionToken (Parser e m) where
  type Expression ExpressionToken = ParseType
  next = (OpRep . Left <$> syntax <?> "Type Pair Operator")
     <|> (OpRep . Right <$> operator <?> "Type Operator")
     <|> (OpNorm <$> typNormal <?> "Type Name")
    where
      typNormal = identifier <&> TypRef . Symbol
      withSpecial v@(l, r) = reservedOp (pack l) $> Left v
                         <|> reservedOp (pack r) $> Right v
      special = foldr1 (<|>) $ withSpecial
            <$> [("(", ")"), ("[", "]"), ("<", ">"), ("{", "}")]
      syntax  = reserved "forall" $> Left ("forall", "forall")
            <|> reservedOp "\\" $> Left ("\\", "\\")
            <|> special

  peek = lookAhead next

  getPower = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op <&> getBindPower

  nud end = withOperator return \case
    Left (Left (l, r)) -> do
      case l of
        "{" -> record
        "<" -> variant
        "forall" -> quantified end
        "\\" -> abstract end
        _ -> do ntok <- peek
                matchPairOperator (Right ("(", ")")) ntok
                  (next $> TypLit (inj $ Tuple [])) $ pratt (void $ reservedOp (pack r)) (-100)
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    Right s -> getOperator s >>= \(Operator assoc _ r op) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> op
        _ -> do right <- pratt end r
                return $ TypCon (TypRef $ Op op) [right]

  led (lookAhead -> end) left = withOperator (return . apply left) \case
    Right s -> getOperator s >>= \(Operator assoc _ r op) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
                  "*" -> pratt end r >>= \right -> return
                    case getTypeLit @Tuple right of
                      Just (Tuple ls) -> TypLit . inj . Tuple $ left:ls
                      Nothing -> TypLit . inj $ Tuple [left, right]
                    -- TypTup ls -> return . TypTup $ left: ls
                    -- right -> return $ TypTup [left, right]
                  "->" -> TypCon (TypRef (Op "->")) . (left:) . pure <$> pratt end r
                  _ -> pratt end r <&> TypCon (TypRef $ Op op) . (left:) . pure
        _ -> return $ TypCon (TypRef $ Op op) [left]
    Left (Left (l, r)) ->
      case l of
        "(" -> do
          ntok <- peek
          matchPairOperator (Right ("(", ")")) ntok
            (next $> left `apply` TypLit (inj $ Tuple [])) $ pratt (void $ reservedOp (pack r)) (-100) <&> apply left
        "{" -> apply left <$> record
        "<" -> apply left <$> variant
        "forall" -> apply left <$> quantified end
        "\\" -> apply left <$> abstract end
        _   -> fail $ "Unrecognized " <> l <> ", It could be an internal error, please report to upstream."
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"

apply :: Type name cons bind inj rep -> Type name cons bind inj rep -> Type name cons bind inj rep
apply (TypCon v vs) r = TypCon v (vs <> [r])
apply l r = TypCon l [r]

-- | equi-recursive type
-- recursive :: ShowErrorComponent e => Parser e m () -> Parser e m (ParseType)
-- recursive (lookAhead -> end) = do
--   name <- Symbol <$> identifier <* reservedOp "."
--   body <- pratt end (-100)
--   return $ TypEqu (name :> TypPht) body

quantified :: ShowErrorComponent e => Parser e m () -> Parser e m ParseType
quantified (lookAhead -> end) = do
  let name = Symbol <$> identifier
      boundTyp = pratt (void . lookAhead $ reservedOp ")") (-100)
      scopBound = do
        tok <- name
        op <- reservedOp "~" $> (:>) <|> reservedOp "=" $> (:~)
        op tok <$> boundTyp
      bound = parens scopBound
          <|> (:> TypPht) <$> name
  bounds <- bound `someTill` reservedOp "."
  body <- pratt end (-100)
  return $ foldr ($) body (TypLet . inj . Forall <$> bounds)

abstract :: ShowErrorComponent e => Parser e m () -> Parser e m ParseType
abstract (lookAhead -> end) = do
  let name = Symbol <$> identifier
  bounds <- ((:> TypPht) <$> name) `someTill` reservedOp "."
  body <- pratt end (-100)
  return $ foldr ($) body (TypLet . inj . Scope <$> bounds)

-- | record type parser
record :: ShowErrorComponent e => Parser e m ParseType
record = do
  let recordPair = do
        name <- fmap Label $ identifier <|> operator
        typ <- reservedOp ":" *> pratt (lookAhead ((reservedOp "}" <|> reservedOp ",") $> ())) (-100)
        return (name, typ)
  fields <- sepBy1 recordPair (reservedOp ",") <* reservedOp "}"
  return . TypLit . inj $ Record fields

-- | variant type parser
variant :: ShowErrorComponent e => Parser e m ParseType
variant = do
  let variantPair = do
        name <- fmap Label $ identifier <|> operator
        typ <- optional $ reservedOp ":" *> pratt (lookAhead ((reservedOp ">" <|> reservedOp ",") $> ())) (-100)
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp ",") <* reservedOp ">"
  return . TypLit . inj $ Variant fields

-- | a specialized version used in type declaration context. see `Tlang.AST.Declaration`.
dataVariant :: ShowErrorComponent e => Parser e m () -> Parser e m ParseType
dataVariant end = do
  let variantPair = do
        name <- fmap Label $ identifier <|> operator
        isEnd <- lookAhead . optional $ void (reservedOp "|") <|> end
        typ <- maybe
                 (Just <$> pratt (lookAhead $ void (reservedOp "|") <|> end) (-100))
                 (const $ return Nothing) isEnd
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp "|") <* end
  return . TypLit . inj $ Variant fields

getOperator :: String -> Parser e m (Operator String)
getOperator op = do
  stat <- ask
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> op <> " is undefined in type level."


liftParser :: ParsecT e Text m a -> Parser e m a
liftParser = Parser . ReaderT . const
getParser :: Parser e m a -> [Operator String] -> ParsecT e Text m a
getParser = runReaderT . unWrapParser

-- | build a type parser from operator table
unParser :: ShowErrorComponent e => [Operator String] -> ParsecT e Text m () -> Integer -> ParsecT e Text m ParseType
unParser r end rbp = getParser (pratt (liftParser end) rbp) r

-- | play and test and feel the parser
play :: Monad m => [Operator String] -> Text -> m (Either String ParseType)
play op txt = first errorBundlePretty <$> runParserT (unParser @Void op eof (-100)) "stdin" txt

