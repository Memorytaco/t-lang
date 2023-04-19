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
import qualified LLVM.AST.Type (Type)

newtype Parser e m a = Parser
  { unWrapParser :: ReaderT [Operator String] (ParsecT e Text m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader [Operator String])
    deriving newtype (Alternative, MonadFail, MonadPlus)
deriving newtype instance ShowErrorComponent e => MonadParsec e Text (Parser e m)

type ParseType c f = ASTType c f LLVM.AST.Type.Type
type ExpressionToken = OperatorClass String (ParseType None Identity)

instance ShowErrorComponent e => OperatorParser ExpressionToken (Parser e m) where
  type Expression ExpressionToken = (ParseType None Identity)
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
            <|> reserved "rec" $> Left ("rec", "rec")
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
        "rec" -> recursive end
        "forall" -> quantified end
        "\\" -> abstract end
        _ -> do ntok <- peek
                matchPairOperator (Right ("(", ")")) ntok
                  (next $> TypUni) $ pratt (void $ reservedOp (pack r)) (-100)
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    Right s -> getOperator s >>= \(Operator assoc _ r op) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> op
        _ -> do right <- pratt end r
                return $ TypApp (TypRef $ Op op) right []

  -- led (lookAhead -> end) left = withOperator (return . TypApp left) \case
  led (lookAhead -> end) left = withOperator (return . apply left) \case
    Right s -> getOperator s >>= \(Operator assoc _ r op) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
                  "*" -> pratt end r >>= \case
                    TypTup ls -> return . TypTup $ left: ls
                    right -> return $ TypTup [left, right]
                  "->" -> TypApp (TypRef (Op "->")) left . (:[]) <$> pratt end r
                  _ -> pratt end r <&> TypApp (TypRef $ Op op) left . (:[])
        _ -> return $ TypApp (TypRef $ Op op) left []
    Left (Left (l, r)) ->
      case l of
        "(" -> do
          ntok <- peek
          matchPairOperator (Right ("(", ")")) ntok
            (next $> left `apply` TypUni) $ pratt (void $ reservedOp (pack r)) (-100) <&> apply left
        "{" -> apply left <$> record
        "<" -> apply left <$> variant
        "rec" -> apply left <$> recursive end
        "forall" -> apply left <$> quantified end
        "\\" -> apply left <$> abstract end
        _   -> fail $ "Unrecognized " <> l <> ", It could be an internal error, please report to upstream."
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"

apply (TypApp h1 h2 ls) r = TypApp h1 h2 (ls <> [r])
apply l r = TypApp l r []

-- | equi-recursive type
recursive :: ShowErrorComponent e => Parser e m () -> Parser e m (ParseType None Identity)
recursive (lookAhead -> end) = do
  name <- Symbol <$> identifier <* reservedOp "."
  body <- pratt end (-100)
  return $ TypEqu (name :> TypBot) body

quantified :: ShowErrorComponent e => Parser e m () -> Parser e m (ParseType None Identity)
quantified (lookAhead -> end) = do
  let name = Symbol <$> identifier
      boundTyp = pratt (void . lookAhead $ reservedOp ")") (-100)
      scopBound = do
        tok <- name
        op <- reservedOp "~" $> (:>) <|> reservedOp "=" $> (:~)
        op tok <$> boundTyp
      bound = parens scopBound
          <|> (:> TypBot) <$> name
  bounds <- bound `someTill` reservedOp "."
  body <- pratt end (-100)
  return $ foldr ($) body (TypAll <$> bounds)

abstract :: ShowErrorComponent e => Parser e m () -> Parser e m (ParseType None Identity)
abstract (lookAhead -> end) = do
  let name = Symbol <$> identifier
  bounds <- ((:> TypBot) <$> name) `someTill` reservedOp "."
  body <- pratt end (-100)
  return $ foldr ($) body (TypAbs <$> bounds)

-- | record type parser
record :: ShowErrorComponent e => Parser e m (ParseType None Identity)
record = do
  let recordPair = do
        name <- fmap Label $ identifier <|> operator
        typ <- reservedOp ":" *> pratt (lookAhead ((reservedOp "}" <|> reservedOp ",") $> ())) (-100)
        return (name, typ)
  fields <- sepBy1 recordPair (reservedOp ",") <* reservedOp "}"
  return $ TypRec fields

-- | variant type parser
variant :: ShowErrorComponent e => Parser e m (ParseType None Identity)
variant = do
  let variantPair = do
        name <- fmap Label $ identifier <|> operator
        typ <- optional $ reservedOp ":" *> pratt (lookAhead ((reservedOp ">" <|> reservedOp ",") $> ())) (-100)
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp ",") <* reservedOp ">"
  return $ TypSum fields

-- | a specialized version used in type declaration context. see `Tlang.AST.Declaration`.
dataVariant :: ShowErrorComponent e => Parser e m () -> Parser e m (ParseType None Identity)
dataVariant end = do
  let variantPair = do
        name <- fmap Label $ identifier <|> operator
        isEnd <- lookAhead . optional $ void (reservedOp "|") <|> end
        typ <- maybe
                 (Just <$> pratt (lookAhead $ void (reservedOp "|") <|> end) (-100))
                 (const $ return Nothing) isEnd
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp "|") <* end
  return $ TypSum fields

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
unParser :: ShowErrorComponent e => [Operator String] -> ParsecT e Text m () -> Integer -> ParsecT e Text m (ParseType None Identity)
unParser r end rbp = getParser (pratt (liftParser end) rbp) r

-- | play and test and feel the parser
play :: Monad m => [Operator String] -> Text -> m (Either String (ParseType None Identity))
play op txt = first errorBundlePretty <$> runParserT (unParser @Void op eof (-100)) "stdin" txt

