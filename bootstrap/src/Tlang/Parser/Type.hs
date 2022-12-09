module Tlang.Parser.Type
  ( Parser
  , ExpressionToken

  , getParser
  , liftParser
  , dataVariant

  , unParser
  , play -- ^ for development only
  )
where

import Tlang.Parser.Pratt
import Tlang.Parser.Lexer

import Tlang.AST

import Data.Text (Text, pack)
import Data.List (find)
import Data.Bifunctor (first)
import Text.Megaparsec
import Control.Monad.Reader (ReaderT (..), ask, runReaderT, MonadReader)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Data.Void (Void)

newtype Parser e m a = Parser
  { unWrapParser :: ReaderT [Operator String] (ParsecT e Text m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader [Operator String])
    deriving newtype (Alternative, MonadFail, MonadPlus)
deriving newtype instance ShowErrorComponent e => MonadParsec e Text (Parser e m)

type ExpressionToken = (OperatorClass String (Type (TypName (Operator String)) ()))

instance ShowErrorComponent e => OperatorParser ExpressionToken (Parser e m) where
  type Expression ExpressionToken = (Type (TypName (Operator String)) ())
  next = (OpNorm <$> typNormal <?> "Type Name")
     <|> (OpRep . Left <$> special <?> "Type Pair Operator")
     <|> (OpRep . Right <$> operator <?> "Type Operator")
    where
      typNormal = identifier >>= return . TypRef . TypName
      withSpecial v@(l, r) = reservedOp (pack l) *> pure (Left v)
                         <|> reservedOp (pack r) *> pure (Right v)
      special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("[", "]"), ("<", ">"), ("{", "}")]

  peek = lookAhead next

  getPower = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op >>= return . getBindPower

  nud end = withOperator return \case
    Left (Left (l, r)) -> do
      case l of
        "{" -> record
        "<" -> variant
        _ -> do ntok <- peek
                matchPairOperator (Right ("(", ")")) ntok
                  (next *> return TypUnit) $ pratt (reservedOp (pack r) *> pure ()) (-100)
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    Right s -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> show op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> show op
        _ -> do right <- pratt end r
                return $ TypApp (TypRef $ TypNameOp op) right

  led (lookAhead -> end) left = withOperator (return . TypApp left) \case
    Right s -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
                  "." -> case left of
                    TypRef name -> pratt end r >>= return . TypAll name
                    _ -> fail $ "expect type variable before operator \".\" but got: " <> show left
                  "*" -> pratt end r >>= \case
                    TypTup ls -> return . TypTup $ left: ls
                    right -> return $ TypTup [left, right]
                  _ -> pratt end r >>= return . TypApp (TypApp (TypRef $ TypNameOp op) left)
        _ -> return $ TypApp (TypRef $ TypNameOp op) left
    Left (Left (l, r)) ->
      case l of
        "(" -> do
          ntok <- peek
          matchPairOperator (Right ("(", ")")) ntok
            (next *> return (TypApp left TypUnit)) $ pratt (reservedOp (pack r) *> pure ()) (-100) >>= return . TypApp left
        "{" -> record >>= return . TypApp left
        "<" -> variant >>= return . TypApp left
        _   -> fail $ "Unrecognized " <> l <> ", It could be an internal error, please report to upstream."
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"

-- | record type parser
record :: ShowErrorComponent e => Parser e m (Type (TypName (Operator String)) ())
record = do
  let recordPair = do
        name <- fmap TypLabel $ identifier <|> operator
        typ <- reservedOp ":" *> pratt (lookAhead ((reservedOp "}" <|> reservedOp ",") *> pure ())) (-100)
        return (name, typ)
  fields <- sepBy1 recordPair (reservedOp ",") <* reservedOp "}"
  return $ TypRec fields

-- | variant type parser
variant :: ShowErrorComponent e => Parser e m (Type (TypName (Operator String)) ())
variant = do
  let variantPair = do
        name <- fmap TypLabel $ identifier <|> operator
        typ <- optional $ reservedOp ":" *> pratt (lookAhead ((reservedOp ">" <|> reservedOp ",") *> pure ())) (-100)
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp ",") <* reservedOp ">"
  return $ TypSum fields

-- | a specialized version used in type declaration context. see `Tlang.AST.Declaration`.
dataVariant :: ShowErrorComponent e => Parser e m () -> Parser e m (Type (TypName (Operator String)) ())
dataVariant end = do
  let variantPair = do
        name <- fmap TypLabel $ identifier <|> operator
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
unParser :: ShowErrorComponent e => [Operator String] -> ParsecT e Text m () -> Integer -> ParsecT e Text m (Type (TypName (Operator String)) ())
unParser r end rbp = getParser (pratt (liftParser end) rbp) r

-- | play and test and feel the parser
play :: Monad m => [Operator String] -> Text -> m (Either String (Type (TypName (Operator String)) ()))
play op txt = first errorBundlePretty <$> runParserT (unParser @Void op eof (-100)) "stdin" txt
 
