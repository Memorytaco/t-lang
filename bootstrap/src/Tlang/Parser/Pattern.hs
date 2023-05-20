module Tlang.Parser.Pattern
  ( ExpressionToken
  , ParsePattern
  , ParsePatternType
  , Parser
  , getParser
  , liftParser
  , unParser
  , play
  )
where

import Tlang.Parser.Pratt
import Tlang.Parser.Lexer

import Tlang.AST

import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Control.Monad.Reader (ReaderT (..), ask, asks, MonadReader, runReaderT)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Data.Text (Text, pack, unpack)
import Data.Functor (($>), (<&>))
import Data.List (find)
import Data.Void (Void)
import Data.Bifunctor (first)

import qualified Tlang.Parser.Type as TypParser

newtype Parser e m a = Parser
  { unWrapParser :: ReaderT ([Operator String], [Operator String]) (ParsecT e Text m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader ([Operator String], [Operator String]))
    deriving newtype (Alternative, MonadFail, MonadPlus)
deriving newtype instance ShowErrorComponent e => MonadParsec e Text (Parser e m)

type ParsePattern typ = Pattern ((:@) typ) Symbol
type ParsePatternType = ParsePattern TypParser.ParseType 

type ExpressionToken = OperatorClass String ParsePatternType

instance (ShowErrorComponent e) => OperatorParser ExpressionToken (Parser e m) where
  type Expression ExpressionToken = ParsePatternType
  next = try (OpNorm <$> wild <?> "Wild Pattern")
     <|> (OpNorm <$> variable <?> "Identifier")
     <|> (OpNorm <$> vlabel <?> "Variant Label")
     <|> (OpNorm <$> num <?> "Literal Match Number")
     <|> (OpNorm <$> str <?> "Literal Match String")
     <|> try (OpRep . Right <$> (try operator <|> syms) <?> "Pattern Operator")
     <|> (OpRep . Left  <$> special <?> "Pair Operator")
    where
      variable = char '?' *> identifier <&> PatRef . Symbol
      vlabel = identifier <&> PatSym . Symbol
      wild = symbol "_" $> PatWild <* notFollowedBy identifier
      num = try floating <|> nat
      nat = integer <&> PatLit . LitInt
      floating = float <&> PatLit . LitNumber
      str = stringLiteral <&> PatLit . LitString
      withSpecial v@(l, r) = (reservedOp (pack l) <|> reserved (pack l)) $> Left v
                         <|> (reservedOp (pack r) <|> reserved (pack r)) $> Right v
      special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("{", "}")]
      syms = fmap unpack . foldl1 (<|>) $ reservedOp <$> [":", "@"]

  peek = lookAhead next

  getPower = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op <&> getBindPower

  nud end = withOperator return \case
    (Left (Left  (l, r))) -> case l of
      "{" -> record
      _ -> patUnit <|> try tuple <|> (pratt (void . lookAhead . reservedOp $ pack r) (-100) <* reservedOp (pack r))
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> show op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> show op
        _ -> do right <- pratt end r
                return $ PatSum (PatRef $ Op s) [right]

  led end left = withOperator (patternSum left) \case
    (Left (Left  (l, r))) -> case l of
      "{" -> record >>= patternSum left
      _ -> patUnit <|> try tuple <|> (pratt (void . lookAhead . reservedOp $ pack r) (-100) <* reservedOp (pack r)) >>= patternSum left
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \(Operator assoc _ r _) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
          "@" -> case left of
              PatWild -> PatBind (Symbol "_") <$> pratt end r
              PatRef name -> PatBind name <$> pratt end r
              _ -> fail "expect plain identifier on the left of '@' for binder syntax"
          "->" -> case left of
              PatRef name -> PatView name <$> pratt end r
              _ -> fail "view pattern projector should be a single function"
          ":" -> case left of
              PatAnno _ -> fail $ "Can't annotate again for pattern, " <> show left
              -- PatSym _ -> fail "Can't annotate symbol for polymorphic variant"
              _ -> do env <- ask
                      let typParser e = liftParser . TypParser.unParser (fst env) (getParser e env)
                      PatAnno . (left :@) <$> typParser (lookAhead end) (-100)
          _ -> do right <- pratt end (-100)
                  return $ PatSum (PatRef $ Op s) [left, right]
        _ -> return $ PatSum (PatRef $ Op s) [left]

patternSum :: ParsePatternType -> ParsePatternType -> Parser e m ParsePatternType
patternSum left right =
  case left of
    PatRef _ -> return $ PatSum left [right]
    PatSym _ -> return $ PatSum left [right]
    PatSum v vs -> return $ PatSum v (vs <> [right])
    _ -> fail "expect sum pattern, but it is not"

record :: ShowErrorComponent e => Parser e m ParsePatternType
record = do
  let rlabel = Symbol <$> identifier <|> Op <$> operator
      lpattern = pratt (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100)
      field = (,) <$> rlabel <*> (reservedOp "=" *> lpattern)
  sections <- sepBy1 field (reservedOp ",")
  void $ reservedOp "}"
  return (PatRec sections)

-- | parser group for parenthesis
tuple, patUnit :: ShowErrorComponent e => Parser e m ParsePatternType
tuple = do
  p1 <- pratt (void . lookAhead $ reservedOp "," <|> reservedOp ")") (-100) <* reservedOp ","
  ps <- sepBy1 (pratt (void . lookAhead $ reservedOp "," <|> reservedOp ")") (-100)) (reservedOp ",") <* reservedOp ")"
  return $ PatTup (p1: ps)
patUnit = reservedOp ")" $> PatUnit

getOperator :: String
            -> Parser e m (Operator String)
getOperator "@" = return $ Operator Infix 999 999 "@"
getOperator "->" = return $ Operator Infix (-10) (-15) "->"
getOperator ":" = return $ Operator Infix (-15) (-20) ":"
getOperator op = do
  stat <- asks snd
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined in term level."

getParser :: Parser e m a -> ([Operator String], [Operator String]) -> ParsecT e Text m a
getParser = runReaderT . unWrapParser
liftParser :: ParsecT e Text m a -> Parser e m a
liftParser = Parser . ReaderT . const

unParser :: ShowErrorComponent e
         => ([Operator String], [Operator String])
         -> ParsecT e Text m ()
         -> Integer
         -> ParsecT e Text m ParsePatternType
unParser r end rbp = getParser (pratt (liftParser end) rbp) r

play :: Monad m
     => ([Operator String], [Operator String]) -> Text
     -> m (Either String ParsePatternType)
play op txt = first errorBundlePretty <$> runParserT (unParser @Void op eof (-100)) "stdin" txt
