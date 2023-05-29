{-# LANGUAGE AllowAmbiguousTypes #-}
module Tlang.Parser.Pattern
  (
    -- ** pattern parser handler
    WithPattern
  , ParsePattern
  , ParsePatternType
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST

import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import Data.Functor (($>), (<&>))
import qualified Data.Kind as Kind (Type)
import Data.List (find)
import Tlang.Generic ((:+:), (:<:) (..))
import Tlang.Extension (LiteralText (..), LiteralInteger (..), LiteralNumber (..), Literal (..))
import Capability.Reader (HasReader, ask)

import qualified Tlang.Parser.Type as TypParser

type ParsePattern typ e = Pattern (LiteralInteger :+: LiteralText :+: LiteralNumber) ((:@) typ) Symbol e
type ParsePatternType e = ParsePattern TypParser.ParseType e

-- | the proxy and e will allow parsing nested expression
data WithPattern (m :: Kind.Type -> Kind.Type) (proxy :: any) (e :: Kind.Type)

instance
  ( MonadFail m, ShowErrorComponent err
  , MonadParsec err Text m, HasPratt proxy e m
  , HasReader "PatternOperator" [Operator String] m
  , HasReader "TypeOperator" [Operator String] m
  , Show e
  )
  => HasPratt (WithPattern m proxy e) (ParsePatternType e) m where
  type Token (WithPattern m proxy e) (ParsePatternType e) = OperatorClass String (Either e (ParsePatternType e))
  next _ = try (OpNorm . Right <$> wild <?> "Wild Pattern")
       <|> try (OpRep . Right <$> (syms <|> try operator) <?> "Pattern Operator")
       <|> (OpNorm . Left  <$> try (pratt @proxy (void . lookAhead $ reservedOp "->") (-100)) <?> "View Expression")
       <|> (OpNorm . Right <$> variable <?> "Identifier")
       <|> (OpNorm . Right <$> vlabel <?> "Variant Label")
       <|> (OpNorm . Right <$> num <?> "Literal Match Number")
       <|> (OpNorm . Right <$> str <?> "Literal Match String")
       <|> (OpRep . Left  <$> special <?> "Pair Operator")
      where
        variable = char '?' *> identifier <&> PatRef . Symbol
        vlabel = identifier <&> PatSym . Symbol
        wild = symbol "_" $> PatWild <* notFollowedBy identifier
        num = try floating <|> nat
        nat = integer <&> PatPrm . inj . LiteralInteger . Literal
        floating = float <&> PatPrm . inj . LiteralNumber . Literal
        str = stringLiteral <&> PatPrm . inj . LiteralText . Literal . pack
        withSpecial v@(l, r) = (reservedOp (pack l) <|> reserved (pack l)) $> Left v
                           <|> (reservedOp (pack r) <|> reserved (pack r)) $> Right v
        special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("{", "}")]
        syms = fmap unpack . foldl1 (<|>) $ reservedOp <$> [":", "@", "->"]

  peek witness = lookAhead $ next witness

  getPower _ = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op <&> getBindPower

  nud _ end = withOperator (either (patternView @proxy end) return) \case
    (Left (Left  (l, r))) -> case l of
      "{" -> record @proxy @e
      "->" -> fail "view pattern projector should be paired with expression: expression -> pat"
      _ -> patUnit <|> try (tuple @proxy @e)
                   <|> (pratt  @(WithPattern m proxy e) (void . lookAhead . reservedOp $ pack r) (-100) <* reservedOp (pack r))
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> show op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> show op
        _ -> do right <- pratt @(WithPattern m proxy e) end r
                return $ PatSum (PatRef $ Op s) [right]

  led _ end left = withOperator (either (patternView @proxy end) (patternSum @e left)) \case
    (Left (Left  (l, r))) -> case l of
      "{" -> record @proxy @e >>= patternSum @e left
      _ ->  patUnit
        <|> try (tuple @proxy @e)
        <|> (pratt @(WithPattern m proxy e) (void . lookAhead . reservedOp $ pack r) (-100) <* reservedOp (pack r))
           >>= patternSum @e left
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \(Operator assoc _ r _) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
          "@" -> case left of
              PatWild -> PatBind (Symbol "_") <$> pratt @(WithPattern m proxy e) end r
              PatRef name -> PatBind name <$> pratt @(WithPattern m proxy e) end r
              _ -> fail "expect plain identifier on the left of '@' for binder syntax"
          "->" -> fail "view pattern projector should be paired with expression: expression -> pat"
          ":" -> case left of
              PatAnno _ -> fail $ "Can't annotate again for pattern, " <> show left
              -- PatSym _ -> fail "Can't annotate symbol for polymorphic variant"
              _ -> do PatAnno . (left :@) <$> pratt @(TypParser.WithType m) (lookAhead end) (-100)
          _ -> do right <- pratt @(WithPattern m proxy e) end (-100)
                  return $ PatSum (PatRef $ Op s) [left, right]
        _ -> return $ PatSum (PatRef $ Op s) [left]

patternView
  :: forall proxy e m err.
    ( MonadFail m, ShowErrorComponent err
    , MonadParsec err Text m
    , HasPratt (WithPattern m proxy e) (ParsePatternType e) m
    )
  => m () -> e -> m (ParsePatternType e)
patternView end e = reservedOp "->" *> pratt @(WithPattern m proxy e) end (-100) <&> PatView e

patternSum :: forall e m. (MonadFail m)
           => ParsePatternType e -> ParsePatternType e -> m (ParsePatternType e)
patternSum left right =
  case left of
    PatRef _ -> return $ PatSum left [right]
    PatSym _ -> return $ PatSum left [right]
    PatSum v vs -> return $ PatSum v (vs <> [right])
    _ -> fail "expect sum pattern, but it is not"

record :: forall proxy e m err. ( MonadFail m, ShowErrorComponent err
  , MonadParsec err Text m, HasPratt proxy e m, HasPratt (WithPattern m proxy e) (ParsePatternType e) m)
       => m (ParsePatternType e)
record = do
  let rlabel = Symbol <$> identifier <|> Op <$> operator
      lpattern = pratt @(WithPattern m proxy e) (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100)
      field = (,) <$> rlabel <*> (reservedOp "=" *> lpattern)
  sections <- sepBy1 field (reservedOp ",")
  void $ reservedOp "}"
  return (PatRec sections)

-- | parser group for parenthesis
tuple
  :: forall proxy e err m.
    ( MonadFail m, ShowErrorComponent err, MonadParsec err Text m, Show e
    , HasPratt proxy e m
    , HasReader "TypeOperator" [Operator String] m
    , HasReader "PatternOperator" [Operator String] m)
    => m (ParsePatternType e)
tuple = do
  p1 <- pratt @(WithPattern m proxy e) (void . lookAhead $ reservedOp "," <|> reservedOp ")") (-100) <* reservedOp ","
  ps <- sepBy1 (pratt @(WithPattern m proxy e) (void . lookAhead $ reservedOp "," <|> reservedOp ")") (-100)) (reservedOp ",") <* reservedOp ")"
  return $ PatTup (p1: ps)
patUnit :: (MonadParsec err Text m, ShowErrorComponent err) => m (ParsePatternType e)
patUnit = reservedOp ")" $> PatUnit

getOperator :: (MonadFail m, HasReader "PatternOperator" [Operator String] m)
            => String -> m (Operator String)
getOperator "@" = return $ Operator Infix 999 999 "@"
getOperator "->" = return $ Operator Infix (-10) (-15) "->"
getOperator ":" = return $ Operator Infix (-15) (-20) ":"
getOperator op = do
  stat <- ask @"PatternOperator"
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined in term level."
