module Tlang.Parser.Expr
  (
    WithExpr
  , ParseExpr
  , ParseExprType
  , ParseLambda
  , ParseLambdaType

  , bigLambda
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST

import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.Text (Text, pack, unpack)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Kind as Kind (Type)

import Capability.Reader (HasReader, asks)

import Tlang.Parser.Type (WithType, ParseType)
import qualified Tlang.Parser.Type as Parser (record)
import Tlang.Parser.Pattern (WithPattern)
import Tlang.Helper.AST.Type (injTypeLit)
import Tlang.Extension
import Tlang.Generic ((:+:), (:<:) (..))

type ParseExpr typ
  = Expr (Let (Pattern (LiteralInteger :+: LiteralText :+: LiteralNumber) ((:@) typ) Symbol)
          :+: Lambda (GPattern (LiteralInteger :+: LiteralText :+: LiteralNumber) ((:@) typ) Symbol) (Bounds Symbol typ)
          :+: Apply)
         (Tuple :+: Record Symbol :+: LiteralText :+: LiteralInteger :+: LiteralNumber
          :+: VisibleType typ :+: Selector Symbol :+: Constructor Symbol)
         ((:@) typ)
    Symbol

type ParseExprType = ParseExpr ParseType
type ParseLambda typ = Lambda (GPattern (LiteralInteger :+: LiteralText :+: LiteralNumber) ((:@) typ) Symbol) (Bounds Symbol typ)
type ParseLambdaType = ParseLambda ParseType ParseExprType

data WithExpr (m :: Kind.Type -> Kind.Type)

instance
  ( ShowErrorComponent err, MonadParsec err Text m, MonadFail m
  , HasReader "TermOperator" [Operator String] m
  , HasReader "TypeOperator" [Operator String] m
  , HasReader "PatternOperator" [Operator String] m
  )
  => HasPratt (WithExpr m) ParseExprType m where
  type Token (WithExpr m) ParseExprType = OperatorClass String ParseExprType
  next _ = (OpNorm <$> try variable <?> "Identifier, Label and Selector")
     <|> (OpNorm <$> literal <?> "Literal")
     <|> (OpNorm <$> lexeme typ <?> "Type literal")
     <|> (OpRep . Left  <$> special <?> "Pair Operator")
     <|> (OpRep . Right <$> (sym <|> operator) <?> "Operator")
    where
      int = ExPrm . inj . LiteralInteger . Literal <$> integer <?> "Integer Number"
      floating = ExPrm . inj . LiteralNumber . Literal <$> float <?> "Floating Number"
      str = ExPrm . inj . LiteralText . Literal . pack <$> stringLiteral <?> "Literal String"
      literal = try floating <|> int <|> str

      name = ExRef . Symbol <$> identifier
      vlabel = char '`' *> identifier <&> ExPrm . inj . flip Constructor Nothing . Symbol
      field = char '.' *> identifier <&> ExPrm . inj . Selector . Symbol
      variable = name <|> vlabel <|> field

      withSpecial v@(l, r) = (reservedOp (pack l) <|> reserved (pack l)) $> Left v
                         <|> (reservedOp (pack r) <|> reserved (pack r)) $> Right v
      special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("let", "in"), ("{", "}"), ("[", "]")]
      sym = reservedOp ":" <|> reservedOp "\\" <&> unpack
      typ = string "@()" $> (visibleType . injTypeLit $ Tuple [])
        <|> string "@(" *> (visibleType <$> pratt @(WithType m) (void . lookAhead $ reservedOp ")") (-100) <* reservedOp ")")
        <|> string "@{" *> (Parser.record <&> visibleType)
        <|> string "@" *> (visibleType . TypRef . Symbol <$> identifier)
        where visibleType = ExPrm . inj . VisibleType

  peek witness = lookAhead (next witness)

  getPower _ = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op <&> getBindPower

  nud _ end = withOperator return \case
    (Left (Left  (l, r))) -> case l of
      "let" -> letExpr (lookAhead end)
      "{" -> record
      "[" -> bigLambda <&> ExStc . inj
      _ -> tunit <|> try tup <|> pratt @(WithExpr m) (void . lookAhead $ reservedOp (pack r)) (-100) <* reservedOp (pack r)
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> show op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> show op
        _ -> case s of
          "\\" -> smallLambda end <&> ExStc . inj
          _ -> do right <- pratt @(WithExpr m) end r
                  return . ExStc . inj $ Apply (ExRef $ Op s) right []

  -- led end left = withOperator (return . ExApp left) \case
  led _ end left = withOperator (return . apply left) \case
    (Left (Left  (l, r))) -> case l of
      "let" -> letExpr (lookAhead end)
      "{" -> record <&> apply left
      "[" -> bigLambda <&> apply left . ExStc . inj
      _ -> do right <- tunit <|> try tup <|> pratt @(WithExpr m) (void . lookAhead $ reservedOp (pack r)) (-100) <* reservedOp (pack r)
              return $ apply left right
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \(Operator assoc _ r _) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
            ":" -> ExInj . (left :@) <$> pratt @(WithType m) end (-100)
            _ -> pratt @(WithExpr m) end r <&> ExStc . inj . Apply (ExRef $ Op s) left . (:[])
        _ -> case s of
            "\\" -> apply left . ExStc . inj <$> smallLambda end
            _ -> return . ExStc . inj $ Apply (ExRef $ Op s) left []
    where
      apply l@(ExPrm v) r = case prj @(Constructor Symbol) v of
        Just (Constructor s Nothing) -> ExPrm . inj $ Constructor s (Just r)
        _ -> ExStc . inj $ Apply l r []
      apply l@(ExStc v) r = case prj @Apply v of
        Just (Apply l1 l2 rs) -> ExStc . inj $ Apply l1 l2 (rs <> [r])
        _ -> ExStc . inj $ Apply l r []
      apply l r = ExStc . inj $ Apply l r []

type ReaderM m =
  ( HasReader "TermOperator" [Operator String] m
  , HasReader "TypeOperator" [Operator String] m
  , HasReader "PatternOperator" [Operator String] m
  )

-- | let binding in expression
letExpr :: forall m err. (ShowErrorComponent err, MonadParsec err Text m, MonadFail m, ReaderM m) => m () -> m ParseExprType
letExpr end = do
  pat <- pratt @(WithPattern m (WithExpr m) ParseExprType)
         (void . lookAhead $ reservedOp "=") (-100) <* reservedOp "=" <?> "binding name"
  initializer <- pratt @(WithExpr m) (void . lookAhead $ reserved "in") (-100) <* reserved "in"
  body <- pratt @(WithExpr m) end (-100)
  return . ExStc . inj $ Let pat initializer body

-- | lambda block parser
bigLambda :: forall m err. (ShowErrorComponent err, MonadParsec err Text m, MonadFail m, ReaderM m) => m ParseLambdaType
bigLambda = do
  let iPattern = Pattern <$> pratt @(WithPattern m (WithExpr m) ParseExprType)
                 (void . lookAhead . foldl1 (<|>) $ reservedOp <$> [",", "=", "|"] ) (-100)
      groupPat = fmap PatGrp $ iPattern `sepBy1` reservedOp ","
      seqPat = fmap PatSeq $ groupPat `sepBy1` reservedOp "|"
      branch = (,) <$> seqPat <*> (reservedOp "=" *> pratt @(WithExpr m) (void . lookAhead $ reservedOp "]" <|> reservedOp "|" ) (-100))
  header <- Lambda . Bounds . fromMaybe [] <$> optional (try $ ((:> TypPht) . Symbol <$> identifier) `manyTill`  reservedOp ";;")
  lambda <-
    try ((`header` []) . (PatGrp [],) <$> pratt @(WithExpr m) (void . lookAhead $ reservedOp "]") (-100))
    <|> header <$> branch <*> (reservedOp "|" *> branch `sepBy1` reservedOp "|" <|> return [])
  reservedOp "]" $> lambda

smallLambda :: forall m err. (ShowErrorComponent err, MonadParsec err Text m, MonadFail m, ReaderM m) => m () -> m ParseLambdaType
smallLambda end = do
  let iPattern = Pattern
             <$> pratt @(WithPattern m (WithExpr m) ParseExprType)
                (void . lookAhead . foldl1 (<|>) $ reservedOp <$> ["=>", ","] ) (-100)
      seqPat = fmap PatSeq $ iPattern <* reservedOp "," >>= \v -> (v:) <$> iPattern `sepBy1` reservedOp ","
      branch = (,) <$> ((try seqPat <|> iPattern) <* reservedOp "=>") <*> pratt @(WithExpr m) (lookAhead $ end <|> void (reservedOp ",")) (-100)
  Lambda (Bounds []) <$> branch <*> (reservedOp "," *> branch `sepBy1` reservedOp "," <|> return []) <* end

record, tup :: forall err m. (ShowErrorComponent err, MonadParsec err Text m, MonadFail m, ReaderM m) => m ParseExprType
record = do
  let rprefix m = optional (oneOf ['&', '.']) >>= maybe m (\a -> (a:) <$> m)
      rlabel = try (Symbol <$> rprefix identifier) <|> Op <$> operator
      field = (,) <$> (rlabel <* reservedOp "=") <*> pratt @(WithExpr m) (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100)
  ExPrm . inj . Record <$> field `sepBy1` reservedOp "," <* reservedOp "}"

tup =
  let field = pratt @(WithExpr m) (void . lookAhead $ reservedOp "," <|> reservedOp ")") (-100)
   in fmap (ExPrm . inj . Tuple) $ field <* reservedOp "," >>= \v -> (v:) <$> field `sepBy1` reservedOp "," <* reservedOp ")"

tunit :: forall err m. (ShowErrorComponent err, MonadParsec err Text m) => m ParseExprType
tunit = reservedOp ")" $> ExPrm (inj $ Tuple [])

getOperator :: (MonadFail m, HasReader "TermOperator" [Operator String] m)
            => String -> m (Operator String)
getOperator "\\" = return $ Operator Unifix 999 999 "\\"
getOperator ":" = return $ Operator Infix (-15) (-20) ":"
getOperator op = do
  res <- asks @"TermOperator" $ find (\(Operator _ _ _ n) -> n == op)
  case res of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined in expression term level."

