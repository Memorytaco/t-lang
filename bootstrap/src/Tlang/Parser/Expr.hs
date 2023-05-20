module Tlang.Parser.Expr
  ( ExpressionToken
  , ParseExpr
  , ParseExprType
  , ParseLambda
  , ParseLambdaType

  , Parser
  , getParser
  , unParser
  , play
  , parseExpr

  , bigLambda
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
import Data.Functor (($>), (<&>))
import Data.Text (Text, pack, unpack)
import Data.List (find)
import Data.Void (Void)
import Data.Bifunctor (first)

import qualified Tlang.Parser.Type as TypParser
import qualified Tlang.Parser.Pattern as PatParser
import Tlang.Helper.AST.Type (injTypeLit)
import Tlang.Extension.Type (Tuple (..))

newtype Parser e m a = Parser
  { unWrapParser :: ReaderT ([Operator String], [Operator String]) (ParsecT e Text m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader ([Operator String], [Operator String]))
    deriving newtype (Alternative, MonadFail, MonadPlus)
deriving newtype instance ShowErrorComponent e => MonadParsec e Text (Parser e m)

type ParseExpr typ = Expr typ ((:@) typ) Symbol
type ParseExprType = ParseExpr TypParser.ParseType
type ParseLambda typ = Lambda typ ((:@) typ) Symbol
type ParseLambdaType = ParseLambda TypParser.ParseType

type ExpressionToken = OperatorClass String ParseExprType

getParser :: Parser e m a -> ([Operator String], [Operator String]) -> ParsecT e Text m a
getParser = runReaderT . unWrapParser

liftParser :: ParsecT e Text m a -> Parser e m a
liftParser = Parser . ReaderT . const

unParser :: ShowErrorComponent e
         => ([Operator String], [Operator String])
         -> ParsecT e Text m ()
         -> Integer
         -> ParsecT e Text m (ParseExprType)
unParser r end rbp = getParser (pratt (liftParser end) rbp) r

play :: Monad m => ([Operator String], [Operator String]) -> Text -> m (Either String (ParseExprType))
play op txt = first errorBundlePretty <$> runParserT (unParser @Void op eof (-100)) "stdin" txt

parseExpr :: Monad m => ([Operator String], [Operator String]) -> Text -> m (Either String (ParseExprType))
parseExpr op txt = first errorBundlePretty
  <$> runParserT (unParser @Void op eof (-100)) "stdin" txt

instance (ShowErrorComponent e) => OperatorParser ExpressionToken (Parser e m) where
  type Expression ExpressionToken = ParseExprType
  next = (OpNorm <$> try variable <?> "Identifier, Label and Selector")
     <|> (OpNorm <$> literal <?> "Literal")
     <|> (OpNorm <$> lexeme typ <?> "Type literal")
     <|> (OpRep . Left  <$> special <?> "Pair Operator")
     <|> (OpRep . Right <$> (sym <|> operator) <?> "Operator")
    where
      nat = ExLit . LitInt <$> integer <?> "Natural Number"
      floating = ExLit . LitNumber <$> float <?> "Floating Number"
      str = ExLit . LitString <$> stringLiteral <?> "Literal String"
      literal = try floating <|> nat <|> str

      name = ExRef . Symbol <$> identifier
      vlabel = char '`' *> identifier <&> flip ExVar Nothing . Symbol
      field = char '.' *> identifier <&> ExSel . Symbol
      variable = name <|> vlabel <|> field

      withSpecial v@(l, r) = (reservedOp (pack l) <|> reserved (pack l)) $> Left v
                         <|> (reservedOp (pack r) <|> reserved (pack r)) $> Right v
      special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("let", "in"), ("{", "}"), ("[", "]")]
      sym = reservedOp ":" <|> reservedOp "\\" <&> unpack
      typ = string "@()" $> ExTyp (injTypeLit (Tuple []))
        <|> string "@(" *> (ExTyp <$> pType (void . lookAhead $ reservedOp ")") (-100) <* reservedOp ")")
        <|> string "@{" *> (asks fst >>= liftParser . TypParser.getParser TypParser.record <&> ExTyp)
        <|> string "@" *> (ExTyp . TypRef . Symbol <$> identifier)

  peek = lookAhead next

  getPower = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op <&> getBindPower

  nud end = withOperator return \case
    (Left (Left  (l, r))) -> case l of
      "let" -> letExpr (lookAhead end)
      "{" -> record
      "[" -> ExAbs <$> bigLambda
      _ -> tunit <|> try tup <|> pratt (void . lookAhead $ reservedOp (pack r)) (-100) <* reservedOp (pack r)
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> show op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> show op
        _ -> case s of
          "\\" -> ExAbs <$> smallLambda end
          _ -> do right <- pratt end r
                  return $ ExApp (ExRef $ Op s) right []

  -- led end left = withOperator (return . ExApp left) \case
  led end left = withOperator (handleNorm left) \case
    (Left (Left  (l, r))) -> case l of
      "let" -> letExpr (lookAhead end)
      "{" -> record <&> apply left
      "[" -> bigLambda <&> apply left . ExAbs
      _ -> do right <- tunit <|> try tup <|> pratt (void . lookAhead $ reservedOp (pack r)) (-100) <* reservedOp (pack r)
              return $ apply left right
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \(Operator assoc _ r _) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
            ":" -> ExAnno . (left :@) <$> pType end (-100)
            _ -> pratt end r <&> ExApp (ExRef $ Op s) left . (:[])
        _ -> case s of
            "\\" -> apply left . ExAbs <$> smallLambda end
            _ -> return $ ExApp (ExRef $ Op s) left []
    where
      apply (ExApp l1 l2 rs) r = ExApp l1 l2 (rs <> [r])
      apply (ExVar s Nothing) r = ExVar s (Just r)
      apply l r = ExApp l r []

-- | special case for application
handleNorm
  :: Monad m => Expr typ anno name -> Expr typ anno name -> m (Expr typ anno name)
handleNorm (ExVar s Nothing) r = return $ ExVar s (Just r)
handleNorm (ExApp l1 l2 rs) r = return $ ExApp l1 l2 (rs <> [r])
handleNorm a r = return $ ExApp a r []

-- | need a complete definition of pattern match syntax
pPattern :: ShowErrorComponent e => Parser e m () -> Integer -> Parser e m (PatParser.ParsePatternType)
pPattern end r = ask >>= \e -> liftParser $ PatParser.unParser e (getParser end e) r
pType :: ShowErrorComponent e => Parser e m () -> Integer -> Parser e m TypParser.ParseType
pType end r = ask >>= \e -> liftParser $ TypParser.unParser (fst e) (getParser end e) r

-- | let binding in expression
letExpr :: ShowErrorComponent e => Parser e m () -> Parser e m (ParseExprType)
letExpr end = do
  pat <- pPattern (void . lookAhead $ reservedOp "=") (-100) <* reservedOp "=" <?> "binding name"
  initializer <- pratt (void . lookAhead $ reserved "in") (-100) <* reserved "in"
  body <- pratt end (-100)
  return $ ExLet pat initializer body

-- | lambda block parser
bigLambda :: ShowErrorComponent e => Parser e m ParseLambdaType
bigLambda = do
  let iPattern = Pattern <$> pPattern (void . lookAhead . foldl1 (<|>) $ reservedOp <$> [",", "=", "|"] ) (-100)
      groupPat = fmap PatGrp $ iPattern `sepBy1` reservedOp ","
      seqPat = fmap PatSeq $ groupPat `sepBy1` reservedOp "|"
      branch = (,) <$> seqPat <*> (reservedOp "=" *> pratt (void . lookAhead $ reservedOp "]" <|> reservedOp "|" ) (-100))
  lambda <-
    try ((flip (Lambda []) []) . (PatGrp [],) <$> pratt (void . lookAhead $ reservedOp "]") (-100))
    <|> Lambda [] <$> branch <*> (reservedOp "|" *> branch `sepBy1` reservedOp "|" <|> return [])
  void $ reservedOp "]"
  return lambda
smallLambda :: ShowErrorComponent e => Parser e m () -> Parser e m ParseLambdaType
smallLambda end = do
  let iPattern = Pattern <$> pPattern (void . lookAhead . foldl1 (<|>) $ reservedOp <$> ["=>", ","] ) (-100)
      seqPat = fmap PatSeq $ iPattern <* reservedOp "," >>= \v -> (v:) <$> iPattern `sepBy1` reservedOp ","
      branch = (,) <$> ((try seqPat <|> iPattern) <* reservedOp "=>") <*> pratt (lookAhead $ end <|> void (reservedOp ",")) (-100)
  Lambda [] <$> branch <*> (reservedOp "," *> branch `sepBy1` reservedOp "," <|> return []) <* end

record, tup, tunit :: ShowErrorComponent e => Parser e m (ParseExprType)
record = do
  let rprefix :: ShowErrorComponent e => Parser e m String -> Parser e m String
      rprefix m = optional (oneOf ['&', '.']) >>= maybe m (\a -> (a:) <$> m)
      rlabel = try (Symbol <$> rprefix identifier) <|> Op <$> operator
      field = (,) <$> (rlabel <* reservedOp "=") <*> pratt (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100)
      -- rowof = RowOf <$> (reservedOp "..." *> pratt (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100))
  ExRec <$> field `sepBy1` reservedOp "," <* reservedOp "}"

tup =
  let field = pratt (void . lookAhead $ reservedOp "," <|> reservedOp ")") (-100)
   in fmap ExTup $ field <* reservedOp "," >>= \v -> (v:) <$> field `sepBy1` reservedOp "," <* reservedOp ")"

tunit = reservedOp ")" $> ExUnit

getOperator :: String
            -> Parser e m (Operator String)
getOperator "\\" = return $ Operator Unifix 999 999 "\\"
getOperator ":" = return $ Operator Infix (-15) (-20) ":"
getOperator op = do
  stat <- asks snd
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined in term level."

