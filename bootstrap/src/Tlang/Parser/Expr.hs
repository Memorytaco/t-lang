module Tlang.Parser.Expr
  ( ExpressionToken
  , declaration
  , Parser
  , getParser
  , unParser
  , play

  , lambda
  )
where

import Tlang.Parser.Pratt
import Tlang.Parser.Lexer

import Tlang.AST

import Text.Megaparsec
import Data.Text (Text, pack)
import Control.Monad.Reader (ReaderT (..), ask, MonadReader, runReaderT)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.List (find)
import Data.Void (Void)

import qualified Tlang.Parser.Type as TypParser

newtype Parser e m a = Parser
  { unWrapParser :: ReaderT ([Operator String], [Operator String]) (ParsecT e Text m) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader ([Operator String], [Operator String]))
    deriving newtype (Alternative, MonadFail, MonadPlus)
deriving newtype instance ShowErrorComponent e => MonadParsec e Text (Parser e m)

type ExpressionToken = OperatorClass String (Expr Op (UnTypedName Op))

getParser :: Parser e m a -> ([Operator String], [Operator String]) -> ParsecT e Text m a
getParser = runReaderT . unWrapParser

liftParser :: ParsecT e Text m a -> Parser e m a
liftParser = Parser . ReaderT . const

unParser :: ShowErrorComponent e
         => ([Operator String], [Operator String])
         -> ParsecT e Text m ()
         -> Integer
         -> ParsecT e Text m (Expr Op (UnTypedName Op))
unParser r end rbp = getParser (pratt (liftParser end) rbp) r

play :: ([Operator String], [Operator String]) -> Text -> IO ()
play op txt = either errorBundlePretty show <$> runParserT (unParser @Void op eof (-100)) "stdin" txt >>= putStrLn 

instance (ShowErrorComponent e) => OperatorParser ExpressionToken (Parser e m) where
  type Expression ExpressionToken = (Expr Op (UnTypedName Op))
  next = try (OpNorm <$> variable <?> "Identifier")
     <|> (OpNorm <$> num <?> "Literal Number")
     <|> (OpNorm <$> str <?> "Literal String")
     <|> try (OpRep . Right <$> operator <?> "Expr Operator")
     <|> (OpRep . Left  <$> special <?> "Pair Operator")
    where
      variable = identifier >>= return . ExRef . UnTypedName TypInfer
      num = try floating <|> nat
      nat = integer >>= return . ExLit UnTyped . LitInt
      floating = float >>= return . ExLit UnTyped . LitNumber
      str = stringLiteral >>= return . ExLit UnTyped . LitString
      withSpecial v@(l, r) = (reservedOp (pack l) <|> reserved (pack l)) *> pure (Left v)
                         <|> (reservedOp (pack r) <|> reserved (pack r)) *> pure (Right v)
      special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("let", "in"), ("{", "}")]

  peek = lookAhead next

  getPower = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op >>= return . getBindPower

  nud end = withOperator return \case
    (Left (Left  (l, r))) -> case l of
      "let" -> letExpr (lookAhead end)
      "{" -> lambda >>= pure . ExLambda UnTyped
      _ -> do ntok <- peek
              matchPairOperator (Right ("(", ")")) ntok
                (next *> (return . ExLit UnTyped $ LitUnit)) $ pratt (reservedOp (pack r) *> pure ()) (-100)
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> show op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> show op
        _ -> do right <- pratt end r
                return $ ExOp op UnTyped right Nothing

  led end left = withOperator (return . ExBeta UnTyped left) \case
    (Left (Left  (l, r))) -> case l of
      "let" -> letExpr (lookAhead end)
      "{" -> lambda >>= pure . ExLambda UnTyped
      _ -> do ntok <- peek
              matchPairOperator (Right ("(", ")")) ntok
                (next *> (return . ExBeta UnTyped left . ExLit UnTyped $ LitUnit))
                $ pratt (reservedOp (pack r) *> pure ()) (-100) >>= return . ExBeta UnTyped left
    (Left (Right (l, r))) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    (Right s) -> getOperator s >>= \op@(Operator assoc _ r _) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> pratt end r >>= return . ExOp op UnTyped left . Just
        _ -> return $ ExOp op UnTyped left Nothing

-- | variable in "variable: typannotation" form, doesn't consume end
declaration :: ShowErrorComponent e => Parser e m () -> Parser e m (UnTypedName Op)
declaration end = do
  r <- ask
  let typParser e = liftParser . TypParser.unParser (fst r) (getParser e r)
  name <- identifier
  typ'maybe <- optional $ (reservedOp ":") *> typParser (lookAhead end) (-100)
  return $ UnTypedName (maybe TypInfer TypAnno typ'maybe) name

-- | let binding in expression
letExpr :: ShowErrorComponent e => Parser e m () -> Parser e m (Expr Op (UnTypedName Op))
letExpr end = do
  var <- declaration (reservedOp "=" *> pure ()) <?> "binding name"
  _ <- reservedOp "="
  initializer <- pratt (reserved "in" *> pure ()) (-100)
  body <- pratt end (-100)
  return $ ExBind UnTyped (var, initializer) body

-- | need a complete definition of pattern match syntax
patternExpr :: ShowErrorComponent e => Parser e m () -> Parser e m (Pattern (UnTypedName Op))
patternExpr end = wild <|> var
  where
    wild = reserved "_" *> pure PatWild
    var = declaration end >>= pure . PatVar

-- | lambda block parser
lambda :: ShowErrorComponent e => Parser e m (Lambda Op (UnTypedName Op))
lambda = do
  let branch = patternExpr ((reservedOp "," <|> reservedOp "=>") *> pure ()) `sepBy` (reservedOp ",")
  patts <- optional . try $ branch <* reservedOp "=>"
  body <- pratt (lookAhead $ reservedOp "}" *> pure ()) (-100)
  _ <- reservedOp "}"
  return $ Lambda [LambdaBranch (maybe [] id patts) body]

getOperator :: String
            -> Parser e m (Operator String)
getOperator op = do
  stat <- snd <$> ask
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined in term level."

