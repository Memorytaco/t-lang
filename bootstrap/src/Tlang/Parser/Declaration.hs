module Tlang.Parser.Declaration
  ( declaration
  , playDef
  , getFixity
  )
where

import Tlang.AST
import Tlang.Parser.Lexer
import qualified Tlang.Parser.Expr as ExprParser
import qualified Tlang.Parser.Type as TypeParser

import Text.Megaparsec
import Data.Text (Text)
import Control.Monad (void)
import Control.Monad.Trans.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans (lift)
import Data.Void (Void)
import Data.Bifunctor (first)

decName :: ShowErrorComponent e => [Op] -> ParsecT e Text m () -> ParsecT e Text m (UnTypedName Op)
decName r (lookAhead -> end) = do
  name <- identifier <|> operator
  typ'maybe <- optional $ reservedOp ":" *> TypeParser.unParser r end (-100)
  pure $ UnTypedName (maybe TypInfer TypAnno typ'maybe) name

defFn :: ShowErrorComponent e => ([Op], [Op]) -> ParsecT e Text m (Declaration Op (TypName Op) (UnTypedName Op))
defFn r = do
  void $ reserved "fn"
  name <- decName (fst r) . void $ reservedOp "{" <|> reservedOp ";;"
  body'maybe <- optional $ (void $ reservedOp "{") *> lambda
  void $ reservedOp ";;"
  return $ FnD name body'maybe
  where
    lambda = ExprParser.getParser ExprParser.lambda r

defLet :: ShowErrorComponent e => ([Op], [Op]) -> ParsecT e Text m (Declaration Op (TypName Op) (UnTypedName Op))
defLet r = do
  void $ reserved "let"
  name <- decName (fst r) . void $ reservedOp "="
  void $ reservedOp "="
  LetD name <$> ExprParser.unParser r (void $ reservedOp ";;") (-100)

defTyp :: ShowErrorComponent e => ([Op], [Op]) -> ParsecT e Text m (Declaration Op (TypName Op) (UnTypedName Op))
defTyp r = do
  void $ reserved "data"
  (try norm <|> phantom) <* reservedOp ";;"
  where
    typName = TypName <$> identifier
    typOpName = do
      op <- lookAhead operator
      if head op == ':'
         then TypName <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> op <> "\" ?"
    name = TypAs <$> typName <|> TypOpAs <$> typOpName
    typVar = typName >>= return . TypAbs
    typVarlist = foldr (.) id <$> manyTill typVar (void $ reservedOp "=" <|> lookAhead (reservedOp ";;"))
    typFull = TypeParser.unParser (fst r) (lookAhead . void $ reservedOp ";;") (-100)
    typVariant :: ShowErrorComponent e => ParsecT e Text m (Type (TypName Op) ())
    typVariant = TypeParser.getParser (TypeParser.dataVariant . lookAhead . void $ reservedOp ";;") (fst r)
    typBody = try typVariant <|> typFull

    phantom = fmap TypD $ name <*> (typVarlist <*> return TypBottom)
    norm = fmap TypD $ name <*> (typVarlist <*> typBody)

defFixity :: ShowErrorComponent e => ParsecT e Text m (Operator String)
defFixity = defUnifix <|> defInfix
  where
    precedence = do
      fixity <- integer
      if fixity < 0 || fixity > 10
         then fail "precedence number is restricted from 0 to 10, including 0 and 10"
         else pure (fixity * 10)

    defUnifix = reserved "unifix" *> ((,) <$> precedence <*> precedence >>= \(lbp, rbp) -> Operator Unifix lbp rbp <$> operator)
            <|> reserved "prefix" *> (precedence >>= \bp -> Operator Prefix bp bp <$> operator)
            <|> reserved "postfix" *> (precedence >>= \bp -> Operator Postfix bp bp <$> operator)

    defInfix =  reserved "infixl" *> (precedence >>= \bp -> Operator Infix (bp-1) bp <$> operator)
            <|> reserved "infixr" *> (precedence >>= \bp -> Operator Infix bp (bp-1) <$> operator)
            <|> reserved "infix" *> (precedence >>= \bp -> Operator Infix bp bp <$> operator)

declaration :: (ShowErrorComponent e, Monad m) => ParsecT e Text (StateT ([Op], [Op]) m) (Declaration Op (TypName Op) (UnTypedName Op))
declaration = do
  r <- lift get
  defFn r <|> defLet r <|> defTyp r <|> do
    op@(Operator _ _ _ s) <- defFixity
    lift . modify $ \(tys, trs) -> if head s == ':' then (op:tys, trs) else (tys, op:trs)
    void $ reservedOp ";;"
    return $ FixD op

-- | get operator fixity information from source file
getFixity :: [Declaration Op (TypName Op) info] -> [Operator String]
getFixity = collect . fmap pick
  where
    pick (FixD op) = Just op
    pick _ = Nothing
    collect = foldl (\ls b -> maybe ls (: ls) b) []

-- playDef :: (Show a) => (ParsecT Void Text IO (Declaration Op (UnTypedName Op)) -> ParsecT Void Text IO a) -> ([Op], [Op]) -> Text -> IO ()
playDef deco op txt =
  let m = flip evalStateT op $ runParserT (deco $ declaration @Void) "stdin" txt
   in first errorBundlePretty <$> m
