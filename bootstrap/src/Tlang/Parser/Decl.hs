module Tlang.Parser.Decl
  ( declaration
  , play
  , getFixity
  , ParseDeclType
  )
where

import Tlang.AST
import Tlang.Parser.Lexer
import qualified Tlang.Parser.Expr as ExprParser
import qualified Tlang.Parser.Type as TypeParser

import Control.Monad (void)
import Control.Monad.Trans.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity)
import Data.Bifunctor (first)
import Data.Functor ((<&>), ($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

type ParseDeclType c f = Declaration (TypeParser.ParseType c f) Symbol

defForeign :: ShowErrorComponent e => ([Operator String], [Operator String]) -> ParsecT e Text m (ParseDeclType None Identity)
defForeign r = do
  void $ reserved "foreign"
  name <- Symbol <$> identifier
  void $ reservedOp ":" <|> fail "fn declaration require type signature"
  sig <- TypeParser.unParser (fst r) (void . lookAhead . choice $ reservedOp <$> ["[", "=", ";;"]) (-100)
  next <- choice $ reservedOp <$> ["[", "=", ";;"]
  case next of
    ";;" -> return $ FnD name sig FnDefault
    "[" -> FnDecl <$> lambda <* reservedOp ";;" <&> FnD name sig
    "=" -> FnSymbol <$> stringLiteral <* reservedOp ";;" <&> FnD name sig
    _ -> error "impossible in Tlang.Parser.Decl.defForeign"
  where
    lambda = ExprParser.getParser ExprParser.bigLambda r

defLet :: ShowErrorComponent e => ([Operator String], [Operator String]) -> ParsecT e Text m (ParseDeclType None Identity)
defLet r = do
  void $ reserved "let"
  name <- Symbol <$> identifier <|> Op <$> operator
  sig'maybe <- optional $ reservedOp ":" *> TypeParser.unParser (fst r) (void . lookAhead $ reservedOp "=" <|> reservedOp ";;") (-100)
  void $ reservedOp "=" <|> fail "let declaration require a value definition"
  let expr = ExprParser.unParser r (void $ reservedOp ";;") (-100)
  case sig'maybe of
    Just typ -> LetD name . ExAnno . (:@ typ) <$> expr
    Nothing -> LetD name <$> expr

defTypAlias :: ShowErrorComponent e => ([Operator String], [Operator String]) -> ParsecT e Text m (ParseDeclType None Identity)
defTypAlias r = do
  void $ reserved "type"
  aliaId <- name
  vars <- typVarlist
  body <- typBody
  void $ reservedOp ";;"
  return (TypF . aliaId $ vars body)
  where
    typName = Symbol <$> identifier <|> do
      op <- lookAhead operator
      if head op == ':'
         then Op <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> op <> "\" ?"
    name = (:==) <$> typName
    typVar = identifier <&> TypAbs . (:> TypBot) . Symbol
    typVarlist = foldr (.) id <$> manyTill typVar (void $ reservedOp "=")
    typBody = TypeParser.unParser (fst r) (lookAhead . void $ reservedOp ";;") (-100)

defData :: ShowErrorComponent e => ([Operator String], [Operator String]) -> ParsecT e Text m (ParseDeclType None Identity)
defData r = do
  void $ reserved "data"
  dataId <- name
  vars <- typVarlist
  body <- typVariant <|> typRec <|> pure TypBot
  void $ reservedOp ";;"
  return (TypD . dataId $ vars body)
  where
    typName = Symbol <$> identifier <|> do
      op <- lookAhead operator
      if head op == ':'
         then Op <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> op <> "\" ?"
    name = (:==) <$> typName
    typVar = identifier <&> TypAbs . (:> TypBot) . Symbol
    typVarlist = foldr (.) id <$> manyTill typVar (void . lookAhead $ reservedOp "|" <|> reservedOp "{" <|> reservedOp ";;")
    typRec = reservedOp "{" *> TypeParser.getParser TypeParser.record (fst r)
    typVariant = TypSum <$> some (reservedOp "|" *> field)
      where
        fieldName = fmap Tlang.AST.Label $ identifier <|> operator
        field = (,) <$> fieldName <*> optional
          (reservedOp ":" *> TypeParser.unParser (fst r) (void . lookAhead $ reservedOp "|" <|> reservedOp ";;") (-100))

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

declaration :: (ShowErrorComponent e, Monad m) => ParsecT e Text (StateT ([Operator String], [Operator String]) m) (ParseDeclType None Identity)
declaration = do
  r <- lift get
  defForeign r <|> defLet r <|> defTypAlias r <|> defData r <|> do
    op@(Operator _ _ _ s) <- defFixity
    lift . modify $ \(tys, trs) -> if head s == ':' then (op:tys, trs) else (tys, op:trs)
    void $ reservedOp ";;"
    return $ FixD op

-- | get operator fixity information from source file
getFixity :: [ParseDeclType None Identity] -> [Operator String]
getFixity = collect . fmap pick
  where
    pick (FixD op) = Just op
    pick _ = Nothing
    collect = foldl (\ls b -> maybe ls (: ls) b) []

play :: (Monad f, Monad m)
     => (ParsecT Void Text (StateT ([Operator String], [Operator String]) m) (ParseDeclType None Identity) -> ParsecT Void Text (StateT s f) c)
     -> s -> Text -> f (Either String c)
play deco op txt =
  let m = flip evalStateT op $ runParserT (deco $ declaration @Void) "stdin" txt
   in first errorBundlePretty <$> m
