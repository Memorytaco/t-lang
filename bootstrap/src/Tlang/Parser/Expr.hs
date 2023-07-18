module Tlang.Parser.Expr
  ( WithExpr
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST
import Tlang.Constraint (Prefix (..), Prefixes (..))

import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Debug
import Text.Megaparsec.Char (char)
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import Data.List (find)
import qualified Data.Kind as Kind (Type)
import Data.Maybe (fromMaybe)
import Tlang.Parser.Type (record)

import Capability.Reader (HasReader, asks)

import Tlang.Extension
import Tlang.Generic ((:<:) (..))

data WithExpr (e :: Kind.Type) (m :: Kind.Type -> Kind.Type) (a :: k)
type ExprC e m = (MonadFail m, ShowErrorComponent e, MonadParsec e Text m)

cons :: (Apply :<: f) => Expr f name -> Expr f name -> Expr f name
cons x@(Expr p) v = case prj @Apply p of
                     Just (Apply a b as) -> Expr . inj $ Apply a b (as <> [v])
                     Nothing -> Expr . inj $ Apply x v []
cons a b = Expr . inj $ Apply a b []

literal :: (Apply :<: f, Monad m) => Expr f name -> Semantic m (Expr f name)
literal a = Semantic (const $ return a) (\_ left -> return $ cons left a) (return Infinite)

-- * compound syntax

instance ( PrattToken (WithExpr e m a) (Expr f name) m
         , PrattToken (WithExpr e m b) (Expr f name) m
         , ExprC e m
         )
  => PrattToken (WithExpr e m (a :- b)) (Expr f name) m where
  tokenize _ parser end = try (tokenize (Proxy @(WithExpr e m a)) parser end)
                      <|> tokenize (Proxy @(WithExpr e m b)) parser end

instance ( PrattToken (WithExpr e m a) (Expr f name) m
         , KnownSymbol msg, ExprC e m, MonadParsecDbg e Text m)
  => PrattToken (WithExpr e m (msg ?- a)) (Expr f name) m where
  tokenize _ parser end = dbg' (symbolVal $ Proxy @msg)
                        $ tokenize (Proxy @(WithExpr e m a)) parser end

instance ( ParserDSL (WithExpr e m a) (Expr f name) m
         , KnownSymbol msg, ExprC e m, MonadParsecDbg e Text m)
  => ParserDSL (WithExpr e m (msg ?- a)) (Expr f name) m where
  syntax _ end = dbg' (symbolVal $ Proxy @msg)
               $ syntax (Proxy @(WithExpr e m a)) end

instance ( ParserDSL (WithExpr e m (msg ?- a)) (Expr f name) m
         , KnownSymbol msg, ExprC e m, MonadParsecDbg e Text m)
  => ParserDSL (msg ?- WithExpr e m a) (Expr f name) m where
  syntax _ end = dbg' (symbolVal $ Proxy @msg)
               $ syntax (Proxy @(WithExpr e m (msg ?- a))) end

-- | expression identifier
instance (ExprC e m, name ~ Name, Apply :<: f)
  => PrattToken (WithExpr e m "identifier") (Expr f name) m where
  tokenize _ _ _ = identifier <&> Val . Name <&> literal

-- | literal text
instance (ExprC e m, Apply :<: f, LiteralText :<: f)
  => PrattToken (WithExpr e m "text") (Expr f name) m where
  tokenize _ _ _ = stringLiteral <&> Expr . inj . LiteralText . Literal <&> literal

-- | literal integer
instance (ExprC e m, Apply :<: f, LiteralInteger :<: f)
  => PrattToken (WithExpr e m "integer") (Expr f name) m where
  tokenize _ _ _ = integer <&> Expr . inj . LiteralInteger . Literal <&> literal

-- | literal floating
instance (ExprC e m, Apply :<: f, LiteralNumber :<: f)
  => PrattToken (WithExpr e m "number") (Expr f name) m where
  tokenize _ _ _ = float <&> Expr . inj . LiteralNumber . Literal <&> literal

-- | operators for expression
instance (ExprC e m, Name ~ name, Apply :<: f, HasReader "TermOperator" [Operator Text] m)
  => PrattToken (WithExpr e m "operator") (Expr f name) m where
  tokenize _ parser _ = do
    pos <- getOffset
    op <- operator
    Operator fixity l r _ <- asks @"TermOperator" (find (\(Operator _ _ _ n) -> n == op)) >>= \case
      Just a -> return a
      Nothing -> setOffset pos >> do fail $ "Operator is not defined in term level: " <> show op
    let nud' end =
          if fixity `elem` [Prefix, Unifix, PreInfix, UniInfix]
             then parser end (Power r) <&> cons (Val $ Name op)
             else fail $ "Wrong position of " <> show op <> ": it has fixity " <> show fixity <> " but expect Prefix or Unifix"
        led' end left =
          case fixity of
            Infix -> parser end (Power r) <&> Expr . inj . Apply (Val $ Name op) left . pure
            PreInfix -> parser end (Power r) <&> Expr . inj . Apply (Val $ Name op) left . pure
            Unifix -> return (cons (Val $ Name op) left)
            Postfix -> return (cons (Val $ Name op) left)
            PostInfix -> do
              right'maybe <- optional $ parser end (Power r)
              case right'maybe of
                Nothing -> return (cons (Val $ Name op) left)
                Just right -> return . Expr . inj $ Apply (Val $ Name op) left [right]
            UniInfix -> do
              right'maybe <- optional $ parser end (Power r)
              case right'maybe of
                Nothing -> return (cons (Val $ Name op) left)
                Just right -> return . Expr . inj $ Apply (Val $ Name op) left [right]
            _ -> fail $ "Wrong position of " <> show op <> ": it has fixity " <> show fixity <> " but expect Infix, Postfix or Unifix"
    return (Semantic nud' led' (return $ Power l))

-- | group operator for expression
instance (ExprC e m, Apply :<: f)
  => PrattToken (WithExpr e m "group") (Expr f name) m where
  tokenize _ parser _ = parens (parser (lookAhead $ reservedOp ")" $> ()) Go) <&> literal

-- | tuple expression
instance (ExprC e m, Apply :<: f, Tuple :<: f)
  => PrattToken (WithExpr e m "tuple") (Expr f name) m where
  tokenize _ parser _ = do
    fields <- parens $ parser (void . lookAhead $ reservedOp "," <|> reservedOp ")") Go `sepBy` reservedOp ","
    return $ literal (Expr . inj $ Tuple fields)

-- | record expression
instance (ExprC e m, Apply :<: f, Record Label :<: f)
  => PrattToken (WithExpr e m "record") (Expr f name) m where
  tokenize _ parser _ = do
    let field = (,) <$> fmap Label identifier <* reservedOp "=" <*> parser (void . lookAhead $ reservedOp "," <|> reservedOp "}") Go
    fields <- braces (field `sepBy` reservedOp ",")
    return $ literal (Expr . inj $ Record fields)

-- | selector expression
instance (ExprC e m, Apply :<: f, Selector Label :<: f)
  => PrattToken (WithExpr e m "selector") (Expr f name) m where
  tokenize _ _ _ = char '.' *> identifier <&> Expr . inj . Selector . Label <&> literal

-- | variant constructor
instance (ExprC e m, Apply :<: f, Constructor Label :<: f)
  => PrattToken (WithExpr e m "constructor") (Expr f name) m where
  tokenize _ parser _ = do
    let item = do
          var <- identifier' <* symbol "|" <&> Label <?> "Expr: constructor label"
          fields <- parser (void . lookAhead $ reservedOp "," <|> reservedOp "]") Go `sepBy` reservedOp ","
                <?> "Expr: constructor fields"
          return (Expr . inj $ Constructor var fields)
    variant <- (char '[' *> item <* symbol "]" <?> "Expr: constructor syntax")
           <|> (char '`' >> fmap (Expr . inj) . Constructor . Label <$> identifier <*> pure [])
    return $ literal variant

-- | type annotation
instance (ExprC e m, (@:) typ :<: f, PrattToken proxy typ m)
  => PrattToken (WithExpr e m (Layer "annotation" proxy typ)) (Expr f name) m where
  tokenize _ _ _ = reservedOp ":" $> Semantic nud' led' (return $ BuiltinL 1)
    where
      nud' _ = fail "Type annotation expect an expression first"
      led' end left = do
        typ :: typ <- pratt @proxy end Go
        return . Expr . inj $ left :@ typ

-- | let expression
instance (ExprC e m, Apply :<: f, Let pat :<: f, PrattToken proxy (pat (Expr f name)) m)
  => PrattToken (WithExpr e m (Layer "let" proxy (Hint pat))) (Expr f name) m where
  tokenize _ parser end = do
    pat :: pat (Expr f name) <-
      reserved "let" >> pratt @proxy (lookAhead (reservedOp "=") $> ()) Go <* reservedOp "=" <?> "Let Expression Head"
    value <- parser (lookAhead (reserved "in") $> ()) Go <* reserved "in"
    ret <- parser end Go
    return . literal . Expr . inj $ Let pat value ret

-- | block lambda for expression
instance ( ExprC e m, Apply :<: f
         , PatGroup :<: pext
         , PrattToken proxy (Pattern plit pext plabel pname (Expr f name)) m
         , Equation (Pattern plit pext plabel pname) (Prefixes Name (Type tbind trep tname a)) :<: f
         )
  => PrattToken (WithExpr e m (Layer ("block" :- Type tbind trep tname a) proxy (Hint (Pattern plit pext plabel pname))))
                (Expr f name) m where
  tokenize _ parser _ = do
    let iPat = pratt @proxy @(Pattern plit pext plabel pname (Expr f name))
                     (void . lookAhead . choice $ reservedOp <$> [",", "=", "|"]) Go
        gPat = iPat `sepBy1` reservedOp "," <&> PatExt . inj . PatGrp <?> "group pattern"
        sPat = gPat `sepBy1` reservedOp "|" <&> PatExt . inj . PatSeq <?> "sequence pattern" <|> fail "expect a pattern definition"
        branch = (,) <$> sPat <*> (reservedOp "=" *> parser (void . lookAhead $ reservedOp "]" <|> reservedOp "|") Go)
             <|> fail "expect equation"
        lambda = do
          heads <- Equation . Prefixes . fromMaybe [] <$> optional
            (try $ ((:> (TypPht :: Type tbind trep tname a)) . Name <$> identifier) `manyTill`  reservedOp ";;")
          heads <$> branch <*> (reservedOp "|" *> branch `sepBy1` reservedOp "|" <|> return [])
    brackets (lambda <&> Expr . inj <?> "block lambda") <&> literal

-- | one line lambda for expression
instance ( ExprC e m, Apply :<: f
         , PrattToken proxy (pat (Expr f name)) m
         , Equation (Grp pat) (Prefixes Name (Type tbind trep tname a)) :<: f
         )
  => PrattToken (WithExpr e m (Layer ("line" :- Type tbind trep tname a) proxy (Hint (Grp pat))))
                (Expr f name) m where
  tokenize _ parser end = do
    let iPat = pratt @proxy @(pat (Expr f name))
                     (void . lookAhead . choice $ reservedOp <$> [",", "="]) Go
               <?> "Lambda Parameter pattern"
        gPat = iPat `sepBy1` reservedOp "," <&> Grp
        branch = (,) <$> gPat <*> (reservedOp "=" *> parser (void $ lookAhead end) Go)
        lambda = do
          heads <- Equation . Prefixes . fromMaybe [] <$> optional
            (try $ ((:> (TypPht :: Type tbind trep tname a)) . Name <$> identifier) `manyTill`  reservedOp ";;")
          heads <$> branch <*> return []
    reservedOp "\\" *> (lambda <&> Expr . inj) <&> literal

-- | type application
instance ( ExprC e m
         , PrattToken proxy (Type tbind trep tname a) m
         , Value (Type tbind trep tname a) :<: f
         , Apply :<: f
         , a ~ Name, Tuple :<: trep, LiteralText :<: trep, LiteralNatural :<: trep
         , Record Label :<: trep
         )
  => PrattToken (WithExpr e m (Layer "@type" proxy (Type tbind trep tname a)))
                (Expr f name) m where
  tokenize _ _ _ = char '@' >> do
    typ <- parens (pratt @proxy @(Type tbind trep tname a) (lookAhead (reservedOp ")") $> ()) Go)
      <|> TypVar . Name <$> identifier
      <|> Type . inj . Tuple <$> parens
            (pratt @proxy (lookAhead (reservedOp "," <|> reservedOp ")") $> ()) Go `sepBy` reservedOp ",")   -- tuple
      <|> Type . inj . LiteralNatural . Literal <$> integer -- integer
      <|> Type . inj . LiteralText . Literal <$> stringLiteral -- text
      <|> Type . inj <$> record (pratt @proxy)
    let nud' _ = fail "Type application requires one argument"
        led' _ left = return . Expr . inj $ Apply left (Expr . inj $ Value typ) []
    return $ Semantic nud' led' (return Infinite)

