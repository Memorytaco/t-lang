module Tlang.Parser.Pattern
  (
    -- ** pattern parser handler
    WithPattern
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST

import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Debug (MonadParsecDbg (..), dbg')
import Data.Text (Text)
import Data.Functor (($>), (<&>))
import qualified Data.Kind as Kind (Type)
import Data.List (find)
import Tlang.Generic ((:<:) (..))
import Tlang.Extension (LiteralText (..), LiteralInteger (..), LiteralNumber (..), Literal (..))
import Capability.Reader (HasReader, asks)

-- | the `a` is where to define and use syntax
data WithPattern (e :: Kind.Type) (m :: Kind.Type -> Kind.Type) (a :: k)
type PatternC e m = (MonadFail m, ShowErrorComponent e, MonadParsec e Text m)

cons :: MonadFail m
     => Pattern lit ext label name expr -> Pattern lit ext label name expr -> m (Pattern lit ext label name expr)
cons (PatSym a as) v = return $ PatSym a (as <> [v])
cons _ _ = fail "Expect a variant symbol on lhs"

-- | a specialised `return` for Semantic
literal :: MonadFail m => Pattern lit ext label name expr -> Semantic m (Pattern lit ext label name expr)
literal a = Semantic (const $ return a) (\_ left -> cons left a) (return Infinite)

-- * compound syntax

instance ( PrattToken (WithPattern e m a) (Pattern lit ext label name expr) m
         , PrattToken (WithPattern e m as) (Pattern lit ext label name expr) m
         , PatternC e m
         )
  => PrattToken (WithPattern e m (a :- as)) (Pattern lit ext label name expr) m where
  tokenize _ parser end = try (tokenize (Proxy @(WithPattern e m a)) parser end)
                      <|> tokenize (Proxy @(WithPattern e m as)) parser end

instance ( PrattToken (WithPattern e m a) (Pattern lit ext label name expr) m
         , KnownSymbol msg, PatternC e m, MonadParsecDbg e Text m)
  => PrattToken (WithPattern e m (msg ?- a)) (Pattern lit ext label name expr) m where
  tokenize _ parser end = dbg' (symbolVal $ Proxy @msg)
                        $ tokenize (Proxy @(WithPattern e m a)) parser end

instance ( PrattToken (WithPattern e m a) (Pattern lit ext label name expr) m
         , KnownSymbol msg, PatternC e m, MonadParsecDbg e Text m)
  => PrattToken (msg ?- WithPattern e m a) (Pattern lit ext label name expr) m where
  tokenize _ parser end = dbg' (symbolVal $ Proxy @msg)
                        $ tokenize (Proxy @(WithPattern e m (msg ?- a))) parser end

instance ( ParserDSL (WithPattern e m a) (Pattern lit ext label name expr) m
         , KnownSymbol msg, PatternC e m, MonadParsecDbg e Text m)
  => ParserDSL (WithPattern e m (msg ?- a)) (Pattern lit ext label name expr) m where
  syntax _ end = dbg' (symbolVal $ Proxy @msg)
               $ syntax (Proxy @(WithPattern e m a)) end

-- | wild pattern
instance PatternC e m
  => PrattToken (WithPattern e m "wild") (Pattern lit ext label name expr) m where
  tokenize _ _ _ = reserved "_" $> literal PatWild <?> "Wild pattern"

-- | variable binding pattern
instance (PatternC e m, name ~ Name)
  => PrattToken (WithPattern e m "variable") (Pattern lit ext label name expr) m where
  tokenize _ _ _ = do
    var <- (char '?' <|> char '!') *> identifier <&> PatVar . Name
    return $ literal var

-- | group operator for pattern
instance PatternC e m
  => PrattToken (WithPattern e m "group") (Pattern lit ext label name expr) m where
  tokenize _ parser _ = parens (parser (lookAhead $ reservedOp ")" $> ()) Go) <&> literal

-- | symbol constructor
instance (PatternC e m, label ~ Label)
  => PrattToken (WithPattern e m "variant") (Pattern lit ext label name expr) m where
  tokenize _ _ _ = identifier <&> literal . flip PatSym [] . Label  <?> "Constructor pattern"

-- | operator symbol constructor
instance (PatternC e m, label ~ Label, HasReader "PatternOperator" [Operator Text] m)
  => PrattToken (WithPattern e m "operator") (Pattern lit ext label name expr) m where
  tokenize _ parser _ = do
    pos <- getOffset
    op <- operator
    Operator fixity l r _ <- asks @"PatternOperator" (find (\(Operator _ _ _ n) -> n == op)) >>= \case
      Just a -> return a
      Nothing -> setOffset pos >> do fail $ "Operator is not defined in term level: " <> show op
    let nud' end =
          if fixity `elem` [Prefix, Unifix]
             then parser end (Power r) <&> PatSym (Label op) . pure
             else fail $ "Wrong position of " <> show op <> ": it has fixity " <> show fixity <> " but expect Prefix or Unifix"
        led' end left =
          case fixity of
            Infix -> parser end (Power r) <&> PatSym (Label op) . (left:) . pure
            Unifix -> return (PatSym (Label op) [left])
            Postfix -> return (PatSym (Label op) [left])
            _ -> fail $ "Wrong position of " <> show op <> ": it has fixity " <> show fixity <> " but expect Infix, Postfix or Unifix"
    return (Semantic nud' led' (return $ Power l))

-- | `@` pattern
instance (PatternC e m, name ~ Name, label ~ Label, LiteralText :<: lit, LiteralNumber :<: lit, LiteralInteger :<: lit)
  => PrattToken (WithPattern e m "binding") (Pattern lit ext label name expr) m where
  tokenize _ parser _ = do
    var <- (char '?' <|> char '!') *> fmap Name identifier' <* char '@'
    pat <- parens (parser (lookAhead $ reservedOp ")" $> ()) Go)
       <|> reserved "_" $> PatWild  -- wild pattern
       <|> (char '?' <|> char '!') *> fmap (PatVar . Name) identifier  -- variable
       <|> PatSym . Label <$> identifier <*> return []  -- a constructor
       <|> (float <&> PatPrm . inj . LiteralNumber . Literal)
       <|> (integer <&> PatPrm . inj . LiteralInteger . Literal)
       <|> (stringLiteral <&> PatPrm . inj . LiteralText . Literal)
       <|> (fmap PatTup . parens $ sepBy (parser (lookAhead $ (reservedOp "," <|> reservedOp ")") $> ()) Go) (reservedOp ","))
    return $ literal (PatBind var pat)

-- | record pattern
instance (PatternC e m, label ~ Label)
  => PrattToken (WithPattern e m "record") (Pattern lit ext label name expr) m where
  tokenize _ parser _ = do
    let field = identifier <|> operator <&> Label
        pair = (,) <$> field <*> (reservedOp "=" *> parser (lookAhead $ (reservedOp "," <|> reservedOp "}") $> ()) Go)
    sections <- braces $ sepBy1 pair (reservedOp ",")
    return $ literal (PatRec sections)

-- | tuple pattern
instance (PatternC e m)
  => PrattToken (WithPattern e m "tuple") (Pattern lit ext label name expr) m where
  tokenize _ parser _ = do
    tup <- try (symbol "(" >> symbol ")" $> PatUnit)
       <|> (fmap PatTup . parens $ sepBy (parser (lookAhead $ (reservedOp "," <|> reservedOp ")") $> ()) Go) (reservedOp ","))
    return $ literal tup

-- | view pattern
instance (PatternC e m, ParserDSL proxy expr m)
  => PrattToken (WithPattern e m (Layer "view" proxy expr)) (Pattern lit ext label name expr) m where
  tokenize _ parser end = do
    e <- runDSL @proxy (lookAhead (reservedOp "->") $> ()) <* reservedOp "->"
    pat <- parser end Go
    return $ literal (PatView e pat)

-- ** extension for pattern

-- | type annotation for pattern
instance (PatternC e m, (@:) typ :<: ext, PrattToken proxy typ m)
  => PrattToken (WithPattern e m (Layer "annotation" proxy typ)) (Pattern lit ext label name expr) m where
  tokenize _ _ _ = reservedOp ":" $> Semantic nud' led' (return $ BuiltinL 1)
    where
      nud' _ = fail "Pattern annotation expect a pattern first"
      led' end left = do
        typ :: typ <- pratt @proxy end Go
        return . PatExt . inj $ left :@ typ
