module Tlang.Parser.Expr
  ( WithExpr
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST

import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Debug
import Text.Megaparsec.Char (char)
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.Text (Text, pack)
import Data.List (find)
import qualified Data.Kind as Kind (Type)
import Data.Maybe (fromMaybe)
import Tlang.Parser.Type (record)

import Capability.Reader (HasReader, asks)

import Tlang.Extension
import Tlang.Generic ((:<:) (..))

data WithExpr (e :: Kind.Type) (m :: Kind.Type -> Kind.Type) (a :: k)
type ExprC e m = (MonadFail m, ShowErrorComponent e, MonadParsec e Text m)

cons :: (Apply :<: struct) => Expr struct prim inj name -> Expr struct prim inj name -> Expr struct prim inj name
cons x@(ExStc p) v = case prj @Apply p of
                     Just (Apply a b as) -> ExStc . inj $ Apply a b (as <> [v])
                     Nothing -> ExStc . inj $ Apply x v []
cons a b = ExStc . inj $ Apply a b []

literal :: (Apply :<: struct, Monad m) => Expr struct prim inj name -> Semantic m (Expr struct prim inj name)
literal a = Semantic (const $ return a) (\_ left -> return $ cons left a) (return Infinite)

-- * compound syntax

instance ( PrattToken (WithExpr e m a) (Expr struct prim inj name) m
         , PrattToken (WithExpr e m b) (Expr struct prim inj name) m
         , ExprC e m
         )
  => PrattToken (WithExpr e m (a :- b)) (Expr struct prim inj name) m where
  tokenize _ parser end = try (tokenize (Proxy @(WithExpr e m a)) parser end)
                      <|> tokenize (Proxy @(WithExpr e m b)) parser end

instance ( PrattToken (WithExpr e m a) (Expr struct prim inj name) m
         , KnownSymbol msg, ExprC e m, MonadParsecDbg e Text m)
  => PrattToken (WithExpr e m (msg ?- a)) (Expr struct prim inj name) m where
  tokenize _ parser end = dbg' (symbolVal $ Proxy @msg)
                        $ tokenize (Proxy @(WithExpr e m a)) parser end

instance ( ParserDSL (WithExpr e m a) (Expr struct prim inj name) m
         , KnownSymbol msg, ExprC e m, MonadParsecDbg e Text m)
  => ParserDSL (WithExpr e m (msg ?- a)) (Expr struct prim inj name) m where
  syntax _ end = dbg' (symbolVal $ Proxy @msg)
               $ syntax (Proxy @(WithExpr e m a)) end

instance ( ParserDSL (WithExpr e m (msg ?- a)) (Expr struct prim inj name) m
         , KnownSymbol msg, ExprC e m, MonadParsecDbg e Text m)
  => ParserDSL (msg ?- WithExpr e m a) (Expr struct prim inj name) m where
  syntax _ end = dbg' (symbolVal $ Proxy @msg)
               $ syntax (Proxy @(WithExpr e m (msg ?- a))) end

-- | expression identifier
instance (ExprC e m, name ~ Symbol, Apply :<: struct)
  => PrattToken (WithExpr e m "identifier") (Expr struct prim inj name) m where
  tokenize _ _ _ = identifier <&> ExRef . Symbol <&> literal

-- | literal text
instance (ExprC e m, Apply :<: struct, LiteralText :<: prim)
  => PrattToken (WithExpr e m "text") (Expr struct prim inj name) m where
  tokenize _ _ _ = stringLiteral <&> ExPrm . inj . LiteralText . Literal . pack <&> literal

-- | literal integer
instance (ExprC e m, Apply :<: struct, LiteralInteger :<: prim)
  => PrattToken (WithExpr e m "integer") (Expr struct prim inj name) m where
  tokenize _ _ _ = integer <&> ExPrm . inj . LiteralInteger . Literal <&> literal

-- | literal floating
instance (ExprC e m, Apply :<: struct, LiteralNumber :<: prim)
  => PrattToken (WithExpr e m "number") (Expr struct prim inj name) m where
  tokenize _ _ _ = float <&> ExPrm . inj . LiteralNumber . Literal <&> literal

-- | operators for expression
instance (ExprC e m, Symbol ~ name, Apply :<: struct, HasReader "TermOperator" [Operator String] m)
  => PrattToken (WithExpr e m "operator") (Expr struct prim inj name) m where
  tokenize _ parser _ = do
    pos <- getOffset
    op <- operator
    Operator fixity l r _ <- asks @"TermOperator" (find (\(Operator _ _ _ n) -> n == op)) >>= \case
      Just a -> return a
      Nothing -> setOffset pos >> do fail $ "Operator is not defined in term level: " <> op
    let nud' end =
          if fixity `elem` [Prefix, Unifix]
             then parser end (Power r) <&> cons (ExRef $ Symbol op)
             else fail $ "Wrong position of " <> op <> " : it has fixity " <> show fixity <> " but expect Prefix or Unifix"
        led' end left =
          case fixity of
            Infix -> parser end (Power r) <&> ExStc . inj . Apply (ExRef $ Symbol op) left . pure
            Unifix -> return (cons (ExRef $ Symbol op) left)
            Postfix -> return (cons (ExRef $ Symbol op) left)
            _ -> fail $ "Wrong position of " <> op <> " : it has fixity " <> show fixity <> " but expect Infix, Postfix or Unifix"
    return (Semantic nud' led' (return $ Power l))

-- | group operator for expression
instance (ExprC e m, Apply :<: struct)
  => PrattToken (WithExpr e m "group") (Expr struct prim inj name) m where
  tokenize _ parser _ = parens (parser (lookAhead $ reservedOp ")" $> ()) Go) <&> literal

-- | tuple expression
instance (ExprC e m, Apply :<: struct, Tuple :<: prim)
  => PrattToken (WithExpr e m "tuple") (Expr struct prim inj name) m where
  tokenize _ parser _ = do
    fields <- parens $ parser (void . lookAhead $ reservedOp "," <|> reservedOp ")") Go `sepBy` reservedOp ","
    return $ literal (ExPrm . inj $ Tuple fields)

-- | record expression
instance (ExprC e m, Apply :<: struct, Record Label :<: prim)
  => PrattToken (WithExpr e m "record") (Expr struct prim inj name) m where
  tokenize _ parser _ = do
    let field = (,) <$> fmap Label identifier <* reservedOp "=" <*> parser (void . lookAhead $ reservedOp "," <|> reservedOp "}") Go
    fields <- braces (field `sepBy` reservedOp ",")
    return $ literal (ExPrm . inj $ Record fields)

-- | selector expression
instance (ExprC e m, Apply :<: struct, Selector Symbol :<: prim)
  => PrattToken (WithExpr e m "selector") (Expr struct prim inj name) m where
  tokenize _ _ _ = char '.' *> identifier <&> ExPrm . inj . Selector . Symbol <&> literal

-- | variant constructor
instance (ExprC e m, Apply :<: struct, Constructor Symbol :<: prim)
  => PrattToken (WithExpr e m "constructor") (Expr struct prim inj name) m where
  tokenize _ parser _ = do
    let item = do
          var <- identifier' <* symbol "|" <&> Symbol <?> "Expr: constructor label"
          fields <- parser (void . lookAhead $ reservedOp "," <|> reservedOp "]") Go `sepBy` reservedOp ","
                <?> "Expr: constructor fields"
          return (ExPrm . inj $ Constructor var fields)
    variant <- (char '[' *> item <* symbol "]" <?> "Expr: constructor syntax")
           <|> (char '`' >> fmap (ExPrm . inj) . Constructor . Symbol <$> identifier <*> pure [])
    return $ literal variant

-- | type annotation
instance (ExprC e m, (:@) typ :<: inj, PrattToken proxy typ m)
  => PrattToken (WithExpr e m (Layer "annotation" proxy typ)) (Expr struct prim inj name) m where
  tokenize _ _ _ = reservedOp ":" $> Semantic nud' led' (return $ BuiltinL 1)
    where
      nud' _ = fail "Type annotation expect an expression first"
      led' end left = do
        typ :: typ <- pratt @proxy end Go
        return . ExInj . inj $ left :@ typ

-- | let expression
instance (ExprC e m, Apply :<: struct, Let pat :<: struct, PrattToken proxy (pat (Expr struct prim inj name)) m)
  => PrattToken (WithExpr e m (Layer "let" proxy (Hint pat))) (Expr struct prim inj name) m where
  tokenize _ parser end = do
    pat :: pat (Expr struct prim inj name) <-
      reserved "let" >> pratt @proxy (lookAhead (reservedOp "=") $> ()) Go <* reservedOp "=" <?> "Let Expression Head"
    value <- parser (lookAhead (reserved "in") $> ()) Go <* reserved "in"
    ret <- parser end Go
    return . literal . ExStc . inj $ Let pat value ret

-- | block lambda for expression
instance ( ExprC e m, Apply :<: struct
         , PatGroup :<: pext
         , PrattToken proxy (Pattern plit pext plabel pname (Expr struct prim inj name)) m
         , Lambda (Pattern plit pext plabel pname) (Bounds Symbol (Type tname tcons tbind tinj trep)) :<: struct
         )
  => PrattToken (WithExpr e m (Layer ("block" :- Type tname tcons tbind tinj trep) proxy (Hint (Pattern plit pext plabel pname))))
                (Expr struct prim inj name) m where
  tokenize _ parser _ = do
    let iPat = pratt @proxy @(Pattern plit pext plabel pname (Expr struct prim inj name))
                     (void . lookAhead . choice $ reservedOp <$> [",", "=", "|"]) Go
        gPat = iPat `sepBy1` reservedOp "," <&> PatExt . inj . PatGrp <?> "group pattern"
        sPat = gPat `sepBy1` reservedOp "|" <&> PatExt . inj . PatSeq <?> "sequence pattern" <|> fail "expect a pattern definition"
        branch = (,) <$> sPat <*> (reservedOp "=" *> parser (void . lookAhead $ reservedOp "]" <|> reservedOp "|") Go)
             <|> fail "expect equation"
        lambda = do
          heads <- Lambda . Bounds . fromMaybe [] <$> optional
            (try $ ((:> (TypPht :: Type tname tcons tbind tinj trep)) . Symbol <$> identifier) `manyTill`  reservedOp ";;")
          heads <$> branch <*> (reservedOp "|" *> branch `sepBy1` reservedOp "|" <|> return [])
    brackets (lambda <&> ExStc . inj <?> "block lambda") <&> literal

-- | one line lambda for expression
instance ( ExprC e m, Apply :<: struct
         , PrattToken proxy (pat (Expr struct prim inj name)) m
         , Lambda (Grp pat) (Bounds Symbol (Type tname tcons tbind tinj trep)) :<: struct
         )
  => PrattToken (WithExpr e m (Layer ("line" :- Type tname tcons tbind tinj trep) proxy (Hint (Grp pat))))
                (Expr struct prim inj name) m where
  tokenize _ parser end = do
    let iPat = pratt @proxy @(pat (Expr struct prim inj name))
                     (void . lookAhead . choice $ reservedOp <$> [",", "=>"]) Go
               <?> "Lambda Parameter pattern"
        gPat = iPat `sepBy1` reservedOp "," <&> Grp
        branch = (,) <$> gPat <*> (reservedOp "=>" *> parser (void $ lookAhead end) Go)
        lambda = do
          heads <- Lambda . Bounds . fromMaybe [] <$> optional
            (try $ ((:> (TypPht :: Type tname tcons tbind tinj trep)) . Symbol <$> identifier) `manyTill`  reservedOp ";;")
          heads <$> branch <*> return []
    reservedOp "\\" *> (lambda <&> ExStc . inj) <&> literal

-- | type application
instance ( ExprC e m
         , PrattToken proxy (Type tname tcons tbind tinj trep) m
         , VisibleType (Type tname tcons tbind tinj trep) :<: prim
         , tname ~ Symbol, Tuple :<: tcons, LiteralText :<: tcons, LiteralNatural :<: tcons
         , Record Label :<: tcons
         )
  => PrattToken (WithExpr e m (Layer "@type" proxy (Type tname tcons tbind tinj trep)))
                (Expr struct prim inj name) m where
  tokenize _ _ _ = char '@' >> do
    typ <- parens (pratt @proxy @(Type tname tcons tbind tinj trep) (lookAhead (reservedOp ")") $> ()) Go)
      <|> TypRef . Symbol <$> identifier
      <|> TypLit . inj . Tuple <$> parens
            (pratt @proxy (lookAhead (reservedOp "," <|> reservedOp ")") $> ()) Go `sepBy` reservedOp ",")   -- tuple
      <|> TypLit . inj . LiteralNatural . Literal <$> integer -- integer
      <|> TypLit . inj . LiteralText . Literal . pack <$> stringLiteral -- text
      <|> TypLit . inj <$> record (pratt @proxy)
    let nud' _ = fail "Type application requires one argument"
        led' _ left = return . ExPrm . inj $ VisibleType typ left
    return $ Semantic nud' led' (return Infinite)

