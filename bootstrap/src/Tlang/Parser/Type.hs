{- | * Parser for type expression
-}

module Tlang.Parser.Type
  ( WithType

  -- ** syntax group
  , record
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST

import Data.Text (Text)
import Data.List (find)
import Data.Functor (($>), (<&>))
import qualified Data.Kind as Kind (Type)
import Text.Megaparsec hiding (Label)
import Tlang.Generic ((:<:) (..))
import Tlang.Extension
  ( Tuple (..), Forall (..), Variant (..), Record (..)
  , Scope (..), Literal (..), LiteralNatural (..), LiteralText (..)
  )
import Control.Monad (when)

import Capability.Reader (HasReader, asks)

-- | symbol for type parser
data WithType (e :: Kind.Type) (m :: Kind.Type -> Kind.Type) (a :: k)
type TypeC e m = (MonadFail m, ShowErrorComponent e, MonadParsec e Text m)

-- pratt @(WithType Void _ ("identifier" :- "operator" :- "group" :- Tuple)) @(TypeAST Identity) eof Go

cons :: Type rep prm bind inj name -> Type rep prm bind inj name -> Type rep prm bind inj name
cons (TypCon a as) rhs = TypCon a (as <> [rhs])
cons a rhs = TypCon a [rhs]

-- | a specialised `return` for Semantic
literal :: Monad m => Type rep prm bind inj name -> Semantic m (Type rep prm bind inj name)
literal a = Semantic (const $ return a) (\_ left -> return $ cons left a) (return Infinite)

-- *** syntax group

record :: TypeC e m => (m () -> Power -> m (Type rep prm bind inj name)) -> m (Record Label (Type rep prm bind inj name))
record parser = braces (sepBy1 pair (reservedOp ",")) <&> Record
  where pair = do
          name <- identifier <|> operator <&> Label
          typ <- reservedOp ":" *> parser (lookAhead $ (reservedOp "}" <|> reservedOp ",") $> ()) Go
          return (name, typ)

-- | use type level
instance ( PrattToken (WithType e m a) (Type rep prm bind inj name) m
         , PrattToken (WithType e m as) (Type rep prm bind inj name) m
         , TypeC e m
         )
  => PrattToken (WithType e m (a :- as)) (Type rep prm bind inj name) m where
  tokenize _ parser end = try (tokenize (Proxy @(WithType e m a)) parser end)
                      <|> tokenize (Proxy @(WithType e m as)) parser end

-- | most common identifier in `Type`
instance (TypeC e m, name ~ Name)
  => PrattToken (WithType e m "identifier") (Type rep prm bind inj name) m where
  tokenize _ _ _ = do
    name <- Name <$> identifier <?> "Type Identifier"
    if name `elem` ["forall"]
       then fail $ "Unexpected reserved: " <> show name
       else return . literal $ TypVar name

-- | "operator" in type
instance (TypeC e m, name ~ Name, HasReader "TypeOperator" [Operator Text] m)
  => PrattToken (WithType e m "operator") (Type rep prm bind inj name) m where
  tokenize _ parser _ = do
    op <- operator
    when (op `elem` ["\\", "."]) do
      fail $ "Unexpected reserved: " <> show op
    Operator fixity l r _ <- asks @"TypeOperator" (find (\(Operator _ _ _ n) -> n == op)) >>= \case
      Just a -> return a
      Nothing -> fail $ "Operator is not defined in type level: " <> show op
    let nud' end =
          if fixity `elem` [Prefix, Unifix]
             then parser end (Power r) <&> TypCon (TypVar $ Name op) . pure
             else fail $ "Wrong position of " <> show op <> " : it has fixity " <> show fixity <> " but expect Prefix or Unifix"
        led' end left =
          case fixity of
            Infix -> parser end (Power r) <&> TypCon (TypVar $ Name op) . (left:) . pure
            Unifix -> return (TypCon (TypVar $ Name op) [left])
            Postfix -> return (TypCon (TypVar $ Name op) [left])
            _ -> fail $ "Wrong position of " <> show op <> " : it has fixity " <> show fixity <> " but expect Infix, Postfix or Unifix"
    return (Semantic nud' led' (return $ Power l))

-- | enable "(any type expression)" group
instance TypeC e m
  => PrattToken (WithType e m "group") (Type rep prm bind inj name) m where
  tokenize _ parser _ = parens (parser (lookAhead (reservedOp ")") $> ()) Go) <&> literal

-- | type level nat number
instance (TypeC e m, LiteralNatural :<: prm)
  => PrattToken (WithType e m "nat") (Type rep prm bind inj name) m where
  tokenize _ _ _ = do
    nat <- TypPrm . inj . LiteralNatural . Literal <$> integer <?> "Natural Number"
    return $ literal nat

-- | type level str number
instance (TypeC e m, LiteralText :<: prm)
  => PrattToken (WithType e m "text") (Type rep prm bind inj name) m where
  tokenize _ _ _ = do
    str <- TypPrm . inj . LiteralText . Literal <$> stringLiteral <?> "Type Level String"
    return $ literal str

-- -- | record literal for type
instance (TypeC e m, Record Label :<: prm)
  => PrattToken (WithType e m "record") (Type rep prm bind inj name) m where
  tokenize _ parser _ = record parser <&> literal . TypPrm . inj

-- | variant literal for type
instance (TypeC e m, Variant Label :<: prm)
  => PrattToken (WithType e m "variant") (Type rep prm bind inj name) m where
  tokenize _ parser _ = do
    let pair = do
          name <- identifier <|> operator <&> Label
          typ <- optional $ reservedOp ":" *> (parser (lookAhead $ (reservedOp "," <|> reservedOp ">") $> ()) Go)
          return (name, typ)
    variant <- angles $ sepBy pair (reservedOp ",")
            <&> TypPrm . inj . Variant
    return $ literal variant

-- | tuple literal for type
instance (TypeC e m, Tuple :<: prm)
  => PrattToken (WithType e m "tuple") (Type rep prm bind inj name) m where
  tokenize _ parser _ = do
    tuple <- parens (sepBy (parser (lookAhead $ (reservedOp "," <|> reservedOp ")") $> ()) Go) (reservedOp ","))
          <&> TypPrm . inj . Tuple
    return $ literal tuple

-- | `Forall` quantified type
instance (TypeC e m, (Forall (Bound Name)) :<: bind)
  => PrattToken (WithType e m "forall") (Type rep prm bind inj name) m where
  tokenize _ parser end = do
    let name = Name <$> identifier
        bound = (:> TypPht) <$> name <|> parens do
          name' <- name
          op <- reservedOp "~" $> (:>) <|> reservedOp "=" $> (:~)
          op name' <$> parser (lookAhead $ reservedOp ")" $> ()) Go
    bounds <- reserved "forall" *> bound `someTill` reservedOp "."
    body <- parser end Go
    return . literal $ foldr ($) body (TypLet . inj . Forall <$> bounds)

-- | `Scope` quantified type
instance (TypeC e m, (Scope (Bound Name)) :<: bind)
  => PrattToken (WithType e m "abstract") (Type rep prm bind inj name) m where
  tokenize _ parser end = do
    let name = Name <$> identifier
        bound = (:> TypPht) <$> name
    bounds <- reservedOp "\\" *> bound `someTill` reservedOp "."
    body <- parser end Go
    return . literal $ foldr ($) body (TypLet . inj . Scope <$> bounds)
