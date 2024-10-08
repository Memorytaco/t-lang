{- | * Parser for type expression
-}

module Language.Parser.Type
  ( TypeSyntax

  -- ** syntax group
  , record
  )
where

import Language.Parser.Class
import Language.Parser.Lexer

import Language.Core

import qualified Language.DataLayout as Rep

import Data.Text (Text)
import Data.List (find)
import Data.Functor (($>), (<&>))
import qualified Data.Kind as Kind (Type)
import Text.Megaparsec hiding (Label)
import Language.Generic ((:<:), (:<<:), inj, injj)
import Language.Core.Extension
  ( Tuple (..), Forall (..), Variant (..), Record (..)
  , Scope (..), Literal (..)
  )
import Control.Monad (when)

import Capability.Reader (HasReader, asks)

-- | Syntax definition DSL for type of language.
--
-- It supports default ":-" sequence operator and also "Layer" extensin.
--
-- Available entries:
--
-- 1. "identifier"
-- 2. "operator"
-- 3. "group"
-- 4. "nat"
-- 5. "text"
-- 6. "record"
-- 7. "variant"
-- 8. "tuple"
-- 9. "rep"
-- 10. "forall"
-- 11. "abstract"
--
data TypeSyntax (e :: Kind.Type) (m :: Kind.Type -> Kind.Type) (a :: k)

type TypeParserEnv e m = (MonadFail m, ShowErrorComponent e, MonadParsec e Text m)

-- pratt @(WithType Void _ ("identifier" :- "operator" :- "group" :- Tuple)) @(TypeAST Identity) eof Go

cons :: Type bind rep name a -> Type bind rep name a -> Type bind rep name a
cons (TypCon a as) rhs = TypCon a (as <> [rhs])
cons a rhs = TypCon a [rhs]

-- | a specialised `return` for Semantic
literal :: Monad m => Type bind rep name a -> Semantic m (Type bind rep name a)
literal a = Semantic (const $ return a) (\_ left -> return $ cons left a) (return Infinite)

-- *** syntax group

record :: TypeParserEnv e m => (m () -> Power -> m (Type bind rep name a)) -> m (Record Label (Type bind rep name a))
record parser = braces (sepBy1 pair (reservedOp ",")) <&> Record
  where pair = do
          name <- identifier <|> operator <&> Label
          typ <- reservedOp ":" *> parser (lookAhead $ (reservedOp "}" <|> reservedOp ",") $> ()) Go
          return (name, typ)

-- | use type level
instance ( PrattToken (TypeSyntax e m x) (Type bind rep name a) m
         , PrattToken (TypeSyntax e m y) (Type bind rep name a) m
         , TypeParserEnv e m
         )
  => PrattToken (TypeSyntax e m (x :- y)) (Type bind rep name a) m where
  tokenize _ parser end = try (tokenize (Proxy @(TypeSyntax e m x)) parser end)
                      <|> tokenize (Proxy @(TypeSyntax e m y)) parser end

-- | most common identifier in `Type`
instance (TypeParserEnv e m, a ~ Name)
  => PrattToken (TypeSyntax e m "identifier") (Type bind rep name a) m where
  tokenize _ _ _ = do
    name <- Name <$> identifier <?> "Type Identifier"
    if name `elem` ["forall"]
       then fail $ "Unexpected reserved: " <> show name
       else return . literal $ TypVar name

-- | "operator" in type
instance (TypeParserEnv e m, a ~ Name, HasReader "TypeOperator" [Operator Text] m)
  => PrattToken (TypeSyntax e m "operator") (Type bind rep name a) m where
  tokenize _ parser _ = do
    op <- operator
    when (op `elem` ["\\", "."]) do
      fail $ "Unexpected reserved: " <> show op
    Operator fixity l r _ <- asks @"TypeOperator" (find (\(Operator _ _ _ n) -> n == op)) >>= \case
      Just a -> return a
      Nothing -> fail $ "Operator is not defined in type level: " <> show op
    let nud' end =
          if fixity `elem` [Prefix, Unifix, PreInfix, UniInfix]
             then parser end (Power r) <&> TypCon (TypVar $ Name op) . pure
             else fail $ "Wrong position of " <> show op <> " : it has fixity " <> show fixity <> " but expect Prefix or Unifix"
        led' end left =
          case fixity of
            Infix -> parser end (Power r) <&> TypCon (TypVar $ Name op) . (left:) . pure
            PreInfix -> parser end (Power r) <&> TypCon (TypVar $ Name op) . (left:) . pure
            Unifix -> return (TypCon (TypVar $ Name op) [left])
            Postfix -> return (TypCon (TypVar $ Name op) [left])
            PostInfix -> do
              right'maybe <- optional $ parser end (Power r)
              case right'maybe of
                Nothing -> return (TypCon (TypVar $ Name op) [left])
                Just right -> return $ TypCon (TypVar $ Name op) [left, right]
            UniInfix -> do
              right'maybe <- optional $ parser end (Power r)
              case right'maybe of
                Nothing -> return (TypCon (TypVar $ Name op) [left])
                Just right -> return $ TypCon (TypVar $ Name op) [left, right]
            _ -> fail $ "Wrong position of " <> show op <> " : it has fixity " <> show fixity <> " but expect Infix, Postfix or Unifix"
    return (Semantic nud' led' (return $ Power l))

-- | enable "(any type expression)" group
instance TypeParserEnv e m
  => PrattToken (TypeSyntax e m "group") (Type bind rep name a) m where
  tokenize _ parser _ = parens (parser (lookAhead (reservedOp ")") $> ()) Go) <&> literal

-- | type level nat number
instance (TypeParserEnv e m, Literal Integer :<: rep)
  => PrattToken (TypeSyntax e m "nat") (Type bind rep name a) m where
  tokenize _ _ _ = do
    nat <- Type . inj . Literal @Integer <$> integer <?> "Natural Number"
    return $ literal nat

-- | type level str number
instance (TypeParserEnv e m, Literal Text :<: rep)
  => PrattToken (TypeSyntax e m "text") (Type bind rep name a) m where
  tokenize _ _ _ = do
    str <- Type . inj . Literal <$> stringLiteral <?> "Type Level String"
    return $ literal str

-- -- | record literal for type
instance (TypeParserEnv e m, Record Label :<: rep)
  => PrattToken (TypeSyntax e m "record") (Type bind rep name a) m where
  tokenize _ parser _ = record parser <&> literal . Type . inj

-- | variant literal for type
instance (TypeParserEnv e m, Variant Label :<: rep)
  => PrattToken (TypeSyntax e m "variant") (Type bind rep name a) m where
  tokenize _ parser _ = do
    let pair = do
          name <- identifier <|> operator <&> Label
          typ <- optional $ reservedOp ":" *> parser (lookAhead $ (reservedOp "," <|> reservedOp ">") $> ()) Go
          return (name, typ)
    variant <- angles $ sepBy pair (reservedOp ",")
            <&> Type . inj . Variant
    return $ literal variant

-- | tuple literal for type
instance (TypeParserEnv e m, Tuple :<: rep)
  => PrattToken (TypeSyntax e m "tuple") (Type bind rep name a) m where
  tokenize _ parser _ = do
    tuple <- parens (sepBy (parser (lookAhead $ (reservedOp "," <|> reservedOp ")") $> ()) Go) (reservedOp ","))
          <&> Type . inj . Tuple
    return $ literal tuple

-- | runtime representation for type
--
-- TODO: we need to build an interface like FFI to deal with foreign low level type
instance (TypeParserEnv e m, Rep.Rep :<: rep)
  => PrattToken (TypeSyntax e m "rep") (Type bind rep name a) m where
  tokenize _ parser _ = do
    typ <- symbol "#[" *> (Rep.Rep . Rep.RepLift <$> prim) <* symbol "]"
    return . literal . Type $ inj typ
    where
      prim = parens prim <|> bit <|> int <|> uint <|> lfloat <|> ptr <|> stru <|> lft
      ptr = reserved "ptr" *> (Rep.ptr <$> prim)
      bit = reserved "bit" *> (Rep.bit <$> integer)
      int = reserved "int8" $> Rep.int8
        <|> reserved "int16" $> Rep.int16
        <|> reserved "int32" $> Rep.int32
        <|> reserved "int64" $> Rep.int64
        <|> reserved "int128" $> Rep.int128
      uint = reserved "uint8" $> Rep.uint8
         <|> reserved "uint16" $> Rep.uint16
         <|> reserved "uint32" $> Rep.uint32
         <|> reserved "uint64" $> Rep.uint64
      lfloat = reserved "float" $> Rep.float
          <|> reserved "double" $> Rep.double
      stru = Rep.struct False <$> braces (prim `sepBy1` reservedOp ",")
      lft = symbol "$" *> parens (parser (lookAhead $ symbol ")" $> ()) Go) <&> Rep.Embed . Rep.DataRep

-- | `Forall` quantified type
instance (TypeParserEnv e m, Forall Prefix :<<: bind, a ~ Name, name ~ Name)
  => PrattToken (TypeSyntax e m "forall") (Type bind rep name a) m where
  tokenize _ parser end = do
    let name = Name <$> identifier
        bound = (:> TypPht) <$> name <|> parens do
          name' <- name
          op <- reservedOp "~" $> (:>) <|> reservedOp "=" $> (:~)
          op name' <$> parser (lookAhead $ reservedOp ")" $> ()) Go
    bounds <- reserved "forall" *> bound `someTill` reservedOp "."
    body <- parser end Go
    return . literal $ foldr collect body bounds
    where
      collect bound typ = TypBnd (injj @(Forall Prefix) @bind $ Forall bound typ)

-- | `Scope` quantified type
instance (TypeParserEnv e m, Scope Prefix :<<: bind, name ~ Name, a ~ Name)
  => PrattToken (TypeSyntax e m "abstract") (Type bind rep name a) m where
  tokenize _ parser end = do
    let name = Name <$> identifier
        bound = (:> TypPht) <$> name
    bounds <- reservedOp "\\" *> bound `someTill` reservedOp "."
    body <- parser end Go
    return . literal $ foldr collect body bounds
    where
      -- addIndex :: Scope b :<: bind => (name, b name) -> Type bind rep name a
      collect bound typ = TypBnd (injj @(Scope Prefix) @bind $ Scope bound typ)
