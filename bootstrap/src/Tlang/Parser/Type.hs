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
import Tlang.Constraint (Prefix (..))

import qualified Tlang.Rep as Rep

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

cons :: Type bind rep name a -> Type bind rep name a -> Type bind rep name a
cons (TypCon a as) rhs = TypCon a (as <> [rhs])
cons a rhs = TypCon a [rhs]

-- | a specialised `return` for Semantic
literal :: Monad m => Type bind rep name a -> Semantic m (Type bind rep name a)
literal a = Semantic (const $ return a) (\_ left -> return $ cons left a) (return Infinite)

-- *** syntax group

record :: TypeC e m => (m () -> Power -> m (Type bind rep name a)) -> m (Record Label (Type bind rep name a))
record parser = braces (sepBy1 pair (reservedOp ",")) <&> Record
  where pair = do
          name <- identifier <|> operator <&> Label
          typ <- reservedOp ":" *> parser (lookAhead $ (reservedOp "}" <|> reservedOp ",") $> ()) Go
          return (name, typ)

-- | use type level
instance ( PrattToken (WithType e m x) (Type bind rep name a) m
         , PrattToken (WithType e m y) (Type bind rep name a) m
         , TypeC e m
         )
  => PrattToken (WithType e m (x :- y)) (Type bind rep name a) m where
  tokenize _ parser end = try (tokenize (Proxy @(WithType e m x)) parser end)
                      <|> tokenize (Proxy @(WithType e m y)) parser end

-- | most common identifier in `Type`
instance (TypeC e m, a ~ Name)
  => PrattToken (WithType e m "identifier") (Type bind rep name a) m where
  tokenize _ _ _ = do
    name <- Name <$> identifier <?> "Type Identifier"
    if name `elem` ["forall"]
       then fail $ "Unexpected reserved: " <> show name
       else return . literal $ TypVar name

-- | "operator" in type
instance (TypeC e m, a ~ Name, HasReader "TypeOperator" [Operator Text] m)
  => PrattToken (WithType e m "operator") (Type bind rep name a) m where
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
instance TypeC e m
  => PrattToken (WithType e m "group") (Type bind rep name a) m where
  tokenize _ parser _ = parens (parser (lookAhead (reservedOp ")") $> ()) Go) <&> literal

-- | type level nat number
instance (TypeC e m, LiteralNatural :<: rep)
  => PrattToken (WithType e m "nat") (Type bind rep name a) m where
  tokenize _ _ _ = do
    nat <- Type . inj . LiteralNatural . Literal <$> integer <?> "Natural Number"
    return $ literal nat

-- | type level str number
instance (TypeC e m, LiteralText :<: rep)
  => PrattToken (WithType e m "text") (Type bind rep name a) m where
  tokenize _ _ _ = do
    str <- Type . inj . LiteralText . Literal <$> stringLiteral <?> "Type Level String"
    return $ literal str

-- -- | record literal for type
instance (TypeC e m, Record Label :<: rep)
  => PrattToken (WithType e m "record") (Type bind rep name a) m where
  tokenize _ parser _ = record parser <&> literal . Type . inj

-- | variant literal for type
instance (TypeC e m, Variant Label :<: rep)
  => PrattToken (WithType e m "variant") (Type bind rep name a) m where
  tokenize _ parser _ = do
    let pair = do
          name <- identifier <|> operator <&> Label
          typ <- optional $ reservedOp ":" *> (parser (lookAhead $ (reservedOp "," <|> reservedOp ">") $> ()) Go)
          return (name, typ)
    variant <- angles $ sepBy pair (reservedOp ",")
            <&> Type . inj . Variant
    return $ literal variant

-- | tuple literal for type
instance (TypeC e m, Tuple :<: rep)
  => PrattToken (WithType e m "tuple") (Type bind rep name a) m where
  tokenize _ parser _ = do
    tuple <- parens (sepBy (parser (lookAhead $ (reservedOp "," <|> reservedOp ")") $> ()) Go) (reservedOp ","))
          <&> Type . inj . Tuple
    return $ literal tuple

-- | runtime representation for type
--
-- TODO: we need to build an interface like FFI to deal with foreign low level type
instance (TypeC e m, Rep.Rep :<: rep)
  => PrattToken (WithType e m "rep") (Type bind rep name a) m where
  tokenize _ parser _ = do
    typ <- symbol "#[" *> (Rep.Rep . Rep.RepLift <$> prim) <* symbol "]"
    return . literal . Type $ inj typ
    where
      prim = parens prim <|> bit <|> int <|> uint <|> float <|> ptr <|> stru <|> lft
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
      float = reserved "float" $> Rep.float
          <|> reserved "double" $> Rep.double
      stru = Rep.struct <$> braces (prim `sepBy1` reservedOp ",")
      lft = symbol "$" *> parens (parser (lookAhead $ symbol ")" $> ()) Go) <&> Rep.Embed . Rep.DataRep

-- | `Forall` quantified type
instance (TypeC e m, (Forall (Prefix Name)) :<: bind, a ~ Name, name ~ Name, Functor rep)
  => PrattToken (WithType e m "forall") (Type bind rep name a) m where
  tokenize _ parser end = do
    let name = Name <$> identifier
        bound = (\a -> (a, a :> TypPht)) <$> name <|> parens do
          name' <- name
          op <- reservedOp "~" $> (:>) <|> reservedOp "=" $> (:~)
          bnd <- op name' <$> parser (lookAhead $ reservedOp ")" $> ()) Go
          return (name', bnd)
    bounds <- reserved "forall" *> bound `someTill` reservedOp "."
    body <- parser end Go
    return . literal $ foldr addIndex body bounds
    where
      addIndex (name, bound) typ = TypBnd (inj $ Forall bound) (lift name typ)

-- | `Scope` quantified type
instance (TypeC e m, (Scope (Prefix Name)) :<: bind, name ~ Name, a ~ Name, Functor rep)
  => PrattToken (WithType e m "abstract") (Type bind rep name a) m where
  tokenize _ parser end = do
    let name = Name <$> identifier
        bound = do
          name' <- name
          return (name', name' :> TypPht)
    bounds <- reservedOp "\\" *> bound `someTill` reservedOp "."
    body <- parser end Go
    return . literal $ foldr addIndex body bounds
    where
      addIndex (name, bound) typ = TypBnd (inj $ Scope bound) (lift name typ)

lift :: (Eq a, Monad m, Functor f) => a -> f a -> f (a >| m a)
lift name = fmap \a -> if name == a then New a else Inc (return a)
