{- | * Parser for type expression
    TODO: add parser for type literal
-}

module Tlang.Parser.Type
  (
    WithType
  , ParseType

  , dataVariant
  , record
  )
where

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Tlang.AST

import Data.Text (Text, pack)
import Data.List (find)
import Data.Functor (($>), (<&>))
import qualified Data.Kind as Kind (Type)
import Text.Megaparsec hiding (Label)
import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Tlang.Generic (inj)
import Tlang.Helper.AST.Type (getTypeLit)
import Tlang.Extension (Tuple (..), Forall (..), Variant (..), Record (..), Scope (..), Literal (..), LiteralNatural (..), LiteralText (..))

import Capability.Reader (HasReader, ask)

type ParseType = TypeAST Identity

-- | symbol for type parser
data WithType (m :: Kind.Type -> Kind.Type)

instance (MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m, ShowErrorComponent e)
  => HasPratt (WithType m) ParseType m where
  type Token (WithType m) ParseType = OperatorClass String ParseType
  next _ = (OpRep . Left <$> syntax <?> "Type Pair Operator")
       <|> (OpRep . Right <$> operator <?> "Type Operator")
       <|> (OpNorm <$> lit <?> "Type Literal")
       <|> (OpNorm <$> typNormal <?> "Type Name")
      where
        lit = nat <|> str
        nat = TypLit . inj . LiteralNatural . Literal <$> integer <?> "Literal Natural"
        str = TypLit . inj . LiteralText . Literal . pack <$> stringLiteral <?> "Literal String"
        typNormal = identifier <&> TypRef . Symbol
        withSpecial v@(l, r) = reservedOp (pack l) $> Left v
                           <|> reservedOp (pack r) $> Right v
        special = foldr1 (<|>) $ withSpecial
              <$> [("(", ")"), ("[", "]"), ("<", ">"), ("{", "}")]
        syntax  = reserved "forall" $> Left ("forall", "forall")
              <|> reservedOp "\\" $> Left ("\\", "\\")
              <|> special

  peek witness = lookAhead (next witness)

  getPower _ = \case
    (OpNorm _) -> return (999, 999)
    (OpRep (Left _)) -> return (999, 999)
    (OpRep (Right op)) -> getOperator op <&> getBindPower

  nud witness end = withOperator return \case
    Left (Left (l, r)) -> do
      case l of
        "{" -> record
        "<" -> variant
        "forall" -> quantified end
        "\\" -> abstract end
        _ -> do ntok <- peek witness
                matchPairOperator (Right ("(", ")")) ntok
                  (next witness $> TypLit (inj $ Tuple [])) $ pratt @(WithType m) (void $ reservedOp (pack r)) (-100)
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"
    Right s -> getOperator s >>= \(Operator assoc _ r op) ->
      case assoc of
        Infix -> fail $ "expect prefix or unifix operator but got infix operator " <> op
        Postfix -> fail $ "expect prefix or unifix operator but got postfix operator " <> op
        _ -> do right <- pratt @(WithType m) end r
                return $ TypCon (TypRef $ Op op) [right]

  led witness (lookAhead -> end) left = withOperator (return . apply left) \case
    Right s -> getOperator s >>= \(Operator assoc _ r op) ->
      case assoc of
        Prefix -> fail $ "expect unifix, postfix or infix operator but got prefix operator " <> s
        Infix -> case s of
                  "*" -> pratt @(WithType m) end r >>= \right -> return
                    case getTypeLit @Tuple right of
                      Just (Tuple ls) -> TypLit . inj . Tuple $ left:ls
                      Nothing -> TypLit . inj $ Tuple [left, right]
                    -- TypTup ls -> return . TypTup $ left: ls
                    -- right -> return $ TypTup [left, right]
                  "->" -> TypCon (TypRef (Op "->")) . (left:) . pure <$> pratt @(WithType m) end r
                  _ -> pratt @(WithType m) end r <&> TypCon (TypRef $ Op op) . (left:) . pure
        _ -> return $ TypCon (TypRef $ Op op) [left]
    Left (Left (l, r)) ->
      case l of
        "(" -> do
          ntok <- peek witness
          matchPairOperator (Right ("(", ")")) ntok
            (next witness $> left `apply` TypLit (inj $ Tuple [])) $ pratt @(WithType m) (void $ reservedOp (pack r)) (-100) <&> apply left
        "{" -> apply left <$> record
        "<" -> apply left <$> variant
        "forall" -> apply left <$> quantified end
        "\\" -> apply left <$> abstract end
        _   -> fail $ "Unrecognized " <> l <> ", It could be an internal error, please report to upstream."
    Left (Right (l, r)) -> fail $ "mismatched operator " <> show r <> ", forget " <> show l <> " ?"

apply :: Type name cons bind inj rep -> Type name cons bind inj rep -> Type name cons bind inj rep
apply (TypCon v vs) r = TypCon v (vs <> [r])
apply l r = TypCon l [r]

quantified :: forall m e. (ShowErrorComponent e, MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m) => m () -> m ParseType
quantified (lookAhead -> end) = do
  let name = Symbol <$> identifier
      boundTyp = pratt @(WithType m) (void . lookAhead $ reservedOp ")") (-100)
      scopBound = do
        tok <- name
        op <- reservedOp "~" $> (:>) <|> reservedOp "=" $> (:~)
        op tok <$> boundTyp
      bound = parens scopBound
          <|> (:> TypPht) <$> name
  bounds <- bound `someTill` reservedOp "."
  body <- pratt @(WithType m) end (-100)
  return $ foldr ($) body (TypLet . inj . Forall <$> bounds)

abstract :: forall m e. (ShowErrorComponent e, MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m) => m () -> m ParseType
abstract (lookAhead -> end) = do
  let name = Symbol <$> identifier
  bounds <- ((:> TypPht) <$> name) `someTill` reservedOp "."
  body <- pratt @(WithType m) end (-100)
  return $ foldr ($) body (TypLet . inj . Scope <$> bounds)

-- | record type parser
record :: forall m e. (ShowErrorComponent e, MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m) => m ParseType
record = do
  let recordPair = do
        name <- fmap Label $ identifier <|> operator
        typ <- reservedOp ":" *> pratt @(WithType m) (lookAhead ((reservedOp "}" <|> reservedOp ",") $> ())) (-100)
        return (name, typ)
  fields <- sepBy1 recordPair (reservedOp ",") <* reservedOp "}"
  return . TypLit . inj $ Record fields

-- | variant type parser
variant :: forall m e. (ShowErrorComponent e, MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m) => m ParseType
variant = do
  let variantPair = do
        name <- fmap Label $ identifier <|> operator
        typ <- optional $ reservedOp ":" *> pratt @(WithType m) (lookAhead ((reservedOp ">" <|> reservedOp ",") $> ())) (-100)
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp ",") <* reservedOp ">"
  return . TypLit . inj $ Variant fields

-- | a specialized version used in type declaration context. see `Tlang.AST.Declaration`.
dataVariant :: forall m e. (ShowErrorComponent e, MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m) => m () -> m ParseType
dataVariant end = do
  let variantPair = do
        name <- fmap Label $ identifier <|> operator
        isEnd <- lookAhead . optional $ void (reservedOp "|") <|> end
        typ <- maybe
                 (Just <$> pratt @(WithType m) (lookAhead $ void (reservedOp "|") <|> end) (-100))
                 (const $ return Nothing) isEnd
        return (name, typ)
  fields <- sepBy1 variantPair (reservedOp "|") <* end
  return . TypLit . inj $ Variant fields

getOperator :: (MonadFail m, MonadParsec e Text m, HasReader "TypeOperator" [Operator String] m) => String -> m (Operator String)
getOperator op = do
  stat <- ask @"TypeOperator"
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> op <> " is undefined in type level."

