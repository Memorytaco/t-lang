module Tlang.Parser
  ( UntypedName (..)
  , TypedName (..)
  , Expr (..)
  , ExprF (..)
  , LitValue (..)
  , TypAnno (..)
  , TypAnnoF (..)
  , LambdaBlock (..)
  , LanguageModule (..)
  , ModuleElement (..)

  , Parser

  , getTypedNameT
  , getExprName
  , getLangModEleNamePair
  , getLangModEleBlock
  , getLangModEleExpr
  , onExprNameF
  , mapExprNameF

  , parseToplevel
  )
where

{-

As the file name suggests.

This module includes
- TypedName, UntypedName. we have UntypedName when building up AST, and then we resolve it to TypedName.
- TypeAnno. it is used to represent user type annotation and it also serves as initial type to literal value.

-}

import Text.Parsec

import qualified Text.Parsec.Token as Token

import Data.Functor.Foldable (Base)
import Data.Functor.Foldable.TH

import Tlang.Lexer.Lexer
import Data.List (find)

import Tlang.Parser.Pratt (Operator (..), PrattParser, OperatorKind (..), OperatorClass (..), OperatorClassSpecial (..))

-- name reference, we don't know what type this name is, so simply records it.
data UntypedName = UntypedName String | UntypedMarker deriving (Show, Eq)

-- Another Wrapper to help analyze expression
data TypedName a = TypedName String a
                 | TypedOnly a
                 deriving (Eq, Functor)

instance Show a => Show (TypedName a) where
  show (TypedName n a) = n <> ": " <> show a
  show (TypedOnly a) = show a

getTypedNameT :: TypedName a -> a
getTypedNameT (TypedName _ a) = a
getTypedNameT (TypedOnly a) = a

-- Expression, parametric with binary operator and also one name tag.
-- TODO: extend or refactor the expression structure to hold lambda;
data Expr op name = ExLit name LitValue -- literal value
                  | ExRef name          -- variable or term name reference
                  | ExCall name (Expr op name) (Expr op name)     -- application
                  | ExBind name (Maybe TypAnno) (Expr op name)    -- variable binding
                  | ExOp op name (Expr op name) (Expr op name)    -- binary operator
                  | ExOpUni op name (Expr op name)                -- uni operator
                  deriving (Show, Eq)

data LitValue = LitInt Integer | LitNumber Double | LitString String deriving (Show, Eq)

getExprName :: Expr op name -> name
getExprName (ExLit n _) = n
getExprName (ExRef n) = n
getExprName (ExCall n _ _) = n
getExprName (ExBind n _ _) = n
getExprName (ExOp _ n _ _) = n
getExprName (ExOpUni _ n _) = n

-- a simple type annotation
data TypAnno = TypName String   -- simple name for type name reference, to be resolved in next pass
             | TypPtr TypAnno   -- simple pointer type
             | TypArrow TypAnno TypAnno -- function type
             | TypArray TypAnno (Maybe Integer) -- array type
             | TypApply TypAnno TypAnno -- type application, TODO: this is not supported now!!
             | TypRec String [(String, TypAnno)]  -- Record type declaration
             deriving (Show, Eq)

-- function block will be represented by a lambda, a function definition is no more than assign a name along with type
-- annotation to a lambda expression.
-- TODO: allow user to use lambda in expression.
data LambdaBlock name = LambdaBlock
    { lambdaVars :: [(name, Maybe TypAnno)]
    , lambdaExprs :: [Expr (Operator String) name]
    } deriving (Show, Eq)

-- language module definition
data LanguageModule name anno = LanguageModule String [ModuleElement name anno] deriving (Show, Eq)
data ModuleElement name anno
  = ModuleFunction name (Maybe anno) (Maybe (LambdaBlock name)) -- FIXME: type annotation for parameter is required
  | ModuleBinding  name (Maybe anno) (Expr (Operator String) name) -- name binding: for global constant or function alias
  | ModuleTemplate name -- TODO, the definition here is not completed
  | ModuleType     name anno  -- TODO, the definition here is not completed
  | ModuleUnsafe   name anno  -- TODO, the definition here is not completed
  deriving (Show, Eq)

getLangModEleNamePair :: ModuleElement name anno -> (name, Maybe anno)
getLangModEleNamePair (ModuleFunction name anno _) = (name, anno)
getLangModEleNamePair (ModuleBinding name anno _) = (name, anno)
getLangModEleNamePair (ModuleTemplate name) = (name, Nothing)
getLangModEleNamePair (ModuleType name anno) = (name, Just anno)
getLangModEleNamePair (ModuleUnsafe name anno) = (name, Just anno)

getLangModEleBlock :: ModuleElement name anno -> Maybe (LambdaBlock name)
getLangModEleBlock (ModuleFunction _ _ block) = block
getLangModEleBlock _ = Nothing
getLangModEleExpr :: ModuleElement name anno -> Maybe (Expr (Operator String) name)
getLangModEleExpr (ModuleBinding _ _ v) = Just v
getLangModEleExpr _ = Nothing

$(makeBaseFunctor ''Expr)
$(makeBaseFunctor ''TypAnno)

onExprNameF :: (name1 -> name2) -> Base (Expr op name1) (Expr op name2) -> (Expr op name2)
onExprNameF f (ExLitF name v) = ExLit (f name) v
onExprNameF f (ExRefF name) = ExRef (f name)
onExprNameF f (ExCallF name v1 v2) = ExCall (f name) v1 v2
onExprNameF f (ExBindF name a v) = ExBind (f name) a v
onExprNameF f (ExOpF op name v1 v2) = ExOp op (f name) v1 v2
onExprNameF f (ExOpUniF op name v) = ExOpUni op (f name) v

mapExprNameF :: (name -> a) -> (a -> a -> a) -> Base (Expr op name) a -> a
mapExprNameF f _ (ExLitF n _) = f n
mapExprNameF f _ (ExRefF n) = f n
mapExprNameF f o (ExCallF n a1 a2) = f n `o` (a1 `o` a2)
mapExprNameF f o (ExBindF n _ a) = f n `o` a
mapExprNameF f o (ExOpF _ n a1 a2) = f n `o` (a1 `o` a2)
mapExprNameF f o (ExOpUniF _ n a) = f n `o` a

{-
-- parser definition
-}

-- Expression
type Parser = PrattParser (Expr (Operator String) UntypedName)

opTable :: [Operator (OperatorClass v)]
opTable =
  [ Operator Infix 55 60 (OpAction "+")
  , Operator Infix 80 80 (OpAction "*")
  , Operator Infix 80 80 (OpAction "/")
  , Operator Prefix 90 90 (OpAction "-")
  , Operator Unifix 70 70 (OpAction "!")
  , Operator Infix 30 30 (OpAction "?")
  , Operator Infix 40 40 (OpAction ":=")

  , Operator Prefix 9999 9999 . OpSpecial $ OpLeft "(" ")"
  , Operator Postfix 9999 9999 . OpSpecial $ OpRight "(" ")"
  ]

term :: Parser () -> Integer -> Parser (Expr (Operator String) UntypedName)
term end rbp = do
  left <- nud' end
  (end *> pure left) <|> led' end rbp left

expr :: Parser (OperatorClass (Expr op UntypedName))
expr = (OpNorm <$> variable <?> "Identifier")
    <|> (OpNorm <$> num <?> "Literal Number")
    <|> (OpNorm <$> str <?> "Literal String")
    <|> (OpAction <$> operator <?> "Operator")
    <|> (OpSpecial <$> special <?> "Pair Operator")
  where
    variable = identifier >>= return . ExRef . UntypedName
    num = try floating <|> nat
    nat = natural >>= return . ExLit UntypedMarker . LitInt
    floating = float >>= return . ExLit UntypedMarker . LitNumber
    str = stringLiteral >>= return . ExLit UntypedMarker . LitString
    withSpecial (l, r) = string l *> spaces *> pure (OpLeft l r)
                     <|> string r *> spaces *> pure (OpRight l r)
    special = foldr1 (<|>) $ withSpecial <$> [("(", ")")]

nud' :: Parser () -> Parser (Expr (Operator String) UntypedName)
nud' end = do
  tok'left <- expr
  case tok'left of
    OpNorm v -> nud end (Operator Unifix 999 999 (OpNorm v))
    _ -> getOperator tok'left >>= nud (lookAhead end)

led' :: Parser () -> Integer -> Expr (Operator String) UntypedName -> Parser (Expr (Operator String) UntypedName)
led' end rbp left = do
  lbp <- lookAhead expr >>= bindPower
  case lbp > rbp of
    False -> return left
    True -> do
      tok'test <- expr
      next <- case tok'test of
        OpNorm v -> led end (Operator Unifix 999 999 (OpNorm v)) left
        _ -> do
          op <- getOperator tok'test
          led (lookAhead end) op left
      (end >> return next) <|> led' end rbp next
  where
    bindPower (OpNorm _) = return 999
    bindPower op = do
      (Operator _ lbp _ _) <- getOperator op
      return lbp

-- nud and led function generator
nud :: Parser ()
    -> Operator (OperatorClass (Expr (Operator String) UntypedName))
    -> Parser (Expr (Operator String) UntypedName)
nud end (Operator k l r (OpAction n))
  | k == Infix = fail $ "Operator " <> n <> " is an infix operator, lack left value"
  | k == Unifix || k == Prefix = do
    right <- term end r
    return $ ExOpUni (Operator Unifix l r n) UntypedMarker right
  | otherwise = fail $ "Operator " <> n <> " is a postfix operator, but appear in a prefix position"
nud _end (Operator _ _ _ (OpNorm v)) = return v
nud _end (Operator k _ _ (OpSpecial (OpLeft l r)))
  | k == Prefix = term (string r *> spaces) 0
  | otherwise = fail $ "Wrong associativity with a left operator " <> l
nud _end (Operator _ _ _ (OpSpecial (OpRight l r)))
  = fail $ "Expect one prefix special operator and actual operator " <> r <> " is postfix or infix, missing paired " <> l

led :: Parser ()
    -> Operator (OperatorClass (Expr (Operator String) UntypedName))
    -> Expr (Operator String) UntypedName
    -> Parser (Expr (Operator String) UntypedName)
led end (Operator k l r (OpAction n)) left
  | k == Infix = do
    right <- term end r
    return $ ExOp (Operator Infix l r n) (UntypedMarker) left right
  | k == Unifix || k == Postfix = do
    return $ ExOpUni (Operator Unifix l r n) UntypedMarker left
  | otherwise = fail $ "Operator " <> n <> " is a prefix operator, but appear in a postfix position"
led _end (Operator _ _ _ (OpNorm right)) left = do
  return $ ExCall UntypedMarker left right
led _end (Operator k _ _ (OpSpecial (OpLeft l r))) left
  | k == Postfix = fail $ "Led: Wrong Associativity of operator " <> l
  | k == Prefix = do
    right <- term (string r *> spaces) 0
    return $ ExCall UntypedMarker left right
  | otherwise = fail $ "Expect one postfix special operator and actual operator " <> r <> " is infix"
led _end (Operator _ _ _ (OpSpecial (OpRight l r))) _
  = fail $ "Expect one prefix special operator and actual operator " <> r <> " is postfix or infix, missing paired " <> l

getOperator :: (Monad m, Foldable t, Eq a, Show a)
            => a -> ParsecT s (t (Operator a)) m (Operator a)
getOperator op = do
  stat <- getState
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined."

-- Type annotation, TODO: refactor type annotation with pratt parser
typName, typApply, typArrow, typArray, typRef :: Parser TypAnno
typName = do
  name <- identifier
  return $ TypName name
typApply = do
  typ <- typName <|> parens typApply
  typs <- many1 (typName <|> parens (try typArrow <|> typApply))
  return $ foldl1 (\a b -> TypApply a b) ([typ] ++ typs)
typArrow = do
  typs <- sepBy1 (try (parens typApply) <|> typArray <|> try typRef <|> try typApply <|> typName) (reservedOp "->")
  return $ foldr1 (\a b -> TypArrow a b) typs
typArray = do
  typ <- brackets $ try (parens typArrow) <|> try typRef <|> try typApply <|> typName
  return $ TypArray typ Nothing
typRef = do
  reserved "ref"
  typ <- try typApply <|> try typArray <|> typName
  return $ TypPtr typ

typParser :: Parser TypAnno
typParser = typArray <|> try typArrow <|> try typRef <|> try typApply <|> typName

-- lambda block parser
lambda :: Parser (LambdaBlock UntypedName)
lambda = do
  names <- maybe [] id <$> optionMaybe (try defVars)
  exprs <- semiSep (term (lookAhead $ (char ';' *> spaces) <|> char '}' *> return ()) 0)
  return $ LambdaBlock names exprs
  where
    varTypAnno = try (parens typArrow)
          <|> try typApply
          <|> typName
    typedVars = do
      name <- identifier
      typ <- optionMaybe (try $ reserved ":" >> varTypAnno)
      return $ (UntypedName name, typ)
    defVars = do -- to define block variable parameter
      names <- commaSep typedVars
      reserved "->"
      return names

-- Top level definition

-- function definition
defFunction :: Parser (ModuleElement UntypedName TypAnno)
defFunction = do
    reserved "fn"
    name <- identifier
    retTyp <- optionMaybe (reservedOp ":" >> retTypAnno)
    blk <- optionMaybe $ braces lambda
    case blk of
      Just _ -> return ()
      Nothing -> reservedOp ";"
    return $ ModuleFunction (UntypedName name) retTyp blk
      where retTypAnno = try typArrow <|> try typApply <|> typName

defBinding :: Parser (ModuleElement UntypedName TypAnno)
defBinding = do
  reserved "let"
  name <- identifier
  reservedOp ":"
  typ <- try (parens typArrow) <|> try typApply <|> typName
  reservedOp "="
  val <- term (lookAhead (reservedOp ";")) 0
  reservedOp ";"
  return $ ModuleBinding (UntypedName name) (Just typ) val

-- TODO: Introduce record type declaration syntax here
-- TODO: Build type alias
defSimpleType :: Parser (ModuleElement UntypedName TypAnno)
defSimpleType = do
  reserved "data"
  name <- identifier
  reservedOp "="
  typ <- typParser
  reservedOp ";"
  return $ ModuleType (UntypedName name) typ

language :: Parser a -> Parser a
language lang = do
  Token.whiteSpace lexer
  lang >>= \res -> eof >> return res

-- Toplevel definition
toplevel :: Parser [ModuleElement UntypedName TypAnno]
toplevel = many $ do
  def <- defSimpleType <|> defBinding <|> defFunction
  return def

-- parseExpr = runParser (language expr) [] "stdin"

parseToplevel :: String -> Either ParseError [ModuleElement UntypedName TypAnno]
parseToplevel = runParser (language toplevel) opTable "<stdin>"
