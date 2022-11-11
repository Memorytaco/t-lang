module Tlang.Parser
where

{-

As the file name suggests.

This module includes
- TypedName, UntypedName. we have UntypedName when building up AST, and then we resolve it to TypedName.
- TypeAnno. it is used to represent user type annotation and it also serves as initial type to literal value.

-}

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor.Identity

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Data.Functor.Foldable (Base)
import Data.Functor.Foldable.TH

import Tlang.Lexer.Lexer

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
                  deriving (Show, Eq)

data LitValue = LitInt Integer | LitNumber Double | LitString String deriving (Show, Eq)

getExprName :: Expr op name -> name
getExprName (ExLit n _) = n
getExprName (ExRef n) = n
getExprName (ExCall n _ _) = n
getExprName (ExBind n _ _) = n
getExprName (ExOp _ n _ _) = n

-- Operator
data Op = OpAdd | OpMinus | OpMulti | OpDivide deriving (Show, Eq, Ord)

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
    , lambdaExprs :: [Expr Op name]
    } deriving (Show, Eq)

-- language module definition
data LanguageModule name anno = LanguageModule String [ModuleElement name anno] deriving (Show, Eq)
data ModuleElement name anno
  = ModuleFunction name (Maybe anno) (Maybe (LambdaBlock name)) -- FIXME: type annotation for parameter is required
  | ModuleBinding  name (Maybe anno) (Expr Op name) -- name binding: for global constant or function alias
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
getLangModEleExpr :: ModuleElement name anno -> Maybe (Expr Op name)
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

mapExprNameF :: (name -> a) -> (a -> a -> a) -> Base (Expr op name) a -> a
mapExprNameF f _ (ExLitF n _) = f n
mapExprNameF f _ (ExRefF n) = f n
mapExprNameF f o (ExCallF n a1 a2) = f n `o` (a1 `o` a2)
mapExprNameF f o (ExBindF n _ a) = f n `o` a
mapExprNameF f o (ExOpF _ n a1 a2) = f n `o` (a1 `o` a2)

{-
-- parser definition
-}

-- Expression
binary
  :: String -> op -> Ex.Assoc
  -> Ex.Operator String () Data.Functor.Identity.Identity (Expr op UntypedName)
binary s f assoc = Ex.Infix (reservedOp s >> return (ExOp f UntypedMarker)) assoc

expr :: Parser (Expr Op UntypedName)
expr = Ex.buildExpressionParser table exprParser
    where table = [ [ binary "*" OpMulti Ex.AssocLeft
                    , binary "/" OpDivide Ex.AssocLeft
                    ]
                  , [ binary "+" OpAdd Ex.AssocLeft
                    , binary "-" OpMinus Ex.AssocLeft
                    ]
                  ]

variable, intLit, floatingLit, stringLit, exprApply, exprParser :: Parser (Expr Op UntypedName)
variable = do
    v <- identifier
    return (ExRef $ UntypedName v)
floatingLit = do
  n <- float
  return (ExLit UntypedMarker $ LitNumber n)
intLit = do
  n <- integer
  return (ExLit UntypedMarker $ LitInt n)
stringLit = do
  s <- stringLiteral
  return (ExLit UntypedMarker $ LitString s)
exprApply = do
  ops <- many $ parens expr <|> stringLit <|> (try floatingLit <|> try intLit) <|> variable
  return $ foldl1 (ExCall UntypedMarker) ops
exprParser = try floatingLit
    <|> try intLit
    <|> try stringLit
    <|> try exprApply 
    <|> variable
    <|> parens expr

-- Type annoation
typName, typApply, typArrow, typArray :: Parser TypAnno
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
  exprs <- semiSep expr
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
      Nothing -> reservedOp ";" -- FIXME: required a semicolon
    return $ ModuleFunction (UntypedName name) retTyp blk
      where retTypAnno = try typArrow <|> try typApply <|> typName

defBinding :: Parser (ModuleElement UntypedName TypAnno)
defBinding = do
  reserved "let"
  name <- identifier
  reservedOp ":"
  typ <- try (parens typArrow) <|> try typApply <|> typName
  reservedOp "="
  val <- expr
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
language p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

-- Toplevel definition
toplevel :: Parser [ModuleElement UntypedName TypAnno]
toplevel = many $ do
  def <- defSimpleType <|> defBinding <|> defFunction
  return def

parseToplevel :: String -> Either ParseError [ModuleElement UntypedName TypAnno]
parseToplevel s = parse (language toplevel) "<stdin>" s
