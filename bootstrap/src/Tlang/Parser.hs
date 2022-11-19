module Tlang.Parser
  ( UntypedName (..)
  , TypedName (..)
  , Expr (..)
  , ExprF (..)
  , LitValue (..)
  , TypAnno (..)
  , TypAnnoF (..)
  , LambdaBlock (..)
  , ModuleElement (..)

  , Parser

  , getTypedNameT
  , getExprName
  , getLangModEleNamePair
  , getLangModEleBlock
  , onExprNameF
  , mapExprNameF

  , parseToplevel
  , parseExpr
  , parseType

  , builtinOperator

  , lambda
  , unSolvedModule
  , parseTopModule
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

import Tlang.Lexer.Lexer
import Data.List (find, intercalate, partition, union, nub)
import Control.Monad (forM)
import Control.Monad.Except (liftEither)

import Tlang.Parser.Pratt

import Tlang.AST

type ParsedModule name anno = Module [(ModuleNameSpace, [ModuleElement name anno])] [ModuleElement name anno]

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
type Parser = PrattParser (Either TypAnno (Expr (Operator String) UntypedName))

builtinOperator :: [Operator (OperatorClass v)]
builtinOperator =
  [ Operator Infix 55 60 (OpAction "+")
  , Operator Infix 80 80 (OpAction "*")
  , Operator Infix 80 80 (OpAction "/")
  , Operator Prefix 90 90 (OpAction "-")
  , Operator Unifix 70 70 (OpAction "!")
  , Operator Infix 30 30 (OpAction "?")
  , Operator Infix 40 40 (OpAction ":=")
  , Operator Infix 10 10 (OpAction "..")  -- ^ Range operator

  , Operator Prefix 900 900 (OpAction "ref")

  , Operator Infix 10 5 (OpAction "->")
  , Operator Prefix 9999 9999 . OpSpecial $ OpLeft "(" ")"
  , Operator Postfix 9999 9999 . OpSpecial $ OpRight "(" ")"
  , Operator Prefix 9999 9999 . OpSpecial $ OpLeft "[" "]"
  , Operator Postfix 9999 9999 . OpSpecial $ OpRight "[" "]"
  ]

term :: Parser () -> Integer -> Parser (Expr (Operator String) UntypedName)
term = pratt nud' led'
  where
    nud' = prattNud expr \end tok ->
      case tok of
        OpNorm v -> exprNud end (Operator Unifix 999 999 (OpNorm v))
        _ -> getOperator tok >>= exprNud (lookAhead end)
    led' = prattLed bindPower expr \end tok left ->
      case tok of
        OpNorm v -> exprLed end (Operator Unifix 999 999 (OpNorm v)) left
        _ -> do
          op <- getOperator tok
          exprLed (lookAhead end) op left
      where
      bindPower (OpNorm _) = return 999
      bindPower op = do
        (Operator _ lbp _ _) <- getOperator op
        return lbp

expr :: Parser (OperatorClass (Either a (Expr op UntypedName)))
expr = (OpNorm . Right <$> variable <?> "Identifier")
    <|> (OpNorm . Right <$> num <?> "Literal Number")
    <|> (OpNorm . Right <$> str <?> "Literal String")
    <|> (OpAction <$> operator <?> "Expr Operator")
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

-- nud and led function generator for expression
exprNud :: Parser ()
    -> Operator (OperatorClass (Either a (Expr (Operator String) UntypedName)))
    -> Parser (Expr (Operator String) UntypedName)
exprNud end (Operator k l r (OpAction n))
  | k == Infix = fail $ "Operator " <> n <> " is an infix operator, lack left value"
  | k == Unifix || k == Prefix = do
    right <- term end r
    return $ ExOpUni (Operator Unifix l r n) UntypedMarker right
  | otherwise = fail $ "Operator " <> n <> " is a postfix operator, but appear in a prefix position"
exprNud _end (Operator _ _ _ (OpNorm v)) = either (const $ fail "Type level value should not appear in term level") return v
exprNud _end (Operator k _ _ (OpSpecial (OpLeft l r)))
  | k == Prefix = term (string r *> spaces) 0
  | otherwise = fail $ "Wrong associativity with a left operator " <> l
exprNud _end (Operator _ _ _ (OpSpecial (OpRight l r)))
  = fail $ "Expect one prefix special operator and actual operator " <> r <> " is postfix or infix, missing paired " <> l

exprLed :: Parser ()
    -> Operator (OperatorClass (Either TypAnno (Expr (Operator String) UntypedName)))
    -> Expr (Operator String) UntypedName
    -> Parser (Expr (Operator String) UntypedName)
exprLed end (Operator k l r (OpAction n)) left
  | k == Infix = do
    right <- term end r
    return $ ExOp (Operator Infix l r n) (UntypedMarker) left right
  | k == Unifix || k == Postfix = do
    return $ ExOpUni (Operator Unifix l r n) UntypedMarker left
  | otherwise = fail $ "Operator " <> n <> " is a prefix operator, but appear in a postfix position"
exprLed _end (Operator _ _ _ (OpNorm right)) left = do
  either (const $ fail "Type level value should not appear in term level") (return . ExCall UntypedMarker left) right
exprLed _end (Operator k _ _ (OpSpecial (OpLeft l r))) left
  | k == Postfix = fail $ "Led: Wrong Associativity of operator " <> l
  | k == Prefix = do
    right <- term (string r *> spaces) 0
    return $ ExCall UntypedMarker left right
  | otherwise = fail $ "Expect one postfix special operator and actual operator " <> r <> " is infix"
exprLed _end (Operator _ _ _ (OpSpecial (OpRight l r))) _
  = fail $ "Expect one prefix special operator and actual operator " <> r <> " is postfix or infix, missing paired " <> l

getOperator :: (Monad m, Foldable t, Eq a, Show a)
            => a -> ParsecT s (t (Operator a)) m (Operator a)
getOperator op = do
  stat <- getState
  case find (\(Operator _ _ _ n) -> n == op) stat of
    Just a -> return a
    Nothing -> fail $ "Operator " <> show op <> " is undefined."

annotation :: Parser () -> Integer -> Parser TypAnno
annotation = pratt nud' led'
  where
    nud' = prattNud typTok \end tok ->
      case tok of
        (OpNorm v) -> either return (const $ fail "Value appear in type level is not allowed") v
        (OpAction s) ->
          case s of
            "ref" -> annotation end 0 >>= return . TypPtr
            _ -> fail $ s <> " doesn't support partial type application"
        (OpSpecial (OpLeft l r)) -> do
          if l == "(" && r == ")"
          then annotation (string r *> spaces) 0
          else flip TypArray Nothing <$> annotation (string r *> spaces) 0
        _ -> fail "Invalid type level token"
    led' = prattLed bindingPower typTok \end tok left -> do
      case tok of
        OpNorm _ -> typParserLed end (Operator Unifix 999 999 tok) left
        _ -> getOperator tok >>= \op -> typParserLed (lookAhead end) op left
      where
        bindingPower :: (OperatorClass (Either TypAnno (Expr (Operator String) UntypedName)))  -> Parser Integer
        bindingPower (OpNorm _) = return 999
        bindingPower op = getOperator op >>= \(Operator _ lbp _ _) -> pure lbp
    typParserLed end (Operator k _ r (OpAction n)) left
      | k == Infix = do
        right <- annotation end r
        return $ TypArrow left right
      | otherwise = fail $ "Wrong place for operator " <> n
    typParserLed _end (Operator _ _ _ (OpNorm v)) left = either (return . TypApply left) (const $ fail "Value appear in type level is not allowed") v
    typParserLed _ (Operator _ _ _ (OpSpecial (OpLeft l r))) left
      | l == "[" && r == "]" = do
        right <- annotation (string r *> spaces) 0
        return . TypApply left $ TypArray right Nothing
      | l == "(" && r == ")" = do
        right <- annotation (string r *> spaces) 0
        return $ TypApply left right
      | otherwise = fail "Unrecognized pair operator in type level"
    typParserLed _ _ _ = fail "Internal error in type parser"

typTok :: Parser (OperatorClass (Either TypAnno b))
typTok = (OpNorm . Left <$> typNormal <?> "Type Name")
     <|> (OpAction <$> typOperator <?> "Type Operator")
     <|> (OpSpecial <$> special <?> "Type Pair Operator")
  where
    typNormal = identifier >>= return . TypName
    typOperator = reservedOp "->" *> pure "->"
              <|> reserved "ref" *> pure "ref"
    withSpecial (l, r) = string l *> spaces *> pure (OpLeft l r)
                     <|> string r *> spaces *> pure (OpRight l r)
    special = foldr1 (<|>) $ withSpecial <$> [("(", ")"), ("[", "]")]

-- lambda block parser
lambda :: Parser (LambdaBlock UntypedName)
lambda = do
  names <- maybe [] id <$> optionMaybe (try defVars)
  exprs <- semiSep (term (lookAhead $ oneOf ";}" *> return ()) 0)
  return $ LambdaBlock names exprs
  where
    typedVars = do
      name <- identifier
      let anno = reservedOp ":" *> annotation (lookAhead $ reservedOp "," <|> reservedOp "=>") 0
      typ <- optionMaybe anno <?> "Lambda Type Annotation"
      return $ (UntypedName name, typ)
    defVars = do -- to define block variable parameter
      names <- commaSep typedVars
      reservedOp "=>"
      return names

unSolvedModule :: Parser (Module [(ModuleNameSpace, [String])] String)
unSolvedModule = do
  reserved "mod"
  moduleName <- intercalate "/" <$> sepBy identifier (string "/") <|> identifier
  reservedOp ";"
  stats <- many useStatement
  defs <- many anyChar
  eof
  return $ Module (ModuleNameSpace moduleName) stats defs
  where
    useStatement :: Parser (ModuleNameSpace, [String])
    useStatement = do
      reserved "use"
      moduleName <- fmap ModuleNameSpace $ intercalate "/" <$> sepBy identifier (string "/") <|> identifier
      useList <- optionMaybe (braces $ sepBy (identifier <|> parens operator) (char ',' *> spaces))
      reservedOp ";"
      case useList of
        Nothing -> return (moduleName, [])
        Just ls -> return (moduleName, ls)

validateModule ::[Module [(ModuleNameSpace, [String])] String] -> Maybe [ModuleNameSpace]
validateModule ls =
  let it (Module name deps _) (rs, ps) = ([name] `union` rs, (fst <$> deps) `union` ps)
      (provided, required) = foldr it ([], []) ls
  in case filter (not . (`elem` provided)) required of
       [] -> Nothing
       as -> Just as

parseModule :: MonadFail m
            => [Module [(ModuleNameSpace, [String])] String]
            -> [ParsedModule UntypedName TypAnno]
            -> m [ParsedModule UntypedName TypAnno]
parseModule [] ls = return ls
parseModule ls [] = do
  case validateModule ls of
    Nothing -> return ()
    Just ns -> fail $ "Miss Module: " <> intercalate ", " (show <$> ns)
  let (as, ts) = partition (\(Module _ deps _) -> deps == []) ls
  case as of
    [] -> fail $ "Missing base module with zero use statements"
    _  -> do
      rs <- forM as \(Module nm deps content) -> do
        case parseTop builtinOperator content of
          Left err -> fail $ show err
          Right res -> return (Module nm [] res)
      parseModule ts rs
parseModule ls ss = do
  let ns = (\(Module nm _ _) -> nm) <$> ss  -- get Module name list
      (as, ts) = flip partition ls \(Module _ deps _) -> and $ ((`elem` ns) . fst) <$> deps
  case as of
    [] -> fail $ "UnSaturated Module Existed"
    _ -> do
      rs <- forM as \(Module nm deps content) -> do
        rdeps <- forM deps lookUpEle
        case parseTop (nub $ (foldr1 (<>) $ fst <$> rdeps) <> builtinOperator) content of
          Left err -> fail $ show err
          Right es -> return $ Module nm (snd <$> rdeps) es
      parseModule ts (rs <> ss)
  where
    lookUpEle (nm, es) = do
      (Module _ _ m) <- case find (\(Module n _ _) -> n == nm) ss of
             Just v -> return v
             Nothing -> fail $ "Can't find Module " <> show nm
      syms <- forM es \s -> do
        let res'maybe = flip find m \case
              (ModuleFunction (UntypedName n) _ _) -> n == s
              (ModuleBinding (UntypedName n) _ _) -> n == s
              (ModuleOperator (Operator _ _ _ n)) -> n == s
              (ModuleType (UntypedName n) _) -> n == s
              (ModuleUnsafe (UntypedName n) _) -> n == s
              _ -> False
        case res'maybe of
          Nothing -> fail $ "Module " <> show nm <> " doesn't export symbol " <> s
          Just t -> return t
      let getOperator (ModuleOperator v) = [v]
          getOperator _ = []
          opDefs = fmap OpAction <$> foldr1 (<>) (fmap getOperator syms)
      return (opDefs, (nm, syms))

-- parseTopModule :: [String] -> Parser [ParsedModule UntypedName TypAnno]
parseTopModule files = do
  ms <- liftEither . sequence $ runParser unSolvedModule builtinOperator "unknown" <$> files
  parseModule ms []

-- Top level definition

-- function definition

defFunction :: Parser (ModuleElement UntypedName TypAnno)
defFunction = do
    reserved "fn"
    name <- identifier
    retTyp <- optionMaybe (reservedOp ":" >> annotation (lookAhead $ reservedOp "{" <|> reservedOp ";") 0)
    blk <- optionMaybe $ braces lambda
    case blk of
      Just _ -> return ()
      Nothing -> reservedOp ";"
    return $ ModuleFunction (UntypedName name) retTyp blk

defBinding :: Parser (ModuleElement UntypedName TypAnno)
defBinding = do
  reserved "let"
  name <- identifier
  reservedOp ":"
  typ <- try $ annotation (lookAhead $ reservedOp "=") 0
  reservedOp "="
  val <- term (lookAhead (reservedOp ";")) 0
  reservedOp ";"
  return $ ModuleBinding (UntypedName name) (Just typ) val

defOperator :: Parser (ModuleElement UntypedName TypAnno)
defOperator = do
  op@(Operator k l r name) <- defUnifix <|> defInfix
  modifyState (\ls -> Operator k l r (OpAction name): ls)
  return $ ModuleOperator op
  where
    defUnifixID = reserved "unifix" *> pure Unifix
              <|> reserved "prefix" *> pure Prefix
              <|> reserved "postfix" *> pure Postfix
    defInfixID =  reserved "infixl" *> pure (\n -> Operator Infix (n-1) n)
              <|> reserved "infixr" *> pure (\n -> Operator Infix n (n-1))
              <|> reserved "infix" *> pure (\n -> Operator Infix n n)
    check precedence
      | precedence < 0 || precedence > 10 = fail "precedence number is restricted from 0 to 10"
      | otherwise = return ()
    defUnifix = do
      keyword <- defUnifixID
      (lbp, rbp) <-
        case keyword of
          Unifix -> do
            rbp <- natural
            lbp <- natural
            return (lbp, rbp)
          _ -> do
            bp <- natural
            return (bp, bp)
      check lbp
      check rbp
      op <- operator
      reservedOp ";"
      return $ Operator keyword (lbp *10) (rbp *10) op
    defInfix = do
      app <- defInfixID
      rlbp <- natural
      check rlbp
      op <- operator
      reservedOp ";"
      return $ app (rlbp *10) op

-- TODO: Introduce record type declaration syntax here
-- TODO: Build type alias
defSimpleType :: Parser (ModuleElement UntypedName TypAnno)
defSimpleType = do
  reserved "data"
  name <- identifier
  reservedOp "="
  typ <- annotation (lookAhead $ reservedOp ";") 0
  reservedOp ";"
  return $ ModuleType (UntypedName name) typ

language :: Parser a -> Parser a
language lang = do
  Token.whiteSpace lexer
  lang >>= \res -> eof >> return res

-- Toplevel definition
toplevel :: Parser [ModuleElement UntypedName TypAnno]
toplevel = many $ do
  def <- defSimpleType <|> defBinding <|> defFunction <|> defOperator
  return def

parseType :: [Operator (OperatorClass (Either TypAnno (Expr (Operator String) UntypedName)))]
          -> String -> Either ParseError TypAnno
parseType stat = runParser (annotation eof 0) stat "<stdin>"
parseExpr :: [Operator (OperatorClass (Either TypAnno (Expr (Operator String) UntypedName)))]
          -> String -> Either ParseError (Expr (Operator String) UntypedName)
parseExpr stat = runParser (term eof 0) stat "<stdin>"

parseTop tb = runParser (language toplevel) tb "<stdin>"

parseToplevel :: String -> Either ParseError [ModuleElement UntypedName TypAnno]
parseToplevel = parseTop builtinOperator
