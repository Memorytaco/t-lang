module Tlang.Parser.Decl
  ( declaration
  , DeclareExtension
  , play
  )
where

import Tlang.AST
import Tlang.Parser.Lexer
import qualified Tlang.Parser.Expr as ExprParser
import qualified Tlang.Parser.Type as TypeParser

import Control.Monad (void)
import Control.Monad.Trans.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

import Tlang.Extension.Decl
import Tlang.Generic

userFFI :: (ShowErrorComponent e, UserFFI TypeParser.ParseType :<: decls)
        => [Operator String] -> ParsecT e Text m (Decl decls Symbol)
userFFI r = do
  void $ reserved "foreign"
  item <- optional ffItem
  name <- Symbol <$> identifier
  void $ reservedOp ":" <|> fail "FFI declaration requires type signature!"
  sig <- TypeParser.unParser r (void . lookAhead $ reservedOp ";;") (-100)
  void $ reservedOp ";;"
  return . declare $ UserFFI item sig name
  where
    ffItem = brackets $ str <|> int <|> sym <|> list <|> record
      where
        str = FFItemS <$> stringLiteral
        int = FFItemI <$> integer
        sym = FFItemF <$> identifier <*> many ffItem
        list = FFItemA <$> brackets (commaSep ffItem)
        record =
          let field = (,) <$> stringLiteral <*> (reservedOp "=" >> ffItem)
           in braces $ FFItemR <$> (commaSep field)

userValue :: (ShowErrorComponent e, UserValue ExprParser.ParseExprType (Maybe TypeParser.ParseType) :<: decls)
          => ([Operator String], [Operator String]) -> ParsecT e Text m (Decl decls Symbol)
userValue r = do
  void $ reserved "let"
  name <- Symbol <$> identifier <|> Op <$> operator
  sig <- optional $ reservedOp ":" *> TypeParser.unParser (fst r) (void . lookAhead $ reservedOp "=" <|> reservedOp ";;") (-100)
  void $ reservedOp "=" <|> fail "let declaration requires a value definition"
  expr <- ExprParser.unParser r (void . lookAhead $ reservedOp ";;") (-100)
  void $ reservedOp ";;"
  return . declare $ UserValue expr sig name

userType :: (ShowErrorComponent e, UserType TypeParser.ParseType [Bound Symbol TypeParser.ParseType] :<: decls)
         => [Operator String] -> ParsecT e Text m (Decl decls Symbol)
userType r = do
  void $ reserved "type"
  alias <- name
  vars <- manyTill typVar (void $ reservedOp "=")
  body <- TypeParser.unParser r (lookAhead . void $ reservedOp ";;") (-100)
  void $ reservedOp ";;"
  return . declare $ UserType body vars alias
  where
    name = Symbol <$> identifier <|> do
      op <- lookAhead operator
      if head op == ':'
         then Op <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> op <> "\" ?"
    typVar :: ShowErrorComponent e => ParsecT e Text m (Bound Symbol TypeParser.ParseType)
    typVar = identifier <&> (:> TypPht) . Symbol

type UserDataExtension = UserEnum Label :+: UserStruct Label :+: UserCoerce :+: UserPhantom

userDataExt
  :: forall exts e m
  .  ( UserEnum Label :<: exts
     , UserStruct Label :<: exts
     , UserCoerce :<: exts
     , UserPhantom :<: exts
     , ShowErrorComponent e
     )
  => ([Operator String], [Operator String])
  -> ParsecT e Text m (UserData [Bound Symbol TypeParser.ParseType] exts TypeParser.ParseType Symbol)

userDataExt r = do
  void $ reserved "data"
  dataName <- name
  vars <- manyTill typVar (void . lookAhead . choice $ reservedOp <$> ["|", "{", "=", ";;"])
  let dataOf :: forall ext _m fs a. (ext :<: fs, Monad _m)
             => _m (ext a) -> _m (UserData [Bound Symbol TypeParser.ParseType] fs a Symbol)
      dataOf = fmap $ injData vars dataName
  dataDef <- dataOf userStruct <|> dataOf userEnums <|> dataOf userCoerce <|> dataOf (return UserPhantom)
  void $ reservedOp ";;"
  return dataDef
  where
    typVar = (:> TypPht) . Symbol <$> identifier 
    name = Symbol <$> identifier <|> do
      op <- lookAhead operator
      if head op == ':'
         then Op <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> op <> "\" ?"
    fieldName = identifier <|> operator <&> Tlang.AST.Label
    userStruct = braces do
      let field = do
            constructor <- fieldName <* reservedOp ":"
            typ <- TypeParser.unParser (fst r) (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100)
            return (UserStruct constructor typ)
      cs1 <- field <* optional (reservedOp ",")
      css <-  commaSep field
      return $ UserStructs cs1 css
    userEnum = reservedOp "|" *> (UserEnum <$> fieldName <*> many field)
      where
        field = TypRef . Symbol <$> identifier
            <|> parens (TypeParser.unParser (fst r) (void . lookAhead $ reservedOp ")") (-100))
    userEnums = UserEnums <$> userEnum <*> many userEnum
    userCoerce = reservedOp "=" >> UserCoerce <$> (TypeParser.unParser (fst r) (void . lookAhead $ reservedOp ";;") (-100))

userData
  :: ( ShowErrorComponent e
     , UserData [Bound Symbol TypeParser.ParseType] UserDataExtension TypeParser.ParseType :<: decls
     )
  => ([Operator String], [Operator String])
  -> ParsecT e Text m (Decl decls Symbol)
userData r = declare <$> userDataExt @UserDataExtension r

defFixity :: ShowErrorComponent e => ParsecT e Text m (Operator String)
defFixity = (defUnifix <|> defInfix) <* reservedOp ";;"
  where
    precedence = do
      fixity <- integer
      if fixity < 0 || fixity > 10
         then fail "precedence number is restricted from 0 to 10, including 0 and 10"
         else pure (fixity * 10)

    defUnifix = reserved "unifix" *> ((,) <$> precedence <*> precedence >>= \(lbp, rbp) -> Operator Unifix lbp rbp <$> operator)
            <|> reserved "prefix" *> (precedence >>= \bp -> Operator Prefix bp bp <$> operator)
            <|> reserved "postfix" *> (precedence >>= \bp -> Operator Postfix bp bp <$> operator)

    defInfix =  reserved "infixl" *> (precedence >>= \bp -> Operator Infix (bp-1) bp <$> operator)
            <|> reserved "infixr" *> (precedence >>= \bp -> Operator Infix bp (bp-1) <$> operator)
            <|> reserved "infix" *> (precedence >>= \bp -> Operator Infix bp bp <$> operator)

type DeclareExtension typ
  = UserItem
  :+: UserType typ [Bound Symbol typ]
  :+: UserFFI typ
  :+: UserValue ExprParser.ParseExprType (Maybe typ)
  :+: UserData [Bound Symbol typ] UserDataExtension typ

declaration
  :: (ShowErrorComponent e, Monad m)
  => ParsecT e Text (StateT ([Operator String], [Operator String]) m) (Decl (DeclareExtension TypeParser.ParseType) Symbol)
declaration = do
  r <- lift get
  userFFI (fst r) <|> userValue r <|> userType (fst r) <|> userData r <|> do
    op@(Operator _ _ _ s) <- defFixity
    lift . modify $ \(tys, trs) -> if head s == ':' then (op:tys, trs) else (tys, op:trs)
    return . declare $ UserItem (ItemSpace "default") [op] (Op s)

play :: (Monad f, Monad m)
     => ( ParsecT Void Text (StateT ([Operator String], [Operator String]) m) (Decl (DeclareExtension TypeParser.ParseType) Symbol)
     -> ParsecT Void Text (StateT s f) c)
     -> s -> Text -> f (Either String c)
play deco op txt =
  let m = flip evalStateT op $ runParserT (deco $ declaration @Void) "stdin" txt
   in first errorBundlePretty <$> m
