module Tlang.Parser.Decl
  ( declaration
  , DeclareExtension
  )
where

import Tlang.AST
import Tlang.Parser.Class (pratt)
import Tlang.Parser.Lexer
import Tlang.Parser.Expr (ParseExprType, WithExpr)
import Tlang.Parser.Type (ParseType, WithType)

import Control.Monad (void)
import Data.Functor ((<&>), ($>))
import Data.Text (Text)
import Text.Megaparsec
import Data.Maybe (fromMaybe)

import Tlang.Extension.Decl
import Tlang.Generic

import Capability.Reader (HasReader, local)
import Capability.State (HasState, get, modify)

type ParserM e m =
  ( ShowErrorComponent e
  , MonadParsec e Text m, MonadFail m
  , HasReader "TermOperator" [Operator String] m
  , HasReader "TypeOperator" [Operator String] m
  , HasReader "PatternOperator" [Operator String] m
  )

userFFI :: forall m e decls. (ParserM e m, UserFFI ParseType :<: decls)
        => m () -> m (Decl decls Symbol)
userFFI end = do
  void $ reserved "foreign"
  items <- optional $ brackets (commaSep ffItem)
  name <- Symbol <$> identifier
  void $ reservedOp ":" <|> fail "FFI declaration requires type signature!"
  sig <- pratt @(WithType m) (lookAhead end) (-100)
  end $> declare (UserFFI (fromMaybe [] items) sig name)
  where
    ffItem = str <|> int <|> sym <|> list <|> record
      where
        str = FFItemS <$> stringLiteral
        int = FFItemI <$> integer
        sym = FFItemF <$> identifier <*> many ffItem
        list = FFItemA <$> brackets (commaSep ffItem)
        record =
          let field = (,) <$> stringLiteral <*> (reservedOp "=" >> ffItem)
           in braces $ FFItemR <$> commaSep field

userValue :: forall e m decls. (ParserM e m, UserValue ParseExprType (Maybe ParseType) :<: decls)
          => m () -> m (Decl decls Symbol)
userValue end = do
  void $ reserved "let"
  name <- Symbol <$> identifier <|> Op <$> operator
  sig <- optional $ reservedOp ":" *> pratt @(WithType m) ((void . lookAhead $ reservedOp "=") <|> end) (-100)
  void $ reservedOp "=" <|> fail "let declaration requires a value definition"
  expr <- pratt @(WithExpr m) (lookAhead end) (-100)
  end $> declare (UserValue expr sig name)

userType :: forall e m decls. (ParserM e m, UserType ParseType [Bound Symbol ParseType] :<: decls)
         => m () -> m (Decl decls Symbol)
userType end = do
  void $ reserved "type"
  alias <- name
  vars <- manyTill typVar (void $ reservedOp "=")
  body <- pratt @(WithType m) (lookAhead end) (-100)
  end $> declare (UserType body vars alias)
  where
    name = Symbol <$> identifier <|> do
      op <- lookAhead operator
      if head op == ':'
         then Op <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> op <> "\" ?"
    typVar :: m (Bound Symbol ParseType)
    typVar = identifier <&> (:> TypPht) . Symbol

type UserDataExtension = UserEnum Label :+: UserStruct Label :+: UserCoerce :+: UserPhantom

userDataExt
  :: forall exts e m
  .  ( UserEnum Label :<: exts
     , UserStruct Label :<: exts
     , UserCoerce :<: exts
     , UserPhantom :<: exts
     , ParserM e m
     )
  => m () -> m (UserData [Bound Symbol ParseType] exts ParseType Symbol)

userDataExt end = do
  void $ reserved "data"
  dataName <- name
  vars <- manyTill typVar (lookAhead . choice . (end:) $ void . reservedOp <$> ["|", "{", "="])
  let dataOf :: forall ext _m fs a. (ext :<: fs, Monad _m)
             => _m (ext a) -> _m (UserData [Bound Symbol ParseType] fs a Symbol)
      dataOf = fmap $ injData vars dataName
  dataDef <- dataOf userStruct <|> dataOf userEnums <|> dataOf userCoerce <|> dataOf (return UserPhantom)
  end $> dataDef
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
            typ <- pratt @(WithType m) (void . lookAhead $ reservedOp "," <|> reservedOp "}") (-100)
            return (UserStruct constructor typ)
      cs1 <- field <* optional (reservedOp ",")
      css <-  commaSep field
      return $ UserStructs cs1 css
    userEnum = reservedOp "|" *> (UserEnum <$> fieldName <*> many field)
      where
        field = TypRef . Symbol <$> identifier
            <|> parens (pratt @(WithType m) (void . lookAhead $ reservedOp ")") (-100))
    userEnums = UserEnums <$> userEnum <*> many userEnum
    userCoerce = reservedOp "=" >> UserCoerce <$> pratt @(WithType m) (lookAhead end) (-100)

userData
  :: ( ParserM e m
     , UserData [Bound Symbol ParseType] UserDataExtension ParseType :<: decls
     )
  => m () -> m (Decl decls Symbol)
userData end = declare <$> userDataExt @UserDataExtension end

defFixity :: ParserM e m => m () -> m (Operator String)
defFixity end = (defUnifix <|> defInfix) <* end
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
  :+: UserValue ParseExprType (Maybe typ)
  :+: UserData [Bound Symbol typ] UserDataExtension typ

declaration
  :: ( ParserM e m
     , HasState "TypeOperator" [Operator String] m
     , HasState "TermOperator" [Operator String] m
     )
  => m () -> m (Decl (DeclareExtension ParseType) Symbol)
declaration end = do
  opty <- get @"TypeOperator"
  optm <- get @"TermOperator"
  local @"TypeOperator" (const opty) $ local @"TermOperator" (const optm) $ local @"PatternOperator" (const optm) $
    userFFI end <|> userValue end <|> userType end <|> userData end <|> do
      op@(Operator _ _ _ s) <- defFixity end
      if head s == ':' then modify @"TypeOperator" (op:) else modify @"TermOperator" (op:)
      return . declare $ UserItem (ItemSpace "default") [op] (Op s)

-- play :: (Monad f, Monad m)
--      => ( ParsecT Void Text (StateT ([Operator String], [Operator String]) m) (Decl (DeclareExtension ParseType) Symbol)
--      -> ParsecT Void Text (StateT s f) c)
--      -> s -> Text -> f (Either String c)
-- play deco op txt =
--   let m = flip evalStateT op $ runParserT (deco $ declaration @Void) "stdin" txt
--    in first errorBundlePretty <$> m
