{-# LANGUAGE AllowAmbiguousTypes #-}
module Tlang.Parser.Decl
  (

  -- ** direct method to define `Decl`
    declaration

  -- ** Decl related type level DSL
  , WithDecl
  , WithDataDef
  )
where

import Tlang.AST hiding (Type)
import qualified Tlang.AST as AST (Type)

import Tlang.Parser.Class
import Tlang.Parser.Lexer

import Control.Monad (void)
import Data.Functor ((<&>), ($>))
import Data.Text (Text, isPrefixOf)
import Text.Megaparsec hiding (Label)
import Data.Maybe (fromMaybe)
import Data.Kind (Type)

import Tlang.Extension.Decl
import Tlang.Generic

import Capability.Reader (HasReader, local)
import Capability.State (HasState, get, modify)

type ParserM e m =
  ( ShowErrorComponent e
  , MonadParsec e Text m, MonadFail m
  , HasReader "TermOperator" [Operator Text] m
  , HasReader "TypeOperator" [Operator Text] m
  , HasReader "PatternOperator" [Operator Text] m
  )

data WithDecl (e :: Type) (m :: Type -> Type) (a :: k)

-- | define sequence operator
instance (ParserM e m, ParserDSL (WithDecl e m a) (Decl decl info) m, ParserDSL (WithDecl e m b) (Decl decl info) m)
  => ParserDSL (WithDecl e m (a :- b)) (Decl decl info) m where
  syntax _ end = try (runDSL @(WithDecl e m a) end) <|> runDSL @(WithDecl e m b) end

-- | foreign interface
instance (ParserM e m, PrattToken proxy typ m, info ~ Name, UserFFI typ :<: decl)
  => ParserDSL (WithDecl e m (Layer "ffi" proxy typ)) (Decl decl info) m where
  syntax _ end = do
    void $ reserved "foreign"
    items <- optional $ brackets (commaSep ffItem)
    name <- Name <$> identifier
    void $ reservedOp ":" <|> fail "FFI declaration requires type signature!"
    sig <- pratt @proxy @typ (lookAhead end) Go
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

-- | top level definition
instance (ParserM e m, PrattToken tProxy typ m, ParserDSL eProxy expr m, info ~ Name, UserValue expr (Maybe typ) :<: decl)
  => ParserDSL (WithDecl e m (Layer "define" (tProxy :- eProxy) (typ :- expr))) (Decl decl info) m where
  syntax _ end = do
    void $ reserved "let"
    name <- fmap Name $ identifier <|> operator
    sig <- optional $ reservedOp ":" *> pratt @tProxy @typ ((void . lookAhead $ reservedOp "=") <|> end) Go
    void $ reservedOp "=" <|> fail "let declaration requires a value definition"
    expr <- runDSL @eProxy @expr (lookAhead end)
    end $> declare (UserValue expr sig name)

instance ( ParserM e m, PrattToken proxy typ m
         , info ~ Name, typ ~ AST.Type trep tcons tbind tinj Name
         , UserType typ [Bound Name typ] :<: decl
         )
  => ParserDSL (WithDecl e m (Layer "type" proxy typ)) (Decl decl info) m where
  syntax _ end = do
    void $ reserved "type"
    alias <- name
    vars <- manyTill typVar (void $ reservedOp "=")
    body <- pratt @proxy @typ (lookAhead end) Go
    end $> declare (UserType body vars alias)
    where
      name = Name <$> identifier <|> do
        op <- try $ lookAhead operator
        if ":" `isPrefixOf` op
           then Name <$> operator
           else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> show (Name op) <> "\" ?"
      typVar :: m (Bound Name typ)
      typVar = identifier <&> (:> TypPht) . Name

data WithDataDef (e :: Type) (m :: Type -> Type) (a :: k)

instance (ParserM e m, PrattToken proxy typ m, UserStruct Label :<: ext)
  => ParserDSL (WithDataDef e m (Layer "struct" proxy typ)) (UserDataDef ext typ) m where
  syntax _ end = braces do
    let fieldName = identifier <|> operator <&> Label
        field = do
          constructor <- fieldName <* reservedOp ":"
          typ <- pratt @proxy @typ (void . lookAhead $ reservedOp "," <|> reservedOp "}") Go
          return (UserStruct constructor typ)
    cs1 <- field <* optional (reservedOp ",")
    css <- commaSep field
    end $> UserDataDef (inj $ UserStructs cs1 css)

instance (ParserM e m, PrattToken proxy typ m, UserEnum Label :<: ext, typ ~ AST.Type trep tcons tbind tinj Name)
  => ParserDSL (WithDataDef e m (Layer "enum" proxy typ)) ((UserDataDef ext typ)) m where
  syntax _ end = do
    let fieldName = identifier <|> operator <&> Label
        field = TypVar . Name <$> identifier
            <|> parens (pratt @proxy @typ (void . lookAhead $ reservedOp ")") Go)
        userEnum = reservedOp "|" >> UserEnum <$> fieldName <*> many field
    UserEnums <$> userEnum <*> userEnum `manyTill` end <&> UserDataDef . inj

instance (ParserM e m, PrattToken proxy typ m, UserCoerce :<: ext)
  => ParserDSL (WithDataDef e m (Layer "coerce" proxy typ)) ((UserDataDef ext typ)) m where
  syntax _ end = reservedOp "=" *> pratt @proxy @typ (lookAhead end $> ()) Go
           <&> UserDataDef . inj . UserCoerce

instance (ParserM e m, PrattToken proxy typ m, UserPhantom :<: ext)
  => ParserDSL (WithDataDef e m (Layer "phantom" proxy typ)) ((UserDataDef ext typ)) m where
  syntax _ end = end $> UserDataDef (inj UserPhantom)

-- | sequence parsing
instance ( ParserM e m
         , ParserDSL (WithDataDef e m a) (UserDataDef ext typ) m
         , ParserDSL (WithDataDef e m b) (UserDataDef ext typ) m
         )
  => ParserDSL (WithDataDef e m (a :- b)) (UserDataDef ext typ) m where
  syntax _ end = runDSL @(WithDataDef e m a) end
           <|> runDSL @(WithDataDef e m b) end

instance ( ParserM e m
         , info ~ Name, UserData [Bound Name typ] def :<: decl
         , typ ~ AST.Type trep tcons tbind tinj Name
         , ParserDSL proxy def m
         )
  => ParserDSL (WithDecl e m (Layer ("data" :- typ) proxy def)) (Decl decl info) m where
  syntax _ end = do
    dataName <- reserved "data" *> name
    vars :: [Bound Name typ] <- manyTill typVar (lookAhead . choice . (end:) $ void . reservedOp <$> ["|", "{", "="])
    body <- runDSL @proxy @def (lookAhead end)
    end $> declare (UserData dataName vars body)
    where
    name = Name <$> identifier <|> do
      op <- lookAhead operator
      if ":" `isPrefixOf` op
         then Name <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> show (Name op) <> "\" ?"
    typVar = (:> TypPht) . Name <$> identifier

instance ( ParserM e m, info ~ Name, UserItem :<: decl
         , HasState "TermOperator" [Operator Text] m
         , HasState "TypeOperator" [Operator Text] m
         )
  => ParserDSL (WithDecl e m "fixity") (Decl decl info) m where
  syntax _ end = do
    op@(Operator _ _ _ s) <- defFixity end
    if ":" `isPrefixOf` s then modify @"TypeOperator" (op:) else modify @"TermOperator" (op:)
    return . declare $ UserItem (ItemSpace "default") [op] (Name s)

defFixity :: ParserM e m => m () -> m (Operator Text)
defFixity end = (defUnifix <|> defInfix) <* end
  where
    precedence = do
      fixity <- integer
      if fixity < 0 || fixity > 10
         then fail "precedence number is restricted from 0 to 10, including 0 and 10"
         else pure (fixity * 10)

    defUnifix = reserved "unifix" *> ((,) <$> precedence <*> precedence >>= \(l, r) -> Operator Unifix l r <$> operator)
            <|> reserved "prefix" *> (precedence >>= \bp -> Operator Prefix bp bp <$> operator)
            <|> reserved "postfix" *> (precedence >>= \bp -> Operator Postfix bp bp <$> operator)

    defInfix =  reserved "infixl" *> (precedence >>= \bp -> Operator Infix (bp-1) bp <$> operator)
            <|> reserved "infixr" *> (precedence >>= \bp -> Operator Infix bp (bp-1) <$> operator)
            <|> reserved "infix" *> (precedence >>= \bp -> Operator Infix bp bp <$> operator)

declaration'
  :: ( ParserM e m
     , HasState "TypeOperator" [Operator Text] m
     , HasState "TermOperator" [Operator Text] m
     , ParserDSL proxy (Decl decl Name) m
     )
  => Proxy proxy -> m () -> m (Decl decl Name)
declaration' wit end = do
  opty <- get @"TypeOperator"
  optm <- get @"TermOperator"
  local @"TypeOperator" (const opty)
    $ local @"TermOperator" (const optm)
    $ local @"PatternOperator" (const optm)
    $ syntax wit end

declaration
  :: forall proxy decl e m
   . ( ParserM e m
     , HasState "TypeOperator" [Operator Text] m
     , HasState "TermOperator" [Operator Text] m
     , ParserDSL proxy (Decl decl Name) m
     )
  => m () -> m (Decl decl Name)
declaration = declaration' (Proxy @proxy)
