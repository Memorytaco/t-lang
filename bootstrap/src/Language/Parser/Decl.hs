{-# LANGUAGE AllowAmbiguousTypes #-}
module Language.Parser.Decl
  (

  -- ** direct method to define `Decl`
    declaration

  -- ** Decl related type level DSL
  , WithDecl
  , WithDataDef
  )
where

import Language.Core hiding (Type)
import Language.Core.Extension.Decl
import Tlang.Constraint (Prefix (..), Prefixes (..))
import qualified Language.Core as AST (Type)

import Language.Parser.Class
import Language.Parser.Lexer

import Control.Monad (void)
import Data.Functor ((<&>), ($>))
import Data.Text (Text, isPrefixOf)
import Text.Megaparsec hiding (Label)
import Data.Maybe (fromMaybe)
import Data.Kind (Type)

import Tlang.Generic

import Capability.Reader (HasReader, local)
import Capability.State (HasState, gets, modify)

type ParserM e m =
  ( ShowErrorComponent e
  , MonadParsec e Text m, MonadFail m
  , HasReader "TermOperator" [Operator Text] m
  , HasReader "TypeOperator" [Operator Text] m
  , HasReader "PatternOperator" [Operator Text] m
  )

-- | decl type level lang
--
-- * `e` is error
-- * `m` is monad
-- * `a` is lang token
data WithDecl (e :: Type) (m :: Type -> Type) (a :: k)

-- | define sequence operator
instance (ParserM e m, ParserDSL (WithDecl e m a) (Decl decl info) m, ParserDSL (WithDecl e m b) (Decl decl info) m)
  => ParserDSL (WithDecl e m (a :- b)) (Decl decl info) m where
  syntax _ end = try (runDSL @(WithDecl e m a) end) <|> runDSL @(WithDecl e m b) end

-- | foreign interface
instance (ParserM e m, PrattToken proxy typ m, info ~ Name, Item (FFI typ Name) :<: decl)
  => ParserDSL (WithDecl e m (Layer "ffi" proxy typ)) (Decl decl info) m where
  syntax _ end = do
    void $ reserved "foreign"
    attrs <- optional $ brackets (commaSep attr)
    name <- Name <$> identifier
    void $ reservedOp ":" <|> fail "FFI declaration requires type signature!"
    sig <- pratt @proxy @typ (lookAhead end) Go
    end $> declare (Item (FFI (fromMaybe (AttrS []) $ AttrS <$> attrs) sig name) name)
    where
      attr = str <|> int <|> sym <|> list <|> record
        where
          str = AttrT <$> stringLiteral
          int = AttrI <$> integer
          sym = AttrC <$> identifier <*> many attr
          list = AttrS <$> brackets (commaSep attr)
          record =
            let field = (,) <$> identifier <*> (reservedOp "=" >> attr)
             in braces $ AttrP <$> commaSep field

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
         , info ~ Name, typ ~ AST.Type tbind trep Name Name
         , Item (AliasType DataPrefix typ Name) :<: decl
         )
  => ParserDSL (WithDecl e m (Layer "type" proxy typ)) (Decl decl info) m where
  syntax _ end = do
    void $ reserved "type"
    alias <- name
    vars <- manyTill typVar (void $ reservedOp "=")
    body <- pratt @proxy @typ (lookAhead end) Go
    let item = AliasType (DataPrefix $ Prefixes vars) (toInteger $ length vars) body alias
    end $> declare (Item item alias)
    where
      name = Name <$> identifier <|> do
        op <- try $ lookAhead operator
        if ":" `isPrefixOf` op
           then Name <$> operator
           else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> show (Name op) <> "\" ?"
      typVar :: m (Prefix Name typ)
      typVar = identifier <&> (:> TypPht) . Name

data WithDataDef (e :: Type) (m :: Type -> Type) (a :: k)

instance (ParserM e m, PrattToken proxy typ m, label ~ Label, DataStruct label :<: x)
  => ParserDSL (WithDataDef e m (Layer "struct" proxy typ)) (DataBody x typ) m where
  syntax _ _ = braces do
    let fieldName = identifier <|> operator <&> Label
        field = do
          constructor <- fieldName <* reservedOp ":"
          typ <- pratt @proxy @typ (void . lookAhead $ reservedOp "," <|> reservedOp "}") Go
          return (constructor, typ)
    cs1 <- field <* optional (reservedOp ",")
    css <- commaSep field
    return $ DataBody (inj $ DataStruct cs1 css)

instance (ParserM e m, PrattToken proxy typ m, field ~ Label, typ ~ AST.Type tbind trep name a, a ~ Name, DataEnum field :<: x)
  => ParserDSL (WithDataDef e m (Layer "enum" proxy typ)) (DataBody x typ) m where
  syntax _ end = do
    let fieldName = identifier <|> operator <&> Label
        field = TypVar . Name <$> identifier
            <|> parens (pratt @proxy @typ (void . lookAhead $ reservedOp ")") Go)
        dataEnum = reservedOp "|" >> (,) <$> fieldName <*> many field
    DataEnum <$> dataEnum <*> dataEnum `manyTill` end <&> DataBody . inj

instance (ParserM e m, PrattToken proxy typ m, Identity :<: x)
  => ParserDSL (WithDataDef e m (Layer "coerce" proxy typ)) (DataBody x typ) m where
  syntax _ end = do
    val <- reservedOp "=" *> pratt @proxy @typ (lookAhead end $> ()) Go <&> DataBody . inj . Identity
    end $> val

instance (ParserM e m, PrattToken proxy typ m, DataNone :<: x)
  => ParserDSL (WithDataDef e m (Layer "phantom" proxy typ)) (DataBody x typ) m where
  syntax _ end = end $> DataBody (inj $ DataNone @typ)

-- | sequence parsing
instance ( ParserM e m
         , ParserDSL (WithDataDef e m a) (DataBody x typ) m
         , ParserDSL (WithDataDef e m b) (DataBody x typ) m
         )
  => ParserDSL (WithDataDef e m (a :- b)) (DataBody x typ) m where
  syntax _ end = runDSL @(WithDataDef e m a) end
           <|> runDSL @(WithDataDef e m b) end

instance ( ParserM e m
         , info ~ Name, typ ~ AST.Type tbind trep name a
         , Item (DataType DataPrefix xt typ Name) :<: decl
         , ParserDSL proxy def m, def ~ (xt typ)
         )
  => ParserDSL (WithDecl e m (Layer ("data" :- typ) proxy def)) (Decl decl info) m where
  syntax _ end = do
    dataName <- reserved "data" *> name
    vars :: [Prefix Name typ] <- manyTill typVar (lookAhead . choice . (end:) $ void . reservedOp <$> ["|", "{", "="])
    body <- runDSL @proxy @def (lookAhead end)
    let item = DataType (DataPrefix $ Prefixes vars) (toInteger $ length vars) body dataName
    end $> declare (Item item dataName)
    where
    name = Name <$> identifier <|> do
      op <- lookAhead operator
      if ":" `isPrefixOf` op
         then Name <$> operator
         else fail $ "type operator should be prefixed with \":\", maybe try " <> "\":" <> show (Name op) <> "\" ?"
    typVar = (:> TypPht) . Name <$> identifier

instance ( ParserM e m, info ~ Name, Item (UserOperator Text) :<: decl
         , HasState "OperatorStore" OperatorStore m
         )
  => ParserDSL (WithDecl e m "fixity") (Decl decl info) m where
  syntax _ end = do
    type'maybe <- reserved "operator" >> optional (reserved "type")
    prefix <- case type'maybe of
      Just _ -> return TypeOperator
      Nothing -> return TermOperator
    op@(Operator _ _ _ s) <- defOperator end
    modify @"OperatorStore" (prefix op:)
    return . declare $ Item (UserOperator $ prefix op) (Name s)

defOperator :: ParserM e m => m () -> m (Operator Text)
defOperator end = do
  left'maybe <- optional $ reserved "_"
  op <- operator
  right'maybe <- optional $ reserved "_"
  val <- precedence
  marker <- mconcat <$> some keyword
  end
  case (left'maybe, right'maybe) of
    (Just _, Nothing) -> return $ Operator marker (val * 10 - 1) (val * 10) op
    (Nothing, Just _) -> return $ Operator marker (val * 10) (val * 10 - 1) op
    (Just _, Just _) -> fail "only one \"_\" marker is allowed"
    _ -> fail "missing marker \"_\""
  where
    precedence = do
      fixity <- integer
      if fixity < 0 || fixity > 10
         then fail "precedence number is restricted from 0 to 10, including 0 and 10"
         else pure (fixity * 10)
    keyword = reserved "prefix" $> Prefix
           <|> reserved "postfix" $> Postfix
           <|> reserved "infix" $> Infix

declaration'
  :: ( ParserM e m
     , HasState "OperatorStore" OperatorStore m
     , ParserDSL proxy (Decl decl Name) m
     )
  => Proxy proxy -> m () -> m (Decl decl Name)
declaration' wit end = do
  (termOp, typeOp) <- gets @"OperatorStore" $ mconcat . fmap \case
    TypeOperator t -> ([], [t])
    TermOperator t -> ([t], [])
  local @"TypeOperator" (const typeOp)
    $ local @"TermOperator" (const termOp)
    $ local @"PatternOperator" (const termOp)
    $ syntax wit end

declaration
  :: forall proxy decl e m
   . ( ParserM e m
     , HasState "OperatorStore" OperatorStore m
     , ParserDSL proxy (Decl decl Name) m
     )
  => m () -> m (Decl decl Name)
declaration = declaration' (Proxy @proxy)
