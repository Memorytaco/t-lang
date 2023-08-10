{- | * Useful driver to parse texts
-}
module Driver.Parser
  ( OperatorSpace (..)

  , ParserMonad (..)

  -- ** core driver
  , driveParser
  , driveParserFail

  -- *** available parser for core driver
  , surfaceDecl
  , surfaceExpr
  , surfaceType
  , surfaceModule

  -- ** low level definition

  -- *** available language
  , DeclLang
  , TypeLang
  , ExprLang, PatLang
  , WholeExpr

  , PredefExprLang
  , PredefDeclLang
  )
where

import Language.Core
import Language.Core.Extension
import Language.Parser
import Tlang.Generic ((:+:))
import qualified Data.Kind as D (Type)

import Capability.Accessors
import Capability.Reader (HasReader, MonadReader (..))
import Capability.State (HasState, MonadState (..))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)

import Control.Monad.State (StateT (..), MonadPlus)
import Control.Monad.Reader (ReaderT (..))
import Control.Applicative (Alternative)

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (MonadParsec, ParseErrorBundle, ParsecT, runParserT, lookAhead, errorBundlePretty, ShowErrorComponent)
import Text.Megaparsec.Debug (MonadParsecDbg)
import Control.Monad.IO.Class (MonadIO)

type ParserMonad' e m = ParsecT e Text (ReaderT ([Operator Text], [Operator Text]) (StateT OperatorStore m))

newtype ParserMonad e m a = ParserMonad
  { runParserMonad :: ParserMonad' e m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, Alternative, MonadPlus, MonadParsec e Text, MonadParsecDbg e Text)
    deriving (HasSource "OperatorStore" OperatorStore, HasSink "OperatorStore" OperatorStore)
        via MonadState (ParserMonad' e m)
    deriving (HasState "OperatorStore" OperatorStore)
        via MonadState (ParserMonad' e m)

    deriving (HasReader "TermOperator" [Operator Text], HasSource "TermOperator" [Operator Text])
        via Rename 1 (Pos 1 () (MonadReader (ParserMonad' e m)))
    deriving (HasReader "PatternOperator" [Operator Text], HasSource "PatternOperator" [Operator Text])
        via Rename 1 (Pos 1 () (MonadReader (ParserMonad' e m)))
    deriving (HasReader "TypeOperator" [Operator Text], HasSource "TypeOperator" [Operator Text])
        via Rename 2 (Pos 2 () (MonadReader (ParserMonad' e m)))

-- | an all in one monad for language parser
driveParser
  :: Monad m
  => OperatorStore
  -> ParserMonad e m a
  -> String -> Text
  -> m (Either (ParseErrorBundle Text e) a, OperatorStore)
driveParser store parser prompt text =
  let openv = mconcat $ store <&> \case
        TypeOperator t -> ([], [t])
        TermOperator t -> ([t], [])
   in runStateT (runReaderT (runParserT (runParserMonad parser) prompt text) openv) store

-- | allow automatic failing for parser
driveParserFail :: (MonadFail m, ShowErrorComponent e)
  => OperatorStore
  -> ParserMonad e m a
  -> String -> Text
  -> m (a, OperatorStore)
driveParserFail store parser prompt text = do
  let openv = mconcat $ store <&> \case
        TypeOperator t -> ([], [t])
        TermOperator t -> ([t], [])
  (res'either , s) <- runStateT (runReaderT (runParserT (runParserMonad parser) prompt text) openv) store
  case res'either of
    Right a -> return (a, s)
    Left e -> fail $ errorBundlePretty e

-- | declaration
type DeclLang e m pExpr pType expr typ = WithDecl e m
  (  Layer "define" (pType :- pExpr) (typ :- expr)
  :- Layer "ffi" pType typ
  :- Layer "type" pType typ
  :- Layer ("data" :- typ)
       ( WithDataDef e m
          (  Layer "struct" pType typ
          :- Layer "enum" pType typ
          :- Layer "coerce" pType typ
          :- Layer "phantom" pType typ
          )
       )
       (DataBody (DataNone :+: Identity :+: DataEnum Label :+: DataStruct Label) typ)
  :- "fixity"
  )

type PredefDeclLang m = DeclLang Void m (PredefExprLang m) (TypeLang Void m) (ExprSurface TypSurface) TypSurface

-- | type language
type TypeLang e m = WithType e m
  (  "identifier"
  :- "operator"
  :- "group"
  :- "nat" :- "text"
  :- "record" :- "variant"
  :- "tuple" :- "rep"
  :- "forall" :- "abstract"
  )

-- | term language
type ExprLang e m typ pat bpat ltyp lpat = WithExpr e m
  (  "identifier"
  :- "text" :- "number" :- "integer"
  :- "selector" :- "constructor"
  :- "group" :- "tuple" :- "record"
  :- Layer "annotation" ltyp typ
  :- Layer "let" lpat (Hint pat)
  :- Layer "@type" ltyp typ
  :- Layer ("block" :- typ) lpat (Hint bpat)
  :- Layer ("line" :- typ) lpat (Hint (Grp pat))
  :- "operator"
  )

-- | pattern language
type PatLang e m typ expr ltyp lexpr = WithPattern e m
  (  "wild"
  :- "binding"
  :- "variable"
  :- Layer "view" lexpr expr
  :- "group" :- "variant" :- "record" :- "tuple" :- "literal"
  :- Layer "annotation" ltyp typ
  :- "operator"
  )

data WholeExpr (e :: D.Type) (m :: D.Type -> D.Type) (t :: D.Type)
type PredefExprLang m = WholeExpr Void m (PatSurface TypSurface :- GPatSurface TypSurface :- TypSurface)

instance ( MonadFail m, MonadParsec e Text m
         , name ~ Name
         , HasReader "TermOperator" [Operator Text] m
         , PrattToken (TypeLang e m) typ m
         , PrattToken (PatLang e m typ (Expr f name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ)))
           (pat (Expr f name)) m
         , PrattToken (PatLang e m typ (Expr f name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ)))
           (bpat (Expr f name)) m
         , PrattToken (ExprLang e m typ pat bpat (TypeLang e m)
                        (PatLang e m typ (Expr f name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ)))
                      )
           (Expr f name) m
         , MonadParsecDbg e Text m
         )
  => Rule (WholeExpr e m (pat :- bpat :- typ)) (Expr f name) m where
  rule _ end =
    pratt @(ExprLang e m typ pat bpat (TypeLang e m)
            (PatLang e m typ (Expr f name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ))))
          (lookAhead end) Go <* end

------------------------
-- ** predefined parsers
------------------------

-- | a parser for surface declaration, you can use it with `driveParser`
surfaceDecl ::
  ( MonadFail m,
    MonadParsecDbg Void Text m,
    HasReader "TermOperator" [Operator Text] m,
    HasReader "TypeOperator" [Operator Text] m,
    HasReader "PatternOperator" [Operator Text] m,
    HasState "OperatorStore" OperatorStore m
  ) => m () -> m DeclSurface
surfaceDecl = declaration @(PredefDeclLang _) @(DeclSurfaceExt TypSurface (ExprSurface TypSurface))

-- | a parser for type, you can use it with `driveParser`
surfaceType ::
  ( MonadParsec Void Text m,
    MonadFail m,
    HasReader "TypeOperator" [Operator Text] m
  ) =>
  m () -> m TypSurface
surfaceType e = pratt @(TypeLang Void _) @TypSurface e Go

-- | a parser for expression, you can use it with `driveParser`
surfaceExpr ::
  ( MonadFail m,
    HasReader "TermOperator" [Operator Text] m,
    HasReader "TypeOperator" [Operator Text] m,
    HasReader "PatternOperator" [Operator Text] m,
    MonadParsecDbg Void Text m
  ) =>
  m () -> m (ExprSurface TypSurface)
surfaceExpr = parseRule @(PredefExprLang _) @(ExprSurface TypSurface)

-- | a parser for module, you can use it with `driveParser`
--
-- @
-- surfaceModule parsedModules declDelimiter end
-- @
surfaceModule ::
  ( MonadFail m,
    HasState "OperatorStore" OperatorStore m,
    MonadParsecDbg Void Text m,
    HasReader "TermOperator" [Operator Text] m,
    HasReader "TypeOperator" [Operator Text] m,
    HasReader "PatternOperator" [Operator Text] m
  ) => [ModuleSurface] -> m () -> m ()
  -> m ModuleSurface
surfaceModule ms sep = module' ms (surfaceDecl (lookAhead sep) <* sep)