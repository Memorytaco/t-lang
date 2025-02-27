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
  , DeclPart  -- sub language of declaration
  , TypePart  -- sub language of type
  , ExprPart  -- sub language of expression
  , PatPart   -- sub language of pattern
  , CoreLanguageDefinition  -- language syntax and also the whole language definition

  , ExprInstance  -- available expression parser
  , DeclInstance  -- available declaration parser
  )
where

import Language.Core
import Language.Core.Extension
import Language.Parser
import Language.Generic ((:+:))
import qualified Data.Kind as D (Type)

import Capability.Accessors
import Capability.Reader (HasReader, MonadReader (..))
import Capability.State (HasState, MonadState (..))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)

import Control.Monad.State (StateT (..))
import Control.Monad (MonadPlus)
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

-- | an all in one monad for language parser.
--
-- this driver accepts an `OperatorStore` and a
-- parser definition and then consumes provided text with a
-- parsing environment hint text (for human reading only).
--
-- it updates `OperatorStore` and returns store. so `driveParser`
-- has some sort of composibility.
driveParser
  :: Monad m
  => OperatorStore
  -> ParserMonad e m a
  -> String -> Text
  -> m (Either (ParseErrorBundle Text e) a, OperatorStore)
driveParser store parser prompt text =
  let openv = mconcat $ store <&> \case
        TypeOperator op -> ([], [op])
        TermOperator op -> ([op], [])
   in runStateT (runReaderT (runParserT (runParserMonad parser) prompt text) openv) store

-- | allow automatic failure for parser. see `driveParser` for more info.
driveParserFail :: (MonadFail m, ShowErrorComponent e)
  => OperatorStore
  -> ParserMonad e m a
  -> String -> Text
  -> m (a, OperatorStore)
driveParserFail store parser prompt text = do
  (res'either , s) <- driveParser store parser prompt text
  case res'either of
    Right a -> return (a, s)
    Left e -> fail $ errorBundlePretty e

-- | declaration
type DeclPart e m pExpr pType expr typ = DeclSyntax e m
  (  Layer "define" (pType :- pExpr) (typ :- expr)
  :- Layer "ffi" pType typ
  :- Layer "type" pType typ
  :- Layer ("data" :- typ)
       ( DeclDataSyntax e m
          (  Layer "struct" pType typ
          :- Layer "enum" pType typ
          :- Layer "coerce" pType typ
          :- Layer "phantom" pType typ
          )
       )
       (DataBody (DataNone :+: Identity :+: DataEnum Label :+: DataStruct Label) typ)
  :- "fixity"
  )

type DeclInstance m = DeclPart Void m (ExprInstance m) (TypePart Void m) (ExprSurface TypSurface) TypSurface

-- | type language
type TypePart e m = TypeSyntax e m
  (  "identifier"
  :- "operator"
  :- "group"
  :- "nat" :- "text"
  :- "record" :- "variant"
  :- "tuple" :- "rep"
  :- "forall" :- "abstract"
  )

-- | term language
type ExprPart e m typ pat bpat ltyp lpat = ExprSyntax e m
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
type PatPart e m typ expr ltyp lexpr = PatternSyntax e m
  (  "wild"
  :- "binding"
  :- "variable"
  :- Layer "view" lexpr expr
  :- "group" :- "variant" :- "record" :- "tuple" :- "literal"
  :- Layer "annotation" ltyp typ
  :- "operator"
  )

data CoreLanguageDefinition (e :: D.Type) (m :: D.Type -> D.Type) (t :: D.Type)

type ExprInstance m =
  CoreLanguageDefinition Void m (PatSurface TypSurface :- GPatSurface TypSurface :- TypSurface)

instance
  ( MonadFail m, MonadParsec e Text m
  , name ~ Name
  , patpart ~ PatPart e m typ (Expr f name) (TypePart e m) (CoreLanguageDefinition e m (pat :- bpat :- typ))

  , HasReader "TermOperator" [Operator Text] m
  , PrattToken (TypePart e m) typ m
  , PrattToken patpart (pat (Expr f name)) m
  , PrattToken patpart (bpat (Expr f name)) m
  , PrattToken (ExprPart e m typ pat bpat (TypePart e m) patpart) (Expr f name) m
  , MonadParsecDbg e Text m
  )
  => Rule (CoreLanguageDefinition e m (pat :- bpat :- typ)) (Expr f name) m where
  rule _ end =
    pratt @(ExprPart e m typ pat bpat (TypePart e m)
            (PatPart e m typ (Expr f name) (TypePart e m) (CoreLanguageDefinition e m (pat :- bpat :- typ))))
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
surfaceDecl = declaration @(DeclInstance _) @(DeclSurfaceExt TypSurface (ExprSurface TypSurface))

-- | a parser for type, you can use it with `driveParser`
surfaceType ::
  ( MonadParsec Void Text m,
    MonadFail m,
    HasReader "TypeOperator" [Operator Text] m
  ) =>
  m () -> m TypSurface
surfaceType e = pratt @(TypePart Void _) @TypSurface e Go

-- | a parser for expression, you can use it with `driveParser`
surfaceExpr ::
  ( MonadFail m,
    HasReader "TermOperator" [Operator Text] m,
    HasReader "TypeOperator" [Operator Text] m,
    HasReader "PatternOperator" [Operator Text] m,
    MonadParsecDbg Void Text m
  ) =>
  m () -> m (ExprSurface TypSurface)
surfaceExpr = parseRule @(ExprInstance _) @(ExprSurface TypSurface)

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
  ) => [ModuleSurface SParsing] -> m () -> m ()
  -> m (ModuleSurface SParsing)
surfaceModule ms sep = module' ms (surfaceDecl (lookAhead sep) <* sep)
