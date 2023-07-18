{- | * Useful driver to parse texts
-}
module Driver.Parser
  ( OperatorSpace (..)
  , driveParser
  , parseDecl
  , ParserMonad (..)

  -- ** available language
  , DeclLang
  , TypeLang
  , ExprLang, PatLang
  , TypeAST
  , ASTPat
  , ASTGPat
  , ASTExpr
  , WholeExpr

  , PredefExprLang
  , PredefDeclLang
  , PredefExprVal
  , PredefDeclExtVal
  , PredefDeclVal
  )
where

import Tlang.AST
import Tlang.Parser
import Tlang.Extension
import Tlang.Generic ((:+:))
import Tlang.Constraint (Prefix (..), Prefixes (..))
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

import Text.Megaparsec (MonadParsec, ParseErrorBundle, ParsecT, runParserT, lookAhead)
import Text.Megaparsec.Debug (MonadParsecDbg)

type TypeAST = StandardType Label (Prefix Name) Name Name
type ASTGPat typ = Pattern (LiteralText :+: LiteralInteger :+: LiteralNumber) ((@:) typ :+: PatGroup) Label Name
type ASTPat typ = Pattern (LiteralText :+: LiteralInteger :+: LiteralNumber) ((@:) typ) Label Name

type ASTExpr typ = Expr
  ( Let (ASTPat typ)
  :+: Equation (ASTGPat typ) (Prefixes Name typ)
  :+: Equation (Grp (ASTPat typ)) (Prefixes Name typ)
  :+: Apply :+: Tuple :+: Record Label
  :+: LiteralText :+: LiteralInteger :+: LiteralNumber
  :+: Value typ :+: Selector Label :+: Constructor Label
  :+: (@:) typ
  ) Name

type ASTDeclExt typ expr =
      Item (UserOperator Text)
  :+: UserType typ [Prefix Name typ]
  :+: Item (FFI typ Name)
  :+: UserValue expr (Maybe typ)
  :+: UserData [Prefix Name typ] (UserDataDef (UserPhantom :+: UserCoerce :+: UserEnum Label :+: UserStruct Label) typ)
type ASTDecl typ expr = Decl (ASTDeclExt typ expr) Name

type PredefExprVal = ASTExpr TypeAST
type PredefDeclExtVal = ASTDeclExt TypeAST PredefExprVal
type PredefDeclVal = ASTDecl TypeAST PredefExprVal

type ParserMonad' e m = ParsecT e Text (ReaderT ([Operator Text], [Operator Text]) (StateT OperatorStore m))

newtype ParserMonad e m a = ParserMonad
  { runParserMonad :: ParserMonad' e m a
  } deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec e Text, MonadParsecDbg e Text)
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

-- runDSL @(WholeExpr Void _ (ASTPat (TypeAST Identity) :- ASTGPat (TypeAST Identity) :- TypeAST Identity)) @(ASTExpr (TypeAST Identity)) eof

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
       (UserDataDef (UserPhantom :+: UserCoerce :+: UserEnum Label :+: UserStruct Label) typ)
  :- "fixity"
  )

type PredefDeclLang m = DeclLang Void m (PredefExprLang m) (TypeLang Void m) PredefExprVal TypeAST

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
type PredefExprLang m = WholeExpr Void m (ASTPat TypeAST :- ASTGPat TypeAST :- TypeAST)

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
  => ParserDSL (WholeExpr e m (pat :- bpat :- typ)) (Expr f name) m where
  syntax _ end =
    pratt @(ExprLang e m typ pat bpat (TypeLang e m)
            (PatLang e m typ (Expr f name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ))))
          (lookAhead end) Go <* end

-- | declaration driver
parseDecl
  :: Monad m
  => OperatorStore -> ParserMonad Void m ()
  -> String -> Text
  -> m (Either (ParseErrorBundle Text Void) PredefDeclVal, OperatorStore)
parseDecl store end = driveParser store (declaration @(PredefDeclLang _) end)
