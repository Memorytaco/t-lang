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
import qualified Data.Kind as D (Type)

import Capability.Accessors
import Capability.Reader (HasReader, MonadReader (..))
import Capability.State (HasState, MonadState (..))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)

import Control.Monad.State (StateT (..), MonadPlus)
import Control.Monad.Reader (ReaderT (..))
import Control.Applicative (Alternative)

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)

import Text.Megaparsec (MonadParsec, ParseErrorBundle, ParsecT, runParserT, lookAhead)
import Text.Megaparsec.Debug (MonadParsecDbg)

type ASTGPat typ = Pattern (LiteralText :+: LiteralInteger :+: LiteralNumber) ((:@) typ :+: PatGroup) Label Symbol
type ASTPat typ = Pattern (LiteralText :+: LiteralInteger :+: LiteralNumber) ((:@) typ) Label Symbol

type ASTExpr typ = Expr
  (Let (ASTPat typ) :+: Lambda (ASTGPat typ) (Bounds Symbol typ)
                    :+: Lambda (Grp (ASTPat typ)) (Bounds Symbol typ)
                    :+: Apply)
  ( Tuple :+: Record Label :+: LiteralText :+: LiteralInteger :+: LiteralNumber
    :+: VisibleType typ :+: Selector Symbol :+: Constructor Symbol)
  ((:@) typ)
  Symbol

type ASTDeclExt typ expr = UserItem
  :+: UserType typ [Bound Symbol typ]
  :+: UserFFI typ
  :+: UserValue expr (Maybe typ)
  :+: UserData [Bound Symbol typ] (UserDataDef (UserPhantom :+: UserCoerce :+: UserEnum Label :+: UserStruct Label) typ)
type ASTDecl typ expr = Decl (ASTDeclExt typ expr) Symbol

type PredefExprVal = ASTExpr (TypeAST Identity)
type PredefDeclExtVal = ASTDeclExt (TypeAST Identity) PredefExprVal
type PredefDeclVal = ASTDecl (TypeAST Identity) PredefExprVal

-- | operator space for recording every available operator
data OperatorSpace = OperatorSpace
  { termOperator :: [Operator String]
  , typeOperator :: [Operator String]
  } deriving (Show, Eq, Generic)

type ParserMonad' e m = ParsecT e Text (StateT OperatorSpace (ReaderT OperatorSpace m))

newtype ParserMonad e m a = ParserMonad
  { runParserMonad ::
      ParsecT e Text (StateT OperatorSpace (ReaderT OperatorSpace m)) a
  } deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec e Text, MonadParsecDbg e Text)
    deriving (HasState "TermOperator" [Operator String], HasSink "TermOperator" [Operator String])
        via Rename "termOperator" (Field "termOperator" () (MonadState (ParserMonad' e m)))
    deriving (HasState "TypeOperator" [Operator String], HasSink "TypeOperator" [Operator String])
        via Rename "typeOperator" (Field "typeOperator" () (MonadState (ParserMonad' e m)))
    deriving (HasReader "TermOperator" [Operator String], HasSource "TermOperator" [Operator String])
        via Rename "termOperator" (Field "termOperator" () (MonadReader (ParserMonad' e m)))
    deriving (HasReader "PatternOperator" [Operator String], HasSource "PatternOperator" [Operator String])
        via Rename "termOperator" (Field "termOperator" () (MonadReader (ParserMonad' e m)))
    deriving (HasReader "TypeOperator" [Operator String], HasSource "TypeOperator" [Operator String])
        via Rename "typeOperator" (Field "typeOperator" () (MonadReader (ParserMonad' e m)))

-- runDSL @(WholeExpr Void _ (ASTPat (TypeAST Identity) :- ASTGPat (TypeAST Identity) :- TypeAST Identity)) @(ASTExpr (TypeAST Identity)) eof

-- | an all in one monad for language parser
driveParser
  :: Monad m
  => ([Operator String], [Operator String])
  -> ParserMonad e m a
  -> String -> Text
  -> m (Either (ParseErrorBundle Text e) a, OperatorSpace)
driveParser (term, typ) parser prompt text =
  let op = OperatorSpace term typ
   in runReaderT (runStateT (runParserT (runParserMonad parser) prompt text) op) op

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

type PredefDeclLang m = DeclLang Void m (PredefExprLang m) (TypeLang Void m) PredefExprVal (TypeAST Identity)

-- | type language
type TypeLang e m = WithType e m
  (  "identifier"
  :- "operator"
  :- "group"
  :- "nat" :- "text"
  :- "record" :- "variant"
  :- "tuple"
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
  :- "group" :- "variant" :- "record" :- "tuple"
  :- Layer "annotation" ltyp typ
  :- "operator"
  )

data WholeExpr (e :: D.Type) (m :: D.Type -> D.Type) (t :: D.Type)
type PredefExprLang m = WholeExpr Void m (ASTPat (TypeAST Identity) :- ASTGPat (TypeAST Identity) :- TypeAST Identity)

instance ( MonadFail m, MonadParsec e Text m
         , name ~ Symbol
         , HasReader "TermOperator" [Operator String] m
         , PrattToken (TypeLang e m) typ m
         , PrattToken (PatLang e m typ (Expr struct prim inj name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ)))
           (pat (Expr struct prim inj name)) m
         , PrattToken (PatLang e m typ (Expr struct prim inj name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ)))
           (bpat (Expr struct prim inj name)) m
         , PrattToken (ExprLang e m typ pat bpat (TypeLang e m)
                        (PatLang e m typ (Expr struct prim inj name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ)))
                      )
           (Expr struct prim inj name) m
         , MonadParsecDbg e Text m
         )
  => ParserDSL (WholeExpr e m (pat :- bpat :- typ)) (Expr struct prim inj name) m where
  syntax _ end =
    pratt @(ExprLang e m typ pat bpat (TypeLang e m)
            (PatLang e m typ (Expr struct prim inj name) (TypeLang e m) (WholeExpr e m (pat :- bpat :- typ))))
          (lookAhead end) Go <* end

-- | declaration driver
parseDecl :: Monad m
          => ([Operator String], [Operator String])
          -> ParserMonad Void m ()
          -> String -> Text
          -> m (Either (ParseErrorBundle Text Void) PredefDeclVal, OperatorSpace)
parseDecl op end = driveParser op (declaration @(PredefDeclLang _) end)
