{- | * Useful driver to parse texts
-}
module Driver.Parser
  ( OperatorSpace (..)
  , runParser
  , parseDecl
  , ParserMonad (..)
  )
where

import Tlang.AST
import Capability.Accessors
import Capability.Reader (HasReader, MonadReader (..))
import Capability.State (HasState, MonadState (..))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)

import Control.Monad.State (StateT (..), MonadPlus)
import Control.Monad.Reader (ReaderT (..))
import Control.Applicative (Alternative)

import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)

import Text.Megaparsec (MonadParsec, ParseErrorBundle, ParsecT, runParserT)
import Text.Megaparsec.Error (ShowErrorComponent)

import Tlang.Parser.Decl (declaration, DeclareExtension)
import Tlang.Parser.Type (ParseType)

-- | operator space for recording every available operator
data OperatorSpace = OperatorSpace
  { termOperator :: [Operator String]
  , typeOperator :: [Operator String]
  } deriving (Show, Eq, Generic)

type ParserMonad' e m = ParsecT e Text (StateT OperatorSpace (ReaderT OperatorSpace m))

newtype ParserMonad e m a = ParserMonad
  { runParserMonad ::
      ParsecT e Text (StateT OperatorSpace (ReaderT OperatorSpace m)) a
  } deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec e Text)
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


-- | an all in one monad for language parser
runParser
  :: Monad m
  => ([Operator String], [Operator String])
  -> ParserMonad e m a
  -> String -> Text
  -> m (Either (ParseErrorBundle Text e) a, OperatorSpace)
runParser (term, typ) parser prompt text =
  let op = OperatorSpace term typ
   in runReaderT (runStateT (runParserT (runParserMonad parser) prompt text) op) op

-- | declaration driver
parseDecl :: Monad m
          => ([Operator String], [Operator String])
          -> ParserMonad Void m ()
          -> String -> Text
          -> m (Either (ParseErrorBundle Text Void) (Decl (DeclareExtension ParseType) Symbol), OperatorSpace)
parseDecl op end = runParser op (declaration end)
