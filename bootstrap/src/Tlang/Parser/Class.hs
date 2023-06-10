{-# LANGUAGE AllowAmbiguousTypes #-}
module Tlang.Parser.Class
  (
  -- ** pratt parser
    pratt
  , Semantic (..)
  , Power (..)
  , PrattToken (..)

  -- ** parser DSL
  , ParserDSL (..)
  , runDSL

  -- ** tag helper, used for defing DSL
  , (:-)
  , type (?-)
  , Layer
  , Hint
  , Try

  -- ** reexport
  , Proxy (..)
  , KnownSymbol
  , symbolVal
  )
where

import Control.Applicative
import Data.Kind (Type)
import Data.Functor (($>))
import Data.Proxy (Proxy (..))
import Text.Megaparsec (MonadParsec (..))
import Data.Text (Text)

import GHC.TypeLits

-- ** basic layout for a pratt parser
--
-- * topparser
-- * nud parser
-- * led parser
-- * dispatch parser
-- * optional end parser
-- * token parser

-- | intermediate name for pratt parser semantic token
--
-- TODO: find a better name
class Monad m => PrattToken (tag :: any) (a :: Type) (m :: Type -> Type) | tag a -> m where
  tokenize :: Proxy tag -> (m () -> Power -> m a)
           -> m () -> m (Semantic m a)

-- | the class where to define syntax of type level parser DSL
--
-- TODO: find a better name for it
class ParserDSL (lang :: any) (target :: Type) (m :: Type -> Type) | lang target -> m where
  syntax :: Proxy lang -> m () -> m target

-- | a short hand of `syntax`
runDSL :: forall lang a m. ParserDSL lang a m => m () -> m a
runDSL = syntax (Proxy @lang)

-- | semantic token, this is the way how do we interprete the token we find.
data Semantic m a = Semantic
  { nud :: m () -> m a      -- ^ null denotation, token may or may not require an argument
  , led :: m () -> a -> m a -- ^ left denotation, token require one or two arguments
  , lbp :: m Power          -- ^ its left binding power, right binding power is set by itself
  }

-- | a mutilayered power for token
data Power
  = Go                -- ^ smallest power, usually means "parse till end"
  | BuiltinL Integer  -- ^ some builtin operator, with normal weak power
  | Power Integer     -- ^ power determined by `Integer`
  | BuiltinH Integer  -- ^ some builtin operator, with normal strong power
  | Infinite          -- ^ unlimited power!!
  deriving (Show, Eq, Ord)

-- | core algorithm for the pratt parser
pratt' :: forall tag a e m. (MonadParsec e Text m, PrattToken tag a m) => Proxy tag -> m () -> Power -> m a
pratt' sel end rbp = do
  tok <- tokenize sel (pratt' sel) end
  left <- nud tok end
  (end $> left) <|> loop left
    where
      loop left = do
        lbp' <- lookAhead (tokenize sel (pratt' sel) end :: m (Semantic m a)) >>= lbp
        if rbp < lbp'
           then do tok <- tokenize sel (pratt' sel) end
                   left' <- led tok end left
                   (end $> left') <|> loop left'
           else return left

-- | if language defined `PrattToken`, then it has a pratt parser automatically.
--
-- To use this parser, you need to feed it with following type level things:
--
--    * a predefined dual syntax represented with type in host language (which is Haskell)
--
--    * the target AST, whether can it be polymorphic is determined by the dual type level parser language
--
-- To see an example of its usage, please refer to module `Tlang.Parser.Type` for tasting.
--
-- The language defined in `Tlang.Parser.Type` actually doesn't require full specification
-- of signature for result AST. Try typing "pratt @@Typelang_you_need_to_define @@(Type _ _ _ _ _)"
-- in ghci and find relevant parser driver to see inferred signature of result AST.
--
-- for "@Typelang_you_need_to_define", you can find some luck around the document.
pratt :: forall tag a e m. (MonadParsec e Text m, PrattToken tag a m) => m () -> Power -> m a
pratt = pratt' (Proxy @tag)
{-# INLINE pratt #-}

-- ** operators type level language

infixr 3 :-
-- | a sequence operator
data a :- b
-- | debug subexpression
infixr 4 ?-
data (info :: Symbol) ?- (a :: k)

-- | sub expression
data Layer (name :: k) (proxy :: any) (e :: Type)

-- | use to hold arbitrary info
data Hint (a :: k)

-- | effect expression, not useful for now
data Try (a :: k)
