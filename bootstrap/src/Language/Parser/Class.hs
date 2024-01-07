{-# LANGUAGE AllowAmbiguousTypes #-}
module Language.Parser.Class
  (
  -- ** pratt parser
    pratt
  , Semantic (..)
  , Power (..)
  , PrattToken (..)

  -- ** parser DSL, with hand written non recursive rule
  , Rule (..)
  , parseRule

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
class Monad m => PrattToken (tag :: any) (a :: Type) (m :: Type -> Type) | tag a -> m where
  tokenize :: Proxy tag -> (m () -> Power -> m a)
           -> m () -> m (Semantic m a)

-- | This is the class where we define syntax of type level parser DSL.
--
-- Let's say we have following simple syntax definition:
--
-- @
-- integer := (0..9)+
-- float := integer {"." integer}
-- target := float | integer
-- @
--
-- and we have primitive rule for digit 0..9. Then, we can then try to define our
-- type level syntax as:
--
-- @
-- import GHC.TypeLits (Symbol)
-- data Token (a :: Symbol) (m :: Type -> Type) -- to hold our rule name and environment
-- data Combine (a :: k1) (b :: k2) (m :: Type -> Type) -- i will use this to provide a way of abstraction
-- @
--
-- and we can assign semantic to these syntax by writing parsing target and Rule instance:
--
-- @
-- data Target = GetInteger Integer | GetFloat Float deriving (Show, Eq, Ord)
--
-- -- The simple integer rule, with rule name `Token "integer"` and Integer as parse Target
-- instance (Monad m) => Rule (Token "integer" m) Integer m where
--   rule _ _ = do
--     nums :: [Int] <- some digit
--     integer <- {logic to convert "nums" into Integer}
--     return integer
--
-- -- This indicates rule (Token "float" m) rely on (Token "integer" m) and it requires
-- -- that rule (Token "integer" m) needs return Integer as result.
-- instance (Rule (Token "integer" m) Integer m)
--   => Rule (Token "float" m) Float m where
--  rule _ e = do
--    let integer = rule (Proxy :: Proxy (Token "integer")) e :: m Integer
--    (deci, frac'maybe) \<- (,) \<$\> integer \<*\> optional (string "." *\> integer)
--    number :: Float <- {logic to convert "decimal" and optional "fractional part" into Float}
--    return number
--
-- -- Here, we define a shorthand to combine two rules. Since we have only
-- -- two syntax rules, we don't need abstraction here and can provide a concrete
-- -- syntax to user.
-- --
-- -- e.g. rule @@(Combine "integer" "float") @@Target endCondition
-- --
-- -- If user provides wrong syntax like @@(Combine "int" "float"), GHC will
-- -- complain no instance for (Rule (Token "int" m).
-- instance
--   ( Rule (Token a m) Integer m
--   , Rule (Token b m) Float m
--   )
--   => Rule (Combine a b m) Target m where
--   rule _ e = do
--     let floatPart = rule (Proxy :: Proxy (Token b)) e
--         integerPart = rule (Proxy :: Proxy (Token a)) e
--     GetFloat <$> floatPart <|> GetInteger <$> integerPart
-- @
class Rule (rule :: r) (target :: Type) (m :: Type -> Type) | rule target -> m where
  -- | rule is the place to put a direct style definition for syntax
  rule :: Proxy rule -> m () -> m target

-- | a shorthand for `rule`.
--
-- @
-- parseRule @(full rule to apply) end
-- @
parseRule :: forall rule a m. Rule rule a m => m () -> m a
parseRule = rule (Proxy @rule)

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
  tok <- nextSemantic
  left <- nud tok end
  (end $> left) <|> loop left
    where
      nextSemantic :: m (Semantic m a)
      nextSemantic = tokenize sel (pratt' sel) end
      loop left = do
        lbp' <- lookAhead nextSemantic >>= lbp
        if rbp < lbp'
          then do tok <- nextSemantic
                  left' <- led tok end left
                  (end $> left') <|> loop left'
          else return left

-- | if language defined `PrattToken`, then it has a pratt parser automatically
-- built from this "token".
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
-- data Parse (val :: any)

-- | use to hold arbitrary info where a `Type` is required.
data Hint (a :: k)

-- | effect expression, not useful for now
data Try (a :: k)
