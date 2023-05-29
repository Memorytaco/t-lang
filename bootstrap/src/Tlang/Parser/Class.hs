{-# LANGUAGE TypeFamilyDependencies, AllowAmbiguousTypes #-}
module Tlang.Parser.Class
  (
    -- ** pratt parser
    HasPratt (..)
  , pratt
  )
where

import Control.Applicative
import Data.Kind (Type)
import Data.Functor (($>), (<&>))
import Data.Proxy (Proxy (..))

-- | basic layout
--
-- - topparser
-- - nud parser
-- - led parser
-- - dispatch parser
-- - optional end parser
-- - token parser

-- | adherence definition for pratt parser
class (Alternative m, Monad m) => HasPratt (tag :: any) (expr :: Type) (m :: Type -> Type) | tag -> expr, tag expr -> m where
  type Token tag expr = token | token -> expr
  getPower :: Proxy tag -> Token tag expr -> m (Integer, Integer)
  next :: Proxy tag -> m (Token tag expr)
  peek :: Proxy tag -> m (Token tag expr)

  nud :: Proxy tag -> m () -> Token tag expr -> m expr         -- ^ general dispatch for nud operator
  led :: Proxy tag -> m () -> expr -> Token tag expr -> m expr -- ^ general dispatch for led operator

-- | a general definition for pratt parser
pratt_ :: forall tag expr m. HasPratt tag expr m => Proxy tag -> m () -> Integer -> m expr
pratt_ witness end rbp = do
  left <- next witness >>= nud witness end
  (end $> left) <|> led' left
  where
    led' left = do
      lbp <- ((peek witness :: m (Token tag expr)) >>= getPower witness) <&> fst
      if lbp > rbp then
        next witness >>= led witness end left >>= \val ->
          end $> val <|> led' val
      else return left

pratt :: forall tag expr m. (HasPratt tag expr m) => m () -> Integer -> m expr
pratt = pratt_ (Proxy @tag)

