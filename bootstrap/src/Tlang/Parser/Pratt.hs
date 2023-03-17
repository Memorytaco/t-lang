module Tlang.Parser.Pratt
  ( OperatorParser (..)
  )
where

import Control.Applicative
import Data.Kind (Type)
import Data.Functor (($>), (<&>))

-- | basic layout
--
-- - topparser
-- - nud parser
-- - led parser
-- - dispatch parser
-- - optional end parser
-- - token parser

-- | adherence definition for pratt parser
class (Alternative m, Monad m) => OperatorParser tok m | m -> tok where
  type Expression tok :: Type
  getPower :: tok -> m (Integer, Integer)
  next :: m tok
  peek :: m tok

  pratt :: m () -> Integer -> m (Expression tok)
  pratt end rbp = do
    left <- next >>= nud end
    (end $> left) <|> led' left
    where
      led' left = do
        lbp <- (peek >>= getPower) <&> fst
        if lbp > rbp
           then next >>= led end left >>= \val -> end $> val <|> led' val
           else return left

  nud :: m () -> tok -> m (Expression tok) -- ^ general dispatch for nud operator
  led :: m () -> Expression tok -> tok -> m (Expression tok) -- ^ general dispatch for led operator

