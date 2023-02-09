module Tlang.Parser.Pratt
  ( OperatorParser (..)
  )
where

-- |
-- - topparser
-- - nud parser
-- - led parser
-- - dispatch parser
-- - optional end parser
-- - token parser

import Control.Applicative
import Data.Kind (Type)

-- | adherence definition for pratt parser
class (Alternative m, Monad m) => OperatorParser tok m | m -> tok where
  type Expression tok :: Type
  getPower :: tok -> m (Integer, Integer)
  next :: m tok
  peek :: m tok

  pratt :: m () -> Integer -> m (Expression tok)
  pratt end rbp = do
    left <- next >>= nud end
    (end *> pure left) <|> led' left
    where
      led' left = do
        lbp <- peek >>= getPower >>= return . fst
        case lbp > rbp of
          False -> return left
          True -> do
            val <- next >>= led end left
            (end *> pure val) <|> led' val

  nud :: m () -> tok -> m (Expression tok) -- ^ general dispatch for nud operator
  led :: m () -> (Expression tok) -> tok -> m (Expression tok) -- ^ general dispatch for led operator

