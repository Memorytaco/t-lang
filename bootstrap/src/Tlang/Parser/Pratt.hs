module Tlang.Parser.Pratt
  ( PrattParser
  , Operator (..)
  , OperatorKind (..)
  , OperatorClass (..)
  , OperatorClassSpecial (..)

  , NudParser
  , LedParser

  , pratt
  , prattNud
  , prattLed
  )
where

import Text.Parsec

-- | TODO: PrattParser's type could be generalize
type PrattParser v = Parsec String [Operator (OperatorClass v)]

data Operator s = Operator OperatorKind Integer Integer s deriving (Show, Eq, Functor)
data OperatorKind =  Prefix | Postfix | Unifix | Infix deriving (Show, Eq)
data OperatorClass v = OpNorm v | OpAction String | OpSpecial OperatorClassSpecial deriving (Show, Eq, Ord)
data OperatorClassSpecial = OpLeft String String | OpRight String String deriving (Show, Eq, Ord)

-- | Signature for a NudParser, it accepts an end only
type NudParser v a = PrattParser v () -> PrattParser v a
-- | Signature for a LedParser, it accepts an end, right binding power and the value
type LedParser v a = PrattParser v () -> Integer -> a -> PrattParser v a

-- | A helper function to help define a basic pratt parser structure, it is the toplevel expression parser.
pratt :: NudParser v a  -- ^nud stage, should be filled with `prattNud`
      -> LedParser v a -- ^led stage, should be filled with `prattLed`
      -> PrattParser v () -> Integer -> PrattParser v a
pratt nud led end rbp = nud end >>= \left -> (end *> pure left) <|> led end rbp left

-- | nud definition helper
prattNud :: PrattParser v tok -- ^a non recursive token parser
         -> (PrattParser v () -> tok -> PrattParser v a)  -- ^user supplied dispatch function
         -> NudParser v a
prattNud expr dispatch end = expr >>= dispatch end

-- | led definition helper
prattLed :: (tok -> PrattParser v Integer)  -- ^fetch token left binding power
         -> PrattParser v tok               -- ^A non recursive token parser
         -> (PrattParser v () -> tok -> a -> PrattParser v a)  -- ^user supplied dispatch function
         -> LedParser v a
         -- -> PrattParser v () -> Integer -> a -> PrattParser v a
prattLed getPower expr dispatch end rbp left =
  lookAhead expr >>= getPower >>= \lbp ->
  case lbp > rbp of
    False -> return left
    True -> do
      tok <- expr
      next <- dispatch end tok left
      (end *> pure next) <|> prattLed getPower expr dispatch end rbp next
