{- | * definition of operator
-}

module Tlang.AST.Operator
  ( builtinStore

  , Operator (..)
  , OperatorSpace (..)
  , OperatorClass (..)
  , OperatorStore
  , Fixity (..)
  )
where

import Data.Text (Text)

-- | a boring data type holding a definition of
data Operator a
  = Operator Fixity Integer Integer a
  deriving stock (Show, Eq, Ord, Functor)

-- | fixity of operator.
--
-- It introduces many combinitations and mixes infix with unifix operator.
-- But there should be no ambiguity.
--
-- * for operator mixed with prefix and postfix, it is disambiguated by its binding power.
-- * for operator mixed with unfix and infix, it is default treated as an infix operator and
--   fall back to prefix operator or postfix operator when there is no other choice.
--   These definitions share one common binding power.
data Fixity
  = Prefix    -- ^ prefix position: <> token
  | Postfix   -- ^ postfix position: token <>
  | Unifix    -- ^ prefix or postfix: token <>, <> token. It is disambiguated by its binding power.
  | Infix     -- ^ infix: token1 <> token2
  | PreInfix  -- ^ infix and prefix: token1 <> token2, <> token.
  | PostInfix -- ^ infix and postfix: token1 <> token2, token <>.
  | UniInfix  -- ^ infix and unifix: token1 <> token2, token <>, <> token.
  deriving (Show, Eq, Ord)

instance Semigroup Fixity where
  UniInfix <> _ = UniInfix
  _ <> UniInfix = UniInfix

  PostInfix <> Prefix = UniInfix
  PostInfix <> Unifix = UniInfix
  PostInfix <> PreInfix = UniInfix
  PostInfix <> _ = PostInfix

  PreInfix <> Postfix = UniInfix
  PreInfix <> Unifix = UniInfix
  PreInfix <> PostInfix = UniInfix
  PreInfix <> _ = PreInfix

  Infix <> Prefix = PreInfix
  Infix <> Postfix = PostInfix
  Infix <> Unifix = UniInfix
  Infix <> v = v

  Unifix <> v
    | v <= Unifix = Unifix
    | v == Infix = UniInfix
    | otherwise = v

  Postfix <> Prefix = Unifix
  Postfix <> Infix = PostInfix
  Postfix <> PreInfix = UniInfix
  Postfix <> v = v

  Prefix <> Postfix = Unifix
  Prefix <> Infix = PreInfix
  Prefix <> PostInfix = UniInfix
  Prefix <> v = v

-- | Fixity defaults to `Infix`
instance Monoid Fixity where
  mempty = Infix

-- | a potential data strcture for encoding quasiquotation.
--
-- TODO: add usage to this structure.
data OperatorClass a
  = OperatorUni (Operator a)  -- ^ operator that is used as infix or unifix
  | OperatorDuo (a, a)        -- ^ environment operator
  deriving stock (Show, Eq, Ord, Functor)

-- | a namespace for defining operator
data OperatorSpace a
  = TermOperator a  -- ^ term level operator
  | TypeOperator a  -- ^ type level operator
  deriving (Show, Eq, Ord, Functor)

-- | builtin operator
builtinStore :: OperatorStore
builtinStore =
  [ TypeOperator $ Operator Infix (-10) (-15)  "->"
  ]

-- | operator space for recording every available operator
type OperatorStore = [OperatorSpace (Operator Text)]

