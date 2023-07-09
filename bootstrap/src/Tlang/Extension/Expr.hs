module Tlang.Extension.Expr
  (
    -- ** expresion structure
    Let (..)
  , Letrec (..)
  , Equation (..)
  , Apply (..)
  , Selector (..)
  , Constructor (..)

  , Lambda (..)
  , Coerce (..)
  , Match (..)
  )
where

-- ** Surface Language

-- | local name binding
data Let binder expr
  = Let (binder expr) expr expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | a helper to distinguish recursive and non-recursive binding
data Letrec binder expr
  = Letrec [(binder expr, expr)] expr

-- | equation group, syntax of lambda for surface language.
-- it supports both light and heavy notation.
--
-- e.g.
--    identity: [ ?a = a ]
--    identity2: \ ?a => a
--    const: [ ?v, _ = v ]
--    const2: \ ?v, _ => v
--    const3: \ ?v => \_ => v
data Equation bind prefix expr
  = Equation prefix
      (bind expr, expr)
      [(bind expr, expr)]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | application, this includes both value application and type application
--
-- e.g.
--    ( \ ?a -> a) 3 := Apply (Lambda a -> a) 3 []
--    List @int := Apply List int []
data Apply expr
  = Apply expr expr [expr]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | @.selector@, field projector
newtype Selector name e = Selector name deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
-- | @`Variant@ for polymorphic variant, variant constructor
data Constructor name e = Constructor name [e] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- ** Core Language Extension

-- | type computation.
--
-- this data type inserts witness of a type coercing instead of simply
-- expressing "coercing" in the core language.
--
-- e.g.
--    (forall (a > ⊥). a) < int := (forall a. a) (CoerceTrans (CoerceIn (CoerceBot int)) CoerceElim)
--                              := int
data Coerce name typ
  = CoerceRefl      -- ^ identity
  | CoerceBot typ   -- ^ bottom, if target type is Bot then replace it with __typ__
  | CoerceHyp name  -- ^ abstract, replace type t with a if a :> t
  | CoerceIn  (Coerce name typ) -- ^ Inner instantiation
  | CoerceOut (Coerce name typ) -- ^ Outer instantiation
  | CoerceElim      -- ^ remove binder and substitution var with bounded typ
  | CoerceIntro     -- ^ add a new binder
  | CoerceTrans (Coerce name typ) (Coerce name typ) -- ^ sequel application of type computation
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | function block, basic computation block, support both type abstraction and value abstraction
data Lambda bind e = Lambda (bind e) e deriving (Show, Eq, Ord, Functor)

-- | pattern match expression
data Match pat e
  = Match e [(pat e, e)]
