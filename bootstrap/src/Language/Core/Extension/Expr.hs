{-# LANGUAGE QuantifiedConstraints #-}
module Language.Core.Extension.Expr
  (
    -- ** expresion structure
    Let (..)
  , LetGrp (..)
  , Letrec (..)
  , Equation (..)
  , Apply (..)
  , Selector (..)
  , Constructor (..)

  , Lambda (..)
  , LambdaI (..)
  , Coerce (..)
  , Match (..)
  )
where

-- ** Surface Language

-- | local name binding
data Let binder expr
  = Let (binder expr) expr expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | group of local name binding. it serves mainly surface language.
data LetGrp binder expr
  = LetGrp [(binder expr, expr)] expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | local name binding, recursive
data Letrec binder expr
  = Letrec [(binder expr, expr)] expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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
  = Equation prefix (bind expr, expr) [(bind expr, expr)]
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

-- | abstraction block, support both type abstraction and value abstraction.
data Lambda bind e = Lambda (bind e) e deriving (Show, Eq, Ord, Functor)

-- | function block, which supports nested data type as deBruijn index.
--
-- Lambda, but with indexed variable.
--
-- It needs another newtype definition to define type level fixpoint.
-- e.g. a definition for `Expr`
--    newtype ExprL ix f a = ExprL (Expr (ix (ExprL ix f) :+: f) a)
--    type ExprNest f a = ExprL (Lambda' (Const Int) Int)
--
-- it uses another layer of wrapper to add type index and keeps other things the same.
data LambdaI bind name f e = LambdaI (bind e) (f (Either name e)) deriving (Functor, Foldable, Traversable)
deriving instance (forall a. Show (bind a), forall a. Show (f a)) => Show (LambdaI bind name f e)
deriving instance (forall a. Eq (bind a), forall a. Eq (f a)) => Eq (LambdaI bind name f e)
deriving instance (forall a. Ord (bind a), forall a. Ord (f a)) => Ord (LambdaI bind name f e)

-- | pattern match expression
data Match match e
  = Match e [(match e, e)]
