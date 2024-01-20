{- |
  a refined implementation of extensible data type.

  please refer https://dl.acm.org/doi/10.1145/2633628.2633635 for more information.
  -}
module Language.Generic.Subsume
  (

  -- ** operators
    inj, prj, split
  , injj, prjj, bisplit

  -- ** type constraint
  , (:<:), (:~:)
  , (:>+:)

  , (:<<:), (:~~:)
  , (:>>+:)
  )
where

import Language.Generic.Data ( type (:+:) (..), type (:++:) (..) )
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import Control.Applicative ((<|>))

data Route = Here | TurnLeft Route | TurnRight Route | Structure Route Route
data Result = Found Route | NotFound | Ambiguous

type family Elem (f :: Type -> Type) (g :: Type -> Type) :: Result where
  Elem f f = 'Found 'Here
  Elem (l :+: r) g = Dive (Elem l g) (Elem r g)
  Elem f (l :+: r) = Choose (Elem f l) (Elem f r)
  Elem f g = 'NotFound
type family Choose (l :: Result) (r :: Result) :: Result where
  Choose ('Found x) ('Found y) = 'Ambiguous
  Choose 'Ambiguous y = 'Ambiguous
  Choose x 'Ambiguous = 'Ambiguous
  Choose ('Found x) y = 'Found ('TurnLeft x)
  Choose x ('Found y) = 'Found ('TurnRight y)
  Choose x y = 'NotFound
type family Dive (l :: Result) (r :: Result) :: Result where
  Dive ('Found x) ('Found y) = 'Found ('Structure x y)
  Dive 'Ambiguous _ = 'Ambiguous
  Dive _ 'Ambiguous = 'Ambiguous
  Dive _ _ = 'NotFound

class Subsume (evi :: Result) (f :: Type -> Type) (g :: Type -> Type) where
  inj' :: Proxy evi -> f a -> g a
  prj' :: Proxy evi -> g a -> Maybe (f a)
instance Subsume ('Found 'Here) f f where
  inj' _ = id
  prj' _ = Just
instance Subsume ('Found p) f l => Subsume ('Found ('TurnLeft p)) f (l :+: r) where
  inj' _ = Inl . inj' (Proxy @('Found p))
  prj' _ (Inl x) = prj' (Proxy @('Found p)) x
  prj' _ _ = Nothing
instance Subsume ('Found p) f r => Subsume ('Found ('TurnRight p)) f (l :+: r) where
  inj' _ = Inr . inj' (Proxy @('Found p))
  prj' _ (Inr x) = prj' (Proxy @('Found p)) x
  prj' _ _ = Nothing
instance (Subsume ('Found tol) l g, Subsume ('Found tor) r g)
  => Subsume ('Found ('Structure tol tor)) (l :+: r) g where
    inj' _ (Inl x) = inj' (Proxy @('Found tol)) x
    inj' _ (Inr x) = inj' (Proxy @('Found tor)) x
    prj' _ x = Inl <$> prj' (Proxy @('Found tol)) x
           <|> Inr <$> prj' (Proxy @('Found tor)) x

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or _ _ = 'True
type family Dup (f :: Type -> Type) (l :: [Type -> Type]) :: Bool where
  Dup (f :+: g) l = Dup f (g ': l)
  Dup f l = Or (Find f l) (Dup' l)
type family Dup' (l :: [Type -> Type]) :: Bool where
  Dup' (f ': l) = Or (Dup f l) (Dup' l)
  Dup' '[] = 'False
type family Find (f :: Type -> Type) (l :: [Type -> Type]) :: Bool where
  Find f (g ': l) = Or (Find' f g) (Find f l)
  Find f '[] = 'False
type family Find' (f :: Type -> Type) (g :: Type -> Type) :: Bool where
  Find' f (l :+: r) = Or (Find' f l) (Find' f r)
  Find' f f = 'True
  Find' f g = 'False

class NoDup (f :: Type -> Type) (a :: Bool)
instance (TypeError ('Text "The Following Type:" ':$$: ('Text "  " ':<>: 'ShowType f) ':$$: 'Text "Contains duplication"))
  => NoDup f 'True
instance NoDup f 'False

-- Check whether `Elem` Reduction gets stucked
-- type family Check (a :: Constraint) (res :: Result) :: Constraint where
--   Check _ ('Found a) = ((), ())
--   Check _ _ = ()

-- type family PotentialParameter (l :: Type -> Type) (r :: Type -> Type) :: Constraint where
--   PotentialParameter l r = TypeError
--     ( 'Text "When Checking Subsumption, The Following Type:" ':$$:
--       'Text "On Left:" ':$$:
--       ('Text "  " ':<>: 'ShowType l) ':$$:
--       'Text "On Right:" ':$$:
--       ('Text "  " ':<>: 'ShowType r) ':$$:
--       'Text "May Contain Type Variables Which Gets Stucked." ':$$:
--       'Text "Try To Manually Add Constraint On Signature:" ':$$:
--       ('Text "  (" ':<>: 'ShowType l ':<>: 'Text ") :<: (" ':<>: 'ShowType r ':<>: 'Text ")") :$$:
--       'Text "Or Specific The Type Use TypeApplication"
--     )

-- | Generic definition to save typing, a type indexed method
type f :<: g = (Subsume (Elem f g) f g, NoDup f (Dup f '[]), NoDup g (Dup g '[]))

-- | Helpful type family to save typing
type family (:>+:) (sup :: Type -> Type) (subs :: [Type -> Type]) :: Constraint where
  sup :>+: (a ': as) = (a :<: sup, sup :>+: as)
  sup :>+: '[] = ()

-- This `Check` code may be redundant, it is too strict to be useful
-- type f :<: g = (Subsume (Elem f g) f g, Check (PotentialParameter f g) (Elem f g), NoDup f (Dup f '[]), NoDup g (Dup g '[]))
type f :~: g = (f :<: g, g :<: f)

-- | injector
inj :: forall f g a. (f :<: g) => f a -> g a
inj = inj' (Proxy @(Elem f g))
-- | projector
prj :: forall f g a. (f :<: g) => g a -> Maybe (f a)
prj = prj' (Proxy @(Elem f g))

-- | matcher
split :: (f :~: (l :+: r)) => (l a -> b) -> (r a -> b) -> f a -> b
split l r x =
  case inj x of
    Inl y -> l y
    Inr y -> r y

-- | * Bisubsume definition

type family Elem2 (f :: Type -> Type -> Type) (g :: Type -> Type -> Type) :: Result where
  Elem2 f f = 'Found 'Here
  Elem2 (l :++: r) g = Dive (Elem2 l g) (Elem2 r g)
  Elem2 f (l :++: r) = Choose (Elem2 f l) (Elem2 f r)
  Elem2 f g = 'NotFound

class Subsume2 (evi :: Result) (f :: Type -> Type -> Type) (g :: Type -> Type -> Type) where
  injj' :: Proxy evi -> f b a -> g b a
  prjj' :: Proxy evi -> g b a -> Maybe (f b a)
instance Subsume2 ('Found 'Here) f f where
  injj' _ = id
  prjj' _ = Just
instance Subsume2 ('Found p) f l => Subsume2 ('Found ('TurnLeft p)) f (l :++: r) where
  injj' _ = Inll . injj' (Proxy @('Found p))
  prjj' _ (Inll x) = prjj' (Proxy @('Found p)) x
  prjj' _ _ = Nothing
instance Subsume2 ('Found p) f r => Subsume2 ('Found ('TurnRight p)) f (l :++: r) where
  injj' _ = Inrr . injj' (Proxy @('Found p))
  prjj' _ (Inrr x) = prjj' (Proxy @('Found p)) x
  prjj' _ _ = Nothing
instance (Subsume2 ('Found tol) l g, Subsume2 ('Found tor) r g)
  => Subsume2 ('Found ('Structure tol tor)) (l :++: r) g where
    injj' _ (Inll x) = injj' (Proxy @('Found tol)) x
    injj' _ (Inrr x) = injj' (Proxy @('Found tor)) x
    prjj' _ x = Inll <$> prjj' (Proxy @('Found tol)) x
           <|> Inrr <$> prjj' (Proxy @('Found tor)) x

type family Dup2 (f :: Type -> Type -> Type) (l :: [Type -> Type -> Type]) :: Bool where
  Dup2 (f :++: g) l = Dup2 f (g ': l)
  Dup2 f l = Or (Find2 f l) (Dup2' l)
type family Dup2' (l :: [Type -> Type -> Type]) :: Bool where
  Dup2' (f ': l) = Or (Dup2 f l) (Dup2' l)
  Dup2' '[] = 'False
type family Find2 (f :: Type -> Type -> Type) (l :: [Type -> Type -> Type]) :: Bool where
  Find2 f (g ': l) = Or (Find2' f g) (Find2 f l)
  Find2 f '[] = 'False
type family Find2' (f :: Type -> Type -> Type) (g :: Type -> Type -> Type) :: Bool where
  Find2' f (l :++: r) = Or (Find2' f l) (Find2' f r)
  Find2' f f = 'True
  Find2' f g = 'False

class NoDup2 (f :: Type -> Type -> Type) (a :: Bool)
instance (TypeError ('Text "The Following Type:" ':$$: ('Text "  " ':<>: 'ShowType f) ':$$: 'Text "Contains duplication"))
  => NoDup2 f 'True
instance NoDup2 f 'False

type f :<<: g = (Subsume2 (Elem2 f g) f g, NoDup2 f (Dup2 f '[]), NoDup2 g (Dup2 g '[]))
type f :~~: g = (f :<<: g, g :<<: f)
type family (:>>+:) (sup :: Type -> Type -> Type) (subs :: [Type -> Type -> Type]) :: Constraint where
  sup :>>+: (a ': as) = (a :<<: sup, sup :>>+: as)
  sup :>>+: '[] = ()

-- | injector
injj :: forall f g b a. (f :<<: g) => f b a -> g b a
injj = injj' (Proxy @(Elem2 f g))
-- | projector
prjj :: forall f g b a. (f :<<: g) => g b a -> Maybe (f b a)
prjj = prjj' (Proxy @(Elem2 f g))
-- | matcher
bisplit :: (f :~~: (l :++: r)) => (l b a -> c) -> (r b a -> c) -> f b a -> c
bisplit l r x =
  case injj x of
    Inll y -> l y
    Inrr y -> r y