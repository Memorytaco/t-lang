{- |
  a refined implementation of extensible data type.

  please refer https://dl.acm.org/doi/10.1145/2633628.2633635 for more information.
  -}
module Language.Generic.Subsume
  (

  -- ** operators
    inj, prj, match2

  -- ** type constraint
  , (:<:), (:~:)
  , (:>+:)

  )
where

import Language.Generic.Data ( type (:+:)(..) )
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
match2 :: (f :~: (l :+: r)) => (l a -> b) -> (r a -> b) -> f a -> b
match2 l r x =
  case inj x of
    Inl y -> l y
    Inr y -> r y

