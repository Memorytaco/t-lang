module Tlang.Subst
  ( Subst (..)
  , Rule (..)
  , Grafting (..)

  , Closure (..)
  , Graft (..)
  , MetaVar (..)
  , (:->) (..)

  , LambdaCalculus (..)
  )
where

import Tlang.AST (Kind (..), None (..))
import Data.Bifunctor.TH (deriveBifunctor)

-- | extension of lambda calculus
data Subst env term = env :! Integer
                    | Cons env term (Subst env term)
                    | Assoc env (Subst env term) (Subst env term)
                    deriving (Eq, Functor, Foldable, Traversable)

-- | A helper name wrapper to enable delayed substitution
data Closure env term = Closure term (Subst env term) deriving (Show, Eq, Functor, Foldable, Traversable)
-- | a helper name wrapper to enable meta variable
data Graft var term = Graft var | UnGraft term deriving (Eq, Functor, Traversable, Foldable)

instance (Show var, Show term) => Show (Graft var term) where
  show (Graft var) = "?" <> show var
  show (UnGraft term) = show term

-- | tagged meta variable
newtype MetaVar tag a = MetaVar { getMetaVar :: a }
  deriving newtype (Show, Eq)
  deriving (Functor, Traversable, Foldable)

-- | first order substitution
data a :-> b = a :-> b deriving (Show, Eq, Functor)

instance {-# Incoherent #-} Show (MetaVar tag String) where
  show = getMetaVar

instance Show term => Show (Subst env term) where
  show (_ :! i) = if i == 0 then "id" else "↑" <> show i
  show (Cons _ t s) = "(" <> show t <> "・" <> show s <> ")"
  show (Assoc _ s1 s2) = "(" <> show s1 <> " ○ " <> show s2 <> ")"

class Grafting v term where
  graft :: v -> term -> term
instance (Foldable t, Functor t, Grafting v a) => Grafting (t v) a where
  graft vs = foldr (.) id $ graft <$> vs

$(deriveBifunctor ''Graft)
$(deriveBifunctor ''(:->))

-- | any calculus support explicit substitution
class Rule env a | a -> env where
  rewrite :: a -> Subst env a -> a
  lmap :: a -> Subst env a -> a
  normalize :: a -> a

data LambdaCalculus e val =
    CVal val
  | CVar Integer
  | CAbs (LambdaCalculus e val)
  | CApp (LambdaCalculus e val) (LambdaCalculus e val)
  | CClos (LambdaCalculus e val) (Subst e (LambdaCalculus e val))
  deriving (Eq)

instance Show val => Show (LambdaCalculus e val) where
  show (CVal v) = show v
  show (CVar i) = show i
  show (CAbs t) = "λ." <> show t <> "|"
  show (CApp a b) = "(" <> show a <> " " <> show b <> ")"
  show (CClos a b) = show a <> "[" <> show b <> "]"

instance Monoid e => Rule e (LambdaCalculus e val) where
  rewrite t (_ :! 0) = t
  rewrite (CVal val) _ = CVal val
  rewrite (CVar i) (_ :! j) = CVar (i + j)
  rewrite (CVar 1) (Cons _ t _) = t
  rewrite (CVar i) (Cons _ _ s2) = rewrite (CVar (i - 1)) s2
  rewrite (CClos v s1) s2 = rewrite v s1 `rewrite` s2
  rewrite t (Assoc _ s1 s2) = rewrite (rewrite t s1) s2
  rewrite (CAbs t) s = CAbs . rewrite t $ Cons mempty (CVar 1) (Assoc mempty s $ mempty :! 1)
  rewrite (CApp t1 t2) s = CApp (rewrite t1 s) (rewrite t2 s)
  lmap = CClos
  normalize = id

instance Rule () (Kind (Graft (MetaVar Kind Integer)) Integer) where
  rewrite a (_ :! 0) = a
  rewrite (KindRef i) (_ :! j) = KindRef $ i + j
  rewrite (KindRef 1) (Cons _ t _) = t
  rewrite (KindRef i) (Cons _ _ s) = rewrite (KindRef (i - 1)) s
  rewrite (KindLift (UnGraft t)) s = rewrite t s
  rewrite v@(KindLift _) _ = v
  rewrite t (Assoc _ s1 s2) = rewrite t s1 `rewrite` s2
  rewrite KindType _ = KindType
  rewrite (KindAbs a t) s = KindAbs a $ rewrite t (Cons () (KindRef 1) (Assoc () s (() :! 1)))
  rewrite (t1 ::> t2) s = rewrite t1 s ::> rewrite t2 s
  lmap = const
  normalize = id

instance Rule () (Kind None Integer) where
  rewrite a (_ :! 0) = a
  rewrite (KindRef i) (_ :! j) = KindRef $ i + j
  rewrite (KindRef 1) (Cons _ t _) = t
  rewrite (KindRef i) (Cons _ _ s) = rewrite (KindRef (i - 1)) s
  rewrite (KindLift None) _ = KindLift None
  rewrite t (Assoc _ s1 s2) = rewrite t s1 `rewrite` s2
  rewrite KindType _ = KindType
  rewrite (KindAbs a t) s = KindAbs a $ rewrite t (Cons () (KindRef 1) (Assoc () s (() :! 1)))
  rewrite (t1 ::> t2) s = rewrite t1 s ::> rewrite t2 s
  lmap = const
  normalize = id

instance Rule () (Kind (Closure ()) (Graft String Integer)) where
  rewrite a (_ :! 0) = a
  rewrite v@(KindRef (Graft _)) s = KindLift $ Closure v s
  rewrite (KindRef (UnGraft i)) (_ :! j) = KindRef . UnGraft $ i + j
  rewrite (KindRef (UnGraft 1)) (Cons _ t _) = t
  rewrite (KindRef (UnGraft i)) (Cons _ _ s) = rewrite (KindRef . UnGraft $ i - 1) s
  rewrite (KindLift (Closure t s1)) s2 = rewrite t (Assoc () s1 s2)
  rewrite t (Assoc _ (_ :! 1) (Cons _ _ s)) = rewrite t s
  rewrite t (Assoc _ (_ :! n) (Cons _ _ s)) = rewrite t $ Assoc () (() :! (n - 1)) s
  rewrite t (Assoc _ s1 s2) = rewrite t s1 `rewrite` s2
  rewrite KindType _ = KindType
  rewrite (KindAbs a t) s = KindAbs a $ rewrite t (Cons () (KindRef $ UnGraft 1) (Assoc () s (() :! 1)))
  rewrite (t1 ::> t2) s = rewrite t1 s ::> rewrite t2 s

  lmap (KindLift (Closure t s1)) s2 = rewrite t (Assoc () s1 s2)
  lmap t s = KindLift (Closure t s)

  normalize t@(KindLift (Closure (KindRef (Graft _)) _)) = t
  normalize (KindLift (Closure t s)) = rewrite t s
  normalize t = t
