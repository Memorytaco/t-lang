{-# LANGUAGE RankNTypes, QuantifiedConstraints #-}
module Language.Core.Type
  ( Type (..)
  , TypeF (..)

  , Kind (..)
  , KindF (..)

  , Arity (..)
  , Variance (..)

  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)
import Language.TH (fixQ)
import Language.Core.Utility
import Prettyprinter (Pretty (..), parens, hsep)

import qualified Data.Kind as T (Type)

-- | Arity of type symbol
newtype Arity = Arity [Variance]
  deriving (Show, Eq, Ord)
  deriving newtype (Semigroup, Monoid)

-- | Variance of parameter position
data Variance = Positive | Negative deriving (Show, Eq, Ord)


-- | type representation. parameterised with some extensions.
-- please refer to `Language.Core.Extension.Type` for all available options.
type Type :: (T.Type -> T.Type -> T.Type) -> (T.Type -> T.Type) -> T.Type -> T.Type -> T.Type
data Type bind rep name a where
  -- | empty type (bottom type) or "forall a. a" type
  TypPht :: Type bind rep name a
  -- | refer to named type or type variable
  TypVar :: a -> Type bind rep name a
  -- | type application or type constructor
  TypCon :: Type bind rep name a -> [Type bind rep name a] -> Type bind rep name a
  -- | a uniform way to represent arbitrary binding logic.
  --   a binder is a bifunctor.
  TypBnd :: bind name (Type bind rep name a) -> Type bind rep name a
  -- | Allow artibrary injection, to provide further information of syntax tree. e.g. literal, kind annotation
  Type :: rep (Type bind rep name a) -> Type bind rep name a
  deriving (Functor, Foldable, Traversable)
$(deriveBifunctor ''Type)

instance (Functor (bind name), Functor rep) => Applicative (Type bind rep name) where
  pure = TypVar
  TypPht <*> _ = TypPht
  TypVar f <*> a = f <$> a
  TypCon f fs <*> a = TypCon (f <*> a) ((<*> a) <$> fs)
  TypBnd fbnd <*> x = TypBnd ((<*> x) <$> fbnd)
  Type f <*> a = Type $ (<*> a) <$> f

instance (Functor (bind name), Functor rep) => Monad (Type bind rep name) where
  TypPht >>= _ = TypPht
  TypVar v >>= f = f v
  TypCon m ms >>= f = TypCon (m >>= f) ((>>= f) <$> ms)
  TypBnd fa >>= f = TypBnd ((>>= f) <$> fa)
  Type fa >>= f = Type ((>>= f) <$> fa)

deriving instance (Eq a, Eq name, forall x. Eq x => Eq (bind name x), forall x. Eq x => Eq (rep x)) => Eq (Type bind rep name a)
deriving instance ( Ord a, Ord name
                  , forall x. Eq x => Eq (bind name x), forall x. Ord x => Ord (bind name x)
                  , forall x. Eq x => Eq (rep x), forall x. Ord x => Ord (rep x)
                  )
                  => Ord (Type bind rep name a)

instance
  ( Show a, Show name, Show (a +> name)
  , forall x. Show x => Show (rep x)
  , forall x. Show x => Show (bind name x)
  ) => Show (Type bind rep name a) where
  show TypPht = "⊥"
  show (TypVar name) = show name
  show (TypBnd binder) = show binder
  show (TypCon t ts) = "(" <> show t <> " " <> show ts <> ")"
  show (Type t) = show t

instance
  ( forall x. Pretty x => Pretty (bind name x)
  , forall x. Pretty x => Pretty (rep x)
  , Pretty name, Pretty a
  )
  => Pretty (Type bind rep name a) where
  pretty TypPht = "⊥"
  pretty (TypCon t ts) =
    parens $ pretty t <>
    if null ts then "" else " " <> hsep (pretty <$> ts)
  pretty (TypBnd binder) = pretty binder
  pretty (TypVar name) = pretty name
  pretty (Type t) = pretty t

-- TODO: refine kind definition in `Language.Core.Type` module

infixr 3 :->

-- | type kind representation, any kind, normal kind (*) and higher kind
data Kind f name a where
  -- | concrete type, language's builtin type kind.
  KindStar :: Kind f name a
  -- | type kinds other than KindRef and KindType, and represent any type level thing.
  -- e.g. literal, value, lifted constructor, etc.
  KindVar :: a -> Kind f name a
  -- | something like "* -> *", means this is a type constructor or type abstraction.
  (:->) :: Kind f name a -> Kind f name a -> Kind f name a
  -- | quantified kind variable, well, this is...
  KindBnd :: name -> Kind f name (name +> Kind f name a) -> Kind f name a
  -- | extend functionality
  Kind :: (f (Kind f name a)) -> Kind f name a
  deriving (Functor, Foldable, Traversable)
$(deriveBifunctor ''Kind)

instance (Functor f) => Applicative (Kind f info) where
  pure = KindVar
  KindStar <*> _ = KindStar
  KindVar f <*> a = f <$> a
  (a :-> b) <*> x = a <*> x :-> b <*> x
  KindBnd name mf <*> x = KindBnd name (lift <$> mf)
    where lift = fmap (<*> x)
  Kind v <*> x = Kind ((<*> x) <$> v)

instance (Functor f) => Monad (Kind f info) where
  KindStar >>= _ = KindStar
  KindVar a >>= f = f a
  (a :-> b) >>= f = (a >>= f) :-> (b >>= f)
  KindBnd name a >>= f = KindBnd name (fmap (>>= f) <$> a)
  Kind v >>= f = Kind ((>>= f) <$> v)

deriving instance (forall x. Eq x => Eq (f x), Eq info, Eq a) => Eq (Kind f info a)
deriving instance (forall x. Ord x => Ord (f x), forall x. Eq x => Eq (f x), Ord info, Ord a) => Ord (Kind f info a)

instance (Show info, Show a, forall x. Show x => Show (f x)) => Show (Kind f info a) where
  show KindStar = "*"
  show (KindVar name) = show name
  show (KindBnd v body) = "{" <> show v <> "}" <> " => " <> show body
  show (v@(_ :-> _) :-> a) = "(" <> show v <> ")" <> " -> " <> show a
  show (a :-> b) = show a <> " -> " <> show b
  show (Kind anno) = show anno

makeBaseFunctor $ fixQ [d|
  instance (Functor rep, Functor (bind name)) => Recursive (Type bind rep name a)
  instance (Functor f) => Recursive (Kind f info a)
  |]
deriving instance (Show name, Show a, Show r, forall x. Show x => Show (rep x), forall x. Show x => Show (bind name x))
  => Show (TypeF bind rep name a r)
