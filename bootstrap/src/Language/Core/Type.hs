{-# LANGUAGE RankNTypes, QuantifiedConstraints #-}
module Language.Core.Type
  ( Type (..)
  , TypeF (..)

  , Kind (..)
  , KindF (..)

  , Closed
  , (:==) (..)
  , type (>|) (..)

  , Variance (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)
import Tlang.TH (fixQ)

-- | type representation. parameterised with some extensions.
-- please refer to `Tlang.Extension.Type` for all available options.
data Type bind rep name a where
  -- | empty type (bottom type) or "forall a. a" type
  TypPht :: Type bind rep name a
  -- | refer to named type or type variable
  TypVar :: a -> Type bind rep name a
  -- | type application or type prmtructor
  TypCon :: Type bind rep name a -> [Type bind rep name a] -> Type bind rep name a
  -- | a uniform way to represent arbitrary binding logic
  TypBnd :: bind (Type bind rep name a)
         -> Type bind rep name (name >| Type bind rep name a)
         -> Type bind rep name a
  -- | Allow artibrary injection, to provide further information of syntax tree. e.g. literal, kind annotation
  Type :: rep (Type bind rep name a) -> Type bind rep name a
  deriving (Functor, Foldable, Traversable)

-- | closed term
data Closed
instance Show Closed where
  show = error "Impossible for Closed"
instance Eq Closed where
  (==) = error "Impossible for Closed"
instance Ord Closed where
  compare = error "Impossible for Closed"

infixr 3 >|
data bind >| free
  = New bind -- ^ introduce new bound variable
  | Inc free -- ^ increase variable index, from (bind2 >| a) to (bind1 >| (bind2 >| a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
$(deriveBifunctor ''(>|))
$(deriveBifunctor ''Type)

instance Applicative ((>|) bind) where
  pure = Inc
  New x <*> _ = New x
  Inc f <*> a = f <$> a

instance Monad ((>|) bind) where
  New x >>= _ = New x
  Inc a >>= f = f a

instance (Functor bind, Functor rep) => Applicative (Type bind rep name) where
  pure = TypVar
  TypPht <*> _ = TypPht
  TypVar f <*> a = f <$> a
  TypCon f fs <*> a = TypCon (f <*> a) ((<*> a) <$> fs)
  TypBnd fbnd fbod <*> x = TypBnd ((<*> x) <$> fbnd) (lift <$> fbod)
    where lift = fmap (<*> x)
  Type f <*> a = Type $ (<*> a) <$> f

instance (Functor bind, Functor rep) => Monad (Type bind rep name) where
  TypPht >>= _ = TypPht
  TypVar v >>= f = f v
  TypCon m ms >>= f = TypCon (m >>= f) ((>>= f) <$> ms)
  TypBnd fa m >>= f =
    let g = fmap (>>= f)
     in TypBnd ((>>= f) <$> fa) (g <$> m)
  Type fa >>= f = Type ((>>= f) <$> fa)

deriving instance (Eq a, Eq name, forall x. Eq x => Eq (bind x), forall x. Eq x => Eq (rep x)) => Eq (Type bind rep name a)
deriving instance ( Ord a, Ord name
                  , forall x. Eq x => Eq (bind x), forall x. Ord x => Ord (bind x)
                  , forall x. Eq x => Eq (rep x), forall x. Ord x => Ord (rep x)
                  )
                  => Ord (Type bind rep name a)

instance
  ( Show a, Show name, Show (a >| name)
  , forall x. Show x => Show (rep x)
  , forall x. Show x => Show (bind x)
  , Show (bind (Type bind rep name a))
  ) => Show (Type bind rep name a) where
  show TypPht = "⊥"
  show (Type t) = show t
  show (TypVar name) = show name
  show (TypBnd binder body) = "Bind { " <> show binder <> " = " <> show body <> " }"
  show (TypCon t ts) = "(" <> show t <> " " <> show ts <> ")"

-- | type variance
data Variance = InVar | CoVar | ContraVar deriving (Show, Eq, Ord)

-- | named type. assign name to type and allow
-- recursive type and type level lambda also.
-- no type level literal is supported now.
data name :== typ = name :== typ  -- ^ pick a name to encapsulate a type, and turn a unnamed type to named type.
                                  --   other information including variable and kind is in `Type` itself.
                                  --   definition for type level operator, which should be prefixed with ":"
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

$(deriveBifunctor ''(:==))

infixr 3 :->

-- | type kind representation, any kind, normal kind (*) and higher kind
data Kind f name a where
  -- | concrete type, language's builtin type kind.
  KindStar :: Kind f name a
  -- | type kinds other than KindRef and KindType, and represent any type level thing.
  -- e.g. literal, value, lifted constructor, etc.
  KindVar :: a -> Kind f name a
  -- | something like "* -> *", means this is a type prmtructor or something else.
  (:->) :: Kind f name a -> Kind f name a -> Kind f name a
  -- | quantified kind variable, well, this is...
  KindBnd :: name -> Kind f name (name >| Kind f name a) -> Kind f name a
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
  instance (Functor rep, Functor bind) => Recursive (Type bind rep name a)
  instance (Functor f) => Recursive (Kind f info a)
  |]
deriving instance (Show name, Show a, Show r, forall x. Show x => Show (rep x), forall x. Show x => Show (bind x))
  => Show (TypeF bind rep name a r)
