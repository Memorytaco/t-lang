module Language.Core.Extension.Type
  (
    -- | * available binder extension
    Scope (..)
  , Forall (..)
  , Constrain (..)
  , Equiv (..)

    -- | * available structure extension
  , Variant (..)
  )
where

import Data.List (intercalate)

-- | * Binder extension

-- | higher kinded type, naming an incompleted type
newtype Scope b t = Scope (b t) deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
-- | universal quantified type, using de bruijn indice. this needs help from `name` type
newtype Forall b a = Forall (b a) deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
-- | type constraint, a predicate
newtype Constrain cs a = Constrain (cs a) deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
-- | Isomorphic equvilent type
newtype Equiv hd t = Equiv (hd t) deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

-- | * Structural type extenstion

-- | variant type, grouped label type
newtype Variant label a
  = Variant [(label, Maybe a)]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show label, Show a) => Show (Variant label a) where
  show (Variant vs) = "<" <> intercalate ", " ((\(a, b) -> show a <> " = " <> show b) <$> vs) <> ">"

