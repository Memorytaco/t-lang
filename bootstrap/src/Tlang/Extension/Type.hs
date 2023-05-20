module Tlang.Extension.Type
  (
    -- | * available binder extension
    Scope (..)
  , Forall (..)
  , Constrain (..)
  , Equiv (..)

    -- | * available structure extension
  , Tuple (..)
  , Record (..)
  , Variant (..)
  , Literal (..)
  )
where

import Data.List (intercalate)
import Data.Text (Text)

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

-- | builtin tuple
newtype Tuple a
  = Tuple [a]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show a) => Show (Tuple a) where
  show (Tuple vs) = "(" <> intercalate ", " (show <$> vs) <> ")"

-- | product type
newtype Record label a
  = Record [(label, a)]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show label, Show a) => Show (Record label a) where
  show (Record vs) = "{" <> intercalate ", " ((\(a, b) -> show a <> " = " <> show b) <$> vs) <> "}"

-- | variant type, grouped label type
newtype Variant label a
  = Variant [(label, Maybe a)]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show label, Show a) => Show (Variant label a) where
  show (Variant vs) = "<" <> intercalate ", " ((\(a, b) -> show a <> " = " <> show b) <$> vs) <> ">"

-- | type level literal value
data Literal
  = Nat Integer -- ^ natural num
  | Str Text    -- ^ constant string
  deriving (Show, Eq, Ord)

