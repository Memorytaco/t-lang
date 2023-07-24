module Language.Core.Extension.Common
  (
    -- ** common structure
    Tuple (..)
  , Record (..)
  , Value (..)
  , Cast (..)
  , Literal (..)
  )
where

import Data.List (intercalate)

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

-- | Inject any constant into the expression
newtype Value val a = Value val
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via val

-- | Type or Kind casting
data Cast t a = Cast t a
  deriving (Show, Eq, Functor)

-- | use host language's type to define literal type
-- use this to define newtype literal
newtype Literal c a = Literal { getLiteral :: c }
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via c

