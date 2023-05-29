module Tlang.Extension
  (
    -- ** sharing extension
    Literal (..)
  , LiteralText (..)
  , LiteralNatural (..)
  , LiteralInteger (..)
  , LiteralNumber (..)

    -- ** common structure
  , Tuple (..)
  , Record (..)

    -- ** reexport other extensions
  , module Type
  , module Expr
  )
where

import Data.Text (Text)
import Data.List (intercalate)

import Tlang.Extension.Type as Type
import Tlang.Extension.Expr as Expr

-- ** a cluster of literals

-- | use host language's type to define literal type
-- use this to define newtype literal
newtype Literal c a = Literal { getLiteral :: c }
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via c

newtype LiteralText a = LiteralText (Literal Text a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype LiteralNatural a = LiteralNatural (Literal Integer a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype LiteralInteger a = LiteralInteger (Literal Integer a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype LiteralNumber a = LiteralNumber (Literal Double a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


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

