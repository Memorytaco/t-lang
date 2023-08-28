module Language.Core.Extension.Common
  (
    -- ** common structure
    Tuple (..)
  , Record (..)
  , Value (..)
  , Binder (..)
  , Cast (..)
  , Literal (..)
  )
where

import Data.List (intercalate)
import Prettyprinter (Pretty (..), (<+>), braces, tupled, lbrace, rbrace, comma, encloseSep)
import Data.Functor ((<&>))

-- | builtin tuple
newtype Tuple a
  = Tuple [a]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Tuple a) where
  pretty (Tuple as) = tupled (as <&> pretty)

instance (Show a) => Show (Tuple a) where
  show (Tuple vs) = "(" <> intercalate ", " (show <$> vs) <> ")"

-- | product type
newtype Record label a
  = Record [(label, a)]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Pretty label, Pretty a) => Pretty (Record label a) where
  pretty (Record fields)
    = braces $ encloseSep lbrace rbrace comma
      $ fields <&> \(a, b) -> pretty a <+> ":=" <+> pretty b

instance (Show label, Show a) => Show (Record label a) where
  show (Record vs) = "{" <> intercalate ", " ((\(a, b) -> show a <> " = " <> show b) <$> vs) <> "}"

-- | Inject any constant into the expression
newtype Value val a = Value val
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving (Pretty) via val

-- | a common extension to represent binder position
newtype Binder val a = Binder val
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving Pretty via val

-- | Type or Kind casting
data Cast t a = Cast t a
  deriving (Show, Eq, Functor)

instance (Pretty t, Pretty a) => Pretty (Cast t a) where
  pretty (Cast t a) = pretty a <+> ":" <+> pretty t

-- | use host language's type to define literal type
-- use this to define newtype literal
newtype Literal c a = Literal { getLiteral :: c }
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving (Show, Pretty) via c

