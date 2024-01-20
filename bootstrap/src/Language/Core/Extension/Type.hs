{-# LANGUAGE QuantifiedConstraints #-}
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
import Prettyprinter (Pretty (..), (<+>), lparen, rparen, backslash, dot, encloseSep, langle, rangle, comma, colon)
import Data.Functor ( (<&>) )
import Data.Bifunctor.TH (deriveBifunctor)

-- | * Binder extension

-- | binder extension wrapper


-- | higher kinded type, naming an incompleted type
data Scope b name t = Scope (b name t) t deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
deriveBifunctor ''Scope
-- | universal quantified type, using de bruijn indice. this needs help from `name` type
data Forall b name t = Forall (b name t) t deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
deriveBifunctor ''Forall
-- | type constraint, a predicate
newtype Constrain cs t = Constrain (cs t) deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
-- | Isomorphic equvilent type, usually used to denote recursive type.
newtype Equiv eq t = Equiv (eq t) deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

-- | * Structural type extenstion

-- | variant type, grouped label type
newtype Variant label a
  = Variant [(label, Maybe a)]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Pretty label, Pretty a) => Pretty (Variant label a) where
  pretty (Variant vs) =
    encloseSep langle rangle comma $ vs <&> \(l, v'maybe) ->
      case v'maybe of
        Just v -> pretty l <+> colon <+> pretty v
        Nothing -> pretty l

instance (Show label, Show a) => Show (Variant label a) where
  show (Variant vs) = "<" <> intercalate ", " ((\(a, b) -> show a <> " = " <> show b) <$> vs) <> ">"

instance (forall x. Pretty x => Pretty (b name x), Pretty t) => Pretty (Scope b name t) where
  pretty (Scope v t) = backslash <> pretty v <> dot <> pretty t


instance (forall x. Pretty x => Pretty (b name x), Pretty t) => Pretty (Forall b name t) where
  pretty (Forall v t) = "forall" <+> lparen <> pretty v <> rparen <> dot <> pretty t
