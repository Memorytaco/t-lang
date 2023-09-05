{- | * AST for Pattern match
-}
{-# LANGUAGE QuantifiedConstraints #-}
module Language.Core.Pattern
  (
    -- ** Structure
    -- | single field pattern
    Pattern (..)
  , PatternF (..)
    -- | group pattern, pattern with different name binding strategies and logics
  , PatGroup (..)
  , Grp (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Tlang.TH (fixQ)
import Prettyprinter
import Data.Functor ((<&>))

-- | pattern matching syntax
data Pattern lit ext label name expr
  = PatWild         -- ^ match every thing and ignore it
  | PatUnit         -- ^ unit pattern
  | PatVar name     -- ^ bind content to a variable or match a constructor
  | PatPrm (lit (Pattern lit ext label name expr))    -- ^ match builtin primitives, its semantic is determined by extension
  | PatTup [Pattern lit ext label name expr]          -- ^ tuple pattern
  | PatRec [(label, Pattern lit ext label name expr)] -- ^ record pattern, open or closed
  | PatSym label [Pattern lit ext label name expr]    -- ^ variant pattern, both for **polymorphic variant** and **closed variant**
  | PatView expr (Pattern lit ext label name expr)    -- ^ allow applying function to the argument and view results
  | PatBind name (Pattern lit ext label name expr)    -- ^ assign a name to the whold pattern
  | PatExt (ext (Pattern lit ext label name expr))    -- ^ pattern extension
  deriving (Functor, Foldable, Traversable)

deriving instance
  ( Show (lit (Pattern lit ext label name expr))
  , Show (ext (Pattern lit ext label name expr))
  , Show name, Show expr, Show label)
  => Show (Pattern lit ext label name expr)
deriving instance
  ( Eq (lit (Pattern lit ext label name expr))
  , Eq (ext (Pattern lit ext label name expr))
  , Eq name, Eq expr, Eq label)
  => Eq (Pattern lit ext label name expr)

instance
  ( forall x. Pretty x => Pretty (lit x)
  , forall x. Pretty x => Pretty (ext x)
  , Pretty label, Pretty name, Pretty expr)
  => Pretty (Pattern lit ext label name expr) where
  pretty PatWild = "_"
  pretty PatUnit = "()"
  pretty (PatVar name) = "?" <> pretty name
  pretty (PatPrm lit) = pretty lit
  pretty (PatTup ts) = tupled (pretty <$> ts)
  pretty (PatRec rs) = encloseSep lbrace rbrace comma $ rs <&> \(l, p) ->
    pretty l <+> "=" <+> pretty p
  pretty (PatSym l ps) = parens $ pretty l <+> hsep (pretty <$> ps)
  pretty (PatView e p) = parens $ pretty e <+> "->" <+> pretty p
  pretty (PatBind name p) = pretty name <> "@" <> pretty p
  pretty (PatExt p) = pretty p


-- | pattern group with logic combination
data PatGroup a

  -- | __pat1 | pat2__, match with sequential patterns, one by one, capture only one of them
  = PatAlt a [a]

  -- | __pat1, pat2, pat3__, match with multiple patterns simutaneously, capture all
  -- of them at a time. some special rules will be applied to it.
  -- thus, called pattern group.
  | PatGrp a [a]
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

instance Pretty a => Pretty (PatGroup a) where
  pretty (PatAlt p ps) = concatWith (\a b -> a <+> pipe <+> b) (pretty <$> p:ps)
  pretty (PatGrp p ps) = concatWith (\a b -> a <+> comma <+> b) (pretty <$> p:ps)

-- | used to define sequence pattern
data Grp f a = Grp (f a) [f a] deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

instance Pretty (f a) => Pretty (Grp f a) where
  pretty (Grp p ps) = concatWith (\a b -> a <+> comma <+> b) (pretty <$> p:ps)

makeBaseFunctor $ fixQ [d|
  instance (Functor ext, Functor lit) => Recursive (Pattern lit ext label name expr)
  |]
