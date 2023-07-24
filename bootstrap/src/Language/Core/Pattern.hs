{- | * AST for Pattern match
-}
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

-- | pattern group with logic combination
data PatGroup a
  = PatSeq [a] -- ^ __pat1 | pat2__, match with sequential patterns, one by one, capture only one of them
  | PatGrp [a] -- ^ __pat1, pat2, pat3__, match with multiple patterns simutaneously, capture all
               -- ^ of them at a time. some special rules will be applied to it.
               -- ^ thus, called pattern group.
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

newtype Grp f a = Grp [f a] deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeBaseFunctor $ fixQ [d|
  instance (Functor ext, Functor lit) => Recursive (Pattern lit ext label name expr)
  |]
