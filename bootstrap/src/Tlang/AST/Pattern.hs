{- | * AST for Pattern match
-}
module Tlang.AST.Pattern
  (
    -- ** Structure
    -- | single field pattern
    Pattern (..)
  , PatternF (..)
    -- | group pattern, pattern with different name binding strategies and logics
  , GPattern (..)
  , GPatternF (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Tlang.TH (fixQ)

-- | pattern matching syntax
data Pattern lit inj name expr
  = PatWild         -- ^ match every thing and ignore it
  | PatUnit         -- ^ unit pattern
  | PatRef name     -- ^ bind content to a variable or match a constructor
  | PatSym name     -- ^ match polymorphic variant symbol
  | PatPrm (lit (Pattern lit inj name expr))   -- ^ match builtin primitives, its semantic is determined by extension
  | PatTup [Pattern lit inj name expr]         -- ^ tuple pattern
  | PatRec [(name, Pattern lit inj name expr)] -- ^ record pattern, open or closed
  | PatSum (Pattern lit inj name expr)         -- ^ variant pattern, both for **polymorphic variant** and **closed variant**
           [Pattern lit inj name expr]
  | PatView expr (Pattern lit inj name expr)   -- ^ allow applying function to the argument and view results
  | PatBind name (Pattern lit inj name expr)   -- ^ assign a name to the whold pattern
  | PatAnno (inj (Pattern lit inj name expr))  -- ^ type annotation for pattern
  deriving Functor

deriving instance (Show (lit (Pattern lit inj name expr)), Show (inj (Pattern lit inj name expr)), Show name, Show expr)
  => Show (Pattern lit inj name expr)
deriving instance (Eq (lit (Pattern lit inj name expr)), Eq (inj (Pattern lit inj name expr)), Eq name, Eq expr)
  => Eq (Pattern lit inj name expr)

-- | pattern group with logic combination
data GPattern lit inj name expr
  = Pattern (Pattern lit inj name expr)   -- ^ pattern for single field
  | PatSeq [GPattern lit inj name expr]   -- ^ __pat1 | pat2__, match with sequential patterns, one by one, capture only one of them
  | PatGrp [GPattern lit inj name expr]   -- ^ __pat1, pat2, pat3__, match with multiple patterns simutaneously, capture all
                                          -- ^ of them at a time. some special rules will be applied to it.
                                          -- ^ thus, called pattern group.
  deriving Functor

deriving instance (Show (lit (Pattern lit inj name expr)), Show (inj (Pattern lit inj name expr)), Show name, Show expr)
  => Show (GPattern lit inj name expr)
deriving instance (Eq (lit (Pattern lit inj name expr)), Eq (inj (Pattern lit inj name expr)), Eq name, Eq expr)
  => Eq (GPattern lit inj name expr)

makeBaseFunctor $ fixQ [d|
  instance (Traversable inj, Traversable lit) => Recursive (Pattern lit inj name expr)
  instance (Traversable inj, Traversable lit) => Recursive (GPattern lit inj name expr)
  |]
