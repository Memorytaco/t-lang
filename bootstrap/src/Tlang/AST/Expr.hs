module Tlang.AST.Expr
  ( Expr (..)
  , ExprF (..)
  , (:@) (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)

import Tlang.TH (fixQ)

-- | type annotation with full power of the type system
data typ :@ term = term :@ typ deriving (Show, Eq, Functor, Traversable, Foldable)
$(deriveBifunctor ''(:@))

-- | expression parametrised with primitives, structures and any annotation
data Expr struct prim inj name
  = ExRef name                                  -- ^ variable or term info reference
  | ExPrm (prim (Expr struct prim inj name))    -- ^ primitive expresion
  | ExStc (struct (Expr struct prim inj name))  -- ^ structure for expression, where effects happen
  | ExInj (inj (Expr struct prim inj name))     -- ^ extensible ability for expression

  -- | ExTyp typ     -- ^ unlift type to term level, enable explicit type application
  -- | ExSel name    -- ^ `.selector`, field projector
  -- | ExVar name (Maybe (Expr struct prim inj name))   -- ^ `` `Variant `` for polymorphic variant, variant constructor

deriving instance (Show (struct (Expr struct prim inj name)), Show (inj (Expr struct prim inj name)), Show (prim (Expr struct prim inj name)), Show name) => Show (Expr struct prim inj name)
deriving instance (Eq (struct (Expr struct prim inj name)), Eq (inj (Expr struct prim inj name)), Eq (prim (Expr struct prim inj name)), Eq name) => Eq (Expr struct prim inj name)

makeBaseFunctor $ fixQ [d|
  instance (Traversable inj, Traversable prim, Traversable struct) => Recursive (Expr struct prim inj name)
  |]
