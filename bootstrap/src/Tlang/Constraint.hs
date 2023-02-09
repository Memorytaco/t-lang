module Tlang.Constraint
  ( Constraint (..)
  , ConstraintF (..)
  , Bound (..)
  , (:<>) (..)
  , Bounds
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)

-- HM(X) constraint framework, both for term language and type language

-- | type constraint, parametrised with a predicate relation `rel`.
data Constraint rel name term typ
  = Satisfiable Bool                            -- ^ truth and falsity of the constraint
  | And (Constraint rel name term typ)          -- ^ Constraint conjunction operator
        (Constraint rel name term typ)
  | Project name (Constraint rel name term typ) -- ^ Existential quantifier
  | Predicate (rel typ)                         -- ^ Predicate application
  | Instantiate term typ                        -- ^ type scheme instatiation
  | Introduce term typ                          -- ^ def term: typ in C
      (Constraint rel name term typ)

deriving instance Functor rel => Functor (Constraint rel term name)
deriving instance Foldable rel => Foldable (Constraint rel term name)
deriving instance Traversable rel => Traversable (Constraint rel term name)
deriving instance (Show (rel typ), Show term, Show name, Show typ) => Show (Constraint rel name term typ)
deriving instance (Eq (rel typ), Eq term, Eq name, Eq typ) => Eq (Constraint rel name term typ)

type FoldFunctor f = (Functor f, Traversable f, Foldable f)
makeBaseFunctor [d| instance (FoldFunctor rel) => Recursive (Constraint rel name term typ) |]
$(deriveBifunctor ''Constraint)

data a :<> b = a :<> b deriving (Show, Eq, Functor, Foldable, Traversable)

-- | an extended MLF relation
data Bound name typ
  = name :~ typ -- ^ equality relation
  | name :> typ -- ^ lower bound or subsume
  deriving (Show, Eq, Functor, Traversable, Foldable)

-- data Prefix qual name typ
--   = typ :~ name  -- ^ `name` is rigid bound to `typ`
--   | typ :< name  -- ^ `name` is an instance of `typ`
--   | qual typ :? name  -- ^ `name` is qualified by `qual typ`
--   deriving (Show, Eq)

-- data Prefix label name typ
--   = Prefix (Bound name typ)
--   | RowRec (Bound name [(label, typ)])        -- ^ record row
--   | RowVar (Bound name [(label, Maybe typ)])  -- ^ variant row
--   deriving (Show, Eq)

-- data Qualify label typ
--   = AllOf [(label, typ)]        -- ^ for record
--   | AnyOf [(label, Maybe typ)]  -- ^ for variant

-- type Prefixs qual name typ = [Prefix qual name typ]
type Bounds name typ = [Bound name typ]

-- $(deriveBifunctor ''Prefix)
$(deriveBifunctor ''(:<>))
$(deriveBifunctor ''Bound)

