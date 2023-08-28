module Language.Core.Constraint
  ( Constraint (..)
  , ConstraintF (..)
  , (:<>) (..)
  , (:>>) (..)
  , Prefix (..)
  , Prefixes (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)
import Tlang.TH (fixQ)
import Prettyprinter (Pretty (..), (<+>))

-- ** HM(X) constraint framework, both for term language and type language

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

makeBaseFunctor $ fixQ [d| instance (Functor rel) => Recursive (Constraint rel name term typ) |]
$(deriveBifunctor ''Constraint)

-- | constraint concatenation
data a :<> b = a :<> b deriving (Show, Eq, Functor, Foldable, Traversable)
-- | first order substitution
data a :>> b = a :>> b deriving (Show, Eq, Functor, Foldable, Traversable)

-- | MLF bounded quantifier
data Prefix name typ
  = name :~ typ -- ^ equality relation, rigid binding
  | name :> typ -- ^ lower bound or subsume, flexible binding
  deriving (Show, Ord, Eq, Functor, Traversable, Foldable)

instance (Pretty name, Pretty typ) => Pretty (Prefix name typ) where
  pretty (name :~ typ) = pretty name <+> "=" <+> pretty typ
  pretty (name :> typ) = pretty name <+> "~" <+> pretty typ

newtype Prefixes name typ = Prefixes [Prefix name typ]
  deriving (Show, Ord, Eq, Functor, Traversable, Foldable)
  deriving Pretty via [Prefix name typ]

-- data Prefix qual name typ
--   = typ :~ name  -- ^ `name` is rigid bound to `typ`
--   | typ :< name  -- ^ `name` is an instance of `typ`
--   | qual typ :? name  -- ^ `name` is qualified by `qual typ`
--   deriving (Show, Eq)

$(deriveBifunctor ''Prefix)
$(deriveBifunctor ''Prefixes)
$(deriveBifunctor ''(:<>))
$(deriveBifunctor ''(:>>))

