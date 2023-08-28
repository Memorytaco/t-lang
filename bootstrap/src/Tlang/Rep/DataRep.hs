{- | * runtime type representation

    This module introduces `DataRep` constructor to help
    translating high level type into machine type, which is
    LLVM IR type in this situation.

-}
{-# LANGUAGE QuantifiedConstraints #-}

module Tlang.Rep.DataRep
  ( DataRep (..)
  , DataRepF (..)
  , SeqT (..)
  )
where

import Tlang.Rep.Class (EncodeLLVMType (..), LLVMTypeClass (..), TypeClass (..))
import Tlang.TH (fixQ)
import qualified LLVM.AST.Type as AST (Type (..))
import Data.Maybe (fromMaybe)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Prettyprinter (Pretty (..), encloseSep, (<+>))

-- | runtime representation for type
data DataRep t a where
  -- | We have PrimitiveT contained, and it is a concrete type
  RepLift :: t (DataRep t a) -> DataRep t a
  -- | Introduce whatever type system using `f`
  DataRep :: a -> DataRep t a
  deriving (Functor)

instance (forall x. Pretty x => Pretty (t x), Pretty a) => Pretty (DataRep t a) where
  pretty (RepLift v) = pretty v
  pretty (DataRep a) = pretty a

instance (Show (t (DataRep t a)), Show a) => Show (DataRep t a) where
  show (RepLift v) = show v
  show (DataRep a) = show a
deriving instance (Eq (t (DataRep t a)), Eq a) => Eq (DataRep t a)
deriving instance (Ord (t (DataRep t a)), Ord a) => Ord (DataRep t a)

-- | A hint provided by user to determin which type we will use, but the result is not guarenteed
data SeqT a where
  SeqVector :: a -> Integer -> SeqT a
  SeqArray  :: a -> Maybe Integer -> SeqT a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Pretty a => Pretty (SeqT a) where
  pretty (SeqVector a i) = encloseSep "<#" "#>" "x" [pretty a, pretty i]
  pretty (SeqArray a (Just i)) = encloseSep "[#" "#]" "x" [pretty a, pretty i]
  pretty (SeqArray a Nothing) = "[#" <+> pretty a  <+> "#]"

instance (EncodeLLVMType (t (DataRep t a)), EncodeLLVMType a) => EncodeLLVMType (DataRep t a) where
    encodeLLVMType (RepLift a) = encodeLLVMType a
    encodeLLVMType (DataRep a) = encodeLLVMType a

instance (LLVMTypeClass (t (DataRep t a)), LLVMTypeClass a) => LLVMTypeClass (DataRep t a) where
    classOf (RepLift a) = classOf a
    classOf (DataRep a) = classOf a

instance (Show a) => Show (SeqT a) where
  show (SeqVector a i) = "<" <> show a <> " x " <> show i <> ">"
  show (SeqArray a i) = "[" <> show a <> " x " <> show (fromMaybe 0 i) <> "]"

instance LLVMTypeClass a => LLVMTypeClass (SeqT a) where
  classOf (SeqVector a _) = classOf a
  classOf _ = Aggregate

instance (LLVMTypeClass a, EncodeLLVMType a) => EncodeLLVMType (SeqT a) where
  encodeLLVMType v@(SeqVector a i) =
    case classOf v of
      Aggregate -> AST.ArrayType (fromInteger i) $ encodeLLVMType a
      Primitive -> AST.VectorType (fromInteger i) $ encodeLLVMType a
  encodeLLVMType (SeqArray a i) = AST.ArrayType (fromInteger $ fromMaybe 0 i) $ encodeLLVMType a

makeBaseFunctor $ fixQ [d| instance (Functor t) => Recursive (DataRep t a) |]
