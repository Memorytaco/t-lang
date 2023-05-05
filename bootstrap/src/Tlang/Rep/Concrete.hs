{- | * runtime type representation

    This module introduces `DataRep` constructor to help
    translating high level type into machine type, which is
    LLVM IR type in this situation.

-}

module Tlang.Rep.Concrete
  ( DataRep (..)
  , SeqT (..)
  )
where

import Tlang.Rep.Primitive
import Tlang.Rep.Class (LLVMTypeEncode (..), LLVMTypeClass (..), TypeClass (..))
import Tlang.TH (fixQ)
import qualified LLVM.AST.Type as AST (Type (..))
import Data.Maybe (fromMaybe)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data DataRep t f where
  -- | We have PrimitiveT contained, and it is a concrete type
  RepLift :: PrimitiveT t (DataRep t f) -> DataRep t f
  -- | Introduce whatever type system using `f`
  DataRep :: f (DataRep t f) -> DataRep t f

deriving instance (Show (t (PrimitiveT t (DataRep t f))), Show (f (DataRep t f))) => Show (DataRep t f)
deriving instance (Eq (PrimitiveT t (DataRep t f)), Eq (f (DataRep t f))) => Eq (DataRep t f)

-- | A hint provided by user to determin which type we will use, but the result is not guarenteed
data SeqT a where
  SeqVector :: a -> Integer -> SeqT a
  SeqArray  :: a -> Maybe Integer -> SeqT a
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq a) => Eq (SeqT a)

instance
  ( LLVMTypeEncode (f (DataRep t f))
  , LLVMTypeEncode (t (PrimitiveT t (DataRep t f)))
  ) => LLVMTypeEncode (DataRep t f) where
    encode (RepLift a) = encode a
    encode (DataRep a) = encode a

instance
  ( LLVMTypeClass (f (DataRep t f))
  , LLVMTypeClass (t (PrimitiveT t (DataRep t f)))
  ) => LLVMTypeClass (DataRep t f) where
    classOf (RepLift a) = classOf a
    classOf (DataRep a) = classOf a

instance (Show a) => Show (SeqT a) where
  show (SeqVector a i) = "<" <> show a <> " x " <> show i <> ">"
  show (SeqArray a i) = "[" <> show a <> " x " <> show (fromMaybe 0 i) <> "]"

instance LLVMTypeClass a => LLVMTypeClass (SeqT a) where
  classOf (SeqVector a _) = classOf a
  classOf _ = Aggregate

instance (LLVMTypeClass a, LLVMTypeEncode a) => LLVMTypeEncode (SeqT a) where
  encode v@(SeqVector a i) = case classOf v of
                               Aggregate -> AST.ArrayType (fromInteger i) $ encode a
                               Primitive -> AST.VectorType (fromInteger i) $ encode a
  encode (SeqArray a i) = AST.ArrayType (fromInteger $ fromMaybe 0 i) $ encode a

type FoldFunctor f = (Functor f, Traversable f, Foldable f)
makeBaseFunctor $ fixQ [d| instance (FoldFunctor t, FoldFunctor f) => Recursive (DataRep t f) |]
