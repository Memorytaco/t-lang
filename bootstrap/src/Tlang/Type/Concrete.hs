module Tlang.Type.Concrete
  ( ConcreteType (..)
  , SeqT (..)
  )
where

{-

This module build up abstraction of record type and sum type.

-}

import Tlang.Type.Primitive
import Tlang.Type.Class (LLVMTypeEncode (..), LLVMTypeClass (..), TypeClass (..))
import qualified LLVM.AST.Type as AST (Type (..))
import Data.Maybe (fromMaybe)

data ConcreteType t f where
  -- | We have BaseType contained, and it is a concrete type
  Lift :: BaseType t (ConcreteType t f) -> ConcreteType t f
  -- | Introduce whatever type system using `f`
  Field :: f (ConcreteType t f) -> ConcreteType t f

-- | A hint provided by user to determin which type we will use, but the result is not guarenteed
data SeqT a where
  SeqVector :: a -> Integer -> SeqT a
  SeqArray  :: a -> Maybe Integer -> SeqT a
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq a) => Eq (SeqT a)

instance (Show (f (ConcreteType t f)), Show (t (BaseType t (ConcreteType t f)))) => Show (ConcreteType t f) where
  show (Lift t) = show t
  show (Field t) = show t

instance
  ( LLVMTypeEncode (f (ConcreteType t f))
  , LLVMTypeEncode (t (BaseType t (ConcreteType t f)))
  ) => LLVMTypeEncode (ConcreteType t f) where
    encode (Lift a) = encode a
    encode (Field a) = encode a

instance
  ( LLVMTypeClass (f (ConcreteType t f))
  , LLVMTypeClass (t (BaseType t (ConcreteType t f)))
  ) => LLVMTypeClass (ConcreteType t f) where
    classOf (Lift a) = classOf a
    classOf (Field a) = classOf a

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

