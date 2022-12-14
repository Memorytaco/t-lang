module Tlang.Type.Concrete
  ( ConcreteType (..)
  -- , ConcreteTypeF (..)
  , SeqT (..)
  )
where

{-

This module build up abstraction of record type and sum type.

-}

import Tlang.Type.Primitive
import Tlang.Type.Class (LLVMTypeEncode (..), TypeEquality (..), LLVMTypeClass (..), TypeClass (..))
import qualified LLVM.AST.Type as AST (Type (..))

data ConcreteType t f where
  -- | We have BaseType contained, and it is a concrete type
  Lift :: BaseType t (ConcreteType t f) -> ConcreteType t f
  -- | Introduce whatever type system using `f`
  Field :: f (ConcreteType t f) -> ConcreteType t f

-- | A hint provided by user to determin which type we will use, but the result is not guarentee
data SeqT a where
  SeqVector :: a -> Integer -> SeqT a
  SeqArray  :: a -> Maybe Integer -> SeqT a

instance
  ( TypeEquality (t (BaseType t (ConcreteType t f))) (t (BaseType t (ConcreteType t f)))
  , TypeEquality (f (ConcreteType t f)) (f (ConcreteType t f))
  , TypeEquality (f (ConcreteType t f)) (ConcreteType t f)
  ) => Eq (ConcreteType t f) where
    (==) = (==?)

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
  show (SeqArray a i) = "[" <> show a <> " x " <> show (maybe 0 id i) <> "]"

instance LLVMTypeClass a => LLVMTypeClass (SeqT a) where
  classOf (SeqVector a _) = classOf a
  classOf _ = Aggregate

instance TypeEquality a a => TypeEquality (SeqT a) (SeqT a) where
  SeqVector a _ ==? SeqVector b _ = a ==? b
  SeqArray a _ ==? SeqArray b _ = a ==? b
  _ ==? _ = False

instance (LLVMTypeClass a, LLVMTypeEncode a) => LLVMTypeEncode (SeqT a) where
  encode v@(SeqVector a i) = case classOf v of
                               Aggregate -> AST.ArrayType (fromInteger i) $ encode a
                               Primitive -> AST.VectorType (fromInteger i) $ encode a
  encode (SeqArray a i) = AST.ArrayType (fromInteger $ maybe 0 id i) $ encode a

instance
  ( TypeEquality (t (BaseType t (ConcreteType t f))) (t (BaseType t (ConcreteType t f)))
  , TypeEquality (f (ConcreteType t f)) (f (ConcreteType t f))
  , TypeEquality (f (ConcreteType t f)) (ConcreteType t f)
  ) => TypeEquality (ConcreteType t f) (ConcreteType t f) where
    Lift a ==? Lift b = a ==? b
    Field a ==? Field b = a ==? b
    Lift a ==? b = b ==? a
    a ==? Lift b = a ==? b

instance
  ( TypeEquality (t (BaseType t (ConcreteType t f))) (t (BaseType t (ConcreteType t f)))
  , TypeEquality (f (ConcreteType t f)) (f (ConcreteType t f))
  , TypeEquality (f (ConcreteType t f)) (ConcreteType t f)
  ) => TypeEquality (ConcreteType t f) (BaseType t (ConcreteType t f)) where
    Lift a ==? Extend b = b ==? a
    Lift a ==? b = a ==? b
    Field a ==? Extend b = a ==? b
    Field _ ==? _ = False

