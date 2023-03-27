module Tlang.Type.Primitive
  ( BaseType (..)

  , ptr
  , bit
  , bool
  , float, double
  , int8, int16, int32, int64, int128, char, short, int, long, longlong
  , uint8, uint16, uint32, uint64, byte, uchar
  , typeSpace
  )
where

{-

Language primitive types, the fundamental building blocks of type.

-}

import Tlang.Type.Class (LLVMTypeEncode (..), LLVMTypeClass (..), TypeClass (..))

import LLVM.AST.Type (Type (..), FloatingPointType (FloatFP, DoubleFP, FP128FP))
import LLVM.AST.AddrSpace (AddrSpace (..))

import Data.List (intercalate)
import Data.Char (toLower)

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)

data BaseType t a where
  -- | see `PrimScalaType`
  Scala   :: ScalaType -> BaseType t a
  -- | pointer type
  Ptr     :: BaseType t a -> Maybe Integer -> BaseType t a
  -- | raw sequential type, array or vector
  Seq     :: t (BaseType t a) -> BaseType t a
  -- | Aggregate type
  Struct  :: [BaseType t a] -> Bool -> BaseType t a
  Extend  :: a -> BaseType t a

deriving instance Functor t => Functor (BaseType t)
deriving instance (Eq a, Eq (t (BaseType t a))) => Eq (BaseType t a)
deriving instance Foldable t => Foldable (BaseType t)
deriving instance Traversable t => Traversable (BaseType t)

-- | Primitive scala type, the very basic type building block.
data ScalaType
  = NumT NumType    -- ^ Number type for floating and integer, all are signed. for arithmetic operation only.
  | DataT BitType   -- ^ Data type used to represent hold user defined data like interpret [u8 x 4] as u32
  deriving (Eq)

data NumType = IntegerT IntegerLength
             | FloatT FloatLength
             deriving (Eq)

newtype BitType = Bit Integer deriving (Eq)

data IntegerLength = I8 | I16 | I32 | I64 | I128 deriving (Show, Eq, Ord, Enum)
data FloatLength = F32 | F64 | F128 deriving (Show, Eq, Ord, Enum)

instance (Show a, Show (t (BaseType t a))) => Show (BaseType t a) where
  show (Scala t) = show t <> "#"
  show (Ptr t _) = show t <> "*"
  show (Seq t) = show t
  show (Struct ts True) = "<{" <> intercalate ", " (show <$> ts) <> "}>"
  show (Struct ts False) = "{" <> intercalate ", " (show <$> ts) <> "}"
  -- show (Extend a) = "{|" <> show a <> "|}"
  show (Extend a) = show a

instance Show ScalaType where
  show (NumT t) = show t
  show (DataT t) = show t

instance Show NumType where
  show (IntegerT v) = toLower <$> show v
  show (FloatT v) = toLower <$> show v

instance Show BitType where
  show (Bit len) = "u" <> show len

instance LLVMTypeEncode NumType where
  encode (IntegerT I8)   = IntegerType 8
  encode (IntegerT I16)  = IntegerType 16
  encode (IntegerT I32)  = IntegerType 32
  encode (IntegerT I64)  = IntegerType 64
  encode (IntegerT I128) = IntegerType 128
  encode (FloatT F32)    = FloatingPointType FloatFP
  encode (FloatT F64)    = FloatingPointType DoubleFP
  encode (FloatT F128)   = FloatingPointType FP128FP
instance LLVMTypeEncode BitType where
  encode (Bit i) = IntegerType $ fromInteger i
instance LLVMTypeEncode ScalaType where
  encode (NumT t) = encode t
  encode (DataT t) = encode t
instance (LLVMTypeEncode a, LLVMTypeEncode (t (BaseType t a))) => LLVMTypeEncode (BaseType t a) where
  encode (Scala t) = encode t
  encode (Ptr t addr'maybe) =
    case addr'maybe of
      Nothing -> undefined
      Just space  -> undefined
  encode (Seq t) = encode t
  encode (Struct ts packed) = StructureType packed $ encode <$> ts  -- FIXME: structure type with non elements is confusing
  encode (Extend t) = encode t

instance (LLVMTypeClass (t (BaseType t a)), LLVMTypeClass a) => LLVMTypeClass (BaseType t a) where
  classOf (Scala _) = Primitive
  classOf (Ptr _ _) = Primitive
  classOf (Seq t) = classOf t
  classOf (Struct _ _) = Aggregate
  classOf (Extend t) = classOf t

bool :: BaseType t a
bool = Scala . DataT $ Bit 1

double, float :: BaseType t a
double = Scala . NumT $ FloatT F64
float = Scala . NumT $ FloatT F32

bit :: Integer -> BaseType t a
bit = Scala . DataT . Bit

int8, int16, int32, int64, int128, char, short, int, long, longlong :: BaseType t a
int8 = Scala . NumT $ IntegerT I8
int16 = Scala . NumT $ IntegerT I16
int32 = Scala . NumT $ IntegerT I32
int64 = Scala . NumT $ IntegerT I64
int128 = Scala . NumT $ IntegerT I128
char = int8
short = int16
int = int32
long = int64
longlong = int128

uint8, uint16, uint32, uint64, byte, uchar :: BaseType t a
uint8 = Scala . DataT $ Bit 8
uint16 = Scala . DataT $ Bit 16
uint32 = Scala . DataT $ Bit 32
uint64 = Scala . DataT $ Bit 64
byte = uint8
uchar = byte

typeSpace :: BitType -> Integer
typeSpace (Bit i) = 2 ^ i

ptr :: BaseType t a -> BaseType t a
ptr = flip Ptr Nothing

type FoldFunctor f = (Functor f, Traversable f, Foldable f)
makeBaseFunctor [d| instance (FoldFunctor t) => Recursive (BaseType t a) |]

