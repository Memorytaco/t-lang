{- | * Language primitive types

    it is the fundamental building blocks of native type.
-}

module Tlang.Rep.Primitive
  ( PrimitiveT (..)

  , ptr
  , bit
  , bool
  , float, double
  , int8, int16, int32, int64, int128, char, short, int, long, longlong
  , uint8, uint16, uint32, uint64, byte, uchar
  , struct
  , valueSpace
  )
where

import Tlang.Rep.Class (LLVMTypeEncode (..), LLVMTypeClass (..), TypeClass (..))
import Tlang.TH (fixQ)

import LLVM.AST.Type (Type (..), FloatingPointType (FloatFP, DoubleFP, FP128FP))
import LLVM.AST.AddrSpace (AddrSpace (..))

import Data.List (intercalate)
import Data.Char (toLower)

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)

-- | primitive type, as the type name suggests.
-- we use it to do a direct translation between host type and llvm IR type.
data PrimitiveT seq a where
  -- | see `PrimScalaType`
  Scala   :: ScalaType -> PrimitiveT seq a
  -- | pointer type
  Ptr     :: PrimitiveT seq a -> Maybe Integer -> PrimitiveT seq a
  -- | raw sequential type, array or vector
  Seq     :: seq (PrimitiveT seq a) -> PrimitiveT seq a
  -- | Aggregate type
  Struct  :: [PrimitiveT seq a] -> Bool -> PrimitiveT seq a
  -- | Allow other elements to be merged into primitive type
  Embed  :: a -> PrimitiveT seq a
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq a, Eq (t (PrimitiveT t a))) => Eq (PrimitiveT t a)
deriving instance (Ord a, Ord (t (PrimitiveT t a))) => Ord (PrimitiveT t a)

-- | Primitive scala type, the very basic type building block.
data ScalaType
  = NumT NumType    -- ^ Number type for floating and integer, all are signed. for arithmetic operation only.
  | DataT BitType   -- ^ Data type is used to represent user defined data, like interpreting memory chunk [u8 x 4] as u32.
                    -- it can also treat 16 bits as u16 and it is endian sensitive.
  deriving (Eq, Ord)

-- | Basic definition number type
data NumType = IntegerT IntegerLength
             | FloatT FloatLength
             deriving (Eq, Ord)

newtype BitType = Bit Integer deriving (Eq, Ord)

data IntegerLength = I8 | I16 | I32 | I64 | I128 deriving (Show, Eq, Ord, Enum)
data FloatLength = F32 | F64 | F128 deriving (Show, Eq, Ord, Enum)

instance (Show a, Show (t (PrimitiveT t a))) => Show (PrimitiveT t a) where
  show (Scala t) = show t <> "#"
  show (Ptr t _) = show t <> "*"
  show (Seq t) = show t
  show (Struct ts True) = "<{" <> intercalate ", " (show <$> ts) <> "}>"
  show (Struct ts False) = "{" <> intercalate ", " (show <$> ts) <> "}"
  -- show (Embed a) = "{|" <> show a <> "|}"
  show (Embed a) = show a

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
instance (LLVMTypeEncode a, LLVMTypeEncode (t (PrimitiveT t a))) => LLVMTypeEncode (PrimitiveT t a) where
  encode (Scala t) = encode t
  encode (Ptr _ addr'maybe) =
    case addr'maybe of
      Nothing -> PointerType $ AddrSpace 0
      Just space  -> PointerType $ AddrSpace (fromInteger space)
  encode (Seq t) = encode t
  -- struct with 0 elements is valid in llvm IR, so we simply keep it.
  encode (Struct ts packed) = StructureType packed $ encode <$> ts
  encode (Embed t) = encode t

instance (LLVMTypeClass (t (PrimitiveT t a)), LLVMTypeClass a) => LLVMTypeClass (PrimitiveT t a) where
  classOf (Scala _) = Primitive
  classOf (Ptr _ _) = Primitive
  classOf (Seq t) = classOf t
  classOf (Struct _ _) = Aggregate
  classOf (Embed t) = classOf t

bool :: PrimitiveT t a
bool = Scala . DataT $ Bit 1

double, float :: PrimitiveT t a
double = Scala . NumT $ FloatT F64
float = Scala . NumT $ FloatT F32

bit :: Integer -> PrimitiveT t a
bit = Scala . DataT . Bit

int8, int16, int32, int64, int128, char, short, int, long, longlong :: PrimitiveT t a
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

uint8, uint16, uint32, uint64, byte, uchar :: PrimitiveT t a
uint8 = Scala . DataT $ Bit 8
uint16 = Scala . DataT $ Bit 16
uint32 = Scala . DataT $ Bit 32
uint64 = Scala . DataT $ Bit 64
byte = uint8
uchar = byte

-- | to show number of enumerations this bit type can hold
valueSpace :: BitType -> Integer
valueSpace (Bit i) = 2 ^ i

ptr :: PrimitiveT t a -> PrimitiveT t a
ptr = flip Ptr Nothing

-- | structure with size align
struct :: [PrimitiveT seq a] -> PrimitiveT seq a
struct ts = Struct ts False

makeBaseFunctor $ fixQ [d| instance (Functor t) => Recursive (PrimitiveT t a) |]
