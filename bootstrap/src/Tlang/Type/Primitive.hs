module Tlang.Type.Primitive
  ( PrimType (..)
  , bool
  , float, double
  , int8, int16, int32, int64, int128, char, short, int, long, longlong
  , uint8, uint16, uint32, uint64, byte, uchar
  , typeSpace, primSize, primArrayEleSize
  )
where

{-

Language primitive types, the fundamental building blocks of type.

-}

import Tlang.Type.Class (LLVMTypeConvert (..))
import LLVM.AST.Type (Type (..), FloatingPointType (FloatFP, DoubleFP, FP128FP))

data PrimType = PrimNum PrimNumType
              | PrimData PrimBitType
              | PrimArray PrimType PrimArrayLength
              deriving (Eq)

instance Show PrimType where
  show (PrimNum v) = show v
  show (PrimData v) = show v
  show (PrimArray t len) = "[" <> show t <> " x " <> show len <> "]"

data PrimNumType = PrimInteger IntegerLength
                 | PrimFloat FloatLength
                 deriving (Eq)
instance Show PrimNumType where
  show (PrimInteger v) = show v
  show (PrimFloat v) = show v

data PrimBitType = PrimBitType Integer BitAlign deriving (Eq)

instance Show PrimBitType where
  show (PrimBitType len _) = "bit" <> show len

data IntegerLength = I8 | I16 | I32 | I64 | I128 deriving (Show, Eq, Ord, Enum)
data FloatLength = F32 | F64 | F128 deriving (Show, Eq, Ord, Enum)
data BitAlign = BitAlignNone
              | BitAlign2 | BitAlign4 | BitAlign8 | BitAlign16 | BitAlign32 | BitAlign64
              deriving (Show, Eq, Ord)
data PrimArrayLength = ArrayLengthNone | ArrayLength Integer deriving (Eq)
instance Show PrimArrayLength where
  show (ArrayLength i) = show i
  show _ = "_"

instance LLVMTypeConvert PrimNumType where
  llvmType (PrimInteger I8)   = IntegerType 8
  llvmType (PrimInteger I16)  = IntegerType 16
  llvmType (PrimInteger I32)  = IntegerType 32
  llvmType (PrimInteger I64)  = IntegerType 64
  llvmType (PrimInteger I128) = IntegerType 128
  llvmType (PrimFloat F32)    = FloatingPointType FloatFP
  llvmType (PrimFloat F64)    = FloatingPointType DoubleFP
  llvmType (PrimFloat F128)   = FloatingPointType FP128FP
instance LLVMTypeConvert PrimBitType where
  llvmType (PrimBitType i _) = IntegerType $ fromInteger i
instance LLVMTypeConvert PrimType where
  llvmType (PrimNum t) = llvmType t
  llvmType (PrimData t) = llvmType t
  llvmType (PrimArray ele len) =
    case len of
      ArrayLengthNone -> ArrayType 0 $ llvmType ele
      ArrayLength i -> ArrayType (fromInteger i) $ llvmType ele

bool :: PrimType
bool = PrimData $ PrimBitType 1 BitAlignNone

double, float :: PrimType
double = PrimNum $ PrimFloat F64
float = PrimNum $ PrimFloat F32

int8, int16, int32, int64, int128, char, short, int, long, longlong :: PrimType
int8 = PrimNum $ PrimInteger I8
int16 = PrimNum $ PrimInteger I16
int32 = PrimNum $ PrimInteger I32
int64 = PrimNum $ PrimInteger I64
int128 = PrimNum $ PrimInteger I128
char = int8
short = int16
int = int32
long = int64
longlong = int128

uint8, uint16, uint32, uint64, byte, uchar :: PrimType
uint8 = PrimData $ PrimBitType 8 BitAlign8
uint16 = PrimData $ PrimBitType 16 BitAlign16
uint32 = PrimData $ PrimBitType 32 BitAlign32
uint64 = PrimData $ PrimBitType 64 BitAlign64
byte = uint8
uchar = byte

primSize :: PrimType -> Integer
primSize (PrimNum ntyp) =
  case ntyp of
    PrimInteger I8 -> 8
    PrimInteger I16 -> 16
    PrimInteger I32 -> 32
    PrimInteger I64 -> 64
    PrimInteger I128 -> 128
    PrimFloat F32 -> 32
    PrimFloat F64 -> 64
    PrimFloat F128 -> 128
primSize (PrimData (PrimBitType i align)) =
  case align of
    BitAlignNone -> i
    BitAlign2 -> i + remain 2 i
    BitAlign4 -> i + remain 4 i
    BitAlign8 -> i + remain 8 i
    BitAlign16 -> i + remain 16 i
    BitAlign32 -> i + remain 32 i
    BitAlign64 -> i + remain 64 i
    where
      remain :: Integer -> Integer -> Integer
      remain r v = r - (v `rem` r)
primSize (PrimArray typ alen) =
  case alen of
    ArrayLengthNone -> primSize typ
    ArrayLength len -> len * primSize typ

primArrayEleSize :: PrimType -> Maybe Integer
primArrayEleSize (PrimArray typ _) = Just $ primSize typ
primArrayEleSize _ = Nothing

primArrayLength :: PrimType -> Either String (Maybe Integer)
primArrayLength (PrimArray _ alen) =
  case alen of
    ArrayLengthNone -> Right Nothing
    ArrayLength len -> Right $ Just len
primArrayLength _ = Left "Not An Array Primitive Type"

typeSpace :: PrimBitType -> Integer
typeSpace (PrimBitType i _) = 2 ^ i
