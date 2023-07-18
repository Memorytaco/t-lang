module std/ctype ;;

// cstring, pointer to a sequence of bytes, ended with \0
data cstr = #[ptr (bit 8)]
;;

data cvoid
;;

// a strong typed c pointer
data cptr a = #[ptr $(a)]
;;

data cint = #[int32]
;;

// c integer
data cint8 = #[int8]
;;
data cint16 = #[int16]
;;
data cint32 = #[int32]
;;
data cint64 = #[int64]
;;

// c unsigned integer
data cuint8 = #[uint8]
;;
data cuint16 = #[uint16]
;;
data cuint32 = #[bit 32]
;;
data cuint64 = #[bit 64]
;;

// c floating
data cfloat = #[float]
;;

// c struct, allow copy of data.
// data cstruct (a : nat) = #[a]
// ;;
