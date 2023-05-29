module std/ctype ;;

// cstring, pointer to a sequence of bytes, ended with \0
data cstr = #[ptr (bit 8)]
;;

data cvoid = #[void]
;;

// a strong typed c pointer
data cptr a = #[ptr a]
;;

data cint = #[native "int"]
;;

// c integer
data cint8 = #[sbit 8]
;;
data cint16 = #[sbit 16]
;;
data cint32 = #[sbit 32]
;;
data cint64 = #[sbit 64]
;;

// c unsigned integer
data cuint8 = #[bit 8]
;;
data cuint16 = #[bit 16]
;;
data cuint32 = #[bit 32]
;;
data cuint64 = #[bit 64]
;;

// c floating
data cfloat = #[native "float"]
;;

// c struct, allow copy of data.
data cstruct (a : nat) = #[a]
;;
