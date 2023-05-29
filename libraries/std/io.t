module std/io ;;

use std/ctype (cstr, cptr, cint)
;;

use std/type (str)
;;

// file descriptor
data fd = cint
;;

type handle = fd
;;

foreign [name "open"] open: str -> handle
;;
