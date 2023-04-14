module std/io ;;

use std/string (cstr)
;;

data descriptor = #[ptr 64]
;;

type handle = descriptor
;;

fn open: cstr -> descriptor = "open"
;;
