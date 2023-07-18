module std/type
;;

use std;;

// optional data defintion
data optional a
| none
| some a
;;

data maybe a
| nothing
| just a
;;

data either a b
| left a
| right b
;;

data ref a
| null
| ref a
;;

data list a
| nil
| cons a (list a)
;;

// a string piece
data str = #[{uint32, ptr (bit 8)}]
;;

