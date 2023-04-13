module std ;;

// for c style void type
data void
;;

// standard boolean type
data bool
| true
| false
;;

// sequence operator
infixl 0 ;
;;
let ; : forall a b. a -> b -> b =
[ _, ?a = a
]
;;

prefix 9 !
;;
let ! : forall a. ( () -> a ) -> a =
[ ?f = f ()
]
;;

let match: forall a b. a -> (a -> b) -> b =
[ ?val, ?f = f val
]
;;

infixr 1 $
;;

let $ : forall a b. (a -> b) -> a -> b =
[ ?f, ?val = f val
]
;;

let when: forall a b. bool -> a -> b =
[ true, ?a, _ = a
| false, _, ?b = b
]
;;

infixl 1 &
;;

let & : forall a b. a -> (a -> b) -> b =
[ ?val, ?f = f val ]
;;

data optional a
| none
| some: a
;;

data maybe a
| nothing
| just: a
;;

data either a b
| left: a
| right: b
;;

data ref a
| null
| ref: a
;;

type ptr a = #[ pointer #64 ]
;;

data list a
| nil
| cons: a * list a
;;
