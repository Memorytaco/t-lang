module std ;;

// standard boolean type
data bool
| false
| true
;;

type tuple a b =
{ fst : a
, snd : b
}
;;

// sequence operator
operator ; _ 0 infix
;;

let ; : forall a b. a -> b -> b =
[ _, ?a = a ]
;;

operator _ $ 0 infix
;;

let $ : forall a b. (a -> b) -> a -> b =
[ ?f, ?val = f val ]
;;

let when: forall a b. bool -> a -> b =
[ true, ?a, _ = a
| false, _, ?b = b
]
;;

operator & _ 0 infix
;;

let & : forall a b. a -> (a -> b) -> b =
[ ?val, ?f = f val ]
;;
