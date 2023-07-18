module main
;;

operator ; _ 0 infix
;;

foreign [cstd] cos: float -> float
;;
foreign [cstd] sin: float -> float
;;

foreign [cstd] printLn : str -> i8
;;
foreign [cstd] printInt: i32 -> i8
;;
foreign [cstd] printString: str -> i8
;;

let i: i32 = 699
;;

let main: i32 -> str -> i32 = \ ?argc =
[ ?argv: str =
    printLn "Hello world";
    let ?i = 32 in
    let ?b = 66 in
    let ?q = { a= 6: i8 , c = 80} in
    printString "I have variables of "; printInt argc;
    printLn "";
    0
]
;;
