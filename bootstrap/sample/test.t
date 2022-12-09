module main
;;

use std ( ; )
;;

fn cos: float -> float
;;
fn sin: float -> float
;;

fn printLn : str -> i8
;;
fn printInt: i32 -> i8
;;
fn printString: str -> i8
;;

// binding is not available now
let i: i32 = 699
;;

let main: i32 -> str -> i32 = {
  argc, argv: str => 
    printLn "Hello world";
    let i = 32 in
    let b = 66 in
    printString "I have variables of "; printInt argc;
    printLn "";
    0
}

;;
