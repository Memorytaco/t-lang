fn cos: float -> float;
fn sin: float -> float;

fn printLn : str -> i8;
fn printInt: i32 -> i8;
fn printString: str -> i8;

// binding is not available now
// let i: i32 = 699;

fn main: i32 {
  argc: i32 ->
  printLn "Hello world";
  printString "I have variables of "; printInt argc;
  printLn "";
  0
}
