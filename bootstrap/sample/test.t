fn cos: float -> float;
fn sin: float -> float;

data string = ref i8;

fn printLn: ref i8 -> i8;
fn printInt: i32 -> i8;
fn printString: ref i8 -> i8;

let i: i32 = 699;

fn test {
  "hello world"
}

fn main: i32 {
  printLn ("hello world!");
  printLn ("We are going to print a number 32");
  printString ("This is it: "); printInt (32); printLn ("");
  0
}
