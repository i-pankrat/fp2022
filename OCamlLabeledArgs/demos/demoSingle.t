1. Mini-ML with basic and anonymous functions, closures and recursion

Factorial
  $ ./demoSingle.exe << EOF
  > let rec fact n = if n = 0 then 1 else n * fact (n - 1) ;;
  > fact 5 ;;
  > EOF
  val fact : int -> int = <fun>
  - : int = 120

Fixpointed factorial
  $ ./demoSingle.exe << EOF
  > let rec fix f x = f (fix f) x
  > let fact self n = if n = 0 then 1 else n * self (n - 1) ;;
  > let f = fix fact ;;
  > f 5 ;;
  > EOF
  val fix : (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f = <fun>
  val fact : (int -> int) -> int -> int = <fun>
  val f : int -> int = <fun>
  - : int = 120

Basic and anonymous function
  $ ./demoSingle.exe << EOF
  > let inc_basic x = x + 1
  > let inc_anonymous = fun x -> x + 1 ;;
  > inc_anonymous 4 ;;
  > EOF
  val inc_basic : int -> int = <fun>
  val inc_anonymous : int -> int = <fun>
  - : int = 5

Partial application
  $ ./demoSingle.exe << EOF
  > let add x y = x + y ;;              
  > let add4 = add 4 ;;     
  > add4 5 ;;
  > EOF
  val add : int -> int -> int = <fun>
  val add4 : int -> int = <fun>
  - : int = 9

Recursive function with multiple arguments
  $ ./demoSingle.exe << EOF
  > let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) ;;
  > pow 4 5 ;;
  > EOF
  val pow : int -> int -> int = <fun>
  - : int = 1024

Labeled arguments
  $ ./demoSingle.exe << EOF
  > let f ~x:x ~y:y = x / y ;;
  > f ~x:4 ~y:5 ;;
  > EOF
  val f : ~x:int -> ~y:int -> int = <fun>
  - : int = 0

Labeled arguments syntactic sugar
  $ ./demoSingle.exe << EOF
  > let f ~x ~y = x / y ;;
  > f ~x:4 ~y:5 ;;
  > EOF
  val f : ~x:int -> ~y:int -> int = <fun>
  - : int = 0

Labeled arguments swapped places
  $ ./demoSingle.exe << EOF
  > let f ~x:x ~y:y = x / y ;;
  > f ~y:4 ~x:5 ;;
  > EOF
  val f : ~x:int -> ~y:int -> int = <fun>
  - : int = 1

Optional arguments
  $ ./demoSingle.exe << EOF
  > let f ?x:(x = 0) ?y:(y = 0) z = x + y + z ;;
  > f ~y:5 (-1) ;;
  > EOF
  val f : ?x:int -> ?y:int -> int -> int = <fun>
  - : int -> int = 4

2. Type inference (with polymorphism)

Somewhat complicated inference
  $ ./demoSingle.exe << EOF
  > let f = (fun x y z -> if x (y * y) then z else x y) ;;
  > EOF
  val f : (int -> bool) -> int -> bool -> bool = <fun>

Polymorphism
  $ ./demoSingle.exe << EOF
  > let f = fun x y -> x ;;
  > EOF
  val f : 'a -> 'b -> 'a = <fun>

  $ ./demoSingle.exe << EOF
  > let x = [ []; [] ] ;;
  > EOF
  val x : 'b list list = [[]; []]

Examples when type checking fails
  $ ./demoSingle.exe << EOF
  > let x = if 1 then ();;
  > EOF
  Type error: Unification failed. Type of the input expression 'int', but expected 'bool'

  $ ./demoSingle.exe << EOF
  > let x = if true then 1 else [1 ; 2] ;;
  > EOF
  Type error: Unification failed. Type of the input expression 'int', but expected 'int list'

  $ ./demoSingle.exe << EOF
  > let x = [ fun x -> x + 1; 1 ] ;;
  > EOF
  Type error: Unification failed. Type of the input expression 'int -> int', but expected 'int'


3. Standart types (numbers, lists, 'option' not implemented)

Numbers
  $ ./demoSingle.exe << EOF
  > let x = ((4 + 1) / 2 * (4 - 2) mod 2 - (3 - 4)) / 2 ;;
  > EOF
  val x : int = 0

  $ ./demoSingle.exe << EOF
  > let x = ((4 <> 1) <= true <> (4 >= 2) > false <> (3 < 4)) <= true ;;
  > EOF
  val x : bool = true

Lists
  $ ./demoSingle.exe << EOF
  > let x = [ 5 / 4; 2; 3] ;;
  > EOF
  val x : int list = [1; 2; 3]

Legal runtime errors
  $ ./demoSingle.exe << EOF
  > let x = 4 / 0;;
  > EOF
  Runtime error: Division by zero


Use of REPL commands
  $ ./demoSingle.exe << EOF
  > #help
  > EOF
  Usage:
   #help         Prints a list of all available commands
   #quit         Exit the toplevel loop and terminate this program
   #use <file>   Read and evaluate source phrases from the given file
