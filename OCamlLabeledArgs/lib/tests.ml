(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parsetree
open Typedtree

(* ------------------------------------------------------ *)
(* --------------------- Test cases --------------------- *)
(* ------------------------------------------------------ *)

(* (1) Factorial *)
let test_factorial_definition = "let rec fact n = if n = 0 then 1 else n * fact (n - 1)"
let test_factorial_call = "fact 5"

let test_factorial_expression =
  "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5"
;;

let test_factorial_expected =
  let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
  fact 5
;;

(* (2) X into the power of Y *)
let test_x_power_y_definition = "let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)"
let test_x_power_y_call = "pow 4 5"

let test_x_power_y_expression =
  "let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) in pow 4 5"
;;

let test_x_power_y_expected =
  let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) in
  pow 4 5
;;

(* (3) Increment *)
let test_increment_definition = "let inc = fun x -> x + 1"
let test_increment_call = "inc 4"
let test_increment_expression = "let inc = fun x -> x + 1 in inc 4"

let test_increment_expected =
  let inc x = x + 1 in
  inc 4
;;

(* (4) Labeled arguments *)
let test_labeled_arguments_definition = "let f ~name1:x ~name2:y = x / y"
let test_labeled_arguments_call = "f ~name1:4 ~name2:5"

let test_labeled_arguments_expression =
  "let f ~name1:x ~name2:y = x / y in f ~name1:4 ~name2:5"
;;

let test_labeled_arguments_expected =
  let f ~name1:x ~name2:y = x / y in
  f ~name1:4 ~name2:5
;;

(* (5) Labeled arguments syntactic sugar *)
let test_labeled_arguments_sugar_definition = "let f ~x ~y = x + y"
let test_labeled_arguments_sugar_call = "f ~x:4 ~y:5"
let test_labeled_arguments_sugar_expression = "let f ~x ~y = x + y in f ~x:4 ~y:5"

let test_labeled_arguments_sugar_expected =
  let f ~x ~y = x + y in
  f ~x:4 ~y:5
;;

(* (6) Optional arguments *)
let test_optional_arguments_definition = "let f ?name:(arg1=4) arg2 = arg1 + arg2"
let test_optional_arguments_call = "f 5"
let test_optional_arguments_expression = "let f ?name:(arg1=4) arg2 = arg1 + arg2 in f 5"

let test_optional_arguments_expected =
  let f ?name:(arg1 = 4) arg2 = arg1 + arg2 in
  f 5
;;

(* (7) Complex optional arguments *)
let test_optional_arguments_complex_definition =
  "let f ?x:(x = 0) ?y:(y = 0) () ?z:(z = 0) () = x + y + z"
;;

let test_optional_arguments_complex_call = "f ~x:5 () ()"

let test_optional_arguments_complex_expression =
  "let f ?x:(x = 0) ?y:(y = 0) () ?z:(z = 0) () = x + y + z in f ~x:5 () ()"
;;

let test_optional_arguments_complex_expected =
  let f ?(x = 0) ?(y = 0) () ?(z = 0) () = x + y + z in
  f ~x:5 () ()
;;

(* (8) Labeled arguments swapped places *)
let test_labeled_arguments_swapped_places_definition =
  "let f ~name2 ~name1 = name1 / name2"
;;

let test_labeled_arguments_swapped_places_call = "f ~name2:4 ~name1:5"

let test_labeled_arguments_swapped_places_expression =
  "let f ~name2 ~name1 = name1 / name2 in f ~name2:4 ~name1:5"
;;

let test_labeled_arguments_swapped_places_expected =
  let f ~name2 ~name1 = name1 / name2 in
  f ~name2:4 ~name1:5
;;

(* (9) Lambda with multiple arguments *)
let test_mult_arguments_definition = "let f = (fun x y z -> if x (y * y) then z else x y)"

(* ------------------------------------------------------ *)
(* -------------------- Parser tests -------------------- *)
(* ------------------------------------------------------ *)

open Parser

(* -------------------- Combinators --------------------- *)
(* Base combinators *)
let%test _ = parse identifier "_" = Ok "_"
let%test _ = parse ignored "\t\n\r " = Ok ()
(* Arguments combinator*)
let%test _ = parse label_parser "~label:" = Ok (ArgLabeled "label")
let%test _ = parse label_parser "?label:" = Ok (ArgOptional "label")
(* Var combinator *)
let%test _ = parse expr_parser "name" = Ok (Var "name")
let%test _ = parse expr_parser "name1" = Ok (Var "name1")
let%test _ = parse expr_parser "_name" = Ok (Var "_name")
let%test _ = parse expr_parser "_name1'" = Ok (Var "_name1'")
(* Const combinator *)
let%test _ = parse expr_parser "true" = Ok (Const (Bool true))
let%test _ = parse expr_parser "false" = Ok (Const (Bool false))
let%test _ = parse expr_parser "19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "+19" = Ok (Const (Int 19))
let%test _ = parse expr_parser "-19" = Ok (Const (Int ~-19))
let%test _ = parse expr_parser "()" = Ok (Const Unit)
let%test _ = parse expr_parser "(  )" = Ok (Const Unit)
(* Binop combinator *)
let%test _ = parse expr_parser "name - name" = Ok (Binop (Minus, Var "name", Var "name"))
let%test _ = parse expr_parser "1 + 9" = Ok (Binop (Plus, Const (Int 1), Const (Int 9)))

let%test _ =
  parse expr_parser "1 * (3 / 9)"
  = Ok (Binop (Mult, Const (Int 1), Binop (Divide, Const (Int 3), Const (Int 9))))
;;

let%test _ =
  parse expr_parser "(1 >= 3) < 9"
  = Ok (Binop (Lt, Binop (Gtq, Const (Int 1), Const (Int 3)), Const (Int 9)))
;;

(* Lambda combinator (fun x -> e) *)
let%test _ = parse expr_parser "fun x -> e" = Ok (Fun (ArgNoLabel, None, "x", Var "e"))

let%test _ =
  parse expr_parser "fun x y -> e"
  = Ok (Fun (ArgNoLabel, None, "x", Fun (ArgNoLabel, None, "y", Var "e")))
;;

(* If-then-else combinator (if b then e else e') *)
let%test _ =
  parse expr_parser "if e1 then e2" = Ok (IfThenElse (Var "e1", Var "e2", Const Unit))
;;

let%test _ =
  parse expr_parser "if e1 then e2 else e3"
  = Ok (IfThenElse (Var "e1", Var "e2", Var "e3"))
;;

(* Let combinator (let x = e in e') *)
let%test _ = parse expr_parser "let x = e in e'" = Ok (Let ("x", Var "e", Var "e'"))

let%test _ =
  parse expr_parser "let x = 1 in let y = 9 in x <= y"
  = Ok (Let ("x", Const (Int 1), Let ("y", Const (Int 9), Binop (Ltq, Var "x", Var "y"))))
;;

let%test _ =
  parse expr_parser "let rec x = e in e'" = Ok (LetRec ("x", Var "e", Var "e'"))
;;

(* App combinator (f x y) *)
let%test _ = parse expr_parser "f x" = Ok (App (Var "f", ArgNoLabel, Var "x"))

let%test _ =
  parse expr_parser "f (x) (y)"
  = Ok (App (App (Var "f", ArgNoLabel, Var "x"), ArgNoLabel, Var "y"))
;;

(* Definition combinator (let f x = x) *)
let%test _ =
  parse definition_parser "let f x y z = x + y + z"
  = Ok
      ( "f"
      , Fun
          ( ArgNoLabel
          , None
          , "x"
          , Fun
              ( ArgNoLabel
              , None
              , "y"
              , Fun
                  ( ArgNoLabel
                  , None
                  , "z"
                  , Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z") ) ) ) )
;;

(* ------------- Combinations of combinators ---------------- *)

(* (1) Factorial *)
let%test _ =
  parse definition_parser test_factorial_definition
  = Ok
      ( "fact"
      , LetRec
          ( "fact"
          , Fun
              ( ArgNoLabel
              , None
              , "n"
              , IfThenElse
                  ( Binop (Eq, Var "n", Const (Int 0))
                  , Const (Int 1)
                  , Binop
                      ( Mult
                      , Var "n"
                      , App (Var "fact", ArgNoLabel, Binop (Minus, Var "n", Const (Int 1)))
                      ) ) )
          , Var "fact" ) )
;;

let%test _ =
  parse expr_parser test_factorial_call = Ok (App (Var "fact", ArgNoLabel, Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_factorial_expression
  = Ok
      (LetRec
         ( "fact"
         , Fun
             ( ArgNoLabel
             , None
             , "n"
             , IfThenElse
                 ( Binop (Eq, Var "n", Const (Int 0))
                 , Const (Int 1)
                 , Binop
                     ( Mult
                     , Var "n"
                     , App (Var "fact", ArgNoLabel, Binop (Minus, Var "n", Const (Int 1)))
                     ) ) )
         , App (Var "fact", ArgNoLabel, Const (Int 5)) ))
;;

(* (2) X into the power of Y *)
let%test _ =
  parse definition_parser test_x_power_y_definition
  = Ok
      ( "pow"
      , LetRec
          ( "pow"
          , Fun
              ( ArgNoLabel
              , None
              , "x"
              , Fun
                  ( ArgNoLabel
                  , None
                  , "y"
                  , IfThenElse
                      ( Binop (Eq, Var "y", Const (Int 0))
                      , Const (Int 1)
                      , Binop
                          ( Mult
                          , Var "x"
                          , App
                              ( App (Var "pow", ArgNoLabel, Var "x")
                              , ArgNoLabel
                              , Binop (Minus, Var "y", Const (Int 1)) ) ) ) ) )
          , Var "pow" ) )
;;

let%test _ =
  parse expr_parser test_x_power_y_call
  = Ok (App (App (Var "pow", ArgNoLabel, Const (Int 4)), ArgNoLabel, Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_x_power_y_expression
  = Ok
      (LetRec
         ( "pow"
         , Fun
             ( ArgNoLabel
             , None
             , "x"
             , Fun
                 ( ArgNoLabel
                 , None
                 , "y"
                 , IfThenElse
                     ( Binop (Eq, Var "y", Const (Int 0))
                     , Const (Int 1)
                     , Binop
                         ( Mult
                         , Var "x"
                         , App
                             ( App (Var "pow", ArgNoLabel, Var "x")
                             , ArgNoLabel
                             , Binop (Minus, Var "y", Const (Int 1)) ) ) ) ) )
         , App (App (Var "pow", ArgNoLabel, Const (Int 4)), ArgNoLabel, Const (Int 5)) ))
;;

(* (3) Increment *)
let%test _ =
  parse definition_parser test_increment_definition
  = Ok ("inc", Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1))))
;;

let%test _ =
  parse expr_parser test_increment_call = Ok (App (Var "inc", ArgNoLabel, Const (Int 4)))
;;

let%test _ =
  parse expr_parser test_increment_expression
  = Ok
      (Let
         ( "inc"
         , Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1)))
         , App (Var "inc", ArgNoLabel, Const (Int 4)) ))
;;

(* (4) Labeled arguments *)
let%test _ =
  parse definition_parser test_labeled_arguments_definition
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name1"
          , None
          , "x"
          , Fun (ArgLabeled "name2", None, "y", Binop (Divide, Var "x", Var "y")) ) )
;;

let%test _ =
  parse expr_parser test_labeled_arguments_call
  = Ok
      (App
         ( App (Var "f", ArgLabeled "name1", Const (Int 4))
         , ArgLabeled "name2"
         , Const (Int 5) ))
;;

let%test _ =
  parse expr_parser test_labeled_arguments_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgLabeled "name1"
             , None
             , "x"
             , Fun (ArgLabeled "name2", None, "y", Binop (Divide, Var "x", Var "y")) )
         , App
             ( App (Var "f", ArgLabeled "name1", Const (Int 4))
             , ArgLabeled "name2"
             , Const (Int 5) ) ))
;;

(* (5) Labeled arguments syntactic sugar *)
let%test _ =
  parse definition_parser test_labeled_arguments_sugar_definition
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "x"
          , None
          , "x"
          , Fun (ArgLabeled "y", None, "y", Binop (Plus, Var "x", Var "y")) ) )
;;

let%test _ =
  parse expr_parser test_labeled_arguments_sugar_call
  = Ok (App (App (Var "f", ArgLabeled "x", Const (Int 4)), ArgLabeled "y", Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_labeled_arguments_sugar_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgLabeled "x"
             , None
             , "x"
             , Fun (ArgLabeled "y", None, "y", Binop (Plus, Var "x", Var "y")) )
         , App
             (App (Var "f", ArgLabeled "x", Const (Int 4)), ArgLabeled "y", Const (Int 5))
         ))
;;

(* (6) Optional arguments *)

let%test _ =
  parse definition_parser test_optional_arguments_definition
  = Ok
      ( "f"
      , Fun
          ( ArgOptional "name"
          , Some (Const (Int 4))
          , "arg1"
          , Fun (ArgNoLabel, None, "arg2", Binop (Plus, Var "arg1", Var "arg2")) ) )
;;

let%test _ =
  parse expr_parser test_optional_arguments_call
  = Ok (App (Var "f", ArgNoLabel, Const (Int 5)))
;;

let%test _ =
  parse expr_parser test_optional_arguments_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgOptional "name"
             , Some (Const (Int 4))
             , "arg1"
             , Fun (ArgNoLabel, None, "arg2", Binop (Plus, Var "arg1", Var "arg2")) )
         , App (Var "f", ArgNoLabel, Const (Int 5)) ))
;;

(* (7) Complex optional arguments *)
let%test _ =
  parse definition_parser test_optional_arguments_complex_definition
  = Ok
      ( "f"
      , Fun
          ( ArgOptional "x"
          , Some (Const (Int 0))
          , "x"
          , Fun
              ( ArgOptional "y"
              , Some (Const (Int 0))
              , "y"
              , Fun
                  ( ArgNoLabel
                  , None
                  , ""
                  , Fun
                      ( ArgOptional "z"
                      , Some (Const (Int 0))
                      , "z"
                      , Fun
                          ( ArgNoLabel
                          , None
                          , ""
                          , Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z") ) ) ) )
          ) )
;;

let%test _ =
  parse expr_parser test_optional_arguments_complex_call
  = Ok
      (App
         ( App (App (Var "f", ArgLabeled "x", Const (Int 5)), ArgNoLabel, Const Unit)
         , ArgNoLabel
         , Const Unit ))
;;

let%test _ =
  parse expr_parser test_optional_arguments_complex_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgOptional "x"
             , Some (Const (Int 0))
             , "x"
             , Fun
                 ( ArgOptional "y"
                 , Some (Const (Int 0))
                 , "y"
                 , Fun
                     ( ArgNoLabel
                     , None
                     , ""
                     , Fun
                         ( ArgOptional "z"
                         , Some (Const (Int 0))
                         , "z"
                         , Fun
                             ( ArgNoLabel
                             , None
                             , ""
                             , Binop (Plus, Binop (Plus, Var "x", Var "y"), Var "z") ) )
                     ) ) )
         , App
             ( App (App (Var "f", ArgLabeled "x", Const (Int 5)), ArgNoLabel, Const Unit)
             , ArgNoLabel
             , Const Unit ) ))
;;

(* (8) Labeled arguments swapped places *)
let%test _ =
  parse definition_parser test_labeled_arguments_swapped_places_definition
  = Ok
      ( "f"
      , Fun
          ( ArgLabeled "name2"
          , None
          , "name2"
          , Fun
              (ArgLabeled "name1", None, "name1", Binop (Divide, Var "name1", Var "name2"))
          ) )
;;

let%test _ =
  parse expr_parser test_labeled_arguments_swapped_places_call
  = Ok
      (App
         ( App (Var "f", ArgLabeled "name2", Const (Int 4))
         , ArgLabeled "name1"
         , Const (Int 5) ))
;;

let%test _ =
  parse expr_parser test_labeled_arguments_swapped_places_expression
  = Ok
      (Let
         ( "f"
         , Fun
             ( ArgLabeled "name2"
             , None
             , "name2"
             , Fun
                 ( ArgLabeled "name1"
                 , None
                 , "name1"
                 , Binop (Divide, Var "name1", Var "name2") ) )
         , App
             ( App (Var "f", ArgLabeled "name2", Const (Int 4))
             , ArgLabeled "name1"
             , Const (Int 5) ) ))
;;

(* (9) Lambda with multiple arguments *)

let%test _ =
  parse definition_parser test_mult_arguments_definition
  = Ok
      ( "f"
      , Fun
          ( ArgNoLabel
          , None
          , "x"
          , Fun
              ( ArgNoLabel
              , None
              , "y"
              , Fun
                  ( ArgNoLabel
                  , None
                  , "z"
                  , IfThenElse
                      ( App (Var "x", ArgNoLabel, Binop (Mult, Var "y", Var "y"))
                      , Var "z"
                      , App (Var "x", ArgNoLabel, Var "y") ) ) ) ) )
;;

(* ------------------------------------------------------ *)
(* -------------------- Infer tests --------------------- *)
(* ------------------------------------------------------ *)
open Infer

let test_ast_infer ?(env = TypeEnv.empty) exp =
  match Infer.infer exp env with
  | Result.Ok t -> Prettyprint.pp_typ Format.std_formatter t
  | Result.Error e -> Prettyprint.pp_error Format.std_formatter e
;;

(* --------------- Test success --------------- *)

let%expect_test _ =
  test_ast_infer (Const (Bool true));
  [%expect {| bool |}]
;;

let%expect_test _ =
  test_ast_infer (Const (Int 19));
  [%expect {| int |}]
;;

let%expect_test _ =
  test_ast_infer (Const Unit);
  [%expect {| unit |}]
;;

let%expect_test _ =
  test_ast_infer (Binop (Plus, Const (Int 4), Const (Int 5)));
  [%expect {| int |}]
;;

let%expect_test _ =
  test_ast_infer (Binop (Lt, Binop (Plus, Const (Int 4), Const (Int 5)), Const (Int 9)));
  [%expect {| bool |}]
;;

let%expect_test _ =
  test_ast_infer
    (IfThenElse
       (Const (Bool true), Const (Int 4), Binop (Plus, Const (Int 4), Const (Int 5))));
  [%expect {| int |}]
;;

let%expect_test _ =
  test_ast_infer (Fun (ArgNoLabel, None, "x", Fun (ArgNoLabel, None, "y", Const Unit)));
  [%expect {| 'a -> 'b -> unit |}]
;;

let%expect_test _ =
  test_ast_infer
    (Fun (ArgLabeled "lab", None, "x", Fun (ArgNoLabel, None, "y", Const Unit)));
  [%expect {| ~lab:'a -> 'b -> unit |}]
;;

let%expect_test _ =
  test_ast_infer
    (Fun (ArgOptional "lab", None, "x", Fun (ArgNoLabel, None, "y", Const Unit)));
  [%expect {| ?lab:'a -> 'b -> unit |}]
;;

let%expect_test _ =
  test_ast_infer (Const Nil);
  [%expect {| 'a list |}]
;;

let%expect_test _ =
  test_ast_infer
    (Cons (Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Const Nil))));
  [%expect {| int list |}]
;;

let%expect_test _ =
  test_ast_infer
    (Cons (Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Const Nil))));
  [%expect {| int list |}]
;;

(* [ [[]]; [[1; 2; 3]] ]*)
let%expect_test _ =
  test_ast_infer
    (Cons
       ( Cons (Const Nil, Const Nil)
       , Cons
           ( Cons
               ( Cons
                   (Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Const Nil)))
               , Const Nil )
           , Const Nil ) ));
  [%expect {| int list list list |}]
;;

(* fun x y z -> if x (y * y) then z else x y *)
let%expect_test _ =
  test_ast_infer
    (Fun
       ( ArgNoLabel
       , None
       , "x"
       , Fun
           ( ArgNoLabel
           , None
           , "y"
           , Fun
               ( ArgNoLabel
               , None
               , "z"
               , IfThenElse
                   ( App (Var "x", ArgNoLabel, Binop (Mult, Var "y", Var "y"))
                   , Var "z"
                   , App (Var "x", ArgNoLabel, Var "y") ) ) ) ));
  [%expect {| (int -> bool) -> int -> bool -> bool |}]
;;

(* --------------- Test failure --------------- *)
let%expect_test _ =
  test_ast_infer (Var "x");
  [%expect {| Type error: Undefined variable 'x' |}]
;;

let%expect_test _ =
  test_ast_infer (Binop (Lt, Binop (Gtq, Const (Int 1), Const (Int 3)), Const (Int 0)));
  [%expect
    {| Type error: Unification failed. Type of the input expression 'bool', but expected 'int' |}]
;;

(* Error in if condition *)
let%expect_test _ =
  test_ast_infer (IfThenElse (Const (Int 1), Const Unit, Const Unit));
  [%expect
    {| Type error: Unification failed. Type of the input expression 'int', but expected 'bool' |}]
;;

(* Error in if branches *)
let%expect_test _ =
  test_ast_infer (IfThenElse (Const (Bool true), Const (Int 1), Const (Bool false)));
  [%expect
    {| Type error: Unification failed. Type of the input expression 'int', but expected 'bool' |}]
;;

(* Elements of a list don't have the same type *)
(* [ [[false; true]]; [[1; 2; 3]] ]*)
let%expect_test _ =
  test_ast_infer
    (Cons
       ( Cons (Cons (Const (Bool false), Cons (Const (Bool true), Const Nil)), Const Nil)
       , Cons
           ( Cons
               ( Cons
                   (Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Const Nil)))
               , Const Nil )
           , Const Nil ) ));
  [%expect
    {| Type error: Unification failed. Type of the input expression 'bool', but expected 'int' |}]
;;

(* --------------- Test testcases --------------- *)
let test_parse_and_infer_single_expr_or_definition ?(env = TypeEnv.empty) test_case =
  let test_inner = function
    | Definition (_name, exp) ->
      (match infer exp env with
       | Result.Ok t -> Prettyprint.pp_typ Format.std_formatter t
       | Result.Error e -> Prettyprint.pp_error Format.std_formatter e)
    | Expression exp ->
      (match infer exp env with
       | Result.Ok t -> Prettyprint.pp_typ Format.std_formatter t
       | Result.Error e -> Prettyprint.pp_error Format.std_formatter e)
    | Command _ ->
      Format.printf "Commands are for the REPL. Test expression inference instead!\n%!"
  in
  match Parser.parse_toplevel test_case with
  | Error e -> Format.printf "%s" e
  | Result.Ok toplevel_input ->
    (match toplevel_input with
     | [ h ] -> test_inner h
     | _ -> Format.printf "Use this function to test single expressions\n%!")
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_factorial_expression;
  [%expect {| int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_factorial_definition;
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_x_power_y_expression;
  [%expect {| int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_x_power_y_definition;
  [%expect {| int -> int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_increment_expression;
  [%expect {| int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_increment_definition;
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_labeled_arguments_expression;
  [%expect {| int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_labeled_arguments_definition;
  [%expect {| ~name1:int -> ~name2:int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_labeled_arguments_sugar_expression;
  [%expect {| int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_labeled_arguments_sugar_definition;
  [%expect {| ~x:int -> ~y:int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_optional_arguments_expression;
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_optional_arguments_definition;
  [%expect {| ?name:int -> int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition
    test_optional_arguments_complex_definition;
  [%expect {| ?x:int -> ?y:int -> 'c -> ?z:int -> 'e -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition
    test_labeled_arguments_swapped_places_expression;
  [%expect {| int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition
    test_labeled_arguments_swapped_places_definition;
  [%expect {| ~name2:int -> ~name1:int -> int |}]
;;

let%expect_test _ =
  test_parse_and_infer_single_expr_or_definition test_mult_arguments_definition;
  [%expect {| (int -> bool) -> int -> bool -> bool |}]
;;

(* ------------------------------------------------------ *)
(* --------------------- Eval tests --------------------- *)
(* ------------------------------------------------------ *)

open Interpret
open Interpret (EvalResult)

let basic = Binop (Plus, Const (Int 4), Const (Int 5))
let int_list = Cons (Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Const Nil)))
let increment = Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1)))

let%test _ =
  let env = IdMap.empty in
  match eval basic env with
  | Ok (VInt 9) -> true
  | _ -> false
;;

let%test _ =
  let env = IdMap.empty in
  match eval int_list env with
  | Ok (VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNil)))) -> true
  | _ -> false
;;

let%test _ =
  let env = IdMap.add "x" (VInt 8) IdMap.empty in
  match eval increment env with
  | Ok
      (VClosure
        (None, env, Fun (ArgNoLabel, None, "x", Binop (Plus, Var "x", Const (Int 1)))))
    when match compare_values (IdMap.find "x" env) (VInt 8) with
         | Result.Ok t when t = 0 -> true
         | _ -> false -> true
  | _ -> false
;;

let test_parse_and_eval_single_expr_ok
  test_case
  ?(env : environment = IdMap.empty)
  expected_value
  =
  let test_inner = function
    | Definition _ ->
      Format.printf
        "Definitions are for the REPL. Test expression evaluation instead!\n%!";
      false
    | Expression exp ->
      let value = eval exp env in
      (match value with
       | Result.Ok v -> compare_values v expected_value = Result.Ok 0
       | Result.Error e ->
         Prettyprint.pp_error Format.std_formatter e;
         false)
    | Command _ ->
      Format.printf "Commands are for the REPL. Test expression evaluation instead!\n%!";
      false
  in
  match Parser.parse_toplevel test_case with
  | Error e ->
    Format.printf "%s" e;
    false
  | Result.Ok toplevel_input ->
    (match toplevel_input with
     | [ h ] -> test_inner h
     | _ ->
       Format.printf "Use this function to test single expressions\n%!";
       false)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_factorial_expression
    (VInt test_factorial_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_x_power_y_expression
    (VInt test_x_power_y_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_increment_expression
    (VInt test_increment_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_labeled_arguments_expression
    (VInt test_labeled_arguments_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_labeled_arguments_sugar_expression
    (VInt test_labeled_arguments_sugar_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_optional_arguments_expression
    (VInt test_optional_arguments_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_optional_arguments_complex_expression
    (VInt test_optional_arguments_complex_expected)
;;

let%test _ =
  test_parse_and_eval_single_expr_ok
    test_labeled_arguments_swapped_places_expression
    (VInt test_labeled_arguments_swapped_places_expected)
;;

(* ------------------------------------------------------ *)
(* ---------------- Prettyprint tests ------------------- *)
(* ------------------------------------------------------ *)

(* Expressions pretty-printing *)
let test_parse_and_prettyprint_single_expr test_case =
  let test_inner = function
    | Definition _ ->
      Format.printf "Definitions are for the REPL. Test expression printing instead!\n%!"
    | Expression exp -> Prettyprint.pp_expr Format.std_formatter exp
    | Command _ ->
      Format.printf "Commands are for the REPL. Test expression printing instead!\n%!"
  in
  match Parser.parse_toplevel test_case with
  | Error e -> Format.printf "%s" e
  | Result.Ok toplevel_input ->
    (match toplevel_input with
     | [ h ] -> test_inner h
     | _ -> Format.printf "Use this function to test single expressions\n%!")
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_factorial_expression;
  [%expect
    {| let rec fact = fun n -> if (n = 0) then 1 else (n * (fact (n - 1))) in (fact 5) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_x_power_y_expression;
  [%expect
    {| let rec pow = fun x -> fun y -> if (y = 0) then 1 else (x * ((pow x) (y - 1))) in ((pow 4) 5) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_increment_expression;
  [%expect {| let inc = fun x -> (x + 1) in (inc 4) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_labeled_arguments_expression;
  [%expect
    {| let f = fun ~name1:x -> fun ~name2:y -> (x / y) in ((f ~name1:4) ~name2:5) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_labeled_arguments_sugar_expression;
  [%expect {| let f = fun ~x:x -> fun ~y:y -> (x + y) in ((f ~x:4) ~y:5) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_optional_arguments_expression;
  [%expect {| let f = fun ?name:(arg1 = 4) -> fun arg2 -> (arg1 + arg2) in (f 5) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_optional_arguments_complex_expression;
  [%expect
    {| let f = fun ?x:(x = 0) -> fun ?y:(y = 0) -> fun  -> fun ?z:(z = 0) -> fun  -> ((x + y) + z) in (((f ~x:5) ()) ()) |}]
;;

let%expect_test _ =
  test_parse_and_prettyprint_single_expr test_labeled_arguments_swapped_places_expression;
  [%expect
    {| let f = fun ~name2:name2 -> fun ~name1:name1 -> (name1 / name2) in ((f ~name2:4) ~name1:5) |}]
;;
