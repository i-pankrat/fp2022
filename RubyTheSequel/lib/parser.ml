(** Copyright 2022-2023, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

(* Helper functions *)

(** Mini-parsers *)

let in_braces l r p = char l *> p <* char r
let round_br p = char '(' *> p <* char ')'
let curly_br p = char '{' *> p <* char '}'
let boxes_br p = char '[' *> p <* char ']'
let vlines_br p = char '|' *> p <* char '|'
let comma = char ','
let dot = char '.'
let ws = char ' '
let spaces = many ws
let nl = char '\n'
let ( |>>= ) a b = return a >>= b
let ( *>| ) a b = a *> return b

(* Trim left spaces *)
let trim_l p = spaces *> p

(* Trim right spaces *)
let trim_r p = p <* spaces

(* Trim left and right spaces *)
let trim p = trim_l (trim_r p)
let trim_nl p = many (ws <|> nl) *> p <* many (ws <|> nl)

let peek_str str =
  peek_string (String.length str)
  >>= function
  | s when String.equal s str -> return s
  | _ -> fail (Format.sprintf "String '%S' not found" str)
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(** Parses int *)
let int_p = take_while1 is_digit >>| int_of_string

(** Parses string literal *)
let string_lit_p = take_while (fun c -> is_letter c || is_digit c || c = '@' || c = '_')

(** Parses boolean literal *)
let bool_p = string "true" <|> string "false" >>| String.equal "true"

(** Parses nil literal *)
let nil_p = string "nil"

let stringlit_p =
  in_braces '\'' '\'' (take_while (fun c -> c != '\''))
  <|> in_braces '"' '"' (take_while (fun c -> c != '"'))
  >>| stringlit
;;

(** Parses variable name *)
let var_p =
  let keywords = [ "end"; "do"; "if"; "elsif"; "while"; "until"; "lambda"; "class" ] in
  peek_char_fail
  >>= fun ch ->
  if is_digit ch
  then fail "Varname cannot start by digit"
  else
    string_lit_p
    >>= function
    | "" -> fail "empty string"
    | str ->
      if List.exists (String.equal str) keywords then fail "found keyword" else return str
;;

(** Parses literal *)
let literal_p =
  choice [ int_p >>| intlit; stringlit_p; bool_p >>| boollit; nil_p *>| nillit ]
;;

(** This type contains expr parser context *)
type skip =
  | SBINOP
  | SSeq
  | NONE
[@@deriving variants, show { with_path = false }]

(** Parses binnop *)
let binop_p expr =
  let chainl1 op e =
    let rec go acc = lift2 (fun f -> f acc) op e >>= go <|> return acc in
    e >>= go
  in
  let priorities =
    [ [ "==", EQ; "!=", NEQ; ">=", GEQ; "<=", LEQ; ">", GTR; "<", LSS ]
    ; [ "||", OR; "+", ADD; "-", SUB ]
    ; [ "&&", AND; "*", MULT; "/", DIV; "%", MOD ]
    ]
  in
  let orders = List.map (List.map (fun (s, t) -> string s *>| binop t)) priorities in
  let assn_p cmp = lift2 assn (var_p <* trim (char '=')) (NONE |>>= cmp) in
  let factor = trim (SBINOP |>>= expr) in
  List.fold_right chainl1 (List.map choice orders) (assn_p expr <|> factor)
;;

(** Conditionals parser *)
let if_p expr seq =
  let then_part =
    both
      (string "if" *> trim (NONE |>>= expr))
      ((string "then" <|> string ";" <|> string "\n") *> trim seq)
  in
  let else_part self =
    let el = string "else" *> trim seq in
    let elsif = peek_str "elsif" *> string "els" *> self in
    option (literal nillit) (el <|> elsif)
  in
  fix (fun self -> lift2 (fun (c, e1) -> ifelse c e1) then_part (else_part self))
  <* trim_l (string "end")
;;

(** Loop parsers (while and until) *)
let loop_p expr seq =
  lift3
    (fun s -> if s = "while" then whilecompound else untilcompound)
    (string "while" <|> string "until")
    (trim_nl (NONE |>>= expr))
    (string "do" *> trim_nl seq <* string "end")
;;

(** Array parser *)
let arr_p expr =
  boxes_br (sep_by (trim_l comma *> many (ws <|> nl)) (NONE |>>= expr)) >>| arr
;;

(** Method and lambda parser *)
let fn_p seq =
  let params in_br = option [] (in_br (sep_by comma (trim var_p))) in
  let lambda_inner = trim (curly_br (lift2 lambdafn (params vlines_br) seq)) in
  let method_inner =
    lift2
      methodfn
      (trim string_lit_p)
      (both (trim (params round_br)) (trim_nl seq <* string "end"))
  in
  string "lambda" *> lambda_inner <|> string "def" *> method_inner
;;

(** Class declaration parser *)
let class_p seq =
  lift2
    classdecl
    (string "class" *> trim var_p)
    (many (trim_nl (fn_p seq)) <* string "end")
;;

(** Main parsing function, parses sequence of expressions *)
let seq_p =
  fix (fun seq ->
    let sep = trim (many1 (char '\n' <|> char ';')) in
    let rec expr ctx =
      let self = NONE |>>= expr in
      (* Skip parser if first argument is equal to context in which expr parser was invoked *)
      let skip_when s p =
        if ctx = s then fail (Format.sprintf "Skip %s" (show_skip s)) else p
      in
      let factor =
        choice
          [ skip_when SBINOP (binop_p expr)
          ; round_br (trim self)
          ; fn_p seq
          ; arr_p expr
          ; if_p expr seq
          ; loop_p expr seq
          ; class_p seq
          ; literal_p >>| literal
          ; var_p >>| var
          ]
      in
      let func_name = take_while1 (fun c -> c != '(' && c != ' ') in
      let params = sep_by comma self in
      let tail =
        both (dot *> trim_l func_name) (round_br params)
        <|> both (return "[]") (boxes_br params)
      in
      lift2 (fun invoker args -> invoke { invoker; args }) factor (round_br params)
      <|> lift2 (List.fold_left methodcall) factor (many tail)
    in
    sep_by1 sep (SSeq |>>= expr)
    >>| fun x -> (if List.length x = 1 then List.hd else seqcompound) x)
;;

let parse = parse_string ~consume:All (trim_nl seq_p)

let test input =
  match parse_string ~consume:All seq_p input with
  | Ok res -> pp_compound Format.std_formatter res
  | Error e -> print_string e
;;

(* Tests added during development, other tests in demos/interpretTests.t *)

let%expect_test _ =
  test {|1|};
  [%expect {|(Literal (IntLit 1)) |}]
;;

let%expect_test _ =
  test {|1+2|};
  [%expect {|(Binop (ADD, (Literal (IntLit 1)), (Literal (IntLit 2))))|}]
;;

let%expect_test _ =
  test {|'1'+'2'|};
  [%expect {|(Binop (ADD, (Literal (StringLit "1")), (Literal (StringLit "2"))))|}]
;;

let%expect_test _ =
  test {|1*2+3|};
  [%expect
    {|
      (Binop (ADD, (Binop (MULT, (Literal (IntLit 1)), (Literal (IntLit 2)))),
         (Literal (IntLit 3))))|}]
;;

let%expect_test _ =
  test {|1+2*3|};
  [%expect
    {|
      (Binop (ADD, (Literal (IntLit 1)),
         (Binop (MULT, (Literal (IntLit 2)), (Literal (IntLit 3))))))|}]
;;

let%expect_test _ =
  test {|x=10|};
  [%expect {|(Assn ("x", (Literal (IntLit 10))))|}]
;;

let%expect_test _ =
  test {|x=y=10|};
  [%expect {|(Assn ("x", (Assn ("y", (Literal (IntLit 10))))))|}]
;;

let%expect_test _ =
  test {|1+2+a=10|};
  [%expect
    {|
    (Binop (ADD, (Binop (ADD, (Literal (IntLit 1)), (Literal (IntLit 2)))),
       (Assn ("a", (Literal (IntLit 10))))))|}]
;;

let%expect_test _ =
  test {|1+(2*3)|};
  [%expect
    {|
      (Binop (ADD, (Literal (IntLit 1)),
         (Binop (MULT, (Literal (IntLit 2)), (Literal (IntLit 3))))))|}]
;;

let%expect_test _ =
  test {|if 1
      2 end|};
  [%expect {|(IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)), (Literal NilLit)))|}]
;;

let%expect_test _ =
  test {|if 1;2 end|};
  [%expect {|(IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)), (Literal NilLit)))|}]
;;

let%expect_test _ =
  test {|if 1 then 2 end|};
  [%expect {|(IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)), (Literal NilLit)))|}]
;;

let%expect_test _ =
  test {|if 1 then 2 else 5 end|};
  [%expect
    {|(IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)), (Literal (IntLit 5))))|}]
;;

let%expect_test _ =
  test {|if 1 then 2 elsif 5 then 4 end|};
  [%expect
    {|
    (IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)),
       (IfElse ((Literal (IntLit 5)), (Literal (IntLit 4)), (Literal NilLit)))))|}]
;;

let%expect_test _ =
  test {|while 1 do a=10 end|};
  [%expect
    {|(WhileCompound ((Literal (IntLit 1)), (Assn ("a", (Literal (IntLit 10))))))|}]
;;

let%expect_test _ =
  test {|until 1 do a=10 end|};
  [%expect
    {|(UntilCompound ((Literal (IntLit 1)), (Assn ("a", (Literal (IntLit 10))))))|}]
;;

let%expect_test _ =
  test {|9;8|};
  [%expect {|(SeqCompound [(Literal (IntLit 9)); (Literal (IntLit 8))])|}]
;;

let%expect_test _ =
  test {|a=[1,2,3]|};
  [%expect
    {|
      (Assn ("a",
         (Arr [(Literal (IntLit 1)); (Literal (IntLit 2)); (Literal (IntLit 3))])))|}]
;;

let%expect_test _ =
  test {|a=[1,2,3]|};
  [%expect
    {|
      (Assn ("a",
         (Arr [(Literal (IntLit 1)); (Literal (IntLit 2)); (Literal (IntLit 3))])))|}]
;;

let%expect_test _ =
  test {|1+2
    2+3|};
  [%expect
    {|
    (SeqCompound
       [(Binop (ADD, (Literal (IntLit 1)), (Literal (IntLit 2))));
         (Binop (ADD, (Literal (IntLit 2)), (Literal (IntLit 3))))])|}]
;;

let%expect_test _ =
  test {|if 1 then 2 else 5 end|};
  [%expect
    {|(IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)), (Literal (IntLit 5))))|}]
;;

let%expect_test _ =
  test {|if 1 then 2 end|};
  [%expect {|(IfElse ((Literal (IntLit 1)), (Literal (IntLit 2)), (Literal NilLit)))|}]
;;

let%expect_test _ =
  test {|if 1 then 2;2 end|};
  [%expect
    {|
    (IfElse ((Literal (IntLit 1)),
       (SeqCompound [(Literal (IntLit 2)); (Literal (IntLit 2))]),
       (Literal NilLit)))|}]
;;

let%expect_test _ =
  test {|if 1 then 2;2 else 2;2 end|};
  [%expect
    {|
    (IfElse ((Literal (IntLit 1)),
       (SeqCompound [(Literal (IntLit 2)); (Literal (IntLit 2))]),
       (SeqCompound [(Literal (IntLit 2)); (Literal (IntLit 2))])))|}]
;;

let%expect_test _ =
  test {|if 1 then 2;2 elsif 5 then x=1;y=x else 2;2 end|};
  [%expect
    {|
    (IfElse ((Literal (IntLit 1)),
       (SeqCompound [(Literal (IntLit 2)); (Literal (IntLit 2))]),
       (IfElse ((Literal (IntLit 5)),
          (SeqCompound
             [(Assn ("x", (Literal (IntLit 1)))); (Assn ("y", (Var "x")))]),
          (SeqCompound [(Literal (IntLit 2)); (Literal (IntLit 2))])))
       ))|}]
;;

let%expect_test _ =
  test {|x=0;while x < 5 do x=x+1 end|};
  [%expect
    {|
    (SeqCompound
       [(Assn ("x", (Literal (IntLit 0))));
         (WhileCompound ((Binop (LSS, (Var "x"), (Literal (IntLit 5)))),
            (Assn ("x", (Binop (ADD, (Var "x"), (Literal (IntLit 1))))))))
         ])|}]
;;

let%expect_test _ =
  test {|x=0;while x < 5 do x=x+1; x=x+5 end|};
  [%expect
    {|
    (SeqCompound
       [(Assn ("x", (Literal (IntLit 0))));
         (WhileCompound ((Binop (LSS, (Var "x"), (Literal (IntLit 5)))),
            (SeqCompound
               [(Assn ("x", (Binop (ADD, (Var "x"), (Literal (IntLit 1))))));
                 (Assn ("x", (Binop (ADD, (Var "x"), (Literal (IntLit 5))))))])
            ))
         ])|}]
;;

let%expect_test _ =
  test {|x=lambda {1+2}|};
  [%expect
    {|
      (Assn ("x",
         (LambdaFn ([], (Binop (ADD, (Literal (IntLit 1)), (Literal (IntLit 2))))))
         ))|}]
;;

let%expect_test _ =
  test {|lambda {|x|1+2}|};
  [%expect
    {|(LambdaFn (["x"], (Binop (ADD, (Literal (IntLit 1)), (Literal (IntLit 2))))))|}]
;;

let%expect_test _ =
  test {|lambda {|x,y,z|x+y}|};
  [%expect {|(LambdaFn (["x"; "y"; "z"], (Binop (ADD, (Var "x"), (Var "y")))))|}]
;;

let%expect_test _ =
  test {|def methodname (var1,var)
    exp1=1
    expr1=2
end|};
  [%expect
    {|
    (MethodFn ("methodname",
       (["var1"; "var"],
        (SeqCompound
           [(Assn ("exp1", (Literal (IntLit 1))));
             (Assn ("expr1", (Literal (IntLit 2))))]))
       ))|}]
;;

let%expect_test _ =
  test {|a.call(b)|};
  [%expect {|(MethodCall ((Var "a"), ("call", [(Var "b")])))|}]
;;

let%expect_test _ =
  test {|a.call()|};
  [%expect {|(MethodCall ((Var "a"), ("call", [])))|}]
;;

let%expect_test _ =
  test {|a.call().call()|};
  [%expect {|(MethodCall ((MethodCall ((Var "a"), ("call", []))), ("call", [])))|}]
;;

let%expect_test _ =
  test {|a.call(c.call(10)).call()|};
  [%expect
    {|
    (MethodCall (
       (MethodCall ((Var "a"),
          ("call", [(MethodCall ((Var "c"), ("call", [(Literal (IntLit 10))])))])
          )),
       ("call", [])))|}]
;;

let%expect_test _ =
  test {|a.[](1,2)|};
  [%expect
    {|
    (MethodCall ((Var "a"), ("[]", [(Literal (IntLit 1)); (Literal (IntLit 2))])
       ))|}]
;;

let%expect_test _ =
  test {|a[1,2]|};
  [%expect
    {|
    (MethodCall ((Var "a"), ("[]", [(Literal (IntLit 1)); (Literal (IntLit 2))])
       ))|}]
;;

let%expect_test _ =
  test {|a[1][3,4].call(2)|};
  [%expect
    {|
    (MethodCall (
       (MethodCall ((MethodCall ((Var "a"), ("[]", [(Literal (IntLit 1))]))),
          ("[]", [(Literal (IntLit 3)); (Literal (IntLit 4))]))),
       ("call", [(Literal (IntLit 2))])))|}]
;;

let%expect_test _ =
  test
    {|class Pair
          def initialize(a,b)
              @b=a
              @a=b
          end
          def get_a()
              @b
          end
          def get_b()
              @a
          end
      end|};
  [%expect
    {|
    (ClassDecl ("Pair",
       [(MethodFn ("initialize",
           (["a"; "b"],
            (SeqCompound [(Assn ("@b", (Var "a"))); (Assn ("@a", (Var "b")))]))
           ));
         (MethodFn ("get_a", ([], (Var "@b"))));
         (MethodFn ("get_b", ([], (Var "@a"))))]
       ))|}]
;;

let%expect_test _ =
  test {|def methodname()
  a=10
end
methodname()|};
  [%expect
    {|
    (SeqCompound
       [(MethodFn ("methodname", ([], (Assn ("a", (Literal (IntLit 10)))))));
         (Invoke { invoker = (Var "methodname"); args = [] })])|}]
;;

let%expect_test _ =
  test
    {|class Pair
  def initialize(a,b)
      @b=a
      @a=b
  end
  def get_a()
      @b
  end
  def get_b()
      @a
  end
end
Pair.new(2,3)|};
  [%expect
    {|
    (SeqCompound
       [(ClassDecl ("Pair",
           [(MethodFn ("initialize",
               (["a"; "b"],
                (SeqCompound [(Assn ("@b", (Var "a"))); (Assn ("@a", (Var "b")))]))
               ));
             (MethodFn ("get_a", ([], (Var "@b"))));
             (MethodFn ("get_b", ([], (Var "@a"))))]
           ));
         (MethodCall ((Var "Pair"),
            ("new", [(Literal (IntLit 2)); (Literal (IntLit 3))])))
         ])|}]
;;
