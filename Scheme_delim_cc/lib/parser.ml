(** Copyright 2021-2022, ArtemKhel and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let ( >> ) x c = x >>| fun _ -> c
let ( let* ) = Utils.( let* )

let isDigit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let isChar = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | '+' | '-' | '/' | '*' | '>' | '<' | '=' -> true
  | _ -> false
;;

let spaces = take_while (fun c -> c = ' ' || c = '\n')
let parens p = spaces *> char '(' *> spaces *> p <* spaces <* char ')' <* spaces
let token t = spaces *> string t
let comment = option "" (spaces *> token ";" *> take_till (fun c -> c = '\n') <* spaces)

let number =
  choice [ token "+"; token "-"; token "" ]
  >>= fun sign -> take_while1 isDigit >>| fun num -> int_of_string (sign ^ num)
;;

let isInitial = function
  | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^' ->
    true
  | c -> isChar c
;;

let isSubsequent = function
  | '.' | '+' | '-' -> true
  | c -> isChar c || isDigit c || isInitial c
;;

let id =
  choice [ token "+"; token "-"; token "..." ]
  <|> (spaces *> satisfy isInitial
      >>= fun i ->
      many (satisfy isSubsequent) >>| fun s -> String.of_seq @@ List.to_seq @@ (i :: s))
;;

type dispatch =
  { definition : dispatch -> definition t
  ; expression : dispatch -> expression t
  }

let d =
  let definition d =
    fix
    @@ fun _ ->
    parens
      (both (token "define" *> id) (spaces *> d.expression d)
      >>| fun (id, expr) -> id, expr)
  in
  let expression d =
    fix
    @@ fun expr ->
    let bool_ = token "#t" *> return true <|> token "#f" *> return false in
    let const =
      spaces
      *> choice
           [ (number >>| fun n -> Int n)
           ; (bool_ >>| fun b -> Bool b)
           ; (id >>| fun s -> String s)
           ]
    in
    let datum =
      fix (fun datum ->
        let dlist = parens (sep_by spaces datum) >>| fun x -> DList x in
        let dconst = const >>| fun c -> DConst c in
        let dabbr =
          choice [ token "'" >> PQuote; token "`" >> PBackquote; token "," >> PComma ]
          >>= fun p -> datum >>| fun d -> DAbbr (p, d)
        in
        spaces *> (dconst <|> dlist <|> dabbr))
    in
    let quote = token "'" *> (parens datum <|> datum) >>| fun d -> Quote d in
    let unquote =
      spaces *> token "," *> choice [ (id >>| fun id -> Var id); expr ]
      >>| fun e -> QUnquote e
    in
    let _quasiquote =
      fix (fun _quasiquote ->
        spaces
        *> choice
             [ unquote
             ; (const >>| fun c -> QConst c)
             ; (parens (many _quasiquote) >>| fun q -> QList q)
             ; (datum >>| fun d -> QDatum d)
             ]
        >>| fun q -> q)
    in
    let quasiquote = spaces *> token "`" *> (_quasiquote >>| fun q -> Quasiquote q) in
    let if_ =
      parens
        (both (token "if" *> expr) expr
        >>= fun (condition, then_) ->
        option
          (If (condition, then_, None))
          (expr >>| fun else_ -> If (condition, then_, Some else_)))
    in
    let formals =
      parens (many id) >>| (fun x -> FormalList x) <|> (id >>| fun x -> Formal x)
    in
    let lambda =
      parens
        (token "lambda" *> formals
        >>= fun formals ->
        many (d.definition d)
        >>= fun defs -> many1 expr >>| fun exprs -> Lambda (formals, defs, exprs))
    in
    let func_call =
      parens (both expr (sep_by spaces expr) >>| fun (func, args) -> FuncCall (func, args))
    in
    choice
      [ lambda
      ; if_
      ; func_call
      ; quote
      ; quasiquote
      ; (number >>| fun n -> Const (Int n))
      ; (bool_ >>| fun b -> Const (Bool b))
      ; (id >>| fun var -> Var var)
      ]
  in
  { definition; expression }
;;

let scheme_parser =
  spaces
  *> many
       (d.definition d
       >>| (fun def -> Def def)
       <|> (d.expression d >>| fun exp -> Expr exp)
       <* comment)
;;

let parse str =
  match parse_string ~consume:All scheme_parser str with
  | Ok ast -> Ok ast
  | Error e -> Error (Format.sprintf "Error <%s> while parsing\n%s\n" e str)
;;

let test str =
  match parse str with
  | Ok ast -> Format.printf "%a" pp_program ast
  | Error e -> Format.printf "%s" e
;;

(* 
let%expect_test _ =
  test {| |};
  [%expect {| |}]
;;
*)
let%expect_test _ =
  test {|`(a ,c '(+ 2 x) `(+ 2 x ,y))|};
  [%expect
    {|
    [(Expr
        (Quasiquote
           (QList
              [(QConst (String "a")); (QUnquote (Var "c"));
                (QDatum
                   (DAbbr (PQuote,
                      (DList
                         [(DConst (String "+")); (DConst (Int 2));
                           (DConst (String "x"))])
                      )));
                (QDatum
                   (DAbbr (PBackquote,
                      (DList
                         [(DConst (String "+")); (DConst (Int 2));
                           (DConst (String "x"));
                           (DAbbr (PComma, (DConst (String "y"))))])
                      )))
                ])))
      ] |}]
;;

let%expect_test _ =
  test {|`,a|};
  [%expect {| [(Expr (Quasiquote (QUnquote (Var "a"))))] |}]
;;

let%expect_test _ =
  test
    {|
  (define map (lambda (f list)
    (if (empty? list)
      '()
      (cons (f (car list)) (map f (cdr list))))))

  (define square (lambda (x) (* x x)))

  (display (map square `(1 ,(+ 1 1) ,(* 1 3) ,(square 2) 5)))
  |};
  [%expect
    {|
    [(Def
        ("map",
         (Lambda ((FormalList ["f"; "list"]), [],
            [(If ((FuncCall ((Var "empty?"), [(Var "list")])),
                (Quote (DList [])),
                (Some (FuncCall ((Var "cons"),
                         [(FuncCall ((Var "f"),
                             [(FuncCall ((Var "car"), [(Var "list")]))]));
                           (FuncCall ((Var "map"),
                              [(Var "f");
                                (FuncCall ((Var "cdr"), [(Var "list")]))]
                              ))
                           ]
                         )))
                ))
              ]
            ))));
      (Def
         ("square",
          (Lambda ((FormalList ["x"]), [],
             [(FuncCall ((Var "*"), [(Var "x"); (Var "x")]))]))));
      (Expr
         (FuncCall ((Var "display"),
            [(FuncCall ((Var "map"),
                [(Var "square");
                  (Quasiquote
                     (QList
                        [(QConst (Int 1));
                          (QUnquote
                             (FuncCall ((Var "+"),
                                [(Const (Int 1)); (Const (Int 1))])));
                          (QUnquote
                             (FuncCall ((Var "*"),
                                [(Const (Int 1)); (Const (Int 3))])));
                          (QUnquote
                             (FuncCall ((Var "square"), [(Const (Int 2))])));
                          (QConst (Int 5))]))
                  ]
                ))
              ]
            )))
      ] |}]
;;
