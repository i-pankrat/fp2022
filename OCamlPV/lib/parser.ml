(** Copyright 2021-2022, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let parse p s = parse_string ~consume:All p s

let is_empty = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lletter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uletter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter ch = is_lletter ch || is_uletter ch

let is_ident_symbol = function
  | c -> is_letter c
;;

let is_kw = function
  | "let" | "match" | "if" | "else" | "than" | "fun" | "with" | "in" | "" -> true
  | _ -> false
;;

let prohibited = function
  | "=" | "match" | "if" | "else" | "than" | "fun" | "with" | "in" | "let" -> true
  | _ -> false
;;

let empty = take_while is_empty
let empty1 = take_while1 is_empty
let token s = empty *> s
let token1 s = empty1 *> s
let trim s = empty *> s <* empty
let pstoken s = empty *> string s
let pstoken1 s = empty1 *> string s
let pparens p = pstoken "(" *> p <* pstoken ")"
let pquotes p = pstoken "\"" *> p <* pstoken "\""
let pbrackets p = pstoken "[" *> p <* pstoken "]"

(* Const constructors *)
let cint x = CInt x
let cbool x = CBool x
let cstring x = CString x
let cnill = CNil

(* Const parsers *)

let psign =
  choice [ pstoken "+" *> return 1; pstoken "-" *> return (-1); pstoken "" *> return 1 ]
;;

let pcint =
  lift2 (fun s v -> cint (s * Int.of_string v)) psign (token @@ take_while1 is_digit)
;;

let pcbool = return Bool.of_string <*> (pstoken "true" <|> pstoken "false") >>| cbool
let pcstring = cstring <$> pquotes @@ take_while (fun x -> x != '"')
let pcnil = pstoken "[]" *> return CNil
let pcunit = pstoken "()" *> return CUnit
let pconst = pcint <|> pcbool <|> pcstring <|> pcnil <|> pcunit

(* Parse ident *)

let pIdent =
  empty *> take_while is_ident_symbol
  >>= fun res -> if is_kw res || prohibited res then fail "keyword" else return res
;;

(* pattern constructors *)
let construct_ptuple t = PTuple t
let construct_pconst c = PConst c
let construct_pcons h t = PCons (h, t)
(* Parse patterns *)

let ppconst = pconst >>| fun x -> PConst x
let ppvar = pIdent >>| fun x -> PVar x
let pptuple t = pparens @@ sep_by (pstoken ",") t
let pplist t = pbrackets @@ sep_by (pstoken ";") t
let pwild = pstoken "_" *> return PWild

(* TEST *)

type pdispatch =
  { cons : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  }

let pack =
  let pattern pack =
    fix @@ fun _ -> choice [ ppvar; ppconst; pwild; pack.cons pack; pack.tuple pack ]
  in
  let tuple pack =
    fix
    @@ fun _ -> construct_ptuple <$> pparens @@ sep_by1 (pstoken ",") (pack.pattern pack)
  in
  let rec create_cons = function
    | [] -> PConst CNil
    | hd :: tl -> construct_pcons hd (create_cons tl)
  in
  let cons pack =
    fix @@ fun _ -> create_cons <$> pbrackets @@ sep_by1 (pstoken ";") (pack.pattern pack)
  in
  { cons; tuple; pattern }
;;

let ppattern =
  fix
  @@ fun _ ->
  let rec create_cons len = function
    | [] -> PConst CNil
    | [ a ] -> if len = 0 then a else construct_pcons a (PConst CNil)
    | hd :: tl -> construct_pcons hd (create_cons (len + 1) tl)
  in
  create_cons 0 <$> sep_by (pstoken "::") (pack.pattern pack)
;;

(* expr constructors *)

let econd i t e = EIfThenElse (i, t, e)
let ematch c pl = EMatch (c, pl)
let epattern p = EPatterns p
let elet name args body = ELet (name, args, body)
let eletrec name args body = ELetRec (name, args, body)
let efun id body = EFun (id, body)
let eapply f a = EApply (f, a)
let evar x = EVar x

(* parse expr *)
let pevar = evar <$> pIdent
let peconst = pconst >>| fun x -> EConst x

(* let econd pexpr =
  let eparse = pexpr in
  lift3
    econd
    (pstoken "if" *> eparse)
    (pstoken "then" *> eparse)
    (pstoken "else" *> eparse)
;; *)

let ematch pexpr =
  let pepatterns = epattern <$> ppattern in
  let pecase = pstoken "|" *> pepatterns in
  let pearrow = pstoken "->" *> pexpr in
  let peline = lift2 (fun c a -> c, a) pecase pearrow in
  let pelines = many1 peline in
  let pematch = pstoken "match" *> pexpr <* pstoken1 "with" in
  lift2 ematch pematch pelines
;;

let eletfun pexpr =
  lift4
    (fun flag args name body ->
      if flag then eletrec args name body else elet args name body)
    (pstoken "let" *> option "false" (pstoken1 "rec")
    >>| fun x ->
    match x with
    | "false" -> false
    | _ -> true)
    pIdent
    pevar
    (pstoken "=" *> pexpr)
;;

let pefun pexpr =
  lift2 (fun a e -> efun a e) (pstoken "fun" *> pevar) (pstoken "->" *> pexpr)
;;

let eletdecl pexpr =
  lift3
    (fun args name body -> elet args name body)
    (pstoken "let" *> pIdent)
    (pstoken1 "=" *> pexpr)
    (pstoken1 "in" *> pexpr)
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let from_str_to_binop = function
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Mult
  | "\\" -> Divide
  | "&&" -> And
  | "||" -> Or
  | "=" -> Eq
  | "!=" -> Neq
  | ">" -> Gt
  | "<" -> Lt
  | ">=" -> Gtq
  | "<=" -> Ltq
  | "::" -> ConsConcat
  | _ -> failwith "Should not reach"
;;

let cebinop op e1 e2 = EBinOp (op, e1, e2)

let pbinop chain1 term binops =
  chain1
    term
    ((fun op expr1 expr2 -> cebinop (from_str_to_binop op) expr1 expr2)
    <$> choice (List.map binops ~f:pstoken))
;;

let pelbinop = pbinop chainl1
let perbinop = pbinop chainr1
let pevalue = peconst <|> pevar

let peapply pexpr =
  let args = choice [ pexpr ] in
  lift2 (fun expr l -> List.fold_left ~f:eapply ~init:expr l) args (many (token1 @@ args))
;;

let pinfixop pexpr =
  let term = choice [ pparens pexpr; pevalue ] in
  let term = peapply term in
  let term = pelbinop term [ "*"; "/" ] in
  let term = pelbinop term [ "+"; "-" ] in
  let term = perbinop term [ "::" ] in
  let term = pelbinop term [ "!="; "="; "<="; "<"; ">="; ">" ] in
  let term = perbinop term [ "&&" ] in
  let term = perbinop term [ "||" ] in
  term
;;

let pexpr =
  fix
  @@ fun pexpr ->
  (* let pcond = econd pexpr in *)
  let pematch = ematch pexpr in
  let pelets = eletdecl pexpr <|> eletfun pexpr in
  let pefun = pefun pexpr in
  let pinfixop = pinfixop pexpr in
  choice [ pelets; pefun; pematch; pinfixop; pevalue ]
;;

(* let eapply pexpr = lift2 eapply pexpr (token1 pexpr) *)

(* PARSER TESTS*)
let interprete_parse_result fm p str =
  match parse p str with
  | Result.Error e -> e
  | Result.Ok ast -> fm ast
;;

(* Parse const tests *)
let%expect_test _ =
  print_string (interprete_parse_result show_const pconst "42");
  [%expect {| (CInt 42) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_const pconst " -42");
  [%expect {| (CInt -42) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_const pconst "false");
  [%expect {| (CBool false) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_const pconst "true");
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_const pconst "\"Ocaml is cool!\"");
  [%expect {| (CString "Ocaml is cool!") |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_const pconst "[]");
  [%expect {| CNil |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_const pconst " ()");
  [%expect {| CUnit |}]
;;

(* Parse patterns tests *)

let%expect_test _ =
  print_string (interprete_parse_result show_pattern ppconst "4");
  [%expect {| (PConst (CInt 4)) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_pattern ppattern "a");
  [%expect {| (PVar "a") |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_pattern ppattern "a :: b :: c");
  [%expect
    {|
    (PCons ((PVar "a"), (PCons ((PVar "b"), (PCons ((PVar "c"), (PConst CNil)))))
       )) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_pattern ppattern "(a, b) :: (c, d)");
  [%expect
    {|
    (PCons ((PTuple [(PVar "a"); (PVar "b")]),
       (PCons ((PTuple [(PVar "c"); (PVar "d")]), (PConst CNil))))) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_pattern ppattern "[a; b; c; d]");
  [%expect
    {|
    (PCons ((PVar "a"),
       (PCons ((PVar "b"),
          (PCons ((PVar "c"), (PCons ((PVar "d"), (PConst CNil)))))))
       )) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_pattern ppattern "((a, b), (c, d))");
  [%expect
    {|
    (PTuple
       [(PTuple [(PVar "a"); (PVar "b")]); (PTuple [(PVar "c"); (PVar "d")])]) |}]
;;

(* EXPR TESTS *)

let%expect_test _ =
  print_string
    (interprete_parse_result
       show_expr
       pexpr
       "match x with | hd :: tl -> true | [] -> false");
  [%expect
    {|
         (EMatch ((EVar "x"),
            [((EPatterns (PCons ((PVar "hd"), (PCons ((PVar "tl"), (PConst CNil)))))),
              (EConst (CBool true)));
              ((EPatterns (PConst CNil)), (EConst (CBool false)))]
            ))
 |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "fun x -> x");
  [%expect {| (EFun ((EVar "x"), (EVar "x"))) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "fun x -> fun y -> x");
  [%expect {| (EFun ((EVar "x"), (EFun ((EVar "y"), (EVar "x"))))) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "fun x -> fun y -> 4");
  [%expect {| (EFun ((EVar "x"), (EFun ((EVar "y"), (EConst (CInt 4)))))) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "let snd x = fun y -> y");
  [%expect {| (ELet ("snd", (EVar "x"), (EFun ((EVar "y"), (EVar "y"))))) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "let x = a in z");
  [%expect {| (ELet ("x", (EVar "a"), (EVar "z")))|}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "a + b");
  [%expect {|
        (EBinOp (Plus, (EVar "a"), (EVar "b"))) |}]
;;

let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "let sum x = fun y -> x + y");
  [%expect
    {|
    (ELet ("sum", (EVar "x"),
       (EFun ((EVar "y"), (EBinOp (Plus, (EVar "x"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  print_string
    (interprete_parse_result show_expr pexpr "let apply f = fun x -> fun z -> f x z");
  [%expect
    {|
    (ELet ("apply", (EVar "f"),
       (EFun ((EVar "x"),
          (EFun ((EVar "z"),
             (EApply ((EApply ((EVar "f"), (EVar "x"))), (EVar "z")))))
          ))
       )) |}]
;;

(* FIX THIS CASE *)
let%expect_test _ =
  print_string (interprete_parse_result show_expr pexpr "let deinc x = x - (1)");
  [%expect
    {| (ELet ("deinc", (EVar "x"), (EBinOp (Minus, (EVar "x"), (EConst (CInt 1)))))) |}]
;;

let%expect_test _ =
  print_string
    (interprete_parse_result
       show_expr
       pexpr
       "let rec fib n = fun acc -> match n with | 1 -> acc | _ -> fib (n - (1)) (acc * n)");
  [%expect
    {|
    (ELetRec ("fib", (EVar "n"),
       (EFun ((EVar "acc"),
          (EMatch ((EVar "n"),
             [((EPatterns (PConst (CInt 1))), (EVar "acc"));
               ((EPatterns PWild),
                (EApply (
                   (EApply ((EVar "fib"),
                      (EBinOp (Minus, (EVar "n"), (EConst (CInt 1)))))),
                   (EBinOp (Mult, (EVar "acc"), (EVar "n"))))))
               ]
             ))
          ))
       ))|}]
;;

let%expect_test _ =
  print_string
    (interprete_parse_result
       show_expr
       pexpr
       "let rec map f = fun list -> match list with | [] -> [] | h :: t -> f h :: map f t");
  [%expect
    {|
    (ELetRec ("map", (EVar "f"),
       (EFun ((EVar "list"),
          (EMatch ((EVar "list"),
             [((EPatterns (PConst CNil)), (EConst CNil));
               ((EPatterns
                   (PCons ((PVar "h"), (PCons ((PVar "t"), (PConst CNil)))))),
                (EBinOp (ConsConcat, (EApply ((EVar "f"), (EVar "h"))),
                   (EApply ((EApply ((EVar "map"), (EVar "f"))), (EVar "t"))))))
               ]
             ))
          ))
       ))|}]
;;
