(** Copyright 2022-2023, Ilya Pankratov *)

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
  | "let" | "match" | "if" | "else" | "than" | "fun" | "with" | "in" -> true
  | _ -> false
;;

let prohibited = function
  | "=" | "" | "+" | "-" | "(" | ")" | "[" | "]" | "#" | "%" | "^" | "&" -> true
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
let pwild = pstoken "_" *> return PWild

let rec create_cons = function
  | [] -> PConst CNil
  | hd :: [] when equal_pattern hd (PConst CNil) -> PConst CNil
  | hd :: tl -> construct_pcons hd (create_cons tl)
;;

let parse_cons_semicolon parser =
  create_cons <$> pbrackets @@ sep_by1 (pstoken ";") parser
;;

let parse_cons_doublecolon parser =
  lift2
    (fun a b -> create_cons @@ (a :: b))
    (parser <* trim @@ pstoken "::")
    (sep_by1 (trim @@ pstoken "::") parser)
;;

let parse_tuple_parens parser =
  construct_ptuple <$> pparens @@ sep_by1 (pstoken ",") parser
;;

let parse_tuple parser =
  lift2
    (fun a b -> construct_ptuple @@ (a :: b))
    (parser <* trim @@ pstoken ",")
    (sep_by1 (trim @@ pstoken ",") parser)
;;

type pdispatch =
  { cons_sc : pdispatch -> pattern t
  ; cons_dc : pdispatch -> pattern t
  ; tuple_p : pdispatch -> pattern t
  ; tuple_wp : pdispatch -> pattern t
  ; value : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  }

let pack =
  let pattern pack =
    choice
      ~failure_msg:"There are no appropriate parser"
      [ pack.tuple_p pack
      ; pack.tuple_wp pack
      ; pack.cons_sc pack
      ; pack.cons_dc pack
      ; pack.value pack
      ]
  in
  let parsers pack =
    choice
      ~failure_msg:"Can not parse internal list/tuple"
      [ pack.value pack; pack.tuple_p pack; pack.cons_sc pack ]
  in
  let value _ = ppvar <|> ppconst <|> pwild in
  let tuple_p pack = fix @@ fun _ -> parse_tuple_parens (parsers pack) in
  let tuple_wp pack = fix @@ fun _ -> parse_tuple (parsers pack) in
  let cons_sc pack = fix @@ fun _ -> parse_cons_semicolon (parsers pack) in
  let cons_dc pack = fix @@ fun _ -> parse_cons_doublecolon (parsers pack) in
  { cons_sc; cons_dc; tuple_p; tuple_wp; value; pattern }
;;

let ppattern = pack.pattern pack

(* expr constructors *)
let econd i t e = EIfThenElse (i, t, e)
let ematch c pl = EMatch (c, pl)
let elet name body = ELet (name, body)
let eletin name body bodyin = ELetIn (name, body, bodyin)
let eletrec name body = ELetRec (name, body)
let eletrecin name body bodyin = ELetRecIn (name, body, bodyin)
let efun id body = EFun (id, body)
let eapply f a = EApply (f, a)
let evar x = EVar x
let ebinop op = EBinOp op

(* parse expr *)
let pevar = evar <$> pIdent
let peconst = pconst >>| fun x -> EConst x

type edispatch =
  { evalue : edispatch -> expr t
  ; econdition : edispatch -> expr t
  ; elet : edispatch -> expr t
  ; eletin : edispatch -> expr t
  ; eletrec : edispatch -> expr t
  ; eletrecin : edispatch -> expr t
  ; ematch : edispatch -> expr t
  ; ebinop : edispatch -> expr t
  ; efun : edispatch -> expr t
  ; eapply : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let econd pif pexpr =
  lift3 econd (pstoken "if" *> pif) (pstoken1 "then" *> pexpr) (pstoken1 "else" *> pexpr)
;;

let ematch pmatch pexpr =
  let pecase = pstoken "|" *> ppattern in
  let pearrow = pstoken "->" *> pexpr in
  let peline = lift2 (fun c a -> c, a) pecase pearrow in
  let pelines = many1 peline in
  let pematch = pstoken "match" *> pmatch <* pstoken1 "with" in
  lift2 ematch pematch pelines
;;

let parens_or_not p = p <|> pparens p
let pargs = many1 @@ parens_or_not @@ ppattern

(* Upgrade using fold_left *)
let construct_efun args body =
  let rec helper = function
    | [] -> body
    | hd :: tl -> efun hd (helper tl)
  in
  helper args
;;

let pefun pexpr =
  lift2
    (fun args expr -> construct_efun args expr)
    (pstoken "fun" *> pargs)
    (pstoken "->" *> pexpr)
;;

let parse_rec_or_not =
  pstoken "let" *> option "false" (pstoken1 "rec") >>| fun x -> x != "false"
;;

let eletfun pexpr =
  lift4
    (fun flag name args body ->
      let body = construct_efun args body in
      if flag then eletrec name body else elet name body)
    parse_rec_or_not
    pIdent
    pargs
    (pstoken "=" *> pexpr)
;;

let eletdecl pexpr =
  let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
  lift5
    (fun flag name args body1 body2 ->
      let body1 = construct_efun args body1 in
      if flag then eletrecin name body1 body2 else eletin name body1 body2)
    parse_rec_or_not
    pIdent
    pargs
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
  | "/" -> Divide
  | "&&" -> And
  | "||" -> Or
  | "=" -> Eq
  | "!=" -> Neq
  | ">" -> Gt
  | "<" -> Lt
  | ">=" -> Gtq
  | "<=" -> Ltq
  | "::" -> ConsConcat
  | _ -> failwith "Such operator does not exist"
;;

let pbinop chain1 term binops =
  chain1
    term
    ((fun op expr1 expr2 -> eapply (eapply (ebinop @@ from_str_to_binop op) expr1) expr2)
    <$> choice ~failure_msg:"Can not parse binary operation" (List.map binops ~f:pstoken)
    )
;;

let pelbinop = pbinop chainl1
let perbinop = pbinop chainr1
let pevalue = pevar <|> peconst

let peapply pexpr =
  lift2
    (fun expr l -> List.fold_left ~f:eapply ~init:expr l)
    pexpr
    (many (token1 @@ pexpr))
;;

let pbinop pexpr =
  let term = pexpr in
  let term = pelbinop term [ "*"; "/" ] in
  let term = pelbinop term [ "+"; "-" ] in
  let term = perbinop term [ "::" ] in
  let term = pelbinop term [ "!="; "="; "<="; "<"; ">="; ">" ] in
  let term = perbinop term [ "&&" ] in
  let term = perbinop term [ "||" ] in
  term
;;

let pack =
  let evalue _ = pevalue in
  let lets pack =
    choice ~failure_msg:"Failed to parse lets" [ pack.elet pack; pack.eletrec pack ]
  in
  let letsin pack =
    choice ~failure_msg:"Failed to parse letsin" [ pack.eletin pack; pack.eletrecin pack ]
  in
  let expr pack =
    choice
      ~failure_msg:"Failed to parse expr"
      (* We have to a mistake with simaltenious parsing of application and binary operations.
         Now we have to wrap binary operation in braces to parse it corretly :( *)
      [ letsin pack
      ; lets pack
      ; pack.econdition pack
      ; pack.ebinop pack
      ; pack.eapply pack
      ; pack.efun pack
      ; pack.ematch pack (* ; letsin pack *)
      ]
  in
  let econdition pack =
    fix
    @@ fun _ ->
    let econd_parser =
      parens_or_not
      @@ choice
           ~failure_msg:"Failed to parse condition statement"
           [ pack.econdition pack
           ; pack.ebinop pack
           ; pack.eapply pack
           ; pack.efun pack
           ; pack.ematch pack
           ]
    in
    econd econd_parser (pack.expr pack)
  in
  let ematch pack =
    fix
    @@ fun _ ->
    let ematch_parse =
      parens_or_not
      @@ choice
           ~failure_msg:"Failed to parse matching expression"
           [ pack.econdition pack; pack.ematch pack; pack.eapply pack; pack.ebinop pack ]
    in
    parens_or_not @@ ematch ematch_parse (pack.expr pack)
  in
  let ebinop pack =
    fix
    @@ fun _ ->
    let ebinop_parse =
      choice
        ~failure_msg:"Failed to parse binary operation"
        [ pevalue
        ; pparens @@ pack.econdition pack
        ; pparens @@ pack.ematch pack
        ; pparens @@ pack.eapply pack
        ; pparens @@ pack.ebinop pack
        ]
    in
    parens_or_not @@ pbinop ebinop_parse
  in
  let efun pack = parens_or_not @@ fix @@ fun _ -> pefun @@ pack.expr pack in
  let eapply pack =
    fix
    @@ fun _ ->
    let eapply_parse =
      parens_or_not
      @@ choice
           ~failure_msg:"Failed to parse application"
           [ pack.evalue pack
           ; pparens @@ pack.econdition pack
           ; pparens @@ pack.ematch pack
           ; pparens @@ pack.eapply pack
           ; pparens @@ pack.ebinop pack
           ]
    in
    peapply eapply_parse
  in
  let lets_parsers pack =
    choice
      ~failure_msg:"Failed to parse lets"
      [ pack.ebinop pack
      ; pack.efun pack
      ; pack.econdition pack
      ; pack.ematch pack
      ; pack.eapply pack
      ; letsin pack
      ]
  in
  let elet pack = fix @@ fun _ -> eletfun @@ lets_parsers pack in
  let eletin pack = fix @@ fun _ -> eletdecl @@ lets_parsers pack in
  let eletrec pack = fix @@ fun _ -> eletfun @@ lets_parsers pack in
  let eletrecin pack = fix @@ fun _ -> eletdecl @@ lets_parsers pack in
  { evalue
  ; econdition
  ; elet
  ; eletin
  ; eletrec
  ; eletrecin
  ; ematch
  ; ebinop
  ; efun
  ; eapply
  ; expr
  }
;;

let pexpr = pack.expr pack

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
