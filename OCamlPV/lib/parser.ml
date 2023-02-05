(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

type error = string

let pp_error ppf error = Format.fprintf ppf "%s" error
let parse_str p s = parse_string ~consume:All p s

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

let is_fst_symbol ch = is_lletter ch || Poly.( = ) ch '_'

let is_ident_symbol = function
  | c -> is_fst_symbol c || is_uletter c || is_digit c
;;

let is_kw = function
  | "let" | "match" | "if" | "else" | "than" | "fun" | "with" | "in" -> true
  | _ -> false
;;

let is_prohibited = function
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

(** Const constructors *)

let cint x = CInt x
let cbool x = CBool x
let cstring x = CString x
let cnill = CNil

(** Const parsers *)

let psign = choice [ pparens @@ (pstoken "-" *> return (-1)); pstoken "" *> return 1 ]

let pcint =
  lift2 (fun s v -> cint (s * Int.of_string v)) psign (token @@ take_while1 is_digit)
;;

let pcbool = return Bool.of_string <*> (pstoken "true" <|> pstoken "false") >>| cbool
let pcstring = cstring <$> pquotes @@ take_while (fun x -> x != '"')
let pcnil = pstoken "[]" *> return CNil
let pcunit = pstoken "()" *> return CUnit
let pconst = pcint <|> pcbool <|> pcstring <|> pcnil <|> pcunit

(** Parse ident *)

let pfstsymbol = empty *> satisfy is_fst_symbol
let pIdent = take_while is_ident_symbol

let pIdent =
  lift2 (fun f ident -> Format.sprintf "%c%s" f ident) pfstsymbol pIdent
  >>= fun res -> if is_kw res || is_prohibited res then fail "keyword" else return res
;;

(** Pattern constructors *)

let construct_ptuple t = PTuple t
let construct_pconst c = PConst c
let construct_pcons h t = PCons (h, t)

(** Parse patterns *)

let ppconst = pconst >>| fun x -> PConst x
let ppvar = pIdent >>| fun x -> PVar x
let pwild = pstoken "_" *> return PWild

let rec create_cons_dc = function
  | [] -> failwith "todo"
  | hd :: [] when equal_pattern hd (PConst CNil) -> PConst CNil
  | [ f; s ] -> construct_pcons f s
  | hd :: tl -> construct_pcons hd (create_cons_dc tl)
;;

let rec create_cons_sc = function
  | [] -> PConst CNil
  | hd :: [] when equal_pattern hd (PConst CNil) -> PConst CNil
  | hd :: tl -> construct_pcons hd (create_cons_sc tl)
;;

let parse_cons_semicolon parser constructor =
  constructor <$> pbrackets @@ sep_by1 (pstoken ";") parser
;;

let parse_cons_doublecolon parser constructor =
  lift2
    (fun a b -> constructor @@ (a :: b))
    (parser <* trim @@ pstoken "::")
    (sep_by1 (trim @@ pstoken "::") parser)
;;

let parse_tuple_parens parser constructor =
  constructor <$> pparens @@ sep_by1 (pstoken ",") parser
;;

let parse_tuple parser constructor =
  lift2
    (fun a b -> constructor @@ (a :: b))
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
  let value _ = pwild <|> ppvar <|> ppconst in
  let tuple_p pack = fix @@ fun _ -> parse_tuple_parens (parsers pack) construct_ptuple in
  let tuple_wp pack = fix @@ fun _ -> parse_tuple (parsers pack) construct_ptuple in
  let cons_sc pack = fix @@ fun _ -> parse_cons_semicolon (parsers pack) create_cons_sc in
  let cons_dc pack =
    fix @@ fun _ -> parse_cons_doublecolon (parsers pack) create_cons_dc
  in
  { cons_sc; cons_dc; tuple_p; tuple_wp; value; pattern }
;;

let ppattern = pack.pattern pack

(** Expr constructors *)

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
let elist h t = EList (h, t)
let etuple t = ETuple t

(** Parse expr *)

let pevar = evar <$> pIdent
let peconst = pconst >>| fun x -> EConst x
let parens_or_not p = p <|> pparens p
let pargs = many @@ parens_or_not @@ ppattern
let pargs1 = many @@ parens_or_not @@ ppattern

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
  ; elist : edispatch -> expr t
  ; etuple : edispatch -> expr t
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
    (pstoken "fun" *> pargs1)
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
    <* empty
  in
  let letsin pack =
    choice ~failure_msg:"Failed to parse letsin" [ pack.eletin pack; pack.eletrecin pack ]
    <* empty
  in
  let expr pack =
    choice
      ~failure_msg:"Failed to parse expr"
      [ letsin pack
      ; lets pack
      ; pack.econdition pack
      ; pack.ebinop pack
      ; pack.eapply pack
      ; pack.efun pack
      ; pack.ematch pack
      ; pack.elist pack
      ; pack.etuple pack
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
           ; pack.elist pack
           ; pack.etuple pack
           ; pparens @@ pack.efun pack
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
  let value_parsers pack =
    choice
      ~failure_msg:"Failed to parse list"
      [ pack.evalue pack; pack.etuple pack; pack.elist pack ]
  in
  let elist pack =
    fix
    @@ fun _ ->
    let rec create_cons_sc = function
      | [] -> EConst CNil
      | hd :: [] when equal_expr hd (EConst CNil) -> EConst CNil
      | hd :: tl -> elist hd (create_cons_sc tl)
    in
    parse_cons_semicolon (value_parsers pack) create_cons_sc
  in
  let etuple pack = fix @@ fun _ -> parse_tuple_parens (value_parsers pack) etuple in
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
  ; elist
  ; etuple
  ; expr
  }
;;

let pexpr = pack.expr pack
let pstatements = sep_by1 (pstoken ";;") pexpr

(** Parser tests *)
let parse program = parse_str pstatements program

let interprete_parse_result f p str =
  match parse_str p str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s" (f ast)
;;

(** Parse const tests *)

let%expect_test _ =
  interprete_parse_result show_const pconst "42";
  [%expect {| (CInt 42) |}]
;;

let%expect_test _ =
  interprete_parse_result show_const pconst "false";
  [%expect {| (CBool false) |}]
;;

let%expect_test _ =
  interprete_parse_result show_const pconst "true";
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  interprete_parse_result show_const pconst "\"Ocaml is cool!\"";
  [%expect {| (CString "Ocaml is cool!") |}]
;;

let%expect_test _ =
  interprete_parse_result show_const pconst "[]";
  [%expect {| CNil |}]
;;

let%expect_test _ =
  interprete_parse_result show_const pconst " ()";
  [%expect {| CUnit |}]
;;

(** Parse patterns tests *)

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "list_map";
  [%expect {| (PVar "list_map") |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "4";
  [%expect {| (PConst (CInt 4)) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "a";
  [%expect {| (PVar "a") |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "[]";
  [%expect {| (PConst CNil) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "[a; b; c; d]";
  [%expect
    {|
    (PCons ((PVar "a"),
       (PCons ((PVar "b"),
          (PCons ((PVar "c"), (PCons ((PVar "d"), (PConst CNil)))))))
       )) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "a :: b :: c :: d :: []";
  [%expect
    {|
    (PCons ((PVar "a"),
       (PCons ((PVar "b"),
          (PCons ((PVar "c"), (PCons ((PVar "d"), (PConst CNil)))))))
       )) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "h :: t";
  [%expect {| (PCons ((PVar "h"), (PVar "t"))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "(a, b, c, d)";
  [%expect {|
    (PTuple [(PVar "a"); (PVar "b"); (PVar "c"); (PVar "d")])|}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "a, b, c, d";
  [%expect {|
    (PTuple [(PVar "a"); (PVar "b"); (PVar "c"); (PVar "d")])|}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "_";
  [%expect {|
    PWild|}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "[(a, b); (c, d)]";
  [%expect
    {|
    (PCons ((PTuple [(PVar "a"); (PVar "b")]),
       (PCons ((PTuple [(PVar "c"); (PVar "d")]), (PConst CNil))))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "((a, b), (c, d))";
  [%expect
    {|
    (PTuple
       [(PTuple [(PVar "a"); (PVar "b")]); (PTuple [(PVar "c"); (PVar "d")])]) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "([a; b], [c; d], [e; f])";
  [%expect
    {|
    (PTuple
       [(PCons ((PVar "a"), (PCons ((PVar "b"), (PConst CNil)))));
         (PCons ((PVar "c"), (PCons ((PVar "d"), (PConst CNil)))));
         (PCons ((PVar "e"), (PCons ((PVar "f"), (PConst CNil)))))]) |}]
;;

let%expect_test _ =
  interprete_parse_result show_pattern ppattern "[[a; b]; [c; d]]";
  [%expect
    {|
    (PCons ((PCons ((PVar "a"), (PCons ((PVar "b"), (PConst CNil))))),
       (PCons ((PCons ((PVar "c"), (PCons ((PVar "d"), (PConst CNil))))),
          (PConst CNil)))
       )) |}]
;;

(** Expression tests *)

(** Test binary operations *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "a + b";
  [%expect {|
        (EApply ((EApply ((EBinOp Plus), (EVar "a"))), (EVar "b"))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "(a + b)";
  [%expect {|
        (EApply ((EApply ((EBinOp Plus), (EVar "a"))), (EVar "b"))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "((a + b))";
  [%expect {|
        (EApply ((EApply ((EBinOp Plus), (EVar "a"))), (EVar "b"))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "a - 1";
  [%expect
    {|
        (EApply ((EApply ((EBinOp Minus), (EVar "a"))), (EConst (CInt 1))))|}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "(a - 1)";
  [%expect
    {|
        (EApply ((EApply ((EBinOp Minus), (EVar "a"))), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "(a + b) * (c - d) * (e / d)";
  [%expect
    {|
        (EApply (
           (EApply ((EBinOp Mult),
              (EApply (
                 (EApply ((EBinOp Mult),
                    (EApply ((EApply ((EBinOp Plus), (EVar "a"))), (EVar "b"))))),
                 (EApply ((EApply ((EBinOp Minus), (EVar "c"))), (EVar "d")))))
              )),
           (EApply ((EApply ((EBinOp Divide), (EVar "e"))), (EVar "d"))))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "1 :: list";
  [%expect
    {|
        (EApply ((EApply ((EBinOp ConsConcat), (EConst (CInt 1)))), (EVar "list"))) |}]
;;

(** Test application *)

let%expect_test _ =
  interprete_parse_result show_expr (pack.eapply pack) "f x y";
  [%expect {|
        (EApply ((EApply ((EVar "f"), (EVar "x"))), (EVar "y"))) |}]
;;

(** Test condition statement *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "if a then b else c";
  [%expect {|
        (EIfThenElse ((EVar "a"), (EVar "b"), (EVar "c"))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "if n = 1 then 1 else n";
  [%expect
    {|
        (EIfThenElse (
           (EApply ((EApply ((EBinOp Eq), (EVar "n"))), (EConst (CInt 1)))),
           (EConst (CInt 1)), (EVar "n"))) |}]
;;

(** Test pattern matching *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "match x with | a -> b | _ -> c";
  [%expect
    {|
        (EMatch ((EVar "x"), [((PVar "a"), (EVar "b")); (PWild, (EVar "c"))])) |}]
;;

(** Test fun *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "fun x -> fun y -> x";
  [%expect {| (EFun ((PVar "x"), (EFun ((PVar "y"), (EVar "x"))))) |}]
;;

(** Test list *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "[a; b; c; d]";
  [%expect
    {|
    (EList ((EVar "a"),
       (EList ((EVar "b"),
          (EList ((EVar "c"), (EList ((EVar "d"), (EConst CNil)))))))
       )) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "[[1]; [2]; [3]; [4]]";
  [%expect
    {|
    (EList ((EList ((EConst (CInt 1)), (EConst CNil))),
       (EList ((EList ((EConst (CInt 2)), (EConst CNil))),
          (EList ((EList ((EConst (CInt 3)), (EConst CNil))),
             (EList ((EList ((EConst (CInt 4)), (EConst CNil))), (EConst CNil)))
             ))
          ))
       )) |}]
;;

(** Test tuple *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "((1, 2), (3, 4))";
  [%expect
    {|
    (ETuple
       [(ETuple [(EConst (CInt 1)); (EConst (CInt 2))]);
         (ETuple [(EConst (CInt 3)); (EConst (CInt 4))])]) |}]
;;

(** Test let and let rec *)

let%expect_test _ =
  interprete_parse_result show_expr pexpr "let id x = x";
  [%expect {| (ELet ("id", (EFun ((PVar "x"), (EVar "x"))))) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "let sum x = fun y -> x + y";
  [%expect
    {|
    (ELet ("sum",
       (EFun ((PVar "x"),
          (EFun ((PVar "y"),
             (EApply ((EApply ((EBinOp Plus), (EVar "x"))), (EVar "y")))))
          ))
       )) |}]
;;

let%expect_test _ =
  interprete_parse_result show_expr pexpr "let sum x y = x + y";
  [%expect
    {|
    (ELet ("sum",
       (EFun ((PVar "x"),
          (EFun ((PVar "y"),
             (EApply ((EApply ((EBinOp Plus), (EVar "x"))), (EVar "y")))))
          ))
       )) |}]
;;

let%expect_test _ =
  interprete_parse_result
    show_expr
    pexpr
    "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))";
  [%expect
    {|
    (ELetRec ("fact",
       (EFun ((PVar "n"),
          (EIfThenElse (
             (EApply ((EApply ((EBinOp Eq), (EVar "n"))), (EConst (CInt 1)))),
             (EConst (CInt 1)),
             (EApply ((EApply ((EBinOp Mult), (EVar "n"))),
                (EApply ((EVar "fact"),
                   (EApply ((EApply ((EBinOp Minus), (EVar "n"))),
                      (EConst (CInt 1))))
                   ))
                ))
             ))
          ))
       )) |}]
;;

(* TODO: fix braces at application *)
let%expect_test _ =
  interprete_parse_result
    show_expr
    pexpr
    "let rec sumn n = if n = 1 then 1 else n + (sumn (n - 1))";
  [%expect
    {|
    (ELetRec ("sumn",
       (EFun ((PVar "n"),
          (EIfThenElse (
             (EApply ((EApply ((EBinOp Eq), (EVar "n"))), (EConst (CInt 1)))),
             (EConst (CInt 1)),
             (EApply ((EApply ((EBinOp Plus), (EVar "n"))),
                (EApply ((EVar "sumn"),
                   (EApply ((EApply ((EBinOp Minus), (EVar "n"))),
                      (EConst (CInt 1))))
                   ))
                ))
             ))
          ))
       ))|}]
;;

(* TODO: fix braces at application *)
let%expect_test _ =
  interprete_parse_result
    show_expr
    pexpr
    "let rec fib n =\n\
    \  match n with \n\
    \    | 1 -> 1 \n\
    \    | _ -> (fib (n - 1)) + (fib (n - 2))\n";
  [%expect
    {|
    (ELetRec ("fib",
       (EFun ((PVar "n"),
          (EMatch ((EVar "n"),
             [((PConst (CInt 1)), (EConst (CInt 1)));
               (PWild,
                (EApply (
                   (EApply ((EBinOp Plus),
                      (EApply ((EVar "fib"),
                         (EApply ((EApply ((EBinOp Minus), (EVar "n"))),
                            (EConst (CInt 1))))
                         ))
                      )),
                   (EApply ((EVar "fib"),
                      (EApply ((EApply ((EBinOp Minus), (EVar "n"))),
                         (EConst (CInt 2))))
                      ))
                   )))
               ]
             ))
          ))
       )) |}]
;;

(** Test let in and let rec in *)

(* TODO: fix braces at application *)
let%expect_test _ =
  interprete_parse_result
    show_expr
    pexpr
    "let list_rev list = \n\
    \ let rec helper acc l = \n\
    \   match l with \n\
    \    | [] -> acc \n\
    \    | hd :: tl -> (helper (hd :: acc) tl) in \n\
    \ (helper [] list) \n";
  [%expect
    {|
    (ELet ("list_rev",
       (EFun ((PVar "list"),
          (ELetRecIn ("helper",
             (EFun ((PVar "acc"),
                (EFun ((PVar "l"),
                   (EMatch ((EVar "l"),
                      [((PConst CNil), (EVar "acc"));
                        ((PCons ((PVar "hd"), (PVar "tl"))),
                         (EApply (
                            (EApply ((EVar "helper"),
                               (EApply (
                                  (EApply ((EBinOp ConsConcat), (EVar "hd"))),
                                  (EVar "acc")))
                               )),
                            (EVar "tl"))))
                        ]
                      ))
                   ))
                )),
             (EApply ((EApply ((EVar "helper"), (EConst CNil))), (EVar "list")))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  interprete_parse_result
    show_statements
    pstatements
    "let rec map f = fun list -> \n\
    \  match list with \n\
    \    | [] -> [] \n\
    \    | h :: t -> (f h) :: (map f t);;\n\
     let list = (map (fun x -> x + x) [1; 2; 3])";
  [%expect
    {|
    [(ELetRec ("map",
        (EFun ((PVar "f"),
           (EFun ((PVar "list"),
              (EMatch ((EVar "list"),
                 [((PConst CNil), (EConst CNil));
                   ((PCons ((PVar "h"), (PVar "t"))),
                    (EApply (
                       (EApply ((EBinOp ConsConcat),
                          (EApply ((EVar "f"), (EVar "h"))))),
                       (EApply ((EApply ((EVar "map"), (EVar "f"))), (EVar "t")))
                       )))
                   ]
                 ))
              ))
           ))
        ));
      (ELet ("list",
         (EApply (
            (EApply ((EVar "map"),
               (EFun ((PVar "x"),
                  (EApply ((EApply ((EBinOp Plus), (EVar "x"))), (EVar "x")))))
               )),
            (EList ((EConst (CInt 1)),
               (EList ((EConst (CInt 2)),
                  (EList ((EConst (CInt 3)), (EConst CNil)))))
               ))
            ))
         ))
      ] |}]
;;
