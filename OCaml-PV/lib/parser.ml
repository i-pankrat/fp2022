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
  | '_' -> true
  | c -> is_letter c
;;

let is_kw = function
  | "let" | "match" | "if" | "else" | "than" | "fun" | "with" -> true
  | _ -> false
;;

let empty = take_while is_empty
let empty1 = take_while1 is_empty
let token s = empty *> s
let trim s = empty *> s <* empty
let pstoken s = empty *> string s
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
  >>= fun res -> if is_kw res then fail "keyword" else return res
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

type pdispatch =
  { cons : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  }

(* TODO: support tuples as a, b not only (a, b) *)
let pack =
  let rec create_cons len = function
    | [] -> PConst CNil
    | [ a ] -> if len = 0 then a else construct_pcons a (PConst CNil)
    | hd :: tl -> construct_pcons hd (create_cons (len + 1) tl)
  in
  let pattern pack =
    fix
    @@ fun _ ->
    create_cons 0
    <$> sep_by (pstoken "::")
        @@ choice [ pack.tuple pack; pack.cons pack; ppconst; ppvar ]
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

let ppattern = pack.pattern pack

(* parse expr *)
