(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Parsetree
open Typedtree
open Errors

let pp_expr =
  let op_to_str = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Divide -> "/"
    | Mod -> "mod"
    | Eq -> "="
    | Neq -> "<>"
    | Lt -> "<"
    | Ltq -> "<="
    | Gt -> ">"
    | Gtq -> ">="
    | And -> "&&"
    | Or -> "||"
  in
  let rec printer ppf = function
    | Const c -> pp_const ppf c
    | IfThenElse (cond, tbody, fbody) ->
      fprintf ppf "if %a then %a else %a" printer cond printer tbody printer fbody
    | Var x -> pp_print_string ppf x
    | Let (name, body, in_e) ->
      fprintf ppf "let %s = %a in %a" name printer body printer in_e
    | LetRec (name, body, in_e) ->
      fprintf ppf "let rec %s = %a in %a" name printer body printer in_e
    | Fun (label, default, name, e) -> pp_fun ppf label default name e
    | Cons _ as l -> pp_list ppf l
    | App (fu, label, arg) -> pp_app ppf fu label arg
    | Binop (op, l, r) -> fprintf ppf "(%a %s %a)" printer l (op_to_str op) printer r
  and pp_const ppf = function
    | Bool b -> pp_print_bool ppf b
    | Int n -> fprintf ppf "%d" n
    | Nil -> fprintf ppf "[]"
    | Unit -> fprintf ppf "()"
  and pp_fun ppf label default name e =
    match label with
    | ArgNoLabel -> fprintf ppf "fun %s -> %a" name printer e
    | ArgLabeled s -> fprintf ppf "fun ~%s:%s -> %a" s name printer e
    | ArgOptional s ->
      (match default with
       | None -> fprintf ppf "fun ?%s -> %a" s printer e
       | Some def_e ->
         fprintf ppf "fun ?%s:(%s = %a) -> %a" s name printer def_e printer e)
  and pp_app ppf fu label arg =
    match label with
    | ArgNoLabel -> fprintf ppf "(%a %a)" printer fu printer arg
    | ArgLabeled s | ArgOptional s -> fprintf ppf "(%a ~%s:%a)" printer fu s printer arg
  and pp_list ppf = function
    | Cons (h, Const Nil) -> fprintf ppf "%a" printer h
    | Cons (h, t) -> fprintf ppf "%a::%a" printer h printer t
    | Var x -> pp_print_string ppf x
    | _ -> fprintf ppf "[]" (* Never happens, if type inference is correct *)
  in
  printer
;;

let pp_typ =
  let rec printer ppf = function
    | TBase t ->
      (match t with
       | TUndef -> fprintf ppf "<type_undef>"
       | TBool -> fprintf ppf "bool"
       | TInt -> fprintf ppf "int"
       | TUnit -> fprintf ppf "unit"
       | TNil -> fprintf ppf "list")
    | TVar x ->
      let rec alpha_of_int i =
        let let_of_int i = String.make 1 (char_of_int (i - 1 + int_of_char 'a')) in
        if i < 1 then "" else alpha_of_int (i / 26) ^ let_of_int (i mod 26)
      in
      fprintf ppf "'%s" (alpha_of_int (x + 1))
    | TList (Arrow (_l, _label, _r) as t) -> fprintf ppf "(%a) list" printer t
    | TList t -> fprintf ppf "%a list" printer t
    | Arrow ((Arrow (_l, _label, _r) as l), label, r) ->
      (match label with
       | ArgNoLabel -> fprintf ppf "(%a) -> %a" printer l printer r
       | ArgLabeled lab -> fprintf ppf "(~%s:%a) -> %a" lab printer l printer r
       | ArgOptional lab -> fprintf ppf "(?%s:%a) -> %a" lab printer l printer r)
    | Arrow (l, label, r) ->
      (match label with
       | ArgNoLabel -> fprintf ppf "%a -> %a" printer l printer r
       | ArgLabeled lab -> fprintf ppf "~%s:%a -> %a" lab printer l printer r
       | ArgOptional lab -> fprintf ppf "?%s:%a -> %a" lab printer l printer r)
  in
  printer
;;

let pp_value =
  let rec printer ppf = function
    | VUndef -> fprintf ppf "undefined"
    | VBool b -> pp_print_bool ppf b
    | VInt n -> fprintf ppf "%d" n
    | VNil -> fprintf ppf "[]"
    | VUnit -> fprintf ppf "()"
    | VCons (hv, tv) ->
      let rec unfold_list = function
        | VNil -> []
        | VCons (h, t) -> h :: unfold_list t
        | _ -> [ VUndef ]
        (* Never happens, if type inference is correct *)
      in
      fprintf
        ppf
        "[%a]"
        (pp_print_list ~pp_sep:(fun _ _ -> printf "; ") printer)
        (hv :: unfold_list tv)
    | VClosure _ -> fprintf ppf "<fun>"
  in
  printer
;;

let pp_type_error =
  let printer ppf = function
    | OccursCheck ->
      fprintf ppf "Occurs check failed. Never-ending loops with unification"
    | NoVariable s -> fprintf ppf "Undefined variable '%s'" s
    | UnificationFailed (l, r) ->
      fprintf
        ppf
        "Unification failed. Type of the input expression '%a', but expected '%a'"
        pp_typ
        l
        pp_typ
        r
  in
  printer
;;

let pp_error =
  let printer ppf = function
    | ParseError s -> fprintf ppf "Parse error: %s\n" s
    | TypeError e -> fprintf ppf "Type error: %a\n" pp_type_error e
    | RuntimeError s -> fprintf ppf "Runtime error: %s\n" s
  in
  printer
;;

let pp_env ppf (env : environment) =
  IdMap.iter (fun name value -> Format.fprintf ppf "%s : %a\n" name pp_value value) env
;;
