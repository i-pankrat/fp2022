(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD_ERROR = sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Interpret (M : MONAD_ERROR) = struct
  open M

  (** Env contains info about values (map: string -> value) *)
  module Env : sig
    include Map.S with type key = string

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    val find : 'a t -> key -> 'a M.t
  end = struct
    include Map.Make (String)

    let find map name =
      match find_opt name map with
      | Some v -> return v
      | None -> fail (Printf.sprintf "%S not found" name)
    ;;

    let pp pp_v ppf m =
      Format.(iter (fun k -> fprintf ppf "@[%a=%a@] " pp_print_string k pp_v) m)
    ;;
  end

  (** Value type *)
  type value =
    | VConst of Ast.c_expr
    | VClosure of string option * value Env.t * expr
  [@@deriving variants, show { with_path = false }]

  let rec eval ?(m = Env.empty) = function
    | EConst c -> return (vconst c)
    | EVar op when Parser.is_op op ->
      (* fun x y -> (+) x y *)
      elam (patvar "0") (elam (patvar "1") (eapp (eapp (evar op) (evar "0")) (evar "1")))
      |> eval ~m
    | EVar name -> Env.find m name
    | EApp (e, arg) -> eval ~m arg >>= fun arg_v -> eval_app m (e, arg_v)
    | EIfElse (c, e1, e2) ->
      eval_as_bool m c >>= fun c_v -> eval ~m (if c_v then e1 else e2)
    | ELet ((NRecF, PatVar name, name_e), body) ->
      eval ~m name_e >>= fun v -> eval ~m:(Env.add name v m) body
    | ELet ((RecF, PatVar name, name_e), body) ->
      let m = Env.add name (vclosure (Some name) m name_e) m in
      eval ~m body
    | ELam _ as f -> return (vclosure None m f)
    | EPrintf str -> eval_printf m str

  (** helper function (evaluate ang check, that result is bool)  *)
  and eval_as_bool m e =
    eval ~m e
    >>= function
    | VConst (CBool b) -> return b
    | v -> fail (Format.sprintf "Expected 'bool', but received %S" (show_value v))

  (** helper function (check, that result is string)  *)
  and as_string = function
    | VConst (CString s) -> return s
    | v -> fail (Format.sprintf "Expected 'string', but received %S" (show_value v))

  (** function, that maps our types to Base ocaml types (to use compare functions from Base module) *)
  and compare_values = function
    | VConst l, VConst r ->
      (match l, r with
       | CBool l, CBool r -> return (Base.compare_bool l r)
       | CInt l, CInt r -> return (Base.compare_int l r)
       | CString l, CString r -> return (Base.compare_string l r)
       | CUnit, CUnit -> return (Base.compare_unit () ())
       | l, r ->
         fail
           (Format.sprintf
              "Values %S, %S are not comparable"
              (show_c_expr l)
              (show_c_expr r)))
    | l, r ->
      fail
        (Format.sprintf "Values %S, %S are not comparable" (show_value l) (show_value r))

  (** evaluates printf  *)
  and eval_printf m str =
    (* List, that contains pair of variable name (pattern name for lambda expression)
         and expr (expression, that matches the matching substring (substring has type 'str_item' ))
      *)
    let names_exprs =
      List.mapi
        (fun i ->
          let new_pat = patvar (string_of_int i) in
          let new_var = evar (string_of_int i) in
          let quoted =
            eapp
              (eapp (evar "^") (eapp (eapp (evar "^") (econst (cstring "'"))) new_var))
              (econst (cstring "'"))
          in
          function
          | Hole HQString -> Some new_pat, quoted
          | Hole HInt | Hole HString -> Some new_pat, eapp (evar "__to_string") new_var
          | Const s -> None, econst (cstring s))
        str
    in
    (* Generate body for printf (expr, that contains all concatenations)
         '("some" ^ x ^ y)'
      *)
    (* we use "0", "1", ... as variable names *)
    let body =
      List.fold_left
        (fun acc (_, el) -> eapp (eapp (evar "^") acc) el)
        (econst (cstring ""))
        names_exprs
    in
    (* Wrap body to lambda expression
         (make from '("some" ^ x ^ y)'
         lambda 'fun x -> fun y -> printf ("some" ^ x ^ y))'
      *)
    List.fold_right
      (function
       | Some new_pat, _ -> elam new_pat
       | _ -> Fun.id)
      names_exprs
      (eapp (evar "printf") body)
    |> eval ~m

  and eval_op m op l r_val =
    match op with
    | "=" ->
      eval ~m l
      >>= fun l_val -> compare_values (l_val, r_val) >>| ( = ) 0 >>| cbool >>| vconst
    | "^" ->
      as_string r_val
      >>= fun r_str ->
      eval ~m l >>= as_string >>| fun l_str -> vconst (cstring (l_str ^ r_str))
    | _ ->
      let* op_f =
        match op with
        | "+" -> return ( + )
        | "-" -> return ( - )
        | "*" -> return ( * )
        | "/" -> return ( / )
        | op -> fail (Format.sprintf "Unexpected op %S" op)
      in
      let* l_val = eval ~m l in
      (match l_val, r_val, op with
       | VConst (CInt _), VConst (CInt 0), "/" -> fail "Division by zero"
       | VConst (CInt l), VConst (CInt r), _ -> return (vconst (cint (op_f l r)))
       | _ -> fail "type error with right operand")

  and eval_app m = function
    (* function to creating String value for integer value in printf function *)
    | EVar "__to_string", VConst v ->
      (match v with
       | CInt i -> string_of_int i
       | CBool i -> if i = true then "1" else "0"
       | CString s -> s
       | CUnit -> "()")
      |> cstring
      |> vconst
      |> return
    | EVar "printf", VConst (CString s) ->
      print_endline s;
      return (VConst CUnit)
    | EApp (EVar op, l), r_v when Parser.is_op op -> eval_op m op l r_v
    | f, arg_v ->
      eval ~m f
      >>= (function
      | VClosure (name, m, e) as closure ->
        let* par, body =
          match e with
          | ELam (PatVar par, body) -> return (par, body)
          | e -> fail (Format.sprintf "Expected function, but received %S" (show_expr e))
        in
        let m = Env.add par arg_v m in
        (match name with
         | Some name -> eval ~m:(Env.add name closure m) body
         | None -> eval ~m body)
      | _ -> fail "Expected function")
  ;;

  (** parse -> inference (fail if we cannot infer types) -> evaluate (evaluate ast from parser) *)
  let run input =
    match Parser.parse input with
    | Ok ast ->
      (match Inferencer.w ast with
       | Ok _ ->
         (match eval ast with
          | Ok v -> pp_value Format.std_formatter v
          | Error e -> print_endline e)
       | Error err -> Inferencer.pp_error Format.std_formatter err)
    | Error e -> Format.fprintf Format.std_formatter "Parsing error (%S)" e
  ;;
end

module Result : MONAD_ERROR with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let ( >>= ) e1 e2 =
    match e1 with
    | Ok x -> e2 x
    | Error s -> Error s
  ;;

  let return = Result.ok
  let fail = Result.error
  let ( let* ) = ( >>= )
  let ( >>| ) f g = f >>= fun x -> return (g x)
end

open Interpret (Result)

let%expect_test _ =
  run {|10|};
  [%expect {|(VConst (CInt 10))|}]
;;

let%expect_test _ =
  run {|10+28|};
  [%expect {|(VConst (CInt 38))|}]
;;

let%expect_test _ =
  run {|28/10|};
  [%expect {|(VConst (CInt 2))|}]
;;

let%expect_test _ =
  run {|1/0|};
  [%expect {|Division by zero|}]
;;

let%expect_test _ =
  run {|1=0|};
  [%expect {|(VConst (CBool false))|}]
;;

let%expect_test _ =
  run {|true=true|};
  [%expect {|(VConst (CBool true))|}]
;;

let%expect_test _ =
  run {|"str" ^ "str2"|};
  [%expect {|(VConst (CString "strstr2"))|}]
;;

let%expect_test _ =
  run {|if true=false then 2 else 5|};
  [%expect {|(VConst (CInt 5))|}]
;;

let%expect_test _ =
  run {|let x = 10 in x+20|};
  [%expect {|(VConst (CInt 30))|}]
;;

let%expect_test _ =
  run {|let x = fun id -> id + 20 in x 10|};
  [%expect {|(VConst (CInt 30))|}]
;;

let%expect_test _ =
  run {|let rec fact = fun n -> if n=1 then 1 else n * (fact (n-1)) in
         fact 10|};
  [%expect {|(VConst (CInt 3628800))|}]
;;

let%expect_test _ =
  run
    {|let rec fix = fun f x -> f (fix f) x in
      let fact = fun self n -> if n = 0 then 1 else n * self (n-1) in
      let f = fix fact in
      f 10|};
  [%expect {|(VConst (CInt 3628800))|}]
;;

let%expect_test _ =
  run {|let x = fun a b -> 10 in let y = x () in y ()|};
  [%expect {|(VConst (CInt 10))|}]
;;

let%expect_test _ =
  run {|printf "a%i" 5|};
  [%expect {|
  a5
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|let plus5 = (+) 5 in
         plus5 10|};
  [%expect {|(VConst (CInt 15))|}]
;;

let%expect_test _ =
  run {|let x = (+) in x 1 2|};
  [%expect {|
  (VConst (CInt 3))|}]
;;

let%expect_test _ =
  run {|printf "str"|};
  [%expect {|
  str
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "%i" 7|};
  [%expect {|
  7
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "%s%s" "str" "str"|};
  [%expect {|
  strstr
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "%S%s" "a" "b2"|};
  [%expect {|
  'a'b2
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "a%Sb%sc" "A" "B"|};
  [%expect {|
  a'A'bBc
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "abcdef%i%s%S" 5 "abc" "def"|};
  [%expect {|
  abcdef5abc'def'
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "k%i%s4%S" 4 "k" "du"|};
  [%expect {|
  k4k4'du'
  (VConst CUnit)|}]
;;

let%expect_test _ =
  run {|printf "k%i%s4%S" 4 "k" "du" = ()|};
  [%expect {|
  k4k4'du'
  (VConst (CBool true))|}]
;;
