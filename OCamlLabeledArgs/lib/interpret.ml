(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parsetree
open Typedtree
open Errors

module Interpret (M : MONADERROR) = struct
  open M

  let rec compare_values v v' =
    match v, v' with
    | VBool b, VBool b' -> return (compare_bool b b')
    | VInt n, VInt n' -> return (compare_int n n')
    | VNil, VNil -> return 0
    | VCons _, VNil -> return 1
    | VNil, VCons _ -> return ~-1
    | VCons (x, xs), VCons (x', xs') ->
      let* c = compare_values x x' in
      if c = 0 then compare_values xs xs' else return c
    | _ -> fail (RuntimeError "could not compare values")
  ;;

  let find name (env : environment) =
    match IdMap.find name env with
    | v -> v
    | exception Not_found -> VUndef
  ;;

  let upd_env name value (env : environment) : environment = IdMap.add name value env

  let rec eval e (env : environment) =
    match e with
    | Const c -> eval_const c
    | Var name -> return (find name env)
    | Binop _ -> eval_binop e env
    | Fun (label, default, name, exp) ->
      return (VClosure (None, env, Fun (label, default, name, exp)))
    | Cons (h, t) -> eval_list h t env
    | App (fu, label, arg) -> eval_app fu label arg env
    | IfThenElse (cond, tbody, fbody) -> eval_if cond tbody fbody env
    | Let (name, body, exp) -> eval_let name body exp env
    | LetRec (name, body, exp) -> eval_letrec name body exp env

  and eval_const = function
    | Bool b -> return (VBool b)
    | Int n -> return (VInt n)
    | Nil -> return VNil
    | Unit -> return VUnit

  and eval_if cond tbody fbody env =
    eval cond env
    >>= function
    | VBool b -> eval (if b then tbody else fbody) env
    | _ -> fail (RuntimeError "error in if condition")

  and eval_list h t env =
    let* h_val = eval h env in
    let* t_val = eval t env in
    return (VCons (h_val, t_val))

  and eval_binop e env =
    match e with
    | Binop (op, l, r) ->
      (match op with
       | Plus -> eval_arithm ( + ) l r env
       | Minus -> eval_arithm ( - ) l r env
       | Mult -> eval_arithm ( * ) l r env
       | Divide -> eval_arithm ~check_div:true ( / ) l r env
       | Mod -> eval_arithm ~check_div:true ( mod ) l r env
       | Eq -> eval_cmp ( = ) l r env
       | Neq -> eval_cmp ( <> ) l r env
       | Lt -> eval_cmp ( < ) l r env
       | Ltq -> eval_cmp ( <= ) l r env
       | Gt -> eval_cmp ( > ) l r env
       | Gtq -> eval_cmp ( >= ) l r env
       | And -> eval_logic ( && ) l r env
       | Or -> eval_logic ( || ) l r env)
    | _ -> fail (RuntimeError "error in binary operation")

  and eval_arithm ?(check_div = false) op l r env =
    let* l_val = eval l env in
    let* r_val = eval r env in
    let* diff = compare_values r_val (VInt 0) in
    if check_div && diff = 0
    then fail (RuntimeError "Division by zero")
    else (
      match l_val, r_val with
      | VInt n, VInt n' -> return (VInt (op n n'))
      | VInt _, _ -> fail (RuntimeError "type error with right operand")
      | _ -> fail (RuntimeError "type error with left operand"))

  and eval_cmp op l r env =
    let* l_val = eval l env in
    let* r_val = eval r env in
    let* diff = compare_values l_val r_val in
    return (VBool (op diff 0))

  and eval_logic op l r env =
    let* l_val = eval l env in
    let* r_val = eval r env in
    match l_val, r_val with
    | VBool b, VBool b' -> return (VBool (op b b'))
    | VBool _, _ -> fail (RuntimeError "type error with right operand")
    | _ -> fail (RuntimeError "type error with left operand")

  and eval_app fu label arg env =
    let* closure = eval fu env in
    match closure with
    | VClosure (name, fu_env, fun_exp) ->
      let* arg_val = eval arg env in
      let* lab, default, arg_name, fu_body =
        match fun_exp with
        | Fun (lab, default, arg_name, fu_body) -> return (lab, default, arg_name, fu_body)
        | _ -> fail (RuntimeError "Not a function")
      in
      let* default_value =
        match default with
        | Some e -> eval e env
        | None -> return VUndef
      in
      let has_unspecified_args, env_updated =
        match lab with
        | ArgNoLabel -> false, upd_env arg_name arg_val fu_env
        | ArgLabeled l ->
          ( false
          , (match label with
             | ArgLabeled apply_l when compare_string apply_l l = 0 ->
               upd_env l arg_val (upd_env arg_name arg_val fu_env)
             | ArgLabeled apply_l ->
               upd_env apply_l arg_val (upd_env apply_l arg_val fu_env)
             | _ -> upd_env l arg_val (upd_env arg_name arg_val fu_env)) )
        | ArgOptional l ->
          (match label with
           | ArgLabeled apply_l when compare_string apply_l l = 0 ->
             false, upd_env l arg_val (upd_env arg_name arg_val fu_env)
           | _ -> true, upd_env l default_value (upd_env arg_name default_value fu_env))
      in
      if has_unspecified_args
      then eval_app fu_body label arg env_updated
      else (
        match name with
        | Some n -> eval fu_body (upd_env n closure env_updated)
        | None -> eval fu_body env_updated)
    | _ -> fail (RuntimeError "This is not a function. It can not be applied.")

  and eval_let name body exp env =
    let* body_val = eval body env in
    eval exp (upd_env name body_val env)

  and eval_letrec name body exp env =
    let env_updated = upd_env name (VClosure (Some name, env, body)) env in
    eval exp env_updated
  ;;
end

module EvalResult : MONADERROR with type 'a t = ('a, error) result = struct
  type 'a t = ('a, error) result

  let ( >>= ) value func =
    match value with
    | Ok x -> func x
    | Error s -> Error s
  ;;

  let return x = Ok x
  let fail error = Error error

  let ( let* ) value func =
    match value with
    | Ok x -> func x
    | Error s -> Error s
  ;;
end
