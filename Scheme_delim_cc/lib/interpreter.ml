(** Copyright 2021-2022, ArtemKhel and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Env
open Utils

module Interpreter = struct
  type env = value Env.M(String).t

  and value =
    | None
    | VBool of bool
    | VInt of int
    | VList of value list
    | VString of string
    | VLambda of formals * definition list * expression list * env
    | VExpr of expression
    | VExprs of expression list
  (* [@@deriving show { with_path = false }] *)
  (* Fails with """Fatal error: Longident.flat""" ¯\_(ツ)_/¯ *)

  let rec show_value = function
    | None -> "#<none>"
    | VBool b -> "%b" #% b
    | VInt d -> "%d" #% d
    | VList l -> "%s" #% ("(" ^ String.concat (List.map l ~f:show_value) ~sep:" " ^ ")")
    | VString s -> "%s" #% s
    | VLambda _ -> "#<lambda>"
    | VExpr _ -> "#<vexpr>"
    | VExprs _ -> "#<vexprs>"
  ;;

  let pp_value fmt v = Format.fprintf fmt "%s" (show_value v)

  let rec eval_arithmetic env op args acc_init =
    match args with
    | [] -> return (VInt acc_init)
    | hd :: tl ->
      let* arg = eval_expr env hd in
      (match arg with
       | VInt n -> eval_arithmetic env op tl (op acc_init n)
       | x -> Error "[arithm2] expected int, got %s" #% (show_value x))

  and eval_arithmetic2 env op args acc_init =
    match args with
    | [] -> Error "[arithm2] not enough arguments"
    | [ x ] -> eval_arithmetic env op [ Const (Int acc_init); x ] acc_init
    | hd :: tl ->
      let* head = eval_expr env hd in
      (match head with
       | VInt head -> eval_arithmetic env op tl head
       | x -> Error "[arithm2] expected int, got %s" #% (show_value x))

  and eval_comparison env op args =
    (* TODO: copypasta *)
    let rec helper first = function
      | hd :: tl ->
        let* second = eval_expr env hd in
        (match second with
         | VInt second ->
           if op first second then helper second tl else return (VBool false)
         | x -> Error "[comparison] expected int, got %s" #% (show_value x))
      | [] -> return (VBool true)
    in
    match args with
    | hd :: tl ->
      let* second = eval_expr env hd in
      (match second with
       | VInt first -> helper first tl
       | x -> Error "[comparison] expected int, got %s" #% (show_value x))
    | [] -> Error "[comparison] not enough arguments"

  and eval_func_call env func args =
    let* func = eval_expr env func in
    match func with
    (* TODO: copypasta *)
    | VString "+" -> eval_arithmetic env ( + ) args 0
    | VString "-" -> eval_arithmetic2 env ( - ) args 0
    | VString "*" -> eval_arithmetic env ( * ) args 1
    | VString "/" -> eval_arithmetic2 env ( / ) args 1
    | VString "=" -> eval_comparison env ( = ) args
    | VString ">" -> eval_comparison env ( > ) args
    | VString ">=" -> eval_comparison env ( >= ) args
    | VString "<" -> eval_comparison env ( < ) args
    | VString "<=" -> eval_comparison env ( <= ) args
    | VString "display" -> eval_display env args
    | VString "list" -> eval_list env args
    | VString "cons" -> eval_cons env args
    | VString "car" -> eval_car env args
    | VString "cdr" -> eval_cdr env args
    | VString "empty?" -> eval_empty env args
    | VString "apply" -> eval_apply env args
    | VLambda (formals, defs, exprs, closure) ->
      eval_lambda env closure formals defs exprs args
    | _ -> Error "[func_call] unsupported op %s" #% (show_value func)

  and list_op env name match_ = function
    | [ e ] ->
      let* v = eval_expr env e in
      (match v with
       | VList l -> match_ l
       | _ -> Error "[%s] Argument must be a list" #% name)
    | _ -> Error "[%s] Too many/few arguments" #% name

  and eval_empty env =
    list_op env "eval_empty" (function
      | [] -> return (VBool true)
      | _ -> return (VBool false))

  and eval_car env =
    list_op env "eval_car" (function
      | hd :: _ -> return hd
      | _ -> Error "[eval_car] List is empty")

  and eval_cdr env =
    list_op env "eval_cdr" (function
      | _ :: tl -> return (VList tl)
      | _ -> Error "[eval_cdr] List is empty")

  and eval_list env args : (value, string) result =
    let rec helper = function
      | [] -> return []
      | hd :: tl ->
        let* hd = eval_expr env hd in
        let* tl = helper tl in
        return (hd :: tl)
    in
    let* l = helper args in
    return (VList l)

  and eval_cons env = function
    | [ f; s ] ->
      let* fe = eval_expr env f in
      let* se = eval_expr env s in
      (match se with
       | VList l -> return (VList (fe :: l))
       | VExprs el ->
         let* r = batch_eval el (eval_expr env) in
         return (VList (fe :: r))
       | _ -> Error "[eval_cons] Second argument must be a list")
    | _ -> Error "[eval_cons] Too many/few arguments"

  and eval_display (env : env) args =
    let rec helper = function
      | hd :: tl ->
        let* res = eval_expr env hd in
        print_string "%s " #% (show_value res);
        helper tl
      | [] ->
        print_newline ();
        return None
    in
    helper args

  and eval_var (env : env) (name : string) =
    match Env.find env name with
    | Some value -> return value
    | None -> return (VString name)

  and batch_eval : 'a. 'a list -> ('a -> (value, 'b) result) -> (value list, 'b) result =
   fun (exprs : 'a list) (f : 'a -> (value, 'b) result) : (value list, 'b) result ->
    let rec helper = function
      | [] -> return []
      | hd :: tl ->
        let* r = f hd in
        let* t = helper tl in
        return (List.cons r t)
    in
    helper exprs

  and eval_lambda env closure formals defs exprs args =
    let lambda_env = Env.merge_envs env closure in
    let create_env = function
      | Formal x ->
        return (Env.set lambda_env ~key:x ~data:(VExprs args))
        (* let* l = batch_eval args (eval_expr env) in
        return (Env.set env ~key:x ~data:(VList l)) *)
      | FormalList list ->
        (match List.zip list args with
         | List.Or_unequal_lengths.Unequal_lengths ->
           Error "[eval_lambda] Too many/few arguments"
         | List.Or_unequal_lengths.Ok zip ->
           let* lambda_env = eval_defs lambda_env zip in
           eval_defs lambda_env defs)
    in
    let rec helper env = function
      | [ x ] -> eval_expr env x
      | hd :: tl ->
        let* _ = eval_expr env hd in
        helper env tl
      | [] -> Error "unreachable? (many1 parser)"
    in
    let* env = create_env formals in
    helper env exprs

  and eval_if env cond then_ (else_ : expression option) =
    let* cond =
      match eval_expr env cond with
      | Error e -> Error e
      | Ok (VBool false) -> return false
      | _ -> return true
    in
    if cond
    then eval_expr env then_
    else (
      match else_ with
      | None -> return None
      | Some e -> eval_expr env e)

  and eval_datum = function
    | DConst c -> eval_const c
    | DList l -> VList (List.map l ~f:eval_datum)
    | DAbbr (p, d) -> eval_dabbr p d

  and eval_const = function
    | Int i -> VInt i
    | String s -> VString s
    | Bool b -> VBool b

  and eval_dabbr _ d = eval_datum d

  and eval_quasiquote env = function
    | QConst c -> return (eval_const c)
    | QDatum d -> return (eval_datum d)
    | QList ql ->
      let* res = batch_eval ql (eval_quasiquote env) in
      return (VList res)
    | QUnquote e -> eval_expr env e

  and eval_apply env args =
    (* Here be kostyli (for Y combinator) *)
    let unpack = function
      | DConst c -> [ Const c ]
      | _ -> failwith "todo"
    in
    match args with
    | [ func; arglist ] ->
      (match arglist with
       | Quote q -> eval_func_call env func (unpack q)
       | FuncCall _ | Lambda _ ->
         let* res = eval_expr env arglist in
         (match res with
          | VList l ->
            eval_func_call
              env
              func
              (List.map l ~f:(function
                | VBool b -> Const (Bool b)
                | VInt i -> Const (Int i)
                | VString s -> Const (String s)
                | _ -> failwith "todo"))
          | _ -> Error "todo")
       | Var _ ->
         let* res = eval_expr env arglist in
         (match res with
          | VExprs e -> eval_func_call env func e
          | _ -> Error "[eval_apply] Expected list")
       | _ -> Error "not implemented")
    | _ -> Error "[eval_apply] Too many/few arguments"

  and eval_expr env = function
    | Const c -> return (eval_const c)
    | FuncCall (func, args) -> eval_func_call env func args
    | Var name -> eval_var env name
    | Lambda (formals, defs, exprs) -> return (VLambda (formals, defs, exprs, env))
    | If (cond, then_, else_) -> eval_if env cond then_ else_
    | Quote q -> return (eval_datum q)
    | Quasiquote q -> eval_quasiquote env q

  and eval_def env (id : id) (expr : expression) =
    let* value = eval_expr env expr in
    let new_env = Env.set env ~key:id ~data:value in
    return new_env

  and eval_defs (env : env) = function
    | [] -> return env
    | (id, expr) :: tl ->
      let* new_env = eval_def env id expr in
      eval_defs new_env tl

  and eval_form env (form : form) =
    match form with
    | Expr expr ->
      let* res = eval_expr env expr in
      return (res, env)
    | Def (id, expr) ->
      let* new_env = eval_def env id expr in
      return (None, new_env)
  ;;

  let eval_program env =
    let rec helper env res = function
      | [] -> return (res, env)
      | hd :: tl ->
        (match eval_form env hd with
         | Ok (res, env) -> helper env res tl
         | Error e -> Result.Error e)
    in
    helper env None
  ;;
end
