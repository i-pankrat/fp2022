(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Real monadic interpreter goes here *)

open Ast
open Base

module type FailMonad = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type ierror =
  [ `DivisionByZero
  | `UnboudnValue
  | `TypeMismatch
  | `UnsupportedOperation
  | `PatternMismatch
  ]

type 'a binop =
  | EmptyBinOp of bin_op
  | PartialBinOp of 'a * bin_op

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VTuple of value list
  | VList of value list
  | VFun of pattern * expr
  | VBinOp of value binop
  | VUnit

let cvint i = VInt i
let cvbool b = VBool b
let cvstring s = VString s
let cvfun p e = VFun (p, e)
let cvbinop op = VBinOp op

type environment = (id, value, String.comparator_witness) Map.t
(* ((id, value, String.comparator_witness) Map.t  ? *)

module Env (M : FailMonad) = struct
  open M

  let empty = Base.Map.empty (module Base.String)

  let extend_by_one id value env =
    match Map.add env ~key:id ~data:value with
    | `Ok env -> env
    | `Duplicate ->
      Map.mapi env ~f:(fun ~key:name ~data:old_value ->
        if Poly.( = ) id name then value else old_value)
  ;;

  List.fold_left

  let extend env bindings =
    return
    @@ List.fold ~f:(fun env (id, value) -> extend_by_one id value env) bindings ~init:env
  ;;

  let find map key =
    match Map.find map key with
    | None -> failwith "Create fail with good error"
    | Some value -> return value
  ;;
end

module Interpret (M : FailMonad) : sig
  val run : expr -> (value, ierror) M.t
end = struct
  open M
  open Env (M)

  let eval_binop arg =
    let eval_comparison = function
      | Eq -> Poly.( = )
      | Neq -> Poly.( <> )
      | Gt -> Poly.( > )
      | Lt -> Poly.( < )
      | Gtq -> Poly.( >= )
      | Ltq -> Poly.( <= )
      | _ -> failwith "Unsupported operation. TODO: fix that"
    in
    function
    | EmptyBinOp op ->
      (match op, arg with
       | Plus, VInt _
       | Minus, VInt _
       | Mult, VInt _
       | Divide, VInt _
       | Mod, VInt _
       | And, VBool _
       | Or, VBool _
       | ConsConcat, VList _
       | Eq, _
       | Neq, _
       | Gt, _
       | Lt, _
       | Gtq, _
       | Ltq, _ -> return @@ cvbinop @@ PartialBinOp (arg, op)
       | _ -> failwith "TODO 10")
    | PartialBinOp (farg, op) ->
      (match op, farg, arg with
       | Plus, VInt i1, VInt i2 -> return @@ cvint (i1 + i2)
       | Minus, VInt i1, VInt i2 -> return @@ cvint (i1 - i2)
       | Mult, VInt i1, VInt i2 -> return @@ cvint (i1 * i2)
       | Divide, VInt _, VInt i2 when i2 = 0 -> fail `DivisionByZero
       | Divide, VInt i1, VInt i2 -> return @@ cvint (i1 / i2)
       | Mod, VInt _, VInt i2 when i2 = 0 -> fail `DivisionByZero
       | Mod, VInt i1, VInt i2 -> return @@ cvint (i1 % i2)
       | And, VBool b1, VBool b2 -> return @@ cvbool (b1 && b2)
       | Or, VBool b1, VBool b2 -> return @@ cvbool (b1 || b2)
       | ConsConcat, _, _ -> failwith "TODO1"
       | op, VInt i1, VInt i2 -> return @@ cvbool @@ (eval_comparison op) i1 i2
       | op, VBool b1, VBool b2 -> return @@ cvbool @@ (eval_comparison op) b1 b2
       | op, VString s1, VString s2 -> return @@ cvbool @@ (eval_comparison op) s1 s2
       | _ -> failwith "NEVER SHOULD HAPPEN")
  ;;

  let eval_pattern = function
    | PWild, _ -> return []
    | PVar name, value -> return [ name, value ]
    | PConst const, cvalue ->
      (match const, cvalue with
       | CBool b1, VBool b2 when Poly.( = ) b1 b2 -> return []
       | CInt i1, VInt i2 when Poly.( = ) i1 i2 -> return []
       | CString s1, VString s2 when Poly.( = ) s1 s2 -> return []
       | _ -> fail `PatternMismatch)
    | _ -> fail `PatternMismatch (* Списки, кортежи нужно добавить ешё*)
  ;;

  let rec eval expr env =
    match expr with
    | EConst const ->
      (match const with
       | CInt i -> return @@ cvint i
       | CBool b -> return @@ cvbool b
       | CString s -> return @@ cvstring s
       | _ -> failwith "TODO3")
    | EFun (pat, expr) -> return @@ cvfun pat expr
    | EVar var -> find env var
    | EApply (func, arg) ->
      let* evaled_fun = eval func env in
      let* evaled_arg = eval arg env in
      (match evaled_fun with
       | VFun (pat, expr) ->
         let* evaled_pat = eval_pattern (pat, evaled_arg) in
         let* new_env = extend env evaled_pat in
         eval expr new_env
       | VBinOp op -> eval_binop evaled_arg op
       | _ -> failwith "TODO")
    | EBinOp op -> return @@ cvbinop @@ EmptyBinOp op
    | EIfThenElse (c, t, e) ->
      let* evaled_condtion = eval c env in
      (match evaled_condtion with
       | VBool res -> if res then eval t env else eval e env
       | _ -> failwith "Typechecker already checked that :)")
    | EMatch (matched, patterns) ->
      let* evaled_match = eval matched env in
      let rec eval_match_expr = function
        | [] -> fail `PatternMismatch
        | (p, e) :: tl ->
          let res = eval_pattern (p, evaled_match) in
          run
            res
            ~ok:(fun res ->
              let* env = extend env res in
              eval e env)
            ~err:(fun _res -> eval_match_expr tl)
      in
      eval_match_expr patterns
    | ELet (_, expr) -> eval expr env
    | ELetRec (name, expr) ->
      let* rec_expr = eval expr env in
      let* env = extend env [ name, rec_expr ] in
      eval expr env
    | ELetIn (name, expr1, expr2) ->
      let* evaled_expr1 = eval expr1 env in
      let* env = extend env [ name, evaled_expr1 ] in
      eval expr2 env
    | ELetRecIn (name, expr1, expr2) ->
      let new_expr = ELetRec (name, expr1) in
      let* evaled1 = eval new_expr env in
      let* env = extend env [ name, evaled1 ] in
      eval expr2 env
  ;;

  let run program =
    let env = empty in
    eval program env
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) monad f = bind monad ~f
end)

(* Tests for interpretator *)
let test = EApply (EFun (PVar "x", EVar "x"), EConst (CInt 5))

let%test _ =
  match InterpretResult.run test with
  | Base.Result.Ok (VInt 5) -> true
  | _ -> false
;;

let test =
  EApply
    ( EApply
        ( EBinOp Mult
        , EApply
            ( EApply
                ( EBinOp Mult
                , EApply (EApply (EBinOp Plus, EConst (CInt 1)), EConst (CInt 1)) )
            , EApply (EApply (EBinOp Minus, EConst (CInt 5)), EConst (CInt 2)) ) )
    , EApply (EApply (EBinOp Divide, EConst (CInt 42)), EConst (CInt 6)) )
;;

let%test _ =
  match InterpretResult.run test with
  | Base.Result.Ok (VInt 42) -> true
  | _ -> false
;;

let test =
  ELetIn
    ( "inc"
    , EFun (PVar "x", EApply (EApply (EBinOp Plus, EConst (CInt 1)), EVar "x"))
    , EApply (EVar "inc", EConst (CInt 5)) )
;;

let%test _ =
  match InterpretResult.run test with
  | Base.Result.Ok (VInt 6) -> true
  | _ -> false
;;

let test =
  ELetRecIn
    ( "sum"
    , EFun
        ( PVar "x"
        , EIfThenElse
            ( EApply (EApply (EBinOp Eq, EConst (CInt 1)), EVar "x")
            , EConst (CInt 1)
            , EApply
                ( EApply (EBinOp Plus, EVar "x")
                , EApply
                    (EVar "sum", EApply (EApply (EBinOp Minus, EVar "x"), EConst (CInt 1)))
                ) ) )
    , EApply (EVar "sum", EConst (CInt 2)) )
;;

let%test _ =
  match InterpretResult.run test with
  | Base.Result.Ok (VInt 3) -> true
  | _ -> false
;;

let test =
  ELetRecIn
    ( "fib"
    , EFun
        ( PVar "n"
        , EIfThenElse
            ( EApply (EApply (EBinOp Gtq, EConst (CInt 1)), EVar "n")
            , EConst (CInt 1)
            , EApply
                ( EApply
                    ( EBinOp Plus
                    , EApply
                        ( EVar "fib"
                        , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 1)) ) )
                , EApply
                    (EVar "fib", EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 2)))
                ) ) )
    , EApply (EVar "fib", EConst (CInt 6)) )
;;

let%test _ =
  match InterpretResult.run test with
  | Base.Result.Ok (VInt 13) -> true
  | _ -> false
;;

let test =
  ELetRecIn
    ( "fac"
    , EFun
        ( PVar "n"
        , EIfThenElse
            ( EApply (EApply (EBinOp Eq, EConst (CInt 1)), EVar "n")
            , EConst (CInt 1)
            , EApply
                ( EApply
                    ( EBinOp Mult
                    , EApply
                        ( EVar "fac"
                        , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 1)) ) )
                , EVar "n" ) ) )
    , EApply (EVar "fac", EConst (CInt 5)) )
;;

let%test _ =
  match InterpretResult.run test with
  | Base.Result.Ok (VInt 120) -> true
  | _ -> false
;;
