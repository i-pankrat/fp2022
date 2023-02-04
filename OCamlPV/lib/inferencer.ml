(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Typedtree

let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `Empty_Pattern
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "Unification failed on %a and %a"
      Pprintast.pp_typ
      l
      Pprintast.pp_typ
      r
  | `Empty_Pattern -> Format.fprintf ppf "Empty pattern"
;;

module R : sig
  include Monad.Infix

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  module Syntax : sig
    val ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | Ty_var b -> b = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | List t -> occurs_in v t
    | Tuple ts -> List.fold ts ~init:false ~f:(fun acc t -> occurs_in v t || acc)
    | Prim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Ty_var b -> VarSetInit.add b acc
      | Arrow (l, r) -> helper (helper acc l) r
      | List t -> helper acc t
      | Tuple ts -> List.fold ts ~init:acc ~f:(fun acc t -> helper acc t)
      | Prim _ -> acc
    in
    helper VarSetInit.empty
  ;;
end

let fold_left map ~init ~f =
  Map.Poly.fold map ~init ~f:(fun ~key ~data acc ->
    let open R.Syntax in
    let* acc = acc in
    f key data acc)
;;

module Subst : sig
  type t

  val pp : Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> ty

  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty) Map.Poly.t

  let pp ppf subst =
    let open Format in
    Map.Poly.iteri subst ~f:(fun ~key ~data ->
      fprintf ppf "'_%d -> %a@\n" key Pprintast.pp_typ data)
  ;;

  let empty = Map.Poly.empty
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* f, t = mapping k v in
    return @@ Map.Poly.singleton f t
  ;;

  let find_exn f m = Map.Poly.find_exn m f
  let find f m = Map.Poly.find m f
  let remove m f = Map.Poly.remove m f

  let apply subst =
    let rec helper = function
      | Ty_var b as ty ->
        (match find_exn b subst with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Arrow (l, r) -> Arrow (helper l, helper r)
      | List t -> List (helper t)
      | Tuple l -> Tuple (List.map ~f:(fun t -> helper t) l)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Prim l, Prim r when String.equal l r -> return empty
    | Prim _, Prim _ -> fail (`Unification_failed (l, r))
    | Ty_var a, Ty_var b when Int.equal a b -> return empty
    | Ty_var b, t | t, Ty_var b -> singleton b t
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | List a, List b -> unify a b
    | Tuple a, Tuple b ->
      let subs =
        List.fold2 a b ~init:(return empty) ~f:(fun subs a b ->
          let* subs = subs in
          let sa = apply subs a in
          let sb = apply subs b in
          let* sub1 = unify sa sb in
          compose subs sub1)
      in
      (match subs with
       | Ok res -> res
       | Unequal_lengths -> fail (`Unification_failed (l, r)))
    | _ -> fail (`Unification_failed (l, r))

  and extend key value extensible_subst =
    match Map.Poly.find extensible_subst key with
    | None ->
      let v = apply extensible_subst value in
      let* s2 = singleton key v in
      fold_left extensible_subst ~init:(return s2) ~f:(fun key value acc ->
        let v = apply s2 value in
        let* mapk, mapv = mapping key v in
        match Map.Poly.add acc ~key:mapk ~data:mapv with
        | `Ok map -> return map
        | `Duplicate -> return acc)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose extensible_subst s2

  and compose s1 s2 = fold_left s2 ~init:(return s1) ~f:extend

  let compose_all s1 =
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    in
    fold_left s1 ~init:(return empty) ~f:compose
  ;;
end

module VarSet = struct
  include VarSetInit

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]
type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (names, ty) -> (not (VarSet.mem v names)) && Type.occurs_in v ty
  ;;

  let free_vars = function
    | S (names, ty) -> VarSet.diff (Type.free_vars ty) names
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  let pp = pp_scheme
end

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, scheme) ->
      VarSet.union acc (Scheme.free_vars scheme))
  ;;

  let apply subst env = List.Assoc.map env ~f:(Scheme.apply subst)

  let pp ppf xs =
    Caml.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Caml.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Caml.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> Ty_var n

let instantiate : scheme -> ty R.t =
 fun (S (names, ty)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    names
    (return ty)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env var env =
  match List.Assoc.find_exn env ~equal:String.equal var with
  | (exception Caml.Not_found) | (exception Not_found_s _) -> fail (`No_variable var)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer =
  let open Ast in
  let rec (pattern_helper : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * ty) R.t) =
   fun env -> function
    | PConst const ->
      (match const with
       | CBool _ -> return (env, Prim "bool")
       | CInt _ -> return (env, Prim "int")
       | CString _ -> return (env, Prim "string")
       | CUnit -> return (env, Prim "unit")
       | CNil ->
         let* tv = fresh_var in
         return (env, List tv))
    | PVar id ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (id, S (VarSet.empty, tv)) in
      return (env, tv)
    | PTuple tuple ->
      let rec tuple_helper envpat =
        let* env, patterns = envpat in
        match patterns with
        | [] -> return (env, [])
        | hd :: tl ->
          let* envhd, tyhd = pattern_helper env hd in
          let new_envpat = return (envhd, tl) in
          let* envtl, tytl = tuple_helper new_envpat in
          return (envtl, tyhd :: tytl)
      in
      let initvalue = return (env, tuple) in
      let* finenv, fintys = tuple_helper initvalue in
      return (finenv, Tuple fintys)
    | PCons (head, tail) ->
      let* env, ty1 = pattern_helper env head in
      let ty1 = List ty1 in
      let* env, ty2 = pattern_helper env tail in
      let* subst = unify ty1 ty2 in
      let finenv = TypeEnv.apply subst env in
      return (finenv, Subst.apply subst ty1)
    | PWild ->
      let* ty = fresh_var in
      return (env, ty)
  in
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * ty) R.t) =
   fun env -> function
    | EVar x -> lookup_env x env
    | EConst const ->
      (match const with
       | CBool _ -> return (Subst.empty, Prim "bool")
       | CInt _ -> return (Subst.empty, Prim "int")
       | CString _ -> return (Subst.empty, Prim "string")
       | CUnit -> return (Subst.empty, Prim "unit")
       | CNil ->
         let* var = fresh_var in
         return (Subst.empty, list_typ var))
    | EBinOp op ->
      (match op with
       | Plus | Minus | Divide | Mult | Mod ->
         return (Subst.empty, arrow int_typ (arrow int_typ int_typ))
       | And | Or -> return (Subst.empty, arrow bool_typ (arrow bool_typ bool_typ))
       | Eq | Neq | Gt | Lt | Gtq | Ltq ->
         let* var = fresh_var in
         return (Subst.empty, arrow var (arrow var bool_typ))
       | ConsConcat ->
         let* var = fresh_var in
         let list = list_typ var in
         return (Subst.empty, arrow list (arrow list list)))
    | EApply (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (Arrow (t2, tv)) in
      let typedres = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, typedres)
    | EIfThenElse (c, th, el) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env th in
      let* s3, t3 = helper env el in
      let* s4 = unify t1 (Prim "bool") in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, Subst.apply s5 t2)
    | ELet (_, e) ->
      let* s, t = helper env e in
      (* let env = TypeEnv.apply s env in *)
      return (s, t)
    | ELetIn (name, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t1 = generalize env2 t1 in
      let* s2, t3 = helper (TypeEnv.extend env2 (name, t1)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t3)
    | ELetRec (name, e) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
      let* s, t = helper env e in
      return (s, t)
    | ELetRecIn (name, e1, e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
      let* s1, t1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s1 s2 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2 = helper TypeEnv.(extend (apply s env) (name, t2)) e2 in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2)
    | EFun (arg, e) ->
      let* env, t1 = pattern_helper env arg in
      let* s, t2 = helper env e in
      let typedres = Arrow (Subst.apply s t1, t2) in
      return (s, typedres)
    | EMatch (cond, matches) ->
      let* cond_sub, cond_ty = helper env cond in
      let env = TypeEnv.apply cond_sub env in
      let rec (matches_helper : (pattern * expr) list -> (Subst.t * ty) R.t) = function
        | [] -> fail `Empty_Pattern
        | hd :: [] ->
          (match hd with
           | pat, expr ->
             let* pat_env, pat_ty = pattern_helper env pat in
             let* s1 = unify cond_ty pat_ty in
             let* s2, expr_ty = helper (TypeEnv.apply s1 pat_env) expr in
             let* finalsub = Subst.compose s1 s2 in
             return (finalsub, Subst.apply finalsub expr_ty))
        | hd :: tl ->
          let* s1, ty1 = matches_helper [ hd ] in
          let* s2, ty2 = matches_helper tl in
          let* s3 = unify ty1 ty2 in
          let* finalsubst = Subst.compose_all [ s1; s2; s3 ] in
          return (finalsubst, Subst.apply s3 ty1)
      in
      let* match_sub, match_ty = matches_helper matches in
      let* finalmatchsub = Subst.compose cond_sub match_sub in
      return (finalmatchsub, Subst.apply finalmatchsub match_ty)
    | EList (h, t) ->
      let* s1, t1 = helper env h in
      let t1 = list_typ t1 in
      let* s2, t2 = helper env t in
      let* s3 = unify t1 t2 in
      let* subst = Subst.compose_all [ s1; s2; s3 ] in
      return (subst, Subst.apply subst t1)
    | ETuple tuple ->
      let* s, t =
        List.fold
          tuple
          ~init:(return (Subst.empty, []))
          ~f:(fun acc expr ->
            let* tuple_s, tuple = acc in
            let* s, t = helper env expr in
            let* subst = Subst.compose s tuple_s in
            return (subst, t :: tuple))
      in
      return (s, tuple_typ @@ List.rev t)
  in
  helper
;;

let w e = Result.map (run (infer TypeEnv.empty e)) ~f:snd
let unify = Subst.unify

(** Unification tests *)

let run_subst subst =
  match R.run subst with
  | Result.Error e -> Format.printf "Error: %a%!" pp_error e
  | Ok subst -> Format.printf "%a%!" Subst.pp subst
;;

(** Arrow unification *)

let%expect_test _ =
  let _ = unify (var_typ 1 @-> var_typ 1) (int_typ @-> var_typ 2) |> run_subst in
  [%expect {|
    '_1 -> int
    '_2 -> int |}]
;;

let%expect_test _ =
  let _ =
    unify (var_typ 1 @-> var_typ 1) ((var_typ 2 @-> int_typ) @-> int_typ @-> int_typ)
    |> run_subst
  in
  [%expect {|
    '_1 -> (int -> int)
    '_2 -> int |}]
;;

let%expect_test _ =
  let _ = unify (var_typ 1 @-> var_typ 2) (var_typ 2 @-> var_typ 3) |> run_subst in
  [%expect {|
    '_1 -> '_3
    '_2 -> '_3 |}]
;;

let%expect_test _ =
  let _ = unify (var_typ 1 @-> bool_typ) (var_typ 2 @-> int_typ) |> run_subst in
  [%expect {| Error |}]
;;

(** List unificaiton *)

let%expect_test _ =
  let _ =
    unify (list_typ @@ list_typ @@ var_typ 1) (list_typ @@ var_typ 2) |> run_subst
  in
  [%expect {| '_2 -> '_1 list |}]
;;

let%expect_test _ =
  let _ = unify (list_typ @@ int_typ) (list_typ @@ var_typ 0) |> run_subst in
  [%expect {| '_0 -> int |}]
;;

(** Tuple unification *)

let%expect_test _ =
  let _ =
    unify
      (tuple_typ [ int_typ; var_typ 1; var_typ 2 ])
      (tuple_typ [ var_typ 3; bool_typ; list_typ @@ int_typ ])
    |> run_subst
  in
  [%expect {|
    '_1 -> bool
    '_2 -> int list
    '_3 -> int |}]
;;

(** Infer tests *)

let run_infer = function
  | Result.Error e -> Format.printf "Error: %a%!" pp_error e
  | Result.Ok typed -> Format.printf "%a%!" Pprintast.pp_typ typed
;;

(** Infer constant type*)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EConst (CInt 4) in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EConst (CBool true) in
    w e |> run_infer
  in
  [%expect {| bool |}]
;;

(** Infer condition statement type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EIfThenElse (EConst (CBool true), EConst (CInt 4), EConst (CInt 5)) in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

(** Infer binary operation type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EApply (EApply (EBinOp Plus, EConst (CInt 4)), EConst (CInt 4)) in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EApply (EApply (EBinOp And, EConst (CBool true)), EConst (CBool false)) in
    w e |> run_infer
  in
  [%expect {| bool |}]
;;

(** Infer fun type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EFun (PVar "x", EApply (EApply (EBinOp Mult, EVar "x"), EVar "x")) in
    w e |> run_infer
  in
  [%expect {| (int -> int) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      EFun
        ( PCons (PVar "a", PCons (PVar "b", PConst CNil))
        , EApply (EApply (EBinOp Mult, EVar "a"), EVar "b") )
    in
    w e |> run_infer
  in
  [%expect {| (int list -> int) |}]
;;

(** Infer "let in" type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELetIn
        ( "x"
        , EApply (EApply (EBinOp Mult, EConst (CInt 5)), EConst (CInt 5))
        , EApply (EApply (EBinOp Mult, EConst (CInt 2)), EVar "x") )
    in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

(** Infer "let" type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELet
        ("inc", EFun (PVar "x", EApply (EApply (EBinOp Plus, EVar "x"), EConst (CInt 1))))
    in
    w e |> run_infer
  in
  [%expect {| (int -> int) |}]
;;

(** Infer fuctorial function type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELetRec
        ( "fac"
        , EFun
            ( PVar "n"
            , EIfThenElse
                ( EApply (EApply (EBinOp Gt, EVar "n"), EConst (CInt 0))
                , EApply
                    ( EApply (EBinOp Mult, EVar "n")
                    , EApply
                        ( EVar "fac"
                        , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 1)) ) )
                , EConst (CInt 1) ) ) )
    in
    w e |> run_infer
  in
  [%expect {| (int -> int) |}]
;;

(** Infer "let rec in" type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELetRecIn
        ( "fac"
        , EFun
            ( PVar "n"
            , EIfThenElse
                ( EApply (EApply (EBinOp Gt, EVar "n"), EConst (CInt 0))
                , EApply
                    ( EApply (EBinOp Mult, EVar "n")
                    , EApply
                        ( EVar "fac"
                        , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 1)) ) )
                , EConst (CInt 1) ) )
        , EApply (EVar "fac", EConst (CInt 5)) )
    in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

(** Infer match type *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      EMatch
        ( EConst (CBool true)
        , [ PConst (CBool true), EConst (CInt 1); PConst (CBool false), EConst (CInt 0) ]
        )
    in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

(** Infer list type *)

(** Some test funcitons to type inferencer test*)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELetRec
        ( "list_sum"
        , EFun
            ( PVar "list"
            , EMatch
                ( EVar "list"
                , [ PConst CNil, EConst (CInt 0)
                  ; ( PCons (PVar "hd", PVar "tl")
                    , EApply
                        ( EApply (EBinOp Plus, EVar "hd")
                        , EApply (EVar "list_sum", EVar "tl") ) )
                  ] ) ) )
    in
    w e |> run_infer
  in
  [%expect {| (int list -> int) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELetRec
        ("always_true", EFun (PVar "a", EMatch (EVar "a", [ PWild, EConst (CBool true) ])))
    in
    w e |> run_infer
  in
  [%expect {| ('_2 -> bool) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELet
        ( "fst"
        , EFun (PVar "a", EMatch (EVar "a", [ PTuple [ PVar "f"; PVar "s" ], EVar "f" ]))
        )
    in
    w e |> run_infer
  in
  [%expect {| (('_1 * '_2) -> '_1) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELetRec
        ( "list_is_empty"
        , EFun
            ( PVar "l"
            , EMatch
                ( EVar "l"
                , [ PConst CNil, EConst (CBool true); PWild, EConst (CBool false) ] ) ) )
    in
    w e |> run_infer
  in
  [%expect {| ('_2 list -> bool) |}]
;;
