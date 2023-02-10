(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Errors
open Typedtree
module Format = Caml.Format (* silencing a warning *)

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
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
    | Result.Error x -> last, Result.Error x
    | Result.Ok a -> f a last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Result.Ok x -> st, Result.Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar name -> name = v
    | Arrow (l, _lab, r) -> occurs_in v l || occurs_in v r
    | TList t -> occurs_in v t
    | TBase _ -> false
  ;;

  let free_vars =
    let empty_set = Base.Set.empty (module Base.Int) in
    let rec helper (acc : (fresh, Base.Int.comparator_witness) Base.Set.t) = function
      | TVar name -> Base.Set.add acc name
      | Arrow (l, _lab, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TBase _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* An association map *)
  type t = (fresh, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping key value =
    if Type.occurs_in key value then fail (TypeError OccursCheck) else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Base.Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Base.Map.find_exn subst key
  let find key subst = Base.Map.find subst key
  let remove subst key = Base.Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar id ->
        (match find_exn id s with
         | exception Base.Not_found_s _ -> TVar id
         | x -> x)
      | TBase t -> TBase t
      | TList t -> TList (helper t)
      | Arrow (l, lab, r) -> Arrow (helper l, lab, helper r)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TBase l, TBase r when l == r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | Arrow (l1, _lab1, r1), Arrow (l2, _lab2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TList l, TList r -> unify l r
    | _ -> fail (TypeError (UnificationFailed (l, r)))

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Base.Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    Base.List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold_right f ini set =
    Base.Set.fold_right set ~init:ini ~f:(fun x acc ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | s, t -> (not (Base.Set.mem s v)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | s, t -> Base.Set.diff (Type.free_vars t) s
  ;;

  let apply sub (s, t) =
    let s2 = Base.Set.fold s ~init:sub ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (Parsetree.id, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.update env id ~f:(fun _ -> scheme)
  let empty = Base.Map.empty (module Base.String)

  let free_vars : t -> (type_num, Base.Int.comparator_witness) Base.Set.t =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.Int))
      ~f:(fun ~key:_ ~data acc -> Base.Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Base.Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n (* Generate new fresh variable *)

let instantiate : scheme -> typ R.t =
 fun (set, t) ->
  VarSet.fold_right
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return @@ Subst.apply s typ)
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env typ ->
  let free = Base.Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;

let lookup_env e map =
  match Base.Map.find map e with
  | None -> fail (TypeError (NoVariable e))
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

open Parsetree

let init_infer =
  let rec (helper : TypeEnv.t -> expr -> (Subst.t * typ) R.t) =
   fun env -> function
    | Const c ->
      (match c with
       | Bool _ -> return (Subst.empty, TBase TBool)
       | Int _ -> return (Subst.empty, TBase TInt)
       | Nil ->
         let* var_typ = fresh_var in
         return (Subst.empty, TList var_typ)
       | Unit -> return (Subst.empty, TBase TUnit))
    | Var name -> lookup_env name env
    | Binop (op, l, r) ->
      let* l_sub, l_typ = helper env l in
      let* r_sub, r_typ = helper env r in
      (match op with
       | Plus | Minus | Mult | Divide | Mod ->
         let* l_sub' = unify l_typ (TBase TInt) in
         let* r_sub' = unify r_typ (TBase TInt) in
         let* final_sub = Subst.compose_all [ l_sub'; r_sub'; l_sub; r_sub ] in
         return (final_sub, TBase TInt)
       | Eq | Neq | Lt | Ltq | Gt | Gtq ->
         let* sub = unify l_typ r_typ in
         let* final_sub = Subst.compose_all [ sub; l_sub; r_sub ] in
         return (final_sub, TBase TBool)
       | And | Or ->
         let* l_sub' = unify l_typ (TBase TBool) in
         let* r_sub' = unify r_typ (TBase TBool) in
         let* final_sub = Subst.compose_all [ l_sub'; r_sub'; l_sub; r_sub ] in
         return (final_sub, TBase TBool))
    | Fun (label, _default, name, exp) ->
      let* var_typ = fresh_var in
      let env = TypeEnv.extend env name (Base.Set.empty (module Base.Int), var_typ) in
      let* sub, t = helper env exp in
      let res_typ = Arrow (Subst.apply sub var_typ, label, t) in
      return (sub, res_typ)
    | Cons (hd, Const Nil) ->
      let* hd_sub, hd_typ = helper env hd in
      return (hd_sub, TList (Subst.apply hd_sub hd_typ))
    | Cons (hd, tl) ->
      let* hd_sub, hd_typ = helper env hd in
      let* tl_sub, tl_typ = helper env tl in
      let* sub = unify (TList hd_typ) tl_typ in
      let* final_sub = Subst.compose_all [ hd_sub; tl_sub; sub ] in
      return (final_sub, TList (Subst.apply final_sub hd_typ))
    | App (fu, label, arg) ->
      let* fu_sub, fu_typ = helper env fu in
      let* arg_sub, arg_typ = helper (TypeEnv.apply fu_sub env) arg in
      let* var_typ = fresh_var in
      let* unified_sub =
        unify (Arrow (arg_typ, label, var_typ)) (Subst.apply arg_sub fu_typ)
      in
      let res_typ = Subst.apply unified_sub var_typ in
      let* final_sub = Subst.compose_all [ fu_sub; arg_sub; unified_sub ] in
      return (final_sub, res_typ)
    | IfThenElse (cond, tbody, fbody) ->
      let* cond_sub, cond_typ = helper env cond in
      let* tbody_sub, tbody_typ = helper env tbody in
      let* fbody_sub, fbody_typ = helper env fbody in
      let* sub = unify cond_typ (TBase TBool) in
      let* sub' = unify tbody_typ fbody_typ in
      let* final_sub = Subst.compose_all [ cond_sub; tbody_sub; fbody_sub; sub; sub' ] in
      return (final_sub, Subst.apply final_sub tbody_typ)
    | Let (name, body, exp) ->
      let* body_sub, body_typ = helper env body in
      let env = TypeEnv.apply body_sub env in
      let exp_typ = snd (generalize env body_typ) in
      let* exp_sub, gen_typ =
        helper (TypeEnv.extend env name (Base.Set.empty (module Base.Int), exp_typ)) exp
      in
      let* final_sub = Subst.compose body_sub exp_sub in
      return (final_sub, gen_typ)
    | LetRec (name, body, exp) ->
      let* var_typ = fresh_var in
      let env = TypeEnv.extend env name (Base.Set.empty (module Base.Int), var_typ) in
      let* body_sub, body_typ = helper env body in
      let* exp_sub = unify (Subst.apply body_sub var_typ) body_typ in
      let* s = Subst.compose exp_sub body_sub in
      let env = TypeEnv.apply s env in
      let exp_typ = snd (generalize env (Subst.apply s var_typ)) in
      let* exp_sub, exp_typ =
        helper
          TypeEnv.(extend (apply s env) name (Base.Set.empty (module Base.Int), exp_typ))
          exp
      in
      let* final_sub = Subst.compose s exp_sub in
      return (final_sub, exp_typ)
  in
  helper
;;

let infer exp env = Caml.Result.map snd (run (init_infer env exp))
