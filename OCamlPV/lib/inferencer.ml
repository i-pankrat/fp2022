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
  ]

let rec pp_typ ppf =
  let open Format in
  function
  | Ty_var n -> fprintf ppf "'_%d" n
  | Prim s -> pp_print_string ppf s
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | List t -> fprintf ppf "%a list" pp_typ t
  | Tuple ts ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf " * ")
         (fun ppf ty -> fprintf ppf "%a" pp_typ ty))
      ts
;;

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_typ l pp_typ r
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
      fprintf ppf "'_%d -> %a@\n" key pp_typ data)
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
    | Tuple a, b | b, Tuple a -> failwith "TODO: unify tuple"
    | _ -> fail (`Unification_failed (l, r))

  and extend key value extensible_map =
    match Map.Poly.find extensible_map key with
    | None ->
      let v = apply extensible_map value in
      let* s2 = singleton key v in
      fold_left extensible_map ~init:(return s2) ~f:(fun key value acc ->
        let v = apply s2 value in
        let* mapk, mapv = mapping key v in
        match Map.Poly.add acc ~key:mapk ~data:mapv with
        | `Ok map -> return map
        | `Duplicate -> return acc)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose extensible_map s2

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
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
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
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

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
 fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
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
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * ty) R.t) =
   fun env -> function
    | EConst const ->
      let prim =
        match const with
        | CBool _ -> "bool"
        | CInt _ -> "int"
        | CString _ -> "string"
        | CUnit -> "unit"
        | CNil -> failwith "TODO"
      in
      return (Subst.empty, Prim prim)
    | EBinOp op ->
      (match op with
       | Plus | Minus | Divide | Mult | Mod ->
         return (Subst.empty, arrow int_typ (arrow int_typ int_typ))
       | And | Or -> return (Subst.empty, arrow bool_typ (arrow bool_typ bool_typ))
       | Eq | Neq | Gt | Lt | Gtq | Ltq ->
         let* var = fresh_var in
         return (Subst.empty, arrow var (arrow var bool_typ))
       | ConsConcat -> failwith "TODO")
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
    | ELet (name, e1, e2) ->
      (match e1 with
       | EVar x ->
         let* tv = fresh_var in
         let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
         let* s, ty = helper env2 e2 in
         let typedres = Arrow (Subst.apply s tv, ty) in
         return (s, typedres)
       | _ as e1 ->
         let* s1, t1 = helper env e1 in
         let env2 = TypeEnv.apply s1 env in
         let t1 = generalize env2 t1 in
         let* s2, t3 = helper (TypeEnv.extend env2 (name, t1)) e2 in
         let* final_subst = Subst.compose s1 s2 in
         return (final_subst, t3))
    | EVar x -> lookup_env x env
    | _ -> failwith "TODO"
  in
  helper
;;

let w e = Result.map (run (infer TypeEnv.empty e)) ~f:snd

(* Unification tests *)
let unify = Subst.unify

let run_subst subst =
  match R.run subst with
  | Result.Error _ -> Format.printf "Error%!"
  | Ok subst -> Format.printf "%a%!" Subst.pp subst
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 1) (int_typ @-> v 2) |> run_subst in
  [%expect {|
    '_1 -> int
    '_2 -> int |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 1) ((v 2 @-> int_typ) @-> int_typ @-> int_typ) |> run_subst in
  [%expect {|
    '_1 -> (int -> int)
    '_2 -> int |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 2) (v 2 @-> v 3) |> run_subst in
  [%expect {|
    '_1 -> '_3
    '_2 -> '_3 |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> bool_typ) (v 2 @-> int_typ) |> run_subst in
  [%expect {| Error |}]
;;

(* Infer tests *)

let run_infer = function
  | Result.Error _ -> Format.printf "Error%!"
  | Result.Ok typed -> Format.printf "%a%!" pp_typ typed
;;

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

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EIfThenElse (EConst (CBool true), EConst (CInt 4), EConst (CInt 5)) in
    w e |> run_infer
  in
  [%expect {| int |}]
;;

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
  [%expect {| (Prim "bool") |}]
;;

(* let  x = 5 * 5 in 2 * x *)
let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      ELet
        ( "x"
        , EApply (EApply (EBinOp Mult, EConst (CInt 5)), EConst (CInt 5))
        , EApply (EApply (EBinOp Mult, EConst (CInt 2)), EVar "x") )
    in
    w e |> run_infer
  in
  [%expect {| (Prim "int") |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      (* let inc x = x + 1 *)
      ELet ("inc", EVar "x", EApply (EApply (EBinOp Plus, EVar "x"), EConst (CInt 1)))
    in
    w e |> run_infer
  in
  [%expect {| (Arrow ((Prim "int"), (Prim "int"))) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      (* let funny x = x = "some_str" *)
      ELet
        ( "funny"
        , EVar "x"
        , EApply (EApply (EBinOp Eq, EVar "x"), EConst (CString "some_str")) )
    in
    w e |> run_infer
  in
  [%expect {| (Arrow ((Prim "string"), (Prim "bool"))) |}]
;;
