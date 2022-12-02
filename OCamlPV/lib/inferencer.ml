open Base

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | Prim of string
  | Ty_var of binder
  | Arrow of ty * ty
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]
(* binder_set is context; *)

let arrow l r = Arrow (l, r)
let int_typ = Prim "int"
let bool_typ = Prim "bool"
let v x = Ty_var x
let ( @-> ) = arrow
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

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) -> failwith "TODO"
;;

let rec pp_typ ppf = function
  | Ty_var n -> Format.fprintf ppf "'_%d" n
  | Prim s -> Format.pp_print_string ppf s
  | Arrow (l, r) -> Format.fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
;;

module R : sig
  include Monad.Infix (*include Monad.Infix with type 'a t := 'a t*)

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
    | Prim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Ty_var b -> VarSet.add b acc
      | Arrow (l, r) -> helper (helper acc l) r
      | Prim _ -> acc
    in
    helper VarSet.empty
  ;;
end

let fold_left map ~init ~f =
  Map.Poly.fold map ~init ~f:(fun ~key ~data acc ->
    let open R.Syntax in
    let* acc = acc in
    f key data acc)
;;

module Subst : sig
  (* Substitution*)
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
    Map.Poly.iteri subst ~f:(fun ~key ~data ->
      Format.fprintf ppf "%d -> %s@\n" key (show_ty data))
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

  let apply s =
    let rec helper = function
      | Ty_var b as ty ->
        (match find_exn b s with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Arrow (l, r) -> Arrow (helper l, helper r)
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

  let compose_all = failwith "TODO"
end

(* Unification tests *)

let unify = Subst.unify

let run_subst subst =
  match R.run subst with
  | Result.Error _ -> Format.printf "Error%!"
  | Ok subst -> Format.printf "%a%!" Subst.pp subst
;;

let%expect_test _ =
  let g = unify (v 1 @-> v 1) (int_typ @-> v 2) in
  let _ =
    g
    |> R.run
    |> fun res ->
    match res with
    | Result.Error _ -> Format.printf "Error%!"
    | Ok subst -> Format.printf "%a%!" Subst.pp subst
  in
  [%expect {| [ 1 -> int, 2 -> int ] |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 1) ((v 2 @-> int_typ) @-> int_typ @-> int_typ) |> run_subst in
  [%expect {| [ 1 -> (int -> int), 2 -> int ] |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 2) (v 2 @-> v 3) |> run_subst in
  [%expect {| [ 1 -> '_3, 2 -> '_3 ] |}]
;;
