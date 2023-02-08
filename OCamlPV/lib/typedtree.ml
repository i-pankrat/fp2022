(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving eq, show { with_path = false }]

module VarSetInit = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type id = string [@@deriving eq, show { with_path = false }]

type pv = id * ty list [@@deriving eq, show { with_path = false }]

and ty =
  | Prim of string
  | Ty_var of binder
  | Arrow of ty * ty
  | List of ty
  | Tuple of ty list
  | MoreTags of binder * pv list
  | LessTags of binder * pv list
    (* I have come up to the conclusion that the simplest way to deal
    with polymorphic variant is to add binder to identify them.
    It's seems to me like a bad way but another are much worse... *)
[@@deriving show { with_path = false }]

let arrow l r = Arrow (l, r)
let int_typ = Prim "int"
let bool_typ = Prim "bool"
let var_typ x = Ty_var x
let list_typ x = List x
let tuple_typ x = Tuple x
let moretags_typ b pv = MoreTags (b, pv)
let lesstags_typ b pv = LessTags (b, pv)
let ( @-> ) = arrow
