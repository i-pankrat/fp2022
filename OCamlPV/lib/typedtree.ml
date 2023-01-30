(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }]

module VarSetInit = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type ty =
  | Prim of string
  | Ty_var of binder
  | Arrow of ty * ty
  | List of ty
  | Tuple of ty list
[@@deriving show { with_path = false }]

let arrow l r = Arrow (l, r)
let int_typ = Prim "int"
let bool_typ = Prim "bool"
let v x = Ty_var x
let ( @-> ) = arrow