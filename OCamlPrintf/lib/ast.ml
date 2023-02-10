(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type pattern = PatVar of string [@@deriving variants, show { with_path = false }]

(** type for recursive flag in let *)
type rec_flag =
  | RecF
  | NRecF
[@@deriving variants, show { with_path = false }]

(** const expression type *)
type c_expr =
  | CInt of int
  | CBool of bool
  | CString of string
  | CUnit
[@@deriving variants, show { with_path = false }]

(* Types for printf parsing *)

(** hole type for substring in Printf *)
type hole =
  | HInt (** %i *)
  | HString (** %s *)
  | HQString (** %S -> Q means quoted*)
[@@deriving variants, show { with_path = false }]

(** substring in Printf *)
type str_item =
  | Const of string
  | Hole of hole
[@@deriving variants, show { with_path = false }]

(** string for printf function *)
type str = str_item list [@@deriving show { with_path = false }]

(** expression type *)
type expr =
  | EConst of c_expr
  | EVar of string
  | EApp of expr * expr (** App f x *)
  | EIfElse of expr * expr * expr (** if expr then expr else expr *)
  | ELam of pattern * expr (** fun pattern -> expr *)
  | ELet of (rec_flag * pattern * expr) * expr (** let [rec] pattern expr in expr *)
  | EPrintf of str
[@@deriving variants, show { with_path = false }]
