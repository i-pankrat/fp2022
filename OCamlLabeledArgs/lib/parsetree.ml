(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Bool of bool
  | Int of int
  | Nil
  | Unit
[@@deriving show { with_path = false }]

type bin_op =
  | Plus
  | Minus
  | Mult
  | Divide
  | Mod
  | Eq
  | Neq
  | Lt
  | Ltq
  | Gt
  | Gtq
  | And
  | Or
[@@deriving show { with_path = false }]

type arg_label =
  | ArgNoLabel
  | ArgLabeled of id
  | ArgOptional of id
[@@deriving show { with_path = false }]

type expr =
  | Const of const
  | Var of id
  | Binop of bin_op * expr * expr
  | Fun of arg_label * expr option * id * expr
  | Cons of expr * expr
  | App of expr * arg_label * expr
  | IfThenElse of expr * expr * expr
  | Let of id * expr * expr
  | LetRec of id * expr * expr
[@@deriving show { with_path = false }]

type definition = id * expr [@@deriving show { with_path = false }]

type command =
  | Help
  | Quit
  | Use of string

type toplevel =
  | Definition of definition
  | Expression of expr
  | Command of command
