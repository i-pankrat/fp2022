(** Copyright 2021-2022, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | CInt of int
  | CBool of bool
  | CNil
  | CString of string
  | CUnit
[@@deriving show { with_path = false }]

and bin_op =
  | Plus
  | Minus
  | Mult
  | Divide
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Gt
  | Lt
  | Gtq
  | Ltq
  | ConsConcat
[@@deriving show { with_path = false }]

and pattern =
  | PConst of const
  | PVar of id
  | PTuple of pattern list
  | PCons of pattern * pattern (* head and tail *)
  | PWild
[@@deriving show { with_path = false }]

and expr =
  | EConst of const
  | EIfThenElse of expr * expr * expr
  | ELet of id * expr * expr
  | ELetRec of id * expr * expr
  | EMatch of expr * (expr * expr) list
  | EBinOp of bin_op * expr * expr
  | EVar of id
  | EPatterns of pattern
  | EFun of expr * expr
  | EApply of expr * expr
[@@deriving show { with_path = false }]
