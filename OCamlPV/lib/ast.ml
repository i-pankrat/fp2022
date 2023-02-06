(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type const =
  | CInt of int
  | CBool of bool
  | CNil
  | CString of string
  | CUnit
[@@deriving eq, show { with_path = false }]

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
  | PPolyVariant of id * pattern list
      (** Polymorphic variants. Number of the elements in the list
          represents the number of arguments which contructor takes*)
[@@deriving eq, show { with_path = false }]

and pvtype =
  | TTuple of pvtype list
  | TList of pvtype
  | TType of id
  | TAny of id
  | TInt
  | TString
  | TBool
  | TNoType
[@@deriving show { with_path = false }]

and expr =
  | EConst of const
  | EIfThenElse of expr * expr * expr
  | ELet of id * expr
  | ELetIn of id * expr * expr
  | ELetRec of id * expr
  | ELetRecIn of id * expr * expr
  | EMatch of expr * (pattern * expr) list
  | EBinOp of bin_op
  | EVar of id
  | EFun of pattern * expr
  | EApply of expr * expr
  | EList of expr * expr
  | ETuple of expr list
  | EPolyVariant of id * expr list (** Polymorphic variants *)
  | EType of id * pvtype
      (** type id = ... It is possible to declare only polymorphic variants. *)
[@@deriving show { with_path = false }]

and statements = expr list [@@deriving show { with_path = false }]
