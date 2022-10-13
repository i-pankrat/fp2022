(** Copyright 2021-2022, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string

(** TODO: AST *)

type const =
  | CInt of int
  | CBool of bool
  | CNil
  | CString of string
  | CUnit

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

and pattern =
  | PConst of const
  | PVar of id
(* Pattern for lists, tuples ??? *)

(* We need onaly one arugents in functions, because we carrying
    For example: let sum x y = x + y
    Will translate to Let(sum, Var(x), Let("Carry", Var(y), Plus(Var(x), Var(y))))
       *)

and expr =
  | EIfThenElse of expr * expr * expr
  | ELet of id * expr * expr
  | ELetRec of id * expr * expr
  | EMatch of expr * (pattern * expr) list
  | EBinOp of bin_op * expr * expr
  | EVar of id
  | EFun of id * expr
  | EApply of expr * expr
(* Add "::" for Lists; add tuples *)
