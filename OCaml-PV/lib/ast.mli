(** Copyright 2021-2022, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string

(** TODO: AST *)

type const =
  | Int of int
  | Bool of bool
  | Nil
  | String of string
  | Unit

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
(* Pattern for lists ??? *)

(* We need onaly one arugents in functions, because we carrying
    For example: let sum x y = x + y
    Will translate to Let(sum, Var(x), Let("Carry", Var(y), Plus(Var(x), Var(y))))
       *)

and expr =
  | IfThenElse of expr * expr * expr
  | Let of id * expr * expr
  | LetRec of id * expr * expr
  | Match of expr * (pattern * expr) list
  | BinOp of bin_op * expr * expr
  | Var of id
  | Fun of id * expr
  | Apply of expr * expr
(* Add "::" for Lists? *)
