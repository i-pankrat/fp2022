(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a binop =
  | EmptyBinOp of Ast.bin_op
  | PartialBinOp of 'a * Ast.bin_op

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VTuple of value list
  | VList of value list
  | VFun of Ast.pattern * Ast.expr * (Ast.id * value) list
  | VBinOp of value binop
  | VPolyVariant of Ast.id * value list
  | VUnit
  | VNil

type ierror
type environment

val pp_ierror : Format.formatter -> ierror -> unit
val empty : environment
val run : ?env:environment -> Ast.expr list -> (environment * value, ierror) result