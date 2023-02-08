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

val pp_ierror : Format.formatter -> ierror -> unit
val run : Ast.expr list -> (value, ierror) result