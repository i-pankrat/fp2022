(** Copyright 2021-2022, ArtemKhel and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Interpreter : sig
  type env = value Base.Map.M(Base.String).t

  and value =
    | None
    | VBool of bool
    | VInt of int
    | VList of value list
    | VString of string
    | VLambda of Ast.formals * Ast.definition list * Ast.expression list * env
    | VExpr of expression
    | VExprs of expression list

  val pp_value : Format.formatter -> value -> unit
  val show_value : value -> string
  val eval_program : env -> Ast.form list -> (value * env, string) result
end
