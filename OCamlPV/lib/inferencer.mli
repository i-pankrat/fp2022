(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  | `Empty_pattern
  | `Empty_input
  | `Not_implemented of string
  ]

val pp_error : Format.formatter -> error -> unit
val check_types : Ast.expr list -> (Typedtree.ty, error) result
