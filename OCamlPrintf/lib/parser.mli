(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : string -> (Ast.expr, string) result
val is_op : string -> bool
