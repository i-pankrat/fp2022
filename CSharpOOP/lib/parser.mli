(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val apply : (char Opal.LazyStream.t -> ('a * 'b) option) -> string -> 'a option

module Statement : sig
  val statement_block :
    char Opal.LazyStream.t -> (Ast.statements * char Opal.LazyStream.t) option
end

module Object : sig
  val object_decl :
    char Opal.LazyStream.t -> (Ast.objects * char Opal.LazyStream.t) option
end

val parser :
  char Opal.LazyStream.t -> (Ast.objects list * char Opal.LazyStream.t) option
