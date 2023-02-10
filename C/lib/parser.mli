(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

val parse : string -> (Ast.function_list, error) result
(** Main entry of parser *)

type dispatch = {
  func_call : dispatch -> Ast.expression Angstrom.t;
      (** Parser for function call *)
  arithmetical : dispatch -> string -> Ast.expression Angstrom.t;
      (** Parser for arithmetic operations *)
  logical : dispatch -> string -> Ast.expression Angstrom.t;
      (** Parser for logic operations without && and || operators *)
  parse_and : dispatch -> string -> Ast.expression Angstrom.t;
      (** Parser for logic operations with && operator *)
  logical_sequence : dispatch -> string -> Ast.expression Angstrom.t;
      (** Parser for logic operations with && and || operators *)
  bracket : dispatch -> Ast.expression Angstrom.t;
      (** Parser for operations in brackets *)
  bracket_singles : dispatch -> string -> Ast.expression Angstrom.t;
  all_singles : dispatch -> string -> Ast.expression Angstrom.t;
  all_ops : dispatch -> string -> Ast.expression Angstrom.t;
  simple_commands : dispatch -> string -> Ast.expression Angstrom.t;
      (** Parser for simple C commands like definition, assign, function call. *)
  statement : dispatch -> Ast.statement Angstrom.t;
      (** Parser for C constructions like simple command, commands/constructions block, if, else if, else, for, while *)
  func_def : dispatch -> Ast.function_list Angstrom.t;
      (** Parser for function definition *)
  statement_block : dispatch -> Ast.statement Angstrom.t;
      (** Parser for C commands/constructions block *)
}

(* A collection of miniparsers *)
val parse_c : dispatch
