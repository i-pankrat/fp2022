(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The parsed AST representation of our mini language. *)

(** Parse tree. *)

(** [id] is a built-in string type for variable names. *)
type id = string

(** [const] represents the constants in the language. *)
type const =
  | Bool of bool (** [true] or [false] *)
  | Int of int (** integer literal *)
  | Nil (** representation of empty list [[]] *)
  | Unit (** [()] *)

(** [bin_op] enumerates all available binary operators. *)
type bin_op =
  | Plus (** [+] operator *)
  | Minus (** [-] operator *)
  | Mult (** [*] operator *)
  | Divide (** [/] operator *)
  | Mod (** [mod] (remainder) operator *)
  | Eq (** [=] operator *)
  | Neq (** [<>] operator *)
  | Lt (** [<] operator *)
  | Ltq (** [<=] operator *)
  | Gt (** [>] operator *)
  | Gtq (** [>=] operator *)
  | And (** [&&] operator *)
  | Or (** [||] operator *)

(** [arg_label] type represents function argument labels. *)
type arg_label =
  | ArgNoLabel (**[T -> ...] *)
  | ArgLabeled of id (** [label:T -> ...] *)
  | ArgOptional of id (** [?label:T -> ...] *)

(** [expr] type represents expressions in the language. *)
type expr =
  | Const of const (** Constant expressions *)
  | Var of id (** Variable names expressions *)
  | Binop of bin_op * expr * expr (** Expressions involving binary operations *)
  (* For anonymous functions "fun a b -> e" is syntactic sugar, 
     which parser has to resolve to "Fun ("x", Fun ("y", e))" *)
  | Fun of arg_label * expr option * id * expr (** Anonymous functions *)
  | Cons of expr * expr (** [Cons] is the representation of the h::t list *)
  | App of expr * arg_label * expr (** Function application *)
  | IfThenElse of expr * expr * expr (** Conditional operator if-then-else *)
  (* Expressions like "let f x = x" are handled by parser,
     which produces definition Let (Var "f", Fun ("x", Var "x")) *)
  | Let of id * expr * expr (** Expression for "let x = e1 in e2" *)
  | LetRec of id * expr * expr (** Fixpointed Let *)

(** [definition] type represents definition in the language. *)
type definition = id * expr [@@deriving show { with_path = false }]

(** Top-level tree. *)

(** [command] type is available commands at toplevel input. *)
type command =
  | Help (** [#help] command prints a list of all available commands *)
  | Quit (** [#quit] command exits the toplevel loop and terminates this program *)
  | Use of string
      (** [#use <file>] command reads and evaluates source phrases from the given file *)

(** [toplevel] type represents all the possible inputs to the REPL. *)
type toplevel =
  | Definition of definition
      (** [Definition] is for representing [let] expressions in top-level input *)
  | Expression of expr
      (** [Expression] is for representing [expr] type in top-level input *)
  | Command of command
      (** [Command] is for representing available [REPL] commands in top-level input *)
