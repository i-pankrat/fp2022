(** Copyright 2021-2022, ArtemKhel and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Formal syntax of Scheme *)
(* https://www.scheme.com/tspl2d/grammar.html *)
(* Quasiquotes *)
(* https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_9.html#SEC78 *)

type const =
  | Int of int
  | String of string
  | Bool of bool

and datum =
  | DConst of const
  | DList of datum list
  | DAbbr of prefix * datum

and prefix =
  | PQuote
  | PBackquote
  | PComma

and quasiquote =
  | QConst of const
  | QDatum of datum
  | QList of quasiquote list
  | QUnquote of expression

and id = string

and formals =
  | FormalList of id list (**  (<variable>* )   *)
  | Formal of id (** <variable>  *)

and expression =
  | Const of const
  | Var of id
  | Quote of datum
  | Quasiquote of quasiquote
  | Lambda of formals * definition list * expression list
      (** (lambda <formals> <definition>* <expression>+) *)
  | If of expression * expression * expression option
      (** (if <expression> <expression> <expression>?) *)
  | FuncCall of expression * expression list (** (<expression> <expression>* ) *)

(**  (define <variable> <expression>) *)
and definition = id * expression

and form =
  | Def of definition
  | Expr of expression

and program = form list [@@deriving show { with_path = false }]
