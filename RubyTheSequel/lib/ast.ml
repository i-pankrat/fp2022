(** Copyright 2022-2023, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Literal values *)
type literal =
  | IntLit of int
  | StringLit of string
  | BoolLit of bool
  | NilLit
[@@deriving variants, show { with_path = false }]

(** Binary operations *)
type binop =
  | AND
  | OR
  | MULT
  | DIV
  | MOD
  | ADD
  | SUB
  | EQ
  | NEQ
  | GEQ
  | LEQ
  | LSS
  | GTR
[@@deriving variants, show { with_path = false }]

type 'a invoke =
  { invoker : 'a
  ; args : 'a list
  }
[@@deriving show { with_path = false }]

type compound =
  | Literal of literal (** literal *)
  | Var of string (** string *)
  | Assn of string * compound (** name = compound *)
  | IfElse of compound * compound * compound
      (** if compound then compound [else compound | elif compound] *)
  | Binop of binop * compound * compound (** compound binop compound *)
  | WhileCompound of compound * compound (** while compound do compound end *)
  | UntilCompound of compound * compound (** until compound do compound end *)
  | SeqCompound of compound list (** compound [[;] | [\n]] compound *)
  | Arr of compound list (** [compound [[, compound]...]] *)
  | Invoke of compound invoke (** comopund.([compound [[, compound]...]]) *)
  | MethodCall of compound * (string * compound list)
      (** compound.string([compound [[, compound]...]]) *)
  | LambdaFn of string list * compound
      (** lambda { [|string [[, string]...] compound |] } *)
  | MethodFn of string * (string list * compound)
  | ClassDecl of string * compound list
[@@deriving variants, show { with_path = false }]

type t = compound [@@deriving show { with_path = false }]
