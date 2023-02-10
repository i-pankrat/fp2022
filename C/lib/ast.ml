(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type ctype = TVoid | TChar | TInt8 | TInt16 | TInt32 | TPointer of ctype
[@@deriving show { with_path = false }]

type const = VInt of Int32.t | VChar of char | VString of string
[@@deriving show { with_path = false }]

type expression =
  | Pointer of expression
  | Address of expression
  | Add of expression * expression
  | Sub of expression * expression
  | Inc of expression
  | Dec of expression
  | UnaryMin of expression
  | UnaryPlus of expression
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | Equal of expression * expression
  | NotEqual of expression * expression
  | Less of expression * expression
  | LessOrEq of expression * expression
  | More of expression * expression
  | MoreOrEq of expression * expression
  | Assign of expression * expression
  | Define of ctype * expression * expression option
  | DefineSeq of expression list
  | FuncCall of name * expression list
  | Cast of ctype * expression
  | Variable of name
  | Value of const
  | Array of ctype * name * expression option * expression list option
  | ArrayElem of name * expression
[@@deriving show { with_path = false }]

and statement =
  | Expression of expression
  | StatementsBlock of statement list
  | If of expression * statement
  | IfSeq of statement list * statement option
  | While of expression * statement
  | For of expression option * expression option * expression option * statement
  | Break
  | Continue
  | Return of expression
[@@deriving show { with_path = false }]

and program_function = {
  function_type : ctype;
  function_name : name;
  function_arguments : (ctype * name) list;
  function_body : statement option;
}
[@@deriving show { with_path = false }]

and function_list = program_function list
[@@deriving show { with_path = false }]
