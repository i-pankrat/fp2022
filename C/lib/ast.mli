(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string

type ctype =
  | TVoid  (** void type in C *)
  | TChar  (** char type in C *)
  | TInt8  (** int_8t type in C *)
  | TInt16  (** int_16t type in C *)
  | TInt32  (** int_32t or int type in C *)
  | TPointer of ctype  (** pointer type in C (int**, char*, ...) *)
[@@deriving show { with_path = false }]

type const =
  | VInt of Int32.t  (** int number *)
  | VChar of char  (** one character (for example, 'c') *)
  | VString of string  (** string const (for example, "Hello") *)
[@@deriving show { with_path = false }]

type expression =
  | Pointer of expression  (** var* *)
  | Address of expression  (** &var *)
  | Add of expression * expression  (** a + b *)
  | Sub of expression * expression  (** a - b *)
  | Inc of expression  (** var++ *)
  | Dec of expression  (** var-- *)
  | UnaryMin of expression  (** -(expr) or -var *)
  | UnaryPlus of expression  (** +(expr) or +var *)
  | Mul of expression * expression  (** a * b *)
  | Div of expression * expression  (** a / b *)
  | Mod of expression * expression  (** a % b *)
  | And of expression * expression  (** a && b *)
  | Or of expression * expression  (** a || b *)
  | Equal of expression * expression  (** a == b *)
  | NotEqual of expression * expression  (** a != b *)
  | Less of expression * expression  (** a < b *)
  | LessOrEq of expression * expression  (** a <= b *)
  | More of expression * expression  (** a > b *)
  | MoreOrEq of expression * expression  (** a >= b *)
  | Assign of expression * expression  (** a = b *)
  | Define of ctype * expression * expression option  (** type var (= expr) *)
  | DefineSeq of expression list  (** type a, b = expr, c; *)
  | FuncCall of name * expression list  (** Function (a, b, c) *)
  | Cast of ctype * expression  (** (type)(expr) *)
  | Variable of name  (** var *)
  | Value of const  (** value (const) *)
  | Array of ctype * name * expression option * expression list option
      (** type arr = {expr1, expr2, expr3} *)
  | ArrayElem of name * expression  (** arr[index] *)
[@@deriving show { with_path = false }]

and statement =
  | Expression of expression
      (** Simple command like Function call, assign or define *)
  | StatementsBlock of statement list
      (** list of statements in {} splitted by ";" or by other statement *)
  | If of expression * statement  (** if (condition) {statements list} *)
  | IfSeq of statement list * statement option
      (** if {...} else if {...} else {...} in list *)
  | While of expression * statement  (** while (condition) {...} *)
  | For of expression option * expression option * expression option * statement
      (** for (initial expr; condition; loop expr) {...} *)
  | Break  (** break; *)
  | Continue  (** continue; *)
  | Return of expression  (** return expr; *)
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
