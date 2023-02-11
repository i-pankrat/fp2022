(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type modifiers =
  | Public  (** Public modifier *)
  | Private  (** Private modifier *)
  | Protected  (** Protected modifier *)
  | Static  (** Static modifier *)
  | Const  (** Const modifier *)
  | Virtual  (** Virtual modifier *)
  | Override  (** Override modifier *)
  | Abstract  (** Abstract modifier *)
  | New  (** New modifier *)

val pp_modifiers : Ppx_show_runtime.Format.formatter -> modifiers -> unit
val show_modifiers : modifiers -> string

type types =
  | TInt  (** Integer type of element (method/variable) *)
  | TVoid  (** Void type of element (method/variable) *)
  | TString  (** String type of element (method/variable) *)
  | TRef of string
      (** Reference (with string name of type) type of element (method/variable) *)
  | TBool  (** Bool type of element (method/variable) *)

val pp_types : Ppx_show_runtime.Format.formatter -> types -> unit
val show_types : types -> string

type values =
  | VInt of int  (** Integer value type of calculation (expression/variable) *)
  | VBool of bool  (** Bool value type of calculation (expression/variable) *)
  | VString of string
      (** String value type of calculation (expression/variable) *)
  | VVoid  (** Void value type of calculation (expression/variable) *)
  | VObjectReference of object_references
      (** Reference value type of calculation (expression/variable) *)

and field_references = {
  key : string;
  field_type : types;
  field_value : values;
  is_const : bool;
  assignments_count : int;
}

and object_references =
  | NullObjectReference  (** Null reference *)
  | ObjectReference of {
      class_key : string;
      ext_interface : string option;
      field_references_table : field_references KeyMap.KeyMap.t;
      number : int;
    }
      (** Not null reference with reference type name, access interface, fields table and creation number *)

val pp_values : Ppx_show_runtime.Format.formatter -> values -> unit

val pp_field_references :
  Ppx_show_runtime.Format.formatter -> field_references -> unit

val pp_object_references :
  Ppx_show_runtime.Format.formatter -> object_references -> unit

val show_values : values -> string
val show_field_references : field_references -> string
val show_object_references : object_references -> string

type names =
  | Name of string
      (** Name of instanse (e.g. [MyClass], [someVar], [MyFunc]) *)

val pp_names : Ppx_show_runtime.Format.formatter -> names -> unit
val show_names : names -> string

type expressions =
  | Add of expressions * expressions  (** Sum expression ([expr_1 + expr_2]) *)
  | Sub of expressions * expressions
      (** Substraction expression ([expr_1 - expr_2]) *)
  | Mult of expressions * expressions
      (** Multiplication expression ([expr_1 * expr_2]) *)
  | Div of expressions * expressions
      (** Division expression ([expr_1 / expr_2]) *)
  | PostInc of expressions
      (** Post increment (equivalent to [v = v + 1]) expression ([expr_1++]) *)
  | PostDec of expressions
      (** Post decrement (equivalent to [v = v - 1]) expression ([expr_1--]) *)
  | PrefInc of expressions
      (** Prefix increment (equivalent to [v = v + 1]) expression ([++expr_1]) *)
  | PrefDec of expressions
      (** Prefix decrement (equivalent to [v = v - 1]) expression ([--expr_1]) *)
  | Mod of expressions * expressions
      (** Modulo expression ([expr_1 % expr_2]) *)
  | And of expressions * expressions
      (** Logic [and] expression ([expr_1 && expr_2]) *)
  | Or of expressions * expressions
      (** Logic [or] expression ([expr_1 || expr_2]) *)
  | Not of expressions  (** Logic [not] expression ([!expr_1]) *)
  | Equal of expressions * expressions
      (** Equal expression ([expr_1 == expr_2]) *)
  | NotEqual of expressions * expressions
      (** Not equal expression ([expr_1 != expr_2]) *)
  | Less of expressions * expressions
      (** Less expression ([expr_1 < expr_2]) *)
  | More of expressions * expressions
      (** More expression ([expr_1 > expr_2]) *)
  | LessOrEqual of expressions * expressions
      (** Less or equal expression ([expr_1 <= expr_2]) *)
  | MoreOrEqual of expressions * expressions
      (** More or equal expression ([expr_1 >= expr_2]) *)
  | This  (** This token for access to current class fields/methods *)
  | Base  (** This token for access to parent class fields/methods *)
  | Null  (** Null expression (e.g. [SomeClass a = null]) *)
  | Value of values  (** Value expression (e.g. [int a = 1]) *)
  | Identifier of string  (** Identifier of variable (e.g. [a = b]) *)
  | ClassCreation of names * expressions list
      (** Class creation (e.g. [SomeClass a = new SomeClass(1, 'a')]) *)
  | CallMethod of expressions * expressions list
      (** Call method (e.g. [Func(1, 'a')]) *)
  | AccessByPoint of expressions * expressions
      (** Access by point to class member (e.g. [a.F()]) *)
  | Assign of expressions * expressions
      (** Assign to variable (e.g. [a = b]) *)
  | Cast of types * expressions  (** Cast expression (e.g. [(A)b]) *)

and statements =
  | Expression of expressions
      (** Expression as statement (e.g. [a = b], [a--], [a.F()]) *)
  | StatementBlock of statements list
      (** Block of statements (e.g. [{s_1; s_2;}]) *)
  | If of expressions * statements * statements option
      (** [if-then] or [if-then-else] statement *)
  | While of expressions * statements
      (** [while (bool) StatementBlock] statement *)
  | For of
      statements option * expressions option * expressions list * statements
      (** For statement (e.g. [for (int i = 0; i < n; i++) something]) *)
  | Break  (** [break] statement *)
  | Continue  (** [continue] statement *)
  | Return of expressions option  (** [return] statement (e.g. [return 1])*)
  | VariableDecl of modifiers option * types * (names * expressions option) list
      (** Declaration of variable statement. Only [const] or without modifier *)
  | Print of expressions  (** Print statement (e.g. [Console.WriteLine(1)])*)

and fields =
  | Field of types * (names * expressions option) list  (** Fields of object *)
  | Method of types * names * (types * names) list * statements option
      (** Methods of object *)
  | Constructor of
      names * (types * names) list * expressions option * statements
      (** Constructors of object *)

and objects =
  | Class of
      modifiers list * names * names list * (modifiers list * fields) list
      (** Class *)
  | Interface of modifiers * names * names list * (modifiers list * fields) list
      (** Interface *)

val pp_expressions : Ppx_show_runtime.Format.formatter -> expressions -> unit
val pp_statements : Ppx_show_runtime.Format.formatter -> statements -> unit
val pp_fields : Ppx_show_runtime.Format.formatter -> fields -> unit
val pp_objects : Ppx_show_runtime.Format.formatter -> objects -> unit
val show_expressions : expressions -> string
val show_statements : statements -> string
val show_fields : fields -> string
val show_objects : objects -> string

type signal =
  | WasBreak  (** Is there break signal (e.g. in loop) *)
  | WasContinue  (** Is there continue signal (e.g. in loop) *)
  | WasReturn  (** Is there return signal (e.g. in loop) *)
  | NoSignal  (** Is there no signal *)

val pp_signal : Ppx_show_runtime.Format.formatter -> signal -> unit
val show_signal : signal -> string

type table_constructor = {
  key : string;
  arguments : (types * names) list;
  call_constructor : expressions option;
  body : statements;
}

val pp_table_constructor :
  Ppx_show_runtime.Format.formatter -> table_constructor -> unit

val show_table_constructor : table_constructor -> string

type table_field = {
  field_type : types;
  field_modifiers : modifiers list;
  key : string;
  sub_tree : expressions option;
}

val pp_table_field : Ppx_show_runtime.Format.formatter -> table_field -> unit
val show_table_field : table_field -> string

type table_method = {
  method_type : types;
  method_modifiers : modifiers list;
  arguments : (types * names) list;
  key : string;
  interface_key : string option;
  body : statements option;
  is_overriden : bool;
}

val pp_table_method : Ppx_show_runtime.Format.formatter -> table_method -> unit
val show_table_method : table_method -> string

type table_class = {
  this_key : string;
  fields_table : table_field KeyMap.KeyMap.t;
  methods_table : table_method KeyMap.KeyMap.t;
  constructors_table : table_constructor KeyMap.KeyMap.t;
  children_keys : string list;
  is_abstract : bool;
  is_static : bool;
  is_class : bool;
  parent_key : string list;
  decl_tree : objects;
}

val pp_table_class : Ppx_show_runtime.Format.formatter -> table_class -> unit
val show_table_class : table_class -> string
