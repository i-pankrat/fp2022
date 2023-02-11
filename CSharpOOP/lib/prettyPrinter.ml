(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let sep_by_space formatter () = fprintf formatter " "
let sep_by_comma formatter () = fprintf formatter ", "
let sep_by_endl formatter () = fprintf formatter "@;<0 0>"
let sep_by_two_endl formatter () = fprintf formatter "@;<0 0>@;<0 0>"

let print_value formatter = function
  | VBool b -> fprintf formatter "%b" b
  | VInt i -> fprintf formatter "%d" i
  | VString s -> fprintf formatter "\"%s\"" s
  | _ -> ()

let print_type formatter = function
  | TInt -> fprintf formatter "int"
  | TBool -> fprintf formatter "bool"
  | TVoid -> fprintf formatter "void"
  | TString -> fprintf formatter "string"
  | TRef s -> fprintf formatter "%s" s

let print_modifier formatter = function
  | Public -> fprintf formatter "%s" "public"
  | Private -> fprintf formatter "%s" "private"
  | Protected -> fprintf formatter "%s" "protected"
  | Static -> fprintf formatter "%s" "static"
  | Const -> fprintf formatter "%s" "const"
  | Abstract -> fprintf formatter "%s" "abstract"
  | Override -> fprintf formatter "%s" "override"
  | Virtual -> fprintf formatter "%s" "virtual"
  | New -> fprintf formatter "%s" "new"

let print_mods_list formatter =
  pp_print_list ~pp_sep:sep_by_space print_modifier formatter

let print_name formatter = function Name n -> fprintf formatter "%s" n

let print_name_list formatter =
  pp_print_list ~pp_sep:sep_by_space print_name formatter

let prev_is_add = function Add (_, _) | Sub (_, _) -> true | _ -> false
let prev_is_or = function Or (_, _) -> true | _ -> false
let prev_is_id = function Value _ | Identifier _ -> true | _ -> false
let prev_is_create = function ClassCreation (_, _) -> true | _ -> false

let rec print_expr formatter = function
  | Add (left, right) ->
      fprintf formatter "%a + %a" print_expr left print_expr right
  | Sub (left, right) ->
      fprintf formatter "%a - %a" print_expr left print_expr right
  | Mult (left, right) ->
      if prev_is_add left then (
        fprintf formatter "%a * " print_expr_in_parens left;
        if prev_is_add right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
      else (
        fprintf formatter "%a * " print_expr left;
        if prev_is_add right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
  | Div (left, right) ->
      if prev_is_add left then (
        fprintf formatter "%a / " print_expr_in_parens left;
        if prev_is_add right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
      else (
        fprintf formatter "%a / " print_expr left;
        if prev_is_add right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
  | Mod (left, right) ->
      if prev_is_add left then (
        fprintf formatter "%a %% " print_expr_in_parens left;
        if prev_is_add right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
      else (
        fprintf formatter "%a %% " print_expr left;
        if prev_is_add right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
  | And (left, right) ->
      if prev_is_or left then (
        fprintf formatter "%a && " print_expr_in_parens left;
        if prev_is_or right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
      else (
        fprintf formatter "%a && " print_expr left;
        if prev_is_or right then
          fprintf formatter "%a" print_expr_in_parens right
        else fprintf formatter "%a" print_expr right)
  | Or (left, right) ->
      fprintf formatter "%a || %a" print_expr left print_expr right
  | Not operand ->
      if prev_is_id operand then fprintf formatter "!%a" print_expr operand
      else fprintf formatter "!%a" print_expr_in_parens operand
  | Less (left, right) ->
      fprintf formatter "%a < %a" print_expr left print_expr right
  | More (left, right) ->
      fprintf formatter "%a > %a" print_expr left print_expr right
  | LessOrEqual (left, right) ->
      fprintf formatter "%a <= %a" print_expr left print_expr right
  | MoreOrEqual (left, right) ->
      fprintf formatter "%a >= %a" print_expr left print_expr right
  | Equal (left, right) ->
      fprintf formatter "%a == %a" print_expr left print_expr right
  | NotEqual (left, right) ->
      fprintf formatter "%a != %a" print_expr left print_expr right
  | PostInc operand -> fprintf formatter "%a++" print_expr operand
  | PrefInc operand -> fprintf formatter "++%a" print_expr operand
  | PostDec operand -> fprintf formatter "%a--" print_expr operand
  | PrefDec operand -> fprintf formatter "--%a" print_expr operand
  | Null -> fprintf formatter "null"
  | This -> fprintf formatter "this"
  | Base -> fprintf formatter "base"
  | Value value -> fprintf formatter "%a" print_value value
  | Identifier id -> fprintf formatter "%s" id
  | AccessByPoint (left, right) ->
      if prev_is_create left then
        fprintf formatter "%a.%a" print_expr_in_parens left print_expr right
      else fprintf formatter "%a.%a" print_expr left print_expr right
  | Cast (left, right) ->
      fprintf formatter "(%a)%a" print_type left print_expr right
  | ClassCreation (name, expression_list) ->
      fprintf formatter "new %a(%a)" print_name name print_expr_list
        expression_list
  | CallMethod (method_expression, expression_list) ->
      fprintf formatter "%a(%a)" print_expr method_expression print_expr_list
        expression_list
  | Assign (left, right) ->
      fprintf formatter "%a = %a" print_expr left print_expr right

and print_expr_in_parens formatter = fprintf formatter "(%a)" print_expr

and print_expr_list formatter =
  pp_print_list ~pp_sep:sep_by_comma print_expr formatter

let print_name_expr_list formatter =
  let print_pair formatter = function
    | Name name, None -> fprintf formatter "%s" name
    | Name name, Some expression ->
        fprintf formatter "%s = %a" name print_expr expression
  in
  pp_print_list ~pp_sep:sep_by_comma print_pair formatter

let rec print_stmt formatter = function
  | Expression expression -> fprintf formatter "%a;" print_expr expression
  | If (condition, then_statement, else_statement_opt) -> (
      sep_by_endl formatter ();
      match else_statement_opt with
      | Some else_statetment -> (
          match then_statement with
          | StatementBlock _ ->
              fprintf formatter "if (%a) %a@;<0 0>else %a" print_expr condition
                print_stmt then_statement print_stmt else_statetment
          | _ ->
              fprintf formatter "if (%a) %a else %a" print_expr condition
                print_stmt then_statement print_stmt else_statetment)
      | None ->
          fprintf formatter "if (%a) %a" print_expr condition print_stmt
            then_statement)
  | While (condition, body) ->
      sep_by_endl formatter ();
      fprintf formatter "while (%a) %a" print_expr condition print_stmt body
  | For (decl_statement_opt, condition_opt, after_expression_list, body) ->
      sep_by_endl formatter ();
      (match decl_statement_opt with
      | Some decl_statement ->
          fprintf formatter "for (%a" print_stmt decl_statement
      | None -> fprintf formatter "for ( ;");
      (match condition_opt with
      | Some condition -> fprintf formatter " %a;" print_expr condition
      | None -> fprintf formatter " ;");
      fprintf formatter " %a) " print_expr_list after_expression_list;
      fprintf formatter " %a" print_stmt body
  | Print expr -> fprintf formatter "Console.WriteLine(%a);" print_expr expr
  | Break -> fprintf formatter "break;"
  | Continue -> fprintf formatter "continue;"
  | Return expresion_opt -> (
      match expresion_opt with
      | Some expr -> fprintf formatter "return %a;" print_expr expr
      | None -> fprintf formatter "return;")
  | VariableDecl (modifier_o, var_type, pairs_list) ->
      (match modifier_o with
      | Some modifier -> fprintf formatter "%a " print_modifier modifier
      | None -> ());
      fprintf formatter "%a " print_type var_type;
      fprintf formatter "%a;" print_name_expr_list pairs_list
  | StatementBlock statement_list ->
      fprintf formatter "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]" print_stmt_list
        statement_list

and print_stmt_list formatter =
  pp_print_list ~pp_sep:sep_by_endl print_stmt formatter

let print_type_name_list formatter =
  let print_type_name_pair formatter = function
    | var_type, name ->
        fprintf formatter "%a %a" print_type var_type print_name name
  in
  pp_print_list ~pp_sep:sep_by_comma print_type_name_pair formatter

let print_class_element formatter = function
  | modifier_list, Method (method_type, method_name, arguments, body_o) -> (
      match body_o with
      | Some body ->
          fprintf formatter "%a" print_mods_list modifier_list;
          sep_by_space formatter ();
          fprintf formatter "%a %a(%a) %a" print_type method_type print_name
            method_name print_type_name_list arguments print_stmt body
      | None ->
          fprintf formatter "%a" print_mods_list modifier_list;
          sep_by_space formatter ();
          fprintf formatter "%a %a(%a);" print_type method_type print_name
            method_name print_type_name_list arguments)
  | modifier_list, Constructor (name, arguments, call_constructor, body) -> (
      match call_constructor with
      | Some call_cnstr ->
          fprintf formatter "%a %a(%a) : %a %a" print_mods_list modifier_list
            print_name name print_type_name_list arguments print_expr call_cnstr
            print_stmt body
      | None ->
          fprintf formatter "%a %a(%a) %a" print_mods_list modifier_list
            print_name name print_type_name_list arguments print_stmt body)
  | modifier_list, Field (field_type, field_expressions) ->
      fprintf formatter "%a %a %a;" print_mods_list modifier_list print_type
        field_type print_name_expr_list field_expressions

let print_class_element_list formatter =
  pp_print_list ~pp_sep:sep_by_two_endl print_class_element formatter

let print_object_decl formatter = function
  | Class (modifier_list, name, parent_name_list, elements) ->
      fprintf formatter "%a " print_mods_list modifier_list;
      (* Have at least one modifier *)
      fprintf formatter "class %a" print_name name;
      (match parent_name_list with
      | [] -> ()
      | names -> fprintf formatter " : %a" print_name_list names);
      fprintf formatter "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]"
        print_class_element_list elements
  | Interface (modifier, name, parent_name_list, elements) ->
      fprintf formatter " %a" print_modifier modifier;
      fprintf formatter "interface %a" print_name name;
      (match parent_name_list with
      | [] -> ()
      | names -> fprintf formatter " : %a" print_name_list names);
      fprintf formatter "@;<0 0>@[<v 2>{@;<0 0>%a@;<0 -2>}@]"
        print_class_element_list elements

let print_obj_decl_list formatter =
  let helper_print ppf =
    pp_print_list ~pp_sep:sep_by_two_endl print_object_decl ppf
  in
  fprintf formatter "@[<v 0>%a@]@;<0 0>@;<0 0>" helper_print

let pretty_print = print_obj_decl_list std_formatter
