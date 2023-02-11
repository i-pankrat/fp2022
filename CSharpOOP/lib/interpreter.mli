(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Interpretation : functor (M : ResultMonad.MONADERROR) -> sig
  type variable = {
    v_type : Ast.types;
    v_key : string;
    is_const : bool;
    assignment_count : int;
    v_value : Ast.values;
    vis_lvl : int;
  }

  val pp_variable : Ppx_show_runtime.Format.formatter -> variable -> unit
  val show_variable : variable -> string

  type context = {
    cur_object : Ast.object_references;
    var_table : variable KeyMap.KeyMap.t;
    last_expr_result : Ast.values;
    runtime_signal : Ast.signal;
    curr_method_type : Ast.types;
    is_main_scope : bool;
    nested_loops_cnt : int;
    visibility_level : int;
    cur_constr_key : string option;
    prev_context : context option;
    obj_created_cnt : int;
    is_creation : bool;
    constr_affilation : string option;
    class_for_cast : Ast.table_class option;
  }

  val pp_context : Ppx_show_runtime.Format.formatter -> context -> unit
  val show_context : context -> string
  val execute : Ast.table_class KeyMap.KeyMap.t -> context M.t
end
