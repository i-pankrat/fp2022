(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open KeyMap
open ResultMonad
open Printf
open Operators
open ClassLoader
open ClassLoader (Result)

module Interpretation (M : MONADERROR) = struct
  open M

  type variable = {
    v_type : types;
    v_key : string;
    is_const : bool;
    assignment_count : int;
    v_value : values;
    vis_lvl : int;
  }
  [@@deriving show { with_path = false }]

  type context = {
    cur_object : object_references;
    var_table : variable KeyMap.t;
    last_expr_result : values;
    runtime_signal : signal;
    curr_method_type : types;
    is_main_scope : bool;
    nested_loops_cnt : int;
    visibility_level : int;
    cur_constr_key : string option;
    prev_context : context option;
    obj_created_cnt : int;
    is_creation : bool;
    constr_affilation : string option;
    class_for_cast : table_class option;
  }
  [@@deriving show { with_path = false }]

  let make_context cur_object var_table =
    return
      {
        cur_object;
        var_table;
        last_expr_result = VVoid;
        runtime_signal = NoSignal;
        curr_method_type = TVoid;
        is_main_scope = true;
        nested_loops_cnt = 0;
        visibility_level = 0;
        cur_constr_key = None;
        prev_context = None;
        obj_created_cnt = 0;
        is_creation = false;
        constr_affilation = None;
        class_for_cast = None;
      }

  let get_type_default_value = function
    | TInt -> VInt 0
    | TString -> VString ""
    | TRef _ -> VObjectReference NullObjectReference
    | TBool -> VBool false
    | TVoid -> VVoid

  let seq_hd seq =
    match seq () with Seq.Nil -> raise Not_found | Seq.Cons (x, _) -> x

  (** Applies the F function to the elements of each list and the already accumulated value and returns the accumulated result *)
  let rec mfold_left2 f acc l1 l2 =
    match (l1, l2) with
    | [], [] -> return acc
    | x :: xs, y :: ys -> f acc x y >>= fun res -> mfold_left2 f res xs ys
    | _, _ -> error "Wrong lists for fold_left2"

  let get_field_list = function
    | Class (_, _, _, field_list) -> List.map snd field_list
    | Interface (_, _, _, field_list) -> List.map snd field_list

  let convert_element_pair_list = function
    | t, p_list -> List.map (function s, f -> (t, s, f)) p_list

  let get_variable_field_pairs_list_typed cd =
    List.concat_map convert_element_pair_list
      (List.filter_map
         (function Field (t, pair_list) -> Some (t, pair_list) | _ -> None)
         (get_field_list cd))

  let get_object_fields = function
    | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
    | ObjectReference { field_references_table = field_ref_table; _ } ->
        field_ref_table

  let get_object_info = function
    | NullObjectReference -> raise (Invalid_argument "NullReferenceException")
    | ObjectReference
        {
          class_key = key;
          ext_interface = interface;
          field_references_table = table;
          number = num;
        } ->
        (key, interface, table, num)

  let get_ref_identifier = function
    | TRef ref_name -> return ref_name
    | _ -> error "Not a reference type"

  let get_elem_if_present_m key ht =
    match KeyMap.find_opt key ht with
    | None -> error "No such element in table"
    | Some el -> return el

  let find_main_class (class_map : table_class KeyMap.t) =
    let class_seq = KeyMap.to_seq class_map in
    let rec iter_classes (class_s : (string * table_class) Seq.t) =
      match class_s () with
      | Seq.Nil -> error "There is no Main function"
      | Seq.Cons ((key, curr_class), tail) -> (
          match KeyMap.find_opt "Main" curr_class.methods_table with
          | None -> iter_classes tail
          | Some _ when key = "Program" -> return curr_class
          | Some _ when key <> "Program" ->
              error "Main function must be inside Program class"
          | Some _ -> error "Find main class, unreachable branch")
    in
    iter_classes class_seq

  let rec find_parent_class_key parent_list class_table =
    match parent_list with
    | [] -> None
    | curr_parent_key :: tail -> (
        match get_element_option curr_parent_key class_table with
        | Some curr_parent when curr_parent.is_class -> Some curr_parent_key
        | _ -> find_parent_class_key tail class_table)

  let startswith test_str sub_str =
    let is_left_longer_than_right =
      String.length sub_str <= String.length test_str
    in
    let check = function
      | true ->
          let sub = String.sub test_str 0 (String.length sub_str) in
          String.equal sub sub_str
      | _ -> false
    in
    check is_left_longer_than_right

  (** Сheck that the left class is a parent of the right *)
  let rec check_parent_tree class_table left_class_key right_class_key =
    match get_element_option right_class_key class_table with
    | None -> false
    | Some clr_by_key when clr_by_key.this_key = left_class_key -> true
    | Some clr_by_key -> (
        match clr_by_key.parent_key with
        | [] -> false
        | par_k :: [] -> check_parent_tree class_table left_class_key par_k
        | par_k :: tail ->
            let is_success =
              check_parent_tree class_table left_class_key par_k
            in
            let rec iter_by_parents is_success = function
              | [] -> is_success
              | par_k :: tail ->
                  iter_by_parents
                    (is_success
                    || check_parent_tree class_table left_class_key par_k)
                    tail
            in
            iter_by_parents is_success tail)

  let find_last_overriden call_class_key cast_class method_name class_table =
    let rec helper_iter_by_childs child_list result =
      match child_list with
      | [] -> return result
      | curr_child_key :: tail -> (
          get_elem_if_present_m curr_child_key class_table >>= fun child ->
          match get_element_option method_name child.methods_table with
          | Some child_method
            when child_method.is_overriden
                 && check_parent_tree class_table curr_child_key call_class_key
            ->
              if not (is_new child_method.method_modifiers) then
                let childrens_of_children = child.children_keys in
                helper_iter_by_childs childrens_of_children child
              else return result
          | _ -> helper_iter_by_childs tail result)
    in
    let childrens = cast_class.children_keys in
    match get_element_option method_name cast_class.methods_table with
    | Some _ -> helper_iter_by_childs childrens cast_class
    | None ->
        error
          (String.concat " "
             [
               "Parent class (interface)";
               cast_class.this_key;
               "not have";
               method_name;
               "for";
               call_class_key;
               "class.";
             ])

  (** Finding the nearest direct descendant of the class cast to *)
  let rec find_nearest_child call_class_key children_list class_table =
    match children_list with
    | [] ->
        error
          (String.concat " " [ "No childs are parents for"; call_class_key ])
    | curr_child_key :: tail ->
        get_elem_if_present_m curr_child_key class_table >>= fun child ->
        if check_parent_tree class_table curr_child_key call_class_key then
          return child
        else find_nearest_child call_class_key tail class_table

  let rec expr_type_check t_expr ctx class_table =
    match t_expr with
    | Add (left, right) -> (
        expr_type_check left ctx class_table >>= function
        | TInt -> (
            expr_type_check right ctx class_table >>= function
            | TInt -> return TInt
            | TString -> return TString
            | _ ->
                error "Wrong right type Add function: must be <int> or <string>"
            )
        | TString -> (
            expr_type_check right ctx class_table >>= function
            | TInt | TString -> return TString
            | _ ->
                error "Wrong right type Add function: must be <int> or <string>"
            )
        | _ -> error "Wrong left type Add function: must be <int> or <string>")
    | Sub (left, right)
    | Div (left, right)
    | Mod (left, right)
    | Mult (left, right) -> (
        expr_type_check left ctx class_table >>= function
        | TInt -> (
            expr_type_check right ctx class_table >>= function
            | TInt -> return TInt
            | _ -> error "Wrong right type: must be <int>")
        | _ -> error "Wrong left type: must be <int>")
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expr_type_check value ctx class_table >>= function
        | TInt -> return TInt
        | _ -> error "Wrong value type: must be <int>")
    | And (left, right) | Or (left, right) -> (
        expr_type_check left ctx class_table >>= function
        | TBool -> (
            expr_type_check right ctx class_table >>= function
            | TBool -> return TBool
            | _ -> error "Wrong right type: must be <bool>")
        | _ -> error "Wrong left type: must be <bool>")
    | Not value -> (
        expr_type_check value ctx class_table >>= function
        | TBool -> return TBool
        | _ -> error "Wrong value type: must be <bool>")
    | Less (left, right)
    | More (left, right)
    | LessOrEqual (left, right)
    | MoreOrEqual (left, right) -> (
        expr_type_check left ctx class_table >>= function
        | TInt -> (
            expr_type_check right ctx class_table >>= function
            | TInt -> return TBool
            | _ -> error "Wrong right type: must be <int>")
        | _ -> error "Wrong left type: must be <int>")
    | Equal (left, right) | NotEqual (left, right) -> (
        expr_type_check left ctx class_table >>= function
        | TInt -> (
            expr_type_check right ctx class_table >>= function
            | TInt -> return TBool
            | _ -> error "Wrong right type in equals-expression: must be <int>")
        | TString -> (
            expr_type_check right ctx class_table >>= function
            | TString -> return TBool
            | _ ->
                error "Wrong right type in equals-expression: must be <string>")
        | TBool -> (
            expr_type_check right ctx class_table >>= function
            | TBool -> return TBool
            | _ -> error "Wrong right type in equals-expression: must be <bool>"
            )
        | TRef left_key -> (
            expr_type_check right ctx class_table >>= function
            | TRef right_key when left_key = right_key -> return TBool
            | TRef "null" -> return TBool
            | _ -> error "Wrong class type in equals-expression")
        | _ -> error "Wrong left type in equals-expression")
    | Null -> return (TRef "null")
    | This -> (
        match ctx.cur_object with
        | ObjectReference { class_key = k; _ } -> return (TRef k)
        | NullObjectReference -> error "Current object is null in context")
    | Base -> (
        match ctx.cur_object with
        | ObjectReference { class_key = key; _ } ->
            get_elem_if_present_m key class_table >>= fun curr_class ->
            (* Search parent class, not interface, in table of classes *)
            let rec find_parent_class = function
              | parent :: tail ->
                  get_elem_if_present_m parent class_table
                  >>= fun parent_class ->
                  if parent_class.is_class then return parent
                  else find_parent_class tail
              | [] ->
                  error
                    (String.concat " "
                       [ "The current"; key; "class has no parent class." ])
            in
            find_parent_class curr_class.parent_key >>= fun par_k ->
            return (TRef par_k)
        | NullObjectReference -> error "Current object is null in context")
    | CallMethod (Base, _) -> return TVoid
    | CallMethod (This, _) -> return TVoid
    | CallMethod (Identifier method_ident, args) ->
        let curr_obj_key =
          match ctx.cur_object with
          | NullObjectReference -> "null"
          | ObjectReference { class_key = key; _ } -> key
        in
        get_elem_if_present_m curr_obj_key class_table >>= fun curr_class ->
        check_method curr_class method_ident args ctx class_table >>= fun mr ->
        return mr.method_type
    | AccessByPoint (obj_expr, Identifier f_key) -> (
        expr_type_check obj_expr ctx class_table >>= function
        | TRef "null" -> error "NullReferenceException"
        | TRef obj_key -> (
            get_elem_if_present_m obj_key class_table >>= fun obj_class ->
            let var_field_o = get_element_option f_key obj_class.fields_table in
            match var_field_o with
            | None -> error "No such field in class"
            | Some var_field -> return var_field.field_type)
        | _ -> error "Wrong type: must be an object of some class")
    | AccessByPoint (obj_expr, CallMethod (Identifier method_ident, args)) -> (
        expr_type_check obj_expr ctx class_table >>= function
        | TRef "null" -> error "NullReferenceException"
        | TRef obj_key ->
            get_elem_if_present_m obj_key class_table >>= fun obj_class ->
            check_method obj_class method_ident args ctx class_table
            >>= fun m_r -> return m_r.method_type
        | _ -> error "Wrong type: must be an object of some class")
    | ClassCreation (Name class_name, args) -> (
        match get_element_option class_name class_table with
        | None ->
            error
              (String.concat " " [ "No such class implemented:"; class_name ])
        | Some cl_elem -> (
            match args with
            | [] -> return (TRef class_name)
            | _ ->
                check_constructor cl_elem args ctx class_table
                >> return (TRef class_name)))
    | Identifier key -> (
        let var_opt = get_element_option key ctx.var_table in
        match var_opt with
        | None -> (
            match ctx.cur_object with
            | ObjectReference { field_references_table = class_field_table; _ }
              -> (
                match get_element_option key class_field_table with
                | None ->
                    error ("No such variable or field with this name : " ^ key)
                | Some field -> return field.field_type)
            | _ -> error "NullReferenceException")
        | Some v -> return v.v_type)
    | Value value -> (
        match value with
        | VBool _ -> return TBool
        | VInt _ -> return TInt
        | VString _ -> return TString
        | VObjectReference NullObjectReference -> return (TRef "null")
        | VObjectReference (ObjectReference { class_key = ck; _ }) ->
            return (TRef ck)
        | _ -> error "Wrong constant value")
    | Assign (left, right) -> (
        expr_type_check left ctx class_table >>= fun lt ->
        match lt with
        | TVoid -> error "Can't assign anything to <void>"
        | TRef cleft_key -> (
            expr_type_check right ctx class_table >>= function
            | TRef "null" -> return (TRef cleft_key)
            | TRef cright_key ->
                check_classname_assign cleft_key cright_key class_table
            | _ -> error "Wrong assign types")
        | _ ->
            expr_type_check right ctx class_table >>= fun rt ->
            if lt = rt then return rt else error "Wrong assign types")
    | Cast (left_type, right) -> (
        expr_type_check right ctx class_table >>= function
        | TInt when left_type = TInt -> return TInt (* (int)int *)
        | TInt when left_type = TBool -> return TBool (* (bool)int *)
        | TBool when left_type = TBool -> return TBool (* (bool)bool *)
        | TRef right_type_id ->
            get_ref_identifier left_type >>= fun left_type_id ->
            check_classname_assign left_type_id right_type_id class_table
        | _ -> error "Wrong cast types")
    | _ -> error "Wrong expression"

  (** Сheck that the right class can be assigned to left and return right type *)
  and check_classname_assign cleft_key cright_key class_table =
    if check_parent_tree class_table cleft_key cright_key then
      return (TRef cright_key)
    else
      error
        (String.concat " "
           [
             "Cannot assign the most general type";
             cright_key;
             "to the least general";
             cleft_key;
           ])

  (** Сheck that the left class is a parent of the right *)
  and check_classname_bool cleft_key cright_key class_table =
    check_parent_tree class_table cleft_key cright_key

  (** Verifies that the called method with the passed number of arguments exists in the singular in the class,
      and that the argument types are the same as expected.
      Get current class, name of method for check, its arguments, context, table of all classes*)
  and check_method cl_r method_name expr_list check_ctx class_table =
    (* Get the position of the element being tested and checks that the resulting type passed is equal to the expected type *)
    let check_type_m : int -> types -> string -> table_method -> bool =
     fun pos curr_type _ value ->
      match List.nth_opt value.arguments pos with
      | None -> false
      | Some (found_type, _) -> (
          match curr_type with
          | TRef "null" -> (
              match found_type with TRef _ -> true | _ -> false)
          | TRef curr_key -> (
              match found_type with
              | TRef found_key ->
                  check_classname_bool found_key curr_key class_table
              | _ -> false)
          | _ -> found_type = curr_type)
    in
    let rec helper_checker curr_ht pos e_list ctx =
      match KeyMap.cardinal curr_ht with
      | 0 -> error "No such method implemented"
      | suit_method_count -> (
          match e_list with
          | [] -> (
              match suit_method_count with
              | 1 ->
                  let _, m = seq_hd (KeyMap.to_seq curr_ht) in
                  return m
              | _ ->
                  error
                    "Cannot resolve method. Too much/few methods have this \
                     name and args")
          | e :: es ->
              expr_type_check e ctx class_table >>= fun e_type ->
              helper_checker
                (KeyMap.filter (check_type_m pos e_type) curr_ht)
                (pos + 1) es ctx)
    in
    KeyMap.filter (fun _ mr -> startswith mr.key method_name) cl_r.methods_table
    |> fun filtered_by_name ->
    helper_checker
      (KeyMap.filter
         (fun _ mr -> List.length mr.arguments = List.length expr_list)
         filtered_by_name)
      0 expr_list check_ctx

  (** Verifies that the called constructor with the passed number of arguments exists in the singular in the class,
      and that the argument types are the same as expected.
      Get current class, constructor arguments, context, table of all classes*)
  and check_constructor cl_r expr_list check_ctx class_table =
    let check_type_c : int -> types -> string -> table_constructor -> bool =
     fun pos curr_type _ value ->
      match List.nth_opt value.arguments pos with
      | None -> false
      | Some (found_type, _) -> (
          match curr_type with
          | TRef "null" -> (
              match found_type with TRef _ -> true | _ -> false)
          | TRef curr_key -> (
              match found_type with
              | TRef found_key ->
                  check_classname_bool found_key curr_key class_table
              | _ -> false)
          | _ -> found_type = curr_type)
    in
    let rec helper_checker_c curr_ht pos e_list ctx =
      match KeyMap.cardinal curr_ht with
      | 0 -> error "No such constructor implemented"
      | suit_constructor_count -> (
          match e_list with
          | [] -> (
              match suit_constructor_count with
              | 1 ->
                  let _, m = seq_hd (KeyMap.to_seq curr_ht) in
                  return m
              | _ -> error "Cannot resolve constructor")
          | e :: es ->
              expr_type_check e ctx class_table >>= fun e_type ->
              helper_checker_c
                (KeyMap.filter (check_type_c pos e_type) curr_ht)
                (pos + 1) es ctx)
    in
    helper_checker_c
      (KeyMap.filter
         (fun _ (cr : table_constructor) ->
           List.length cr.arguments = List.length expr_list)
         cl_r.constructors_table)
      0 expr_list check_ctx

  let get_obj_value = function
    | VObjectReference o -> o
    | _ -> NullObjectReference

  let add_var_to_context ctx var =
    { ctx with var_table = KeyMap.add var.v_key var ctx.var_table }

  let inc_visibility_level ctx =
    { ctx with visibility_level = ctx.visibility_level + 1 }

  let dec_visibility_level ctx =
    { ctx with visibility_level = ctx.visibility_level - 1 }

  let check_assign_cnt_f : field_references -> unit M.t =
   fun fld ->
    match fld.assignments_count with
    | 0 -> return ()
    | _ when not fld.is_const -> return ()
    | _ -> error "Assignment to a constant field"

  let check_assign_cnt_v var =
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assignment to a constant variable"

  and can_access_modifier modifier_list called_class curr_obj_value class_table
      =
    match curr_obj_value with
    | ObjectReference { class_key = curr_obj_key; _ } ->
        is_public modifier_list
        || (is_private modifier_list && called_class = curr_obj_key)
        || is_protected modifier_list
           && check_parent_tree class_table curr_obj_key called_class
        (* Can get access to protected child elements from the parent class *)
    | _ -> false

  (** Delete nested variables (e.g. in loops) *)
  let delete_scope_var : context -> context M.t =
   fun in_ctx ->
    let delete key (el : variable) ctx =
      if el.vis_lvl = ctx.visibility_level then
        { ctx with var_table = KeyMap.remove key ctx.var_table }
      else ctx
    in
    let rec iter_by_variables seq_varibles result =
      match seq_varibles () with
      | Seq.Nil -> result
      | Seq.Cons ((variable_key, variable), next) ->
          iter_by_variables next (delete variable_key variable result)
    in
    let seq_var_table = KeyMap.to_seq in_ctx.var_table in
    return (iter_by_variables seq_var_table in_ctx)

  (** Expressions that can be used as statements (on single line) *)
  let is_good_for_stmt = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
    | CallMethod (_, _)
    | AccessByPoint (_, CallMethod (_, _))
    | Assign (_, _) ->
        true
    | _ -> false

  (** Interpretate statements *)
  let rec eval_stmt stmt sctx class_table =
    match stmt with
    | StatementBlock st_list ->
        let rec helper_eval : statements list -> context -> context M.t =
         fun stmt_list hctx ->
          match stmt_list with
          | [] -> return hctx
          | statement :: stmt_tail -> (
              match statement with
              | (Break | Continue | Return _) when stmt_tail <> [] ->
                  error "Statemets block contains unreachable code"
              | _
                when hctx.nested_loops_cnt >= 1
                     && hctx.runtime_signal = WasBreak ->
                  return hctx
              | _
                when hctx.nested_loops_cnt >= 1
                     && hctx.runtime_signal = WasContinue ->
                  return hctx
              | _ when hctx.runtime_signal = WasReturn -> return hctx
              | _ ->
                  eval_stmt statement hctx class_table >>= fun head_ctx ->
                  helper_eval stmt_tail head_ctx)
        in
        helper_eval st_list sctx >>= fun stmt_bl_ctx ->
        if stmt_bl_ctx.is_main_scope then return stmt_bl_ctx
        else delete_scope_var stmt_bl_ctx
    | While (bool_expr, lstmt) -> (
        let was_main = sctx.is_main_scope in
        let rec loop s ctx =
          if ctx.runtime_signal = WasBreak then
            match s with
            (* If there was a StatemetntBlock, then we still need to lower the visibility level *)
            | StatementBlock _ ->
                return
                  (dec_visibility_level
                     {
                       ctx with
                       runtime_signal = NoSignal;
                       nested_loops_cnt = ctx.nested_loops_cnt - 1;
                     })
            | _ ->
                return
                  {
                    ctx with
                    runtime_signal = NoSignal;
                    nested_loops_cnt = ctx.nested_loops_cnt - 1;
                  }
          else
            eval_expr bool_expr ctx class_table >>= fun bool_expr_ctx ->
            match bool_expr_ctx.last_expr_result with
            | VBool false -> (
                match s with
                | StatementBlock _ ->
                    return
                      (dec_visibility_level
                         {
                           bool_expr_ctx with
                           nested_loops_cnt = ctx.nested_loops_cnt - 1;
                           is_main_scope = was_main;
                         })
                | _ ->
                    return
                      {
                        bool_expr_ctx with
                        nested_loops_cnt = ctx.nested_loops_cnt - 1;
                      })
            | VBool true -> (
                eval_stmt s bool_expr_ctx class_table >>= fun lctx ->
                match lctx.runtime_signal with
                | WasReturn -> return lctx
                | WasContinue -> loop s { lctx with runtime_signal = NoSignal }
                | _ -> loop s lctx)
            | _ -> error "Wrong expression type for loop <while> condition"
        in
        match lstmt with
        | StatementBlock _ ->
            loop lstmt
              (inc_visibility_level
                 {
                   sctx with
                   nested_loops_cnt = sctx.nested_loops_cnt + 1;
                   is_main_scope = false;
                 })
        | _ ->
            loop lstmt
              { sctx with nested_loops_cnt = sctx.nested_loops_cnt + 1 })
    | Break ->
        if sctx.nested_loops_cnt <= 0 then error "No loop for <break>"
        else return { sctx with runtime_signal = WasBreak }
    | Continue ->
        if sctx.nested_loops_cnt <= 0 then error "No loop for <continue>"
        else return { sctx with runtime_signal = WasContinue }
    | If (bool_expr, then_stmt, else_stmt_opt) -> (
        eval_expr bool_expr sctx class_table >>= fun bool_expr_ctx ->
        let was_main = bool_expr_ctx.is_main_scope in
        match bool_expr_ctx.last_expr_result with
        | VBool true -> (
            match then_stmt with
            | StatementBlock _ ->
                eval_stmt then_stmt
                  (inc_visibility_level
                     { bool_expr_ctx with is_main_scope = false })
                  class_table
                >>= fun tctx ->
                return
                  (dec_visibility_level { tctx with is_main_scope = was_main })
            | _ -> eval_stmt then_stmt bool_expr_ctx class_table)
        | VBool false -> (
            match else_stmt_opt with
            | Some (StatementBlock _ as else_stmt) ->
                eval_stmt else_stmt
                  (inc_visibility_level
                     { bool_expr_ctx with is_main_scope = false })
                  class_table
                >>= fun ectx ->
                return
                  (dec_visibility_level { ectx with is_main_scope = was_main })
            | Some else_stmt -> eval_stmt else_stmt bool_expr_ctx class_table
            | None -> return sctx)
        | _ -> error "Wrong expression type in <if> condition")
    | For (decl_stmt_opt, bool_expr_opt, after_expr_list, body_stmt) ->
        let was_main = sctx.is_main_scope in
        (* With a loop for visibility_level always increases, despite the presence/absence of a body block *)
        (match decl_stmt_opt with
        | None ->
            return (inc_visibility_level { sctx with is_main_scope = false })
        | Some dec_stmt ->
            eval_stmt dec_stmt
              (inc_visibility_level { sctx with is_main_scope = false })
              class_table)
        >>= fun dec_ctx ->
        let rec loop body_stmt afs ctx =
          if ctx.runtime_signal = WasBreak then
            delete_scope_var
              (dec_visibility_level
                 {
                   ctx with
                   runtime_signal = NoSignal;
                   nested_loops_cnt = ctx.nested_loops_cnt - 1;
                   is_main_scope = was_main;
                 })
          else
            (* Standard: we look at the result of the boolean expression, if true - calculate
               the body and increments after *)
            (match bool_expr_opt with
            | None -> return { ctx with last_expr_result = VBool true }
            | Some bool_expr -> eval_expr bool_expr ctx class_table)
            >>= fun bool_expr_ctx ->
            match bool_expr_ctx.last_expr_result with
            | VBool false ->
                delete_scope_var
                  (dec_visibility_level
                     {
                       bool_expr_ctx with
                       nested_loops_cnt = bool_expr_ctx.nested_loops_cnt - 1;
                       is_main_scope = was_main;
                     })
            | VBool true -> (
                (* Execute commands after condition in loop definition --- for (decl; bool; this func) *)
                let rec eval_inc_expr_list e_list c =
                  match e_list with
                  | [] -> return c
                  | e :: es ->
                      if is_good_for_stmt e then
                        eval_expr e c class_table >>= fun ehctx ->
                        eval_inc_expr_list es ehctx
                      else error "Wrong expression in after body"
                in
                (* Variables inside the block itself will be in a larger visibility_level than
                   from the initializer *)
                eval_stmt body_stmt
                  {
                    bool_expr_ctx with
                    visibility_level = dec_ctx.visibility_level + 1;
                  }
                  class_table
                >>= fun body_ctx ->
                match body_ctx.runtime_signal with
                | WasReturn -> return { body_ctx with is_main_scope = was_main }
                | WasContinue ->
                    eval_inc_expr_list afs body_ctx >>= fun after_ctx ->
                    loop body_stmt afs
                      { after_ctx with runtime_signal = NoSignal }
                | _ ->
                    eval_inc_expr_list afs body_ctx >>= fun after_ctx ->
                    loop body_stmt afs after_ctx)
            | _ -> error "Wrong expression type in loop <for> condition"
        in
        loop body_stmt after_expr_list
          { dec_ctx with nested_loops_cnt = sctx.nested_loops_cnt + 1 }
    | Return None when sctx.curr_method_type = TVoid ->
        return
          { sctx with last_expr_result = VVoid; runtime_signal = WasReturn }
    | Return None -> error "Return value type mismatch"
    | Return (Some expr) ->
        expr_type_check expr sctx class_table >>= fun expr_type ->
        if expr_type <> sctx.curr_method_type then
          error "Return value type mismatch"
        else
          eval_expr expr sctx class_table >>= fun ret_ctx ->
          return { ret_ctx with runtime_signal = WasReturn }
    | Expression expr ->
        if is_good_for_stmt expr then
          eval_expr expr sctx class_table >>= fun ectx -> return ectx
        else error "Wrong expression in statement"
    | VariableDecl (modifier, vars_type, var_list) ->
        let is_const : modifiers option -> bool = function
          | Some Const -> true
          | _ -> false
        in
        let rec helper_vardec var_ctx = function
          | [] -> return var_ctx
          | (Name name, var_expr_opt) :: tail -> (
              match var_ctx.cur_object with
              | NullObjectReference ->
                  error "Cannot assign value to variable of null-object"
              | ObjectReference { field_references_table = frt; _ } ->
                  (if KeyMap.mem name var_ctx.var_table || KeyMap.mem name frt
                  then error "Variable with this name is already defined"
                  else
                    match var_expr_opt with
                    (* If there is nothing, initialize with the base value *)
                    | None ->
                        let new_var =
                          {
                            v_type = vars_type;
                            v_key = name;
                            is_const = is_const modifier;
                            assignment_count = 0;
                            v_value = get_type_default_value vars_type;
                            vis_lvl = var_ctx.visibility_level;
                          }
                        in
                        return (add_var_to_context var_ctx new_var)
                    | Some var_expr -> (
                        expr_type_check var_expr var_ctx class_table
                        >>= fun var_expr_type ->
                        let add_var ve interface =
                          eval_expr ve var_ctx class_table
                          >>= fun var_expr_ctx ->
                          let var_value =
                            match var_expr_ctx.last_expr_result with
                            | VObjectReference (ObjectReference obj_value) ->
                                VObjectReference
                                  (ObjectReference
                                     {
                                       obj_value with
                                       ext_interface = interface;
                                     })
                            | otherwise -> otherwise
                          in
                          let new_var =
                            {
                              v_type = var_expr_type;
                              v_key = name;
                              is_const = is_const modifier;
                              assignment_count = 1;
                              v_value = var_value;
                              vis_lvl = var_expr_ctx.visibility_level;
                            }
                          in
                          return (add_var_to_context var_expr_ctx new_var)
                        in
                        match var_expr_type with
                        | TRef "null" -> (
                            match vars_type with
                            | TRef cleft -> add_var var_expr (Some cleft)
                            | _ -> error "Wrong assign type in declaration")
                        | TRef cright -> (
                            match vars_type with
                            | TRef cleft ->
                                check_classname_assign cleft cright class_table
                                >>
                                if cleft = cright then add_var var_expr None
                                else add_var var_expr (Some cleft)
                            | _ -> error "Wrong assign type in declaration")
                        | _ when var_expr_type = vars_type ->
                            add_var var_expr None
                        | _ ->
                            error
                              ("Wrong value type for declared variable: "
                             ^ show_types var_expr_type)))
                  >>= fun head_ctx -> helper_vardec head_ctx tail)
        in
        helper_vardec sctx var_list
    | Print print_expr ->
        eval_expr print_expr sctx class_table >>= fun new_ctx ->
        let printer = function
          | VInt value -> return (printf "%d\n" value)
          | VBool value -> return (printf "%b\n" value)
          | VString value -> return (printf "%s\n" value)
          | VObjectReference value -> (
              match value with
              | NullObjectReference -> error "NullReferenceException"
              | ObjectReference ob -> return (printf "%s\n" ob.class_key))
          | VVoid -> error "Impossible to print void"
        in
        printer new_ctx.last_expr_result
        (* Console.WriteLine return nothing *)
        >> return { new_ctx with last_expr_result = VVoid }

  and eval_expr expr ectx class_table =
    let eval_e e_expr ctx =
      let eval_op left right op =
        eval_expr left ctx class_table >>= fun lctx ->
        eval_expr right lctx class_table >>= fun rctx ->
        let l_value = lctx.last_expr_result in
        let r_value = rctx.last_expr_result in
        try
          let new_value = op l_value r_value in
          return { rctx with last_expr_result = new_value }
        with
        | Invalid_argument m -> error m
        | Division_by_zero -> error "Division by zero"
      in
      let eval_un v_expr op =
        eval_expr v_expr ctx class_table >>= fun vctx ->
        let v = vctx.last_expr_result in
        try
          let new_v = op v in
          return { vctx with last_expr_result = new_v }
        with Invalid_argument m -> error m
      in
      match e_expr with
      | Add (left, right) -> eval_op left right ( ++ )
      | Sub (left, right) -> eval_op left right ( -- )
      | Mult (left, right) -> eval_op left right ( ** )
      | Div (left, right) -> eval_op left right ( // )
      | Mod (left, right) -> eval_op left right ( %% )
      | And (left, right) -> eval_op left right ( &&& )
      | Or (left, right) -> eval_op left right ( ||| )
      | Not bexp -> eval_un bexp ( !!! )
      | Less (left, right) -> eval_op left right ( <<< )
      | More (left, right) -> eval_op left right ( >>> )
      | LessOrEqual (left, right) -> eval_op left right ( ==<< )
      | MoreOrEqual (left, right) -> eval_op left right ( >>== )
      | Equal (left, right) -> (
          expr_type_check left ctx class_table >>= function
          | TRef l_key ->
              get_elem_if_present_m l_key class_table >>= fun left_clr ->
              get_elem_if_present_m equals_key left_clr.methods_table
              >>= fun left_eqr ->
              if left_eqr.is_overriden then
                eval_expr
                  (AccessByPoint
                     (left, CallMethod (Identifier "Equals", [ right ])))
                  ctx class_table
              else eval_op left right ( === )
          | _ -> eval_op left right ( === ))
      | NotEqual (left, right) -> eval_op left right ( !=! )
      | Value v -> return { ctx with last_expr_result = v }
      | Identifier id -> (
          match get_element_option id ctx.var_table with
          | Some var_by_id ->
              return { ctx with last_expr_result = var_by_id.v_value }
          | None -> (
              try
                let frt = function
                  | NullObjectReference ->
                      raise (Invalid_argument "NullReferenceException")
                  | ObjectReference object_class ->
                      object_class.field_references_table
                in
                let frt_curr_object = frt ctx.cur_object in
                match get_element_option id frt_curr_object with
                | Some f -> return { ctx with last_expr_result = f.field_value }
                | None -> error "No such variable or field"
              with Invalid_argument m | Failure m -> error m))
      | Null ->
          return
            { ctx with last_expr_result = VObjectReference NullObjectReference }
      | CallMethod (This, args) ->
          (match ctx.cur_constr_key with
          | None ->
              error "this(...) call must be in Constructor(...) : this(...)"
          | Some k -> return k)
          >>= fun external_constr_key ->
          let get_cur_class_key =
            match ctx.cur_object with
            | NullObjectReference -> error "NullReferenceException"
            | ObjectReference { class_key = key; _ } -> return key
          in
          get_cur_class_key >>= fun curr_class_key ->
          get_elem_if_present_m curr_class_key class_table >>= fun curr_class ->
          check_constructor curr_class args ctx class_table
          >>= fun curr_class_constr ->
          if curr_class_constr.key = external_constr_key then
            error "Constructor recursion"
          else
            prepare_constructor_block curr_class_constr.body curr_class
              curr_class_constr.call_constructor class_table
            >>= fun constr_body ->
            (try
               prepare_table_with_args_exn KeyMap.empty args
                 curr_class_constr.arguments ctx class_table
             with Invalid_argument m -> error m)
            >>= fun (args_table, vctx) ->
            eval_stmt constr_body
              { vctx with var_table = args_table; is_creation = true }
              class_table
            >>= fun res_ctx ->
            return
              {
                res_ctx with
                last_expr_result = VVoid;
                var_table = ctx.var_table;
                constr_affilation = ctx.constr_affilation;
                is_creation = true;
              }
      | CallMethod (Base, args) -> (
          ((match ctx.cur_constr_key with
           | None ->
               error "base(...) call must be in Constructor(...) : base(...)"
           | Some k -> return k)
          >>
          match ctx.constr_affilation with
          | None ->
              error "base(...) call must be in Constructor(...) : base(...)"
          | Some c_aff -> return c_aff)
          >>= fun curr_class_key ->
          get_elem_if_present_m curr_class_key class_table >>= fun curr_class ->
          let current_class_parent_key =
            find_parent_class_key curr_class.parent_key class_table
          in
          match current_class_parent_key with
          | None -> error "Bad base(...) call usage: this class has no parent"
          | Some par_key ->
              get_elem_if_present_m par_key class_table >>= fun parent_class ->
              check_constructor parent_class args ctx class_table
              >>= fun curr_class_constr ->
              prepare_constructor_block curr_class_constr.body parent_class
                curr_class_constr.call_constructor class_table
              >>= fun par_constr_body ->
              (try
                 prepare_table_with_args_exn KeyMap.empty args
                   curr_class_constr.arguments ctx class_table
               with Invalid_argument m -> error m)
              >>= fun (args_table, vctx) ->
              eval_stmt par_constr_body
                {
                  vctx with
                  var_table = args_table;
                  is_creation = true;
                  constr_affilation = Some par_key;
                  cur_constr_key = Some curr_class_constr.key;
                }
                class_table
              >>= fun res_ctx ->
              return
                {
                  res_ctx with
                  last_expr_result = VVoid;
                  var_table = ctx.var_table;
                  constr_affilation = ctx.constr_affilation;
                  is_creation = true;
                })
      | This ->
          return { ctx with last_expr_result = VObjectReference ctx.cur_object }
      | Cast (left_type, obj_expr) -> (
          eval_expr obj_expr ctx class_table >>= fun octx ->
          let obj_value = octx.last_expr_result in
          match obj_value with
          | VInt n when left_type = TInt ->
              return { octx with last_expr_result = VInt n }
          | VInt n when left_type = TBool ->
              return
                {
                  octx with
                  last_expr_result =
                    (if n <> 0 then VBool true else VBool false);
                }
          | VBool f when left_type = TBool ->
              return { octx with last_expr_result = VBool f }
          | VObjectReference (ObjectReference obj) ->
              get_ref_identifier left_type >>= fun left_type_id ->
              (* Has already been verified in the typechecker that this cast is possible *)
              return
                {
                  octx with
                  last_expr_result =
                    VObjectReference
                      (ObjectReference
                         { obj with ext_interface = Some left_type_id });
                }
          | _ ->
              error
                (String.concat " "
                   [
                     "Cannot cast";
                     show_values obj_value;
                     "to";
                     show_types left_type;
                   ]))
      | AccessByPoint (obj_expr, Identifier f_key) -> (
          eval_expr obj_expr ctx class_table >>= fun octx ->
          let obj = octx.last_expr_result in
          match obj with
          | VObjectReference
              (ObjectReference
                {
                  class_key = cl_k;
                  field_references_table = frt;
                  ext_interface = interface;
                  _;
                }) ->
              let cast_class_helper =
                match interface with
                | Some key -> get_elem_if_present_m key class_table
                | None -> get_elem_if_present_m cl_k class_table
              in
              cast_class_helper >>= fun cast_class ->
              let is_cast_to_interface =
                if not cast_class.is_class then
                  error
                    (String.concat " "
                       [
                         "Cannot access to";
                         cast_class.this_key;
                         "interface field. Interface doesn't have fields";
                       ])
                else
                  (* Check that parent (cast class) have this field *)
                  get_elem_if_present_m f_key cast_class.fields_table
                  >>= fun inter_field ->
                  match
                    can_access_modifier inter_field.field_modifiers cl_k
                      octx.cur_object class_table
                  with
                  | true ->
                      get_elem_if_present_m f_key frt >>= fun fld ->
                      return { octx with last_expr_result = fld.field_value }
                  | false ->
                      error
                        (String.concat ""
                           [ "Cannot access to "; cl_k; "."; f_key ])
              in
              is_cast_to_interface
          | _ -> error "Cannot access field of non-object")
      | AccessByPoint (obj_expr, CallMethod (Identifier method_name, args)) -> (
          eval_expr obj_expr ctx class_table >>= fun octx ->
          let obj = octx.last_expr_result in
          match obj with
          | VObjectReference NullObjectReference ->
              error "NullReferenceException"
          | VObjectReference
              (ObjectReference
                 { class_key = cl_k; ext_interface = interface; _ } as
              current_class) -> (
              match get_element_option cl_k class_table with
              | None ->
                  error
                    (String.concat " "
                       [
                         "No such class"; cl_k; "to call"; method_name; "method";
                       ])
              | Some obj_class -> (
                  let cast_class_helper =
                    match interface with
                    | Some key -> get_elem_if_present_m key class_table
                    | None -> get_elem_if_present_m cl_k class_table
                  in
                  check_method obj_class method_name args octx class_table
                  >>= fun method_before_cast ->
                  cast_class_helper >>= fun cast_class ->
                  let last_overriden_class =
                    find_last_overriden cl_k cast_class method_before_cast.key
                      class_table
                  in
                  let nc =
                    find_nearest_child cl_k cast_class.children_keys class_table
                  in
                  let m_name =
                    if not cast_class.is_class then
                      nc >>= fun nearest_child ->
                      match
                        get_element_option
                          (String.concat ""
                             [
                               cast_class.this_key; "."; method_before_cast.key;
                             ])
                          nearest_child.methods_table
                      with
                      | None -> return (nearest_child, method_before_cast.key)
                      | Some _ ->
                          return
                            ( nearest_child,
                              String.concat ""
                                [
                                  cast_class.this_key;
                                  ".";
                                  method_before_cast.key;
                                ] )
                    else
                      last_overriden_class >>= fun last_ovr ->
                      return (last_ovr, method_before_cast.key)
                  in
                  m_name >>= fun (obj_to_call, meth_name) ->
                  check_method obj_to_call meth_name args octx class_table
                  >>= fun mr ->
                  let check_method_modifiers =
                    match
                      can_access_modifier mr.method_modifiers cl_k
                        octx.cur_object class_table
                    with
                    | true -> return mr
                    | false ->
                        error
                          (String.concat ""
                             [ "Cannot access to "; cl_k; "."; meth_name; "()" ])
                  in
                  check_method_modifiers >>= fun mr ->
                  (match mr.body with
                  | None -> error "Error: abstract class creation"
                  | Some b -> return b)
                  >>= fun m_body ->
                  (try
                     prepare_table_with_args_exn KeyMap.empty args mr.arguments
                       octx class_table
                   with Invalid_argument m -> error m)
                  >>= fun (new_vt, new_ctx) ->
                  eval_stmt m_body
                    {
                      ctx with
                      class_for_cast = Some obj_class;
                      constr_affilation = None;
                      is_creation = false;
                      prev_context = Some ctx;
                      cur_constr_key = None;
                      visibility_level = 0;
                      nested_loops_cnt = 0;
                      is_main_scope = false;
                      curr_method_type = mr.method_type;
                      runtime_signal = NoSignal;
                      last_expr_result = VVoid;
                      var_table = new_vt;
                      cur_object = current_class;
                    }
                    class_table
                  >>= fun m_res_ctx ->
                  match m_res_ctx.prev_context with
                  | None ->
                      return
                        {
                          new_ctx with
                          last_expr_result =
                            (if mr.method_type = TVoid then VVoid
                            else m_res_ctx.last_expr_result);
                          obj_created_cnt = m_res_ctx.obj_created_cnt;
                          is_creation = false;
                          class_for_cast = None;
                        }
                  | Some prev_ctx ->
                      return
                        {
                          prev_ctx with
                          last_expr_result =
                            (if mr.method_type = TVoid then VVoid
                            else m_res_ctx.last_expr_result);
                          obj_created_cnt = m_res_ctx.obj_created_cnt;
                          is_creation = false;
                          class_for_cast = None;
                        }))
          | _ -> error "Cannot access field of non-object")
      | CallMethod (Identifier m, args) ->
          eval_expr
            (AccessByPoint (This, CallMethod (Identifier m, args)))
            ctx class_table
      | ClassCreation (Name class_name, creation_args) ->
          get_elem_if_present_m class_name class_table >>= fun obj_class ->
          if obj_class.is_abstract || not obj_class.is_class then
            error
              (String.concat " "
                 [
                   "Creation of interface or abstract classes";
                   obj_class.this_key;
                   "not allowed";
                 ])
          else
            check_constructor obj_class creation_args ctx class_table
            >>= fun curr_constr ->
            let rec init_object curr_class init_ctx =
              let field_tuples =
                get_variable_field_pairs_list_typed curr_class.decl_tree
              in
              let rec helper_init acc_ht help_ctx = function
                | [] -> return help_ctx
                | (curr_f_type, Name f_name, f_expr_o) :: fields_tail ->
                    let is_const_field f_key =
                      get_elem_if_present_m f_key obj_class.fields_table
                      >>= fun checked_field ->
                      return (is_const checked_field.field_modifiers)
                    in
                    (match f_expr_o with
                    | Some f_expr -> (
                        expr_type_check f_expr help_ctx class_table
                        >>= fun expr_type ->
                        is_const_field f_name >>= fun is_const_f ->
                        let add_field field_expr =
                          eval_expr field_expr help_ctx class_table
                          >>= fun field_expr_ctx ->
                          let new_field =
                            KeyMap.add f_name
                              {
                                key = f_name;
                                field_type = curr_f_type;
                                field_value = field_expr_ctx.last_expr_result;
                                is_const = is_const_f;
                                assignments_count = 1;
                              }
                              acc_ht
                          in
                          return (field_expr_ctx, new_field)
                        in
                        match expr_type with
                        | TRef "null" -> (
                            match curr_f_type with
                            | TRef _ -> add_field f_expr
                            | _ ->
                                error
                                  ("Wrong assign type in field declaration. \
                                    Cannot assign <null> to not reference \
                                    field " ^ f_name))
                        | TRef cright -> (
                            match curr_f_type with
                            | TRef cleft ->
                                check_classname_assign cleft cright class_table
                                >>= fun _ -> add_field f_expr
                            | _ ->
                                error "Wrong assign type in field declaration")
                        | _ when expr_type = curr_f_type -> add_field f_expr
                        | _ -> error "Wrong assign type in declaration")
                    | None ->
                        is_const_field f_name >>= fun is_const_f ->
                        let new_field =
                          KeyMap.add f_name
                            {
                              key = f_name;
                              field_type = curr_f_type;
                              field_value = get_type_default_value curr_f_type;
                              is_const = is_const_f;
                              assignments_count = 0;
                            }
                            acc_ht
                        in
                        return (help_ctx, new_field))
                    >>= fun (head_ctx, head_ht) ->
                    let get_obj_num = function
                      | NullObjectReference ->
                          raise (Invalid_argument "NullReferenceException")
                      | ObjectReference curr_obj -> curr_obj.number
                    in
                    (try get_obj_num head_ctx.cur_object |> fun n -> return n
                     with Invalid_argument m -> error m)
                    >>= fun num ->
                    helper_init head_ht
                      {
                        head_ctx with
                        cur_object =
                          ObjectReference
                            {
                              class_key = class_name;
                              ext_interface = None;
                              field_references_table = head_ht;
                              number = num;
                            };
                      }
                      fields_tail
              in
              let current_class_parent_key =
                find_parent_class_key curr_class.parent_key class_table
              in
              match current_class_parent_key with
              | None -> helper_init KeyMap.empty init_ctx field_tuples
              | Some par_key ->
                  (* Create parent for getting parents field map *)
                  get_elem_if_present_m par_key class_table
                  >>= fun parent_class ->
                  init_object parent_class init_ctx >>= fun par_ctx ->
                  helper_init
                    (get_object_fields par_ctx.cur_object)
                    par_ctx field_tuples
            in
            let new_object =
              ObjectReference
                {
                  class_key = class_name;
                  ext_interface = None;
                  field_references_table = KeyMap.empty;
                  number = ctx.obj_created_cnt + 1;
                }
            in
            init_object obj_class
              {
                cur_object = new_object;
                var_table = KeyMap.empty;
                last_expr_result = VVoid;
                runtime_signal = NoSignal;
                curr_method_type = TVoid;
                is_main_scope = false;
                nested_loops_cnt = 0;
                visibility_level = 0;
                prev_context = Some ctx;
                obj_created_cnt = ctx.obj_created_cnt + 1;
                cur_constr_key = None;
                is_creation = false;
                constr_affilation = None;
                class_for_cast = None;
              }
            >>= fun initres_ctx ->
            let get_new_var_table =
              try
                prepare_table_with_args_exn KeyMap.empty creation_args
                  curr_constr.arguments ctx class_table
              with Invalid_argument m -> error m
            in
            get_new_var_table >>= fun (vt, _) ->
            prepare_constructor_block curr_constr.body obj_class
              curr_constr.call_constructor class_table
            >>= fun constr_body ->
            eval_stmt constr_body
              {
                initres_ctx with
                var_table = vt;
                is_creation = true;
                is_main_scope = false;
                constr_affilation = Some obj_class.this_key;
                cur_constr_key = Some curr_constr.key;
              }
              class_table
            >>= fun c_ctx ->
            return
              {
                ctx with
                last_expr_result = VObjectReference c_ctx.cur_object;
                runtime_signal = NoSignal;
                obj_created_cnt = c_ctx.obj_created_cnt;
              }
      | Assign (Identifier var_key, val_expr) ->
          eval_expr val_expr ctx class_table >>= fun val_evaled_ctx ->
          update_identifier_v var_key val_evaled_ctx.last_expr_result
            val_evaled_ctx
      | Assign (AccessByPoint (obj_expr, Identifier f_name), val_expr) ->
          eval_expr val_expr ctx class_table >>= fun val_evaled_ctx ->
          update_field_v obj_expr f_name val_evaled_ctx class_table
      | PostInc (AccessByPoint (obj_expr, Identifier f))
      | PrefInc (AccessByPoint (obj_expr, Identifier f)) ->
          eval_expr
            (Assign
               ( AccessByPoint (obj_expr, Identifier f),
                 Add (AccessByPoint (obj_expr, Identifier f), Value (VInt 1)) ))
            ctx class_table
      | PostInc (Identifier var_key) | PrefInc (Identifier var_key) ->
          eval_expr
            (Assign
               (Identifier var_key, Add (Identifier var_key, Value (VInt 1))))
            ctx class_table
      | PostDec (AccessByPoint (obj_expr, Identifier f))
      | PrefDec (AccessByPoint (obj_expr, Identifier f)) ->
          eval_expr
            (Assign
               ( AccessByPoint (obj_expr, Identifier f),
                 Sub (AccessByPoint (obj_expr, Identifier f), Value (VInt 1)) ))
            ctx class_table
      | PostDec (Identifier var_key) | PrefDec (Identifier var_key) ->
          eval_expr
            (Assign
               (Identifier var_key, Sub (Identifier var_key, Value (VInt 1))))
            ctx class_table
      | _ -> error "Wrong expression construction"
    in
    expr_type_check expr ectx class_table >> eval_e expr ectx

  and update_identifier_v var_key new_val val_evaled_ctx =
    (* If need update existing var in variable table *)
    if KeyMap.mem var_key val_evaled_ctx.var_table then
      get_elem_if_present_m var_key val_evaled_ctx.var_table >>= fun old_var ->
      check_assign_cnt_v old_var >>= fun _ ->
      let updated_var_table =
        update_element val_evaled_ctx.var_table var_key
          {
            old_var with
            v_value = new_val;
            assignment_count = old_var.assignment_count + 1;
          }
      in
      return { val_evaled_ctx with var_table = updated_var_table }
    else
      (* If need update field of current class *)
      match val_evaled_ctx.cur_object with
      | NullObjectReference -> error "NullReferenceException"
      | ObjectReference
          {
            class_key = key;
            ext_interface = interface;
            field_references_table = curr_frt;
            number = num;
          } as curr_obj ->
          if KeyMap.mem var_key curr_frt then
            get_elem_if_present_m var_key curr_frt >>= fun old_field ->
            check_assign_cnt_f old_field
            >>
            if val_evaled_ctx.is_creation then
              let updated_field_table =
                update_element curr_frt var_key
                  { old_field with field_value = new_val }
              in
              return
                {
                  val_evaled_ctx with
                  cur_object =
                    ObjectReference
                      {
                        class_key = key;
                        ext_interface = interface;
                        field_references_table = updated_field_table;
                        number = num;
                      };
                }
            else
              try
                update_object_state_exn val_evaled_ctx.cur_object var_key
                  val_evaled_ctx.last_expr_result val_evaled_ctx
                |> fun new_context -> return new_context
              with
              | Invalid_argument m -> error m
              | Not_found -> error "No such field"
          else error ("No such variable " ^ var_key)

  and update_field_v obj_expr field_name val_evaled_ctx class_table =
    eval_expr obj_expr val_evaled_ctx class_table >>= fun obj_evaled_ctx ->
    let obj_ref = get_obj_value obj_evaled_ctx.last_expr_result in
    (* obj_evaled_ctx stores the last result of the left side of the expression, the class to which we want to assign *)
    let new_val = val_evaled_ctx.last_expr_result in
    (* val_evaled_ctx stores the last result of the right side of the expression *)
    try
      get_object_info obj_ref |> fun (class_key, interface, frt, num) ->
      if KeyMap.mem field_name frt then
        get_elem_if_present_m field_name frt >>= fun old_field ->
        check_assign_cnt_f old_field
        >>
        if obj_evaled_ctx.is_creation then
          let updated_field_table =
            update_element frt field_name
              { old_field with field_value = new_val }
          in
          return
            {
              obj_evaled_ctx with
              cur_object =
                ObjectReference
                  {
                    class_key;
                    ext_interface = interface;
                    field_references_table = updated_field_table;
                    number = num;
                  };
            }
        else
          update_object_state_exn obj_ref field_name new_val obj_evaled_ctx
          |> fun new_context -> return new_context
      else
        error
          (String.concat " "
             [ "No such field"; field_name; "in class"; class_key ])
    with
    | Invalid_argument m | Failure m -> error m
    | Not_found -> error ("No such field " ^ field_name)

  (* Since we are updating a reference value, we need to update all references to this variable *)
  and update_object_state_exn obj field_key new_value update_ctx =
    let seq_field_ref_table frt = KeyMap.to_seq frt in
    let seq_var_table cont = KeyMap.to_seq cont.var_table in
    (* Iterate by class fields and update all reference to f_key with n_val *)
    let rec update_states field_seq f_key n_val o_num assign_cnt result =
      match field_seq () with
      | Seq.Nil -> result
      | Seq.Cons ((old_field_key, curr_field_ref), tail) -> (
          match curr_field_ref with
          | { field_value = f_val; _ } as old_field -> (
              match f_val with
              | VObjectReference
                  (ObjectReference
                    {
                      class_key = c_key;
                      ext_interface = interface;
                      field_references_table = frt;
                      number = fnum;
                    }) ->
                  if fnum = o_num then
                    let updated_fields_table =
                      match get_element_option f_key frt with
                      | None -> raise Not_found
                      | Some old_field ->
                          KeyMap.add f_key
                            {
                              old_field with
                              field_value = n_val;
                              assignments_count = assign_cnt;
                            }
                            frt
                    in
                    let updated_class =
                      ObjectReference
                        {
                          class_key = c_key;
                          ext_interface = interface;
                          field_references_table =
                            update_states
                              (seq_field_ref_table updated_fields_table)
                              f_key n_val o_num assign_cnt result;
                          number = fnum;
                        }
                    in
                    update_states tail f_key n_val o_num assign_cnt
                      (KeyMap.add old_field_key
                         {
                           old_field with
                           field_value = VObjectReference updated_class;
                         }
                         result)
                  else
                    update_states tail f_key n_val o_num assign_cnt
                      (KeyMap.add old_field_key old_field result)
              | _ ->
                  update_states tail f_key n_val o_num assign_cnt
                    (KeyMap.add old_field_key old_field result)))
    in
    (* Iterate by existing variables in varialbe table and update all reference to f_key with n_val *)
    let rec helper_update seq_variables f_key new_val obj_num assign_cnt result
        =
      match seq_variables () with
      | Seq.Nil -> result
      | Seq.Cons ((curr_var_key, curr_var), tail) -> (
          match curr_var.v_value with
          | VObjectReference
              (ObjectReference
                {
                  class_key = c_key;
                  ext_interface = interface;
                  field_references_table = frt;
                  number = fnum;
                }) ->
              if obj_num = fnum then
                let updated_fields_table =
                  match get_element_option f_key frt with
                  | None -> raise Not_found
                  | Some old_field ->
                      KeyMap.add f_key
                        {
                          old_field with
                          field_value = new_val;
                          assignments_count = assign_cnt;
                        }
                        frt
                in
                let updated_class =
                  ObjectReference
                    {
                      class_key = c_key;
                      ext_interface = interface;
                      field_references_table =
                        update_states
                          (seq_field_ref_table updated_fields_table)
                          f_key new_val obj_num assign_cnt KeyMap.empty;
                      number = fnum;
                    }
                in
                helper_update tail f_key new_val obj_num assign_cnt
                  (KeyMap.add curr_var_key
                     { curr_var with v_value = VObjectReference updated_class }
                     result)
              else
                let updated_class =
                  ObjectReference
                    {
                      class_key = c_key;
                      ext_interface = interface;
                      field_references_table =
                        update_states (seq_field_ref_table frt) f_key new_val
                          obj_num assign_cnt KeyMap.empty;
                      number = fnum;
                    }
                in
                helper_update tail f_key new_val obj_num assign_cnt
                  (KeyMap.add curr_var_key
                     { curr_var with v_value = VObjectReference updated_class }
                     result)
          | _ ->
              helper_update tail f_key new_val obj_num assign_cnt
                (KeyMap.add curr_var_key curr_var result))
    in
    get_object_info obj
    |> fun (obj_key, obj_interface, object_frt, object_number) ->
    (match get_element_option field_key object_frt with
    | None -> raise Not_found
    | Some f ->
        let updated_field_table =
          update_element object_frt field_key
            {
              f with
              field_value = new_value;
              assignments_count = f.assignments_count + 1;
            }
        in
        let updated_obj =
          ObjectReference
            {
              class_key = obj_key;
              ext_interface = obj_interface;
              field_references_table = updated_field_table;
              number = object_number;
            }
        in
        (updated_obj, f.assignments_count + 1))
    |> fun (updated_obj, assign_cnt) ->
    let new_var_table =
      helper_update (seq_var_table update_ctx) field_key new_value object_number
        assign_cnt KeyMap.empty
    in
    let prev_upd_ctx =
      match update_ctx.prev_context with
      | None -> None
      | Some prev_ctx ->
          let prev_ctx_upd_var_table =
            helper_update (seq_var_table prev_ctx) field_key new_value
              object_number assign_cnt KeyMap.empty
          in
          Some { prev_ctx with var_table = prev_ctx_upd_var_table }
    in
    let new_context =
      {
        update_ctx with
        cur_object = updated_obj;
        var_table = new_var_table;
        last_expr_result = VObjectReference updated_obj;
        prev_context = prev_upd_ctx;
      }
    in
    new_context

  (** Make table of variables by arguments. Concatenate arguments for calling method and base/this method/constructor and return variable table of args. Context doesn't update *)
  and prepare_table_with_args_exn ht args_l m_arg_list pr_ctx class_table =
    mfold_left2
      (fun (h_ht, hctx) arg -> function
        | head_type, Name head_name ->
            eval_expr arg hctx class_table >>= fun he_ctx ->
            let add_field =
              KeyMap.add head_name
                {
                  v_type = head_type;
                  v_key = head_name;
                  is_const = false;
                  assignment_count = 1;
                  v_value = he_ctx.last_expr_result;
                  vis_lvl = 0;
                }
                h_ht
            in
            return (add_field, he_ctx))
      (ht, pr_ctx) args_l m_arg_list

  (** Add call base/this constructor expression to statement body of constructor *)
  and prepare_constructor_block current_body current_class
      current_call_constructor class_table =
    match current_body with
    | StatementBlock body -> (
        let current_class_parent_key =
          find_parent_class_key current_class.parent_key class_table
        in
        match (current_call_constructor, current_class_parent_key) with
        | Some (CallMethod (Base, _)), None ->
            error "base() call in constructor in not child class"
        | Some (CallMethod (Base, _) as call_constructor), Some _ ->
            return (StatementBlock (Expression call_constructor :: body))
        | Some (CallMethod (This, _) as call_constructor), _ ->
            return (StatementBlock (Expression call_constructor :: body))
        | None, _ -> return current_body
        | _ -> error "Incorrect constructor call in constructor")
    | _ -> error "Must be statement block in constructor"

  let execute : table_class KeyMap.t -> context M.t =
   fun ht ->
    find_main_class ht >>= fun main_cl ->
    make_context
      (ObjectReference
         {
           class_key = main_cl.this_key;
           ext_interface = None;
           field_references_table = KeyMap.empty;
           number = 0;
         })
      KeyMap.empty
    >>= fun ctx ->
    let main = KeyMap.find "Main" main_cl.methods_table in
    match main.body with
    | None -> error "Main() method cannot be abstract"
    | Some body_main -> eval_stmt body_main ctx ht
end
