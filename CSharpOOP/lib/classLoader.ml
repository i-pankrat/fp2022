(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open KeyMap
open Parser
open ResultMonad

let get_type_list = List.map fst

let get_parent_key_list_from_key (method_name : string) =
  match String.split_on_char '.' method_name with
  | [] -> [] (* empty string *)
  | _ :: [] -> [] (* method name *)
  | parent_key :: _ -> [ parent_key ]
(* parent interface key :: method name*)

let make_method_key method_name arguments =
  String.concat "" (method_name :: List.map show_types (get_type_list arguments))

let make_constructor_key constructor_name arguments =
  String.concat ""
    (constructor_name :: List.map show_types (get_type_list arguments))

let rec convert_name_list_to_string_list = function
  | Name x :: xs -> [ x ] @ convert_name_list_to_string_list xs
  | [] -> []

let equals_key = make_method_key "Equals" [ (TRef "Object", Name "obj") ]
let to_string_key = make_method_key "ToString" []

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_public = List.mem Public
  let is_private = List.mem Private
  let is_protected = List.mem Protected
  let is_abstract = List.mem Abstract
  let is_static = List.mem Static
  let is_const = List.mem Const
  let is_virtual = List.mem Virtual
  let is_override = List.mem Override
  let is_new = List.mem New

  (** Return false if modifiers have public&private, public&protected, private&protected *)
  let check_mutually_exclusive_modifiers modifier_list =
    (not (is_public modifier_list))
    && (not (is_protected modifier_list))
    && is_private modifier_list
    || (not (is_public modifier_list))
       && is_protected modifier_list
       && not (is_private modifier_list)
    || is_public modifier_list
       && (not (is_protected modifier_list))
       && not (is_private modifier_list)

  let get_element_option key hashtable = KeyMap.find_opt key hashtable

  let update_element hashtable old_key new_value =
    KeyMap.add old_key new_value hashtable

  let rec seq_iter seq action base =
    match seq () with
    | Seq.Nil -> return base
    | Seq.Cons (x, next) -> action x >> seq_iter next action base

  let merge_map_function _ v1 v2 =
    match (v1, v2) with
    | None, None -> None
    | v, None -> v
    | None, v -> v
    | v1, _ -> v1

  (** Add hardcoded class Object and its methods Equals() and ToString() to hashtable *)
  let prepare_object_class hashtable =
    let constructors_table = KeyMap.empty in
    let fields_table = KeyMap.empty in
    let methods_table = KeyMap.empty in
    let equals =
      {
        method_type = TInt;
        method_modifiers = [ Virtual; Public ];
        arguments = [ (TRef "Object", Name "obj") ];
        key = equals_key;
        interface_key = None;
        is_overriden = false;
        body =
          apply Statement.statement_block
            {|
              {
                if(this == obj) return 1;
                else return 0;
              }
            |};
      }
    in
    let to_string =
      {
        method_type = TString;
        method_modifiers = [ Virtual; Public ];
        arguments = [];
        key = to_string_key;
        interface_key = None;
        is_overriden = false;
        body =
          apply Statement.statement_block
            {|
              {
                return "Object";
              }
            |};
      }
    in
    let methods_table' = KeyMap.add equals.key equals methods_table in
    let methods_table'' = KeyMap.add to_string.key to_string methods_table' in
    let get_declaration_tree =
      match
        apply Object.object_decl
          {|
                public class Object 
                {
                    public int Equals(Object obj) 
                    {
                      if (this == obj) return 1;
                      else return 0;
                    }
                          
                    public string ToString() 
                    {
                      return "Object";
                    }
                  }
            |}
      with
      | Some decl_tree -> return decl_tree
      | None -> error "Error in parsing Object class"
    in
    get_declaration_tree >>= fun declaration_tree ->
    let object_hardcode =
      {
        this_key = "Object";
        fields_table;
        methods_table = methods_table'';
        constructors_table;
        children_keys = [];
        is_abstract = false;
        is_static = false;
        is_class = true;
        parent_key = [];
        decl_tree = declaration_tree;
      }
    in
    return (KeyMap.add object_hardcode.this_key object_hardcode hashtable)

  (** Checking modifiers for class methods --- list of modifiers (e.g. const, static, public, etc.), parsed object element (field/method/constructor) *)
  let check_modifiers_class_element = function
    | modifier_list, field_method_constructor -> (
        match field_method_constructor with
        | Method (_, _, _, _) when is_const modifier_list ->
            error "Methods cannot be const"
        | Method (_, _, _, _)
          when is_abstract modifier_list && is_virtual modifier_list ->
            error "Abstract methods are virtual by default"
        | Method (_, _, _, _)
          when is_abstract modifier_list && is_override modifier_list ->
            error "Abstract methods cannot be override"
        | Method (_, _, _, _)
          when is_virtual modifier_list && is_override modifier_list ->
            error "Virtual and override - mutually exclusive modifiers"
        | Method (_, _, _, _)
          when (is_virtual modifier_list && is_private modifier_list)
               || (is_abstract modifier_list && is_private modifier_list) ->
            error "Virtual and abstract methods can not be private"
        | Method (_, _, _, _)
          when not (check_mutually_exclusive_modifiers modifier_list) ->
            error
              "Public, protected and private is mutually exclusive modifiers \
               for method"
        | Method (TVoid, Name "Main", [], _)
          when is_static modifier_list
               && (not (is_abstract modifier_list))
               && (not (is_virtual modifier_list))
               && (not (is_override modifier_list))
               && (not (is_private modifier_list))
               && (not (is_protected modifier_list))
               && is_public modifier_list ->
            return ()
        | Method (_, Name "Main", _, _) when not (is_public modifier_list) ->
            error "Main method must be public"
        | Method (_, Name "Main", _, _) ->
            error "Only one Main method can be in the program"
        | Method (_, _, _, _) when is_static modifier_list ->
            error "Static methods other than Main are not supported"
        | Method (_, _, _, _) -> return ()
        | Field (_, _)
          when not (check_mutually_exclusive_modifiers modifier_list) ->
            error
              "Public, protected and private is mutually exclusive modifiers \
               for field"
        | Field (_, _)
          when (not (is_static modifier_list))
               && (not (is_abstract modifier_list))
               && (not (is_virtual modifier_list))
               && not (is_override modifier_list) ->
            return ()
        | Field (_, _) -> error "Wrong field modifiers"
        | Constructor (_, _, _, _)
          when is_public modifier_list
               && (not (is_static modifier_list))
               && (not (is_abstract modifier_list))
               && (not (is_const modifier_list))
               && (not (is_virtual modifier_list))
               && not (is_override modifier_list) ->
            return ()
        | Constructor (_, _, _, _) -> error "Wrong constructor modifiers")

  (** Checking modifiers for interface methods --- list of modifiers (e.g. const, static, public, etc.), parsed object element (field/method/constructor) *)
  let check_modifiers_interface_element = function
    | modifier_list, field_method_constructor -> (
        match field_method_constructor with
        | Method (_, _, _, _)
          when is_private modifier_list || is_protected modifier_list
               || is_static modifier_list || is_abstract modifier_list
               || is_virtual modifier_list || is_override modifier_list
               || is_const modifier_list || is_new modifier_list ->
            error
              "Interface methods cannot be \
               private/protected/static/abstract/virtual/override/const/new"
        | Method (_, Name "Main", _, _) ->
            error
              "Interface cannot contain Main method. Supports only one Main \
               method"
        | Method (_, _, _, _) -> return ()
        | Field (_, _) -> error "Interfaces cannot have fields"
        | Constructor (_, _, _, _) ->
            error "Interfaces cannot have constructors")

  let check_modifiers_object = function
    | Class (modifier_list, _, _, _) when is_const modifier_list ->
        error "Classes cannot be const"
    | Class (modifier_list, _, _, _)
      when (not (is_static modifier_list))
           && (not (is_virtual modifier_list))
           && (not (is_override modifier_list))
           && (not (is_private modifier_list))
           && not (is_protected modifier_list) ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"
    | Interface (modifier, _, _, _)
      when (not (is_static [ modifier ]))
           && (not (is_virtual [ modifier ]))
           && (not (is_const [ modifier ]))
           && (not (is_private [ modifier ]))
           && (not (is_protected [ modifier ]))
           && (not (is_new [ modifier ]))
           && not (is_override [ modifier ]) ->
        return ()
    | Interface (_, _, _, _) -> error "Wrong interface modifiers"

  let many_add_objects object_list_ast object_map =
    let add_class_in_map cl_map adding_class =
      check_modifiers_object adding_class
      >>
      match adding_class with
      | Class (class_modifier_list, Name class_key, class_parent, fields) -> (
          (* Function of adding a class element to the corresponding map *)
          let add_class_elem field_elem field_map method_map constructor_map =
            check_modifiers_class_element field_elem
            >>
            match field_elem with
            (* Create field map for class *)
            | field_modifier_list, Field (field_type, arg_list) ->
                let rec helper_add_var list field_map method_map constructor_map
                    =
                  match list with
                  | [] -> return (field_map, method_map, constructor_map)
                  | (Name key, sub_tree) :: ps ->
                      let table_field =
                        {
                          field_type;
                          key;
                          field_modifiers = field_modifier_list;
                          sub_tree;
                        }
                      in
                      (match KeyMap.find_opt key field_map with
                      | None ->
                          let field_map' =
                            KeyMap.add key table_field field_map
                          in
                          return (field_map', method_map, constructor_map)
                      | _ ->
                          error
                            (String.concat " "
                               [
                                 "The field with this key:";
                                 key;
                                 "already exists";
                               ]))
                      >>= fun (field_m, method_m, constructor_m) ->
                      helper_add_var ps field_m method_m constructor_m
                in
                helper_add_var arg_list field_map method_map constructor_map
            (* Create method map for class *)
            | ( method_modifier_list,
                Method (method_type, Name method_key, args, body) ) -> (
                let key = make_method_key method_key args in
                let is_abstract_class = is_abstract class_modifier_list in
                let is_abstract' = is_abstract method_modifier_list in
                let is_abstract_body =
                  match is_abstract' with
                  | true -> (
                      match is_abstract_class with
                      | false -> error "Abstract method in non-abstract class"
                      | true -> (
                          match body with
                          | Some _ -> error "Abstract method cannot have body"
                          | None -> return ()))
                  | false -> (
                      match body with
                      | Some _ -> return ()
                      | None -> error "Body missing in non-abstract method")
                in
                let method_parent =
                  match get_parent_key_list_from_key method_key with
                  | [] -> None
                  | parent_key :: _ -> Some parent_key
                in
                let table_method =
                  {
                    method_type;
                    method_modifiers = method_modifier_list;
                    arguments = args;
                    key;
                    interface_key = method_parent;
                    is_overriden = false;
                    body;
                  }
                in
                is_abstract_body
                >>
                match KeyMap.find_opt key method_map with
                | None ->
                    let method_map' = KeyMap.add key table_method method_map in
                    return (field_map, method_map', constructor_map)
                | _ ->
                    error
                      (String.concat " "
                         [ "The method with this key:"; key; "already exists" ])
                )
            (* Create constructor map for class *)
            | _, Constructor (Name name, arguments, call_constructor, body) -> (
                let constructor_key = make_constructor_key name arguments in
                let match_name_with_class =
                  if name = class_key then return ()
                  else error "Constructor name does not match class name"
                in
                let table_constructor =
                  { key = constructor_key; arguments; call_constructor; body }
                in
                match_name_with_class
                >>
                match KeyMap.find_opt constructor_key constructor_map with
                | None ->
                    let constructor_map' =
                      KeyMap.add constructor_key table_constructor
                        constructor_map
                    in
                    return (field_map, method_map, constructor_map')
                | _ ->
                    error
                      (String.concat " "
                         [
                           "The constructor with this key:";
                           constructor_key;
                           "already exists";
                         ]))
          in
          (* Iterate by class fields *)
          let rec iter_fields fields field_map method_map constructor_map =
            match fields with
            | [] -> return (field_map, method_map, constructor_map)
            | x :: xs ->
                add_class_elem x field_map method_map constructor_map
                >>= fun (field_m, method_m, constructors_m) ->
                iter_fields xs field_m method_m constructors_m
          in
          iter_fields fields KeyMap.empty KeyMap.empty KeyMap.empty
          >>= fun (field_map, method_map, constructor_map) ->
          (* All classes are children of Class Object *)
          let add_parent parent =
            match parent with
            | [] -> [ "Object" ]
            | _ -> convert_name_list_to_string_list parent
          in
          let is_abstract' = is_abstract class_modifier_list in
          let is_static' = is_static class_modifier_list in
          let parent_key = add_parent class_parent in
          match KeyMap.find_opt class_key cl_map with
          | None ->
              let class_t =
                {
                  this_key = class_key;
                  fields_table = field_map;
                  methods_table = method_map;
                  constructors_table = constructor_map;
                  children_keys = [];
                  is_static = is_static';
                  is_abstract = is_abstract';
                  is_class = true;
                  parent_key;
                  decl_tree = adding_class;
                }
              in
              let cl_map = KeyMap.add class_key class_t cl_map in
              return cl_map
          | _ ->
              error
                (String.concat " "
                   [ "The class with this key:"; class_key; "already exists" ]))
      | Interface (_, Name interface_key, interface_parent, fields) -> (
          let add_class_elem field_elem field_map method_map constructor_map =
            check_modifiers_interface_element field_elem
            >>
            match field_elem with
            (* Create method map for class *)
            | _, Method (method_type, Name method_key, args, body) -> (
                let key = make_method_key method_key args in
                let is_body =
                  match body with
                  | Some _ -> error "Interface method cannot have body"
                  | None -> return ()
                in
                let method_parent =
                  match get_parent_key_list_from_key method_key with
                  | [] -> None
                  | parent_key :: _ -> Some parent_key
                in
                let table_method =
                  {
                    method_type;
                    method_modifiers = [ Abstract; Virtual; Public ];
                    arguments = args;
                    key;
                    interface_key = method_parent;
                    is_overriden = false;
                    body;
                  }
                in
                is_body
                >>
                match KeyMap.find_opt key method_map with
                | None ->
                    let method_map' = KeyMap.add key table_method method_map in
                    return (field_map, method_map', constructor_map)
                | _ ->
                    error
                      (String.concat " "
                         [ "The method with this key:"; key; "already exists" ])
                )
            (* Interface doesn't support fields and constructors *)
            | _, Field (_, _) -> error "Interface cannot have fields"
            | _, Constructor (_, _, _, _) ->
                error "Interface cannot have constructors"
          in
          (* Iterate by interface fields *)
          let rec iter_fields fields field_map method_map constructor_map =
            match fields with
            | [] -> return (field_map, method_map, constructor_map)
            | x :: xs ->
                add_class_elem x field_map method_map constructor_map
                >>= fun (field_m, method_m, constructors_m) ->
                iter_fields xs field_m method_m constructors_m
          in
          iter_fields fields KeyMap.empty KeyMap.empty KeyMap.empty
          >>= fun (field_map, method_map, constructor_map) ->
          (* Interface doesn't have default parent *)
          let add_parent parent =
            match parent with
            | [] -> [ "Object" ]
            | _ -> convert_name_list_to_string_list parent
          in
          let parent_key = add_parent interface_parent in
          match KeyMap.find_opt interface_key cl_map with
          | None ->
              let interface_t =
                {
                  this_key = interface_key;
                  fields_table = field_map;
                  methods_table = method_map;
                  constructors_table = constructor_map;
                  children_keys = [];
                  is_static = false;
                  is_abstract = false;
                  is_class = false;
                  parent_key;
                  decl_tree = adding_class;
                }
              in
              let cl_map = KeyMap.add interface_key interface_t cl_map in
              return cl_map
          | _ ->
              error
                (String.concat " "
                   [
                     "The interface with this key:";
                     interface_key;
                     "already exists";
                   ]))
    in
    (* Iterate by objects in AST *)
    let rec iter_classes object_list_ast class_map =
      match object_list_ast with
      | [] -> return class_map
      | x :: xs ->
          add_class_in_map class_map x >>= fun class_m ->
          iter_classes xs class_m
    in
    iter_classes object_list_ast object_map >>= fun class_m -> return class_m

  (** Make default (empty) constructor for class *)
  let get_default_constructor table_class =
    let constructor_key = make_constructor_key table_class.this_key [] in
    let table_constructor =
      {
        key = constructor_key;
        arguments = [];
        call_constructor = None;
        body = StatementBlock [];
      }
    in
    KeyMap.add table_class.this_key table_constructor KeyMap.empty

  (** Add default (empty) constructor to class for all non-static classes in sequence *)
  let rec build_new_table_class seq_classes result =
    match seq_classes () with
    | Seq.Nil -> result
    | Seq.Cons ((key, x), next) ->
        let x_default_constructor = get_default_constructor x in
        let new_class = { x with constructors_table = x_default_constructor } in
        let result' = Seq.append (Seq.return (key, new_class)) result in
        if x.is_static then build_new_table_class next result
        else build_new_table_class next result'

  (** Add an empty constructor to all classes that don't have a constructor *)
  let many_add_default_constructor ht =
    let classes_without_constructors =
      KeyMap.filter
        (fun _ table_class ->
          KeyMap.cardinal table_class.constructors_table = 0
          && table_class.is_class)
        ht
    in
    let classes_with_constructors =
      KeyMap.filter
        (fun _ table_class ->
          KeyMap.cardinal table_class.constructors_table <> 0
          || not table_class.is_class)
        ht
    in
    let seq_classes = KeyMap.to_seq classes_without_constructors in
    let updated_classes =
      build_new_table_class seq_classes Seq.empty |> KeyMap.of_seq
    in
    let ht_with_all_constructors =
      KeyMap.merge merge_map_function classes_with_constructors updated_classes
    in
    return ht_with_all_constructors

  (** Checks the list of parents for the existence of parents -- table of all classes, list of parent keys and is already inheritance from class flag *)
  let rec check_children ht parent_key_list already_from_class =
    match parent_key_list with
    | [] -> return ()
    | parent_key :: tail -> (
        let parent = get_element_option parent_key ht in
        let check_parent =
          match parent with
          | None -> error "No parent class found"
          | Some parent
            when (not parent.is_static)
                 && ((parent.is_class && not already_from_class)
                    || not parent.is_class) ->
              return ()
          | Some parent when parent.is_static ->
              error "Static class cannot be inherited"
          | Some parent when parent.is_class && not already_from_class ->
              error "Static class cannot be inherited"
          | Some _ -> error "Classes cannot have two or more class parents"
        in
        check_parent
        >>
        match parent with
        | None -> return ()
        | Some parent when parent.is_class -> check_children ht tail true
        | Some _ -> check_children ht tail already_from_class)

  (** Iterate by all class sequence and check parent list for errors *)
  let rec iterate_for_checking_childs ht seq_classes =
    match seq_classes () with
    | Seq.Nil -> return ()
    | Seq.Cons ((_, children), next) -> (
        match children.parent_key with
        | [] -> iterate_for_checking_childs ht next
        | parent_list ->
            check_children ht parent_list false
            >> iterate_for_checking_childs ht next)

  (** Create class table with updated children key field. Without any check *)
  let rec build_parents_table ht parent_key_list children_key result =
    match parent_key_list with
    | [] -> result
    | parent_key :: tail -> (
        (* Search parent in already added to new table *)
        let parent = get_element_option parent_key result in
        match parent with
        | None -> build_parents_table ht tail children_key result
        | Some parent ->
            let updated_parent =
              {
                parent with
                children_keys = children_key :: parent.children_keys;
              }
            in
            build_parents_table ht tail children_key
              (KeyMap.add parent_key updated_parent result))

  (** Iterate by all classes in sequence and update their parents 'children_keys' field*)
  let rec iterate_for_add_children ht seq_classes result =
    match seq_classes () with
    | Seq.Nil -> result
    | Seq.Cons ((key, children), next) ->
        let new_parent_table =
          match children.parent_key with
          | [] -> (
              let alredy_added = get_element_option key result in
              match alredy_added with
              | None -> KeyMap.add key children KeyMap.empty
              | Some _ -> KeyMap.empty)
          | parent_list -> build_parents_table ht parent_list key result
        in
        let result' = KeyMap.merge merge_map_function new_parent_table result in
        iterate_for_add_children ht next result'

  (** Update children keys for all parents *)
  let many_add_children ht =
    let seq_classes = KeyMap.to_seq ht in
    let updated_classes = iterate_for_add_children ht seq_classes ht in
    iterate_for_checking_childs ht seq_classes >> return updated_classes

  (*Functions for implementing inheritance*)

  let rec build_new_children_fields (seq_fields : (string * table_field) Seq.t)
      (children : table_class) (result : table_class KeyMap.t) =
    match seq_fields () with
    | Seq.Nil -> (result, children)
    | Seq.Cons ((key, parent_field), next) ->
        let new_fields_table =
          match get_element_option key children.fields_table with
          | None ->
              if is_private parent_field.field_modifiers then
                children.fields_table
              else
                KeyMap.add parent_field.key parent_field children.fields_table
          | Some _ -> children.fields_table
        in
        let new_children = { children with fields_table = new_fields_table } in
        build_new_children_fields next new_children
          (KeyMap.add children.this_key new_children result)

  let many_field_inheritance ht parent children =
    let seq_parent_fields = KeyMap.to_seq parent.fields_table in
    return (build_new_children_fields seq_parent_fields children ht)

  let is_base_method = function CallMethod (Base, _) -> true | _ -> false
  let is_this_method = function CallMethod (This, _) -> true | _ -> false

  let many_check_parent_constructor ht parent =
    let check_call_constructor (class_key, constructor) =
      match constructor.call_constructor with
      | Some call_constructor -> (
          match is_this_method call_constructor with
          | true -> return ()
          | false ->
              error
                (String.concat " "
                   [
                     "The called method must be a constructor of";
                     class_key;
                     "class";
                   ]))
      | None -> return ()
    in
    let seq_parent_constructors = KeyMap.to_seq parent.constructors_table in
    seq_iter seq_parent_constructors check_call_constructor ht

  let many_check_children_constructor ht children =
    let check_call_constructor (class_key, constructor) =
      match constructor.call_constructor with
      | Some call_constructor -> (
          match
            is_this_method call_constructor || is_base_method call_constructor
          with
          | true -> return ()
          | false ->
              error
                (String.concat " "
                   [
                     "The called method must be a constructor of";
                     class_key;
                     "or parent class";
                   ]))
      | None -> return ()
    in
    let seq_parent_constructors = KeyMap.to_seq children.constructors_table in
    seq_iter seq_parent_constructors check_call_constructor ht

  (** Iterate by all methods in parent sequence and check children methods for errors *)
  let rec iterate_for_checking_methods (ht : table_class KeyMap.t)
      (children : table_class) (seq_methods : (string * table_method) Seq.t) =
    match seq_methods () with
    | Seq.Nil -> return ()
    | Seq.Cons ((parent_method_key, parent_method), next) -> (
        match get_element_option parent_method_key children.methods_table with
        | None when is_abstract parent_method.method_modifiers ->
            if children.is_abstract then
              iterate_for_checking_methods ht children next
            else
              error
                (String.concat " "
                   [ "Abstract method"; parent_method_key; "must be overriden" ])
        | None when not (is_abstract parent_method.method_modifiers) ->
            iterate_for_checking_methods ht children next
        | _ -> iterate_for_checking_methods ht children next)

  let rec build_new_children_methods
      (seq_methods : (string * table_method) Seq.t) (parent : table_class)
      (children : table_class) (result : table_class KeyMap.t) =
    match seq_methods () with
    | Seq.Nil -> (result, children)
    | Seq.Cons ((parent_method_key, parent_method), next) ->
        let new_methods_table =
          match
            get_element_option
              (String.concat "" [ parent.this_key; "."; parent_method_key ])
              children.methods_table
            (* children method name looks like IInterface.Method()*)
          with
          | None -> (
              match parent_method.interface_key with
              | None -> (
                  match
                    get_element_option parent_method_key children.methods_table
                  with
                  | None when is_abstract parent_method.method_modifiers ->
                      if children.is_abstract then
                        KeyMap.add parent_method_key parent_method
                          children.methods_table
                      else children.methods_table
                        (* Error branch, abstract methods must be overriden *)
                  | None when not (is_abstract parent_method.method_modifiers)
                    ->
                      KeyMap.add parent_method_key parent_method
                        children.methods_table
                  | Some child_overriden_method ->
                      KeyMap.add parent_method_key
                        { child_overriden_method with is_overriden = true }
                        children.methods_table
                  | _ -> children.methods_table)
              | Some _ -> children.methods_table)
          | Some child_overriden_method ->
              KeyMap.add
                (String.concat "" [ parent.this_key; "."; parent_method_key ])
                { child_overriden_method with is_overriden = true }
                children.methods_table
        in
        let new_children =
          { children with methods_table = new_methods_table }
        in
        let children_is_class =
          if children.is_class then
            build_new_children_methods next parent new_children
              (KeyMap.add children.this_key new_children result)
          else build_new_children_methods next parent children result
        in
        children_is_class

  (** Adds explicitly unimplemented methods of parent classes to children *)
  let many_method_inheritance ht parent children =
    let seq_parent_methods = KeyMap.to_seq parent.methods_table in
    iterate_for_checking_methods ht children seq_parent_methods
    >> return (build_new_children_methods seq_parent_methods parent children ht)

  let many_check_method_override ht parent children =
    let not_change_access_modifiers parent_m children_m =
      is_public parent_m.method_modifiers
      && is_public children_m.method_modifiers
      || is_protected parent_m.method_modifiers
         && is_protected children_m.method_modifiers
      || is_private parent_m.method_modifiers
         && is_private children_m.method_modifiers
    in
    let check_children_method_override :
        table_class -> string * table_method -> unit t =
     fun parent (children_method_key, children_method) ->
      match is_override children_method.method_modifiers with
      | false -> return ()
      | true -> (
          match get_element_option children_method_key parent.methods_table with
          | None ->
              error
                (String.concat " "
                   [
                     "Cannot override non-existent";
                     children_method_key;
                     "method in parent";
                   ])
          | Some parent_method -> (
              match
                is_virtual parent_method.method_modifiers
                || is_abstract parent_method.method_modifiers
                || is_override parent_method.method_modifiers
              with
              | true
                when not_change_access_modifiers parent_method children_method
                ->
                  return ()
              (* Otherwise -- access modifiers changed *)
              | true ->
                  error
                    (String.concat ""
                       [
                         "Cannot change access modifiers when overriding ";
                         parent.this_key;
                         ".";
                         children_method_key;
                       ])
              (* Parent method not virtual/abstract/override *)
              | false ->
                  error
                    (String.concat " "
                       [
                         "Cannot override non-virtual or non-abstract";
                         children_method_key;
                         "method in";
                         parent.this_key;
                         "parent class";
                       ])))
    in
    let seq_children_methods = KeyMap.to_seq children.methods_table in
    seq_iter seq_children_methods (check_children_method_override parent) ht

  (** Checks that classes implement all methods of parent interfaces *)
  let many_check_interface_inheritance ht parent children =
    let check_parent_interface_inheritance :
        table_class -> table_class -> string * table_method -> unit t =
     fun curr_parent curr_children (parent_method_key, _) ->
      match
        get_element_option parent_method_key curr_children.methods_table
      with
      | None -> (
          match
            get_element_option
              (String.concat ""
                 [ curr_parent.this_key; "."; parent_method_key ])
              curr_children.methods_table
            (* children method name looks like IInterface.Method()*)
          with
          | None ->
              error
                (String.concat " "
                   [
                     "Class";
                     curr_children.this_key;
                     "doesn't realize parent";
                     curr_parent.this_key;
                     "interface";
                     parent_method_key;
                     "method";
                   ])
          | Some _ -> return ())
      | Some _ -> return ()
    in
    let seq_parent_methods = KeyMap.to_seq parent.methods_table in
    let parent_is_class =
      if parent.is_class then return ht
      else
        seq_iter seq_parent_methods
          (check_parent_interface_inheritance parent children)
          ht
    in
    parent_is_class

  let rec iter_by_childs ht action_func = function
    | [] -> return ht
    | children_key :: tail ->
        action_func children_key ht >>= fun new_ht ->
        iter_by_childs new_ht action_func tail

  let rec transfert ht parent children =
    many_check_interface_inheritance ht parent children
    >> many_field_inheritance ht parent children
    >>= fun (ht_with_fields, children_with_fields) ->
    many_method_inheritance ht_with_fields parent children_with_fields
    >>= fun (full_updated_ht, full_updated_children) ->
    many_check_method_override full_updated_ht parent full_updated_children
    >> many_check_parent_constructor full_updated_ht parent
    >> many_check_children_constructor full_updated_ht full_updated_children
    >> transfert_on_children full_updated_ht full_updated_children

  and transfert_on_children ht children =
    let childrens_of_children = children.children_keys in
    let rec iter_by_childs ht' = function
      | [] -> return ht'
      | children_children_key :: tail -> (
          match get_element_option children_children_key ht' with
          | None ->
              error
                (String.concat " "
                   [
                     "There is no";
                     children_children_key;
                     "children in class table";
                   ])
          | Some children_of_children ->
              transfert ht' children children_of_children >>= fun updated_ht ->
              iter_by_childs updated_ht tail)
    in
    iter_by_childs ht childrens_of_children

  let inheritance ht =
    match get_element_option "Object" ht with
    | None -> error "There is no Object class =("
    | Some object_class ->
        let processing children_key ht =
          match get_element_option children_key ht with
          | None ->
              error
                (String.concat " "
                   [ "There is no"; children_key; "children in class table" ])
          | Some children -> transfert ht object_class children
        in
        iter_by_childs ht processing object_class.children_keys

  let load class_list class_table =
    match class_list with
    | [] -> error "Syntax error or empty file"
    | _ ->
        prepare_object_class class_table >>= fun class_table_with_object ->
        many_add_objects class_list class_table_with_object
        >>= fun class_table_with_classes ->
        many_add_default_constructor class_table_with_classes
        >>= fun class_table_without_inheritance ->
        many_add_children class_table_without_inheritance
        >>= fun class_table_ready_to_inheritance ->
        inheritance class_table_ready_to_inheritance
end
