(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open KeyMap

(** SUM operation *)
let ( ++ ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VInt (left_value + right_value)
  | VString left_value, VString right_value -> VString (left_value ^ right_value)
  | VInt left_value, VString right_value ->
      VString (string_of_int left_value ^ right_value)
  | VString left_value, VInt right_value ->
      VString (left_value ^ string_of_int right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < + >")

let%test _ = VInt 1 ++ VInt 2 = VInt 3
let%test _ = VString "1" ++ VString "2" = VString "12"
let%test _ = VInt 1 ++ VString "2" = VString "12"
let%test _ = VString "1" ++ VInt 2 = VString "12"

(** SUB operation *)
let ( -- ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VInt (left_value - right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < - >")

let%test _ = VInt 1 -- VInt 2 = VInt (-1)

(** MULT operation *)
let ( ** ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VInt (left_value * right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < * >")

let%test _ = VInt 1 ** VInt 2 = VInt 2

(** DIV operation *)
let ( // ) left right =
  match (left, right) with
  | VInt _, VInt right_value when right_value = 0 -> raise Division_by_zero
  | VInt left_value, VInt right_value -> VInt (left_value / right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < / >")

let%test _ = VInt 1 // VInt 2 = VInt 0

(** MOD operation *)
let ( %% ) left right =
  match (left, right) with
  | VInt _, VInt right_value when right_value = 0 -> raise Division_by_zero
  | VInt left_value, VInt right_value -> VInt (left_value mod right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < % >")

let%test _ = VInt 1 %% VInt 2 = VInt 1

(** More *)
let ( >>> ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value > right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (>) >")

let%test _ = VInt 1 >>> VInt 2 = VBool false
let%test _ = VInt 2 >>> VInt 2 = VBool false
let%test _ = VInt 3 >>> VInt 2 = VBool true

(** Less *)
let ( <<< ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value < right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (<) >")

let%test _ = VInt 1 <<< VInt 2 = VBool true
let%test _ = VInt 2 <<< VInt 2 = VBool false
let%test _ = VInt 3 <<< VInt 2 = VBool false

(** More or equal *)
let ( >>== ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value >= right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (>=) >")

let%test _ = VInt 1 >>== VInt 2 = VBool false
let%test _ = VInt 2 >>== VInt 2 = VBool true
let%test _ = VInt 3 >>== VInt 2 = VBool true

(** Less or equal *)
let ( ==<< ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value <= right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < (<=) >")

let%test _ = VInt 1 ==<< VInt 2 = VBool true
let%test _ = VInt 2 ==<< VInt 2 = VBool true
let%test _ = VInt 3 ==<< VInt 2 = VBool false

(** Logic AND *)
let ( &&& ) left right =
  match (left, right) with
  | VBool left_value, VBool right_value -> VBool (left_value && right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < && >")

let%test _ = VBool false &&& VBool false = VBool false
let%test _ = VBool false &&& VBool true = VBool false
let%test _ = VBool true &&& VBool false = VBool false
let%test _ = VBool true &&& VBool true = VBool true

(** Logic OR *)
let ( ||| ) left right =
  match (left, right) with
  | VBool left_value, VBool right_value -> VBool (left_value || right_value)
  | _ -> raise (Invalid_argument "Wrong argument types in < || >")

let%test _ = VBool false ||| VBool false = VBool false
let%test _ = VBool false ||| VBool true = VBool true
let%test _ = VBool true ||| VBool false = VBool true
let%test _ = VBool true ||| VBool true = VBool true

(** Logic NOT *)
let ( !!! ) = function
  | VBool value -> VBool (not value)
  | _ -> raise (Invalid_argument "Wrong types for < ! >")

let%test _ = !!!(VBool false) = VBool true
let%test _ = !!!(VBool true) = VBool false

(** Is equal *)
let ( === ) left right =
  match (left, right) with
  | VInt left_value, VInt right_value -> VBool (left_value = right_value)
  | VBool left_value, VBool right_value -> VBool (left_value = right_value)
  | VVoid, VVoid -> VBool true
  | VString left_value, VString right_value -> VBool (left_value = right_value)
  | VObjectReference left_value, VObjectReference right_value -> (
      match (left_value, right_value) with
      | NullObjectReference, NullObjectReference -> VBool true
      | NullObjectReference, _ | _, NullObjectReference -> VBool false
      | ( ObjectReference
            {
              class_key = _;
              ext_interface = _;
              field_references_table = _;
              number = left_number;
            },
          ObjectReference
            {
              class_key = _;
              ext_interface = _;
              field_references_table = _;
              number = right_number;
            } ) ->
          VBool (left_number = right_number))
  | _ -> raise (Invalid_argument "Wrong types in < == >")

let%test _ = VInt 1 === VInt 2 = VBool false
let%test _ = VInt 2 === VInt 2 = VBool true
let%test _ = VInt 3 === VInt 2 = VBool false
let%test _ = VBool false === VBool false = VBool true
let%test _ = VBool false === VBool true = VBool false
let%test _ = VBool true === VBool false = VBool false
let%test _ = VBool true === VBool true = VBool true
let%test _ = VVoid === VVoid = VBool true
let%test _ = VString "1" === VString "2" = VBool false
let%test _ = VString "2" === VString "2" = VBool true
let%test _ = VString "22" === VString "2" = VBool false

let%test _ =
  VObjectReference NullObjectReference === VObjectReference NullObjectReference
  = VBool true

let%test _ =
  VObjectReference NullObjectReference
  === VObjectReference
        (ObjectReference
           {
             class_key = "Test";
             ext_interface = None;
             field_references_table = KeyMap.empty;
             number = 1;
           })
  = VBool false

let%test _ =
  VObjectReference
    (ObjectReference
       {
         class_key = "Test";
         ext_interface = None;
         field_references_table = KeyMap.empty;
         number = 1;
       })
  === VObjectReference
        (ObjectReference
           {
             class_key = "Test";
             ext_interface = None;
             field_references_table = KeyMap.empty;
             number = 1;
           })
  = VBool true

let%test _ =
  VObjectReference
    (ObjectReference
       {
         class_key = "Test";
         ext_interface = None;
         field_references_table = KeyMap.empty;
         number = 1;
       })
  === VObjectReference
        (ObjectReference
           {
             class_key = "Test";
             ext_interface = None;
             field_references_table = KeyMap.empty;
             number = 2;
           })
  = VBool false

(** Is not equal *)
let ( !=! ) left right = !!!(left === right)
