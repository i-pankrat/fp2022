(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parsetree

module IdMap = Map.Make (struct
  type t = id

  let compare = compare
end)

type value =
  | VUndef
  | VUnit
  | VNil
  | VBool of bool
  | VInt of int
  | VCons of value * value
  | VClosure of id option * value IdMap.t * expr

type environment = value IdMap.t

(* Types *)
type type_num = int

type base_type =
  | TUndef
  | TBool
  | TInt
  | TNil
  | TUnit

type typ =
  | TBase of base_type
  | TVar of type_num
  | TList of typ
  | Arrow of typ * arg_label * typ

type scheme = (type_num, Base.Int.comparator_witness) Base.Set.t * typ
