(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The typed AST representation of our mini language. *)

(** Typed tree *)

(** Interpretation part. *)

open Parsetree

(** [IdMap] is a mapping from variable names to values. *)
module IdMap : Map.S with type key = id

(** [value] represents possible output values of expressions. *)
type value =
  | VUndef (** internal undefined value *)
  | VUnit (** internal value for [()] *)
  | VNil (** internal value for [[]] *)
  | VBool of bool (** represents boolean values *)
  | VInt of int (** represents integer values *)
  | VCons of value * value (** represent value for non-empty list *)
  | VClosure of id option * environment * expr
      (** represents high-order functions in form of (optional name for recursion, env, Fun (...) ) *)

(** [environment] represents the context of the evaluation. *)
and environment = value IdMap.t

(** Inference part. *)

(** [type_num] enumerates types. *)
type type_num = int

(** [base_type] represents ground types in the language. *)
type base_type =
  | TUndef (** undefined type used in repl *)
  | TBool (** boolean type *)
  | TInt (** integer type *)
  | TNil (** empty list type *)
  | TUnit (** unit type *)

(** [typ] type is the result of type inference *)
type typ =
  | TBase of base_type (** represent base types *)
  | TVar of type_num (** represents type variable *)
  | TList of typ (** represents t list type *)
  | Arrow of typ * Parsetree.arg_label * typ (** represents function types *)

(** [scheme] type is to model polymorphism *)
type scheme = (type_num, Base.Int.comparator_witness) Base.Set.t * typ
