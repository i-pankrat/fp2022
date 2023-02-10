(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** [Errors] contains definition of errors in our mini-language. *)

type type_error =
  | OccursCheck (** [OccursCheck] error is to avoid never-ending loops with unification *)
  | NoVariable of Parsetree.id
      (** [NoVariable] error is to indicate undefined variables *)
  | UnificationFailed of Typedtree.typ * Typedtree.typ
      (** [UnificationFailed] error is thrown when the types of the input expressions do not match correctly *)

type error =
  | ParseError of string (** [ParseError] happens on parsing stage *)
  | TypeError of type_error (** [TypeError] happens on inference stage *)
  | RuntimeError of string (** [RuntimeError] happens on evaluation stage *)

module type MONADERROR = sig
  type 'a t = ('a, error) result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
