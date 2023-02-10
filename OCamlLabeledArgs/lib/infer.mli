(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Module [Infer] is responsible for inferencing types of expressions in our mini-language. *)

(** [TypEnv] module represent the typing context. *)
module TypeEnv : sig
  type t = (string, Typedtree.scheme, Base.String.comparator_witness) Base.Map.t

  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
end

(** [infer] is the entry point for inferencing.
    It takes [Parsetree.expr] type and [TypeEnv.t] type which represents the typing context. *)
val infer : Parsetree.expr -> TypeEnv.t -> (Typedtree.typ, Errors.error) result
