(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
  val error : string -> 'a t
end

module Result : sig
  type 'a t = ('a, string) Result.t

  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( >> ) : ('a, 'b) result -> ('c, 'b) result -> ('c, 'b) result
  val return : 'a -> ('a, 'b) result
  val error : 'a -> ('b, 'a) result
end
