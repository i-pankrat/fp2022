(** Copyright 2022-2023, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MONAD_FAIL = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( *> ) : 'a t -> 'b -> 'b t
  val ( <* ) : 'a t -> 'b -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let** ) : 'a t -> ('a -> 'b) -> 'b t
  val choice : 'a t list -> 'a t
end
