(** Copyright 2022-2023, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open InterpretMonadFailInf

module Result : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( *> ) : 'a t -> 'b -> 'b t
  val ( <* ) : 'a t -> 'b -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let** ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
end

module Eval (M : MONAD_FAIL) : sig
  type state
  type value [@@deriving show { with_path = false }]

  type env =
    { state : state
    ; ret : value
    }

  val eval : ?state:state -> Ast.t -> env M.t
  val run : string -> env M.t
end
