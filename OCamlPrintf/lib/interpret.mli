module type MONAD_ERROR = sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Result : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Interpret (M : MONAD_ERROR) : sig
  type value [@@deriving show { with_path = false }]

  module Env : sig
    type 'a t
  end

  (** evaluate without inference *)
  val eval : ?m:value Env.t -> Ast.expr -> value M.t

  (** parse, inference and evaluate *)
  val run : string -> unit
end
