module type MonadDef =
  sig
    type 'a t
    val pure : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

module type S =
  sig
    type 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val join : 'a t t -> 'a t
    val sequence : 'a t list -> 'a list t
    val map : ('a -> 'b t) -> 'a list -> 'b list t
    val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  end

module Make :
  functor (M : MonadDef) -> S with type 'a t = 'a M.t
