module type MonadDef = sig
  type 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t
  val sequence : ('a t) list -> ('a list) t
  val map : ('a -> 'b t) -> 'a list -> ('b list) t
  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
end

module Make (M : MonadDef) : (S with type 'a t = 'a M.t) = struct
  open M

  type 'a t = 'a M.t

  let (>>=) = bind

  let join mmx = 
    mmx >>= fun mx ->
    mx >>= fun x ->
    pure x

  let rec sequence = function
    | [] -> pure []
    | m::ms -> begin
        m >>= fun x ->
        sequence ms >>= fun xs ->
        pure (x::xs)
      end

  let map f xs = 
    sequence (List.map f xs)

  let rec fold f x = function
    | [] -> pure x
    | y::ys -> begin
        f x y >>= fun x' ->
        fold f x' ys
      end
end
