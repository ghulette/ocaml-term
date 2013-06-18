type 'a t = Nil | Cons of 'a * 'a t Lazy.t

val from_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val iterate : ('a -> 'a) -> 'a -> 'a t
val repeat : 'a -> 'a t

val hd : 'a t -> 'a
val tl : 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val length : 'a t -> int
val nth : 'a t -> int -> 'a
val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val concat : 'a t t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t

val pure : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t

val nats : int t
