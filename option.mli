type 'a t = 'a option
val fmap : ('a -> 'b) -> 'a option -> 'b option
val pure : 'a -> 'a option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val from_some : 'a option -> 'a
val from_option : 'a -> 'a option -> 'a
val map : ('a -> 'b option) -> 'a list -> 'b list
