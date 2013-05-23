type ('a, 'b) t
val empty : ('a, 'b) t
val lookup : 'a list -> ('a, 'b) t -> 'b option
val insert : 'a list -> 'b -> ('a, 'b) t -> ('a, 'b) t
