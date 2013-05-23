type 'a t
val empty : 'a t
val lookup : string -> 'a t -> 'a option
val insert : string -> 'a -> 'a t -> 'a t
