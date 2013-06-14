type 'a t
type var = Intern.t
val empty : 'a t
val singleton : var -> 'a -> 'a t
val lookup : var -> 'a t -> 'a option
val extend : var -> 'a -> 'a t -> 'a t option
val alias : var -> var -> 'a t -> 'a t option
val union : 'a t -> 'a t -> 'a t option
val unions : 'a t list -> 'a t option
