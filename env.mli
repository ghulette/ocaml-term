type 'a t
type var = Intern.t
val empty : 'a t
val lookup : var -> 'a t -> 'a option
val extend : var -> 'a -> 'a t -> 'a t option
