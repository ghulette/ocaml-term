type 'a t
type var = Intern.t
val empty : 'a t
val lookup : var -> 'a t -> 'a option
val extend : var -> 'a -> 'a t -> 'a t option
val to_list : 'a t -> (string * 'a) list
val from_list : (string * 'a) list -> 'a t option
