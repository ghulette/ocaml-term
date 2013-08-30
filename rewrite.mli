type t

val make : Term.t -> Term.t -> t
val apply : t -> Term.t -> Term.t option
