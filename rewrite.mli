type t

val rule : Term.t -> Term.t -> t
val apply : t -> Term.t -> Term.t option
