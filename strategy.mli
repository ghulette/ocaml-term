type t

val apply : t -> Term.t -> Term.t option
val rule : Term.t -> Term.t -> t
val succeed : t
val fail : t
val negate : t -> t
val test : t -> t
val sequence : t -> t -> t
val left_choice : t -> t -> t
