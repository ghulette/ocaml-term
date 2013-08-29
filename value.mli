type t

val bool_value : bool -> t
val char_value : char -> t
val int_value : int -> t
val real_value : float -> t

val to_string : t -> string
