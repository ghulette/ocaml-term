type atom = Intern.t

type value =
  | BoolVal of bool
  | CharVal of char
  | IntVal of Int32.t
  | RealVal of Int64.t

type t =
  | TermVar of Env.var
  | TermVal of value
  | TermList of t list
  | TermAppl of atom * t list

type env = t Env.t

val to_string : t -> string
val is_ground : t -> bool
val unify : t -> t -> env -> env option
