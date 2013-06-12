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

val is_ground : t -> bool
