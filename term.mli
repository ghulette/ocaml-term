type atom = Intern.t

type value =
  | BoolVal of bool
  | CharVal of char
  | IntVal of Int32.t
  | RealVal of Int64.t

type var = Env.var

type t =
  | TermVar of var
  | TermVal of value
  | TermAppl of atom * t list

type env = t Env.t

val to_string : t -> string
val is_ground : t -> bool
val variables : t -> var list
val subst : env -> t -> t
val unify : t -> t -> env -> env option
val anti_unify : t -> t -> t
