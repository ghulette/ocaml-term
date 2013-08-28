type atom = Intern.t
type var = Env.var

type t =
  | TermVar of var
  | TermVal of Value.t
  | TermAppl of atom * t list

type env = t Env.t

val to_string : t -> string
val is_ground : t -> bool
val variables : t -> var list
val subst : env -> t -> t
val unify : t -> t -> env option
val anti_unify : t -> t -> t
