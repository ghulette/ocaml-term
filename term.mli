type atom = Intern.t

type t =
  | TermVar of atom
  | TermVal of Value.t
  | TermAppl of atom * t list

val to_string : t -> string
val is_ground : t -> bool
val variables : t -> atom list

(* Unification and substitution *)
type env
val subst : env -> t -> t
val unify : t -> t -> env option

(* Anti-unification *)
val anti_unify : t -> t -> t
