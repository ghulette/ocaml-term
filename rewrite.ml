type t = {lhs : Term.t; rhs : Term.t}

let rule t1 t2 = 
  {lhs = t1; rhs = t2}

let apply r t =
  match Term.unify (r.lhs) t with
  | Some e -> Some (Term.subst e (r.rhs))
  | None -> None
