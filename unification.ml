(*******************************************************************************

  Unification, substitution, and environment

*******************************************************************************)

module O : (Monad.S with type 'a t = 'a option) = Monad.Make (Option)

module Env = struct

  module M : (Map.S with type key = Intern.t) = Map.Make (Intern)

  type 'a t = 'a M.t

  let empty = M.empty

  let find x e = 
    try Some (M.find x e) with Not_found -> None

  (* Extend will fail if the variable is already bound to a different
     value than the one we are trying to bind. *)
  let extend x v e =
    match find x e with
      | None -> Some (M.add x v e)
      | Some v' when v = v' -> Some e
      | Some _ -> None

  let to_list e =
    List.map (fun (x,v) -> (Intern.to_string x,v)) (M.bindings e)

  let from_list l =
    O.fold (fun e (x,v) -> extend (Intern.intern x) v e) empty l
end

open Term_base

type env = t Env.t

let rec dealias x e = 
  match Env.find x e with
  | Some (TermVar y) -> dealias y e
  | _ -> x

let bind x v e =
  let x' = dealias x e in
  Env.extend x' v e

let rec subst e t =
  match t with
  | TermVar x -> 
    let x' = dealias x e in
    begin match Env.find x' e with
    | Some v -> v
    | None -> TermVar x'
    end
  | TermVal _ -> t
  | TermAppl (f,ts) -> TermAppl (f,List.map (subst e) ts)

let rec unify' t1 t2 e =
  match t1,t2 with
  | TermVar x,TermVar y when dealias x e = dealias y e -> Some e
  | TermVar x,t -> bind x t e
  | t,TermVar x -> bind x t e
  | TermVal v1,TermVal v2 when v1 = v2 -> Some e
  | TermAppl (f1,ts1),TermAppl (f2,ts2) when f1 = f2 ->
    let ts = List.combine ts1 ts2 in
    O.fold (fun e (t1,t2) -> unify' t1 t2 e) e ts
  | _ -> None

let unify t1 t2 =
  unify' t1 t2 Env.empty
