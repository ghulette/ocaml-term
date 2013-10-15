module O : (Monad.S with type 'a t = 'a option) = Monad.Make (Option)

type atom = Intern.t

type t =
  | TermVar of atom
  | TermVal of Value.t
  | TermAppl of atom * (t list)


let rec string_of_terms ts = 
  let ss = List.map string_of_term ts in
  Util.join "," ss

and string_of_term = function
  | TermVar x -> "<" ^ (Intern.to_string x) ^ ">"
  | TermVal x -> Value.to_string x
  | TermAppl (f,[]) -> Intern.to_string f
  | TermAppl (f,ts) -> 
    (Intern.to_string f) ^ "(" ^ (string_of_terms ts) ^ ")"

let to_string = string_of_term

let rec is_ground = function
  | TermVar _ -> false
  | TermVal _ -> true
  | TermAppl (_,ts) -> List.for_all is_ground ts

module S = Set.Make (Intern)

let unions ss = 
  List.fold_left S.union S.empty ss

let rec variable_set = function
  | TermVar x -> S.singleton x
  | TermVal _ -> S.empty
  | TermAppl (_,ts) -> unions (List.map variable_set ts)

let variables t =
  S.elements (variable_set t)


(*******************************************************************************

  Unification, substitution, and environment

*******************************************************************************)

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

module TermPair = struct
  type term = t
  type t = term * term
  let compare = compare
end

module F = Memo.Sigma (TermPair) (Memo.GenSym)

let rec anti_unify' sigma = function
  | TermAppl (f,ts1),TermAppl (g,ts2) when f = g ->
    let ts = List.map (anti_unify' sigma) (List.combine ts1 ts2) in
    TermAppl (f,ts)
  | TermVal x,TermVal y when x = y -> TermVal x
  | tt -> let x = F.memo sigma tt in TermVar x

let anti_unify t1 t2 =
  anti_unify' F.empty (t1,t2)
