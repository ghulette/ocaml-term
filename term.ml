module O : (Monad.S with type 'a t = 'a option) = Monad.Make (Option)

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
  | TermAppl of atom * (t list)

type env = t Env.t

let string_of_value = function
  | BoolVal b -> string_of_bool b
  | CharVal c -> Char.escaped c
  | IntVal i -> Int32.to_string i
  | RealVal r -> Int64.to_string r

let rec string_of_terms ts = 
  let ss = List.map string_of_term ts in
  Util.join "," ss

and string_of_term = function
  | TermVar x -> "<" ^ (Intern.to_string x) ^ ">"
  | TermVal x -> string_of_value x
  | TermList ts -> "[" ^ (string_of_terms ts) ^ "]"
  | TermAppl (f,[]) -> Intern.to_string f
  | TermAppl (f,ts) -> 
    (Intern.to_string f) ^ "(" ^ (string_of_terms ts) ^ ")"

let to_string = string_of_term

let rec is_ground = function
  | TermVar _ -> false
  | TermVal _ -> true
  | TermList ts -> List.for_all is_ground ts
  | TermAppl (_,ts) -> List.for_all is_ground ts

let rec dealias x e = 
  match Env.lookup x e with
  | Some (TermVar y) -> dealias y e
  | _ -> x

let bind x v e =
  let x' = dealias x e in
  Env.extend x' v e

let rec unify t1 t2 e =
  match t1,t2 with
  | TermVar x,TermVar y when dealias x e = dealias y e -> Some e
  | TermVar x,t -> bind x t e
  | t,TermVar x -> bind x t e
  | TermVal v1,TermVal v2 when v1 = v2 -> Some e
  | TermAppl (f1,ts1),TermAppl (f2,ts2) when f1 = f2 ->
    let ts = List.combine ts1 ts2 in
    O.fold (fun e (t1,t2) -> unify t1 t2 e) e ts
  | _ -> None
