open Env
module O : (Monad.S with type 'a t = 'a option) = Monad.Make (Option)
open O

type atom = Intern.t

type value =
  | BoolVal of bool
  | CharVal of char
  | IntVal of Int32.t
  | RealVal of Int64.t

type t =
  | TermVar of var
  | TermVal of value
  | TermList of t list
  | TermAppl of atom * (t list)
  
let rec is_ground = function
  | TermVar _ -> false
  | TermVal _ -> true
  | TermList ts -> List.for_all is_ground ts
  | TermAppl (_,ts) -> List.for_all is_ground ts

let rec unify t1 t2 =
  match t1,t2 with
  | TermVar x,t -> Some (singleton x t)
  | t,TermVar x -> Some (singleton x t)
  | TermVal v1,TermVal v2 when v1 = v2 -> Some empty
  | TermAppl (f1,ts1),TermAppl (f2,ts2) when f1 = f2 ->  
    let ems = List.map2 unify ts1 ts2 in
    sequence ems >>= fun es ->
    unions es
  | _ -> None
