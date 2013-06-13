module M : (Map.S with type key = Intern.t) = Map.Make (Intern)
module O : (Monad.S with type 'a t = 'a option) = Monad.Make (Option)
open O

type 'a t = 'a M.t

type var = M.key

let empty = M.empty
let singleton = M.singleton

let rec lookup x e = 
  try Some (M.find x e) with Not_found -> None

let extend x v e =
  try 
    let v' = M.find x e in
    if v = v' then None else Some (M.add x v e)
  with 
    Not_found -> Some (M.add x v e)

let union e1 e2 = 
  fold (fun e (x,v) -> extend x v e) e1 (M.bindings e2)

let unions = function
  | [] -> Some empty
  | e::es -> fold union e es
 