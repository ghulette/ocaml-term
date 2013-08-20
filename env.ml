module M : (Map.S with type key = Intern.t) = Map.Make (Intern)

type 'a t = 'a M.t

type var = M.key

let empty = M.empty

let rec lookup x e = 
  try Some (M.find x e) with Not_found -> None

(* Extend will fail if the variable is already bound to a different
   value than the one we are trying to bind. *)
let extend x v e =
  try 
    let v' = M.find x e in
    if v = v' then Some e else None
  with 
    Not_found -> Some (M.add x v e)

      
