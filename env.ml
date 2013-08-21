module M : (Map.S with type key = Intern.t) = Map.Make (Intern)
module O : (Monad.S with type 'a t = 'a option) = Monad.Make (Option)

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

let to_list e =
  List.map (fun (x,v) -> (Intern.to_string x,v)) (M.bindings e)

let from_list l =
  O.fold (fun e (x,v) -> extend (Intern.intern x) v e) empty l
