type var = Intern.t

module M = Map.Make (Intern)

type 'a t = 'a M.t

let extend e x v = M.add x v e

let lookup e x = M.find x e
