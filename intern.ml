
module R = Relation.Make (String) (struct type t = int let compare = compare end)

type t = int

type db = {
  n : int;
  r : R.t
}

let empty = 
  { n = 0; r = R.empty }

let extend s db = 
  let n' = succ db.n in 
  { n = n'; r = R.extend db.r s n' }

(* Global variable *)
let db = ref empty

let reset () =
  db := empty

let intern s = 
  match R.lookupl !db.r s with
  | Some n -> n
  | None -> db := extend s !db; !db.n

let to_string i =
  match R.lookupr !db.r i with
  | Some s -> s
  | None -> raise Not_found
