module Int = struct 
  type t = int 
  let compare = compare 
end

module R = Relation.Make (String) (Int)

type t = int

type m = {
  n : int;
  r : R.t
}

let empty = 
  { n = 0; r = R.empty }

let extend s m = 
  let n' = succ m.n in 
  { n = n'; r = R.extend m.r s n' }

let put m s = 
  match R.lookupl !m.r s with
  | Some n -> n
  | None -> m := extend s !m; !m.n

let get m i =
  match R.lookupr !m.r i with
  | Some s -> s
  | None -> raise Not_found

(* Use a global string database. *)

let db = ref empty
let reset () = db := empty
let intern = put db
let to_string = get db
