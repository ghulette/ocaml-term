type 'a t =
  | Node of (char list * 'a t) list
  | Leaf of 'a

let empty = Node []

let rec prefix_of xs ys =
  match (xs,ys) with
  | (x::xs',y::ys') when x = y -> prefix_of xs' ys'
  | ([],ys) -> Some ys
  | (_,_) -> None
