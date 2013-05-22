type 'a t =
  | Node of ((char list) * ('a t)) list
  | Leaf of 'a

let explode s = 
  let cs = ref [] in
  for i = 0 to (String.length s - 1) do
    cs := s.[i] :: !cs
  done; List.rev (!cs)

let replace k1 k2 v = function
  | [] -> raise Not_found
  | (x::xs) when x = k1 -> 

let empty = Node []

let rec prefix xs ys =
  match xs,ys with
  | x::xs',y::ys' when x = y -> prefix xs' ys'
  | _,[] -> Some xs
  | _,_ -> None

let rec descend k = function
  | [] -> None
  | ((lbl,r)::edgs) -> 
    match prefix k lbl with
    | None -> descend k edgs
    | Some k' -> Some (lbl,k',r)

let rec lookup k = function
  | Leaf x -> if k = [] then Some x else None
  | Node edgs -> 
    match descend k edgs with
    | None -> None
    | Some (_,k',r) -> lookup k' r

let rec insert k v = function
  | Node edges -> 
    begin match descend k edges with
    | None -> Node [(k,Leaf v)]
    | Some (pre,rest,t) -> Node (replace edges pre (insert k' v t))
    end
  | Leaf x ->
    begin match k with
    | [] -> Leaf v
    | _ -> Node [([],Leaf x);(k,Leaf v)]
    end
  
