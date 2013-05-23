type ('a,'b) t = {
  value : 'b option;
  children : 'a -> ('a,'b) t option
}

let extend k v e = 
  fun x -> if x = k then Some v else e x

let with_default d = function
  | Some x -> x
  | None -> d

let empty = {
  value = None;
  children = fun _ -> None
}

let rec lookup k t =
  match k with
  | [] -> t.value
  | c::cs -> 
    match t.children c with
    | Some t' -> lookup cs t'
    | None -> None

let rec insert k v t =
  match k with
  | [] -> {t with value = Some v}
  | c::k' ->
    let child = with_default empty (t.children c) in
    {t with children = extend c (insert k' v child) t.children}


