type 'a t = 'a option

let fmap f = function
  | None -> None
  | Some x -> Some (f x)

let pure x = 
  Some x

let bind mx f = 
  match mx with
  | None -> None
  | Some x -> f x

let from_some = function
  | None -> raise (Invalid_argument "None")
  | Some x -> x

let from_option default = function
  | None -> default
  | Some x -> x

let rec map f = function
  | [] -> []
  | x::xs -> 
    match f x with
      | None -> map f xs
      | Some y -> y :: (map f xs)
