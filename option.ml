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
