let rec prepend_each s = function
  | [] -> []
  | x :: xs -> s :: x :: (prepend_each s xs)

let intersperse s = function
  | [] -> []
  | x :: xs -> x :: (prepend_each s xs)

let join sep ss =
  let ss' = intersperse sep ss in
  List.fold_left (^) "" ss' 
