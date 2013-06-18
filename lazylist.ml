type 'a t = 
  | Nil 
  | Cons of 'a * 'a t Lazy.t

let hd = function
  | Nil -> failwith "hd"
  | Cons (x,_) -> x

let tl = function
  | Nil -> failwith "tl"
  | Cons (_,lazy xs) -> xs

let rec append xs ys = 
  match xs with
  | Nil -> ys
  | Cons (x,lazy xs') -> Cons (x,lazy (append xs' ys))

let rec length = function 
  | Nil -> 0
  | Cons (_,lazy xs) -> succ (length xs)

let rec nth s = function
  | 0 -> hd s
  | n -> nth (tl s) (n-1)

let from_list l = 
  List.fold_right (fun x s -> Cons (x,lazy s)) l Nil

let rec to_list = function
  | Nil -> []
  | Cons (x,lazy xs) -> x :: to_list xs

let rec take n s = 
  match n,s with
  | 0,_ -> Nil
  | _,Nil -> Nil
  | _,_ -> Cons (hd s, lazy (take (n-1) (tl s)))

let rec drop n s = 
  if n <= 0 then s else drop (n-1) (tl s)

let rec iterate f x =
  Cons (x,lazy (iterate f (f x)))

let repeat x = iterate (fun _ -> x) x

let rec map f = function
  | Nil -> Nil
  | Cons (x,lazy xs) -> Cons (f x,lazy (map f xs))

let rec fold_left f x = function
  | Nil -> x
  | Cons (y,lazy ys) -> f (fold_left f x ys) y

let rec fold_right f y = function
  | Nil -> y
  | Cons (x,lazy xs) -> fold_right f (f x y) xs

let concat = function
  | Nil -> Nil
  | Cons (x,lazy xs) -> fold_right append x xs

let rec filter p = function
  | Nil -> Nil
  | Cons (x,lazy xs) -> 
    if p x then Cons (x,lazy (filter p xs)) else filter p xs

let rec unfold f y =
  match f y with
  | None -> Nil
  | Some (x,y') -> Cons (x,lazy (unfold f y'))

let rec zip l1 l2 =
  match l1,l2 with
  | Nil,_ -> Nil
  | _,Nil -> Nil
  | Cons (x,lazy xs),Cons (y,lazy ys) -> Cons ((x,y),lazy (zip xs ys))

let rec unzip = function
  | Nil -> (Nil,Nil)
  | Cons ((x,y),lazy xys) -> 
    let (xs,ys) = unzip xys in
    (Cons (x,lazy xs),Cons (y,lazy ys))

let nats = 
  iterate succ 0

let pure x = 
  Cons (x,lazy Nil)

let bind mx f =
  concat (map f mx)
