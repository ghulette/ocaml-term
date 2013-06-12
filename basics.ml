let comp f g x = f (g x)

let app f x = f x

let flip f = fun x y -> f y x
