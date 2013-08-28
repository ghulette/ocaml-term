type t =
  | BoolVal of bool
  | CharVal of char
  | IntVal of Int32.t
  | RealVal of Int64.t

let bool_value b = BoolVal b
let char_value c = CharVal c
let int_value n = IntVal n
let real_value x = RealVal (Int64.bits_of_float x)

let to_string = function
  | BoolVal b -> string_of_bool b
  | CharVal c -> Char.escaped c
  | IntVal n -> Int32.to_string n
  | RealVal x -> string_of_float (Int64.float_of_bits x)
