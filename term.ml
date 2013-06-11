type atom = Intern.t

type t = 
  | TermBool of bool
  | TermChar of char
  | TermInt of Int32.t
  | TermReal of Int64.t
  | TermAppl of atom * (t list)
  | TermList of t list
  | TermVar of atom

let rec is_ground = function
  | TermBool _ -> true
  | TermChar _ -> true
  | TermInt _ -> true
  | TermReal _ -> true
  | TermAppl (_,ts) -> List.for_all is_ground ts
  | TermList ts -> List.for_all is_ground ts
  | TermVar _ -> false
