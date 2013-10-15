type atom = Intern.t

type t =
  | TermVar of atom
  | TermVal of Value.t
  | TermAppl of atom * (t list)

let rec string_of_terms ts =
  let ss = List.map string_of_term ts in
  Util.join "," ss

and string_of_term = function
  | TermVar x -> "<" ^ (Intern.to_string x) ^ ">"
  | TermVal x -> Value.to_string x
  | TermAppl (f,[]) -> Intern.to_string f
  | TermAppl (f,ts) -> 
    (Intern.to_string f) ^ "(" ^ (string_of_terms ts) ^ ")"

let to_string = string_of_term

let rec is_ground = function
  | TermVar _ -> false
  | TermVal _ -> true
  | TermAppl (_,ts) -> List.for_all is_ground ts

module S = Set.Make (Intern)

let unions ss = 
  List.fold_left S.union S.empty ss

let rec variable_set = function
  | TermVar x -> S.singleton x
  | TermVal _ -> S.empty
  | TermAppl (_,ts) -> unions (List.map variable_set ts)

let variables t =
  S.elements (variable_set t)
