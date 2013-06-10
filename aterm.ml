type atom = Intern.t

type t = 
  | TermBool of bool
  | TermChar of char
  | TermInt of Int32.t
  | TermReal of Int64.t
  | TermAppl of atom * (t list)
  | TermList of t list
  | TermVar of atom
