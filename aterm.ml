type t = 
  | ATermAppl of Intern.t * (t list)
  | ATermList of t list
  | ATermInt of Int32.t
  | ATermReal of Int64.t
  | ATermPlaceholder of t
