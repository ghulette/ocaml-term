type afun = Intern.t

type t = 
  | ATermAppl of afun * (t list)
  | ATermList of t list
  | ATermInt of Int32.t
  | ATermReal of Int64.t
  | ATermPlaceholder of t
