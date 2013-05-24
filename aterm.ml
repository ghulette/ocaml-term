type t = 
  | AFun of string
  | ATermAppl of t * t
  | ATermInt of Int32.t
  | ATermReal of Int64.t
