type t = {
  n : int ref;
  inf : string array;
  outf : int Prefix.t ref
}

let db = {
  n = ref 0;
  inf = [||];
  outf = ref Prefix.empty
}
