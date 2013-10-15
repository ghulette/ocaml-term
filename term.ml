include Term_base

let from_string s = 
  let lexbuf = Lexing.from_string s in
  Grammar.term Token.lex lexbuf

let from_channel c =
  let lexbuf = Lexing.from_channel c in
  Grammar.term Token.lex lexbuf

include Unification
include Antiunification
