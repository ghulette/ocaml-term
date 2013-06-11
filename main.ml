let term_from_string s = 
  let lexbuf = Lexing.from_string s in
  Grammar.term Token.lex lexbuf

let term_from_channel c =
  let lexbuf = Lexing.from_channel c in
  Grammar.term Token.lex lexbuf
