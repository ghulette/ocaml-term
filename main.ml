let aterm_from_string s = 
  let lexbuf = Lexing.from_string s in
  Parser.aterm Token.lex lexbuf

let aterm_from_channel c =
  let lexbuf = Lexing.from_channel c in
  Parser.aterm Token.lex lexbuf
