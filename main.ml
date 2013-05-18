let parse s = 
  let lexbuf = Lexing.from_string s in
  Parser.aterm Token.lex lexbuf
