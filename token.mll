{
open Grammar
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let ident = (alpha | '_')(digit | alpha | ['_' '*' '+' '-'])*

rule lex = parse
  | [' ' '\n' '\t']   { lex lexbuf }
  | "("               { LPAREN }
  | ")"               { RPAREN }
  | "["               { LBRACKET }
  | "]"               { RBRACKET }
  | "<"               { LANGLE }
  | ">"               { RANGLE }
  | ","               { COMMA }
  | digit+ as s       { INT (int_of_string s) } 
  | ident as s        { ID s }
