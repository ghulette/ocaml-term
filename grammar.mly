%token LPAREN RPAREN LBRACKET RBRACKET LANGLE RANGLE COMMA
%token <Int32.t> INT
%token <string> ID

%{
open Term
open Intern
%}

%start term
%type <Term.t> term

%%

tfun: ID                  { intern $1 }

terms1:
  | terms1 COMMA term     { $3 :: $1 }
  | term                  { [$1] }

terms: terms1             { List.rev $1 }

appl:
  | tfun LPAREN terms RPAREN { TermAppl ($1,$3) }
  | tfun                     { TermAppl ($1,[]) }

term:
  | appl                   { $1 }
  | LANGLE tfun RANGLE     { TermVar $2 }
  | INT                    { TermVal (Value.int_value $1) }
