%token LPAREN RPAREN LBRACKET RBRACKET LANGLE RANGLE COMMA
%token <Int32.t> INT
%token <Int64.t> REAL
%token <string> ID

%{
open Aterm
open Intern
%}

%start aterm
%type <Aterm.t> aterm

%%

afun: ID { intern $1 }

aterms1:
  | aterms1 COMMA aterm     { $3 :: $1 }
  | aterm                   { [$1] }

aterms: aterms1 { List.rev $1 }

appl:
  | afun LPAREN aterms RPAREN { TermAppl ($1,$3) }
  | afun                      { TermAppl ($1,[]) }

list:
  | LBRACKET RBRACKET         { TermList [] }
  | LBRACKET aterms RBRACKET  { TermList $2 }

aterm:
  | appl                   { $1 }
  | list                   { $1 }
  | LANGLE afun RANGLE     { TermVar $2 }
  | INT                    { TermInt $1 }
  | REAL                   { TermReal $1 }
