%token LPAREN RPAREN LBRACKET RBRACKET LANGLE RANGLE COMMA
%token <Int32.t> INT
%token <Int64.t> REAL
%token <string> ID

%{
open Aterm
%}

%start aterm
%type <Aterm.t> aterm

%%

afun: ID { Intern.intern $1 }

aterms1:
  | aterms1 COMMA aterm     { $3 :: $1 }
  | aterm                   { [$1] }

aterms: aterms1 { List.rev $1 }

appl:
  | afun LPAREN aterms RPAREN { ATermAppl ($1,$3) }
  | afun                      { ATermAppl ($1,[]) }

list:
  | LBRACKET RBRACKET         { ATermList [] }
  | LBRACKET aterms RBRACKET  { ATermList $2 }

aterm:
  | appl                   { $1 }
  | list                   { $1 }
  | LANGLE afun RANGLE     { ATermVar $2 }
  | INT                    { ATermInt $1 }
  | REAL                   { ATermReal $1 }
