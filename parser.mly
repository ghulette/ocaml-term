%token LPAREN RPAREN COMMA EOF
%token <Int32.t> INT
%token <Int64.t> REAL
%token <string> ID

%{
open Aterm
%}

%start aterm
%type <Aterm.t> aterm

%%

exp1:
  | exp1 exp2                             { ATermAppl ($1,$2) }
  | exp2                                  { $1 }

exp2:
  | ID                                    { AFun $1 }
  | INT                                   { ATermInt $1 }
  | REAL                                  { ATermReal $1 }
  | LPAREN exp1 RPAREN                    { $2 }

aterm:
  | exp1 EOF                              { $1 }
