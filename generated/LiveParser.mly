%{
open NamedExpr
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE HOLE
%token FUN LET IN IF THEN ELSE MATCH WITH FIX
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW PLUS CONS EQUAL DOT BAR
%token EOF

%start <nexpr> nexpr

%%

nexpr:
  | expr EOF { $1 }

expr:
  | LET IDENT EQUAL expr IN expr { NELet ($2, $4, $6) }
  | IF expr THEN expr ELSE expr { NEIf ($2, $4, $6) }
  | MATCH expr WITH LBRACKET RBRACKET ARROW expr BAR IDENT CONS IDENT ARROW expr
      { NEMatchList ($2, $7, $9, $11, $13) }
  | FUN IDENT ARROW expr { NEAbs ($2, $4) }
  | FIX IDENT IDENT DOT expr { NEFix ($2, $3, $5) }
  | cons_expr { $1 }
;

cons_expr:
  | plus_expr CONS cons_expr { NECons ($1, $3) }
  | plus_expr { $1 }
;

plus_expr:
  | plus_expr PLUS app_expr { NEPlus ($1, $3) }
  | app_expr { $1 }
;

app_expr:
  | app_expr atom_expr { NEApp ($1, $2) }
  | atom_expr { $1 }
;

atom_expr:
  | INT { NEInt $1 }
  | TRUE { NETrue }
  | FALSE { NEFalse }
  | HOLE { NEHole }
  | IDENT { NEVar $1 }
  | LBRACKET RBRACKET { NENil }
  | LPAREN expr RPAREN { $2 }
;
