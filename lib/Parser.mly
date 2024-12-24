%{
open Syntax

%}

%start <prog> prog

%nonassoc "in"
%nonassoc below_SEMI
%nonassoc ";"
%nonassoc "let"
%nonassoc "of" "fun" "with"
%nonassoc "then"
%nonassoc "else"
%left "|"
%nonassoc below_COMMA
%left ","
%right "->"
%left "="
%left "+" "-"
%left "*" "/"
%nonassoc below_HASH
%nonassoc "#"
%nonassoc "<int>" "<id>" "<ctor>" "(" "["

%%

lopt(X): { [] }
  | x = X { x }

// reversed left-recursive list
rev_llist(X): { [] }
  | xs = rev_llist(X) x = X { x :: xs }

// left-recursive list
%inline llist(X):
  | xs = rev(rev_llist(X)) { xs }

// reversed non-empty left-recursive list
rev_llist1(X):
  | x = X { [x] }
  | xs = rev_llist1(X) x = X { x :: xs }

// non-empty left-recursive list
%inline llist1(X):
  | xs = rev(rev_llist1(X)) { xs }

// inlined reversed left-recursive list with separator
%inline inline_rev_sep_llist1(S, X):
  | x = X { [x] }
  | xs = rev_sep_llist1(S, X) S x = X { x :: xs }

// reversed left-recursive list with separator
rev_sep_llist1(S, X):
  | xs = inline_rev_sep_llist1(S, X) { xs }

// left-recursive list with separator
%inline sep_llist1(S, X):
  | xs = rev(rev_sep_llist1(S, X)) { xs }

// inlined left-recursive list with separator
%inline inline_sep_llist1(S, X):
  | xs = rev(inline_rev_sep_llist1(S, X)) { xs }

// reversed left-recursive list with separator and at least two elements
rev_sep_llist2(S, X):
  | x1 = X S x2 = X { [x1; x2] }
  | xs = rev_sep_llist2(S, X) S x = X { x :: xs }

%inline sep_llist2(S, X):
  | xs = rev(rev_sep_llist2(S, X)) { xs }

// right-recursive list with separator and an optional terminator
sep_or_terminated_rlist1(D, X):
  | x = X D? { [x] }
  | x = X D xs = sep_or_terminated_rlist1(D, X) { x :: xs }

// reversed left-recursive list with separator and optionally preceded by a separator
rev_preceded_or_sep_llist1(D, X):
  | D? x = X { [x] }
  | xs = rev_preceded_or_sep_llist1(D, X) D x = X { x :: xs }

// left-recursive list with separator and optionally preceded by a separator
%inline preceded_or_sep_llist1(D, X):
  | xs = rev(rev_preceded_or_sep_llist1(D, X)) { xs }

// Grammar of the Optimus language

pattern:
  | "<int>" { PInt $1 }
  | "<bool>" { PBool $1 }
  | "<id>" { if $1 = "_" then PAny else PVar $1 }
  | "<ctor>" { PApp ($1, None) }
  | "<ctor>" pattern { PApp ($1, Some $2) }
  | "(" ")" { PUnit }
  | "(" pattern ")" { $2 }
  | pattern_comma_list %prec below_COMMA { PTup $1 }

simple_pattern:
  | "_" { PAny }
  | "<id>" { PVar $1 }
  | "(" ")" { PUnit }
  | "(" simple_pattern ")" { $2 }
  | simple_pattern_comma_list %prec below_COMMA { PTup $1 }

%inline simple_pattern_comma_list:
  sep_llist2(",", simple_pattern) { $1 }

%inline pattern_comma_list:
  sep_llist2(",", pattern) { $1 }

case : pattern "->" expr { ($1, $3) }

%inline cases:
  preceded_or_sep_llist1("|", case) { $1 }

%inline expr_comma_list:
  sep_llist2(",", expr) { $1 }

%inline expr_semi_list:
  sep_or_terminated_rlist1(";", expr) { $1 }

%inline infix_op1:
  | "+" { "+" }
  | "-" { "-" }

%inline infix_op2:
  | "*" { "*" }
  | "/" { "/" }

simple_expr:
  | "(" ")" { Unit }
  | "<id>" { Var $1 }
  | "<ctor>" { CApp (Ctor ($1, None), []) }
  | "<int>" { Int $1 }
  | "<bool>" { Bool $1 }
  | "<str>" { Str $1 }
  | "<builtin>" { Builtin (Builtin $1) }
  | "(" seq_expr ")" { $2 }
  | "[" expr_semi_list "]" { Arr $2 }
  | simple_expr "." "<id>" { Sel ($1, $3) }

expr:
  | simple_expr %prec below_HASH { $1 }
  | expr_comma_list %prec below_COMMA { Tup $1 }
  | simple_expr llist1(simple_expr) { App ($1, $2) }
  | expr infix_op1 expr { Op ($2, $1, $3) }
  | expr infix_op2 expr { Op ($2, $1, $3) }
  | "let" simple_pattern "=" expr "in" expr { Let (BOne ($2, $4), $6) }
  | "let" "rec" simple_pattern "=" expr "in" expr { Let (BRec [($3, $5)], $7) }
  | "match" expr "with" cases { Match ($2, (MatchPattern $4)) }
  | "fun" simple_pattern+ "->" expr { Lam ($2, $4) }
  | "if" expr "then" expr "else" expr { If ($2, $4, $6) }
  | "if" expr "then" expr { If ($2, $4, Unit) }

seq_expr:
  | expr %prec below_SEMI { $1 }
  | expr ";" seq_expr { Let (BSeq $1, $3) }

items: { [] }
  | "let" simple_pattern "=" seq_expr ";;" items { Term (Some $2, $4) :: $6 }
  | seq_expr ";;" items { Term (None, $1) :: $3 }
  | seq_expr { [Term (None, $1)] }

prog:
  | items "<eof>" { $1 }