%{
open Syntax
%}

%start <prog> prog

%nonassoc "in"
%nonassoc below_SEMI
%nonassoc ";"
%nonassoc "let"
%nonassoc below_WITH
%nonassoc "of" "with"
%nonassoc "and"
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
  | x1 = X S x2 = X { [x2; x1] }
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

// Grammar of the ant language

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

%inline infix_op0:
  | "=" { "=" }

%inline infix_op2:
  | "+" { "+" }
  | "-" { "-" }

%inline infix_op3:
  | "*" { "*" }
  | "/" { "/" }

simple_expr:
  | "(" ")" { Unit }
  | "<id>" { Var $1 }
  | "<ctor>" { Ctor $1 }
  | "<int>" { Int $1 }
  | "<bool>" { Bool $1 }
  | "<str>" { Str $1 }
  | "<builtin>" { Builtin (Builtin $1) }
  | "(" seq_expr ")" { $2 }
  | "[" expr_semi_list "]" { Arr $2 }
  | simple_expr "." "<id>" { Sel ($1, $3) }

%inline binding:
  | simple_pattern "=" expr { ($1, $3) }

%inline and_binding:
  | "and" simple_pattern "=" expr { ($2, $4) }

expr:
  | simple_expr %prec below_HASH { $1 }
  | expr_comma_list %prec below_COMMA { Tup $1 }
  | simple_expr llist1(simple_expr) { List.fold_left (fun acc e -> App (acc, [e])) $1 $2 }
  | expr infix_op0 expr { Op ($2, $1, $3) }
  | expr infix_op2 expr { Op ($2, $1, $3) }
  | expr infix_op3 expr { Op ($2, $1, $3) }
  | "let" binding "in" expr { let (p, e) = $2 in Let (BOne (p, e), $4) }
  | "let" "rec" binding and_binding* "in" expr { Let (BRec ($3 :: $4), $6) }
  | "match" expr "with" cases { Match ($2, (MatchPattern $4)) }
  | "fun" simple_pattern+ "->" expr { List.fold_right (fun acc e -> Lam ([acc], e)) $2 $4 }
  | "if" expr "then" expr "else" expr { If ($2, $4, $6) }
  | "if" expr "then" expr { If ($2, $4, Unit) }

seq_expr:
  | expr %prec below_SEMI { $1 }
  | expr ";" seq_expr { Let (BSeq $1, $3) }

type_args: { [] }
  | llist1(atomic_type) { $1 }

core_type:
  | function_type { $1 }

function_type:
  | tuple_type { $1 }
  | tuple_type "->" function_type { TArrow ($1, $3) }

tuple_type:
  | applied_type { $1 }
  | sep_llist2("*", applied_type) { TTuple $1 }

delimited_type:
  | "(" core_type ")" { $2 }

applied_type:
  | atomic_type type_args { if $2 = [] then $1 else TApply ($1, $2)  }

atomic_type:
  | delimited_type { $1 }
  | "'" "<id>" { TNamedVar $2 }
  | "<id>" { TNamed $1 }

ctor_args:
  | inline_sep_llist1("*", applied_type) %prec below_HASH { $1 }
  // TODO: add support for record types

ctor_decl:
  | "<ctor>" { ($1, []) }
  | "<ctor>" "of" ctor_args { ($1, $3) }

ctor_decls:
  | "|" { [] }
  | preceded_or_sep_llist1("|", ctor_decl) { $1 }

type_parameter:
  | "'" "<id>" { $2 }

type_parameters:
  | llist(type_parameter) { $1 }

type_kind:
  | ctor_decls { fun params -> Enum { params; ctors = $1 } }

%inline type_decl:
  | "type" "<id>" type_parameters "=" type_kind { ($2, $5 $3) }

%inline and_type_decl:
  | "and" "<id>" type_parameters "=" type_kind { ($2, $5 $3) }

item:
  | "let" simple_pattern "=" seq_expr { Term (Some $2, $4) }
  | type_decl and_type_decl* { let (x, td) = $1 in if $2 = [] then Type (TBOne (x, td)) else Type (TBRec ($1 :: $2)) }
  | seq_expr { Term (None, $1) }

items: { [] }
  | item ";;" items { $1 :: $3 }
  | item { [$1] }

prog:
  | items "<eof>" { $1 }