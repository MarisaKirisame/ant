%{
open Syntax
open SynInfo
%}

%start <'a prog> prog

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
%left "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/"
%nonassoc below_HASH
%nonassoc "<int>" "<id>" "<ctor>" "<raw_ctor>" "(" "["

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
  | "<ctor>" delimited_pattern { PCtorApp ($1, Some $2, empty_info) }
  | delimited_pattern { $1 }
  | pattern_comma_list %prec below_COMMA { PTup ($1, empty_info) }

delimited_pattern:
  | "<ctor>" { PCtorApp ($1, None, empty_info) }
  | "<int>" { PInt $1 }
  | "<bool>" { PBool $1 }
  | "<id>" { if $1 = "_" then PAny else PVar ($1, empty_info) }
  | "(" ")" { PUnit }
  | "(" pattern ")" { $2 }

simple_pattern:
  | "_" { PAny }
  | "<id>" { PVar ($1, empty_info) }
  | "(" ")" { PUnit }
  | "(" simple_pattern ")" { $2 }
  | simple_pattern_comma_list %prec below_COMMA { PTup ($1, empty_info) }

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

%inline infix_op1:
  | "<" { "<" }
  | "<=" { "<=" }
  | ">" { ">" }
  | ">=" { ">=" }

%inline infix_op2:
  | "+" { "+" }
  | "-" { "-" }

%inline infix_op3:
  | "*" { "*" }
  | "/" { "/" }

simple_expr:
  | "(" ")" { Unit }
  | "<id>" { Var ($1, empty_info) }
  | "<raw_ctor>" { App (Ctor ($1, empty_info), [], empty_info) }
  | "<ctor>" { Ctor ($1, empty_info) }
  | "<int>" { Int $1 }
  | "<bool>" { Bool $1 }
  | "<str>" { Str $1 }
  | "<builtin>" { Builtin (Builtin $1, empty_info) }
  | "(" seq_expr ")" { $2 }
  | "[" expr_semi_list "]" { Arr ($2, empty_info) }
  | simple_expr "." "<id>" { Sel ($1, FName $3, empty_info) }
  | simple_expr "." "<int>" { Sel ($1, FIndex $3, empty_info) }

%inline binding:
  | simple_pattern "=" seq_expr { ($1, $3) }

%inline and_binding:
  | "and" simple_pattern "=" seq_expr { ($2, $4) }

expr:
  | simple_expr %prec below_HASH { $1 }
  | expr_comma_list %prec below_COMMA { Tup ($1, empty_info) }
  | simple_expr llist1(simple_expr)
    {
      match $1 with
      | App (Ctor _ as c, _, info) -> App (c, $2, info)
      | _ -> App ($1, $2, empty_info)
    }
  | expr infix_op0 expr { Op ($2, $1, $3, empty_info) }
  | expr infix_op1 expr { Op ($2, $1, $3, empty_info) }
  | expr infix_op2 expr { Op ($2, $1, $3, empty_info) }
  | expr infix_op3 expr { Op ($2, $1, $3, empty_info) }
  | "let" binding "in" expr { let (p, e) = $2 in Let (BOne (p, e, empty_info), $4, empty_info) }
  | "let" "rec" binding and_binding* "in" expr { Let (BRec (List.map (fun (p, e) -> (p, e, empty_info)) ($3 :: $4)), $6, empty_info) }
  | "match" expr "with" cases { Match ($2, (MatchPattern $4), empty_info) }
  | "fun" simple_pattern+ "->" expr { Lam ($2, $4, empty_info) }
  | "if" expr "then" expr "else" expr { If ($2, $4, $6, empty_info) }
  | "if" expr "then" expr { If ($2, $4, Unit, empty_info) }

seq_expr:
  | expr %prec below_SEMI { $1 }
  | expr ";" seq_expr { Let (BSeq ($1, empty_info), $3, empty_info) }

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
  | "<id>" { 
    match $1 with
    | "int" -> TInt
    | "bool" -> TBool
    | "float" -> TFloat
    | "unit" -> TUnit
    | _ -> TNamed $1
  }

ctor_args:
  | inline_sep_llist1("*", applied_type) %prec below_HASH { $1 }
  // TODO: add support for record types

ctor_decl:
  | "<ctor>" { ($1, [], empty_info) }
  | "<ctor>" "of" ctor_args { ($1, $3, empty_info) }

ctor_decls:
  | "|" { [] }
  | preceded_or_sep_llist1("|", ctor_decl) { $1 }

type_parameter:
  | "'" "<id>" { $2 }

type_parameters:
  | llist(type_parameter) { $1 }

type_kind:
  | ctor_decls { fun params -> Enum { params; ctors = $1 } }

%inline let_decl:
  | "let" binding { $2 }

%inline let_rec_decl:
  | "let" "rec" binding and_binding* { ($3 :: $4) }

%inline type_decl:
  | "type" "<id>" type_parameters "=" type_kind { ($2, $5 $3) }

%inline and_type_decl:
  | "and" "<id>" type_parameters "=" type_kind { ($2, $5 $3) }

item:
  | let_decl { let (p, e) = $1 in Term (BOne (p, e, empty_info)) }
  | let_rec_decl { Term (BRec (List.map (fun (p, e) -> (p, e, empty_info)) $1)) }
  | type_decl and_type_decl* {
      let (x, td) = $1 in
      if $2 = [] then Type (TBOne (x, td))
      else Type (TBRec ($1 :: $2))
    }
  | seq_expr { Term (BSeq ($1, empty_info)) }

items: { [] }
  | item ";;" items { $1 :: $3 }
  | item { [$1] }

prog:
  | items "<eof>" { ($1, empty_info) }
