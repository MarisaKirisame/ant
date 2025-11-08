open Common
open PPrint
open Syntax
open Memo
open State
open Code

let rec compile_ty (x : ty) : document =
  match x with
  | TUnit -> string "unit"
  | TInt -> string "int"
  | TFloat -> string "float"
  | TBool -> string "bool"
  | TApply (f, xs) -> compile_ty f ^^ string " " ^^ separate_map (string " ") compile_ty xs
  | TArrow (con, cov) -> compile_ty con ^^ string " -> " ^^ compile_ty cov
  | TTuple xs -> string "(" ^^ separate_map (string ", ") compile_ty xs ^^ string ")"
  | TVar tyref -> compile_ty !tyref
  | TNamed name -> string name
  | TNamedVar name -> string name

let compile_ctor (name, tys) =
  if List.is_empty tys then string name
  else string name ^^ string " of " ^^ separate_map (string " * ") (fun x -> parens (compile_ty x)) tys

let comple_type_decl name (Enum { params; ctors }) =
  string name ^^ string " "
  ^^ separate_map (string " ") string params
  ^^ string " = "
  ^^ separate_map (string "| ") compile_ctor ctors

let compile_type_binding x =
  match x with
  | TBOne (name, ty_kind) -> string "type " ^^ comple_type_decl name ty_kind
  | TBRec decls ->
      string "type "
      ^^ separate_map (hardline ^^ string "and ") (fun (name, ty_kind) -> comple_type_decl name ty_kind) decls

let rec compile_pat (p : pattern) : document =
  match p with
  | PAny -> string "_"
  | PInt v -> string (string_of_int v)
  | PBool v -> string (string_of_bool v)
  | PVar name -> string name
  | PUnit -> string "()"
  | PTup xs -> string "(" ^^ separate_map (string ", ") compile_pat xs ^^ string ")"
  | PApp (name, None) -> string name
  | PApp (name, Some p') -> string name ^^ string " " ^^ compile_pat p'

and parens_compile_pat p = parens (compile_pat p)

let rec compile_expr (e : expr) : document =
  match e with
  | Unit -> string "()"
  | Int v -> string (string_of_int v)
  | Float v -> string (string_of_float v)
  | Bool v -> string (string_of_bool v)
  | Str v -> string v
  | Builtin (Builtin b) -> string b
  | Var name -> string name
  | GVar name -> string name
  | Ctor name -> string name
  | App (Ctor name, args) -> string name ^^ string " " ^^ parens (separate_map (string ", ") parens_compile_expr args)
  | App (fn, args) -> parens_compile_expr fn ^^ string " " ^^ separate_map (string " ") parens_compile_expr args
  | Op (op, lhs, rhs) -> parens (parens_compile_expr lhs ^^ string op ^^ parens_compile_expr rhs)
  | Tup xs -> string "(" ^^ separate_map (string ", ") compile_expr xs ^^ string ")"
  | Arr xs -> string "[]" ^^ separate_map (string "; ") compile_expr xs ^^ string "]"
  | Lam (ps, value) ->
      string "fun " ^^ separate_map (string " ") parens_compile_pat ps ^^ string " -> " ^^ parens_compile_expr value
  | Let (binding, value) -> compile_binding binding (parens_compile_expr value)
  | Sel (expr, prop) -> parens_compile_expr expr ^^ string "." ^^ string prop
  | If (c, p, n) ->
      string "if " ^^ compile_expr c ^^ string " then " ^^ parens_compile_expr p ^^ string " else "
      ^^ parens_compile_expr n
  | Match (tgt, MatchPattern cases) ->
      string "match " ^^ compile_expr tgt ^^ string " with "
      ^^ separate_map (string "| ") (fun (p, e) -> parens_compile_pat p ^^ string " -> " ^^ parens_compile_expr e) cases

and parens_compile_expr e = parens (compile_expr e)

and compile_binding (b : binding) (cont : document) : document =
  match b with
  | BSeq e -> compile_expr e ^^ string ";" ^^ cont
  | BOne (p, e) | BCont (p, e) -> string "let " ^^ compile_let (p, e) ^^ string " in " ^^ cont
  | BRec xs | BRecC xs -> string "let rec " ^^ separate_map (string " and ") compile_let xs ^^ string " in " ^^ cont

and compile_let (p, e) = parens_compile_pat p ^^ string " = " ^^ parens (compile_expr e)

let compile_stmt (x : stmt) : document =
  match x with
  | Type tb -> compile_type_binding tb
  | Fun (_name, _args, _body) -> failwith "Not implemented (TODO)"
  | Term (pat, e) -> (
      match pat with
      | None -> compile_expr e
      | Some pat -> string "let rec " ^^ parens_compile_pat pat ^^ string " = " ^^ compile_expr e)

let compile_plain (xs : stmt list) : document =
  let ys = List.map compile_stmt xs in
  separate (string ";;" ^^ hardline ^^ hardline) ys

module Backend = struct
  let compile = compile_plain
end
