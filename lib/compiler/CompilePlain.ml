open Common
open PPrint
open Syntax
open Memo
open State
open Code

let type_alias_module : string option ref = ref None

let is_valid_module_path (value : string) : bool =
  let is_upper = function 'A' .. 'Z' -> true | _ -> false in
  let is_ident_char = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false in
  let is_valid_segment (segment : string) : bool =
    let len = String.length segment in
    if len = 0 then false
    else if not (is_upper segment.[0]) then false
    else
      let rec loop i = if i >= len then true else if is_ident_char segment.[i] then loop (i + 1) else false in
      loop 1
  in
  List.for_all is_valid_segment (String.split_on_char '.' value)

let set_type_alias_module value =
  (match value with
  | None -> ()
  | Some module_name ->
      if not (is_valid_module_path module_name) then invalid_arg ("Invalid module path for --type-alias: " ^ module_name));
  type_alias_module := value

let rec compile_ty (x : 'a ty) : document =
  match x with
  | TUnit -> string "unit"
  | TInt -> string "int"
  | TFloat -> string "float"
  | TBool -> string "bool"
  | TApply (f, []) -> compile_ty f
  | TApply (f, xs) -> parens (separate_map (string ", ") compile_ty xs) ^^ space ^^ compile_ty f
  | TArrow (con, cov) -> compile_ty con ^^ string " -> " ^^ compile_ty cov
  | TTuple xs -> string "(" ^^ separate_map (string ", ") compile_ty xs ^^ string ")"
  | TNamed name -> string name
  | TNamedVar name -> string ("'" ^ name)

let compile_ctor (name, tys, _) =
  if List.is_empty tys then string name
  else string name ^^ string " of " ^^ separate_map (string " * ") (fun x -> parens (compile_ty x)) tys

let comple_type_decl name (Enum { params; ctors }) =
  separate_map space string (List.map (fun s -> "'" ^ s) params)
  ^^ space ^^ string name ^^ space ^^ string " = "
  ^^ separate_map (string "| ") compile_ctor ctors

let compile_type_binding x =
  match x with
  | TBOne (name, ty_kind) -> string "type " ^^ comple_type_decl name ty_kind
  | TBRec decls ->
      string "type "
      ^^ separate_map (hardline ^^ string "and ") (fun (name, ty_kind) -> comple_type_decl name ty_kind) decls

let compile_type_alias module_name =
  let params_doc params =
    match params with
    | [] -> empty
    | [ x ] -> string ("'" ^ x) ^^ space
    | _ -> parens (separate_map (string ", ") (fun p -> string ("'" ^ p)) params) ^^ space
  in
  let alias_decl name params =
    params_doc params ^^ string name ^^ space ^^ string "=" ^^ space ^^ params_doc params ^^ string module_name
    ^^ string "." ^^ string name
  in
  function
  | TBOne (name, Enum { params; _ }) -> string "type " ^^ alias_decl name params
  | TBRec decls ->
      string "type "
      ^^ separate_map (hardline ^^ string "and ") (fun (name, Enum { params; _ }) -> alias_decl name params) decls

let rec compile_pat (p : 'a pattern) : document =
  match p with
  | PAny -> string "_"
  | PInt v -> string (string_of_int v)
  | PBool v -> string (string_of_bool v)
  | PVar (name, _) -> string name
  | PUnit -> string "()"
  | PTup (xs, _) -> string "(" ^^ separate_map (string ", ") compile_pat xs ^^ string ")"
  | PCtorApp (name, None, _) -> string name
  | PCtorApp (name, Some p', _) -> string name ^^ space ^^ compile_pat p'

and parens_compile_pat p = parens (compile_pat p)

let rec compile_expr (e : 'a expr) : document =
  match e with
  | Unit -> string "()"
  | Int v -> string (string_of_int v)
  | Float v -> string (string_of_float v)
  | Bool v -> string (string_of_bool v)
  | Str v -> string v
  | Builtin (Builtin b, _) -> string b
  | Var (name, _) -> string name
  | GVar (name, _) -> string name
  | Ctor (name, _) -> string name
  | App (Ctor (name, _), args, _) ->
      string name ^^ space ^^ parens (separate_map (string ", ") parens_compile_expr args)
  | App (fn, args, _) -> parens_compile_expr fn ^^ space ^^ separate_map space parens_compile_expr args
  | Op (op, lhs, rhs, _) -> parens (parens_compile_expr lhs ^^ string op ^^ parens_compile_expr rhs)
  | Tup (xs, _) -> string "(" ^^ separate_map (string ", ") compile_expr xs ^^ string ")"
  | Arr (xs, _) -> string "[]" ^^ separate_map (string "; ") compile_expr xs ^^ string "]"
  | Lam (ps, value, _) ->
      string "fun " ^^ separate_map space parens_compile_pat ps ^^ string " -> " ^^ parens_compile_expr value
  | Let (binding, value, _) -> compile_binding binding (parens_compile_expr value)
  | Sel (expr, prop, _) -> parens_compile_expr expr ^^ string "." ^^ pp_field prop
  | If (c, p, n, _) ->
      string "if " ^^ compile_expr c ^^ string " then " ^^ parens_compile_expr p ^^ string " else "
      ^^ parens_compile_expr n
  | Match (tgt, MatchPattern cases, _) ->
      string "match " ^^ compile_expr tgt ^^ string " with "
      ^^ separate_map (string "| ") (fun (p, e) -> parens_compile_pat p ^^ string " -> " ^^ parens_compile_expr e) cases

and parens_compile_expr e = parens (compile_expr e)

and compile_binding (b : 'a binding) (cont : document) : document =
  match b with
  | BSeq (e, _) -> compile_expr e ^^ string ";" ^^ cont
  | BOne (p, e, info) | BCont (p, e, info) -> string "let " ^^ compile_let (p, e, info) ^^ string " in " ^^ cont
  | BRec xs | BRecC xs -> string "let rec " ^^ separate_map (string " and ") compile_let xs ^^ string " in " ^^ cont

and compile_let (p, e, _) = parens_compile_pat p ^^ string " = " ^^ parens (compile_expr e)

let compile_stmt (x : 'a stmt) : document =
  match x with
  | Type tb -> (
      match !type_alias_module with
      | None -> compile_type_binding tb
      | Some module_name -> compile_type_alias module_name tb)
  | Term (BSeq (e, _)) -> compile_expr e
  | Term (BOne (pat, e, _) | BRec [ (pat, e, _) ]) ->
      string "let rec " ^^ parens_compile_pat pat ^^ string " = " ^^ compile_expr e
  | _ -> failwith "Not implemented (TODO)"

let compile_plain (xs : 'a stmt list) : document =
  let alias_module = !type_alias_module in
  let ys = List.map compile_stmt xs in
  type_alias_module := None;
  let body = separate (string ";;" ^^ hardline ^^ hardline) ys in
  match alias_module with
  | None -> body
  | Some module_name -> string "open " ^^ string module_name ^^ hardline ^^ hardline ^^ body

module Backend = struct
  let compile (stmts, _) = compile_plain stmts
end
