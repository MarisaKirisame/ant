open Syntax
open PPrint
open Code
module Hashtbl = Core.Hashtbl

let doc_of_code (c : 'a code) : document = Ir.ir_to_doc (Code.to_ir c)
let seq_fn name = from_ir (Ir.Function ("Seq." ^ name))
let word_fn name = from_ir (Ir.Function ("Word." ^ name))
let option_fn name = from_ir (Ir.Function ("Option." ^ name))
let seq_set_constructor_degree = seq_fn "set_constructor_degree"
let seq_from_constructor = seq_fn "from_constructor"
let seq_from_int = seq_fn "from_int"
let seq_appends_fn = seq_fn "appends"
let seq_list_match = seq_fn "list_match"
let seq_splits = seq_fn "splits"
let seq_to_int = seq_fn "to_int"
let word_get_value = word_fn "get_value"
let option_get = option_fn "get"

let list_literal_of_codes (xs : 'a code list) : 'a list code =
  code (string "[" ^^ separate (string "; ") (List.map doc_of_code xs) ^^ string "]")

let seq_appends xs = app seq_appends_fn (list_literal_of_codes xs)
let var name : 'a code = code (string name)
let paren_if_negative (i : int code) value = if value < 0 then from_ir (Ir.Paren (Code.to_ir i)) else i

type env = { arity : (string, int) Hashtbl.t; ctag : (string, int) Hashtbl.t }

let new_env () : env = { arity = Hashtbl.create (module Core.String); ctag = Hashtbl.create (module Core.String) }

let compile_seq_ty ty =
  match ty with TNamed "int" -> string "int" | TNamed _ -> string "Seq.seq" | _ -> failwith (show_ty ty)

let compile_ocaml_adt adt_name ctors =
  let ctor_doc (con_name, types) =
    let head = string con_name in
    match types with
    | [] -> head
    | _ ->
        head ^^ space ^^ string "of" ^^ space ^^ separate (space ^^ string "*" ^^ space) (List.map compile_seq_ty types)
  in
  let cases =
    match ctors with
    | [] -> empty
    | _ -> break 1 ^^ string "|" ^^ space ^^ separate_map (break 1 ^^ string "|" ^^ space) ctor_doc ctors
  in
  string "type ocaml_" ^^ string adt_name ^^ space ^^ string "=" ^^ nest 2 cases

let compile_adt_constructors (e : env) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      let params = List.mapi (fun i ty -> (ty, "x" ^ string_of_int i)) types in
      Hashtbl.add_exn ~key:con_name ~data:(List.length params) e.arity;
      let constructor_index = Hashtbl.length e.ctag in
      let degree = 1 - List.length params in
      let degree_code = paren_if_negative (int degree) degree in
      let set_constructor_degree_doc =
        string "let () = " ^^ doc_of_code (app2 seq_set_constructor_degree (int constructor_index) degree_code)
      in
      Hashtbl.add_exn ~key:con_name ~data:constructor_index e.ctag;
      let param_docs = List.map (fun (_, name) -> string name) params in
      let params_doc = if param_docs = [] then empty else space ^^ separate space param_docs in
      let value_codes =
        List.map
          (fun (ty, name) ->
            let name_code = var name in
            match ty with
            | TNamed "int" -> app seq_from_int name_code
            | TNamed _ -> name_code
            | _ -> failwith (show_ty ty))
          params
      in
      let body = seq_appends (app seq_from_constructor (int constructor_index) :: value_codes) in
      let register_constructor_doc =
        string "let " ^^ string adt_name ^^ string "_" ^^ string con_name ^^ params_doc ^^ space
        ^^ string ": Seq.seq = " ^^ doc_of_code body
      in
      group set_constructor_degree_doc ^^ break 1 ^^ group register_constructor_doc)
    ctors

let compile_adt_ffi e adt_name ctors =
  let from_case (con_name, types) =
    let params = List.mapi (fun i ty -> (ty, "x" ^ string_of_int i)) types in
    let pattern =
      match params with
      | [] -> string con_name
      | _ -> string con_name ^^ parens (separate (comma ^^ space) (List.map (fun (_, name) -> string name) params))
    in
    let body =
      string adt_name ^^ string "_" ^^ string con_name
      ^^ match params with [] -> empty | _ -> space ^^ separate space (List.map (fun (_, name) -> string name) params)
    in
    group @@ align @@ string "| " ^^ pattern ^^ space ^^ string "->" ^^ nest 2 (break 1 ^^ body)
  in
  let from_ocaml_doc =
    group @@ string "let from_ocaml_" ^^ string adt_name ^^ space ^^ string "x ="
    ^^ nest 2
         (break 1 ^^ string "match" ^^ space ^^ string "x" ^^ space ^^ string "with"
         ^^ concat_map (fun case -> break 1 ^^ case) (List.map from_case ctors))
  in
  let to_case (con_name, types) =
    let params = List.mapi (fun i ty -> (ty, "x" ^ string_of_int i)) types in
    let tag = string (string_of_int (Hashtbl.find_exn e.ctag con_name)) in
    let value_doc =
      match params with
      | [] -> string con_name
      | _ ->
          let binding =
            string "let ["
            ^^ separate (string "; ") (List.map (fun (_, name) -> string name) params)
            ^^ string "] = "
            ^^ doc_of_code (app seq_splits (var "t"))
            ^^ space ^^ string "in"
          in
          let converted_args =
            separate (comma ^^ space)
              (List.map
                 (fun (ty, name) ->
                   match ty with
                   | TNamed "int" -> doc_of_code (app seq_to_int (var name))
                   | TNamed _ -> string name
                   | _ -> failwith (show_ty ty))
                 params)
          in
          binding ^^ break 1 ^^ string con_name ^^ parens converted_args
    in
    group @@ align @@ string "| " ^^ tag ^^ space ^^ string "->" ^^ nest 2 (break 1 ^^ value_doc)
  in
  let default_case = string "| _ -> failwith \"unreachable\"" in
  let to_ocaml_doc =
    group @@ string "let to_ocaml_" ^^ string adt_name ^^ space ^^ string "x ="
    ^^ nest 2
         (break 1 ^^ string "let (h, t) = "
         ^^ doc_of_code (app option_get (app seq_list_match (var "x")))
         ^^ space ^^ string "in" ^^ break 1 ^^ string "match "
         ^^ parens (doc_of_code (app word_get_value (var "h")))
         ^^ space ^^ string "with"
         ^^ concat_map (fun case -> break 1 ^^ case) (List.map to_case ctors @ [ default_case ]))
  in
  from_ocaml_doc ^^ break 1 ^^ to_ocaml_doc

let compile_adt (e : env) adt_name ctors =
  let generate_ocaml_adt = compile_ocaml_adt adt_name ctors in
  let generate_adt_constructors = compile_adt_constructors e adt_name ctors in
  generate_ocaml_adt ^^ break 1 ^^ generate_adt_constructors ^^ break 1 ^^ compile_adt_ffi e adt_name ctors

let rec compile_pp_expr (e : expr) : document =
  match e with
  | Lam (xs, e) -> string "fun " ^^ separate_map space pp_pattern' xs ^^ string " -> " ^^ compile_pp_expr e
  | Match (value, MatchPattern cases) ->
      string "match (" ^^ string "to_ocaml_int_list" ^^ string " " ^^ compile_pp_expr value ^^ string ") with | "
      ^^ separate_map
           (break 1 ^^ string "|")
           (fun (pat, expr) -> pp_pattern pat ^^ string " -> " ^^ compile_pp_expr expr)
           cases
  | Ctor cname -> string "int_list_" ^^ string cname
  | Var x -> string x
  | GVar x -> string x
  | App (Ctor cname, []) -> string "int_list_" ^^ string cname
  | App (Ctor cname, es) ->
      string "int_list_" ^^ string cname ^^ string "(" ^^ separate_map (string ",") compile_pp_expr es ^^ string ")"
  | App (f, xs) -> string "(" ^^ separate_map (string " ") compile_pp_expr (f :: xs) ^^ string ")"
  | Op (op, l, r) -> string "(" ^^ compile_pp_expr l ^^ string op ^^ compile_pp_expr r ^^ string ")"
  | Int i -> string "(" ^^ string (string_of_int i) ^^ string ")"
  | _ -> failwith (show_expr e)

let compile_pp_stmt (e : env) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) -> (* TODO *) compile_adt e name ctors
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (x, tm) ->
      let name = match x with Some x -> pp_pattern x | None -> underscore in
      string "let rec" ^^ space ^^ name ^^ space ^^ string "=" ^^ space ^^ group @@ compile_pp_expr tm ^^ string ";;"
  | Fun (_name, _args, _body) -> failwith "Not implemented (TODO)"

let compile_ant x =
  string "open Ant" ^^ break 1 ^^ string "module Word = Seq.Word" ^^ break 1
  ^^ separate_map (break 1) (compile_pp_stmt (new_env ())) x
