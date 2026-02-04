open Syntax
open PPrint
open Code
module Hashtbl = Core.Hashtbl

let doc_of_code (c : 'a code) : document = Ir.ir_to_doc (Code.to_ir c)
let seq_fn name = from_ir (Ir.Function ("Seq." ^ name))
let seq_set_constructor_degree = seq_fn "set_constructor_degree"
let seq_from_constructor = seq_fn "from_constructor"
let seq_from_int = seq_fn "from_int"
let seq_appends_fn = seq_fn "appends"

let list_literal_of_codes (xs : 'a code list) : 'a list code =
  code (string "[" ^^ separate (string "; ") (List.map doc_of_code xs) ^^ string "]")

let seq_appends xs = app_ seq_appends_fn (list_literal_of_codes xs)
let var name : 'a code = code (string name)
let paren_if_negative (i : int code) value = if value < 0 then from_ir (Ir.Paren (Code.to_ir i)) else i

type env = { arity : (string, int) Hashtbl.t; ctag : (string, int) Hashtbl.t }

let new_env () : env = { arity = Hashtbl.create (module Core.String); ctag = Hashtbl.create (module Core.String) }

let compile_seq_ty ty =
  match ty with
  | TInt -> string "int"
  | TNamed _ -> string "Seq.seq"
  | _ -> failwith (Printf.sprintf "Unsupported type: %s" (Syntax.string_of_document @@ Syntax.pp_ty ty))

let with_registered_constructor (e : env) con_name types k =
  let params = List.mapi (fun i ty -> (ty, "x" ^ string_of_int i)) types in
  let arity = List.length params in
  Hashtbl.add_exn ~key:con_name ~data:arity e.arity;
  let constructor_index = Hashtbl.length e.ctag in
  Hashtbl.add_exn ~key:con_name ~data:constructor_index e.ctag;
  k ~params ~arity ~constructor_index

let compile_adt_constructors (e : env) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types, _) ->
      with_registered_constructor e con_name types (fun ~params ~arity ~constructor_index ->
          let degree = 1 - arity in
          let degree_code = paren_if_negative (int_ degree) degree in
          let set_constructor_degree_doc =
            string "let () = " ^^ doc_of_code (app2_ seq_set_constructor_degree (int_ constructor_index) degree_code)
          in
          let param_docs = List.map (fun (_, name) -> string name) params in
          let params_doc = if param_docs = [] then empty else space ^^ separate space param_docs in
          let value_codes =
            List.map
              (fun (ty, name) ->
                let name_code = var name in
                match ty with
                | TInt -> app_ seq_from_int name_code
                | TNamed _ -> name_code
                | _ -> failwith (Syntax.string_of_document @@ Syntax.pp_ty ty))
              params
          in
          let body = seq_appends (app_ seq_from_constructor (int_ constructor_index) :: value_codes) in
          let register_constructor_doc =
            string "let " ^^ string adt_name ^^ string "_" ^^ string con_name ^^ params_doc ^^ space
            ^^ string ": Seq.seq = " ^^ doc_of_code body
          in
          group set_constructor_degree_doc ^^ break 1 ^^ group register_constructor_doc))
    ctors

let compile_adt (e : env) (tb : 'a ty_binding) =
  let compile_conv is_to ty v =
    match ty with
    | TInt ->
        if is_to then string "(Seq.to_int " ^^ v ^^ string ")" else string "(Seq.from_int " ^^ v ^^ string ")"
    | TNamed _ -> v
    | _ -> failwith (Printf.sprintf "Unsupported type: %s" (Syntax.string_of_document @@ Syntax.pp_ty ty))
  in
  let ops : 'a CompileFfi.ops =
    {
      type_name_of = (fun name -> "ocaml_" ^ name);
      compile_ty = compile_seq_ty;
      compile_conv = (fun ~is_to ty v -> compile_conv is_to ty v);
      appends = (fun xs -> string "Seq.appends" ^^ space ^^ brackets (separate (semi ^^ space) xs));
      from_constructor = (fun tag -> string "Seq.from_constructor" ^^ space ^^ tag);
      list_match = (fun v -> string "Seq.list_match" ^^ space ^^ v);
      word_get_value = (fun v -> string "Word.get_value" ^^ space ^^ v);
      splits = (fun v -> string "Seq.splits" ^^ space ^^ v);
      splits_n = (fun _ -> None);
      tag_expr = (fun ~cname:_ ~tag_id -> string (string_of_int tag_id));
      match_tag = (fun ~cname:_ ~tag_id -> string "|" ^^ space ^^ string (string_of_int tag_id) ^^ space ^^ string "->");
    }
  in
  let type_defs = CompileFfi.compile_type_defs ops tb in
  let constructors =
    match tb with
    | TBOne (adt_name, Enum { ctors; _ }) -> compile_adt_constructors e adt_name ctors
    | TBRec _ -> failwith "Not implemented (TODO)"
  in
  let conversions = CompileFfi.compile_conversions ops e.ctag tb in
  type_defs ^^ break 1 ^^ constructors ^^ break 1 ^^ conversions

let rec compile_pp_expr (e : 'a expr) : document =
  match e with
  | Lam (xs, e, _) -> string "fun " ^^ separate_map space pp_pattern' xs ^^ string " -> " ^^ compile_pp_expr e
  | Match (value, MatchPattern cases, _) ->
      string "match (" ^^ string "to_ocaml_int_list" ^^ space ^^ compile_pp_expr value ^^ string ") with | "
      ^^ separate_map
           (break 1 ^^ string "|")
           (fun (pat, expr) -> pp_pattern pat ^^ string " -> " ^^ compile_pp_expr expr)
           cases
  | Ctor (cname, _) -> string "int_list_" ^^ string cname
  | Var (x, _) -> string x
  | GVar (x, _) -> string x
  | App (Ctor (cname, _), [], _) -> string "int_list_" ^^ string cname
  | App (Ctor (cname, _), es, _) ->
      string "int_list_" ^^ string cname ^^ string "(" ^^ separate_map (string ",") compile_pp_expr es ^^ string ")"
  | App (f, xs, _) -> string "(" ^^ separate_map space compile_pp_expr (f :: xs) ^^ string ")"
  | Op (op, l, r, _) -> string "(" ^^ compile_pp_expr l ^^ string op ^^ compile_pp_expr r ^^ string ")"
  | Int i -> string "(" ^^ string (string_of_int i) ^^ string ")"
  | _ -> failwith (Syntax.string_of_document @@ Syntax.pp_expr e)

let compile_pp_stmt (e : env) (s : 'a stmt) : document =
  match s with
  | Type (TBOne _ as tb) -> compile_adt e tb
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (BSeq (tm, _)) -> compile_pp_expr tm
  | Term (BOne (x, tm, _)) ->
      string "let rec" ^^ space ^^ pp_pattern x ^^ space ^^ string "=" ^^ space ^^ group @@ compile_pp_expr tm
      ^^ string ";;"
  | Term (BRec bindings) ->
      string "let rec" ^^ space
      ^^ separate_map
           (space ^^ string "and" ^^ space)
           (fun (x, tm, _) -> pp_pattern x ^^ space ^^ string "=" ^^ space ^^ group @@ compile_pp_expr tm)
           bindings
      ^^ string ";;"
  | _ -> failwith "Not implemented (TODO)"

let compile_ant x =
  string "open Ant" ^^ break 1 ^^ string "module Word = Seq.Word" ^^ break 1
  ^^ separate_map (break 1) (compile_pp_stmt (new_env ())) x

module Backend = struct
  let compile (stmts, _) = compile_ant stmts
end
