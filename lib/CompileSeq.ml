open Syntax
open PPrint
open Code
module Hashtbl = Core.Hashtbl
module Ty = Type

let doc_of_code (c : 'a code) : document = Ir.ir_to_doc (Code.to_ir c)
let seq_fn name = from_ir (Ir.Function ("Seq." ^ name))
let seq_set_constructor_degree = seq_fn "set_constructor_degree"
let seq_from_constructor = seq_fn "from_constructor"
let seq_appends_fn = seq_fn "appends"

let list_literal_of_codes (xs : 'a code list) : 'a list code =
  code (string "[" ^^ separate (string "; ") (List.map doc_of_code xs) ^^ string "]")

let seq_appends xs = app_ seq_appends_fn (list_literal_of_codes xs)
let var name : 'a code = code (string name)
let paren_if_negative (i : int code) value = if value < 0 then from_ir (Ir.Paren (Code.to_ir i)) else i

type env = { arity : (string, int) Hashtbl.t; ctag : (string, int) Hashtbl.t; ctor_owner : (string, string) Hashtbl.t }

let new_env () : env =
  {
    arity = Hashtbl.create (module Core.String);
    ctag = Hashtbl.create (module Core.String);
    ctor_owner = Hashtbl.create (module Core.String);
  }

let rec compile_seq_ty ty =
  match ty with
  | TUnit -> string "unit"
  | TInt -> string "int"
  | TFloat -> string "float"
  | TBool -> string "bool"
  | TNamedVar name -> string ("'" ^ name)
  | TNamed _ | TApply (TNamed _, _) -> string "Seq.seq"
  | _ -> failwith (Printf.sprintf "Unsupported type: %s" (Syntax.string_of_document @@ Syntax.pp_ty ty))

let with_registered_constructor (e : env) adt_name con_name types k =
  let params = List.mapi (fun i _ -> "x" ^ string_of_int i) types in
  let arity = List.length params in
  Hashtbl.add_exn ~key:con_name ~data:arity e.arity;
  Hashtbl.add_exn ~key:con_name ~data:adt_name e.ctor_owner;
  let constructor_index = Hashtbl.length e.ctag in
  Hashtbl.add_exn ~key:con_name ~data:constructor_index e.ctag;
  k ~params ~arity ~constructor_index

let compile_adt_constructors (e : env) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types, _) ->
      with_registered_constructor e adt_name con_name types (fun ~params ~arity ~constructor_index ->
          let degree = 1 - arity in
          let degree_code = paren_if_negative (int_ degree) degree in
          let set_constructor_degree_doc =
            string "let () = " ^^ doc_of_code (app2_ seq_set_constructor_degree (int_ constructor_index) degree_code)
          in
          let param_docs = List.map string params in
          let params_doc = if param_docs = [] then empty else space ^^ separate space param_docs in
          let body = seq_appends (app_ seq_from_constructor (int_ constructor_index) :: List.map var params) in
          let register_constructor_doc =
            string "let " ^^ string adt_name ^^ string "_" ^^ string con_name ^^ params_doc ^^ space
            ^^ string ": Seq.seq = " ^^ doc_of_code body
          in
          group set_constructor_degree_doc ^^ break 1 ^^ group register_constructor_doc))
    ctors

let compile_adt (e : env) (tb : 'a ty_binding) =
  let split_expr n v =
    match n with
    | 1 -> string "List.hd" ^^ space ^^ parens (string "Seq.splits" ^^ space ^^ v)
    | 2 -> string "(match Seq.splits " ^^ v ^^ string " with [a; b] -> (a, b) | _ -> failwith \"bad arity\")"
    | 3 -> string "(match Seq.splits " ^^ v ^^ string " with [a; b; c] -> (a, b, c) | _ -> failwith \"bad arity\")"
    | 4 ->
        string "(match Seq.splits " ^^ v ^^ string " with [a; b; c; d] -> (a, b, c, d) | _ -> failwith \"bad arity\")"
    | _ -> assert false
  in
  let compile_conv is_to ty v =
    match ty with
    | TUnit -> if is_to then string "(ignore (Seq.to_int " ^^ v ^^ string "); ())" else string "(Seq.from_int 0)"
    | TInt -> if is_to then string "(Seq.to_int " ^^ v ^^ string ")" else string "(Seq.from_int " ^^ v ^^ string ")"
    | TBool ->
        if is_to then string "(Seq.to_int " ^^ v ^^ string " <> 0)"
        else string "(Seq.from_int (if " ^^ v ^^ string " then 1 else 0))"
    | TNamedVar name ->
        let conv_name = if is_to then "to_generic_" ^ name else "from_generic_" ^ name in
        string conv_name ^^ space ^^ v
    | TNamed _ | TApply (TNamed _, _) -> v
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
      splits_n = (fun n -> match n with 1 | 2 | 3 | 4 -> Some (fun v -> split_expr n v) | _ -> None);
      tag_expr = (fun ~cname:_ ~tag_id -> string (string_of_int tag_id));
      match_tag = (fun ~cname:_ ~tag_id -> string "|" ^^ space ^^ string (string_of_int tag_id) ^^ space ^^ string "->");
    }
  in
  let type_defs = CompileFfi.compile_type_defs ops tb in
  let constructors =
    let decls = match tb with TBOne (adt_name, kind) -> [ (adt_name, kind) ] | TBRec decls -> decls in
    separate_map (break 1) (fun (adt_name, Enum { ctors; _ }) -> compile_adt_constructors e adt_name ctors) decls
  in
  let conversions = CompileFfi.compile_conversions ops e.ctag tb in
  type_defs ^^ break 1 ^^ constructors ^^ break 1 ^^ conversions

let type_of_expr (e : SynInfo.info expr) : Ty.ty option =
  match e with
  | Unit -> Some (Ty.TPrim Ty.Unit)
  | Int _ -> Some (Ty.TPrim Ty.Int)
  | Float _ -> Some (Ty.TPrim Ty.Float)
  | Bool _ -> Some (Ty.TPrim Ty.Bool)
  | Str _ -> Some (Ty.TPrim Ty.Str)
  | Builtin (_, info)
  | Var (_, info)
  | GVar (_, info)
  | Ctor (_, info)
  | App (_, _, info)
  | Op (_, _, _, info)
  | Tup (_, info)
  | Arr (_, info)
  | Lam (_, _, info)
  | Let (_, _, info)
  | Sel (_, _, info)
  | If (_, _, _, info)
  | Match (_, _, info) ->
      info.ty

let ctor_ref (e : env) cname =
  let owner = Hashtbl.find_exn e.ctor_owner cname in
  string owner ^^ string "_" ^^ string cname

let rec seq_decoder_doc (ty : Ty.ty) : document =
  match Ty.repr ty with
  | Ty.TPrim Ty.Int -> string "(fun x -> Seq.to_int x)"
  | Ty.TPrim Ty.Bool -> string "(fun x -> Seq.to_int x <> 0)"
  | Ty.TPrim Ty.Unit -> string "(fun x -> ignore (Seq.to_int x); ())"
  | Ty.TApp _ | Ty.TVar _ -> string "(fun x -> x)"
  | _ -> string "(fun x -> x)"

let match_target_doc (ty : Ty.ty) (value_doc : document) : document =
  match Ty.repr ty with
  | Ty.TApp (name, args, _) ->
      let args_doc =
        match args with [] -> empty | _ -> space ^^ separate_map space (fun arg -> parens (seq_decoder_doc arg)) args
      in
      string "to_ocaml_" ^^ string name ^^ args_doc ^^ space ^^ value_doc
  | _ -> value_doc

let rec compile_expr_to_seq (e : env) (expr : SynInfo.info expr) : document =
  let expr_doc = compile_pp_expr e expr in
  match type_of_expr expr with
  | Some ty -> (
      match Ty.repr ty with
      | Ty.TPrim Ty.Int -> string "(Seq.from_int " ^^ expr_doc ^^ string ")"
      | Ty.TPrim Ty.Bool -> string "(Seq.from_int (if " ^^ expr_doc ^^ string " then 1 else 0))"
      | Ty.TPrim Ty.Unit -> string "(let _ = " ^^ expr_doc ^^ string " in Seq.from_int 0)"
      | Ty.TApp _ | Ty.TVar _ -> expr_doc
      | _ ->
          failwith
            (Printf.sprintf "Unsupported expression-to-seq conversion: %s"
               (Syntax.string_of_document @@ Syntax.pp_expr expr)))
  | None -> expr_doc

and compile_pp_expr (e : env) (expr : SynInfo.info expr) : document =
  match expr with
  | Unit -> string "()"
  | Int i -> string "(" ^^ string (string_of_int i) ^^ string ")"
  | Float f -> string (string_of_float f)
  | Bool b -> string (string_of_bool b)
  | Str s -> dquotes (string (String.escaped s))
  | Builtin (Builtin b, _) -> string b
  | Var (x, _) -> string x
  | GVar (x, _) -> string x
  | Ctor (cname, _) -> ctor_ref e cname
  | App (Ctor (cname, _), [], _) -> ctor_ref e cname
  | App (Ctor (cname, _), args, _) ->
      ctor_ref e cname ^^ space ^^ separate_map space (fun arg -> parens (compile_expr_to_seq e arg)) args
  | App (f, xs, _) -> parens (separate_map space (compile_pp_expr e) (f :: xs))
  | Op (op, l, r, _) -> parens (compile_pp_expr e l ^^ string " " ^^ string op ^^ string " " ^^ compile_pp_expr e r)
  | Tup (xs, _) -> parens (separate_map (string ", ") (compile_pp_expr e) xs)
  | Arr (xs, _) -> brackets (separate_map (string "; ") (compile_pp_expr e) xs)
  | Lam (xs, body, _) -> string "fun " ^^ separate_map space pp_pattern' xs ^^ string " -> " ^^ compile_pp_expr e body
  | Let (binding, value, _) -> compile_binding e binding (parens_compile_expr e value)
  | Sel (target, field, _) -> parens_compile_expr e target ^^ string "." ^^ pp_field field
  | If (c, p, n, _) ->
      string "if " ^^ compile_pp_expr e c ^^ string " then " ^^ parens_compile_expr e p ^^ string " else "
      ^^ parens_compile_expr e n
  | Match (value, MatchPattern cases, _) ->
      let value_doc = compile_pp_expr e value in
      let target_doc = match type_of_expr value with Some ty -> match_target_doc ty value_doc | None -> value_doc in
      string "match " ^^ parens target_doc ^^ string " with "
      ^^ separate_map (string "| ")
           (fun (pat, branch) -> pp_pattern pat ^^ string " -> " ^^ compile_pp_expr e branch)
           cases

and parens_compile_expr e expr = parens (compile_pp_expr e expr)

and compile_binding (e : env) (binding : SynInfo.info binding) (cont : document) : document =
  match binding with
  | BSeq (value, _) -> compile_pp_expr e value ^^ string ";" ^^ cont
  | BOne (pat, value, _) | BCont (pat, value, _) -> string "let " ^^ compile_let e (pat, value) ^^ string " in " ^^ cont
  | BRec xs | BRecC xs ->
      string "let rec "
      ^^ separate_map (string " and ") (compile_let e) (List.map (fun (p, v, _) -> (p, v)) xs)
      ^^ string " in " ^^ cont

and compile_let e (pat, value) = pp_pattern' pat ^^ string " = " ^^ parens (compile_pp_expr e value)

let compile_pp_stmt (e : env) (stmt : SynInfo.info stmt) : document =
  match stmt with
  | Type tb -> compile_adt e tb
  | Term (BSeq (tm, _)) -> string "let _ = " ^^ compile_pp_expr e tm ^^ string ";;"
  | Term (BOne (pat, tm, _) | BCont (pat, tm, _)) ->
      string "let " ^^ pp_pattern pat ^^ space ^^ string "=" ^^ space ^^ group (compile_pp_expr e tm) ^^ string ";;"
  | Term (BRec [] | BRecC []) -> failwith "Empty recursive group"
  | Term (BRec bindings | BRecC bindings) ->
      string "let rec" ^^ space
      ^^ separate_map
           (space ^^ string "and" ^^ space)
           (fun (pat, tm, _) -> pp_pattern pat ^^ space ^^ string "=" ^^ space ^^ group (compile_pp_expr e tm))
           bindings
      ^^ string ";;"

let compile_ant stmts =
  let env = new_env () in
  string "open Ant" ^^ break 1 ^^ string "module Word = Seq.Word" ^^ break 1
  ^^ separate_map (break 1) (compile_pp_stmt env) stmts

module Backend = struct
  let compile (stmts, _) = compile_ant (Obj.magic stmts : SynInfo.info stmt list)
end
