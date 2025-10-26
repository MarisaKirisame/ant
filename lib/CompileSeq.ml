open Syntax
open PPrint
module Hashtbl = Core.Hashtbl

type env = { arity : (string, int) Hashtbl.t; ctag : (string, int) Hashtbl.t }

let new_env () : env = { arity = Hashtbl.create (module Core.String); ctag = Hashtbl.create (module Core.String) }

let compile_ocaml_adt adt_name ctors =
  string
    ("type ocaml_" ^ adt_name ^ " = "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             if List.length types = 0 then con_name
             else
               con_name ^ " of "
               ^ String.concat " * "
                   (List.map
                      (fun ty ->
                        match ty with TNamed "int" -> "int" | TNamed _ -> "Seq.seq" | _ -> failwith (show_ty ty))
                      types))
           ctors))

let compile_adt_constructors (e : env) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      Hashtbl.add_exn ~key:con_name ~data:(List.length types) e.arity;
      let set_constructor_degree =
        string
          ("let () = Seq.set_constructor_degree" ^ " "
          ^ string_of_int (Hashtbl.length e.ctag)
          ^ "("
          ^ string_of_int (1 - List.length types)
          ^ ")")
      in
      Hashtbl.add_exn ~key:con_name ~data:(Hashtbl.length e.ctag) e.ctag;
      let register_constructor =
        string
          ("let " ^ adt_name ^ "_" ^ con_name ^ " "
          ^ String.concat " " (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
          ^ ": Seq.seq = Seq.appends ["
          ^ String.concat ";"
              (("Seq.from_constructor " ^ string_of_int (Hashtbl.find_exn e.ctag con_name))
              :: List.mapi
                   (fun i ty ->
                     let argname = "x" ^ string_of_int i in
                     match ty with
                     | TNamed "int" -> "Seq.from_int " ^ argname
                     | TNamed _ -> argname
                     | _ -> failwith (show_ty ty))
                   types)
          ^ "]")
      in
      set_constructor_degree ^^ break 1 ^^ register_constructor)
    ctors

let compile_adt_ffi e adt_name ctors =
  ignore e;
  string
    ("let from_ocaml_" ^ adt_name ^ " x = match x with | "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             let args = List.mapi (fun i _ -> "x" ^ string_of_int i) types in
             (if List.length types = 0 then con_name else con_name ^ "(" ^ String.concat ", " args ^ ")")
             ^ " -> " ^ adt_name ^ "_" ^ con_name ^ " " ^ String.concat " " args)
           ctors))
  ^^ break 1
  ^^ string
       ("let to_ocaml_" ^ adt_name
      ^ " x = let (h, t) = Option.get (Seq.list_match x) in match (Word.get_value h) with | "
       ^ String.concat " | "
           (List.map
              (fun (con_name, types) ->
                string_of_int (Hashtbl.find_exn e.ctag con_name)
                ^ " -> "
                ^
                if List.length types = 0 then con_name
                else
                  "let ["
                  ^ String.concat ";" (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
                  ^ "] = Seq.splits t in " ^ con_name ^ "("
                  ^ String.concat ","
                      (List.mapi
                         (fun i ty ->
                           match ty with
                           | TNamed "int" -> "Seq.to_int(" ^ "x" ^ string_of_int i ^ ")"
                           | TNamed _ -> "x" ^ string_of_int i
                           | _ -> failwith (show_ty ty))
                         types)
                  ^ ")")
              ctors))

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
  | Var x -> string x
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
