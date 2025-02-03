open Common
open PPrint
open Syntax
open Memo
(* As K come with interpretative overhead,
 *   we want to use K as little as possible,
 *   instead storing the computed temporary variables onto the env as a stack,
 *   only using K whenever we do a non-tail function call.
 *)

type 'a code = document

type ctx = {
  arity : (string, int) Hashtbl.t;
  ctag : (string, int) Hashtbl.t;
  constructor_degree : int Dynarray.t;
  conts : (string * (words -> state -> state) code) Dynarray.t;
  func_pc : (string, int) Hashtbl.t;
}

let add_cont (ctx : ctx) (name : string) (arity : int) : ctx =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, string "todo:cont");
  ctx

let new_ctx () : ctx =
  add_cont
    {
      arity = Hashtbl.create (module Core.String);
      ctag = Hashtbl.create (module Core.String);
      constructor_degree = Dynarray.create ();
      conts = Dynarray.create ();
      func_pc = Hashtbl.create (module Core.String);
    }
    "cont_done" 0

let codes : document option Dynarray.t = Dynarray.create ()

type pc = int

let add_code (c : document option) : pc =
  let pc = Dynarray.length codes in
  Dynarray.add_last codes c;
  pc

let set_code (i : int) (c : document) : unit = Dynarray.set codes i (Some c)

let ant_pp_ocaml_adt adt_name ctors =
  string
    ("type ocaml_" ^ adt_name ^ " = "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             if List.length types == 0 then con_name
             else
               con_name ^ " of "
               ^ String.concat " * "
                   (List.map
                      (fun ty ->
                        match ty with TNamed "int" -> "int" | TNamed _ -> "Memo.seq" | _ -> failwith (show_ty ty))
                      types))
           ctors))

let ant_pp_adt_constructors (e : ctx) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      Hashtbl.add_exn ~key:con_name ~data:(List.length types) e.arity;
      Hashtbl.add_exn ~key:con_name ~data:(Hashtbl.length e.ctag) e.ctag;
      Dynarray.add_last e.constructor_degree (1 - List.length types);
      let register_constructor =
        string
          ("let " ^ adt_name ^ "_" ^ con_name ^ " "
          ^ String.concat " " (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
          ^ ": Memo.seq = Memo.appends ["
          ^ String.concat ";"
              (("Memo.from_constructor " ^ string_of_int (Hashtbl.find_exn e.ctag con_name))
              :: List.mapi
                   (fun i ty ->
                     let argname = "x" ^ string_of_int i in
                     match ty with
                     | TNamed "int" -> "Memo.from_int " ^ argname
                     | TNamed _ -> argname
                     | _ -> failwith (show_ty ty))
                   types)
          ^ "]")
      in
      register_constructor)
    ctors

let ant_pp_adt_ffi e adt_name ctors =
  ignore e;
  string
    ("let from_ocaml_" ^ adt_name ^ " x = match x with | "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             let args = List.mapi (fun i _ -> "x" ^ string_of_int i) types in
             (if List.length types == 0 then con_name else con_name ^ "(" ^ String.concat ", " args ^ ")")
             ^ " -> " ^ adt_name ^ "_" ^ con_name ^ " " ^ String.concat " " args)
           ctors))
  ^^ break 1
  ^^ string
       ("let to_ocaml_" ^ adt_name
      ^ " x = let (h, t) = Option.get (Memo.list_match x) in match (Word.get_value h) with | "
       ^ String.concat " | "
           (List.map
              (fun (con_name, types) ->
                string_of_int (Hashtbl.find_exn e.ctag con_name)
                ^ " -> "
                ^
                if List.length types == 0 then con_name
                else
                  "let ["
                  ^ String.concat ";" (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
                  ^ "] = Memo.splits t in " ^ con_name ^ "("
                  ^ String.concat ","
                      (List.mapi
                         (fun i ty ->
                           match ty with
                           | TNamed "int" -> "Memo.to_int(" ^ "x" ^ string_of_int i ^ ")"
                           | TNamed _ -> "x" ^ string_of_int i
                           | _ -> failwith (show_ty ty))
                         types)
                  ^ ")")
              ctors))

let ant_pp_adt (e : ctx) adt_name ctors =
  (*force evaluation order via let*)
  let generate_ocaml_adt = ant_pp_ocaml_adt adt_name ctors in
  let generate_adt_constructors = ant_pp_adt_constructors e adt_name ctors in
  generate_ocaml_adt ^^ break 1 ^^ generate_adt_constructors ^^ break 1 ^^ ant_pp_adt_ffi e adt_name ctors

type env = (string, int) Hashtbl.t

let new_env () : env = Hashtbl.create (module Core.String)

let return (loc : int) : pc =
  add_code (Some (string "(fun x -> todo \"return last element in env by applying the continuation\"; x)"))

let rec ant_pp_expr (ctx : ctx) (env : env) (c : expr) (e : int) (k : pc) : pc =
  match c with
  | Var name ->
      let loc = Hashtbl.find_exn env name in
      add_code
        (Some
           (string
              ("(fun x -> " ^ "Dynarray.add_last x.e (" ^ "Dynarray.get x.e " ^ string_of_int loc ^ ");"
             ^ "x.c <- pc_to_exp " ^ string_of_int k ^ ";" ^ " x)")))
  | Match (value, cases) -> ant_pp_expr ctx env value e (ant_pp_cases ctx env cases (e + 1) k)
  | Ctor cname -> add_code (Some (string ("(fun x -> todo \"" ^ cname ^ "\")")))
  | App (App (Ctor cname, [ x0 ]), [ x1 ]) ->
      ant_pp_expr ctx env x0 e
        (ant_pp_expr ctx env x1 (e + 1)
           (let let_x1 = string "let x1 = (Dynarray.pop_last x.e).seq in " in
            let let_x0 = string "let x0 = (Dynarray.pop_last x.e).seq in " in
            let add_last =
              string
                ("Dynarray.add_last x.e " ^ "(value_at_depth (Memo.appends [Memo.from_constructor "
                ^ string_of_int (Hashtbl.find_exn ctx.ctag cname)
                ^ ";x0;x1]) x.d)" ^ "; ")
            in
            add_code
              (Some
                 (string "(fun x -> " ^^ let_x1 ^^ let_x0 ^^ add_last
                 ^^ string ("x.c <- pc_to_exp " ^ string_of_int k ^ "; x)")))))
  | App (Var "list_incr", [ x ]) ->
      ant_pp_expr ctx env x e
        (add_code
           (Some
              (string
                 ("(fun x -> let sf = env_keep_last_n x 1 in x.c <- pc_to_exp "
                 ^ string_of_int (Hashtbl.find_exn ctx.func_pc "list_incr")
                 ^ "; x)"))))
  | Op ("+", x0, x1) ->
      ant_pp_expr ctx env x0 e
        (ant_pp_expr ctx env x1 (e + 1)
           (let let_x1 = string "let x1 = (Dynarray.pop_last x.e).seq in " in
            let let_x0 = string "let x0 = (Dynarray.pop_last x.e).seq in " in
            let add_last =
              string "Dynarray.add_last x.e (value_at_depth (Memo.from_int (Memo.to_int x0 + Memo.to_int x1)) x.d);"
            in
            add_code
              (Some
                 (string "(fun x -> " ^^ let_x1 ^^ let_x0 ^^ add_last
                 ^^ string ("x.c <- pc_to_exp " ^ string_of_int k ^ "; x)")))))
  | Int i ->
      let add_last = string ("Dynarray.add_last x.e (value_at_depth (Memo.from_int (" ^ string_of_int i ^ ")) x.d);") in
      add_code (Some (string "(fun x -> " ^^ add_last ^^ string ("x.c <- pc_to_exp " ^ string_of_int k ^ "; x)")))
  | _ -> failwith ("ant_pp_expr: " ^ show_expr c)

and ant_pp_cases ctx (env : env) (MatchPattern c : cases) (e : int) (k : pc) : pc =
  add_code
    (Some
       (string ("(fun x -> " ^ "let last = (Dynarray.pop_last x.e).seq in ")
       ^^ break 1
       ^^ string "let (hd, tl) = Option.get (resolve_seq x last) in "
       ^^ break 1 ^^ string " match Word.get_value hd with "
       ^^ separate_map (break 1)
            (fun (pat, expr) ->
              (* special casing for now, as pat design need changes. *)
              match pat with
              | PApp (cname, None) ->
                  string "| "
                  ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                  ^^ string " -> "
                  ^^ string ("(x.c <- pc_to_exp " ^ string_of_int (ant_pp_expr ctx env expr (e - 1) k) ^ "; x)")
              | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                  Hashtbl.add_exn env ~key:x0 ~data:(e - 1);
                  Hashtbl.add_exn env ~key:x1 ~data:(e - 1 + 1);
                  string "| "
                  ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                  ^^ string " -> "
                  ^^ string
                       "(let [x0; x1] = Memo.splits tl in Dynarray.add_last x.e (value_at_depth x0 x.d); \
                        Dynarray.add_last x.e (value_at_depth x1 x.d);"
                  ^^ string ("x.c <- pc_to_exp " ^ string_of_int (ant_pp_expr ctx env expr (e - 1 + 2) k) ^ ";")
                  ^^ string " x)"
              | _ -> failwith (show_pattern pat))
            c
       ^^ string ")"))

let ant_pp_stmt (ctx : ctx) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) -> (* TODO *) ant_pp_adt ctx name ctors
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (x, Lam (ps, term)) ->
      let env = new_env () in
      List.iter
        (fun p -> match p with PVar n -> Hashtbl.add_exn env n (Hashtbl.length env) | _ -> failwith (show_pattern p))
        ps;
      let env_length = Hashtbl.length env in
      let ret_pc = return env_length in
      let name = match x with Some (PVar x) -> x in
      let entry_code = add_code None in
      Hashtbl.add_exn ctx.func_pc name entry_code;
      let term_code = ant_pp_expr ctx env term env_length ret_pc in
      set_code entry_code (string ("(fun x -> x.c <- pc_to_exp " ^ string_of_int term_code ^ "; x)"));
      string "let rec" ^^ space ^^ string name ^^ space
      ^^ separate_map space (fun i -> string ("(x" ^ string_of_int i ^ " : seq)")) (List.init env_length (fun x -> x))
      ^^ string ": seq " ^^ string "=" ^^ space ^^ group @@ string "exec_cek "
      ^^ string ("(pc_to_exp " ^ string_of_int term_code ^ ")")
      ^^ string "(Dynarray.of_list" ^^ string "["
      ^^ separate_map (string ";") (fun i -> string ("(x" ^ string_of_int i ^ ")")) (List.init env_length (fun x -> x))
      ^^ string "]" ^^ string ")" ^^ string "(Memo.from_constructor "
      ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag "cont_done"))
      ^^ string ")" ^^ string ";;"
  | Fun (_name, _args, _body) -> failwith "Not implemented (TODO)"
  | _ -> failwith (show_stmt s)

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (ant_pp_stmt ctx) x in
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Common"
  ^^ break 1 ^^ generated_stmt ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string ("let " ^ string_of_int i ^ " = add_exp ") ^^ Option.get (Dynarray.get codes i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string
              ("let () = Memo.set_constructor_degree " ^ string_of_int i ^ " " ^ "("
              ^ string_of_int (Dynarray.get ctx.constructor_degree i)
              ^ ")")))
