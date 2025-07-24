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
  conts : (string * (state -> words -> state) code) Dynarray.t;
  func_pc : (string, int) Hashtbl.t;
}

let add_cont (ctx : ctx) (name : string) (arity : int) (app : (state -> words -> state) code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app)

let new_ctx () : ctx =
  let ctx =
    {
      arity = Hashtbl.create (module Core.String);
      ctag = Hashtbl.create (module Core.String);
      constructor_degree = Dynarray.create ();
      conts = Dynarray.create ();
      func_pc = Hashtbl.create (module Core.String);
    }
  in
  add_cont ctx "cont_done" 0 (string "(fun x tl -> exec_done x)");
  ctx

let codes : document option Dynarray.t = Dynarray.create ()

type pc = int

let add_code (c : document option) : pc =
  let pc = Dynarray.length codes in
  Dynarray.add_last codes c;
  pc

let set_code (i : int) (c : document) : unit = Dynarray.set codes i (Some c)

let add_code_k (k : pc -> document * 'a) : 'a =
  let pc = add_code None in
  let code, ret = k pc in
  set_code pc code;
  ret

let ant_pp_ocaml_adt adt_name ctors =
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
                        match ty with TNamed "int" -> "int" | TNamed _ -> "Value.seq" | _ -> failwith (show_ty ty))
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
          ^ ": Value.seq = Memo.appends ["
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
             (if List.length types = 0 then con_name else con_name ^ "(" ^ String.concat ", " args ^ ")")
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
                if List.length types = 0 then con_name
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

let apply_cont : pc = add_code None

type env = (string, int) Hashtbl.t

let new_env () : env = Hashtbl.create (module Core.String)

type scope = {
  meta_env : (string, int option) Hashtbl.t linear;
  (*Note: env_length is not the amount of entries in meta_env above! It is the length of the environment when executing the cek machine.*)
  env_length : int;
}

let new_scope () = { meta_env = make_linear (Hashtbl.create (module Core.String)); env_length = 0 }
let push_s s = { meta_env = s.meta_env; env_length = s.env_length + 1 }

let extend_s s name =
  let meta_env = write_linear s.meta_env in
  Hashtbl.add_exn meta_env ~key:name ~data:(Some s.env_length);
  { meta_env = make_linear meta_env; env_length = s.env_length + 1 }

let drop_s s name =
  assert (Option.is_some (Hashtbl.find_exn (read_linear s.meta_env) name));
  let meta_env = write_linear s.meta_env in
  Hashtbl.remove meta_env name;
  { meta_env = make_linear meta_env; env_length = s.env_length - 1 }

let pop_n s n =
  assert (s.env_length >= n);
  { meta_env = s.meta_env; env_length = s.env_length - n }

let pop_s s = pop_n s 1
let dup_s s = { meta_env = make_linear (Hashtbl.copy (read_linear s.meta_env)); env_length = s.env_length }

type kont = { k : scope -> pc; fv : (string, unit) Hashtbl.t linear }

let dup_fv (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  make_linear (Hashtbl.copy (read_linear fv))

let empty_fv () : (string, unit) Hashtbl.t linear = make_linear (Hashtbl.create (module Core.String))

let drop (s : scope) (vars : string list) (k : kont) : pc =
  let new_s, n =
    List.fold_left
      (fun (s, n) var ->
        match Hashtbl.find_exn (read_linear s.meta_env) var with None -> (s, n) | Some _ -> (drop_s s var, n + 1))
      (s, 0) vars
  in
  add_code_k (fun pc ->
      ( string
          ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; drop_n x " ^ string_of_int s.env_length
         ^ " " ^ string_of_int n ^ " (pc_to_exp "
          ^ string_of_int (k.k new_s)
          ^ "))"),
        pc ))

let return (s : scope) : pc =
  add_code
    (Some
       (string
          ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; return_n x " ^ string_of_int s.env_length
         ^ " (pc_to_exp " ^ string_of_int apply_cont ^ "))")))

let add_fv (v : string) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  let fv = write_linear fv in
  ignore (Hashtbl.add fv ~key:v ~data:());
  make_linear fv

let remove_fv (v : string) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  let fv = write_linear fv in
  ignore (Hashtbl.remove fv v);
  make_linear fv

let rec fv_expr (e : expr) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  match e with
  | Ctor _ -> fv
  | App (f, xs) -> List.fold_left (fun fv e -> fv_expr e fv) (fv_expr f fv) xs
  | Op (_, x, y) -> fv_expr y (fv_expr x fv)
  | Var name -> add_fv name fv
  | Int _ -> fv
  | _ -> failwith ("fv_expr: " ^ show_expr e)

let rec fv_pat (pat : pattern) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  match pat with
  | PApp (_, None) -> fv
  | PApp (_, Some x) -> fv_pat x fv
  | PTup xs -> List.fold_left (fun fv x -> fv_pat x fv) fv xs
  | PVar name -> remove_fv name fv
  | _ -> failwith (show_pattern pat)

let fv_cases (MatchPattern c : cases) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  List.fold_left (fun fv (pat, e) -> fv_pat pat (fv_expr e fv)) fv c

type keep_t = { mutable keep : bool; mutable source : string option }

let keep_only (s : scope) (fv : (string, unit) Hashtbl.t linear) : int Dynarray.t * scope =
  let keep : keep_t Dynarray.t = Dynarray.init s.env_length (fun _ -> { keep = true; source = None }) in
  Hashtbl.iteri (read_linear s.meta_env) ~f:(fun ~key ~data ->
      match data with None -> () | Some i -> Dynarray.set keep i { keep = false; source = Some key });
  Hashtbl.iter_keys (read_linear fv) ~f:(fun v ->
      let i = Option.get (Hashtbl.find_exn (read_linear s.meta_env) v) in
      (Dynarray.get keep i).keep <- true);
  let keep_idx : int Dynarray.t = Dynarray.create () in
  let meta_env : (string, int option) Hashtbl.t = Hashtbl.create (module Core.String) in
  Dynarray.iteri
    (fun i k ->
      if k.keep then (
        (match k.source with
        | None -> ()
        | Some v -> Hashtbl.add_exn meta_env ~key:v ~data:(Some (Dynarray.length keep_idx)));
        Dynarray.add_last keep_idx i)
      else ())
    keep;
  Hashtbl.iteri (read_linear s.meta_env) ~f:(fun ~key ~data ->
      if Option.is_some data then ignore (Hashtbl.add meta_env ~key ~data:None));
  (keep_idx, { meta_env = make_linear meta_env; env_length = Dynarray.length keep_idx })

let rec ant_pp_expr (ctx : ctx) (s : scope) (c : expr) (k : kont) : pc =
  match c with
  | Var name ->
      add_code_k (fun pc ->
          let loc = Option.get (Hashtbl.find_exn (read_linear s.meta_env) name) in
          ( string
              ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; " ^ "push_env x ("
             ^ "Dynarray.get x.e " ^ string_of_int loc ^ ");" ^ "x.c <- pc_to_exp "
              ^ string_of_int (k.k (push_s s))
              ^ ";" ^ " stepped x)"),
            pc ))
  | Match (value, cases) ->
      ant_pp_expr ctx s value { k = (fun s -> ant_pp_cases ctx (dup_s s) cases k); fv = fv_cases cases (dup_fv k.fv) }
  | Ctor cname ->
      add_code_k (fun pc ->
          ( string
              ("(fun x -> assert_env_length x " ^ string_of_int s.env_length
             ^ "; (push_env x (value_at_depth (Memo.from_constructor "
              ^ string_of_int (Hashtbl.find_exn ctx.ctag cname)
              ^ ") x.d)); x.c <- pc_to_exp "
              ^ string_of_int (k.k (push_s s))
              ^ "; stepped x)"),
            pc ))
  | App (App (Ctor cname, [ x0 ]), [ x1 ]) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s ->
              ant_pp_expr ctx s x1
                {
                  k =
                    (fun s ->
                      add_code_k (fun pc ->
                          let let_x1 = string "let x1 = (pop_env x).seq in " in
                          let let_x0 = string "let x0 = (pop_env x).seq in " in
                          let add_last =
                            string
                              ("push_env x " ^ "(value_at_depth (Memo.appends [Memo.from_constructor "
                              ^ string_of_int (Hashtbl.find_exn ctx.ctag cname)
                              ^ ";x0;x1]) x.d)" ^ "; ")
                          in

                          ( string ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; ")
                            ^^ let_x1 ^^ let_x0 ^^ add_last
                            ^^ string
                                 ("x.c <- pc_to_exp " ^ string_of_int (k.k (push_s (pop_s (pop_s s)))) ^ "; stepped x)"),
                            pc )));
                  fv = k.fv;
                });
          fv = fv_expr x1 (dup_fv k.fv);
        }
  | App (Var "list_incr", [ x ]) ->
      let cont_name = "cont_" ^ string_of_int (Dynarray.length ctx.conts) in
      let keep, keep_s = keep_only s k.fv in
      print_endline (string_of_int keep_s.env_length);
      (* subtracting 1 to remove the arguments; adding 1 for the next continuation*)
      let keep_length = keep_s.env_length in
      add_cont ctx cont_name (keep_length + 1)
        (string "(fun x tl -> restore_env x "
        ^^ string (string_of_int keep_length)
        ^^ string " tl; x.k <- value_at_depth (get_next_cont tl) x.d; x.c <- pc_to_exp "
        ^^ string (string_of_int (k.k (push_s keep_s)))
        ^^ string "; stepped x)");
      ant_pp_expr ctx s x
        {
          k =
            (fun s ->
              add_code_k (fun pc ->
                  ( string
                      ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; let keep = env_call x ["
                      ^ String.concat ";" (List.map string_of_int (Dynarray.to_list keep))
                      ^ "] 1 in x.k <- value_at_depth (Memo.appends [Memo.from_constructor "
                      ^ string_of_int (Hashtbl.find_exn ctx.ctag cont_name)
                      ^ "; keep; x.k.seq]) x.d; x.c <- pc_to_exp "
                      ^ string_of_int (Hashtbl.find_exn ctx.func_pc "list_incr")
                      ^ "; stepped x)"),
                    pc )));
          fv = k.fv;
        }
  | Op ("+", x0, x1) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s ->
              ant_pp_expr ctx s x1
                {
                  k =
                    (fun s ->
                      let x0 =
                        string ("(resolve_seq x (Dynarray.get x.e " ^ string_of_int (s.env_length - 2) ^ ").seq)")
                      in
                      let x1 =
                        string ("(resolve_seq x (Dynarray.get x.e " ^ string_of_int (s.env_length - 1) ^ ").seq)")
                      in
                      let add_last = string "push_env x (value_at_depth (Memo.from_int (x0 + x1)) x.d);" in
                      add_code_k (fun pc ->
                          ( string ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; match ")
                            ^^ x0
                            ^^ string " with  | None -> resolve_failed x memo | Some (x0, _) -> match "
                            ^^ x1
                            ^^ string
                                 " with None -> resolve_failed x memo | Some (x1, _) -> (Dynarray.remove_last \
                                  x.e;Dynarray.remove_last x.e;"
                            ^^ add_last
                            ^^ string
                                 ("x.c <- pc_to_exp " ^ string_of_int (k.k (push_s (pop_s (pop_s s)))) ^ "; stepped x))"),
                            pc )));
                  fv = fv_expr x1 (dup_fv k.fv);
                });
          fv = k.fv;
        }
  | Int i ->
      let add_last = string ("push_env x (value_at_depth (Memo.from_int (" ^ string_of_int i ^ ")) x.d);") in
      add_code_k (fun pc ->
          ( string ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; ")
            ^^ add_last
            ^^ string ("x.c <- pc_to_exp " ^ string_of_int (k.k (push_s s)) ^ "; stepped x)"),
            pc ))
  | _ -> failwith ("ant_pp_expr: " ^ show_expr c)

and ant_pp_cases (ctx : ctx) (s : scope) (MatchPattern c : cases) (k : kont) : pc =
  add_code_k (fun pc ->
      ( string
          ("(fun x -> assert_env_length x " ^ string_of_int s.env_length ^ "; "
         ^ "let last = (Dynarray.get_last x.e).seq in ")
        ^^ break 1
        ^^ string
             "match (resolve_seq x last) with | None -> resolve_failed x memo | Some (hd, tl) -> Dynarray.remove_last \
              x.e;"
        ^^ break 1
        ^^ string " (match Word.get_value hd with "
        ^^ (let s = pop_s s in
            separate_map (break 1)
              (fun (pat, expr) ->
                (* special casing for now, as pat design need changes. *)
                match pat with
                | PApp (cname, None) ->
                    string "| "
                    ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                    ^^ string " -> "
                    ^^ string ("(x.c <- pc_to_exp " ^ string_of_int (ant_pp_expr ctx s expr k) ^ "; x)")
                | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                    string "| "
                    ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                    ^^ string " -> "
                    ^^ string
                         "(let [x0; x1] = Memo.splits tl in push_env x (value_at_depth x0 x.d); push_env x \
                          (value_at_depth x1 x.d);"
                    ^^ string
                         ("x.c <- pc_to_exp "
                         ^ string_of_int
                             (ant_pp_expr ctx
                                (extend_s (extend_s s x0) x1)
                                expr
                                { k = (fun s -> drop s [ x1; x0 ] k); fv = k.fv })
                         ^ ";")
                    ^^ string "stepped x)"
                | _ -> failwith (show_pattern pat))
              c)
        ^^ string "))",
        pc ))

let ant_pp_stmt (ctx : ctx) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) -> (* TODO *) ant_pp_adt ctx name ctors
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (x, Lam (ps, term)) ->
      let s =
        List.fold_left
          (fun s p -> match p with PVar n -> extend_s s n | _ -> failwith (show_pattern p))
          (new_scope ()) ps
      in
      let arg_num = s.env_length in
      let name = match x with Some (PVar x) -> x | _ -> failwith "bad match" in
      add_code_k (fun entry_code ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_code;
          let term_code = ant_pp_expr ctx s term { k = return; fv = empty_fv () } in
          ( string ("(fun x -> x.c <- pc_to_exp " ^ string_of_int term_code ^ "; x)"),
            string "let rec" ^^ space ^^ string name ^^ space
            ^^ separate space (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
            ^^ string ": Value.seq " ^^ string "=" ^^ space ^^ group @@ string "exec_cek "
            ^^ string ("(pc_to_exp " ^ string_of_int term_code ^ ")")
            ^^ string "(Dynarray.of_list" ^^ string "["
            ^^ separate (string ";") (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ ")")))
            ^^ string "]" ^^ string ")" ^^ string "(Memo.from_constructor "
            ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag "cont_done"))
            ^^ string ")" ^^ string " memo" ))
  | Fun (_name, _args, _body) -> failwith "Not implemented (TODO)"
  | _ -> failwith (show_stmt s)

let generate_apply_cont ctx =
  set_code apply_cont
    (string
       "(fun x -> assert_env_length x 1; match resolve_seq x x.k.seq with | None -> resolve_failed x memo | Some (hd, \
        tl) -> match Word.get_value hd with "
    ^^ separate_map (break 1)
         (fun (name, action) ->
           string ("| " ^ string_of_int (Hashtbl.find_exn ctx.ctag name) ^ " -> ") ^^ action ^^ string " x tl")
         (Dynarray.to_list ctx.conts)
    ^^ string ")")

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (ant_pp_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1
  ^^ string "let memo = Array.init "
  ^^ string (string_of_int (Dynarray.length codes))
  ^^ string "(fun _ -> ref State.Root)" ^^ break 1 ^^ generated_stmt ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string ("let ()" ^ " = add_exp ")
            ^^ Option.get (Dynarray.get codes i)
            ^^ string " "
            ^^ string (string_of_int i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string
              ("let () = Value.set_constructor_degree " ^ string_of_int i ^ " " ^ "("
              ^ string_of_int (Dynarray.get ctx.constructor_degree i)
              ^ ")")))
