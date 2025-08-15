open Common
open PPrint
open Syntax
open Memo
open State
open Word

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
  conts : (string * (state -> words -> update -> state) code) Dynarray.t;
  func_pc : (string, int) Hashtbl.t;
}

let add_cont (ctx : ctx) (name : string) (arity : int) (app : (state -> words -> update -> state) code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app)

let fresh_name : (string, int) Hashtbl.t = Hashtbl.create (module Core.String)

let gensym (base : string) : string =
  let n = Option.value (Hashtbl.find fresh_name base) ~default:0 in
  Hashtbl.set fresh_name ~key:base ~data:(n + 1);
  base ^ "_" ^ string_of_int n

let int (i : int) : int code = string (string_of_int i)

let lam (a : string) (f : 'a code -> 'b code) : ('a -> 'b) code =
  let a = gensym a |> string in
  string "(fun " ^^ a ^^ string " -> " ^^ f a ^^ string ")"

let lam2 (a : string) (b : string) (f : 'a code -> 'b code -> 'c code) : ('a -> 'b -> 'c) code =
  let a = gensym a |> string in
  let b = gensym b |> string in
  string "(fun " ^^ a ^^ string " " ^^ b ^^ string " -> " ^^ f a b ^^ string ")"

let lam3 (a : string) (b : string) (c : string) (f : 'a code -> 'b code -> 'c code -> 'd code) :
    ('a -> 'b -> 'c -> 'd) code =
  let a = gensym a |> string in
  let b = gensym b |> string in
  let c = gensym c |> string in
  string "(fun " ^^ a ^^ string " " ^^ b ^^ string " " ^^ c ^^ string " -> " ^^ f a b c ^^ string ")"

let lam4 (a : string) (b : string) (c : string) (d : string) (f : 'a code -> 'b code -> 'c code -> 'd code -> 'e code) :
    ('a -> 'b -> 'c -> 'd -> 'e) code =
  let a = gensym a |> string in
  let b = gensym b |> string in
  let c = gensym c |> string in
  let d = gensym d |> string in
  string "(fun " ^^ a ^^ string " " ^^ b ^^ string " " ^^ c ^^ string " " ^^ d ^^ string " -> " ^^ f a b c d
  ^^ string ")"

let app (f : ('a -> 'b) code) (a : 'a code) : 'b code = group (f ^^ space ^^ a)
let app2 (f : ('a -> 'b -> 'c) code) (a : 'a code) (b : 'b code) : 'c code = app (app f a) b
let app3 (f : ('a -> 'b -> 'c -> 'd) code) (a : 'a code) (b : 'b code) (c : 'c code) : 'd code = app (app2 f a b) c

let app4 (f : ('a -> 'b -> 'c -> 'd -> 'e) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code) : 'e code =
  app (app3 f a b c) d

let app5 (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code)
    (e : 'e code) : 'f code =
  app (app4 f a b c d) e

let assert_env_length (s : state code) (e : int code) : unit code = app2 (string "assert_env_length") s e

let return_n (s : state code) (n : int code) (return_exp : exp code) (store : store code) (update : update code) :
    state code =
  app5 (string "return_n") s n return_exp store update

let drop_n (s : state code) (e : int code) (n : int code) (return_exp : exp code) : state code =
  app4 (string "drop_n") s e n return_exp

let push_env (s : state code) (v : int code) : unit code = app2 (string "push_env") s v
let pc_to_exp (pc : int code) : exp code = app (string "pc_to_exp") pc
let seq (x : unit code) (y : unit -> unit code) : unit code = group (x ^^ string "; " ^^ y ())
let seq_b1 (x : unit code) (y : unit -> unit code) : unit code = group (x ^^ string ";" ^^ break 1 ^^ y ())
let paren (x : 'a code) : 'a code = string "(" ^^ x ^^ string ")"
let dyn_array_get (arr : 'a Dynarray.t code) (i : int code) : 'a code = app2 (string "Dynarray.get") arr i
let dyn_array_remove_last (arr : 'a Dynarray.t code) : unit code = app (string "Dynarray.remove_last") arr

let state_env (s : state code) : env code =
  (* paren (app (string "state.e") s) *)
  paren s ^^ string ".e"

let state_kont (s : state code) : kont code =
  (* paren (app (string "state.k") s) *)
  paren s ^^ string ".k"

let stepped (s : state code) : state code = app (string "stepped") s
let set_c (s : state code) (c : exp code) : unit code = paren s ^^ string ".c <- " ^^ c
let set_k (s : state code) (k : kont code) : unit code = paren s ^^ string ".k <- " ^^ k
let from_constructor (ctag : int code) : Value.seq code = app (string "Memo.from_constructor") ctag
let pop_env (s : state code) : Value.value code = app (string "pop_env") s

let env_call (s : state code) (keep : int list code) (nargs : int code) : Value.seq code =
  app3 (string "env_call") s keep nargs

let restore_env (s : state code) (n : int code) (seqs : Value.seq code) : unit code =
  app3 (string "restore_env") s n seqs

let get_next_cont (seqs : Value.seq code) : Value.seq code = app (string "get_next_cont") seqs

let resolve (s : state code) (store : store code) (src : Reference.source code) (update : update code) :
    (Word.t code * Value.seq code) option code =
  app4 (string "resolve") s store src update

let memo_appends (xs : Value.seq code list) : Value.seq code =
  app (string "Memo.appends") (string "[" ^^ separate (string ";") xs ^^ string "]")

let memo_from_int (i : int code) : Value.seq code = app (string "Memo.from_int") i
let memo_splits (seq : Value.seq code) : Value.seq list code = app (string "Memo.splits") seq

let let_in (a : string) (value : 'a code) (body : 'a code -> 'b code) : 'b code =
  let a = gensym a |> string in
  string "let " ^^ a ^^ string " = " ^^ value ^^ string " in " ^^ body a

let list_literal (xs : 'a code list) : 'a list code = string "[" ^^ separate (string ";") xs ^^ string "]"
let list_literal_of (f : 'a -> 'b code) (xs : 'a list) : 'b list code = list_literal (List.map f xs)

let match_option (x : 'a option code) (none : unit -> 'b code) (pat : 'a code) (some : unit -> 'b code) : 'b code =
  string "match " ^^ x ^^ string " with | None -> " ^^ none () ^^ string " | Some " ^^ pat ^^ string " -> " ^^ some ()

let src_E (i : int) : Reference.source code = string "Source.E " ^^ string (string_of_int i)

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
  add_cont ctx "cont_done" 0 (string "(fun x tl store upate -> exec_done x update)");
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
      ( lam3 "x" "store" "update" (fun x store update ->
            seq
              (assert_env_length x (int s.env_length))
              (fun _ -> drop_n x (int s.env_length) (int n) (paren (pc_to_exp (int (k.k new_s)))))),
        pc ))

let return (s : scope) : pc =
  add_code
    (Some
       (lam3 "x" "store" "update" (fun x store update ->
            seq
              (assert_env_length x (int s.env_length))
              (fun _ -> return_n x (int s.env_length) (paren (pc_to_exp (int apply_cont))) store update))))

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
          ( lam3 "x" "store" "update" (fun x store update ->
                seq
                  (assert_env_length x (int s.env_length))
                  (fun _ ->
                    seq
                      (push_env x (paren (dyn_array_get (state_env x) (int loc))))
                      (fun _ -> seq (set_c x (pc_to_exp (int (k.k (push_s s))))) (fun _ -> stepped x)))),
            pc ))
  | Match (value, cases) ->
      ant_pp_expr ctx s value { k = (fun s -> ant_pp_cases ctx (dup_s s) cases k); fv = fv_cases cases (dup_fv k.fv) }
  | Ctor cname ->
      add_code_k (fun pc ->
          ( lam3 "x" "store" "update" (fun x store update ->
                seq
                  (assert_env_length x (int s.env_length))
                  (fun _ ->
                    seq
                      (push_env x (paren (from_constructor (int (Hashtbl.find_exn ctx.ctag cname)))))
                      (fun _ -> seq (set_c x (pc_to_exp (int (k.k (push_s s))))) (fun _ -> stepped x)))),
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
                          ( lam3 "x" "store" "update" (fun x store update ->
                                seq
                                  (assert_env_length x (int s.env_length))
                                  (fun _ ->
                                    let_in "x1" (pop_env x) (fun x1 ->
                                        let_in "x0" (pop_env x) (fun x0 ->
                                            seq
                                              (push_env x
                                                 (paren
                                                    (memo_appends
                                                       [
                                                         from_constructor (int (Hashtbl.find_exn ctx.ctag cname));
                                                         x0;
                                                         x1;
                                                       ])))
                                              (fun _ ->
                                                seq
                                                  (set_c x (pc_to_exp (int (k.k (push_s (pop_s (pop_s s)))))))
                                                  (fun _ -> stepped x)))))),
                            pc )));
                  fv = k.fv;
                });
          fv = fv_expr x1 (dup_fv k.fv);
        }
  | App (Var "list_incr", [ x ]) ->
      let cont_name = "cont_" ^ string_of_int (Dynarray.length ctx.conts) in
      let keep, keep_s = keep_only s k.fv in
      (* subtracting 1 to remove the arguments; adding 1 for the next continuation*)
      let keep_length = keep_s.env_length in
      add_cont ctx cont_name (keep_length + 1)
        (lam4 "x" "tl" "store" "update" (fun x tl store update ->
             seq
               (assert_env_length x (int keep_length))
               (fun _ ->
                 seq
                   (restore_env x (int keep_length) tl)
                   (fun _ ->
                     seq
                       (set_k x (get_next_cont tl))
                       (fun _ -> seq (set_c x (pc_to_exp (int (k.k (push_s keep_s))))) (fun _ -> stepped x))))));
      ant_pp_expr ctx s x
        {
          k =
            (fun s ->
              add_code_k (fun pc ->
                  ( lam3 "x" "store" "update" (fun x store update ->
                        seq
                          (assert_env_length x (int s.env_length))
                          (fun _ ->
                            let_in "keep"
                              (env_call x (paren (list_literal_of int (Dynarray.to_list keep))) (int 1))
                              (fun keep ->
                                seq
                                  (set_k x
                                     (memo_appends
                                        [
                                          from_constructor (int (Hashtbl.find_exn ctx.ctag cont_name));
                                          keep;
                                          state_kont x;
                                        ]))
                                  (fun _ ->
                                    seq
                                      (set_c x (pc_to_exp (int (Hashtbl.find_exn ctx.func_pc "list_incr"))))
                                      (fun _ -> stepped x))))),
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
                      add_code_k (fun pc ->
                          ( lam3 "x" "store" "update" (fun x store update ->
                                seq
                                  (assert_env_length x (int s.env_length))
                                  (fun _ ->
                                    let x0 = resolve x store (paren (src_E (s.env_length - 2))) update in
                                    let x1 = resolve x store (paren (src_E (s.env_length - 1))) update in
                                    let x0_s = gensym "x0" |> string in
                                    let x1_s = gensym "x1" |> string in
                                    match_option x0
                                      (fun _ -> x)
                                      (paren (x0_s ^^ string ", _"))
                                      (fun _ ->
                                        match_option x1
                                          (fun _ -> x)
                                          (paren (x1_s ^^ string ", _"))
                                          (fun _ ->
                                            seq
                                              (dyn_array_remove_last (state_env x))
                                              (fun _ ->
                                                seq
                                                  (dyn_array_remove_last (state_env x))
                                                  (fun _ ->
                                                    seq
                                                      (push_env x
                                                         (paren (memo_from_int (paren (x0_s ^^ string " + " ^^ x1_s)))))
                                                      (fun _ ->
                                                        seq
                                                          (set_c x (pc_to_exp (int (k.k (push_s (pop_s (pop_s s)))))))
                                                          (fun _ -> stepped x)))))))),
                            pc )));
                  fv = fv_expr x1 (dup_fv k.fv);
                });
          fv = k.fv;
        }
  | Int i ->
      add_code_k (fun pc ->
          ( lam3 "x" "store" "update" (fun x store update ->
                seq
                  (assert_env_length x (int s.env_length))
                  (fun _ ->
                    seq
                      (push_env x (paren (memo_from_int (int i))))
                      (fun _ -> seq (set_c x (pc_to_exp (int (k.k (push_s s))))) (fun _ -> stepped x)))),
            pc ))
  | _ -> failwith ("ant_pp_expr: " ^ show_expr c)

and ant_pp_cases (ctx : ctx) (s : scope) (MatchPattern c : cases) (k : kont) : pc =
  add_code_k (fun pc ->
      ( lam3 "x" "store" "update" (fun x store update ->
            seq
              (assert_env_length x (int s.env_length))
              (fun _ ->
                let_in "last"
                  (src_E (s.env_length - 1))
                  (fun last ->
                    let m = resolve x store last update in
                    let hd = gensym "hd" |> string in
                    let tl = gensym "tl" |> string in
                    match_option m
                      (fun _ -> x)
                      (paren (hd ^^ string ", " ^^ tl))
                      (fun _ ->
                        seq_b1
                          (dyn_array_remove_last (state_env x))
                          (fun _ ->
                            let s = pop_s s in
                            let t =
                              separate_map (break 1)
                                (fun (pat, expr) ->
                                  (* special casing for now, as pat design need changes. *)
                                  match pat with
                                  | PApp (cname, None) ->
                                      string "| "
                                      ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                                      ^^ string " -> "
                                      ^^ paren
                                           (seq
                                              (set_c x (pc_to_exp (int (ant_pp_expr ctx s expr k))))
                                              (fun _ -> stepped x))
                                  | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                                      string "| "
                                      ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                                      ^^ string " -> "
                                      ^^ paren
                                           (let x0_s = gensym "x0" |> string in
                                            let x1_s = gensym "x1" |> string in
                                            seq
                                              (string "let [" ^^ x0_s ^^ string "; " ^^ x1_s ^^ string "] ="
                                             ^^ memo_splits tl ^^ string " in " ^^ push_env x x0_s)
                                              (fun _ ->
                                                seq (push_env x x1_s) (fun _ ->
                                                    seq
                                                      (set_c x
                                                         (paren
                                                            (pc_to_exp
                                                               (int
                                                                  (ant_pp_expr ctx
                                                                     (extend_s (extend_s s x0) x1)
                                                                     expr
                                                                     { k = (fun s -> drop s [ x1; x0 ] k); fv = k.fv })))))
                                                      (fun _ -> stepped x))))
                                  | _ -> failwith (show_pattern pat))
                                c
                            in
                            string " (match Word.get_value " ^^ hd ^^ string " with " ^^ t ^^ string ")"))))),
        pc ))

let ant_pp_stmt (ctx : ctx) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) -> ant_pp_adt ctx name ctors
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
          ( string ("(fun x store update -> x.c <- pc_to_exp " ^ string_of_int term_code ^ "; stepped x)"),
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
       "(fun x store update -> assert_env_length x 1; match resolve x store K update with | None -> x | Some (hd, tl) \
        -> match Word.get_value hd with "
    ^^ separate_map (break 1)
         (fun (name, action) ->
           string ("| " ^ string_of_int (Hashtbl.find_exn ctx.ctag name) ^ " -> ")
           ^^ action ^^ string " x tl store update")
         (Dynarray.to_list ctx.conts)
    ^^ string ")")

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (ant_pp_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Value"
  ^^ break 1 ^^ string "let memo = Array.init "
  ^^ string (string_of_int (Dynarray.length codes))
  ^^ string "(fun _ -> ref State.BlackHole)" ^^ break 1 ^^ generated_stmt ^^ break 1
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
