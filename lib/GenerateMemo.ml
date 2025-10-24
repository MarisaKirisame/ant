open Common
open PPrint
open Syntax
open Memo
open State
open Word

(*todo: implement tail call*)
(*todo: do not do a stack machine*)

(* As K come with interpretative overhead,
 *   we want to use K as little as possible,
 *   instead storing the computed temporary variables onto the env as a stack,
 *   only using K whenever we do a non-tail function call.
 *)
type ir = Raw of document | Seqs of ir list | Unit | Function of string

let rec ir_to_doc ir =
  match ir with
  | Raw doc -> doc
  | Seqs [] -> string "()"
  | Seqs [ x ] -> ir_to_doc x
  | Seqs (x :: xs) -> parens (List.fold_left (fun acc x -> acc ^^ string ";" ^^ ir_to_doc x) (ir_to_doc x) xs)
  | Unit -> string "()"
  | Function str -> string str

type 'a code = Code of ir

let show_ir ir =
  let doc = ir_to_doc ir in
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.8 80 buf doc;
  Buffer.contents buf

let rec optimize_ir ir =
  match ir with
  | Raw d -> Raw d
  | Seqs xs ->
      let xs = List.map optimize_ir xs in
      Seqs (List.flatten (List.map (fun ir -> match ir with Unit -> [] | Seqs xs -> xs | x -> [ x ]) xs))
  | Function f -> Function f
  | Unit -> Unit

let code (doc : document) = Code (Raw doc)

let uncode (Code ir) : document =
  (*print_endline (show_ir ir);*)
  ir_to_doc (optimize_ir ir)

let from_ir (Code ir) = ir
let to_ir ir = Code ir

type ctx = {
  arity : (string, int) Hashtbl.t;
  ctag : (string, int) Hashtbl.t;
  constructor_degree : int Dynarray.t;
  conts : (string * (world code -> words code -> unit code)) Dynarray.t;
  mutable conts_count : int;
  func_pc : (string, int) Hashtbl.t;
}

let add_cont (ctx : ctx) (name : string) (arity : int) (app : world code -> words code -> unit code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app);
  ctx.conts_count <- ctx.conts_count + 1

let fresh_name : (string, int) Hashtbl.t = Hashtbl.create (module Core.String)

let gensym (base : string) : string =
  let n = Option.value (Hashtbl.find fresh_name base) ~default:0 in
  Hashtbl.set fresh_name ~key:base ~data:(n + 1);
  base ^ "_" ^ string_of_int n

let int (i : int) : int code = code $ string (string_of_int i)
let unit : unit code = Code Unit

let lam (a : string) (f : 'a code -> 'b code) : ('a -> 'b) code =
  let a = gensym a |> string in
  code $ string "(fun " ^^ a ^^ string " -> " ^^ uncode (f (code a)) ^^ string ")"

let lam2 (a : string) (b : string) (f : 'a code -> 'b code -> 'c code) : ('a -> 'b -> 'c) code =
  let a = gensym a |> string in
  let b = gensym b |> string in
  code $ string "(fun " ^^ a ^^ string " " ^^ b ^^ string " -> " ^^ uncode (f (code a) (code b)) ^^ string ")"

let lam3 (a : string) (b : string) (c : string) (f : 'a code -> 'b code -> 'c code -> 'd code) :
    ('a -> 'b -> 'c -> 'd) code =
  let a = gensym a |> string in
  let b = gensym b |> string in
  let c = gensym c |> string in
  code
  $ string "(fun " ^^ a ^^ string " " ^^ b ^^ string " " ^^ c ^^ string " -> "
    ^^ uncode (f (code a) (code b) (code c))
    ^^ string ")"

let lam4 (a : string) (b : string) (c : string) (d : string) (f : 'a code -> 'b code -> 'c code -> 'd code -> 'e code) :
    ('a -> 'b -> 'c -> 'd -> 'e) code =
  let a = gensym a |> string in
  let b = gensym b |> string in
  let c = gensym c |> string in
  let d = gensym d |> string in
  code
  $ string "(fun " ^^ a ^^ string " " ^^ b ^^ string " " ^^ c ^^ string " " ^^ d ^^ string " -> "
    ^^ uncode (f (code a) (code b) (code c) (code d))
    ^^ string ")"

let app (f : ('a -> 'b) code) (a : 'a code) : 'b code = code $ parens (group (uncode f ^^ space ^^ uncode a))
let app2 (f : ('a -> 'b -> 'c) code) (a : 'a code) (b : 'b code) : 'c code = app (app f a) b
let app3 (f : ('a -> 'b -> 'c -> 'd) code) (a : 'a code) (b : 'b code) (c : 'c code) : 'd code = app (app2 f a b) c

let app4 (f : ('a -> 'b -> 'c -> 'd -> 'e) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code) : 'e code =
  app (app3 f a b c) d

let app5 (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) code) (a : 'a code) (b : 'b code) (c : 'c code) (d : 'd code)
    (e : 'e code) : 'f code =
  app (app4 f a b c d) e

let assert_env_length (w : world code) (e : int code) : unit code = app2 (code $ string "assert_env_length") w e
let return_n (w : world code) (n : int code) (exp : exp code) : unit code = app3 (code $ string "return_n") w n exp
let drop_n (w : world code) (e : int code) (n : int code) : unit code = app3 (code $ string "drop_n") w e n
let pc_to_exp (pc : int code) : exp code = app (to_ir $ Function "pc_to_exp") pc
let seq (x : unit code) (y : unit -> 'a code) : 'a code = to_ir (Seqs [ from_ir x; from_ir (y ()) ])
let seqs (xs : (unit -> unit code) list) : unit code = List.fold_left seq unit xs

let seq_b1 (x : unit code) (y : unit -> unit code) : unit code =
  code $ parens (group (uncode x ^^ string ";" ^^ break 1 ^^ uncode (y ())))

let zro (x : ('a * 'b) code) : 'a code = app (code $ string "fst") x
let fst (x : ('a * 'b) code) : 'b code = app (code $ string "snd") x
let add (x : int code) (y : int code) : int code = code $ parens (uncode x ^^ string " + " ^^ uncode y)
let dyn_array_get (arr : 'a Dynarray.t code) (i : int code) : 'a code = app2 (code $ string "Dynarray.get") arr i
let dyn_array_remove_last (arr : 'a Dynarray.t code) : unit code = app (code $ string "Dynarray.remove_last") arr
let world_state (w : world code) : state code = code $ parens (uncode w ^^ string ".state")
let state_env (s : state code) : env code = code $ parens (uncode s ^^ string ".e")
let world_env (w : world code) : env code = state_env $ world_state w
let state_kont (s : state code) : kont code = code $ parens (uncode s ^^ string ".k")
let world_kont (w : world code) : kont code = state_kont $ world_state w
let stepped (w : world code) : unit code = app (code $ string "stepped") w

let set_c (w : world code) (c : exp code) : unit code =
  code $ parens (uncode (world_state w) ^^ string ".c <- " ^^ uncode c)

let set_k (w : world code) (k : kont code) : unit code =
  code $ parens (uncode (world_state w) ^^ string ".k <- " ^^ uncode k)

let from_constructor (ctag : int code) : Value.seq code = app (code $ string "Memo.from_constructor") ctag
let to_unit (x : 'a code) : unit code = app (code $ string "ignore") x
let pop_env (w : world code) : Value.value code = app (code $ string "pop_env") w
let goto (w : world code) pc : unit code = seq (set_c w (pc_to_exp (int pc))) (fun _ -> stepped w)
let push_env (w : world code) (v : words code) : unit code = app2 (code $ string "push_env") w v
let get_env (w : world code) (i : int code) : words code = dyn_array_get (state_env $ world_state w) i
let exec_done (w : world code) : unit code = app (code $ string "exec_done") w

let env_call (w : world code) (keep : int list code) (nargs : int code) : Value.seq code =
  app3 (code $ string "env_call") w keep nargs

let restore_env (w : world code) (n : int code) (seqs : Value.seq code) : unit code =
  app3 (code $ string "restore_env") w n seqs

let get_next_cont (seqs : Value.seq code) : Value.seq code = app (code $ string "get_next_cont") seqs

let resolve (w : world code) (src : Reference.source code) : (Word.t * Value.seq) option code =
  app2 (code $ string "resolve") w src

let memo_appends (xs : Value.seq code list) : Value.seq code =
  app (code $ string "Memo.appends") (code (string "[" ^^ separate (string ";") (List.map uncode xs) ^^ string "]"))

let memo_from_int (i : int code) : Value.seq code = app (code $ string "Memo.from_int") i
let int_from_word (w : Word.t code) : int code = app (code $ string "Word.to_int") w
let memo_splits (seq : Value.seq code) : Value.seq list code = app (code $ string "Memo.splits") seq

let let_in (a : string) (value : 'a code) (body : 'a code -> 'b code) : 'b code =
  let a = gensym a |> string in
  code $ string "let " ^^ a ^^ string " = " ^^ uncode value ^^ string " in " ^^ uncode (body (code a))

let list_literal (xs : 'a code list) : 'a list code =
  code $ string "[" ^^ separate (string ";") (List.map uncode xs) ^^ string "]"

let list_literal_of (f : 'a -> 'b code) (xs : 'a list) : 'b list code = list_literal (List.map f xs)

let match_option (x : 'a option code) (none : unit -> 'b code) (a : string) (some : 'a code -> 'b code) : 'b code =
  let a = gensym a |> string in
  code
  $ string "match " ^^ uncode x ^^ string " with | None -> "
    ^^ uncode (none ())
    ^^ string " | Some " ^^ a ^^ string " -> "
    ^^ uncode (some (code a))

let src_E (i : int) : Reference.source code = code $ parens (string "Source.E " ^^ string (string_of_int i))

let new_ctx () : ctx =
  let ctx =
    {
      arity = Hashtbl.create (module Core.String);
      ctag = Hashtbl.create (module Core.String);
      constructor_degree = Dynarray.create ();
      conts = Dynarray.create ();
      conts_count = 0;
      func_pc = Hashtbl.create (module Core.String);
    }
  in
  add_cont ctx "cont_done" 0 (fun w _ -> exec_done w);
  ctx

let codes : (world -> unit) code option Dynarray.t = Dynarray.create ()

type pc = int

let add_code (c : (world -> unit) code option) : pc =
  let pc = Dynarray.length codes in
  Dynarray.add_last codes c;
  pc

let set_code (i : int) (c : (world -> unit) code) : unit = Dynarray.set codes i (Some c)

let add_code_k (k : pc -> (world -> unit) code * 'a) : 'a =
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
             else con_name ^ " of " ^ String.concat " * " (List.map (fun _ -> "Value.seq") types))
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
              :: List.mapi (fun i _ -> "x" ^ string_of_int i) types)
          ^ "]")
      in
      register_constructor)
    ctors

(*todo: distinguish ffi inner type.*)
let ant_pp_adt_ffi e adt_name ctors =
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
                  ^ String.concat "," (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
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
  progressed : bool;
}

let new_scope () = { meta_env = make_linear (Hashtbl.create (module Core.String)); env_length = 0; progressed = false }
let push_s s = { s with env_length = s.env_length + 1; progressed = true }

let extend_s s name =
  print_endline ("extending: " ^ name);

  let meta_env = write_linear s.meta_env in

  Hashtbl.add_exn meta_env ~key:name ~data:(Some s.env_length);

  { s with meta_env = make_linear meta_env; env_length = s.env_length + 1 }

let drop_s s name =
  assert (Option.is_some (Hashtbl.find_exn (read_linear s.meta_env) name));
  let meta_env = write_linear s.meta_env in
  Hashtbl.remove meta_env name;
  { s with meta_env = make_linear meta_env; env_length = s.env_length - 1 }

let pop_n s n =
  assert (s.env_length >= n);
  { s with meta_env = s.meta_env; env_length = s.env_length - n }

let pop_s s = pop_n s 1

(*todo: we actually need to dup a bunch. lets switch to a functional data structure. *)
let dup_s s = { s with meta_env = make_linear (Hashtbl.copy (read_linear s.meta_env)); env_length = s.env_length }

type kont = { k : scope -> world code -> unit code; fv : (string, unit) Hashtbl.t linear }

let dup_fv (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  make_linear (Hashtbl.copy (read_linear fv))

let empty_fv () : (string, unit) Hashtbl.t linear = make_linear (Hashtbl.create (module Core.String))

let drop (s : scope) (vars : string list) (w : world code) (k : kont) : unit code =
  Hashtbl.iter_keys (read_linear s.meta_env) ~f:(fun x -> print_endline ("dropping has:" ^ x));
  let new_s, n =
    List.fold_left
      (fun (s, n) var ->
        print_endline ("dropping: " ^ var);
        match Hashtbl.find_exn (read_linear s.meta_env) var with None -> (s, n) | Some _ -> (drop_s s var, n + 1))
      (s, 0) vars
  in
  seqs
    [
      (fun _ -> assert_env_length w (int s.env_length));
      (fun _ -> drop_n w (int s.env_length) (int n));
      (fun _ -> k.k new_s w);
    ]

let return (s : scope) (w : world code) : unit code =
  seq (assert_env_length w (int s.env_length)) (fun _ -> return_n w (int s.env_length) (pc_to_exp (int apply_cont)))

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
  | Ctor _ | Int _ | GVar _ -> fv
  | App (f, xs) -> fv_exprs xs (fv_expr f fv)
  | Op (_, x, y) -> fv_expr y (fv_expr x fv)
  | Var name -> add_fv name fv
  | Match (value, cases) -> fv_expr value (fv_cases cases fv)
  | If (i, t, e) -> fv_expr i (fv_expr t (fv_expr e fv))
  | Let (BOne (l, v), r) -> fv_expr v (fv_pat l (fv_expr r fv))
  | _ -> failwith ("fv_expr: " ^ show_expr e)

and fv_exprs (es : expr list) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  List.fold_left (fun fv e -> fv_expr e fv) fv es

and fv_pat (pat : pattern) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
  match pat with
  | PApp (_, None) -> fv
  | PApp (_, Some x) -> fv_pat x fv
  | PTup xs -> List.fold_left (fun fv x -> fv_pat x fv) fv xs
  | PVar name -> remove_fv name fv
  | _ -> failwith (show_pattern pat)

and fv_cases (MatchPattern c : cases) (fv : (string, unit) Hashtbl.t linear) : (string, unit) Hashtbl.t linear =
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
  Hashtbl.iteri (read_linear s.meta_env) ~f:(fun ~key ~data:_ -> ignore (Hashtbl.add meta_env ~key ~data:None));
  (keep_idx, { s with meta_env = make_linear meta_env; env_length = Dynarray.length keep_idx })

let reading (s : scope) (f : scope -> world code -> unit code) (w : world code) : unit code =
  let make_code w = f { s with progressed = false } w in
  if s.progressed then goto w (add_code $ Some (lam "w" make_code)) else make_code w

let rec ant_pp_expr (ctx : ctx) (s : scope) (c : expr) (k : kont) : world code -> unit code =
  match c with
  | Var name ->
      let loc = Option.get (Hashtbl.find_exn (read_linear s.meta_env) name) in
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (get_env w (int loc)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Match (value, cases) ->
      ant_pp_expr ctx s value
        {
          k = (fun s -> reading s $ fun s w -> ant_pp_cases ctx (dup_s s) cases k w);
          fv = fv_cases cases (dup_fv k.fv);
        }
  | Ctor cname ->
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (from_constructor (int (Hashtbl.find_exn ctx.ctag cname))));
            (fun _ -> k.k (push_s s) w);
          ]
  | App (Ctor cname, [ x0 ]) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              seqs
                [
                  (fun _ -> assert_env_length w (int s.env_length));
                  (fun _ ->
                    let_in "x0" (pop_env w) (fun x0 ->
                        push_env w (memo_appends [ from_constructor (int (Hashtbl.find_exn ctx.ctag cname)); x0 ])));
                  (fun _ -> k.k (push_s (pop_s s)) w);
                ]);
          fv = k.fv;
        }
  | App (Ctor cname, [ x0; x1 ]) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              ant_pp_expr ctx s x1
                {
                  k =
                    (fun s w ->
                      seqs
                        [
                          (fun _ -> assert_env_length w (int s.env_length));
                          (fun _ ->
                            let_in "x1" (pop_env w) (fun x1 ->
                                let_in "x0" (pop_env w) (fun x0 ->
                                    push_env w
                                      (memo_appends
                                         [ from_constructor (int (Hashtbl.find_exn ctx.ctag cname)); x0; x1 ]))));
                          (fun _ -> k.k (push_s (pop_s (pop_s s))) w);
                        ]);
                  fv = k.fv;
                }
                w);
          fv = fv_expr x1 (dup_fv k.fv);
        }
  | App (GVar f, xs) ->
      let cont_name = "cont_" ^ string_of_int ctx.conts_count in
      let keep, keep_s = keep_only s k.fv in
      (* subtracting 1 to remove the arguments; adding 1 for the next continuation*)
      let keep_length = keep_s.env_length in
      add_cont ctx cont_name (keep_length + 1) (fun w tl ->
          seqs
            [
              (fun _ -> set_k w (get_next_cont tl));
              (fun _ -> restore_env w (int keep_length) tl);
              (fun _ -> k.k (push_s keep_s) w);
            ]);
      let xs_length = List.length xs in
      ant_pp_exprs ctx s xs
        {
          k =
            (fun s w ->
              seqs
                [
                  (fun _ -> assert_env_length w (int s.env_length));
                  (fun _ ->
                    let_in "keep"
                      (env_call w (list_literal_of int (Dynarray.to_list keep)) (int xs_length))
                      (fun keep ->
                        set_k w
                          (memo_appends
                             [ from_constructor (int (Hashtbl.find_exn ctx.ctag cont_name)); keep; world_kont w ])));
                  (fun _ -> goto w (Hashtbl.find_exn ctx.func_pc f));
                ]);
          fv = k.fv;
        }
  | Op ("+", x0, x1) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              ant_pp_expr ctx s x1
                {
                  k =
                    (fun s ->
                      reading s $ fun s w ->
                      seqs
                        [
                          (fun _ -> assert_env_length w (int s.env_length));
                          (fun _ ->
                            match_option
                              (resolve w (src_E (s.env_length - 2)))
                              (fun _ -> unit)
                              "x0"
                              (fun x0 ->
                                match_option
                                  (resolve w (src_E (s.env_length - 1)))
                                  (fun _ -> unit)
                                  "x1"
                                  (fun x1 ->
                                    seqs
                                      [
                                        (fun _ -> to_unit $ pop_env w);
                                        (fun _ -> to_unit $ pop_env w);
                                        (fun _ ->
                                          push_env w
                                            (memo_from_int (add (int_from_word (zro x0)) (int_from_word (zro x1)))));
                                        (fun _ -> k.k (push_s (pop_s (pop_s s))) w);
                                      ])));
                        ]);
                  fv = fv_expr x1 (dup_fv k.fv);
                }
                w);
          fv = k.fv;
        }
  | Int i ->
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (memo_from_int (int i)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Let (BOne (PVar l, v), r) ->
      ant_pp_expr ctx s v
        {
          k =
            (fun s w ->
              ant_pp_expr ctx
                (extend_s (pop_s s) l)
                r
                { k = (fun s w -> drop s [ l ] w k); fv = add_fv l (dup_fv k.fv) }
                w);
          fv = fv_pat (PVar l) (fv_expr r (dup_fv k.fv));
        }
  | _ -> failwith ("ant_pp_expr: " ^ show_expr c)

and ant_pp_exprs (ctx : ctx) (s : scope) (cs : expr list) (k : kont) : world code -> unit code =
  match cs with
  | [] -> fun w -> k.k s w
  | c :: cs -> ant_pp_expr ctx s c { k = (fun s w -> ant_pp_exprs ctx s cs k w); fv = fv_exprs cs (dup_fv k.fv) }

and ant_pp_cases (ctx : ctx) (s : scope) (MatchPattern c : cases) (k : kont) : world code -> unit code =
 fun w ->
  seq
    (assert_env_length w (int s.env_length))
    (fun _ ->
      let_in "last"
        (src_E (s.env_length - 1))
        (fun last ->
          let m = resolve w last in
          match_option m
            (fun _ -> unit)
            "x"
            (fun x ->
              seq_b1
                (to_unit $ pop_env w)
                (fun _ ->
                  let s = pop_s s in
                  let t =
                    separate_map (break 1)
                      (fun (pat, expr) ->
                        let s = dup_s s in
                        (*todo: special casing for now, as pat design need changes. *)
                        match pat with
                        | PApp (cname, None) ->
                            string "| "
                            ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^ (uncode $ ant_pp_expr ctx s expr k w)
                        | PApp (cname, Some (PVar x0)) ->
                            string "| "
                            ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^
                            let x0_s = gensym "x0" |> string in
                            uncode
                              (seq
                                 (code
                                    (string "let [" ^^ x0_s ^^ string "] ="
                                    ^^ uncode (memo_splits (fst x))
                                    ^^ string " in "
                                    ^^ uncode (push_env w (code x0_s))))
                                 (fun _ ->
                                   ant_pp_expr ctx (extend_s s x0) expr
                                     { k = (fun s w -> drop s [ x0 ] w k); fv = k.fv }
                                     w))
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                            string "| "
                            ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^
                            let x0_s = gensym "x0" |> string in
                            let x1_s = gensym "x1" |> string in
                            uncode
                              (seq
                                 (code
                                    (string "let [" ^^ x0_s ^^ string "; " ^^ x1_s ^^ string "] ="
                                    ^^ uncode (memo_splits (fst x))
                                    ^^ string " in "
                                    ^^ uncode (push_env w (code x0_s))))
                                 (fun _ ->
                                   seq
                                     (push_env w (code x1_s))
                                     (fun _ ->
                                       ant_pp_expr ctx
                                         (extend_s (extend_s s x0) x1)
                                         expr
                                         { k = (fun s w -> drop s [ x1; x0 ] w k); fv = k.fv }
                                         w)))
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1; PVar x2 ])) ->
                            string "| "
                            ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^
                            let x0_s = gensym "x0" |> string in
                            let x1_s = gensym "x1" |> string in
                            let x2_s = gensym "x2" |> string in
                            uncode
                              (seq
                                 (code
                                    (string "let [" ^^ x0_s ^^ string "; " ^^ x1_s ^^ string "; " ^^ x2_s
                                   ^^ string "] ="
                                    ^^ uncode (memo_splits (fst x))
                                    ^^ string " in "
                                    ^^ uncode (push_env w (code x0_s))))
                                 (fun _ ->
                                   seqs
                                     [
                                       (fun _ -> push_env w (code x1_s));
                                       (fun _ -> push_env w (code x2_s));
                                       (fun _ ->
                                         ant_pp_expr ctx
                                           (extend_s (extend_s (extend_s s x0) x1) x2)
                                           expr
                                           { k = (fun s w -> drop s [ x2; x1; x0 ] w k); fv = k.fv }
                                           w);
                                     ]))
                        | _ -> failwith (show_pattern pat))
                      c
                  in
                  code (string " (match Word.get_value " ^^ (uncode $ zro x) ^^ string " with " ^^ t ^^ string ")")))))

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
          ( lam "w" (fun w -> ant_pp_expr ctx s term { k = (fun s w -> return s w); fv = empty_fv () } w),
            string "let rec" ^^ space ^^ string name ^^ space
            ^^ separate space (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
            ^^ string ": Value.seq " ^^ string "=" ^^ space ^^ group @@ string "exec_cek "
            ^^ string ("(pc_to_exp " ^ string_of_int entry_code ^ ")")
            ^^ string "(Dynarray.of_list" ^^ string "["
            ^^ separate (string ";") (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ ")")))
            ^^ string "]" ^^ string ")" ^^ string "(Memo.from_constructor "
            ^^ string (string_of_int (Hashtbl.find_exn ctx.ctag "cont_done"))
            ^^ string ")" ^^ string " memo" ))
  | Fun (_name, _args, _body) -> failwith "Not implemented (TODO)"
  | _ -> failwith (show_stmt s)

let generate_apply_cont ctx =
  set_code apply_cont
    (lam "w" (fun w ->
         (* We have to be careful: ctx.conts will grow as we apply the lambdas, 
          *   so we cannot do a single map over the whole array, 
          *   instead we have to take elements out on by one.
          *)
         let cont_codes = Dynarray.create () in
         let rec loop () =
           match Dynarray.pop_last_opt ctx.conts with
           | None -> cont_codes
           | Some (name, action) ->
               let code =
                 string ("| " ^ string_of_int (Hashtbl.find_exn ctx.ctag name) ^ " -> ")
                 ^^ uncode (action w (code $ string "tl"))
               in
               Dynarray.add_last cont_codes code;
               loop ()
         in
         let rec loop i =
           if i == Dynarray.length ctx.conts then cont_codes
           else
             let name, action = Dynarray.get ctx.conts i in
             let code =
               string ("| " ^ string_of_int (Hashtbl.find_exn ctx.ctag name) ^ " -> ")
               ^^ uncode (action w (code $ string "tl"))
             in
             Dynarray.add_last cont_codes code;
             loop (i + 1)
         in
         seq
           (assert_env_length w (int 1))
           (fun _ ->
             code
             $ string "match resolve " ^^ uncode w
               ^^ string " K with | None -> () | Some (hd, tl) -> match Word.get_value hd with "
               ^^ separate (break 1) (Dynarray.to_list (loop 0)))))

let generate_apply_cont_ ctx =
  set_code apply_cont
    (lam "w" (fun w ->
         seq
           (assert_env_length w (int 1))
           (fun _ ->
             code
             $ string "match resolve " ^^ uncode w
               ^^ string " K with | None -> () | Some (hd, tl) -> match Word.get_value hd with "
               ^^ separate_map (break 1)
                    (fun (name, action) ->
                      string ("| " ^ string_of_int (Hashtbl.find_exn ctx.ctag name) ^ " -> ")
                      ^^ uncode (action w (code $ string "tl")))
                    (Dynarray.to_list ctx.conts))))

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
            ^^ uncode (Option.get (Dynarray.get codes i))
            ^^ string " "
            ^^ string (string_of_int i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string
              ("let () = Value.set_constructor_degree " ^ string_of_int i ^ " " ^ "("
              ^ string_of_int (Dynarray.get ctx.constructor_degree i)
              ^ ")")))
