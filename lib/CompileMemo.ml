open Common
open PPrint
open Syntax
open Memo
open State
open Code

(*
 * CompileMemo builds the code generator that emits the specialised CEK VM.
 * There are three layers that cooperate:
 *   - Helpers in [Code] provide a small IR DSL.  Everything in this file
 *     should construct documents through those helpers instead of
 *     concatenating strings by hand.
 *   - A collection of combinators below (scope handling,
 *     continuation registry) tracks meta information such as constructor
 *     tags, environment layouts, and the code fragments for each generated
 *     continuation.
 *   - The printers at the bottom (`compile_*`, `generate_apply_cont`,
 *     `pp_cek_ant`) walk syntax/IR and produce OCaml source that encodes
 *     the CEK state machine, storing the resulting snippets in [codes] for
 *     later emission.
 * The goal is that high level transformations only talk in terms of the
 * DSL (int/lam/app/seq/…) while environment bookkeeping and code emission
 * stay centralised here.
 *)

(*todo: implement tail call*)
(*todo: do not do a stack machine*)

(* As K come with interpretative overhead,
 *   we want to use K as little as possible,
 *   instead storing the computed temporary variables onto the env as a stack,
 *   only using K whenever we do a non-tail function call.
 *)

module OrdStr = struct
  type t = string

  let compare (x : string) (y : string) : int = if x < y then -1 else if x > y then 1 else 0
end

exception DupKey

module MakeMap (Ord : Stdlib.Map.OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  let add_exn x data t = if exists (fun y _ -> y == x) t then raise DupKey else add x data t
end

module MapStr = MakeMap (OrdStr)

type ctx = {
  arity : (string, int) Hashtbl.t;
  ctag : (string, int) Hashtbl.t;
  constructor_degree : int Dynarray.t;
  conts : (string * (world code -> words code -> unit code)) Dynarray.t;
  mutable conts_count : int;
  func_pc : (string, pc) Hashtbl.t;
}

let add_cont (ctx : ctx) (name : string) (arity : int) (app : world code -> words code -> unit code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app);
  ctx.conts_count <- ctx.conts_count + 1

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

type pc = Pc of int

let pc_to_int (Pc pc) = pc

let int_to_pc pc = Pc pc

let add_code (c : (world -> unit) code option) : pc =
  let pc = Dynarray.length codes in
  Dynarray.add_last codes c;
  int_to_pc pc

let set_code (pc : pc) (c : (world -> unit) code) : unit =
  Dynarray.set codes (pc_to_int pc) (Some c)

let add_code_k (k : pc -> (world -> unit) code * 'a) : 'a =
  let pc = add_code None in
  let code, ret = k pc in
  set_code pc code;
  ret

let compile_ocaml_adt adt_name ctors =
  string
    ("type ocaml_" ^ adt_name ^ " = "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             if List.length types = 0 then con_name
             else con_name ^ " of " ^ String.concat " * " (List.map (fun _ -> "Value.seq") types))
           ctors))

let with_registered_constructor (ctx : ctx) con_name types k =
  let params = List.mapi (fun i ty -> (ty, "x" ^ string_of_int i)) types in
  let arity = List.length params in
  Hashtbl.add_exn ~key:con_name ~data:arity ctx.arity;
  let constructor_index = Hashtbl.length ctx.ctag in
  Hashtbl.add_exn ~key:con_name ~data:constructor_index ctx.ctag;
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  k ~params ~arity ~constructor_index

let with_splits count splits k =
  let_in "splits" splits (fun parts ->
      let rec gather idx acc =
        if idx = count then k (List.rev acc)
        else
          let_in ("split" ^ string_of_int idx) (list_nth parts (int idx)) (fun value -> gather (idx + 1) (value :: acc))
      in
      gather 0 [])

let compile_adt_constructors (e : ctx) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      with_registered_constructor e con_name types (fun ~params ~arity:_ ~constructor_index ->
          let param_names = List.map snd params in
          let param_docs = List.map string param_names in
          let param_codes : Value.seq code list = List.map (fun name -> code (string name)) param_names in
          let ctor_tag = int constructor_index in
          let body = memo_appends (from_constructor ctor_tag :: param_codes) in
          string "let "
          ^^ string (adt_name ^ "_" ^ con_name)
          ^^ (if param_docs = [] then empty else space ^^ separate space param_docs)
          ^^ string ": Value.seq = " ^^ uncode body))
    ctors

(*todo: distinguish ffi inner type.*)
let compile_adt_ffi e adt_name ctors =
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
  ^^
  let head =
    string ("let to_ocaml_" ^ adt_name ^ " x = let (h, t) = Option.get (Memo.list_match x) in match ")
    ^^ uncode (word_get_value (code $ string "h"))
    ^^ string " with | "
  in
  let cases =
    String.concat " | "
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
         ctors)
  in
  head ^^ string (cases ^ " | _ -> failwith \"unreachable\"")

let compile_adt (e : ctx) adt_name ctors =
  let generate_ocaml_adt = compile_ocaml_adt adt_name ctors in
  let generate_adt_constructors = compile_adt_constructors e adt_name ctors in
  generate_ocaml_adt ^^ break 1 ^^ generate_adt_constructors ^^ break 1 ^^ compile_adt_ffi e adt_name ctors

let apply_cont : pc = add_code None

type env = int MapStr.t

let new_env () : env = MapStr.empty

type scope = {
  meta_env : int option MapStr.t linear;
  (*Note: env_length is not the amount of entries in meta_env above! It is the length of the environment when executing the cek machine.*)
  env_length : int;
  progressed : bool;
}

let new_scope () = { meta_env = make_linear MapStr.empty; env_length = 0; progressed = false }
let push_s s = { s with env_length = s.env_length + 1; progressed = true }

let extend_s s name =
  let meta_env = write_linear s.meta_env in

  (* Hashtbl.add_exn meta_env ~key:name ~data:(Some s.env_length); *)
  let meta_env = MapStr.add_exn name (Some s.env_length) meta_env in

  { s with meta_env = make_linear meta_env; env_length = s.env_length + 1 }

let drop_s s name =
  assert (Option.is_some (MapStr.find name (read_linear s.meta_env)));
  let meta_env = write_linear s.meta_env in
  (* Hashtbl.remove meta_env name; *)
  let meta_env = MapStr.remove name meta_env in
  { s with meta_env = make_linear meta_env; env_length = s.env_length - 1 }

let pop_n s n =
  assert (s.env_length >= n);
  { s with meta_env = s.meta_env; env_length = s.env_length - n }

let pop_s s = pop_n s 1

(*todo: we actually need to dup a bunch. lets switch to a functional data structure. *)
let dup_s s = { s with meta_env = make_linear (read_linear s.meta_env); env_length = s.env_length }

type kont = { k : scope -> world code -> unit code; fv : unit MapStr.t linear }

let dup_fv (fv : unit MapStr.t linear) : unit MapStr.t linear = make_linear (read_linear fv)
let empty_fv () : unit MapStr.t linear = make_linear MapStr.empty

let drop (s : scope) (vars : string list) (w : world code) (k : kont) : unit code =
  let new_s, n =
    List.fold_left
      (fun (s, n) var ->
        match MapStr.find var (read_linear s.meta_env) with None -> (s, n) | Some _ -> (drop_s s var, n + 1))
      (s, 0) vars
  in
  seqs
    [
      (fun _ -> assert_env_length w (int s.env_length));
      (fun _ -> drop_n w (int s.env_length) (int n));
      (fun _ -> k.k new_s w);
    ]

let return (s : scope) (w : world code) : unit code =
  seq (assert_env_length w (int s.env_length))
    (fun _ -> return_n w (int s.env_length) (pc_to_exp (int (pc_to_int apply_cont))))

let add_fv (v : string) (fv : unit MapStr.t linear) : unit MapStr.t linear =
  let fv = write_linear fv in
  let fv = MapStr.add v () fv in
  make_linear fv

let remove_fv (v : string) (fv : unit MapStr.t linear) : unit MapStr.t linear =
  let fv = write_linear fv in
  let fv = MapStr.remove v fv in
  make_linear fv

let rec fv_expr (e : expr) (fv : unit MapStr.t linear) : unit MapStr.t linear =
  match e with
  | Ctor _ | Int _ | GVar _ -> fv
  | App (f, xs) -> fv_exprs xs (fv_expr f fv)
  | Op (_, x, y) -> fv_expr y (fv_expr x fv)
  | Var name -> add_fv name fv
  | Match (value, cases) -> fv_expr value (fv_cases cases fv)
  | If (i, t, e) -> fv_expr i (fv_expr t (fv_expr e fv))
  | Let (BOne (l, v), r) -> fv_expr v (fv_pat l (fv_expr r fv))
  | _ -> failwith ("fv_expr: " ^ show_expr e)

and fv_exprs (es : expr list) (fv : unit MapStr.t linear) : unit MapStr.t linear =
  List.fold_left (fun fv e -> fv_expr e fv) fv es

and fv_pat (pat : pattern) (fv : unit MapStr.t linear) : unit MapStr.t linear =
  match pat with
  | PApp (_, None) -> fv
  | PApp (_, Some x) -> fv_pat x fv
  | PTup xs -> List.fold_left (fun fv x -> fv_pat x fv) fv xs
  | PVar name -> remove_fv name fv
  | _ -> failwith (show_pattern pat)

and fv_cases (MatchPattern c : cases) (fv : unit MapStr.t linear) : unit MapStr.t linear =
  List.fold_left (fun fv (pat, e) -> fv_pat pat (fv_expr e fv)) fv c

type keep_t = { mutable keep : bool; mutable source : string option }

let keep_only (s : scope) (fv : unit MapStr.t linear) : int Dynarray.t * scope =
  let keep : keep_t Dynarray.t = Dynarray.init s.env_length (fun _ -> { keep = true; source = None }) in
  MapStr.iter
    (fun key data -> match data with None -> () | Some i -> Dynarray.set keep i { keep = false; source = Some key })
    (read_linear s.meta_env);
  MapStr.iter
    (fun v _ ->
      let i = Option.get (MapStr.find v (read_linear s.meta_env)) in
      (Dynarray.get keep i).keep <- true)
    (read_linear fv);
  let keep_idx : int Dynarray.t = Dynarray.create () in
  let _, meta_env =
    Dynarray.fold_left
      (fun (i, acc) k ->
        if k.keep then (
          Dynarray.add_last keep_idx i;
          match k.source with
          | None -> (i + 1, acc)
          | Some v -> (i + 1, MapStr.add_exn v (Some (Dynarray.length keep_idx)) acc))
        else (i + 1, acc))
      (0, MapStr.empty) keep
  in
  let others = MapStr.map (fun _ -> None) (read_linear s.meta_env) in
  let meta_env = MapStr.union (fun _ x _ -> Some x) meta_env others in
  (keep_idx, { s with meta_env = make_linear meta_env; env_length = Dynarray.length keep_idx })

let reading (s : scope) (f : scope -> world code -> unit code) (w : world code) : unit code =
  let make_code w = f { s with progressed = false } w in
  if s.progressed then goto w (add_code $ Some (lam "w" make_code)) else make_code w

let rec compile_pp_expr (ctx : ctx) (s : scope) (c : expr) (k : kont) : world code -> unit code =
  match c with
  | Var name ->
      let loc = Option.get (MapStr.find name (read_linear s.meta_env)) in
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (get_env w (int loc)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Match (value, cases) ->
      compile_pp_expr ctx s value
        {
          k = (fun s -> reading s $ fun s w -> compile_pp_cases ctx (dup_s s) cases k w);
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
      compile_pp_expr ctx s x0
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
      compile_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              compile_pp_expr ctx s x1
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
      compile_pp_exprs ctx s xs
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
                  (fun _ -> goto w (pc_to_int (Hashtbl.find_exn ctx.func_pc f)));
                ]);
          fv = k.fv;
        }
  | Op ("+", x0, x1) ->
      compile_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              compile_pp_expr ctx s x1
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
      compile_pp_expr ctx s v
        {
          k =
            (fun s w ->
              compile_pp_expr ctx
                (extend_s (pop_s s) l)
                r
                { k = (fun s w -> drop s [ l ] w k); fv = add_fv l (dup_fv k.fv) }
                w);
          fv = fv_pat (PVar l) (fv_expr r (dup_fv k.fv));
        }
  | _ -> failwith ("compile_pp_expr: " ^ show_expr c)

and compile_pp_exprs (ctx : ctx) (s : scope) (cs : expr list) (k : kont) : world code -> unit code =
  match cs with
  | [] -> fun w -> k.k s w
  | c :: cs ->
      compile_pp_expr ctx s c { k = (fun s w -> compile_pp_exprs ctx s cs k w); fv = fv_exprs cs (dup_fv k.fv) }

and compile_pp_cases (ctx : ctx) (s : scope) (MatchPattern c : cases) (k : kont) : world code -> unit code =
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
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^ (uncode $ compile_pp_expr ctx s expr k w)
                        | PApp (cname, Some (PVar x0)) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^ uncode
                                 (with_splits 1
                                    (memo_splits (pair_value x))
                                    (function
                                      | [ x0_v ] ->
                                          seq (push_env w x0_v) (fun _ ->
                                              compile_pp_expr ctx (extend_s s x0) expr
                                                { k = (fun s w -> drop s [ x0 ] w k); fv = k.fv }
                                                w)
                                      | _ -> failwith "with_splits: unexpected arity"))
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^ uncode
                                 (with_splits 2
                                    (memo_splits (pair_value x))
                                    (function
                                      | [ x0_v; x1_v ] ->
                                          seqs
                                            [
                                              (fun _ -> push_env w x0_v);
                                              (fun _ -> push_env w x1_v);
                                              (fun _ ->
                                                compile_pp_expr ctx
                                                  (extend_s (extend_s s x0) x1)
                                                  expr
                                                  { k = (fun s w -> drop s [ x1; x0 ] w k); fv = k.fv }
                                                  w);
                                            ]
                                      | _ -> failwith "with_splits: unexpected arity"))
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1; PVar x2 ])) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^ uncode
                                 (with_splits 3
                                    (memo_splits (pair_value x))
                                    (function
                                      | [ x0_v; x1_v; x2_v ] ->
                                          seqs
                                            [
                                              (fun _ -> push_env w x0_v);
                                              (fun _ -> push_env w x1_v);
                                              (fun _ -> push_env w x2_v);
                                              (fun _ ->
                                                compile_pp_expr ctx
                                                  (extend_s (extend_s (extend_s s x0) x1) x2)
                                                  expr
                                                  { k = (fun s w -> drop s [ x2; x1; x0 ] w k); fv = k.fv }
                                                  w);
                                            ]
                                      | _ -> failwith "with_splits: unexpected arity"))
                        | _ -> failwith (show_pattern pat))
                      c
                  in
                  let default_case = string "| _ -> failwith \"unreachable\"" in
                  code
                    (string " (match "
                    ^^ uncode (word_get_value (zro x))
                    ^^ string " with " ^^ t ^^ break 1 ^^ default_case ^^ string ")")))))

let compile_pp_stmt (ctx : ctx) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) -> compile_adt ctx name ctors
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (x, Lam (ps, term)) ->
      let s =
        List.fold_left
          (fun s p -> match p with PVar n -> extend_s s n | _ -> failwith (show_pattern p))
          (new_scope ()) ps
      in
      let arg_num = s.env_length in
      let name = match x with Some (PVar x) -> x | _ -> failwith "bad match" in
      let cont_done_tag = int (Hashtbl.find_exn ctx.ctag "cont_done") in
      add_code_k (fun entry_code ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_code;
          ( lam "w" (fun w -> compile_pp_expr ctx s term { k = (fun s w -> return s w); fv = empty_fv () } w),
            string "let rec" ^^ space ^^ string name ^^ space
            ^^ separate space (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
            ^^ string ": Value.seq " ^^ string "=" ^^ space ^^ group @@ string "exec_cek "
            ^^ string ("(pc_to_exp " ^ string_of_int (pc_to_int entry_code) ^ ")")
            ^^ string "(Dynarray.of_list" ^^ string "["
            ^^ separate (string ";") (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ ")")))
            ^^ string "]" ^^ string ")" ^^ string "("
            ^^ uncode (from_constructor cont_done_tag)
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
         let rec loop i =
           if i == Dynarray.length ctx.conts then cont_codes
           else
             let name, action = Dynarray.get ctx.conts i in
             let code =
               string "| "
               ^^ uncode (int (Hashtbl.find_exn ctx.ctag name))
               ^^ string " -> "
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
               ^^ string " K with | None -> () | Some (hd, tl) -> match "
               ^^ uncode (word_get_value (code $ string "hd"))
               ^^ string " with "
               ^^ separate (break 1) (Dynarray.to_list (loop 0))
               ^^ break 1
               ^^ string "| _ -> failwith \"unreachable\"")))

let generate_apply_cont_ ctx =
  set_code apply_cont
    (lam "w" (fun w ->
         seq
           (assert_env_length w (int 1))
           (fun _ ->
             code
             $ string "match resolve " ^^ uncode w
               ^^ string " K with | None -> () | Some (hd, tl) -> match "
               ^^ uncode (word_get_value (code $ string "hd"))
               ^^ string " with "
               ^^ separate_map (break 1)
                    (fun (name, action) ->
                      string "| "
                      ^^ uncode (int (Hashtbl.find_exn ctx.ctag name))
                      ^^ string " -> "
                      ^^ uncode (action w (code $ string "tl")))
                    (Dynarray.to_list ctx.conts)
               ^^ break 1
               ^^ string "| _ -> failwith \"unreachable\"")))

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (compile_pp_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Value"
  ^^ break 1 ^^ string "let memo = Array.init "
  ^^ uncode (int (Dynarray.length codes))
  ^^ string "(fun _ -> ref State.BlackHole)" ^^ break 1 ^^ generated_stmt ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string "let () = add_exp " ^^ uncode (Option.get (Dynarray.get codes i)) ^^ string " " ^^ uncode (int i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string "let () = Value.set_constructor_degree "
            ^^ uncode (int i)
            ^^ string " ("
            ^^ uncode (int (Dynarray.get ctx.constructor_degree i))
            ^^ string ")"))
