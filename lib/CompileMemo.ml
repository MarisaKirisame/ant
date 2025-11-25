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
  add_cont ctx "cont_done" 0 (fun w _ -> exec_done_ w);
  ctx

let codes : (world -> unit) code option Dynarray.t = Dynarray.create ()

let add_code (c : (world -> unit) code option) : pc =
  let pc = Dynarray.length codes in
  Dynarray.add_last codes c;
  int_to_pc pc

let set_code (pc : pc) (c : (world -> unit) code) : unit = Dynarray.set codes (pc_to_int pc) (Some c)

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
  let_in_ "splits" splits (fun parts ->
      let rec gather idx acc =
        if idx = count then k (List.rev acc)
        else
          let_in_
            ("split" ^ string_of_int idx)
            (list_nth_ parts (int_ idx))
            (fun value -> gather (idx + 1) (value :: acc))
      in
      gather 0 [])

let compile_adt_constructors (e : ctx) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      with_registered_constructor e con_name types (fun ~params ~arity:_ ~constructor_index ->
          let param_names = List.map snd params in
          let param_docs = List.map string param_names in
          let param_codes : Value.seq code list = List.map (fun name -> code (string name)) param_names in
          let ctor_tag = int_ constructor_index in
          let body = memo_appends_ (from_constructor_ ctor_tag :: param_codes) in
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
    ^^ uncode (word_get_value_ (code $ string "h"))
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
  meta_env : int option MapStr.t;
  (*Note: env_length is not the amount of entries in meta_env above! It is the length of the environment when executing the cek machine.*)
  env_length : int;
  progressed : bool;
}

let check_scope s =
  let seen : bool Array.t = Array.init s.env_length (fun _ -> false) in
  MapStr.iter
    (fun key data ->
      match data with
      | None -> ()
      | Some i ->
          if not (i < s.env_length) then
            failwith
              ("check_scope: variable " ^ key ^ " mapped to invalid index " ^ string_of_int i ^ " with env_length "
             ^ string_of_int s.env_length)
          else if Array.get seen i then
            failwith ("check_scope: variable " ^ key ^ " mapped to duplicate index " ^ string_of_int i)
          else Array.set seen i true)
    s.meta_env

let new_scope () = { meta_env = MapStr.empty; env_length = 0; progressed = false }
let push_s s = { s with env_length = s.env_length + 1; progressed = true }

let extend_s s name =
  check_scope s;
  let meta_env = s.meta_env in

  (* Hashtbl.add_exn meta_env ~key:name ~data:(Some s.env_length); *)
  let meta_env = MapStr.add_exn name (Some s.env_length) meta_env in

  let ret = { s with meta_env; env_length = s.env_length + 1 } in
  check_scope ret;
  ret

let drop_s s name =
  assert (Option.is_some (MapStr.find name s.meta_env));
  let meta_env = s.meta_env in
  (* Hashtbl.remove meta_env name; *)
  let meta_env = MapStr.remove name meta_env in
  { s with meta_env; env_length = s.env_length - 1 }

let pop_n s n =
  check_scope s;
  assert (s.env_length >= n);
  let ret = { s with meta_env = s.meta_env; env_length = s.env_length - n } in
  check_scope ret;
  ret

let pop_s s = pop_n s 1

(*todo: we actually need to dup a bunch. lets switch to a functional data structure. *)
(* let dup_s s = { s with meta_env = s.meta_env; env_length = s.env_length } *)

type kont = { k : scope -> world code -> unit code; fv : unit MapStr.t }

let dup_fv (fv : unit MapStr.t) : unit MapStr.t = fv
let empty_fv () : unit MapStr.t = MapStr.empty

let drop (s : scope) (vars : string list) (w : world code) (k : kont) : unit code =
  let new_s, n =
    List.fold_left
      (fun (s, n) var -> match MapStr.find var s.meta_env with None -> (s, n) | Some _ -> (drop_s s var, n + 1))
      (s, 0) vars
  in
  seqs_
    [
      (fun _ -> assert_env_length_ w (int_ s.env_length));
      (fun _ -> drop_n_ w (int_ s.env_length) (int_ n));
      (fun _ -> k.k new_s w);
    ]

let return (s : scope) (w : world code) : unit code =
  seq_
    (assert_env_length_ w (int_ s.env_length))
    (fun _ -> return_n_ w (int_ s.env_length) (pc_to_exp_ (pc_ apply_cont)))

let add_fv (v : string) (fv : unit MapStr.t) : unit MapStr.t = MapStr.add v () fv
let remove_fv (v : string) (fv : unit MapStr.t) : unit MapStr.t = MapStr.remove v fv

let rec fv_expr (e : expr) (fv : unit MapStr.t) : unit MapStr.t =
  match e with
  | Ctor _ | Int _ | GVar _ -> fv
  | App (f, xs) -> fv_exprs xs (fv_expr f fv)
  | Op (_, x, y) -> fv_expr y (fv_expr x fv)
  | Var name -> add_fv name fv
  | Match (value, cases) -> fv_expr value (fv_cases cases fv)
  | If (i, t, e) -> fv_expr i (fv_expr t (fv_expr e fv))
  | Let (BOne (l, v), r) -> fv_expr v (fv_pat l (fv_expr r fv))
  | _ -> failwith ("fv_expr: " ^ show_expr e)

and fv_exprs (es : expr list) (fv : unit MapStr.t) : unit MapStr.t = List.fold_left (fun fv e -> fv_expr e fv) fv es

and fv_pat (pat : pattern) (fv : unit MapStr.t) : unit MapStr.t =
  match pat with
  | PApp (_, None) | PAny -> fv
  | PApp (_, Some x) -> fv_pat x fv
  | PTup xs -> List.fold_left (fun fv x -> fv_pat x fv) fv xs
  | PVar name -> remove_fv name fv
  | _ -> failwith (show_pattern pat)

and fv_cases (MatchPattern c : cases) (fv : unit MapStr.t) : unit MapStr.t =
  List.fold_left (fun fv (pat, e) -> fv_pat pat (fv_expr e fv)) fv c

type keep_t = { mutable keep : bool; mutable source : string option }

let keep_only (s : scope) (fv : unit MapStr.t) : int Dynarray.t * scope =
  check_scope s;
  let keep : keep_t Dynarray.t = Dynarray.init s.env_length (fun _ -> { keep = true; source = None }) in
  MapStr.iter
    (fun key data -> match data with None -> () | Some i -> Dynarray.set keep i { keep = false; source = Some key })
    s.meta_env;
  MapStr.iter
    (fun v _ ->
      let i =
        match MapStr.find_opt v s.meta_env with Some (Some i) -> i | _ -> failwith ("keep_only not found:" ^ v)
      in
      (Dynarray.get keep i).keep <- true)
    fv;
  let keep_idx : int Dynarray.t = Dynarray.create () in
  let _, meta_env =
    Dynarray.fold_left
      (fun (i, acc) k ->
        if k.keep then (
          let ret =
            match k.source with
            | None -> (i + 1, acc)
            | Some v -> (i + 1, MapStr.add_exn v (Some (Dynarray.length keep_idx)) acc)
          in
          Dynarray.add_last keep_idx i;
          ret)
        else (i + 1, acc))
      (0, MapStr.empty) keep
  in
  let others = MapStr.map (fun _ -> None) s.meta_env in
  let meta_env = MapStr.union (fun _ x _ -> Some x) meta_env others in
  let s = { s with meta_env; env_length = Dynarray.length keep_idx } in
  check_scope s;
  (keep_idx, s)

let reading (s : scope) (f : scope -> world code -> unit code) (w : world code) : unit code =
  let make_code w = f { s with progressed = false } w in
  if s.progressed then goto_ w (add_code $ Some (lam_ "w" make_code)) else make_code w

let rec compile_pp_expr (ctx : ctx) (s : scope) (c : expr) (k : kont) : world code -> unit code =
  match c with
  | Var name ->
      let loc =
        match MapStr.find name s.meta_env with
        | Some loc -> loc
        | None -> failwith ("compile_pp_expr cannot find var: " ^ name)
      in
      fun w ->
        seqs_
          [
            (fun _ -> assert_env_length_ w (int_ s.env_length));
            (fun _ -> push_env_ w (get_env_ w (int_ loc)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Match (value, cases) ->
      compile_pp_expr ctx s value
        { k = (fun s -> reading s $ fun s w -> compile_pp_cases ctx s cases k w); fv = fv_cases cases (dup_fv k.fv) }
  | Ctor cname ->
      fun w ->
        seqs_
          [
            (fun _ -> assert_env_length_ w (int_ s.env_length));
            (fun _ -> push_env_ w (from_constructor_ (int_ (Hashtbl.find_exn ctx.ctag cname))));
            (fun _ -> k.k (push_s s) w);
          ]
  | App (Ctor cname, [ x0 ]) ->
      compile_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              seqs_
                [
                  (fun _ -> assert_env_length_ w (int_ s.env_length));
                  (fun _ ->
                    let_in_ "x0" (pop_env_ w) (fun x0 ->
                        push_env_ w (memo_appends_ [ from_constructor_ (int_ (Hashtbl.find_exn ctx.ctag cname)); x0 ])));
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
                      seqs_
                        [
                          (fun _ -> assert_env_length_ w (int_ s.env_length));
                          (fun _ ->
                            let_in_ "x1" (pop_env_ w) (fun x1 ->
                                let_in_ "x0" (pop_env_ w) (fun x0 ->
                                    push_env_ w
                                      (memo_appends_
                                         [ from_constructor_ (int_ (Hashtbl.find_exn ctx.ctag cname)); x0; x1 ]))));
                          (fun _ -> k.k (push_s (pop_s (pop_s s))) w);
                        ]);
                  fv = k.fv;
                }
                w);
          fv = fv_expr x1 (dup_fv k.fv);
        }
  | App (Ctor cname, [ x0; x1; x2 ]) ->
      compile_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              compile_pp_expr ctx s x1
                {
                  k =
                    (fun s w ->
                      compile_pp_expr ctx s x2
                        {
                          k =
                            (fun s w ->
                              seqs_
                                [
                                  (fun _ -> assert_env_length_ w (int_ s.env_length));
                                  (fun _ ->
                                    let_in_ "x2" (pop_env_ w) (fun x2 ->
                                        let_in_ "x1" (pop_env_ w) (fun x1 ->
                                            let_in_ "x0" (pop_env_ w) (fun x0 ->
                                                push_env_ w
                                                  (memo_appends_
                                                     [
                                                       from_constructor_ (int_ (Hashtbl.find_exn ctx.ctag cname));
                                                       x0;
                                                       x1;
                                                       x2;
                                                     ])))));
                                  (fun _ -> k.k (push_s (pop_s (pop_s (pop_s s)))) w);
                                ]);
                          fv = k.fv;
                        }
                        w);
                  fv = fv_expr x1 (dup_fv k.fv);
                }
                w);
          fv = fv_expr x2 (fv_expr x1 (dup_fv k.fv));
        }
  | App (GVar f, xs) ->
      check_scope s;
      let cont_name = "cont_" ^ string_of_int ctx.conts_count in
      let keep, keep_s = keep_only s k.fv in
      (* subtracting 1 to remove the arguments; adding 1 for the next continuation*)
      let keep_length = keep_s.env_length in
      add_cont ctx cont_name (keep_length + 1) (fun w tl ->
          let code =
            seqs_
              [
                (fun _ -> set_k_ w (get_next_cont_ tl));
                (fun _ -> restore_env_ w (int_ keep_length) tl);
                (fun _ -> k.k (push_s keep_s) w);
              ]
          in
          code);
      let xs_length = List.length xs in
      compile_pp_exprs ctx s xs
        {
          k =
            (fun s w ->
              seqs_
                [
                  (fun _ -> assert_env_length_ w (int_ s.env_length));
                  (fun _ ->
                    let_in_ "keep"
                      (env_call_ w (list_literal_of_ int_ (Dynarray.to_list keep)) (int_ xs_length))
                      (fun keep ->
                        set_k_ w
                          (memo_appends_
                             [ from_constructor_ (int_ (Hashtbl.find_exn ctx.ctag cont_name)); keep; world_kont_ w ])));
                  (fun _ -> goto_ w (Hashtbl.find_exn ctx.func_pc f));
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
                      seqs_
                        [
                          (fun _ -> assert_env_length_ w (int_ s.env_length));
                          (fun _ ->
                            match_option_
                              (resolve_ w (src_E_ (s.env_length - 2)))
                              (fun _ -> unit_)
                              "x0"
                              (fun x0 ->
                                match_option_
                                  (resolve_ w (src_E_ (s.env_length - 1)))
                                  (fun _ -> unit_)
                                  "x1"
                                  (fun x1 ->
                                    seqs_
                                      [
                                        (fun _ -> to_unit_ $ pop_env_ w);
                                        (fun _ -> to_unit_ $ pop_env_ w);
                                        (fun _ ->
                                          push_env_ w
                                            (memo_from_int_
                                               (add_ (int_from_word_ (zro_ x0)) (int_from_word_ (zro_ x1)))));
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
        seqs_
          [
            (fun _ -> assert_env_length_ w (int_ s.env_length));
            (fun _ -> push_env_ w (memo_from_int_ (int_ i)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Let (BOne (PVar l, v), r) ->
      check_scope s;
      compile_pp_expr ctx s v
        {
          k =
            (fun s w ->
              check_scope s;
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
  seq_
    (assert_env_length_ w (int_ s.env_length))
    (fun _ ->
      let_in_ "last"
        (src_E_ (s.env_length - 1))
        (fun last ->
          let m = resolve_ w last in
          match_option_ m
            (fun _ -> unit_)
            "x"
            (fun x ->
              seq_b1_
                (to_unit_ $ pop_env_ w)
                (fun _ ->
                  let s = pop_s s in
                  let t =
                    Stdlib.List.map
                      (fun (pat, expr) ->
                        (*todo: special casing for now, as pat design need changes. *)
                        match pat with
                        | PApp (cname, None) -> (int_ (Hashtbl.find_exn ctx.ctag cname), compile_pp_expr ctx s expr k w)
                        | PApp (cname, Some (PVar x0)) ->
                            ( int_ (Hashtbl.find_exn ctx.ctag cname),
                              with_splits 1
                                (memo_splits_ (pair_value_ x))
                                (function
                                  | [ x0_v ] ->
                                      seq_ (push_env_ w x0_v) (fun _ ->
                                          compile_pp_expr ctx (extend_s s x0) expr
                                            { k = (fun s w -> drop s [ x0 ] w k); fv = k.fv }
                                            w)
                                  | _ -> failwith "with_splits: unexpected arity") )
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                            ( int_ (Hashtbl.find_exn ctx.ctag cname),
                              with_splits 2
                                (memo_splits_ (pair_value_ x))
                                (function
                                  | [ x0_v; x1_v ] ->
                                      seqs_
                                        [
                                          (fun _ -> push_env_ w x0_v);
                                          (fun _ -> push_env_ w x1_v);
                                          (fun _ ->
                                            compile_pp_expr ctx
                                              (extend_s (extend_s s x0) x1)
                                              expr
                                              { k = (fun s w -> drop s [ x1; x0 ] w k); fv = k.fv }
                                              w);
                                        ]
                                  | _ -> failwith "with_splits: unexpected arity") )
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1; PVar x2 ])) ->
                            ( int_ (Hashtbl.find_exn ctx.ctag cname),
                              with_splits 3
                                (memo_splits_ (pair_value_ x))
                                (function
                                  | [ x0_v; x1_v; x2_v ] ->
                                      seqs_
                                        [
                                          (fun _ -> push_env_ w x0_v);
                                          (fun _ -> push_env_ w x1_v);
                                          (fun _ -> push_env_ w x2_v);
                                          (fun _ ->
                                            compile_pp_expr ctx
                                              (extend_s (extend_s (extend_s s x0) x1) x2)
                                              expr
                                              { k = (fun s w -> drop s [ x2; x1; x0 ] w k); fv = k.fv }
                                              w);
                                        ]
                                  | _ -> failwith "with_splits: unexpected arity") )
                        | PAny -> (raw "_", compile_pp_expr ctx s expr k w)
                        | _ -> failwith (show_pattern pat))
                      c
                  in
                  let default_case = (raw "_", unreachable_) in
                  paren $ match_int_ (word_get_value_ (zro_ x)) (List.append t [ default_case ])))))

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
      let cont_done_tag = int_ (Hashtbl.find_exn ctx.ctag "cont_done") in
      add_code_k (fun entry_code ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_code;
          ( lam_ "w" (fun w -> compile_pp_expr ctx s term { k = (fun s w -> return s w); fv = empty_fv () } w),
            string "let rec" ^^ space ^^ string name ^^ space
            ^^ separate space (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
            ^^ string ": exec_result " ^^ string "=" ^^ space ^^ group @@ string "(exec_cek "
            ^^ string ("(pc_to_exp (int_to_pc " ^ string_of_int (pc_to_int entry_code) ^ "))")
            ^^ string "(Dynarray.of_list" ^^ string "["
            ^^ separate (string ";") (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ ")")))
            ^^ string "]" ^^ string ")" ^^ string "("
            ^^ uncode (from_constructor_ cont_done_tag)
            ^^ string ")" ^^ string " memo)" ))
  | _ -> failwith (show_stmt s)

let generate_apply_cont ctx =
  set_code apply_cont
    (lam_ "w" (fun w ->
         (* We have to be careful: ctx.conts will grow as we apply the lambdas, 
          *   so we cannot do a single map over the whole array, 
          *   instead we have to take elements out on by one.
          *)
         let cont_codes = Dynarray.create () in
         let rec loop tl i =
           if i == Dynarray.length ctx.conts then cont_codes
           else
             let name, action = Dynarray.get ctx.conts i in
             let code = (Hashtbl.find_exn ctx.ctag name, action w tl) in
             Dynarray.add_last cont_codes code;
             loop tl (i + 1)
         in
         seq_
           (assert_env_length_ w (int_ 1))
           (fun _ ->
             match_resolve_destruct_
               (resolve_ w (code $ string "K"))
               (fun _ -> unit_)
               "hd" "tl"
               (fun hd tl ->
                 paren $ match_int_default_ (word_get_value_ hd) (Dynarray.to_list (loop tl 0)) unreachable_))))

let generate_apply_cont_ ctx =
  set_code apply_cont
    (lam_ "w" (fun w ->
         seq_
           (assert_env_length_ w (int_ 1))
           (fun _ ->
             match_resolve_destruct_
               (resolve_ w (code $ string "K"))
               (fun _ -> unit_)
               "hd" "tl"
               (fun hd tl ->
                 paren
                 $ match_int_default_ (word_get_value_ hd)
                     (List.init (Dynarray.length ctx.conts) (fun i ->
                          let name, action = Dynarray.get ctx.conts i in
                          (Hashtbl.find_exn ctx.ctag name, action w tl)))
                     unreachable_))))

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (compile_pp_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Value"
  ^^ break 1 ^^ string "open Common" ^^ break 1 ^^ string "let memo = init_memo () " ^^ break 1 ^^ generated_stmt
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string "let () = add_exp " ^^ uncode (Option.get (Dynarray.get codes i)) ^^ string " " ^^ uncode (int_ i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string "let () = Words.set_constructor_degree "
            ^^ uncode (int_ i)
            ^^ string " ("
            ^^ uncode (int_ (Dynarray.get ctx.constructor_degree i))
            ^^ string ")"))

module Backend = struct
  let compile = pp_cek_ant
end
