open Core
open Common
open PPrint
open Syntax
open Memo
open State
open Code
module StrMap = Map.Make (String)
module StrSet = Set.Make (String)
module Liveness = CompileMemo.Liveness

type words = Value.seq
type 'a str_tbl = (string, 'a) Hashtbl.t

type ctx = {
  arity : int str_tbl;
  ctag : int str_tbl;
  ctag_name : string str_tbl;
  constructor_degree : int Dynarray.t;
  conts : (string * (world code -> words code -> unit code)) Dynarray.t;
  mutable conts_count : int;
  func_pc : pc str_tbl;
}

type loc = Slot of int [@@deriving equal]

type scope = {
  meta_env : loc option StrMap.t;
  (* Note: env_length is not the amount of entries in meta_env above! 
   * It is the length of the environment when executing the cek machine.
   *)
  env_length : int;
  progressed : bool;
}

let ( let* ) e f = e f
let new_scope () = { meta_env = StrMap.empty; env_length = 0; progressed = false }

let check_scope s =
  let seen : bool Array.t = Array.init s.env_length ~f:(fun _ -> false) in
  Map.iteri s.meta_env ~f:(fun ~key ~data ->
      match data with
      | None -> ()
      | Some (Slot i) ->
          if not (i < s.env_length) then
            failwith
              [%string
                "check_scope: variable %{key} mapped to invalid index %{string_of_int i} with env_length \
                 %{string_of_int s.env_length}"]
          else if Array.get seen i then
            failwith [%string "check_scope: variable %{key} mapped to duplicate index %{string_of_int i}"]
          else Array.set seen i true)

let add_s s name loc =
  check_scope s;
  let meta_env = s.meta_env in
  let meta_env = Map.add_exn ~key:name ~data:(Some loc) meta_env in
  let ret = { s with meta_env; env_length = s.env_length } in
  check_scope ret;
  ret

let drop_s s name =
  check_scope s;
  assert (Option.is_some (Map.find s.meta_env name));
  let meta_env = s.meta_env in
  let meta_env = Map.remove meta_env name in
  let r = { s with meta_env; env_length = s.env_length - 1 } in
  check_scope r;
  r

let init_params s (params : string list) =
  check_scope s;
  let i, meta_env =
    List.fold_left params ~init:(0, s.meta_env) ~f:(fun (i, meta_env) name ->
        (i + 1, Map.add_exn ~key:name ~data:(Some (Slot i)) meta_env))
  in
  let r = { s with meta_env; env_length = s.env_length + i } in
  check_scope r;
  r

let get_ctor_tag_name (name : string) : string = "tag_" ^ name
let ctor_tag_name (ctx : ctx) (cname : string) : int code = raw (Hashtbl.find_exn ctx.ctag_name cname)

let add_cont (ctx : ctx) (name : string) (arity : int) (app : world code -> words code -> unit code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Hashtbl.add_exn ctx.ctag_name ~key:name ~data:(get_ctor_tag_name name);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app);
  ctx.conts_count <- ctx.conts_count + 1

let new_ctx () : ctx =
  let ctx =
    {
      arity = Hashtbl.create (module Core.String);
      ctag = Hashtbl.create (module Core.String);
      ctag_name = Hashtbl.create (module Core.String);
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

let with_splits count splits k =
  [%seqs
    let$ splits = splits in
    let rec gather idx acc =
      if idx = count then k (List.rev acc)
      else
        let_in_
          ("split" ^ string_of_int idx)
          (list_nth_ splits (int_ idx))
          (fun value -> gather (idx + 1) (value :: acc))
    in
    gather 0 []]

let register_constructor (ctx : ctx) con_name types =
  let arity = List.length types in
  Hashtbl.add_exn ~key:con_name ~data:arity ctx.arity;
  let constructor_index = Hashtbl.length ctx.ctag in
  Hashtbl.add_exn ~key:con_name ~data:constructor_index ctx.ctag;
  let tag_name = get_ctor_tag_name con_name in
  Hashtbl.add_exn ~key:con_name ~data:tag_name ctx.ctag_name;
  Dynarray.add_last ctx.constructor_degree (1 - arity)

let register_constructors (e : ctx) ctors =
  List.iter ~f:(fun (con_name, types, _) -> register_constructor e con_name types) ctors

let apply_cont : pc = add_code None

let ensure_trivial_patterns (pats : 'a pattern list) : string list =
  List.map pats ~f:(fun p ->
      match p with
      | PVar (n, _) -> n
      | _ -> failwith [%string "unsupported nested patterns: %{Syntax.string_of_document @@ Syntax.pp_pattern p}"])

let entry_code (entry_pc : pc) (arg_num : int) (name : string) (cont_done_tag : int code) : document =
  string "let rec" ^^ space ^^ string name ^^ space ^^ string "memo" ^^ space
  ^^ separate space (List.init arg_num ~f:(fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
  ^^ string ": exec_result " ^^ string "=" ^^ space ^^ group @@ string "(exec_cek "
  ^^ string ("(pc_to_exp (int_to_pc " ^ string_of_int (pc_to_int entry_pc) ^ "))")
  ^^ string "(Dynarray.of_list" ^^ string "["
  ^^ separate (string ";") (List.init arg_num ~f:(fun i -> string ("(x" ^ string_of_int i ^ ")")))
  ^^ string "]" ^^ string ")" ^^ string "("
  ^^ uncode (from_constructor_ cont_done_tag)
  ^^ string ")" ^^ string " memo)"

type kont = scope -> world code -> unit code
type kont_with_loc = loc * scope -> world code -> unit code
type kont_with_locs = loc list * scope -> world code -> unit code

let get_loc (loc : loc) : world code -> words code = fun w -> match loc with Slot i -> get_env_ w (int_ i)

let set_loc (dst : loc) (value : words code) (w : world code) : unit code =
  match dst with Slot i -> set_env_ w (int_ i) value

let resolve_loc (l : loc) : world code -> (Word.Word.t * words) code =
 fun w -> match l with Slot i -> resolve_ w (src_E_ i)

let alloc_n_slots (s : scope) (n : int) : loc list * scope =
  check_scope s;
  let r = { s with env_length = s.env_length + n; progressed = true } in
  check_scope r;
  let locs = List.init n ~f:(fun i -> Slot (s.env_length + i)) in
  (locs, r)

let alloc_slot (s : scope) =
  let [ loc ], s = alloc_n_slots s 1 in
  (loc, s)

let free_n_slots (s : scope) (n : int) =
  check_scope s;
  assert (s.env_length >= n);
  let r = { s with env_length = s.env_length - n; progressed = true } in
  check_scope r;
  r

let free_slot (s : scope) = free_n_slots s 1

let drop (s : scope) (vars : string list) : scope =
  List.fold_left
    ~f:(fun s var -> match Map.find_exn s.meta_env var with None -> s | Some _ -> drop_s s var)
    ~init:s vars

let return_with (s : scope) (v : words code) (w : world code) : unit code =
  [%seqs return_n_with_ w (int_ s.env_length) v (pc_to_exp_ (pc_ apply_cont))]

let reading (s : scope) (f : kont) (w : world code) : unit code =
  let make_code w = f { s with progressed = false } w in
  if s.progressed then goto_ w (add_code $ Some (lam_ "w" make_code)) else make_code w

(* if a variable has already been in a earlier slot, in some cases we don't need to move it *)
let rec compile_expr_no_move (ctx : ctx) (s : scope) (dst : loc) (c : 'a expr) (k : kont_with_loc) :
    world code -> unit code =
  match c with
  | Var (name, _) -> begin
      match Map.find s.meta_env name with
      | Some (Some loc) -> fun w -> k (loc, s) w
      | Some None -> failwith [%string "compile_expr: variable %{name} has been dropped"]
      | None -> failwith [%string "compile_expr: variable %{name} not found in scope"]
    end
  | _ -> compile_expr ctx s dst c k

and compile_expr (ctx : ctx) (s : scope) (dst : loc) (c : 'a expr) (k : kont_with_loc) : world code -> unit code =
  match c with
  | Int i ->
      fun w ->
        [%seqs
          set_loc dst (memo_from_int_ @@ int_ i) w;
          k (dst, s) w]
  | Var (name, _) -> begin
      match Map.find s.meta_env name with
      | Some (Some loc) ->
          fun w ->
            [%seqs
              assert_env_length_ w (int_ s.env_length);
              set_loc dst (get_loc loc w) w;
              k (dst, s) w]
      | Some None -> failwith [%string "compile_expr: variable %{name} has been dropped"]
      | None -> failwith [%string "compile_expr: variable %{name} not found in scope"]
    end
  | Ctor (cname, _) ->
      fun w ->
        [%seqs
          set_loc dst (from_constructor_ @@ ctor_tag_name ctx cname) w;
          k (dst, s) w]
  | App (Ctor (cname, _), xs, _) ->
      check_scope s;
      let n_args = List.length xs in
      assert (n_args > 0);
      let slots, s = alloc_n_slots s @@ n_args in
      fun w ->
        [%seqs
          assert_env_length_ w (int_ @@ (s.env_length - n_args));
          grow_env_ ~comment:"call to constructor" w (int_ @@ n_args);
          compile_exprs compile_expr_no_move ctx s slots xs
            (fun (l, s) w ->
              [%seqs
                let ct =
                  memo_appends_ (from_constructor_ (ctor_tag_name ctx cname) :: List.map l ~f:(fun l -> get_loc l w))
                in
                [%seqs
                  set_loc dst ct w;
                  assert_env_length_ w (int_ s.env_length);
                  shrink_env_ w (int_ @@ n_args);
                  k (dst, free_n_slots s n_args) w]])
            w]
  | If (c, t, f, _) ->
      check_scope s;
      let a, s = alloc_slot s in
      let b, s = alloc_slot s in
      let cond_name = gensym "cond" in
      let if_k if_cont (_l, _s) w =
        assert (equal_loc b _l);
        [%seqs
          set_loc dst (get_loc b w) w;
          app_ if_cont unit_]
      in
      let cond_k (_l, s) w =
        assert (equal_loc a _l);
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          let_pat_in_ (var_pat_ cond_name) (resolve_loc a w)
            [%seqs
              let$ if_cont =
                paren @@ lam_unit_
                @@ fun _ ->
                [%seqs
                  assert_env_length_ w (int_ s.env_length);
                  shrink_env_ w (int_ 2);
                  k (dst, free_n_slots s 2) w]
              in
              let cond_bool = code @@ parens @@ uncode (int_from_word_ @@ zro_ @@ var_ cond_name) ^^ string " <> 0" in
              let then_branch = compile_expr ctx s b t (if_k if_cont) w in
              let else_branch = compile_expr ctx s b f (if_k if_cont) w in
              if_ cond_bool then_branch else_branch]]
      in
      fun w ->
        [%seqs
          assert_env_length_ w (int_ @@ (s.env_length - 2));
          grow_env_ ~comment:"if" w (int_ 2);
          compile_expr ctx s a c cond_k w]
  | Match (scrut, cases, _) -> fun w -> compile_cases ctx s dst scrut cases k w
  | Op (op, x0, x1, _) ->
      check_scope s;
      let op_code =
        match op with
        | "+" -> add_
        | "<" -> lt_
        | "<=" -> le_
        | ">" -> gt_
        | ">=" -> ge_
        | _ -> failwith ("compile_pp_expr: unsupported op " ^ op)
      in
      let slot0, s = alloc_slot s in
      let slot1, s = alloc_slot s in
      let x0_eval_k =
       fun (l0, s) ->
        let* l1, s = compile_expr_no_move ctx s slot1 x1 in
        reading s @@ fun s w ->
        [%seqs
          let$ x0 = resolve_loc l0 w in
          let$ x1 = resolve_loc l1 w in
          let$ r = memo_from_int_ (op_code (int_from_word_ (zro_ x0)) (int_from_word_ (zro_ x1))) in
          set_loc dst r w;
          assert_env_length_ w (int_ s.env_length);
          shrink_env_ w (int_ 2);
          k (dst, free_n_slots s 2) w]
      in
      fun w ->
        [%seqs
          assert_env_length_ w (int_ @@ (s.env_length - 2));
          grow_env_ ~comment:"op" w (int_ 2);
          compile_expr_no_move ctx s slot0 x0 x0_eval_k w]
  | Let (BOne (PVar (x, _), v, _), r, _) ->
      check_scope s;
      let slot, s = alloc_slot s in
      let r_eval_k =
       fun (l_r, s) w ->
        assert (equal_loc slot l_r);
        [%seqs
          set_loc dst (get_loc slot w) w;
          assert_env_length_ w (int_ s.env_length);
          shrink_env_ w (int_ 1);
          k (dst, drop s [ x ]) w]
      in
      let v_eval_k =
       fun (l_v, s) w ->
        let s = add_s s x l_v in
        compile_expr ctx s slot r r_eval_k w
      in
      fun w ->
        [%seqs
          assert_env_length_ w (int_ @@ (s.env_length - 1));
          grow_env_ ~comment:"let" w (int_ 1);
          compile_expr_no_move ctx s slot v v_eval_k w]
  | _ -> fun _w -> unit_

and compile_exprs f (ctx : ctx) (s : scope) (dsts : loc list) (cs : 'a expr list) (k : kont_with_locs) :
    world code -> unit code =
  let rec aux acc s cs dsts =
    match (cs, dsts) with
    | [], [] -> fun w -> k (List.rev acc, s) w
    | c :: cs, dst :: dsts ->
        let* l, s = f ctx s dst c in
        aux (l :: acc) s cs dsts
    | _ -> failwith [%string "compile_exprs: different number of expressions and destinations"]
  in
  aux [] s cs dsts

and compile_cases (ctx : ctx) (s : scope) (dst : loc) (scrut : 'a expr) (MatchPattern c : 'a cases) (k : kont_with_loc)
    : world code -> unit code =
  let a, s = alloc_slot s in
  let b, s = alloc_slot s in
  (* share cont: there is a bug *)
  let _match_k match_cont (_l, _s) w =
    assert (equal_loc b _l);
    [%seqs
      set_loc dst (get_loc b w) w;
      app_ match_cont unit_]
  in
  let match_k _ (_l, _s) w =
    assert (equal_loc b _l);
    [%seqs
      set_loc dst (get_loc b w) w;
      assert_env_length_ w (int_ s.env_length);
      shrink_env_ w (int_ 2);
      k (dst, free_n_slots s 2) w]
  in
  let scrut_k (_l, s) =
    assert (equal_loc a _l);
    reading s @@ fun s w ->
    [%seqs
      assert_env_length_ w (int_ s.env_length);
      let$ x = resolve_loc a w in
      (* let$ match_cont =
        paren @@ lam_unit_
        @@ fun _ ->
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          shrink_env_ w (int_ 2);
          k (dst, free_n_slots s 2) w]
      in *)
      let case cname =
        string (string_of_int @@ Hashtbl.find_exn ctx.ctag cname)
        ^^ space ^^ string "(*" ^^ space
        ^^ string (Hashtbl.find_exn ctx.ctag_name cname)
        ^^ space ^^ string "*)"
      in
      let t =
        List.map
          ~f:(fun (pat, expr) ->
            match pat with
            | PAny -> (string "_", compile_expr ctx s b expr (match_k ()) w)
            | PVar (x, _) -> (string x, compile_expr ctx (add_s s x a) b expr (match_k ()) w)
            | PCtorApp (cname, None, _) -> (case cname, compile_expr ctx s b expr (match_k ()) w)
            | PCtorApp (cname, Some (PVar (x0, _)), _) ->
                let c =
                  with_splits 1
                    (memo_splits_ @@ pair_value_ x)
                    (function
                      | [ x0_v ] ->
                          let slot, s = alloc_slot s in
                          [%seqs
                            assert_env_length_ w (int_ @@ (s.env_length - 1));
                            push_env_ w x0_v;
                            let clean_k =
                             fun (l, s) w ->
                              [%seqs
                                to_unit_ @@ pop_env_ w;
                                match_k () (l, s) w]
                            in
                            compile_expr ctx (add_s s x0 slot) b expr clean_k w]
                      | _ -> failwith "with_splits: unexpected arity")
                in
                (case cname, c)
            | PCtorApp (cname, Some (PTup (xs, _)), _) ->
                let xs = ensure_trivial_patterns xs in
                let n = List.length xs in
                let c =
                  with_splits n
                    (memo_splits_ @@ pair_value_ x)
                    (function
                      | ys when List.length ys = n ->
                          let slots, s = alloc_n_slots s n in
                          [%seqs
                            assert_env_length_ w (int_ @@ (s.env_length - n));
                            seqs_ (List.map ys ~f:(fun y -> fun _ -> [%seqs push_env_ w y]));
                            let clean_k =
                             fun (l, s) w ->
                              [%seqs
                                assert_env_length_ w (int_ s.env_length);
                                shrink_env_ w (int_ n);
                                match_k () (l, s) w]
                            in
                            let s = List.fold2_exn ~f:add_s ~init:s xs slots in
                            compile_expr ctx s b expr clean_k w]
                      | _ -> failwith "with_splits: unexpected arity")
                in
                (case cname, c)
            | _ -> failwith ("fv_pat: " ^ Syntax.string_of_document @@ Syntax.pp_pattern pat))
          c
      in
      let default_case = (string "_", unreachable_ (Dynarray.length codes)) in
      paren $ match_raw_ (word_get_value_ (zro_ x)) (List.append t [ default_case ])]
  in
  fun w ->
    [%seqs
      assert_env_length_ w (int_ @@ (s.env_length - 2));
      grow_env_ ~comment:"match" w (int_ 2);
      compile_expr ctx s a scrut scrut_k w]

let compile_stmt (ctx : ctx) (s : 'a stmt) : document =
  match s with
  | Type (TBOne (_, Enum { params = _; ctors }) as tb) ->
      register_constructors ctx ctors;
      CompileType.compile_ty_binding ctx.ctag tb
  | Type (TBRec trs as tb) ->
      List.iter trs ~f:(fun (_, Enum { params = _; ctors }) -> register_constructors ctx ctors);
      CompileType.compile_ty_binding ctx.ctag tb
  | Term (BOne (x, Lam (ps, term, _), _) | BRec [ (x, Lam (ps, term, _), _) ]) ->
      let params = ensure_trivial_patterns ps in
      let s = init_params (new_scope ()) params in
      let arg_num = s.env_length in
      let name = match x with PVar (x, _) -> x | _ -> failwith "unsupported pattern at top level term" in
      let cont_done_tag = ctor_tag_name ctx "cont_done" in
      add_code_k (fun entry_pc ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_pc;
          let r =
            ( lam_ "w" (fun w ->
                  let r, s = alloc_slot s in
                  [%seqs
                    assert_env_length_ w (int_ @@ (s.env_length - 1));
                    grow_env_ ~comment:"return value" w (int_ 1);
                    compile_expr ctx s r term (fun (l, s) w -> return_with s (get_loc l w) w) w]),
              entry_code entry_pc arg_num name cont_done_tag )
          in
          r)
  | _ -> failwith [%string "unsupported statement: %{Syntax.string_of_document @@ Syntax.pp_stmt s}"]

let generate_apply_cont ctx =
  set_code apply_cont
    (lam_ "w" (fun w ->
         (* We have to be careful: ctx.conts will grow as we apply the lambdas, 
          *   so we cannot do a single map over the whole array, 
          *   instead we have to take elements out on by one.
          *)
         let cont_codes = Dynarray.create () in
         let rec loop tl i =
           if Int.equal i @@ Dynarray.length ctx.conts then cont_codes
           else
             let name, action = Dynarray.get ctx.conts i in
             let code = (Hashtbl.find_exn ctx.ctag_name name, action w tl) in
             Dynarray.add_last cont_codes code;
             loop tl (i + 1)
         in
         [%seqs
           assert_env_length_ w (int_ 1);
           let hd, hd_pat = genvar "hd" in
           let tl, tl_pat = genvar "tl" in
           let pat = pair_pat_ hd_pat tl_pat in
           let_pat_in_ pat
             (resolve_ w (raw "K"))
             (paren
             $ match_ctor_tag_default_ (word_get_value_ hd)
                 (Dynarray.to_list (loop tl 0))
                 (unreachable_ (pc_to_int apply_cont)))]))

let generate_apply_cont_ ctx =
  set_code apply_cont
    (lam_ "w" (fun w ->
         [%seqs
           assert_env_length_ w (int_ 1);
           let hd, hd_pat = genvar "hd" in
           let tl, tl_pat = genvar "tl" in
           let pat = pair_pat_ hd_pat tl_pat in
           let_pat_in_ pat
             (resolve_ w (raw "K"))
             (paren
             $ match_ctor_tag_default_ (word_get_value_ hd)
                 (List.init (Dynarray.length ctx.conts) ~f:(fun i ->
                      let name, action = Dynarray.get ctx.conts i in
                      (Hashtbl.find_exn ctx.ctag_name name, action w tl)))
                 (unreachable_ (pc_to_int apply_cont)))]))

let ctor_tag_decls ctx =
  let xs = List.sort ~compare:(fun (_, x) (_, y) -> x - y) (Hashtbl.to_alist ctx.ctag) in
  separate_map (break 1)
    (fun (name, tag) ->
      let tag_name = get_ctor_tag_name name in
      string "let " ^^ string tag_name ^^ string " = " ^^ string (string_of_int tag))
    xs

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (compile_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Value"
  ^^ break 1 ^^ string "open Common" ^^ break 1 ^^ ctor_tag_decls ctx ^^ break 1 ^^ generated_stmt ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) ~f:(fun i ->
            string "let () = add_exp " ^^ uncode (Option.value_exn (Dynarray.get codes i)) ^^ space ^^ uncode (int_ i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) ~f:(fun i ->
            string "let () = Words.set_constructor_degree "
            ^^ uncode (int_ i)
            ^^ string " ("
            ^^ uncode (int_ (Dynarray.get ctx.constructor_degree i))
            ^^ string ")"))

module Backend = struct
  let compile prog =
    let prog = Tail.mark_tail_prog prog in
    let stmts, _ = Liveness.annotate_prog_with_liveness prog in
    pp_cek_ant stmts
end
