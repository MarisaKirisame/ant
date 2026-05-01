open Common
open PPrint
open Syntax
open Memo
open State
open Code
module Hashtbl = AntHashtbl

(* todo: update
Stack usage rationale is documented in docs/internal.md#compilememo-backend. *)

exception DupKey

module MakeMap (Ord : Stdlib.Map.OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  let add_exn x data t = if mem x t then raise DupKey else add x data t
end

module StrMap = MakeMap (String)

let string_of_strmap_keys (map : 'a StrMap.t) : string =
  let keys = StrMap.bindings map |> List.map fst in
  Printf.sprintf "[%s]" (String.concat "; " keys)

module Liveness = struct
  type fv_t = unit StrMap.t

  let empty_fv : fv_t = StrMap.empty
  let add_fv (fv : fv_t) (v : string) : fv_t = StrMap.add v () fv
  let remove_fv (fv : fv_t) (v : string) : fv_t = StrMap.remove v fv
  let fv_union (l : fv_t) (r : fv_t) : fv_t = StrMap.union (fun _ _ _ -> Some ()) l r
  let fv_unions (sets : fv_t list) : fv_t = List.fold_left fv_union StrMap.empty sets
  let fv_diff (l : fv_t) (r : fv_t) : fv_t = StrMap.filter (fun k _ -> not (StrMap.mem k r)) l
  let fv_t_of_expr (expr : fv_t option expr) : fv_t = Option.get (expr_tag expr)
  let fv_t_of_cases (cases : fv_t option cases) : fv_t = Option.get (cases_tag cases)
  let fv_t_of_pattern (pat : fv_t option pattern) : fv_t = Option.get (pattern_tag pat)

  let rec annotate_pattern (pat : _ pattern) : fv_t option pattern =
    match pat with
    | PCtorApp (ctor, None, _) -> PCtorApp (ctor, None, Some empty_fv)
    | PCtorApp (ctor, Some p, _) ->
        let p = annotate_pattern p in
        PCtorApp (ctor, Some p, Some (fv_t_of_pattern p))
    | PTup (patterns, _) ->
        let patterns = List.map annotate_pattern patterns in
        PTup (patterns, Some (fv_unions (List.map fv_t_of_pattern patterns)))
    | PVar (name, _) -> PVar (name, Some (add_fv empty_fv name))
    | PAny _ -> PAny (Some empty_fv)
    | _ -> failwith ("annotate_pattern:" ^ Syntax.string_of_document @@ Syntax.pp_pattern pat)

  let rec annotate_cases (cases : _ cases) fv : fv_t option cases =
    let (MatchPattern (cs, _)) = cases in
    let annotated = List.map (fun (pat, expr) -> (annotate_pattern pat, annotate_expr expr fv)) cs in
    let branch_reqs = List.map (fun (pat, expr) -> fv_diff (fv_t_of_expr expr) (fv_t_of_pattern pat)) annotated in
    MatchPattern (annotated, Some (fv_unions branch_reqs))

  and annotate_expr (expr : _ expr) fv : fv_t option expr =
    match expr with
    | Match (cond, cases, _) ->
        let cases = annotate_cases cases fv in
        let cond = annotate_expr cond (fv_t_of_cases cases) in
        Match (cond, cases, Some (fv_t_of_expr cond))
    | Builtin _ | Unit _ | Int _ | Float _ | Bool _ | Str _ | GVar _ | Ctor _ -> expr_tag_map (fun _ -> Some fv) expr
    | Let (BOne (PVar (name, _), value, _), body, _) ->
        let body = annotate_expr body fv in
        let value = annotate_expr value (remove_fv (fv_t_of_expr body) name) in
        Let (BOne (PVar (name, None), value, None), body, Some (fv_t_of_expr value))
    | App (fn, args, _) ->
        let args, fv = annotate_exprs args fv in
        let fn = annotate_expr fn fv in
        App (fn, args, Some (fv_t_of_expr fn))
    | Var (name, _) -> Var (name, Some (add_fv fv name))
    | Op (op, l, r, _) ->
        let r = annotate_expr r fv in
        let l = annotate_expr l (fv_t_of_expr r) in
        Op (op, l, r, Some (fv_t_of_expr l))
    | If (cond, thn, els, _) ->
        let els = annotate_expr els fv in
        let thn = annotate_expr thn fv in
        let cond = annotate_expr cond (fv_union (fv_t_of_expr thn) (fv_t_of_expr els)) in
        If (cond, thn, els, Some (fv_t_of_expr cond))
    | _ -> failwith ("annotate_expr: " ^ Syntax.string_of_document @@ Syntax.pp_expr expr)

  and annotate_exprs (exprs : _ expr list) fv : _ expr list * fv_t =
    match exprs with
    | [] -> ([], fv)
    | e :: es ->
        let es, fv = annotate_exprs es fv in
        let e = annotate_expr e fv in
        (e :: es, fv_t_of_expr e)

  let rec annotate_ty (ty : _ ty) : fv_t option ty =
    match ty with
    | TUnit -> TUnit
    | TInt -> TInt
    | TFloat -> TFloat
    | TBool -> TBool
    | TApply (a, b) -> TApply (annotate_ty a, List.map annotate_ty b)
    | TArrow (a, b) -> TArrow (annotate_ty a, annotate_ty b)
    | TTuple tys -> TTuple (List.map annotate_ty tys)
    | TNamed name -> TNamed name
    | TNamedVar name -> TNamedVar name

  let annotate_ty_kind = function
    | Enum { params; ctors } ->
        let ctors = List.map (fun (name, tys, tag) -> (name, List.map annotate_ty tys, None)) ctors in
        Enum { params; ctors }

  let annotate_ty_binding (binding : _ ty_binding) : fv_t option ty_binding =
    match binding with
    | TBOne (name, kind) -> TBOne (name, annotate_ty_kind kind)
    | TBRec defs -> TBRec (List.map (fun (name, kind) -> (name, annotate_ty_kind kind)) defs)

  let rec annotate_stmt (stmt : _ stmt) : fv_t option stmt =
    match stmt with
    | Type binding -> Type (annotate_ty_binding binding)
    | Term (BRec [ (x, Lam (ps, term, _), _) ]) ->
        Term
          (BRec [ (annotate_pattern x, Lam (List.map annotate_pattern ps, annotate_expr term empty_fv, None), None) ])
    | Term (BOne (x, Lam (ps, term, _), _)) ->
        Term (BOne (annotate_pattern x, Lam (List.map annotate_pattern ps, annotate_expr term empty_fv, None), None))
    | _ -> failwith ("annotate_stmt: " ^ Syntax.string_of_document @@ Syntax.pp_stmt stmt)

  let annotate_stmt_list (stmts : _ stmt list) : fv_t option stmt list = List.map (fun stmt -> annotate_stmt stmt) stmts
  let annotate_prog_with_liveness ((stmts, _) : _ prog) : fv_t option prog = (annotate_stmt_list stmts, None)
end

open Liveness

type ctx = {
  arity : (string, int) Hashtbl.t;
  ctag : (string, int) Hashtbl.t;
  ctag_name : (string, string) Hashtbl.t;
  constructor_degree : int Dynarray.t;
  conts : (string * (world code -> words code -> unit code)) Dynarray.t;
  mutable conts_count : int;
  func_pc : (string, pc) Hashtbl.t;
}

let get_ctor_tag_name (name : string) : string = "tag_" ^ name

let add_cont (ctx : ctx) (name : string) (arity : int) (app : world code -> words code -> unit code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Hashtbl.add_exn ctx.ctag_name ~key:name ~data:(get_ctor_tag_name name);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app);
  ctx.conts_count <- ctx.conts_count + 1

let ctor_tag_name (ctx : ctx) (cname : string) : int code = raw (Hashtbl.find_exn ctx.ctag_name cname)

let new_ctx () : ctx =
  let ctx =
    {
      arity = Hashtbl.create ();
      ctag = Hashtbl.create ();
      ctag_name = Hashtbl.create ();
      constructor_degree = Dynarray.create ();
      conts = Dynarray.create ();
      conts_count = 0;
      func_pc = Hashtbl.create ();
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
  List.iter (fun (con_name, types, _) -> register_constructor e con_name types) ctors

let apply_cont : pc = add_code None

type location = int
type env = { values : location StrMap.t; env_length : int; changed : bool; w : world code }
type kont = env -> unit code

let new_env w = { values = StrMap.empty; env_length = 0; changed = false; w }

let check_env env =
  StrMap.iter
    (fun _ loc -> if loc < 0 || loc >= env.env_length then failwith ("Invalid location: " ^ string_of_int loc))
    env.values

let fresh_loc env =
  let used = Array.make env.env_length false in
  StrMap.iter (fun _ loc -> used.(loc) <- true) env.values;
  let rec find_loc i = if i >= env.env_length then None else if not used.(i) then Some i else find_loc (i + 1) in
  find_loc 0

let extend_env env name =
  check_env env;
  let env =
    match fresh_loc env with
    | Some loc -> { env with values = StrMap.add_exn name loc env.values; changed = true }
    | None ->
        {
          env with
          values = StrMap.add_exn name env.env_length env.values;
          env_length = env.env_length + 1;
          changed = true;
        }
  in
  check_env env;
  env

let bind env x loc : env = { env with values = StrMap.add_exn x loc env.values }

let return loc : kont =
 fun env ->
  [%seqs
    assert_env_length_ env.w (int_ env.env_length);
    return_n_ env.w (int_ loc) (pc_to_exp_ (pc_ apply_cont))]

type keep_t = { mutable keep : bool; mutable source : string option }

let keep_only (env : env) (fv : fv_t) : int Dynarray.t * env =
  let keep : string list Dynarray.t = Dynarray.init env.env_length (fun _ -> []) in
  StrMap.iter
    (fun v _ ->
      let i = match StrMap.find_opt v env.values with Some i -> i | _ -> failwith ("keep_only not found:" ^ v) in
      Dynarray.set keep i (v :: Dynarray.get keep i))
    fv;
  let keep_idx : int Dynarray.t = Dynarray.create () in
  let values = ref StrMap.empty in
  Dynarray.iteri
    (fun i vs ->
      if not (List.is_empty vs) then (
        List.iter (fun v -> values := StrMap.add_exn v (Dynarray.length keep_idx) !values) vs;
        Dynarray.add_last keep_idx i))
    keep;
  let env = { env with values = !values; env_length = Dynarray.length keep_idx } in
  (keep_idx, env)

let reading env (f : env -> unit code) : unit code =
  let make_code w = f { env with changed = false; w } in
  if env.changed then goto_ env.w (add_code $ Some (lam_ "w" make_code)) else f env

let shrink (env : env) (fv : fv_t) : env =
  let values = StrMap.filter (fun k _ -> StrMap.mem k fv) env.values in
  { env with values; changed = true }

let assign env name value (f : env -> unit code) : unit code =
  match fresh_loc env with
  | Some loc ->
      [%seqs
        set_env_ env.w (int_ loc) value;
        f { env with values = StrMap.add_exn name loc env.values; changed = true }]
  | None ->
      [%seqs
        push_env_ env.w value;
        f
          {
            env with
            values = StrMap.add_exn name env.env_length env.values;
            env_length = env.env_length + 1;
            changed = true;
          }]

let ( let* ) e f = e f

let rec assigns env bindings (f : env -> unit code) : unit code =
  match bindings with
  | [] -> f env
  | (name, value) :: rest ->
      let* env = assign env name value in
      assigns env rest f

let allocate env name value fv (f : env -> unit code) : unit code =
  let env = shrink env fv in
  assign env name value f

let allocates env bindings fv (f : env -> unit code) : unit code =
  let env = shrink env fv in
  assigns env bindings f

let debug = false

let rec compile_pp_expr (ctx : ctx) (env : env) (c : fv_t option expr) (k : location -> kont) : unit code =
  if debug then
    Printf.printf "compile_pp_expr: compiling %s (with fv %s) with env %s\n"
      (Syntax.string_of_document @@ Syntax.pp_expr c)
      (string_of_strmap_keys (fv_t_of_expr c))
      (string_of_strmap_keys env.values);
  StrMap.iter
    (fun k _ ->
      if not (StrMap.mem k env.values) then
        failwith ("compile_pp_expr: unbound variable " ^ k ^ " in " ^ Syntax.string_of_document @@ Syntax.pp_expr c))
    (fv_t_of_expr c);
  check_env env;
  let loc_from_var env x =
    match x with Var (v, _) -> StrMap.find v env.values | _ -> failwith "argument not anf-ed"
  in
  match c with
  | Var (name, _) ->
      let loc = StrMap.find name env.values in
      k loc env
  | Match (value, cases, _) ->
      let* loc = compile_pp_expr ctx env value in
      fun env -> compile_pp_cases ctx env cases loc k
  | If (cond, thn, els, _) ->
      reading env $ fun env ->
      let cond = loc_from_var env cond in
      let cond_name = gensym "cond" in
      [%seqs
        assert_env_length_ env.w (int_ env.env_length);
        let_pat_in_ (var_pat_ cond_name)
          (resolve_ env.w (src_E_ cond))
          (let cond_bool = code $ parens (uncode (int_from_word_ (zro_ (var_ cond_name))) ^^ string " <> 0") in
           let then_branch = compile_pp_expr ctx env thn k in
           let else_branch = compile_pp_expr ctx env els k in
           if_ cond_bool then_branch else_branch)]
  | Let (BOne (PVar (l, _), v, _), r, _) -> (
      let simple env value = allocate env l value (fv_t_of_expr r) (fun env -> compile_pp_expr ctx env r k) in
      match v with
      | Var (v, _) ->
          let env = bind env l (StrMap.find v env.values) in
          compile_pp_expr ctx env r k
      | Ctor (cname, _) -> simple env (from_constructor_ (ctor_tag_name ctx cname))
      | Int (i, _) -> simple env (memo_from_int_ (int_ i))
      | Bool (b, _) -> simple env (memo_from_int_ (int_ (if b then 1 else 0)))
      | App (Ctor (cname, _), xs, _) ->
          let xs = List.map (fun x -> get_env_ env.w (int_ (loc_from_var env x))) xs in
          simple env (memo_appends_ (from_constructor_ (ctor_tag_name ctx cname) :: xs))
      | Op (op, x0, x1, _) ->
          let op_code =
            match op with
            | "+" -> add_
            | "*" -> mul_
            | "/" -> div_
            | "-" -> sub_
            | "=" -> eq_
            | "<" -> lt_
            | "<=" -> le_
            | ">" -> gt_
            | ">=" -> ge_
            | "&&" -> land_
            | "||" -> lor_
            | _ -> failwith ("compile_pp_expr: unsupported op " ^ op)
          in
          reading env $ fun env ->
          let x0 = loc_from_var env x0 in
          let x1 = loc_from_var env x1 in
          [%seqs
            assert_env_length_ env.w (int_ env.env_length);
            let$ x0 = resolve_ env.w (src_E_ x0) in
            let$ x1 = resolve_ env.w (src_E_ x1) in
            simple env (memo_from_int_ (op_code (int_from_word_ (zro_ x0)) (int_from_word_ (zro_ x1))))]
      | App (GVar (f, _), xs, _) ->
          let xs = List.map (loc_from_var env) xs in
          let keep, keep_env = keep_only env (remove_fv (fv_t_of_expr r) l) in
          let keep_length = keep_env.env_length in
          let xs_length = List.length xs in
          let cont_name = "cont_" ^ string_of_int ctx.conts_count in
          add_cont ctx cont_name (keep_length + 1) (fun w tl ->
              [%seqs
                set_k_ w (get_next_cont_ tl);
                restore_env_ w (int_ keep_length) tl;
                let env =
                  { keep_env with values = StrMap.add l keep_length keep_env.values; env_length = keep_length + 1; w }
                in
                compile_pp_expr ctx env r k]);
          [%seqs
            assert_env_length_ env.w (int_ env.env_length);
            (let$ keep = env_call_ env.w (list_literal_of_ int_ (Dynarray.to_list keep)) (list_literal_of_ int_ xs) in
             set_k_ env.w (memo_appends_ [ from_constructor_ (ctor_tag_name ctx cont_name); keep; world_kont_ env.w ]));
            goto_ env.w (Hashtbl.find_exn ctx.func_pc f)]
      | _ -> failwith ("compile_pp_expr(let): " ^ Syntax.string_of_document @@ Syntax.pp_expr c))
  | App (GVar (f, _), xs, info) ->
      let xs = List.map (loc_from_var env) xs in
      [%seqs
        assert_env_length_ env.w (int_ env.env_length);
        to_unit_ $ env_call_ env.w (list_literal_of_ int_ []) (list_literal_of_ int_ xs);
        goto_ env.w (Hashtbl.find_exn ctx.func_pc f)]
  | _ -> failwith ("compile_pp_expr: " ^ Syntax.string_of_document @@ Syntax.pp_expr c)

and compile_pp_cases (ctx : ctx) (env : env) (MatchPattern (c, _) : 'a cases) loc (k : location -> kont) : unit code =
  [%seqs
    assert_env_length_ env.w (int_ env.env_length);
    let* env = reading env in
    [%seqs
      let$ x = resolve_ env.w (src_E_ loc) in
      let case cname =
        string (string_of_int @@ Hashtbl.find_exn ctx.ctag cname)
        ^^ space ^^ string "(*" ^^ space
        ^^ string (Hashtbl.find_exn ctx.ctag_name cname)
        ^^ space ^^ string "*)"
      in
      let t =
        Stdlib.List.map
          (fun (pat, expr) ->
            (*todo: special casing for now, as pat design need changes. *)
            match pat with
            | PCtorApp (cname, None, _) -> (case cname, compile_pp_expr ctx env expr k)
            | PCtorApp (cname, Some (PTup (xs, _)), _)
              when List.for_all (function PVar _ | PAny _ -> true | _ -> false) xs ->
                let xs =
                  List.map (function PVar (name, _) -> name | PAny _ -> gensym "_" | _ -> failwith "impossible") xs
                in
                let n = List.length xs in
                ( case cname,
                  with_splits n
                    (memo_splits_ (pair_value_ x))
                    (function
                      | ys when List.length ys == n ->
                          let* env = allocates env (List.combine xs ys) (fv_t_of_expr expr) in
                          compile_pp_expr ctx env expr k
                      | _ -> failwith "with_splits: unexpected arity") )
            | PAny _ -> (string "_", compile_pp_expr ctx env expr k)
            | PVar (x_, _) ->
                ( string x_,
                  let env = bind env x_ loc in
                  compile_pp_expr ctx env expr k )
            (*
          | PCtorApp (cname, Some (PVar (x0, _)), _) ->
              ( case cname,
                with_splits 1
                  (memo_splits_ (pair_value_ x))
                  (function
                    | [ x0_v ] ->
                        [%seqs
                          to_unit_ $ pop_env_ w;
                          push_env_ w x0_v;
                          compile_pp_expr ctx (extend_s s x0) expr (fun s w -> drop s [ x0 ] w k) w]
                    | _ -> failwith "with_splits: unexpected arity") )
          *)
            | _ -> failwith ("compile_pp_cases: " ^ Syntax.string_of_document @@ Syntax.pp_pattern pat))
          c
      in
      let default_case = (string "_", unreachable_ (Dynarray.length codes)) in
      paren $ match_raw_ (word_get_value_ (zro_ x)) (List.append t [ default_case ])]]

let compile_pp_stmt (ctx : ctx) (s : 'a stmt) : document =
  match s with
  | Type (TBOne (_, Enum { params = _; ctors }) as tb) ->
      register_constructors ctx ctors;
      CompileType.compile_ty_binding ctx.ctag tb
  | Type (TBRec trs as tb) ->
      List.iter (fun (_, Enum { params = _; ctors }) -> register_constructors ctx ctors) trs;
      CompileType.compile_ty_binding ctx.ctag tb
  | Term (BOne (x, Lam (ps, term, _), _) | BRec [ (x, Lam (ps, term, _), _) ]) ->
      let arg_num = List.length ps in
      let name = match x with PVar (x, _) -> x | _ -> failwith "bad match" in
      let cont_done_tag = ctor_tag_name ctx "cont_done" in
      add_code_k (fun entry_code ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_code;
          let r =
            ( lam_ "w" (fun w ->
                  let env =
                    List.fold_left
                      (fun env p ->
                        match p with
                        | PVar (n, _) -> extend_env env n
                        | _ -> failwith ("fv_pat: " ^ Syntax.string_of_document @@ Syntax.pp_pattern p))
                      (new_env w) ps
                  in
                  let env = { env with changed = false } in
                  compile_pp_expr ctx env term return),
              string "let rec" ^^ space ^^ string name ^^ space ^^ string "memo" ^^ space
              ^^ separate space (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
              ^^ string ": exec_result " ^^ string "=" ^^ space ^^ group @@ string "(exec_cek "
              ^^ string ("(pc_to_exp (int_to_pc " ^ string_of_int (pc_to_int entry_code) ^ "))")
              ^^ string "(Dynarray.of_list" ^^ string "["
              ^^ separate (string ";") (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ ")")))
              ^^ string "]" ^^ string ")" ^^ string "("
              ^^ uncode (from_constructor_ cont_done_tag)
              ^^ string ")" ^^ string " memo)" )
          in
          r)
  | _ -> failwith (Syntax.string_of_document @@ Syntax.pp_stmt s)

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
             let code = (Hashtbl.find_exn ctx.ctag name, Hashtbl.find_exn ctx.ctag_name name, action w tl) in
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
             $ match_ctor_tag_literal_default_ (word_get_value_ hd)
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
             $ match_ctor_tag_literal_default_ (word_get_value_ hd)
                 (List.init (Dynarray.length ctx.conts) (fun i ->
                      let name, action = Dynarray.get ctx.conts i in
                      (Hashtbl.find_exn ctx.ctag name, Hashtbl.find_exn ctx.ctag_name name, action w tl)))
                 (unreachable_ (pc_to_int apply_cont)))]))

let ctor_tag_decls ctx =
  let xs = List.sort (fun (_, x) (_, y) -> x - y) (Hashtbl.to_alist ctx.ctag) in
  separate_map (break 1)
    (fun (name, tag) ->
      let tag_name = get_ctor_tag_name name in
      string "let " ^^ string tag_name ^^ string " = " ^^ string (string_of_int tag))
    xs

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (compile_pp_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Value"
  ^^ break 1 ^^ string "open Common" ^^ break 1 ^^ ctor_tag_decls ctx ^^ break 1 ^^ generated_stmt ^^ break 1
  ^^ string "let populate_state () =" ^^ break 1 ^^ string "  Memo.reset ();" ^^ break 1 ^^ string "  Words.reset ();"
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string "add_exp " ^^ uncode (Option.get (Dynarray.get codes i)) ^^ space ^^ uncode (int_ i) ^^ semi))
  ^^ break 1
  ^^ separate
       (semi ^^ break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string "Words.set_constructor_degree "
            ^^ uncode (int_ i)
            ^^ string " ("
            ^^ uncode (int_ (Dynarray.get ctx.constructor_degree i))
            ^^ string ")"))
  ^^ string ";;"

module Backend = struct
  let compile prog =
    let stmts, _ = annotate_prog_with_liveness prog in
    pp_cek_ant stmts
end
