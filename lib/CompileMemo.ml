open Common
open PPrint
open Syntax
open Memo
open State
open Code

(* Backend overview moved to docs/internal.md#compilememo-backend. *)

(*todo: do not do a stack machine*)

(* Stack usage rationale is documented in docs/internal.md#compilememo-backend. *)

exception DupKey

module MakeMap (Ord : Stdlib.Map.OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  let add_exn x data t = if mem x t then raise DupKey else add x data t
end

module StrMap = MakeMap (String)

module Liveness = struct
  type stx_info = { fv : unit StrMap.t; tail : bool }

  let add_fv (v : string) (fv : unit StrMap.t) : unit StrMap.t = StrMap.add v () fv
  let remove_fv (v : string) (fv : unit StrMap.t) : unit StrMap.t = StrMap.remove v fv
  let fv_union (l : unit StrMap.t) (r : unit StrMap.t) : unit StrMap.t = StrMap.union (fun _ _ _ -> Some ()) l r
  let fv_unions (sets : unit StrMap.t list) : unit StrMap.t = List.fold_left fv_union StrMap.empty sets
  let stx_info_of_tag tail fv = { tail; fv }

  let rec annotate_pattern (pat : bool pattern) (fv_after : unit StrMap.t) : stx_info pattern * unit StrMap.t =
    match pat with
    | PVar (name, tag) ->
        let info = stx_info_of_tag tag fv_after in
        (PVar (name, info), remove_fv name fv_after)
    | PTup (patterns, tag) ->
        let patterns', fv_before = annotate_pattern_list patterns fv_after in
        (PTup (patterns', stx_info_of_tag tag fv_after), fv_before)
    | PCtorApp (ctor, payload, tag) -> (
        match payload with
        | None -> (PCtorApp (ctor, None, stx_info_of_tag tag fv_after), fv_after)
        | Some payload ->
            let payload', fv_before = annotate_pattern payload fv_after in
            (PCtorApp (ctor, Some payload', stx_info_of_tag tag fv_after), fv_before))
    | PAny | PInt _ | PBool _ | PUnit -> (pattern_tag_map (fun _ -> failwith "impossible") pat, fv_after)

  and annotate_pattern_list (patterns : bool pattern list) (fv_after : unit StrMap.t) :
      stx_info pattern list * unit StrMap.t =
    List.fold_right
      (fun pat (acc, fv_tail) ->
        let pat', fv_before = annotate_pattern pat fv_tail in
        (pat' :: acc, fv_before))
      patterns ([], fv_after)

  and annotate_expr_list (exprs : bool expr list) (fv_after : unit StrMap.t) : stx_info expr list * unit StrMap.t =
    List.fold_right
      (fun expr (acc, fv_tail) ->
        let expr', fv_before = annotate_expr expr fv_tail in
        (expr' :: acc, fv_before))
      exprs ([], fv_after)

  and annotate_cases (cases : bool cases) (fv_after : unit StrMap.t) : stx_info cases * unit StrMap.t =
    let (MatchPattern cs) = cases in
    let annotated, branch_reqs =
      List.fold_right
        (fun (pat, expr) (acc, reqs) ->
          let expr', fv_before_expr = annotate_expr expr fv_after in
          let pat', fv_before_pat = annotate_pattern pat fv_before_expr in
          ((pat', expr') :: acc, fv_before_pat :: reqs))
        cs ([], [])
    in
    (MatchPattern annotated, fv_unions branch_reqs)

  and annotate_binding_list (entries : (bool pattern * bool expr * bool) list) (fv_after : unit StrMap.t) :
      (stx_info pattern * stx_info expr * stx_info) list * unit StrMap.t =
    List.fold_right
      (fun (pat, expr, tag) (acc, fv_tail) ->
        let pat', fv_for_expr = annotate_pattern pat fv_tail in
        let expr', fv_before = annotate_expr expr fv_for_expr in
        let info = stx_info_of_tag tag fv_tail in
        ((pat', expr', info) :: acc, fv_before))
      entries ([], fv_after)

  and annotate_binding (binding : bool binding) (fv_after : unit StrMap.t) : stx_info binding * unit StrMap.t =
    match binding with
    | BSeq (expr, tag) ->
        let expr', fv_before = annotate_expr expr fv_after in
        (BSeq (expr', stx_info_of_tag tag fv_after), fv_before)
    | BOne (pat, expr, tag) ->
        let pat', fv_for_expr = annotate_pattern pat fv_after in
        let expr', fv_before = annotate_expr expr fv_for_expr in
        (BOne (pat', expr', stx_info_of_tag tag fv_after), fv_before)
    | BCont (pat, expr, tag) ->
        let pat', fv_for_expr = annotate_pattern pat fv_after in
        let expr', fv_before = annotate_expr expr fv_for_expr in
        (BCont (pat', expr', stx_info_of_tag tag fv_after), fv_before)
    | BRec entries ->
        let entries', fv_before = annotate_binding_list entries fv_after in
        (BRec entries', fv_before)
    | BRecC entries ->
        let entries', fv_before = annotate_binding_list entries fv_after in
        (BRecC entries', fv_before)

  and annotate_expr (expr : bool expr) (fv_after : unit StrMap.t) : stx_info expr * unit StrMap.t =
    match expr with
    | Unit | Int _ | Float _ | Bool _ | Str _ -> (expr_tag_map (fun _ -> failwith "impossible") expr, fv_after)
    | Builtin (b, tag) -> (Builtin (b, stx_info_of_tag tag fv_after), fv_after)
    | Var (name, tag) ->
        let info = stx_info_of_tag tag fv_after in
        (Var (name, info), add_fv name fv_after)
    | GVar (name, tag) -> (GVar (name, stx_info_of_tag tag fv_after), fv_after)
    | Ctor (name, tag) -> (Ctor (name, stx_info_of_tag tag fv_after), fv_after)
    | App (fn, args, tag) ->
        let args', fv_for_fn = annotate_expr_list args fv_after in
        let fn', fv_before = annotate_expr fn fv_for_fn in
        (App (fn', args', stx_info_of_tag tag fv_after), fv_before)
    | Op (op, lhs, rhs, tag) ->
        let rhs', fv_for_lhs = annotate_expr rhs fv_after in
        let lhs', fv_before = annotate_expr lhs fv_for_lhs in
        (Op (op, lhs', rhs', stx_info_of_tag tag fv_after), fv_before)
    | Tup (values, tag) ->
        let values', fv_before = annotate_expr_list values fv_after in
        (Tup (values', stx_info_of_tag tag fv_after), fv_before)
    | Arr (values, tag) ->
        let values', fv_before = annotate_expr_list values fv_after in
        (Arr (values', stx_info_of_tag tag fv_after), fv_before)
    | Lam (params, body, tag) ->
        let body', fv_body_entry = annotate_expr body StrMap.empty in
        let params', fv_closure = annotate_pattern_list params fv_body_entry in
        let fv_total = fv_union fv_after fv_closure in
        (Lam (params', body', stx_info_of_tag tag fv_total), fv_total)
    | Let (binding, body, tag) ->
        let body', fv_after_binding = annotate_expr body fv_after in
        let binding', fv_before = annotate_binding binding fv_after_binding in
        (Let (binding', body', stx_info_of_tag tag fv_after), fv_before)
    | Sel (target, field, tag) ->
        let target', fv_before = annotate_expr target fv_after in
        (Sel (target', field, stx_info_of_tag tag fv_after), fv_before)
    | If (cond, if_true, if_false, tag) ->
        let if_true', fv_true = annotate_expr if_true fv_after in
        let if_false', fv_false = annotate_expr if_false fv_after in
        let cond_req = fv_union fv_true fv_false in
        let cond', fv_before = annotate_expr cond cond_req in
        (If (cond', if_true', if_false', stx_info_of_tag tag fv_after), fv_before)
    | Match (cond, cases, tag) ->
        let cases', fv_cases = annotate_cases cases fv_after in
        let cond', fv_before = annotate_expr cond fv_cases in
        (Match (cond', cases', stx_info_of_tag tag fv_after), fv_before)

  let rec annotate_ty (ty : bool ty) : stx_info ty =
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
        let ctors =
          List.map (fun (name, tys, tag) -> (name, List.map annotate_ty tys, stx_info_of_tag tag StrMap.empty)) ctors
        in
        Enum { params; ctors }

  let annotate_ty_binding (binding : bool ty_binding) : stx_info ty_binding =
    match binding with
    | TBOne (name, kind) -> TBOne (name, annotate_ty_kind kind)
    | TBRec defs -> TBRec (List.map (fun (name, kind) -> (name, annotate_ty_kind kind)) defs)

  let rec annotate_stmt (stmt : bool stmt) (fv_after : unit StrMap.t) : stx_info stmt * unit StrMap.t =
    match stmt with
    | Type binding -> (Type (annotate_ty_binding binding), fv_after)
    | Term binding ->
        let binding', fv_before = annotate_binding binding fv_after in
        (Term binding', fv_before)

  let rec annotate_stmt_list (stmts : bool stmt list) (fv_after : unit StrMap.t) : stx_info stmt list * unit StrMap.t =
    match stmts with
    | [] -> ([], fv_after)
    | stmt :: rest ->
        let rest', fv_for_stmt = annotate_stmt_list rest fv_after in
        let stmt', fv_before = annotate_stmt stmt fv_for_stmt in
        (stmt' :: rest', fv_before)

  let annotate_prog_with_liveness ((stmts, prog_tag) : bool prog) : stx_info prog =
    let stmts', _ = annotate_stmt_list stmts StrMap.empty in
    (stmts', stx_info_of_tag prog_tag StrMap.empty)
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
  List.iter (fun (con_name, types, _) -> register_constructor e con_name types) ctors

let apply_cont : pc = add_code None

type env = int StrMap.t

let new_env () : env = StrMap.empty

type scope = {
  meta_env : int option StrMap.t;
  (* Note: env_length is not the amount of entries in meta_env above! 
   * It is the length of the environment when executing the cek machine.
   *)
  env_length : int;
  progressed : bool;
}

let check_scope s =
  let seen : bool Array.t = Array.init s.env_length (fun _ -> false) in
  StrMap.iter
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

let new_scope () = { meta_env = StrMap.empty; env_length = 0; progressed = false }
let push_s s = { s with env_length = s.env_length + 1; progressed = true }

let extend_s s name =
  check_scope s;
  let meta_env = s.meta_env in

  (* Hashtbl.add_exn meta_env ~key:name ~data:(Some s.env_length); *)
  let meta_env = StrMap.add_exn name (Some s.env_length) meta_env in

  let ret = { s with meta_env; env_length = s.env_length + 1 } in
  check_scope ret;
  ret

let drop_s s name =
  assert (Option.is_some (StrMap.find name s.meta_env));
  let meta_env = s.meta_env in
  (* Hashtbl.remove meta_env name; *)
  let meta_env = StrMap.remove name meta_env in
  { s with meta_env; env_length = s.env_length - 1 }

let pop_n s n =
  check_scope s;
  assert (s.env_length >= n);
  let ret = { s with meta_env = s.meta_env; env_length = s.env_length - n } in
  check_scope ret;
  ret

let pop_s s = pop_n s 1

type kont = scope -> world code -> unit code

let drop (s : scope) (vars : string list) (w : world code) (k : kont) : unit code =
  let new_s, n =
    List.fold_left
      (fun (s, n) var -> match StrMap.find var s.meta_env with None -> (s, n) | Some _ -> (drop_s s var, n + 1))
      (s, 0) vars
  in
  [%seqs
    assert_env_length_ w (int_ s.env_length);
    drop_n_ w (int_ s.env_length) (int_ n);
    k new_s w]

let return (s : scope) (w : world code) : unit code =
  [%seqs
    assert_env_length_ w (int_ s.env_length);
    return_n_ w (int_ s.env_length) (pc_to_exp_ (pc_ apply_cont))]

type keep_t = { mutable keep : bool; mutable source : string option }

let keep_only (s : scope) (fv : unit StrMap.t) : int Dynarray.t * scope =
  check_scope s;
  let keep : keep_t Dynarray.t = Dynarray.init s.env_length (fun _ -> { keep = true; source = None }) in
  StrMap.iter
    (fun key data -> match data with None -> () | Some i -> Dynarray.set keep i { keep = false; source = Some key })
    s.meta_env;
  StrMap.iter
    (fun v _ ->
      let i =
        match StrMap.find_opt v s.meta_env with Some (Some i) -> i | _ -> failwith ("keep_only not found:" ^ v)
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
            | Some v -> (i + 1, StrMap.add_exn v (Some (Dynarray.length keep_idx)) acc)
          in
          Dynarray.add_last keep_idx i;
          ret)
        else (i + 1, acc))
      (0, StrMap.empty) keep
  in
  let others = StrMap.map (fun _ -> None) s.meta_env in
  let meta_env = StrMap.union (fun _ x _ -> Some x) meta_env others in
  let s = { s with meta_env; env_length = Dynarray.length keep_idx } in
  check_scope s;
  (keep_idx, s)

let reading (s : scope) (f : scope -> world code -> unit code) (w : world code) : unit code =
  let make_code w = f { s with progressed = false } w in
  if s.progressed then goto_ w (add_code $ Some (lam_ "w" make_code)) else make_code w

let ( let* ) e f = e f

let rec compile_pp_expr (ctx : ctx) (s : scope) (c : 'a expr) (k : kont) : world code -> unit code =
  match c with
  | Var (name, _) ->
      let loc =
        match StrMap.find name s.meta_env with
        | Some loc -> loc
        | None -> failwith ("compile_pp_expr cannot find var: " ^ name)
      in
      fun w ->
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          push_env_ w (get_env_ w (int_ loc));
          k (push_s s) w]
  | Match (value, cases, _) ->
      let* s = compile_pp_expr ctx s value in
      reading s $ fun s w -> compile_pp_cases ctx s cases k w
  | Ctor (cname, _) ->
      fun w ->
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          push_env_ w (from_constructor_ (ctor_tag_name ctx cname));
          k (push_s s) w]
  | App (Ctor (cname, _), xs, _) ->
      let* s = compile_pp_exprs ctx s xs in
      fun w ->
        let length = List.length xs in
        let rec aux =
         fun n acc ->
          match n with
          | 0 ->
              [%seqs
                push_env_ w (memo_appends_ (from_constructor_ (ctor_tag_name ctx cname) :: acc));
                k (push_s (List.fold_left (fun s _ -> pop_s s) s (List.init length (fun i -> i)))) w]
          | n ->
              [%seqs
                let$ ctor_arg = pop_env_ w in
                aux (n - 1) (ctor_arg :: acc)]
        in
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          aux (List.length xs) []]
  | App (GVar (f, _), xs, info) ->
      let at_tail_pos = info.tail in
      check_scope s;
      let keep, keep_s = keep_only s info.fv in
      let keep_length = keep_s.env_length in
      let xs_length = List.length xs in
      if at_tail_pos then (
        assert (keep_length = 0);
        (* a tail call cannot keep anything *)
        let* s = compile_pp_exprs ctx s xs in
        fun w ->
          [%seqs
            assert_env_length_ w (int_ s.env_length);
            to_unit_ $ env_call_ w (list_literal_of_ int_ []) (int_ xs_length);
            goto_ w (Hashtbl.find_exn ctx.func_pc f)])
      else
        let cont_name = "cont_" ^ string_of_int ctx.conts_count in
        (* subtracting 1 to remove the arguments; adding 1 for the next continuation*)
        add_cont ctx cont_name (keep_length + 1) (fun w tl ->
            [%seqs
              set_k_ w (get_next_cont_ tl);
              restore_env_ w (int_ keep_length) tl;
              k (push_s keep_s) w]);
        let* s = compile_pp_exprs ctx s xs in
        fun w ->
          [%seqs
            assert_env_length_ w (int_ s.env_length);
            (let$ keep = env_call_ w (list_literal_of_ int_ (Dynarray.to_list keep)) (int_ xs_length) in
             set_k_ w (memo_appends_ [ from_constructor_ (ctor_tag_name ctx cont_name); keep; world_kont_ w ]));
            goto_ w (Hashtbl.find_exn ctx.func_pc f)]
  | If (cond, thn, els, _) ->
      let* s = compile_pp_expr ctx s cond in
      fun w ->
        let cond_name = gensym "cond" in
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          let_pat_in_ (var_pat_ cond_name)
            (resolve_ w (src_E_ (s.env_length - 1)))
            [%seqs
              to_unit_ $ pop_env_ w;
              let$ if_kont = paren (lam_unit_ (fun _ -> k s w)) in
              let k = fun _ _ -> app_ if_kont unit_ in
              let cond_bool = code $ parens (uncode (int_from_word_ (zro_ (var_ cond_name))) ^^ string " <> 0") in
              let then_branch = compile_pp_expr ctx (pop_s s) thn k in
              let else_branch = compile_pp_expr ctx (pop_s s) els k in
              if_ cond_bool (then_branch w) (else_branch w)]]
  | Op (op, x0, x1, _) ->
      let op_code =
        match op with
        | "+" -> add_
        | "<" -> lt_
        | "<=" -> le_
        | ">" -> gt_
        | ">=" -> ge_
        | _ -> failwith ("compile_pp_expr: unsupported op " ^ op)
      in
      let* s = compile_pp_expr ctx s x0 in
      let* s = compile_pp_expr ctx s x1 in
      reading s $ fun s w ->
      [%seqs
        assert_env_length_ w (int_ s.env_length);
        let$ x0 = resolve_ w (src_E_ (s.env_length - 2)) in
        let$ x1 = resolve_ w (src_E_ (s.env_length - 1)) in
        to_unit_ $ pop_env_ w;
        to_unit_ $ pop_env_ w;
        push_env_ w (memo_from_int_ (op_code (int_from_word_ (zro_ x0)) (int_from_word_ (zro_ x1))));
        k (push_s (pop_s (pop_s s))) w]
  | Int i ->
      fun w ->
        [%seqs
          assert_env_length_ w (int_ s.env_length);
          push_env_ w (memo_from_int_ (int_ i));
          k (push_s s) w]
  | Let (BOne (PVar (l, _), v, _), r, _) ->
      check_scope s;
      let* s = compile_pp_expr ctx s v in
      check_scope s;
      let* s = compile_pp_expr ctx (extend_s (pop_s s) l) r in
      fun w -> drop s [ l ] w k
  | _ -> failwith ("compile_pp_expr: " ^ Syntax.string_of_document @@ Syntax.pp_expr c)

and compile_pp_exprs (ctx : ctx) (s : scope) (cs : 'a expr list) (k : kont) : world code -> unit code =
  match cs with [] -> fun w -> k s w | c :: cs -> compile_pp_expr ctx s c (fun s w -> compile_pp_exprs ctx s cs k w)

and compile_pp_cases (ctx : ctx) (s : scope) (MatchPattern c : 'a cases) (k : kont) : world code -> unit code =
 fun w ->
  [%seqs
    assert_env_length_ w (int_ s.env_length);
    let$ last = src_E_ (s.env_length - 1) in
    let$ x = resolve_ w last in
    let s = pop_s s in
    let dummy = gensym "c" in
    let g cname =
      string dummy ^^ string " when " ^^ string dummy ^^ string " = " ^^ string (Hashtbl.find_exn ctx.ctag_name cname)
    in
    let t =
      Stdlib.List.map
        (fun (pat, expr) ->
          (*todo: special casing for now, as pat design need changes. *)
          match pat with
          | PCtorApp (cname, None, _) ->
              ( g cname,
                [%seqs
                  to_unit_ $ pop_env_ w;
                  compile_pp_expr ctx s expr k w] )
          | PCtorApp (cname, Some (PVar (x0, _)), _) ->
              ( g cname,
                with_splits 1
                  (memo_splits_ (pair_value_ x))
                  (function
                    | [ x0_v ] ->
                        [%seqs
                          to_unit_ $ pop_env_ w;
                          push_env_ w x0_v;
                          compile_pp_expr ctx (extend_s s x0) expr (fun s w -> drop s [ x0 ] w k) w]
                    | _ -> failwith "with_splits: unexpected arity") )
          | PCtorApp (cname, Some (PTup (xs, _)), _) when List.for_all (function PVar _ -> true | _ -> false) xs ->
              let xs = List.map (function PVar (name, _) -> name | _ -> failwith "impossible") xs in
              let n = List.length xs in
              ( g cname,
                with_splits n
                  (memo_splits_ (pair_value_ x))
                  (function
                    | ys when List.length ys == n ->
                        [%seqs
                          to_unit_ $ pop_env_ w;
                          seqs_ (List.map (fun y -> fun _ -> [%seqs push_env_ w y]) ys);
                          compile_pp_expr ctx
                            (List.fold_left (fun s x -> extend_s s x) s xs)
                            expr
                            (fun s w -> drop s (List.rev xs) w k)
                            w]
                    | _ -> failwith "with_splits: unexpected arity") )
          | PAny ->
              ( string "_",
                [%seqs
                  to_unit_ $ pop_env_ w;
                  compile_pp_expr ctx s expr k w] )
          | PVar (x_, _) ->
              ( string x_,
                [%seqs
                  (*note that we are not -1ing because s is already popped.*)
                  push_env_ w (get_env_ w (int_ s.env_length));
                  to_unit_ $ pop_env_ w;
                  compile_pp_expr ctx (extend_s s x_) expr (fun s w -> drop s [ x_ ] w k) w] )
          | _ -> failwith ("fv_pat: " ^ Syntax.string_of_document @@ Syntax.pp_pattern pat))
        c
    in
    let default_case = (string "_", unreachable_ (Dynarray.length codes)) in
    paren $ match_raw_ (word_get_value_ (zro_ x)) (List.append t [ default_case ])]

let compile_pp_stmt (ctx : ctx) (s : 'a stmt) : document =
  match s with
  | Type (TBOne (_, Enum { params = _; ctors }) as tb) ->
      register_constructors ctx ctors;
      CompileType.compile_ty_binding tb
  | Type (TBRec trs as tb) ->
      List.iter (fun (_, Enum { params = _; ctors }) -> register_constructors ctx ctors) trs;
      CompileType.compile_ty_binding tb
  | Term (BOne (x, Lam (ps, term, _), _) | BRec [ (x, Lam (ps, term, _), _) ]) ->
      let s =
        List.fold_left
          (fun s p ->
            match p with
            | PVar (n, _) -> extend_s s n
            | _ -> failwith ("fv_pat: " ^ Syntax.string_of_document @@ Syntax.pp_pattern p))
          (new_scope ()) ps
      in
      let arg_num = s.env_length in
      let name = match x with PVar (x, _) -> x | _ -> failwith "bad match" in
      let cont_done_tag = ctor_tag_name ctx "cont_done" in
      add_code_k (fun entry_code ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_code;
          let r =
            ( lam_ "w" (fun w -> compile_pp_expr ctx s term (fun s w -> return s w) w),
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
                 (List.init (Dynarray.length ctx.conts) (fun i ->
                      let name, action = Dynarray.get ctx.conts i in
                      (Hashtbl.find_exn ctx.ctag_name name, action w tl)))
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
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string "let () = add_exp " ^^ uncode (Option.get (Dynarray.get codes i)) ^^ space ^^ uncode (int_ i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string "let () = Words.set_constructor_degree "
            ^^ uncode (int_ i)
            ^^ string " ("
            ^^ uncode (int_ (Dynarray.get ctx.constructor_degree i))
            ^^ string ")"))

module Backend = struct
  let compile prog =
    let prog = Tail.mark_tail_prog prog in
    let stmts, _ = annotate_prog_with_liveness prog in
    pp_cek_ant stmts
end
