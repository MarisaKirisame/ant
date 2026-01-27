open Common
open Syntax

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
