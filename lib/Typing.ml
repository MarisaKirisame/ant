open! Core
open Type
open Syntax
open SynInfo

module ResolveGlobal = struct
  open Syntax
  open Core

  type env = { globals : (string, unit) Hashtbl.t; locals : String.Set.t }

  let rec collect_pattern_vars acc p =
    match p with
    | PVar (x, _) -> Set.add acc x
    | PTup (ps, _) -> List.fold ~init:acc ~f:collect_pattern_vars ps
    | PCtorApp (_, Some p, _) -> collect_pattern_vars acc p
    | PCtorApp (_, None, _) | PInt _ | PBool _ | PUnit | PAny -> acc

  let rec resolve_expr (env : env) (e : 'a expr) : 'a expr =
    let recurse = resolve_expr env in
    match e with
    | Var (n, info) -> (
        if Set.mem env.locals n then e
        else
          match Hashtbl.find env.globals n with
          | Some _ -> GVar (n, info) (* It is a global variable *)
          | None -> e (* Unknown variable, let the Typer catch the error *))
    | GVar _ | Ctor _ | Int _ | Float _ | Bool _ | Str _ | Unit | Builtin _ -> e
    | Lam (ps, body, info) ->
        let new_locals = List.fold ~init:env.locals ~f:collect_pattern_vars ps in
        let new_env = { env with locals = new_locals } in
        Lam (ps, resolve_expr new_env body, info)
    | App (f, xs, info) -> App (recurse f, List.map ~f:recurse xs, info)
    | Op (op, l, r, info) -> Op (op, recurse l, recurse r, info)
    | If (c, t, e, info) -> If (recurse c, recurse t, recurse e, info)
    | Tup (es, info) -> Tup (List.map ~f:recurse es, info)
    | Arr (es, info) -> Arr (List.map ~f:recurse es, info)
    | Let (BSeq (l, b_info), r, info) -> Let (BSeq (recurse l, b_info), recurse r, info)
    | Let (BOne (p, def, b_info), body, info) ->
        let resolved_def = recurse def in
        let new_locals = collect_pattern_vars env.locals p in
        let body_env = { env with locals = new_locals } in
        Let (BOne (p, resolved_def, b_info), resolve_expr body_env body, info)
    | Let (BRec bindings, body, info) ->
        let all_rec_locals = List.fold bindings ~init:env.locals ~f:(fun acc (p, _, _) -> collect_pattern_vars acc p) in
        let rec_env = { env with locals = all_rec_locals } in
        let resolved_bindings = List.map bindings ~f:(fun (p, def, b_info) -> (p, resolve_expr rec_env def, b_info)) in
        Let (BRec resolved_bindings, resolve_expr rec_env body, info)
    | Let ((BRecC _ | BCont _), _, _) -> failwith "Not supported"
    | Match (target, MatchPattern cases, info) ->
        let resolved_target = recurse target in
        let resolved_cases =
          List.map cases ~f:(fun (p, expr) ->
              let case_locals = collect_pattern_vars env.locals p in
              let case_env = { env with locals = case_locals } in
              (p, resolve_expr case_env expr))
        in
        Match (resolved_target, MatchPattern resolved_cases, info)
    | Sel (e, field, info) -> Sel (recurse e, field, info)
end

exception ElaborationError of string * int

let elab_error s = raise (ElaborationError (s, 0))
let debug = false

let update_ctx_nodup ctx ~(key : string) ~value =
  match Map.add ctx ~key ~data:value with
  | `Ok ctx -> ctx
  | `Duplicate -> elab_error [%string "update_ctx_nodup: duplicate definition: %{key}"]

let update_ctx_shadow ctx ~(key : string) ~value = Map.update ctx key ~f:(fun _ -> value)

let rec cycle_free = function
  | TVar { contents = Unbound _ } -> ()
  | TVar { contents = Link ty } -> cycle_free ty
  | (TArrow (_, _, ls) | TTup (_, ls) | TArr (_, ls) | TApp (_, _, ls)) when TyLevel.(ls.level_new = marker_level) ->
      elab_error "cycle_free: cycle detected"
  | TArrow (t1, t2, ls) ->
      let level = ls.level_new in
      ls.level_new <- TyLevel.marker_level;
      List.iter ~f:cycle_free t1;
      cycle_free t2;
      ls.level_new <- level
  | TPrim _ -> ()
  | TTup (ts, ls) | TArr (ts, ls) | TApp (_, ts, ls) ->
      let level = ls.level_new in
      ls.level_new <- TyLevel.marker_level;
      List.iter ~f:cycle_free ts;
      ls.level_new <- level

let to_be_level_adjusted = ref []
let reset_level_adjustment () = to_be_level_adjusted := []

let update_level l = function
  | TVar ({ contents = Unbound (n, l') } as tvr) ->
      assert (not TyLevel.(l' = generic_level));
      if TyLevel.(l < l') then tvr := Unbound (n, l)
  | TVar _ -> assert false
  | (TArrow (_, _, ls) | TTup (_, ls) | TArr (_, ls) | TApp (_, _, ls)) as ty ->
      if TyLevel.(ls.level_new = generic_level) then
        let s = Syntax.string_of_document @@ Type.pp_ty ty in
        elab_error [%string "failed to update level: generic level in %{s}"]
      else assert (not TyLevel.(ls.level_new = generic_level));
      if TyLevel.(ls.level_new = marker_level) then elab_error "update_level: cycle detected";
      if TyLevel.(l < ls.level_new) then (
        if TyLevel.(ls.level_new = ls.level_old) then to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.level_new <- l)
  | _ -> ()

let rec unify ty1 ty2 =
  if Core.phys_equal ty1 ty2 then ()
  else
    match (repr ty1, repr ty2) with
    | ( (TVar ({ contents = Unbound (_, l1) } as tv1) as t1),
        (* unify two free vars *)
        (TVar ({ contents = Unbound (_, l2) } as tv2) as t2) ) ->
        if Core.phys_equal tv1 tv2 then () (* the same variable *)
        else if
          (* bind the higher-level var *)
          TyLevel.(l2 < l1)
        then tv1 := Link t2
        else tv2 := Link t1
    | TVar ({ contents = Unbound (_, l) } as tv), t' | t', TVar ({ contents = Unbound (_, l) } as tv) ->
        update_level l t';
        tv := Link t'
    | TArrow (tyl1, tyl2, ll), TArrow (tyr1, tyr2, lr) ->
        if TyLevel.(ll.level_new = marker_level || lr.level_new = marker_level) then elab_error "unify: cycle detected";
        let min_level = TyLevel.min ll.level_new lr.level_new in
        ll.level_new <- TyLevel.marker_level;
        lr.level_new <- TyLevel.marker_level;
        if List.length tyl1 <> List.length tyr1 then elab_error "unify: the arity of arrows must be the same";
        List.iter2_exn ~f:(unify_lev min_level) tyl1 tyr1;
        unify_lev min_level tyl2 tyr2;
        ll.level_new <- min_level;
        lr.level_new <- min_level
    | TPrim t1, TPrim t2 when equal_pty t1 t2 -> ()
    | TTup (tys1, l1), TTup (tys2, l2) | TArr (tys1, l1), TArr (tys2, l2) ->
        if TyLevel.(l1.level_new = marker_level || l2.level_new = marker_level) then elab_error "unify: cycle detected";
        let min_level = TyLevel.min l1.level_new l2.level_new in
        l1.level_new <- TyLevel.marker_level;
        l2.level_new <- TyLevel.marker_level;
        if List.length tys1 <> List.length tys2 then elab_error "unify: the arity of tuples or arrays must be the same";
        List.iter2_exn ~f:(unify_lev min_level) tys1 tys2;
        l1.level_new <- min_level;
        l2.level_new <- min_level
    | TApp (name1, args1, l1), TApp (name2, args2, l2) ->
        if TyLevel.(l1.level_new = marker_level || l2.level_new = marker_level) then elab_error "unify: cycle detected";
        let min_level = TyLevel.min l1.level_new l2.level_new in
        l1.level_new <- TyLevel.marker_level;
        l2.level_new <- TyLevel.marker_level;
        if String.equal name1 name2 then () else elab_error [%string "unify: type mismatch: %{name1} and %{name2}"];
        if List.length args1 <> List.length args2 then
          elab_error "unify: the arity of type applications must be the same";
        List.iter2_exn ~f:(unify_lev min_level) args1 args2;
        l1.level_new <- min_level;
        l2.level_new <- min_level
    | _ ->
        let s1 = Syntax.string_of_document @@ Type.pp_ty ty1 in
        let s2 = Syntax.string_of_document @@ Type.pp_ty ty2 in
        elab_error [%string "unify: type mismatch: %{s1} and %{s2}"]

and unify_lev l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level l ty1;
  unify ty1 ty2

let force_delayed_adjustments () =
  let rec loop acc level ty =
    match repr ty with
    | TVar ({ contents = Unbound (name, l) } as tvr) when TyLevel.(level < l) ->
        tvr := Unbound (name, level);
        acc
    | (TArrow (_, _, ls) | TTup (_, ls) | TArr (_, ls) | TApp (_, _, ls)) when TyLevel.(ls.level_new = marker_level) ->
        elab_error "force_delayed_adjustments: cycle detected"
    | (TArrow (_, _, ls) | TTup (_, ls) | TArr (_, ls) | TApp (_, _, ls)) as ty ->
        if TyLevel.(level < ls.level_new) then ls.level_new <- level;
        adjust_one acc ty
    | _ -> acc
  (* only deals with composite types *)
  and adjust_one acc = function
    | (TArrow (_, _, ls) | TTup (_, ls) | TArr (_, ls) | TApp (_, _, ls)) as ty
      when TyLevel.(ls.level_old <= TyLevel.current_level ()) ->
        ty :: acc (* update later *)
    | (TArrow (_, _, ls) | TTup (_, ls) | TArr (_, ls) | TApp (_, _, ls)) when TyLevel.(ls.level_old = ls.level_new) ->
        acc (* already updated *)
    | TArrow (ty1, ty2, ls) ->
        let level = ls.level_new in
        ls.level_new <- TyLevel.marker_level;
        let acc = List.fold_left ~init:acc ~f:(fun acc ty -> loop acc level ty @ acc) ty1 in
        let acc = loop acc level ty2 in
        ls.level_new <- level;
        ls.level_old <- level;
        acc
    | TTup (tys1, ls) | TArr (tys1, ls) | TApp (_, tys1, ls) ->
        let level = ls.level_new in
        ls.level_new <- TyLevel.marker_level;
        let acc = List.fold_left ~init:acc ~f:(fun acc ty -> loop acc level ty @ acc) tys1 in
        ls.level_new <- level;
        ls.level_old <- level;
        acc
    | _ -> assert false
  in
  to_be_level_adjusted := List.fold_left ~f:adjust_one ~init:[] !to_be_level_adjusted

let generalize ty =
  force_delayed_adjustments ();
  let rec loop ty =
    match repr ty with
    | TVar ({ contents = Unbound (name, l) } as tvr) when TyLevel.(current_level () < l) ->
        tvr := Unbound (name, TyLevel.generic_level)
    | TArrow (ty1, ty2, ls) when TyLevel.(current_level () < ls.level_new) ->
        let ty1 = List.map ~f:repr ty1 in
        let ty2 = repr ty2 in
        List.iter ~f:loop ty1;
        loop ty2;
        let l = List.fold_left ~init:(get_level ty2) ~f:(fun ml ty -> TyLevel.max ml (get_level ty)) ty1 in
        ls.level_old <- l;
        ls.level_new <- l (* set the exact level upper bound *)
    | (TApp (_, tys, ls) | TTup (tys, ls) | TArr (tys, ls)) when TyLevel.(current_level () < ls.level_new) ->
        let tys = List.map ~f:repr tys in
        List.iter ~f:loop tys;
        let l = List.fold_left ~init:TyLevel.ground_level ~f:(fun ml ty -> TyLevel.max ml (get_level ty)) tys in
        ls.level_old <- l;
        ls.level_new <- l (* set the exact level upper bound *)
    | _ -> ()
  in
  if debug then
    print_endline [%string "generalize: before %{Syntax.string_of_document @@ Type.pp_ty ~print_level:true ty}"];
  loop ty;
  if debug then
    print_endline [%string "generalize: after %{Syntax.string_of_document @@ Type.pp_ty ~print_level:true ty}"]

let instantiate ty =
  let fold_aux loop subst args =
    let args_rev, subst' =
      List.fold_left
        ~f:(fun (tys, subst) ty ->
          let ty, subst = loop subst ty in
          (ty :: tys, subst))
        ~init:([], subst) args
    in
    (List.rev args_rev, subst')
  in
  let rec loop subst = function
    | TVar { contents = Unbound (name, l) } when TyLevel.(l = generic_level) -> (
        match List.Assoc.find ~equal:String.equal subst name with
        | Some ty -> (ty, subst)
        | None ->
            let tv = new_tvar () in
            (tv, (name, tv) :: subst))
    | TVar { contents = Link ty } -> loop subst ty
    | TArrow (ty1, ty2, ls) when TyLevel.(ls.level_new = generic_level) ->
        let ty1, subst = fold_aux loop subst ty1 in
        let ty2, subst = loop subst ty2 in
        (new_arrow ty1 ty2, subst)
    | TApp (name, tys, ls) when TyLevel.(ls.level_new = generic_level) ->
        let tys, subst = fold_aux loop subst tys in
        (new_app name tys, subst)
    | TTup (tys, ls) when TyLevel.(ls.level_new = generic_level) ->
        let tys, subst = fold_aux loop subst tys in
        (new_tup tys, subst)
    | TArr (tys, ls) when TyLevel.(ls.level_new = generic_level) ->
        let tys, subst = fold_aux loop subst tys in
        (new_arr tys, subst)
    | ty' -> (ty', subst)
  in
  if debug then
    print_endline [%string "instantiate: before %{Syntax.string_of_document @@ Type.pp_ty ~print_level:true ty}"];
  let ty' = fst (loop [] ty) in
  if debug then
    print_endline [%string "instantiate: after %{Syntax.string_of_document @@ Type.pp_ty ~print_level:true ty'}"];
  ty'

let type_of_builtin builtin =
  match builtin with
  | "print_endline" -> new_arrow [ TPrim Str ] (TPrim Unit)
  | "print_string" -> new_arrow [ TPrim Str ] (TPrim Unit)
  | _ -> elab_error "type_of_builtin: unknown builtin"

(* currently assume all operators are polymorphic *)
let type_of_op op =
  match op with
  | "+" | "-" | "*" | "/" | "%" ->
      let t = new_tvar () in
      new_arrow [ t; t ] t
  | "<" | "<=" | ">" | ">=" | "==" | "!=" ->
      let t = new_tvar () in
      new_arrow [ t; t ] (TPrim Bool)
  | _ -> elab_error [%string "type_of_op: unknown op: %{op}"]

let rec type_of_pattern (ctx : Type.ty StrMap.t) (p : 'a pattern) : 'a pattern * Type.ty =
  match p with
  | PAny -> (p, new_tvar ())
  | PInt _ -> (p, TPrim Int)
  | PBool _ -> (p, TPrim Bool)
  | PVar (x, info) ->
      let tv = new_tvar () in
      (PVar (x, { info with ty = Some tv }), tv)
  | PUnit -> (p, TPrim Unit)
  | PTup (ps, info) ->
      let ps, ty_ps = List.unzip @@ List.map ~f:(type_of_pattern ctx) ps in
      let ty = new_tup ty_ps in
      (PTup (ps, { info with ty = Some ty }), ty)
  | PCtorApp (c, None, info) -> (
      match Map.find ctx c with
      | Some ty ->
          let ty = instantiate ty in
          (PCtorApp (c, None, { info with ty = Some ty }), ty)
      | None -> elab_error [%string "Constructor not found: %{c}"])
  | PCtorApp (c, Some (PTup (args, info')), info) -> (
      match Map.find ctx c with
      | Some ty ->
          let ty = instantiate ty in
          let args, ty_args = List.unzip @@ List.map ~f:(type_of_pattern ctx) args in
          let tyr = new_tvar () in
          unify ty (new_arrow ty_args tyr);
          (PCtorApp (c, Some (PTup (args, info')), { info with ty = Some tyr }), tyr)
      | None -> elab_error [%string "Constructor not found: %{c}"])
  | PCtorApp (c, Some p, info) -> (
      match Map.find ctx c with
      | Some ty ->
          let ty = instantiate ty in
          let p, ty_p = type_of_pattern ctx p in
          let tyr = new_tvar () in
          unify ty (new_arrow [ ty_p ] tyr);
          (PCtorApp (c, Some p, { info with ty = Some tyr }), tyr)
      | None -> elab_error [%string "Constructor not found: %{c}"])

type binder_kind = Nothing | Trivial | NonTrivial

let rec bind_pattern_variables_nodup ctx p =
  let rec loop ctx = function
    | PAny -> ctx
    | PInt _ -> ctx
    | PBool _ -> ctx
    | PUnit -> ctx
    | PVar (x, { ty = Some t; _ }) -> update_ctx_nodup ctx ~key:x ~value:t
    | PVar (x, _) -> elab_error [%string "Cannot infer the type of variable: %{x}"]
    | PTup (ps, _) -> List.fold_left ~init:ctx ~f:loop ps
    | PCtorApp (_, None, _) -> ctx
    | PCtorApp (_, Some p, _) -> loop ctx p
  in
  loop ctx p

let rec bind_pattern_variables_shadow ctx p =
  (* NOTE: We still need ensure no duplicate bindings inside the same pattern. *)
  let dup = Hashtbl.create (module String) in
  let rec loop ctx = function
    | PAny -> ctx
    | PInt _ -> ctx
    | PBool _ -> ctx
    | PUnit -> ctx
    | PVar (x, { ty = Some t; _ }) -> (
        match Hashtbl.add dup ~key:x ~data:() with
        | `Ok -> update_ctx_shadow ctx ~key:x ~value:t
        | `Duplicate -> elab_error [%string "Variable %{x} already bound"])
    | PVar (x, _) -> elab_error [%string "Cannot infer the type of variable: %{x}"]
    | PTup (ps, _) -> List.fold_left ~init:ctx ~f:loop ps
    | PCtorApp (_, None, _) -> ctx
    | PCtorApp (_, Some p, _) -> loop ctx p
  in
  loop ctx p

let rec generalize_pattern_variables p =
  match p with
  | PAny -> ()
  | PInt _ -> ()
  | PBool _ -> ()
  | PUnit -> ()
  | PVar (_, { ty = Some t; _ }) -> generalize t
  | PVar (x, _) -> elab_error [%string "Cannot infer the type of variable: %{x}"]
  | PTup (ps, _) -> List.iter ~f:(fun p -> generalize_pattern_variables p) ps
  | PCtorApp (_, None, _) -> ()
  | PCtorApp (_, Some p, _) -> generalize_pattern_variables p

let rec type_of (ctx : Type.ty StrMap.t) (e : info expr) : info expr * Type.ty =
  try
    match e with
    | Unit -> (e, TPrim Unit)
    | Int _ -> (e, TPrim Int)
    | Float _ -> (e, TPrim Float)
    | Bool _ -> (e, TPrim Bool)
    | Str _ -> (e, TPrim Str)
    | Builtin (Builtin name, info) ->
        let ty = type_of_builtin name in
        let ty = instantiate ty in
        (Builtin (Builtin name, { info with ty = Some ty }), type_of_builtin name)
    | Var (x, info) -> (
        match Map.find ctx x with
        | None -> elab_error [%string "Variable not found: %{x}"]
        | Some ty ->
            let ty' = instantiate ty in
            (Var (x, { info with ty = Some ty' }), ty'))
    | GVar (x, info) -> (
        match Map.find ctx x with
        | None -> elab_error [%string "Global definition not found: %{x}"]
        | Some ty ->
            let ty' = instantiate ty in
            (GVar (x, { info with ty = Some ty' }), ty'))
    | Ctor (x, info) -> (
        match Map.find ctx x with
        | None -> elab_error [%string "Constructor not found: %{x}"]
        | Some ty ->
            let ty' = instantiate ty in
            (Ctor (x, { info with ty = Some ty' }), ty'))
    | App (f, xs, info) ->
        let f, tyf = type_of ctx f in
        let args, ty_args = List.unzip @@ List.map ~f:(type_of ctx) xs in
        let tyr = new_tvar () in
        unify tyf (new_arrow ty_args tyr);
        (App (f, args, { info with ty = Some tyr }), tyr)
    | If (c, t, f, info) ->
        let c, tyc = type_of ctx c in
        let t, tyt = type_of ctx t in
        let f, tyf = type_of ctx f in
        unify tyc (TPrim Bool);
        unify tyt tyf;
        (If (c, t, f, { info with ty = Some tyt }), tyt)
    | Op (op, l, r, info) ->
        let tyop = type_of_op op in
        let l, tyl = type_of ctx l in
        let r, tyr = type_of ctx r in
        let ty = new_tvar () in
        unify tyop (new_arrow [ tyl; tyr ] ty);
        (Op (op, l, r, { info with ty = Some ty }), ty)
    | Let (BSeq (e1, info), e2, info') ->
        let e1, ty1 = type_of ctx e1 in
        unify ty1 (TPrim Unit);
        let e2, ty2 = type_of ctx e2 in
        (Let (BSeq (e1, { info with ty = Some ty1 }), e2, { info' with ty = Some ty2 }), ty2)
    | Let (BOne (p, e1, info), e2, info') ->
        TyLevel.enter ();
        let e1, ty1 = type_of ctx e1 in
        cycle_free ty1;
        let p, ty_p = type_of_pattern ctx p in
        unify ty_p ty1;
        TyLevel.leave ();
        generalize_pattern_variables p;
        let e2, ty2 = type_of (bind_pattern_variables_shadow ctx p) e2 in
        (Let (BOne (p, e1, { info with ty = Some ty1 }), e2, { info' with ty = Some ty2 }), ty2)
    | Let (BRec xs, e2, info') ->
        TyLevel.enter ();
        let xs =
          List.map xs ~f:(fun (p, e, info) ->
              let p, ty_p = type_of_pattern ctx p in
              (p, ty_p, e, info))
        in
        let rec_ctx = List.fold_left xs ~init:ctx ~f:(fun ctx (p, _, _, _) -> bind_pattern_variables_shadow ctx p) in
        let ys =
          List.map xs ~f:(fun (p, ty_p, e, info) ->
              let e, ty_e = type_of rec_ctx e in
              unify ty_p ty_e;
              (p, e, { info with ty = Some ty_p }, ty_p))
        in
        TyLevel.leave ();
        List.iter ys ~f:(fun (p, _, _, _) -> generalize_pattern_variables p);
        List.iter ys ~f:(fun (_, _, _, ty) -> cycle_free ty);
        let body_ctx = List.fold_left ys ~init:ctx ~f:(fun ctx (p, _, _, _) -> bind_pattern_variables_shadow ctx p) in
        let final = List.map ys ~f:(fun (p, e, info, _) -> (p, e, info)) in
        let e2, ty2 = type_of body_ctx e2 in
        (Let (BRec final, e2, { info' with ty = Some ty2 }), ty2)
    | Let (_, _, _) -> failwith "unknown let binding"
    | Tup (es, info) ->
        let es, ty_es = List.unzip @@ List.map ~f:(type_of ctx) es in
        let ty = new_tup ty_es in
        (Tup (es, { info with ty = Some ty }), ty)
    | Arr (es, info) ->
        let es, ty_es = List.unzip @@ List.map ~f:(type_of ctx) es in
        let ty = new_arr ty_es in
        (Arr (es, { info with ty = Some ty }), ty)
    | Lam (ps, e, info) ->
        let ps, ty_ps = List.unzip @@ List.map ~f:(type_of_pattern ctx) ps in
        let ctx = List.fold_left ~init:ctx ~f:(fun ctx p -> bind_pattern_variables_shadow ctx p) ps in
        let e, ty_e = type_of ctx e in
        let ty = new_arrow ty_ps ty_e in
        (Lam (ps, e, { info with ty = Some ty }), ty)
    | Sel (_e, FIndex _i, _info) -> failwith "not implemented"
    | Sel (_e, FName _x, _info) -> failwith "not implemented"
    | Match (_e, MatchPattern [], _info) -> failwith "todo: bottom type"
    | Match (e, MatchPattern cases, info) ->
        let type_of_case (p, e) =
          let p, ty_p = type_of_pattern ctx p in
          let ctx = bind_pattern_variables_shadow ctx p in
          let e, ty_e = type_of ctx e in
          ((p, e), ty_p, ty_e)
        in
        let e, ty_e = type_of ctx e in
        let cases, ty_cases, ty_exprs = List.unzip3 @@ List.map ~f:type_of_case cases in
        List.iter ~f:(fun ty_case -> unify ty_case ty_e) ty_cases;
        let tyr = List.hd_exn ty_exprs in
        List.iter ~f:(fun ty_expr -> unify ty_expr tyr) (List.tl_exn ty_exprs);
        (Match (e, MatchPattern cases, { info with ty = Some tyr }), tyr)
  with ElaborationError (msg, l) ->
    let s = Syntax.string_of_document @@ Syntax.pp_expr e in
    print_endline [%string "!!!!!! Type error %{msg} in expression:\n%{s}\n"];
    if l < 4 then raise (ElaborationError (msg, l + 1)) else failwith "Maximum reporting depth reached"

let top_type_of_prog (p : info prog) : info prog =
  let rec convert_ty (ctx : Type.ty StrMap.t) (arity : int StrMap.t) (ty : 'a Syntax.ty) : Type.ty =
    match ty with
    | TUnit -> Type.(TPrim Unit)
    | TInt -> Type.(TPrim Int)
    | TFloat -> Type.(TPrim Float)
    | TBool -> Type.(TPrim Bool)
    | TNamed f -> (
        match Map.find arity f with
        | Some 0 -> Type.new_app f []
        | Some x -> failwith [%string "Type %{f} requires %{string_of_int x} arguments, but you provided 0"]
        | None -> failwith [%string "Type %{f} not defined"])
    | TApply (TNamed f, xs) -> (
        match Map.find arity f with
        | Some x when x = List.length xs -> Type.new_app f (List.map ~f:(convert_ty ctx arity) xs)
        | Some x ->
            failwith
              [%string
                "Type %{f} requires %{string_of_int x} arguments, but you provided %{string_of_int (List.length xs)}"]
        | None -> failwith [%string "Type %{f} not defined"])
    | TApply (_, _) ->
        let s = Syntax.string_of_document @@ Syntax.pp_ty ty in
        failwith [%string "Type application has invalid syntax: %{s}"]
    | TArrow (ty1, ty2) ->
        let rec flatten acc ty = match ty with TArrow (t1, t2) -> flatten (t1 :: acc) t2 | _ -> (List.rev acc, ty) in
        let args, ret = flatten [ ty1 ] ty2 in
        Type.new_arrow (List.map ~f:(convert_ty ctx arity) args) (convert_ty ctx arity ret)
    | TTuple tys -> Type.(new_tup (List.map ~f:(convert_ty ctx arity) tys))
    | TNamedVar x ->
        if Map.mem ctx x then Map.find_exn ctx x
        else failwith [%string "Type variable not found: %{x}. Ensure it is defined in the type declaration"]
  in
  let open Type in
  let reset () =
    reset_level_adjustment ();
    TyLevel.reset ();
    TyFresh.reset ()
  in
  let print_ctx ctx =
    Map.iteri
      ~f:(fun ~key ~data ->
        print_endline [%string "ctx: %{key} -> %{Syntax.string_of_document @@ Type.pp_ty ~print_level:true data}"])
      ctx;
    print_endline "--------------------------------"
  in
  let decl_ctor ctx arity ename cname xs ys info =
    TyLevel.enter ();

    let ty_params = List.map ~f:(fun _ -> new_tvar ()) ys in
    let tyr = new_app ename ty_params in
    let ctx =
      List.fold2_exn ~init:ctx
        ~f:(fun ctx param ty_param -> update_ctx_nodup ctx ~key:param ~value:ty_param)
        ys ty_params
    in
    let ty_args = List.map ~f:(convert_ty ctx arity) xs in
    let ctor_ty = match ty_args with [] -> tyr | _ -> new_arrow ty_args tyr in

    TyLevel.leave ();

    generalize ctor_ty;
    (cname, ctor_ty, { info with ty = Some ctor_ty })
  in
  let decl_type ctx arity = function
    | TBOne (name, Enum { params; ctors }) ->
        let arity = update_ctx_nodup arity ~key:name ~value:(List.length params) in
        let ty_ctors =
          List.map ~f:(fun (cname, cargs, info) -> decl_ctor ctx arity name cname cargs params info) ctors
        in
        let ctx, infos =
          List.fold_map ~init:ctx
            ~f:(fun ctx (name, ty, info) -> (update_ctx_nodup ctx ~key:name ~value:ty, info))
            ty_ctors
        in
        let ctors = List.map2_exn ~f:(fun (name, sty, _) info -> (name, sty, info)) ctors infos in
        (TBOne (name, Enum { params; ctors }), ctx, arity)
    | TBRec rec_groups ->
        let arity =
          List.fold ~init:arity
            ~f:(fun arity (name, Enum { params; ctors = _ }) ->
              update_ctx_nodup arity ~key:name ~value:(List.length params))
            rec_groups
        in
        let ctx, new_rec_groups =
          List.fold_map ~init:ctx rec_groups ~f:(fun ctx (name, Enum { params; ctors }) ->
              let ty_ctors =
                List.map ~f:(fun (cname, cargs, info) -> decl_ctor ctx arity name cname cargs params info) ctors
              in
              let ctx, infos =
                List.fold_map ~init:ctx ty_ctors ~f:(fun ctx (name, ty, info) ->
                    (update_ctx_nodup ctx ~key:name ~value:ty, info))
              in
              let ctors = List.map2_exn ~f:(fun (name, sty, _) info -> (name, sty, info)) ctors infos in
              (ctx, (name, Enum { params; ctors })))
        in
        (TBRec new_rec_groups, ctx, arity)
  in
  let resolve_ctx = Hashtbl.create (module String) in
  let infer_top_level (ctx : ty StrMap.t) (arity : int StrMap.t) stmt =
    let stmt_resolved =
      match stmt with
      | Term (BOne (p, e, info)) ->
          let env = { ResolveGlobal.globals = resolve_ctx; ResolveGlobal.locals = String.Set.empty } in
          let e_resolved = ResolveGlobal.resolve_expr env e in

          let vars = ResolveGlobal.collect_pattern_vars String.Set.empty p in
          Set.iter vars ~f:(fun v -> Hashtbl.set resolve_ctx ~key:v ~data:());

          Term (BOne (p, e_resolved, info))
      | Term (BRec bindings) ->
          List.iter bindings ~f:(fun (p, _, _) ->
              let vars = ResolveGlobal.collect_pattern_vars String.Set.empty p in
              Set.iter vars ~f:(fun v -> Hashtbl.set resolve_ctx ~key:v ~data:()));

          let env = { ResolveGlobal.globals = resolve_ctx; ResolveGlobal.locals = String.Set.empty } in
          let bindings_resolved =
            List.map bindings ~f:(fun (p, e, info) -> (p, ResolveGlobal.resolve_expr env e, info))
          in
          Term (BRec bindings_resolved)
      | Term (BSeq (e, info)) ->
          let env = { ResolveGlobal.globals = resolve_ctx; ResolveGlobal.locals = String.Set.empty } in
          Term (BSeq (ResolveGlobal.resolve_expr env e, info))
      | Type _ -> stmt
      | _ -> failwith "Unknown top term binding in resolve"
    in
    match stmt_resolved with
    | Type tb ->
        let tb, ctx, arity = decl_type ctx arity tb in
        (Type tb, ctx, arity)
    | Term (BSeq (e, info)) ->
        let e, ty = type_of ctx e in
        cycle_free ty;
        (Term (BSeq (e, { info with ty = Some ty })), ctx, arity)
    | Term (BOne (p, e, info)) ->
        TyLevel.enter ();
        let e, ty_e = type_of ctx e in
        let p, ty_p = type_of_pattern ctx p in
        unify ty_p ty_e;
        TyLevel.leave ();
        generalize_pattern_variables p;
        cycle_free ty_p;
        (Term (BOne (p, e, { info with ty = Some ty_p })), bind_pattern_variables_shadow ctx p, arity)
    | Term (BRec xs) ->
        TyLevel.enter ();
        let ys =
          List.map xs ~f:(fun (p, e, info) ->
              let p, ty_p = type_of_pattern ctx p in
              (p, ty_p, e, info))
        in
        let ctx_rec = List.fold ys ~init:ctx ~f:(fun ctx (p, _, _, _) -> bind_pattern_variables_shadow ctx p) in
        let checked =
          List.map ys ~f:(fun (p, ty_p, e, info) ->
              let e, ty_e = type_of ctx_rec e in
              unify ty_p ty_e;
              (p, e, { info with ty = Some ty_p }))
        in
        TyLevel.leave ();
        List.iter checked ~f:(fun (p, _, _) -> generalize_pattern_variables p);
        List.iter checked ~f:(fun (_, _, info) -> cycle_free @@ Option.value_exn info.ty);
        let ctx_final = List.fold checked ~init:ctx ~f:(fun ctx (p, _, _) -> bind_pattern_variables_shadow ctx p) in
        (Term (BRec checked), ctx_final, arity)
    | _ -> failwith "Unknown top term binding"
  in
  let _ctx, _arity, stmts =
    List.fold_left ~init:(StrMap.empty, StrMap.empty, [])
      ~f:(fun (ctx, arity, stmts) stmt ->
        reset ();
        let stmt, ctx, arity = infer_top_level ctx arity stmt in
        if debug then print_ctx ctx;
        (ctx, arity, stmt :: stmts))
      (fst p)
  in
  (List.rev stmts, { (snd p) with ty = Some (TPrim Unit) })

open PPrint

let pp_top_type_of_prog ?(print_level = false) (p : info prog) : PPrint.document =
  let pp_ty = Type.pp_ty ~print_level in
  let f stmt =
    match stmt with
    | Type (TBOne (_, Enum { params = _; ctors })) ->
        separate_map (break 1)
          (fun (name, _, info) ->
            parens
              (star ^^ space ^^ string name ^^ colon ^^ space
              ^^ Option.value_map ~f:pp_ty ~default:empty info.ty
              ^^ space ^^ star))
          ctors
        ^^ break 1 ^^ Syntax.pp_stmt stmt
    | Type (TBRec _) -> Syntax.pp_stmt stmt
    | Term (BSeq (_, info)) ->
        parens (star ^^ space ^^ pp_ty (Option.value_exn info.ty) ^^ space ^^ star) ^^ break 1 ^^ Syntax.pp_stmt stmt
    | Term (BOne (p, _, info)) ->
        parens (star ^^ space ^^ pp_pattern p ^^ space ^^ pp_ty (Option.value_exn info.ty) ^^ space ^^ star)
        ^^ break 1 ^^ Syntax.pp_stmt stmt
    | Term (BRec xs) ->
        parens
          (star ^^ space
          ^^ separate_map
               (space ^^ string "and" ^^ space)
               (fun (p, _, info) -> pp_pattern p ^^ space ^^ pp_ty (Option.value_exn info.ty))
               xs
          ^^ space ^^ star)
        ^^ break 1 ^^ Syntax.pp_stmt stmt
    | _ -> failwith "Unknown top term binding"
  in
  separate_map (break 1) f (fst p)
