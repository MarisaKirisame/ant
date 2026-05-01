open Syntax
open Fresh
module Ctx = Map.Make (String)
module SSet = Set.Make (String)
module Fresh = Fresh.Make ()

let ( let* ) a b = a b

(* NOTE: this file requires refactoring, given the AST has been typed *)

let free, free_p =
  let rec do_expr s = function
    | Unit _ | Int _ | Float _ | Bool _ | Str _ | Builtin _ -> s
    | Var (x, _) -> SSet.add x s
    | GVar (x, _) -> SSet.add x s
    | Ctor _ -> s
    | Lam (ps, e, _) -> SSet.diff (do_expr s e) (List.fold_left do_pattern SSet.empty ps)
    | App (f, xs, _) -> List.fold_left do_expr s (f :: xs)
    | Op (_, e1, e2, _) -> do_expr (do_expr s e1) e2
    | Tup (es, _) -> List.fold_left do_expr s es
    | Let (BOne (x, e1, _), e2, _) ->
        SSet.union (do_expr s e1) (SSet.diff (do_expr SSet.empty e2) (do_pattern SSet.empty x))
    | Let (BRec xs, e2, _) ->
        SSet.union
          (List.fold_left do_expr s (List.map (fun (_, e, _) -> e) xs))
          (SSet.diff (do_expr SSet.empty e2) (List.fold_left do_pattern SSet.empty (List.map (fun (p, _, _) -> p) xs)))
    | Let (BSeq (e1, _), e2, _) -> do_expr (do_expr s e1) e2
    | If (e1, e2, e3, _) -> do_expr (do_expr (do_expr s e1) e2) e3
    | Match (cond, MatchPattern (cases, _), _) ->
        List.fold_left
          (fun s (p, e) -> SSet.union s @@ SSet.diff (do_expr SSet.empty e) (do_pattern SSet.empty p))
          (do_expr s cond) cases
  and do_pattern s = function
    | PAny _ | PInt (_, _) | PBool (_, _) | PUnit _ -> s
    | PVar (x, _) -> SSet.add x s
    | PTup (ps, _) -> List.fold_left do_pattern s ps
    | PCtorApp (_, None, _) -> s
    | PCtorApp (_, Some p, _) -> do_pattern s p
  in
  ((fun e -> do_expr SSet.empty e), fun p -> do_pattern SSet.empty p)

let empty_info = SynInfo.empty_info

type defunc_ctx = string Ctx.t

let defunc ctx expr =
  (* this is too restrictive *)
  (* https://ocaml.org/manual/5.0/letrecvalues.html *)
  let allowed_rhs_for_let_rec expr = match expr with Lam _ -> true | _ -> false in
  let rec aux (ctx : defunc_ctx) (expr : 'a expr) : 'a expr * _ list =
    match expr with
    | Unit tag -> (Unit tag, [])
    | Int (i, tag) -> (Int (i, tag), [])
    | Float (f, tag) -> (Float (f, tag), [])
    | Bool (b, tag) -> (Bool (b, tag), [])
    | Str (s, tag) -> (Str (s, tag), [])
    | Builtin (b, i) -> (Builtin (b, i), [])
    | Var (x, i) -> ( match Ctx.find_opt x ctx with Some c -> (Ctor (c, i), []) | None -> (Var (x, i), []))
    | GVar (x, i) -> (GVar (x, i), [])
    | Ctor (x, i) -> (Ctor (x, i), [])
    | Op (op, e1, e2, i) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (Op (op, e1, e2, i), l1 @ l2)
    | Tup (es, i) ->
        let es, ls = List.split @@ List.map (aux ctx) es in
        (Tup (es, i), List.concat ls)
    | Let (BSeq (e1, i1), e2, i2) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (Let (BSeq (e1, i1), e2, i2), l1 @ l2)
    | Let (BOne (x, e1, i1), e2, i2) ->
        let x, e1, e2, l = de_single_binding ctx x e1 e2 i1 i2 in
        (Let (BOne (x, e1, i1), e2, i2), l)
    | Let (BRec xs, e2, i) ->
        let xs, ls = de_rec_bindings ctx xs in
        let e2, l = aux ctx e2 in
        (Let (BRec xs, e2, i), List.concat ls @ l)
    | If (e1, e2, e3, i) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        let e3, l3 = aux ctx e3 in
        (If (e1, e2, e3, i), l1 @ l2 @ l3)
    | Match (cond, MatchPattern (cases, info0), info1) ->
        let cond, l = aux ctx cond in
        let ls, cases =
          List.fold_left_map
            (fun acc (p, e) ->
              let e, l = aux ctx e in
              (l :: acc, (p, e)))
            [] cases
        in
        (Match (cond, MatchPattern (cases, info0), info1), l @ List.concat ls)
    | App _ -> de_app ctx expr
    | Lam _ -> de_lam ctx (Fresh.next_fresh "`C_'lam") expr
  and de_lam ctx ct (expr : 'a expr) =
    match expr with
    | Lam (ps, x, i) ->
        let fvs =
          let keys, _ = List.split @@ Ctx.bindings ctx in
          SSet.diff (free expr) @@ SSet.of_list keys
        in
        let sorted_fvl = List.map (fun x -> Var (x, i)) @@ SSet.elements fvs in
        let sorted_p_fvl = List.map (fun x -> PVar (x, i)) @@ SSet.elements fvs in
        let body, l = aux ctx x in
        let abs, case =
          if SSet.is_empty fvs then (Ctor (ct, i), PCtorApp (ct, None, i))
          else (App (Ctor (ct, i), sorted_fvl, i), PCtorApp (ct, Some (PTup (sorted_p_fvl, i)), i))
        in
        (abs, (PTup (case :: ps, i), body) :: l)
    | _ -> aux ctx expr
  and de_app ctx (expr : 'a expr) =
    match expr with
    | App (Ctor (x, i), xs, i') ->
        let xs, ls = List.split @@ List.map (aux ctx) xs in
        (* constructor application *) (App (Ctor (x, i), xs, i'), List.concat ls)
    | App (f, xs, i) ->
        let f, l1 = aux ctx f in
        let xs, l2 = List.split @@ List.map (aux ctx) xs in
        (App (Var ("_'defunc_apply", i), f :: xs, i), l1 @ List.concat l2)
    | _ -> aux ctx expr
  and gen_symbol binding =
    match binding with
    | PVar (x, _), Lam (_, _, _), _ -> (x, Fresh.next_fresh "`C_'lam")
    | _ -> failwith "Unsupported binding in gen_symbol"
  and de_single_binding ctx x e1 e2 i1 _i2 =
    match e1 with
    | Lam _ ->
        let xv, ct = gen_symbol (x, e1, i1) in
        let new_ctx = Ctx.add xv ct ctx in
        let e1, l1 = de_lam ctx ct e1 in
        let e2, l2 = aux new_ctx e2 in
        (x, e1, e2, l1 @ l2)
    | _ ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (x, e1, e2, l1 @ l2)
  and de_rec_bindings ctx bindings =
    assert (List.for_all (fun (_, e, _) -> allowed_rhs_for_let_rec e) bindings);
    let symbols = List.map gen_symbol bindings in
    let new_ctx = List.fold_left (fun ctx (x, y) -> Ctx.add x y ctx) ctx symbols in
    let ls, e =
      List.fold_left_map
        (fun acc ((x, e, i), (_, ct)) ->
          let e, l = de_lam new_ctx ct e in
          (l :: acc, (x, e, i)))
        [] (List.combine bindings symbols)
    in
    (e, ls)
  in
  aux ctx expr

let defunc_prog (prog : 'a prog) =
  let (_, cases), prog =
    List.fold_left_map
      (fun ((ctx, cases) as acc) item ->
        match item with
        | Type _ -> (acc, item)
        | Term (BOne (p, e, i)) ->
            let e, its = defunc ctx e in
            ((ctx, its @ cases), Term (BOne (p, e, i)))
        | Term (BRec xs) ->
            let e, ls = defunc ctx (Let (BRec xs, Tup ([], empty_info), empty_info)) in
            ((ctx, ls @ cases), Term (BOne (PUnit empty_info, e, empty_info)))
        | Term (BSeq (e, i)) ->
            let e, its = defunc ctx e in
            ((ctx, its @ cases), Term (BSeq (e, i)))
        | _ -> failwith "Unsupported item")
      (Ctx.empty, []) (fst prog)
  in
  let defunc_apply =
    Term
      (BOne
         ( PVar ("_'defunc_apply", empty_info),
           Let
             ( BRec
                 [
                   ( PVar ("_'defunc_apply", empty_info),
                     Lam
                       ( [ PVar ("_'f", empty_info); PVar ("_'a", empty_info) ],
                         Match
                           ( Tup ([ Var ("_'f", empty_info); Var ("_'a", empty_info) ], empty_info),
                             MatchPattern (cases, empty_info),
                             empty_info ),
                         empty_info ),
                     empty_info );
                 ],
               Var ("_'defunc_apply", empty_info),
               empty_info ),
           empty_info ))
  in
  (defunc_apply :: prog, empty_info)

let empty_info = SynInfo.empty_info

let mk_fresh_pat prefix =
  let name = Fresh.next_fresh prefix in
  (PVar (name, empty_info), Var (name, empty_info))

let rec anf_stmt stmt =
  match stmt with
  | Type _ -> stmt
  | Term binding -> Term (anf_binding binding)
  | _ -> failwith (Syntax.string_of_document @@ Syntax.pp_stmt stmt)

and anf_binding = function
  | BSeq (expr, info) -> BSeq (anf_entry expr, info)
  | BOne (pattern, expr, info) -> BOne (pattern, anf_entry expr, info)
  | BRec entries -> BRec (List.map (fun (pattern, expr, info) -> (pattern, anf_entry expr, info)) entries)

and anf_entry expr =
  match expr with
  | Lam (params, body, info) -> Lam (params, anf_tail body, info)
  | _ -> failwith ("unknown case in anf_entry: " ^ string_of_document @@ Syntax.pp_expr expr)

and is_simple = function Lam _ | Ctor _ | GVar _ | Int _ | Bool _ -> true | _ -> false

and anf_simple expr =
  match expr with
  | Lam (params, body, info) -> Lam (params, anf_tail body, info)
  | Ctor _ | GVar _ | Int _ | Bool _ -> expr
  | expr -> failwith ("unknown case in anf_simple: " ^ string_of_document @@ pp_expr expr)

and bind_value expr k =
  match expr with
  | Op (op, lhs, rhs, info) ->
      let* lhs' = bind_value lhs in
      let* rhs' = bind_value rhs in
      bind_computation (Op (op, lhs', rhs', info)) k
  | App (GVar (x, gv_info), args, app_info) ->
      let* args' = bind_values args in
      bind_computation (App (GVar (x, gv_info), args', app_info)) k
  | App (Ctor (x, ctor_info), args, app_info) ->
      let* args' = bind_values args in
      bind_computation (App (Ctor (x, ctor_info), args', app_info)) k
  | Var _ -> k expr
  | _ when is_simple expr -> bind_computation (anf_simple expr) k
  | _ -> failwith ("unknown case in bind_value: " ^ string_of_document @@ Syntax.pp_expr expr)

and bind_values exprs k =
  match exprs with
  | [] -> k []
  | expr :: rest ->
      let* expr' = bind_value expr in
      let* rest' = bind_values rest in
      k (expr' :: rest')

and bind_computation expr k =
  let binding_pat, binding_var = mk_fresh_pat "_" in
  Let (BOne (binding_pat, expr, empty_info), k binding_var, empty_info)

and anf_tail expr =
  match expr with
  | Var _ -> expr
  | _ when is_simple expr ->
      let* expr = bind_value expr in
      expr
  | App _ | Op _ ->
      let* expr = bind_value expr in
      expr
  | Match (scrutinee, MatchPattern (cases, info0), info1) ->
      let* scrutinee' = bind_value scrutinee in
      Match (scrutinee', MatchPattern (List.map (fun (pattern, body) -> (pattern, anf_tail body)) cases, info0), info1)
  | App (GVar (func_name, info), args, app_info) ->
      let* args = bind_values args in
      App (GVar (func_name, info), args, app_info)
  | Let (BOne (pattern, expr, info0), body, info1) ->
      (*this is not right. we have to collapse let of let, but the current implementation doesn't.
    we have to rewrite this in future *)
      let* expr = bind_value expr in
      Let (BOne (pattern, expr, info0), anf_tail body, info1)
  | If (cond, then_branch, else_branch, info) ->
      let* cond' = bind_value cond in
      If (cond', anf_tail then_branch, anf_tail else_branch, info)
  | _ -> failwith ("anf_tail not implemented: " ^ string_of_document @@ Syntax.pp_expr expr)

let anf_prog ((stmts, info) : 'a prog) : 'a prog =
  Fresh.reset ();
  (List.map anf_stmt stmts, info)
