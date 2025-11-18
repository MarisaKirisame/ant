open Syntax
open Fresh
module Ctx = Map.Make (String)
module SSet = Set.Make (String)
module Fresh = Fresh.Make ()

let free, free_p =
  let rec do_expr s = function
    | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ -> s
    | Var (x, _) -> SSet.add x s
    | GVar (x, _) -> SSet.add x s
    | Ctor _ -> s
    | Lam (ps, e, _) -> SSet.diff (do_expr s e) (List.fold_left do_pattern SSet.empty ps)
    | App (f, xs, _) -> List.fold_left do_expr s (f :: xs)
    | Op (_, e1, e2, _) -> do_expr (do_expr s e1) e2
    | Tup (es, _) | Arr (es, _) -> List.fold_left do_expr s es
    | Let ((BOne (x, e1) | BCont (x, e1)), e2, _) ->
        SSet.union (do_expr s e1) (SSet.diff (do_expr SSet.empty e2) (do_pattern SSet.empty x))
    | Let ((BRec xs | BRecC xs), e2, _) ->
        SSet.union
          (List.fold_left do_expr s (List.map snd xs))
          (SSet.diff (do_expr SSet.empty e2) (List.fold_left do_pattern SSet.empty (List.map fst xs)))
    | Let (BSeq e1, e2, _) -> do_expr (do_expr s e1) e2
    | Sel (e, _, _) -> do_expr s e
    | If (e1, e2, e3, _) -> do_expr (do_expr (do_expr s e1) e2) e3
    | Match (cond, MatchPattern cases, _) ->
        List.fold_left
          (fun s (p, e) -> SSet.union s @@ SSet.diff (do_expr SSet.empty e) (do_pattern SSet.empty p))
          (do_expr s cond) cases
  and do_pattern s = function
    | PAny | PInt _ | PBool _ -> s
    | PVar (x, _) -> SSet.add x s
    | PUnit -> s
    | PTup (ps, _) -> List.fold_left do_pattern s ps
    | PCtorApp (_, None, _) -> s
    | PCtorApp (_, Some p, _) -> do_pattern s p
  in
  (fun e -> do_expr SSet.empty e), (fun p -> do_pattern SSet.empty p)
(* Commented out because we cannot confirm the 'a of expr now
type cps_ctx = int Ctx.t

let cps ctx expr =
  let mk_fresh prefix =
    let k = Fresh.next_fresh prefix in
    (PVar k, Var k)
  in
  let mk_fresh_params n prefix =
    let rec aux acc acc2 n =
      if n = 0 then (acc, acc2)
      else
        let x = Fresh.next_fresh prefix in
        aux (PVar x :: acc) (Var x :: acc2) (n - 1)
    in
    let l1, l2 = aux [] [] n in
    (List.rev l1, List.rev l2)
  in
  let is_atomic = function
    | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | Ctor _ | Lam _ -> true
    | _ -> false
  in
  let ( let* ) e f = e f in
  let rec atom = function
    | Unit -> Unit
    | Int i -> Int i
    | Float f -> Float f
    | Bool b -> Bool b
    | Str s -> Str s
    | Builtin b -> Builtin b
    | Var x -> Var x
    | Ctor x ->
        let n = Ctx.find x ctx in
        if n = 0 then Ctor x
        else
          let pas, vas = mk_fresh_params n "_'a" in
          let pks, vks = mk_fresh_params n "_'k" in
          List.fold_right2
            (fun (pa, _) (pk, vk) acc -> Lam ([ pa; pk ], App (vk, [ acc ])))
            (List.combine pas vas) (List.combine pks vks)
            (App (Ctor x, vas))
    | Lam (xs, e) ->
        let pk, vk = mk_fresh "_'k" in
        Lam (xs @ [ pk ], cps'' e vk)
    | _ -> failwith "not an atom"
  and cps' e k =
    match e with
    | x when is_atomic x -> k (atom x)
    | App (f, xs) ->
        assert (List.length xs <= 1);
        let* f = cps' f in
        let* xs = cps_l' xs in
        let pa, va = mk_fresh "_'a" in
        let pc, vc = mk_fresh "_'cont" in
        Let (BCont (pc, Lam ([ pa ], k va)), App (f, xs @ [ vc ]))
    | Op (op, e1, e2) ->
        let* e1 = cps' e1 in
        let* e2 = cps' e2 in
        k (Op (op, e1, e2))
    | Tup es ->
        let* es = cps_l' es in
        k (Tup es)
    | Arr es ->
        let* es = cps_l' es in
        k (Arr es)
    | Let (BOne (x, e1), e2) ->
        let* e1 = cps' e1 in
        let e2 = cps' e2 k in
        Let (BOne (x, e1), e2)
    | Let (BRec xs, e2) ->
        let rhs = List.map snd xs in
        let* rhs = cps_l' rhs in
        let e2 = cps' e2 k in
        Let (BRec (List.combine (List.map fst xs) rhs), e2)
    | Sel (e, x) ->
        let* e = cps' e in
        k (Sel (e, x))
    | If (e1, e2, e3) ->
        let pk, vk = mk_fresh "_'k" in
        let pa, va = mk_fresh "_'a" in
        let pc, vc = mk_fresh "_'cont" in
        let* e1 = cps' e1 in
        Let (BCont (pc, Lam ([ pa ], k va)), App (Lam ([ pk ], If (e1, cps'' e2 vk, cps'' e3 vk)), [ vc ]))
    | Match (cond, MatchPattern cases) ->
        let* cond = cps' cond in
        let arms = List.map snd cases in
        let* arms = cps_l' arms in
        Match (cond, MatchPattern (List.combine (List.map fst cases) arms))
    | _ -> failwith "not an valid expr"
  and cps_l' es k = match es with [] -> k [] | e :: es' -> cps' e (fun e' -> cps_l' es' (fun es' -> k (e' :: es')))
  and cps'' e cont =
    match e with
    | x when is_atomic x -> App (cont, [ atom x ])
    | App (f, xs) ->
        assert (List.length xs <= 1);
        let* f = cps' f in
        let* xs = cps_l' xs in
        App (f, xs @ [ cont ])
    | Op (op, e1, e2) ->
        let* e1 = cps' e1 in
        let* e2 = cps' e2 in
        App (cont, [ Op (op, e1, e2) ])
    | Tup es ->
        let* es = cps_l' es in
        App (cont, [ Tup es ])
    | Arr es ->
        let* es = cps_l' es in
        App (cont, [ Arr es ])
    | Let (BOne (x, e1), e2) ->
        let* e1 = cps' e1 in
        let e2 = cps' e2 (fun x -> App (cont, [ x ])) in
        Let (BOne (x, e1), e2)
    | Let (BRec xs, e2) ->
        let rhs = List.map snd xs in
        let* rhs = cps_l' rhs in
        let e2 = cps' e2 (fun x -> App (cont, [ x ])) in
        Let (BRec (List.combine (List.map fst xs) rhs), e2)
    | Sel (e, x) ->
        let* e = cps' e in
        App (cont, [ Sel (e, x) ])
    | If (e1, e2, e3) ->
        let* e1 = cps' e1 in
        If (e1, cps'' e2 cont, cps'' e3 cont)
    | Match (cond, MatchPattern cases) ->
        let* cond = cps' cond in
        let arms = List.map snd cases in
        let arms = List.map (fun e -> cps'' e cont) arms in
        Match (cond, MatchPattern (List.combine (List.map fst cases) arms))
    | _ -> failwith "not an valid expr"
  in
  cps' expr (fun x -> x)

let cps_prog (prog : prog) =
  let scan_ctors_arity ctx =
    let aux ctx (kind : ty_kind) =
      match kind with
      | Enum { ctors; _ } ->
          List.fold_left
            (fun ctx (name, params) ->
              assert (not @@ Ctx.mem name ctx);
              Ctx.add name (List.length params) ctx)
            ctx ctors
    in
    function
    | TBOne (_, kind) -> aux ctx kind
    | TBRec kinds -> List.fold_left (fun ctx (_, kind) -> aux ctx kind) ctx kinds
  in
  let _, prog =
    List.fold_left_map
      (fun ctx item ->
        match item with
        | Type tb -> (scan_ctors_arity ctx tb, item)
        | Term (p, e) -> (ctx, Term (p, cps ctx e))
        | _ -> failwith "Unsuppored item")
      Ctx.empty prog
  in
  prog

type defunc_ctx = string Ctx.t

let defunc ctx expr =
  (* this is too restrictive *)
  (* https://ocaml.org/manual/5.0/letrecvalues.html *)
  let allowed_rhs_for_let_rec expr = match expr with Lam _ -> true | _ -> false in
  let rec aux (ctx : defunc_ctx) (expr : expr) =
    match expr with
    | Unit -> (Unit, [])
    | Int i -> (Int i, [])
    | Float f -> (Float f, [])
    | Bool b -> (Bool b, [])
    | Str s -> (Str s, [])
    | Builtin b -> (Builtin b, [])
    | Var x -> ( match Ctx.find_opt x ctx with Some c -> (Ctor c, []) | None -> (Var x, []))
    | GVar x -> (GVar x, [])
    | Ctor x -> (Ctor x, [])
    | Op (op, e1, e2) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (Op (op, e1, e2), l1 @ l2)
    | Tup es ->
        let es, ls = List.split @@ List.map (aux ctx) es in
        (Tup es, List.concat ls)
    | Arr es ->
        let es, ls = List.split @@ List.map (aux ctx) es in
        (Arr es, List.concat ls)
    | Let (BSeq e1, e2) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (Let (BSeq e1, e2), l1 @ l2)
    | Let (BOne (x, e1), e2) ->
        let x, e1, e2, l = de_single_binding x e1 e2 in
        (Let (BOne (x, e1), e2), l)
    | Let (BCont (x, e1), e2) ->
        let x, e1, e2, l = de_single_binding x e1 e2 in
        (Let (BCont (x, e1), e2), l)
    | Let (BRec xs, e2) ->
        let xs, ls = de_rec_bindings ctx xs in
        let e2, l = aux ctx e2 in
        (Let (BRec xs, e2), List.concat ls @ l)
    | Let (BRecC xs, e2) ->
        let xs, ls = de_rec_bindings ctx xs in
        let e2, l = aux ctx e2 in
        (Let (BRecC xs, e2), List.concat ls @ l)
    | Sel (e, x) ->
        let e, l = aux ctx e in
        (Sel (e, x), l)
    | If (e1, e2, e3) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        let e3, l3 = aux ctx e3 in
        (If (e1, e2, e3), l1 @ l2 @ l3)
    | Match (cond, MatchPattern cases) ->
        let cond, l = aux ctx cond in
        let ls, cases =
          List.fold_left_map
            (fun acc (p, e) ->
              let e, l = aux ctx e in
              (l :: acc, (p, e)))
            [] cases
        in
        (Match (cond, MatchPattern cases), l @ List.concat ls)
    | App _ -> de_app ctx expr
    | Lam _ -> de_lam ctx (Fresh.next_fresh "`C_'lam") expr
  and de_lam ctx ct (expr : expr) =
    match expr with
    | Lam (ps, x) ->
        let fvs =
          let keys, _ = List.split @@ Ctx.bindings ctx in
          SSet.diff (free expr) @@ SSet.of_list keys
        in
        let sorted_fvl = List.map (fun x -> Var x) @@ SSet.elements fvs in
        let sorted_p_fvl = List.map (fun x -> PVar x) @@ SSet.elements fvs in
        let body, l = aux ctx x in
        let abs, case =
          if SSet.is_empty fvs then (Ctor ct, PCtorApp (ct, None))
          else (App (Ctor ct, sorted_fvl), PCtorApp (ct, Some (PTup sorted_p_fvl)))
        in
        (abs, (PTup (case :: ps), body) :: l)
    | _ -> aux ctx expr
  and de_app ctx (expr : expr) =
    match expr with
    | App (Ctor x, xs) ->
        let xs, ls = List.split @@ List.map (aux ctx) xs in
        (* constructor application *) (App (Ctor x, xs), List.concat ls)
    | App (f, xs) ->
        let f, l1 = aux ctx f in
        let xs, l2 = List.split @@ List.map (aux ctx) xs in
        (App (Var "_'defunc_apply", f :: xs), l1 @ List.concat l2)
    | _ -> aux ctx expr
  and gen_symbol binding =
    match binding with
    | PVar x, Lam (_, _) -> (x, Fresh.next_fresh "`C_'lam")
    | _, Lam _ -> failwith "Pattern must be a variable in the left-hand side of a let binding when defunctionalizing"
    | _ -> failwith "Not a lambda in the right-hand side of a let binding"
  and de_single_binding x e1 e2 =
    match e1 with
    | Lam _ ->
        let xv, ct = gen_symbol (x, e1) in
        let new_ctx = Ctx.add xv ct ctx in
        let e1, l1 = de_lam ctx ct e1 in
        let e2, l2 = aux new_ctx e2 in
        (x, e1, e2, l1 @ l2)
    | _ ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (x, e1, e2, l1 @ l2)
  and de_rec_bindings ctx bindings =
    assert (List.for_all (fun (_, e) -> allowed_rhs_for_let_rec e) bindings);
    let symbols = List.map gen_symbol bindings in
    let new_ctx = List.fold_left (fun ctx (x, y) -> Ctx.add x y ctx) ctx symbols in
    let ls, e =
      List.fold_left_map
        (fun acc ((x, e), (_, ct)) ->
          let e, l = de_lam new_ctx ct e in
          (l :: acc, (x, e)))
        [] (List.combine bindings symbols)
    in
    (e, ls)
  in
  aux ctx expr

let defunc_prog (prog : prog) =
  let (_, cases), prog =
    List.fold_left_map
      (fun ((ctx, cases) as acc) item ->
        match item with
        | Type _ -> (acc, item)
        | Term (p, e) ->
            let e, its = defunc ctx e in
            ((ctx, its @ cases), Term (p, e))
        | _ -> failwith "Unsupported item")
      (Ctx.empty, []) prog
  in
  let defunc_apply =
    Term
      ( Some (PVar "_'defunc_apply"),
        Let
          ( BRec
              [
                ( PVar "_'defunc_apply",
                  Lam ([ PVar "_'f"; PVar "_'a" ], Match (Tup [ Var "_'f"; Var "_'a" ], MatchPattern cases)) );
              ],
            Var "_'defunc_apply" ) )
  in
  defunc_apply :: prog *)
