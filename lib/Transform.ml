open Syntax
open Fresh
module Ctx = Map.Make (String)
module SSet = Set.Make (String)

let free, free_p =
  let rec do_expr s = function
    | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ -> s
    | Var x -> SSet.add x s
    | Ctor _ -> s
    | Lam (ps, e) ->
        SSet.diff (do_expr s e) (List.fold_left do_pattern SSet.empty ps)
    | App (f, xs) -> List.fold_left do_expr s (f :: xs)
    | Op (_, e1, e2) -> do_expr (do_expr s e1) e2
    | Tup es | Arr es -> List.fold_left do_expr s es
    | Let ((BOne (x, e1) | BCont (x, e1)), e2) ->
        SSet.union (do_expr s e1)
          (SSet.diff (do_expr SSet.empty e2) (do_pattern SSet.empty x))
    | Let ((BRec xs | BRecC xs), e2) ->
        SSet.union
          (List.fold_left do_expr s (List.map snd xs))
          (SSet.diff (do_expr SSet.empty e2)
             (List.fold_left do_pattern SSet.empty (List.map fst xs)))
    | Let (BSeq e1, e2) -> do_expr (do_expr s e1) e2
    | Sel (e, _) -> do_expr s e
    | If (e1, e2, e3) -> do_expr (do_expr (do_expr s e1) e2) e3
    | Match (cond, MatchPattern cases) ->
        List.fold_left
          (fun s (p, e) ->
            SSet.union s
            @@ SSet.diff (do_expr SSet.empty e) (do_pattern SSet.empty p))
          (do_expr s cond) cases
  and do_pattern s = function
    | PAny | PInt _ | PBool _ -> s
    | PVar x -> SSet.add x s
    | PUnit -> s
    | PTup ps -> List.fold_left do_pattern s ps
    | PApp (_, None) -> s
    | PApp (_, Some p) -> do_pattern s p
  in
  (do_expr SSet.empty, do_pattern SSet.empty)

type ctx = int Ctx.t

let cps ctx expr =
  let mk_fresh prefix =
    let k = next_fresh prefix in
    (PVar k, Var k)
  in
  let mk_fresh_params n prefix =
    let rec aux acc acc2 n =
      if n = 0 then (acc, acc2)
      else
        let x = next_fresh prefix in
        aux (PVar x :: acc) (Var x :: acc2) (n - 1)
    in
    let l1, l2 = aux [] [] n in
    (List.rev l1, List.rev l2)
  in
  let is_atomic = function
    | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | Ctor _
    | Lam _ ->
        true
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
        let* e2 = cps' e2 in
        k (Let (BOne (x, e1), e2))
    | Let (BRec xs, e2) ->
        let rhs = List.map snd xs in
        let* rhs = cps_l' rhs in
        let* e2 = cps' e2 in
        let r = Let (BRec (List.combine (List.map fst xs) rhs), e2) in
        k r
    | Sel (e, x) ->
        let* e = cps' e in
        k (Sel (e, x))
    | If (e1, e2, e3) ->
        let pk, vk = mk_fresh "_'k" in
        let pa, va = mk_fresh "_'a" in
        let pc, vc = mk_fresh "_'cont" in
        let* e1 = cps' e1 in
        Let
          ( BCont (pc, Lam ([ pa ], k va)),
            App (Lam ([ pk ], If (e1, cps'' e2 vk, cps'' e3 vk)), [ vc ]) )
    | Match (cond, MatchPattern cases) ->
        let* cond = cps' cond in
        let arms = List.map snd cases in
        let* arms = cps_l' arms in
        Match (cond, MatchPattern (List.combine (List.map fst cases) arms))
    | _ -> failwith "not an valid expr"
  and cps_l' es k =
    match es with
    | [] -> k []
    | e :: es' -> cps' e (fun e' -> cps_l' es' (fun es' -> k (e' :: es')))
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
        let* e2 = cps' e2 in
        App (cont, [ Let (BOne (x, e1), e2) ])
    | Let (BRec xs, e2) ->
        let rhs = List.map snd xs in
        let* rhs = cps_l' rhs in
        let* e2 = cps' e2 in
        let r = Let (BRec (List.combine (List.map fst xs) rhs), e2) in
        App (cont, [ r ])
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
    | TBRec kinds ->
        List.fold_left (fun ctx (_, kind) -> aux ctx kind) ctx kinds
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

let defunc ctx expr =
  (* this is too restrictive *)
  (* https://ocaml.org/manual/5.0/letrecvalues.html *)
  let allowed_rhs_for_let_rec bv expr =
    SSet.disjoint (free expr) bv
    || match expr with Var _ | Lam _ -> true | _ -> false
  in
  let rec aux (ctx : ctx) (expr : expr) =
    match expr with
    | Unit -> (Unit, [])
    | Int i -> (Int i, [])
    | Float f -> (Float f, [])
    | Bool b -> (Bool b, [])
    | Str s -> (Str s, [])
    | Builtin b -> (Builtin b, [])
    | Var x -> (Var x, [])
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
    | Let (BOne (x, e1), e2) ->
        let e1, l1 = aux ctx e1 in
        let e2, l2 = aux ctx e2 in
        (Let (BOne (x, e1), e2), l1 @ l2)
    | Let (BRec xs, e2) ->
        let xs, ls = de_rec_bindings ctx xs in
        let e2, l = aux ctx e2 in
        (Let (BRec xs, e2), List.concat ls @ l)
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
    | _ -> de ctx expr
  and de = de_w_bound ~bv:SSet.empty
  and de_w_bound ctx ~bv (expr : expr) =
    match expr with
    | Lam (ps, x) ->
        let ct = next_fresh "`C_'lam" in
        let fvs = SSet.diff (free expr) bv in
        let sorted_fvl = List.map (fun x -> Var x) @@ SSet.elements fvs in
        let sorted_p_fvl = List.map (fun x -> PVar x) @@ SSet.elements fvs in
        let body, l = aux ctx x in
        let abs, case =
          if SSet.is_empty fvs then (Ctor ct, PApp (ct, None))
          else (App (Ctor ct, sorted_fvl), PApp (ct, Some (PTup sorted_p_fvl)))
        in
        (abs, (PTup (case :: ps), body) :: l)
    | App (Ctor x, xs) ->
        let xs, ls = List.split @@ List.map (aux ctx) xs in
        (* constructor application *) (App (Ctor x, xs), List.concat ls)
    | App (f, xs) ->
        let f, l1 = aux ctx f in
        let xs, l2 = List.split @@ List.map (aux ctx) xs in
        (App (Var "_'defunc_apply", f :: xs), l1 @ List.concat l2)
    | _ -> aux ctx expr
  and de_rec_bindings _ctx bindings =
    let bv =
      List.fold_left SSet.union SSet.empty
        (List.map (fun x -> free_p @@ fst x) bindings)
    in
    assert (List.for_all (fun (_, e) -> allowed_rhs_for_let_rec bv e) bindings);
    let ls, e =
      List.fold_left_map
        (fun acc (x, e) ->
          let e, l = de_w_bound ctx ~bv e in
          (l :: acc, (x, e)))
        [] bindings
    in
    (e, ls)
  in
  de ctx expr

let defunc_prog (prog : prog) =
  let (_, _), prog =
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
  prog
