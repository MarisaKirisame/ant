(* let steps_file = "eval_steps_demand_driven.json"

module Common = RunLiveCommon
module LC = Common.LC

module DemandedExpansion = struct
  open LiveCEK

  let id_counter = ref 0

  let fresh_id () =
    incr id_counter;
    !id_counter

  let oracle : (int, expr) Hashtbl.t = Hashtbl.create 32

  let rec get_blocking_id_value = function
    | VStuck s -> get_blocking_id_stuck s
    | VCons (h, t) -> ( match get_blocking_id_value h with Some id -> Some id | None -> get_blocking_id_value t)
    | VAbs (_, env) | VFix (_, env) -> get_blocking_id_env env
    | _ -> None

  and get_blocking_id_env = function
    | LC.Nil -> None
    | LC.Cons (v, vs) -> ( match get_blocking_id_value v with Some id -> Some id | None -> get_blocking_id_env vs)

  and get_blocking_id_stuck stuck =
    match stuck with
    | SHole id -> id
    | STypeError _ -> None
    | SIndexError -> None
    | SApp (s, _) | SAdd0 (s, _) | SGt0 (s, _) | SIf (s, _, _) | SMatchList (s, _, _) -> get_blocking_id_stuck s
    | SAdd1 (_, s) | SGt1 (_, s) -> get_blocking_id_stuck s

  let reveal_shallow target_expr =
    match target_expr with
    | EPlus (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        EPlus (EHole (Some id1), EHole (Some id2))
    | ELt (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        ELt (EHole (Some id1), EHole (Some id2))
    | ELe (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        ELe (EHole (Some id1), EHole (Some id2))
    | EGt (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        EGt (EHole (Some id1), EHole (Some id2))
    | EGe (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        EGe (EHole (Some id1), EHole (Some id2))
    | EIf (c, t, e) ->
        let id1, id2, id3 = (fresh_id (), fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 c;
        Hashtbl.add oracle id2 t;
        Hashtbl.add oracle id3 e;
        EIf (EHole (Some id1), EHole (Some id2), EHole (Some id3))
    | ELet (l, r) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 l;
        Hashtbl.add oracle id2 r;
        ELet (EHole (Some id1), EHole (Some id2))
    | EApp (f, x) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 f;
        Hashtbl.add oracle id2 x;
        EApp (EHole (Some id1), EHole (Some id2))
    | EAbs e ->
        let id1 = fresh_id () in
        Hashtbl.add oracle id1 e;
        EAbs (EHole (Some id1))
    | ECons (h, t) ->
        let id1, id2 = (fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 h;
        Hashtbl.add oracle id2 t;
        ECons (EHole (Some id1), EHole (Some id2))
    | EMatchList (v, n, c) ->
        let id1, id2, id3 = (fresh_id (), fresh_id (), fresh_id ()) in
        Hashtbl.add oracle id1 v;
        Hashtbl.add oracle id2 n;
        Hashtbl.add oracle id3 c;
        EMatchList (EHole (Some id1), EHole (Some id2), EHole (Some id3))
    | EFix e ->
        let id1 = fresh_id () in
        Hashtbl.add oracle id1 e;
        EFix (EHole (Some id1))
    | EInt _ | EVar _ | ETrue | EFalse | ENil | EHole _ -> target_expr

  let rec apply_expansion expr target_id expansion =
    match expr with
    | EHole (Some id) when id = target_id -> expansion
    | EHole id -> EHole id
    | EPlus (l, r) -> EPlus (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | ELt (l, r) -> ELt (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | ELe (l, r) -> ELe (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EGt (l, r) -> EGt (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EGe (l, r) -> EGe (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EAbs e -> EAbs (apply_expansion e target_id expansion)
    | EApp (f, x) -> EApp (apply_expansion f target_id expansion, apply_expansion x target_id expansion)
    | ELet (l, r) -> ELet (apply_expansion l target_id expansion, apply_expansion r target_id expansion)
    | EIf (c, t, e) ->
        EIf
          ( apply_expansion c target_id expansion,
            apply_expansion t target_id expansion,
            apply_expansion e target_id expansion )
    | ECons (h, t) -> ECons (apply_expansion h target_id expansion, apply_expansion t target_id expansion)
    | EMatchList (v, n, c) ->
        EMatchList
          ( apply_expansion v target_id expansion,
            apply_expansion n target_id expansion,
            apply_expansion c target_id expansion )
    | EFix e -> EFix (apply_expansion e target_id expansion)
    | EInt _ | EVar _ | ETrue | EFalse | ENil -> expr

  let interactive prog use =
    Hashtbl.clear oracle;
    id_counter := 0;
    let start_prog = EHole (Some 0) in
    Hashtbl.add oracle 0 prog;
    let rec loop iter current_prog =
      match get_blocking_id_value (use iter current_prog) with
      | None -> ()
      | Some blocking_id ->
          let target_subtree = Hashtbl.find oracle blocking_id in
          let expansion = reveal_shallow target_subtree in
          let next_prog = apply_expansion current_prog blocking_id expansion in
          loop (iter + 1) next_prog
    in
    loop 0 start_prog
end

let demanded_interactive = DemandedExpansion.interactive

let run () =
  Common.with_outchannel steps_file (fun oc ->
      let write_steps = Common.write_steps_json oc in
      Common.LC.populate_state ();
      let memo = Ant.Memo.init_memo () in
      let eval expr = Common.eval_expression ~memo ~write_steps expr in
      print_endline "demanded_interactive quicksort (list fixed):";
      demanded_interactive Common.quicksort_expr (fun i e ->
          Printf.printf "step %d ast: %s\n" i (Common.expr_to_string e);
          let applied = LC.EApp (e, Common.random_list_expr) in
          let value = eval applied in
          Printf.printf "step %d value: %s\n" i (Common.value_to_string value);
          value);
      Common.write_memo_stats_json oc memo)
