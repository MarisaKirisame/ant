let steps_file = "eval_steps_left_to_right.json"

module Common = RunLiveCommon
module LC = Common.LC

(* Produce a sequence of partial expressions that reveal subexpressions from left to right. The
   list always starts with a single hole and ends with the fully constructed input expression. *)
let left_to_right (expr : LC.expr) : LC.expr list =
  let tail = function [] -> failwith "left_to_right: not expecting empty list" | _ :: t -> t in
  let rec build e =
    match e with
    | LC.EHole x -> [ LC.EHole x ]
    | LC.EInt _ | LC.EVar _ | LC.ETrue | LC.EFalse | LC.ENil -> [ LC.EHole None; e ]
    | LC.EAbs body ->
        let steps_body = build body in
        LC.EHole None :: LC.EAbs (LC.EHole None) :: List.map (fun s -> LC.EAbs s) (tail steps_body)
    | LC.EFix body ->
        let steps_body = build body in
        LC.EHole None :: LC.EFix (LC.EHole None) :: List.map (fun s -> LC.EFix s) (tail steps_body)
    | LC.EPlus (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.EPlus (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EPlus (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.EPlus (l, s)) (tail sr)
    | LC.ELt (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.ELt (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ELt (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.ELt (l, s)) (tail sr)
    | LC.ELe (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.ELe (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ELe (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.ELe (l, s)) (tail sr)
    | LC.EGt (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.EGt (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EGt (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.EGt (l, s)) (tail sr)
    | LC.EGe (l, r) ->
        let sl = build l in
        let sr = build r in
        LC.EHole None
        :: LC.EGe (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EGe (s, LC.EHole None)) (tail sl)
        @ List.map (fun s -> LC.EGe (l, s)) (tail sr)
    | LC.EApp (fn, arg) ->
        let sfn = build fn in
        let sarg = build arg in
        LC.EHole None
        :: LC.EApp (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EApp (s, LC.EHole None)) (tail sfn)
        @ List.map (fun s -> LC.EApp (fn, s)) (tail sarg)
    | LC.ELet (bound, body) ->
        let sbound = build bound in
        let sbody = build body in
        LC.EHole None
        :: LC.ELet (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ELet (s, LC.EHole None)) (tail sbound)
        @ List.map (fun s -> LC.ELet (bound, s)) (tail sbody)
    | LC.EIf (cond, thn, els) ->
        let scond = build cond in
        let sthn = build thn in
        let sels = build els in
        LC.EHole None
        :: LC.EIf (LC.EHole None, LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EIf (s, LC.EHole None, LC.EHole None)) (tail scond)
        @ List.map (fun s -> LC.EIf (cond, s, LC.EHole None)) (tail sthn)
        @ List.map (fun s -> LC.EIf (cond, thn, s)) (tail sels)
    | LC.ECons (hd, tl) ->
        let shd = build hd in
        let stl = build tl in
        LC.EHole None
        :: LC.ECons (LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.ECons (s, LC.EHole None)) (tail shd)
        @ List.map (fun s -> LC.ECons (hd, s)) (tail stl)
    | LC.EMatchList (target, nil_case, cons_case) ->
        let starget = build target in
        let snil = build nil_case in
        let scons = build cons_case in
        LC.EHole None
        :: LC.EMatchList (LC.EHole None, LC.EHole None, LC.EHole None)
        :: List.map (fun s -> LC.EMatchList (s, LC.EHole None, LC.EHole None)) (tail starget)
        @ List.map (fun s -> LC.EMatchList (target, s, LC.EHole None)) (tail snil)
        @ List.map (fun s -> LC.EMatchList (target, nil_case, s)) (tail scons)
  in
  build expr

let random_list_expr = List.fold_right (fun n acc -> LC.ECons (LC.EInt n, acc)) Common.random_list LC.ENil

let run () =
  Common.with_outchannel steps_file (fun oc ->
      let write_steps = Common.write_steps_json oc in
      let memo = Ant.Memo.init_memo () in
      let eval expr = Common.eval_expression ~memo ~write_steps expr in
      print_endline "left_to_right quicksort (list fixed):";
      left_to_right Common.quicksort_expr
      |> List.iteri (fun i e ->
          let applied = LC.EApp (e, random_list_expr) in
          Printf.printf "step %d value: %s\n" i (Common.value_to_string (eval applied)));
      Common.write_memo_stats_json oc memo)
