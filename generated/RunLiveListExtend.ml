let steps_file = "eval_steps_list_extend.json"

module Common = RunLiveCommon
module LC = Common.LC

(* List of increasing-length prefixes of Common.random_list *)
let (random_lists_exprs, _) =
    List.fold_right
        (fun n (res, acc) ->
            let new_acc = LC.ECons (LC.EInt n, acc) in
            (new_acc :: res, new_acc)
        )
        (List.take 100 Common.random_list) (* Using the whole list takes kinda long *)
        ([], LC.ENil)
let random_lists_exprs = List.rev random_lists_exprs

let run () =
  Common.with_outchannel steps_file (fun oc ->
      let write_steps = Common.write_steps_json oc in
      Common.LC.populate_state ();
      let memo = Ant.Memo.init_memo () in
      let eval expr = Common.eval_expression ~memo ~write_steps expr in
      print_endline "list_extend quicksort (quicksort expression fixed):";
      random_lists_exprs
      |> List.iteri (fun i e ->
          let applied = LC.EApp (Common.quicksort_expr, e) in
          Printf.printf "step %d value: %s\n" i (Common.value_to_string (eval applied)));
      Common.write_memo_stats_json oc memo)
