let steps_file = "eval_steps_simple.json"

module Common = RunLiveCommon
module LC = Common.LC

let mapinc =
  LC.EFix
    (LC.EMatchList
       ( LC.EVar (Common.nat_from_int 0),
         LC.EVar (Common.nat_from_int 0),
         LC.ECons
           ( LC.EPlus (LC.EInt 1, LC.EVar (Common.nat_from_int 1)),
             LC.EApp (LC.EVar (Common.nat_from_int 3), LC.EVar (Common.nat_from_int 0)) ) ))

let run () =
  Common.with_outchannel steps_file (fun oc ->
      let write_steps = Common.write_steps_json oc in
      let memo = Ant.Memo.init_memo () in
      let eval expr = Common.eval_expression ~memo ~write_steps expr in
      print_endline "mapinc:";
      print_endline (Common.expr_to_string mapinc);
      print_endline (Common.value_to_string (eval (LC.EInt 42)));
      let repeat_list x =
        let rec build n acc = if n == 0 then acc else build (n - 1) (LC.ECons (LC.EInt 1, acc)) in
        build x LC.ENil
      in
      let nats x acc =
        let rec build n = if n == x then acc else LC.ECons (LC.EInt n, build (n + 1)) in
        build 0
      in
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, repeat_list 2))));
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, repeat_list 40))));
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, repeat_list 45))));
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, nats 40 LC.ENil))));
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, nats 45 LC.ENil))));
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, nats 46 LC.ENil))));
      print_endline (Common.value_to_string (eval (LC.EApp (mapinc, nats 45 (nats 45 LC.ENil)))));
      print_endline
        (Common.value_to_string (eval (LC.ELet (mapinc, LC.EApp (LC.EVar (Common.nat_from_int 0), nats 45 LC.ENil)))));
      Common.write_memo_stats_json oc memo)
