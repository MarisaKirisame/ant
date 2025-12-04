let steps_file = "eval_steps_demand_driven.json"

module Common = RunLiveCommon
module LC = Common.LC

let random_list =
  [
    17;
    81;
    91;
    31;
    90;
    70;
    14;
    62;
    9;
    20;
    70;
    75;
    23;
    25;
    8;
    70;
    88;
    53;
    95;
    50;
    36;
    47;
    94;
    23;
    18;
    55;
    57;
    44;
    22;
    97;
    96;
    74;
    48;
    76;
    17;
    8;
    31;
    67;
    69;
    80;
    29;
    16;
    75;
    13;
    54;
    97;
    65;
    85;
    45;
    58;
  ]

let random_list_expr = List.fold_right (fun n acc -> LC.ECons (LC.EInt n, acc)) random_list LC.ENil

let run () =
  Common.with_steps_writer steps_file (fun write_steps ->
      let memo = Ant.Memo.init_memo () in
      let eval expr = Common.eval_expression ~memo ~write_steps expr in
      print_endline "demanded_interactive quicksort (list fixed):";
      Common.demanded_interactive Common.quicksort_expr (fun i e ->
          Printf.printf "step %d ast: %s\n" i (Common.expr_to_string e);
          let applied = LC.EApp (e, random_list_expr) in
          let value = eval applied in
          Printf.printf "step %d value: %s\n" i (Common.value_to_string value);
          value))
