let steps_file = "eval_steps_left_to_right.json"

module Common = RunLiveCommon
module LC = Common.LC

let quicksort_nexpr =
  Common.parse_nexpr
    "let append = fix append xs. fun ys -> match xs with [] -> ys | h :: t -> h :: (append t ys) in let filter = fun p \
     -> fix filter xs. match xs with [] -> [] | h :: t -> if p h then h :: (filter t) else (filter t) in fix quicksort \
     xs. match xs with [] -> [] | pivot :: rest -> let smaller = quicksort (filter (fun x -> x < pivot) rest) in let \
     greater = quicksort (filter (fun x -> x >= pivot) rest) in append smaller (pivot :: greater)"

let quicksort_expr = Common.expr_of_nexpr quicksort_nexpr

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
      let eval expr = Common.eval_expression ~write_steps expr in
      print_endline "left_to_right quicksort (list fixed):";
      Common.left_to_right quicksort_expr
      |> List.iteri (fun i e ->
          let applied = LC.EApp (e, random_list_expr) in
          Printf.printf "step %d value: %s\n" i (Common.value_to_string (eval applied))))
