module Sexp = Sexplib.Sexp

let list_to_string l = match l with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int l) ^ " :: []"

let length = 512

(* Random, without structure *)
let random_input =
  let rng = Random.State.make [| 42 |] in
  List.init length (fun _ -> Random.State.int rng 100)

let rec remove_index l i =
  match l with
  | [] -> []
  | hd::tl -> if i = 0 then tl else hd :: remove_index tl (i - 1)

let random_input_removed =
  let rng = Random.State.make [| 42 |] in
  remove_index random_input (Random.State.int rng length)

(* Repeating one number, very structured *)

let repeated_input =
  List.init length (Fun.const 1)

(* Low entropy, semi-structured input *)

let low_entropy_input =
  let rng = Random.State.make [| 42 |] in
  let chunks = [
    List.init 7 (fun _ -> Random.State.int rng 100);
    List.init 8 (fun _ -> Random.State.int rng 100);
    List.init 8 (fun _ -> Random.State.int rng 100);
    List.init 9 (fun _ -> Random.State.int rng 100);
  ] in
  let rec make_list chunks_to_concat =
    match chunks_to_concat with
    | 0 -> []
    | n -> List.append (List.nth chunks (Random.State.int rng 4)) (make_list (n - 1))
  in
  make_list (length / 8)

let map_expr =
  "fix map xs. fun f -> match xs with [] -> [] | h :: t -> f h :: t"
  |> RunLiveCommon.parse_nexpr
  |> RunLiveCommon.expr_of_nexpr

let incr_expr =
  "fun x -> x + 1"
  |> RunLiveCommon.parse_nexpr
  |> RunLiveCommon.expr_of_nexpr


let run_with_test ~steps_file ~memo ~test =
  RunLiveCommon.with_outchannel steps_file (fun oc ->
    let write_steps = RunLiveCommon.write_steps_json oc in
    RunLiveCommon.LC.populate_state ();
    let eval expr = RunLiveCommon.eval_expression ~memo ~write_steps expr in
    let list_expr =
      test
      |> list_to_string
      |> RunLiveCommon.parse_nexpr
      |> RunLiveCommon.expr_of_nexpr
    in
    let expr = RunLiveCommon.LC.EApp (RunLiveCommon.LC.EApp (map_expr, list_expr), incr_expr) in
    Format.printf "expr: %a@." RunLiveCommon.pp_expr expr;
    let value = eval expr in
    Printf.printf "value: %s\n" (RunLiveCommon.value_to_string value);
    RunLiveCommon.write_memo_stats_json oc memo)

let run () =
  RunLiveCommon.LC.populate_state ();
  let memo = Ant.Memo.init_memo () in
  run_with_test ~steps_file:"eval_steps_live_asymptotic_random.json" ~memo ~test:random_input;
  Ant.Memo.reset ();

  RunLiveCommon.LC.populate_state ();
  let memo = Ant.Memo.init_memo () in
  run_with_test ~steps_file:"eval_steps_live_asymptotic_repeated.json" ~memo ~test:repeated_input;
  Ant.Memo.reset ();

  RunLiveCommon.LC.populate_state ();
  let memo = Ant.Memo.init_memo () in
  run_with_test ~steps_file:"eval_steps_live_asymptotic_low_entropy.json" ~memo ~test:low_entropy_input;
  Ant.Memo.reset ();

  RunLiveCommon.LC.populate_state ();
  let memo = Ant.Memo.init_memo () in
  run_with_test ~steps_file:"/dev/null" ~memo ~test:random_input;
  run_with_test ~steps_file:"eval_steps_live_asymptotic_warmed_up.json" ~memo ~test:random_input_removed;
  Ant.Memo.reset ();
  ()