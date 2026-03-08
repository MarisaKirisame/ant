module LC = ArithCEK
module Plain = ArithPlain
module Memo = Ant.Memo
module Word = Ant.Word.Word
module Frontend = ArithFrontend
module Json = Yojson.Safe
module State = Ant.State

let steps_file = "eval_steps_arith.json"

type step_writer = Memo.exec_result -> unit

let current_memo : State.memo option ref = ref None
let current_write_steps : step_writer option ref = ref None

let with_memo f =
  let memo = Memo.init_memo () in
  f memo

let with_outchannel steps_path f =
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let plain_var_of_lc = function LC.X -> Plain.X | LC.Y -> Plain.Y

let rec plain_expr_of_lc = function
  | LC.Const n -> Plain.Const n
  | LC.Var v -> Plain.Var (plain_var_of_lc v)
  | LC.Add (a, b) -> Plain.Add (plain_expr_of_lc a, plain_expr_of_lc b)
  | LC.Mul (a, b) -> Plain.Mul (plain_expr_of_lc a, plain_expr_of_lc b)
  | LC.Exp x -> Plain.Exp (plain_expr_of_lc x)
  | LC.Log x -> Plain.Log (plain_expr_of_lc x)

let rec string_of_expr = function
  | LC.Const n -> string_of_int n
  | LC.Var LC.X -> "X"
  | LC.Var LC.Y -> "Y"
  | LC.Add (a, b) -> Printf.sprintf "(%s + %s)" (string_of_expr a) (string_of_expr b)
  | LC.Mul (a, b) -> Printf.sprintf "(%s * %s)" (string_of_expr a) (string_of_expr b)
  | LC.Exp x -> Printf.sprintf "exp(%s)" (string_of_expr x)
  | LC.Log x -> Printf.sprintf "log(%s)" (string_of_expr x)

let write_memo_stats_json oc (memo : State.memo) : unit =
  let stats = Memo.memo_stats memo in
  let depth_breakdown =
    `List
      (List.init (Stdlib.Dynarray.length stats.by_depth) (fun i ->
           let node = Stdlib.Dynarray.get stats.by_depth i in
           `Assoc [ ("depth", `Int node.depth); ("node_count", `Int node.node_count) ]))
  in
  let rule_stat =
    `List
      (List.map
         (fun (entry : Memo.rule_stat) ->
           `Assoc
             [
               ("size", `Int entry.size);
               ("pvar_length", `Int entry.pvar_length);
               ("sc", `Int entry.sc);
               ("hit_count", `Int entry.hit_count);
               ("insert_time", `Int entry.insert_time);
               ("depth", `Int entry.depth);
               ("rule", `String "");
             ])
         stats.rule_stat)
  in
  let node_stat =
    `List
      (List.map
         (fun (entry : Memo.node_stat) ->
           `Assoc
             [
               ("depth", `Int entry.depth);
               ("insert_time", `Int entry.insert_time);
               ( "node_state",
                 `String (match entry.node_state with Memo.Stem_node -> "stem" | Memo.Branch_node -> "branch") );
               ("rule", `String "");
             ])
         stats.node_stat)
  in
  let hashtable_stat =
    `List
      (List.map
         (fun (entry : Memo.hashtable_stat) -> `Assoc [ ("depth", `Int entry.depth); ("size", `Int entry.size) ])
         stats.hashtable_stat)
  in
  let json =
    `Assoc
      [
        ("name", `String "memo_stats");
        ("depth_breakdown", depth_breakdown);
        ("rule_stat", rule_stat);
        ("node_stat", node_stat);
        ("stem_nodes", `Int stats.node_counts.stem_nodes);
        ("branch_nodes", `Int stats.node_counts.branch_nodes);
        ("total_nodes", `Int stats.node_counts.total_nodes);
        ("hashtable_stat", hashtable_stat);
      ]
  in
  Yojson.Safe.to_string json |> output_string oc;
  output_char oc '\n';
  flush oc

let write_steps_json oc (r : Memo.exec_result) : unit =
  let json_of_profile entries = `List (List.map (fun (name, time) -> `List [ `String name; `Int time ]) entries) in
  let json =
    `Assoc
      [
        ("name", `String "exec_time");
        ("step", `Int r.step);
        ("without_memo_step", `Int r.without_memo_step);
        ("memo_profile", Ant.Profile.dump_profile Ant.Profile.memo_profile |> json_of_profile);
        ("plain_profile", Ant.Profile.dump_profile Ant.Profile.plain_profile |> json_of_profile);
        ("cek_profile", Ant.Profile.dump_profile Ant.Profile.cek_profile |> json_of_profile);
      ]
  in
  Yojson.Safe.to_string json |> output_string oc;
  output_char oc '\n';
  flush oc

let eval_plain_slot = Ant.Profile.register_slot Ant.Profile.plain_profile "arith_eval_plain"
let eval_cek_slot = Ant.Profile.register_slot Ant.Profile.cek_profile "arith_eval_cek"
let main_plain_slot = Ant.Profile.register_slot Ant.Profile.plain_profile "arith_main_plain"
let main_cek_slot = Ant.Profile.register_slot Ant.Profile.cek_profile "arith_main_cek"

let eval_plain expr x y =
  let plain_expr = plain_expr_of_lc expr in
  Gc.full_major ();
  let _ = Ant.Profile.with_slot eval_plain_slot (fun () -> Plain.eval plain_expr x y) in
  ()

type eval_details = { value : int; runtime_seconds : float; steps_with_memo : int; steps_without_memo : int }

let eval_main_expr_with_details expr x y =
  let seq_expr = LC.from_ocaml_expr expr in
  let seq_x = Memo.from_int x in
  let seq_y = Memo.from_int y in
  let start = Unix.gettimeofday () in
  let res =
    match !current_memo with
    | Some memo -> Ant.Profile.with_slot main_cek_slot (fun () -> LC.main memo seq_expr seq_x seq_y)
    | None -> with_memo (fun memo -> Ant.Profile.with_slot main_cek_slot (fun () -> LC.main memo seq_expr seq_x seq_y))
  in
  let stop = Unix.gettimeofday () in
  let plain_expr = plain_expr_of_lc expr in
  let _ = Ant.Profile.with_slot main_plain_slot (fun () -> Plain.main plain_expr x y) in
  Option.iter (fun write_steps -> write_steps res) !current_write_steps;
  let result = Word.get_value (Memo.to_word res.words) in
  {
    value = result;
    runtime_seconds = stop -. start;
    steps_with_memo = res.step;
    steps_without_memo = res.without_memo_step;
  }

let eval_expr_with_details expr x y =
  let seq_expr = LC.from_ocaml_expr expr in
  let seq_x = Memo.from_int x in
  let seq_y = Memo.from_int y in
  let start = Unix.gettimeofday () in
  let res =
    Ant.Profile.with_slot eval_cek_slot (fun () ->
        match !current_memo with
        | Some memo -> LC.eval memo seq_expr seq_x seq_y
        | None -> with_memo (fun memo -> LC.eval memo seq_expr seq_x seq_y))
  in
  let stop = Unix.gettimeofday () in
  eval_plain expr x y;
  Option.iter (fun write_steps -> write_steps res) !current_write_steps;
  let result = Word.get_value (Memo.to_word res.words) in
  {
    value = result;
    runtime_seconds = stop -. start;
    steps_with_memo = res.step;
    steps_without_memo = res.without_memo_step;
  }

let eval_string_with_details ?(print_compiled = true) code x y =
  let expr = Frontend.compile_string code in
  if print_compiled then (
    Printf.printf "compiled: %s\n" (string_of_expr expr);
    print_endline "")
  else ();
  eval_expr_with_details expr x y

let eval_string ?(print_compiled = true) code x y =
  let details = eval_string_with_details ~print_compiled code x y in
  details.value

let expect_int label expected actual =
  if expected <> actual then failwith (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let expect_true label actual = if not actual then failwith (Printf.sprintf "%s: expected true, got false" label)

let test_parser_precedence () =
  let code = "X + 2 * Y" in
  let result = eval_string ~print_compiled:false code 3 4 in
  expect_int "parser respects precedence" 11 result

let test_parser_parentheses () =
  let code = "(X + 2) * Y" in
  let result = eval_string ~print_compiled:false code 3 4 in
  expect_int "parser respects parentheses" 20 result

let test_expr_equal_structural_true () =
  let left = Plain.Add (Plain.Const 1, Plain.Var Plain.X) in
  let right = Plain.Add (Plain.Const 1, Plain.Var Plain.X) in
  expect_true "expr_equal accepts identical structure" (Plain.expr_equal left right)

let test_expr_equal_structural_false () =
  let left = Plain.Mul (Plain.Const 2, Plain.Var Plain.Y) in
  let right = Plain.Mul (Plain.Var Plain.Y, Plain.Const 2) in
  expect_true "expr_equal rejects reordered structure" (not (Plain.expr_equal left right))

let test_diffx_simplify () =
  let expr = Frontend.compile_string "X * X + 3" |> plain_expr_of_lc in
  let derived = Plain.diffx expr |> Plain.simplify_aux in
  let expected = Plain.Mul (Plain.Const 2, Plain.Var Plain.X) in
  expect_true "simplify_aux reduces derivative" (Plain.expr_equal derived expected)

let test_diffx_exp_log () =
  let exp_expr = Frontend.compile_string "exp X" |> plain_expr_of_lc in
  let exp_diff = Plain.diffx exp_expr |> Plain.simplify_aux in
  let exp_expected = Plain.Exp (Plain.Var Plain.X) in
  expect_true "diffx handles exp" (Plain.expr_equal exp_diff exp_expected);
  let log_expr = Frontend.compile_string "log X" |> plain_expr_of_lc in
  let log_diff = Plain.diffx log_expr |> Plain.simplify_aux in
  let log_expected = Plain.Exp (Plain.Mul (Plain.Const (-1), Plain.Log (Plain.Var Plain.X))) in
  expect_true "diffx handles log" (Plain.expr_equal log_diff log_expected)

let test_simplify_fixpoint () =
  let expr = Frontend.compile_string "X + 0 + 0" |> plain_expr_of_lc in
  let simplified = Plain.simplify_aux expr in
  let expected = Plain.Var Plain.X in
  expect_true "simplify_aux reaches fixpoint" (Plain.expr_equal simplified expected)

let test_eval_exp_log () =
  let code = "exp 3 + log 8" in
  let result = eval_string ~print_compiled:false code 0 0 in
  expect_int "eval exp/log with integers" 11 result

let test_eval () =
  let code = "X * Y + 2" in
  let result = eval_string ~print_compiled:false code 3 4 in
  expect_int "eval returns integer result" 14 result

let make_term n =
  match n mod 8 with
  | 0 -> LC.Var LC.X
  | 1 -> LC.Var LC.Y
  | 2 -> LC.Const ((n mod 11) + 1)
  | 3 -> LC.Add (LC.Var LC.X, LC.Const ((n mod 7) + 1))
  | 4 -> LC.Mul (LC.Var LC.X, LC.Var LC.Y)
  | 5 -> LC.Exp (LC.Add (LC.Var LC.X, LC.Const ((n mod 5) + 1)))
  | 6 -> LC.Log (LC.Add (LC.Var LC.Y, LC.Const ((n mod 9) + 2)))
  | _ -> LC.Mul (LC.Add (LC.Var LC.X, LC.Const ((n mod 3) + 1)), LC.Add (LC.Var LC.Y, LC.Const ((n mod 4) + 1)))

let lcg seed = ((seed * 1103515245) + 12345) land 0x7fffffff

let make_random_expr terms seed =
  let rec build i cur_seed acc =
    if i <= 0 then acc
    else
      let next_seed = lcg cur_seed in
      let term = make_term next_seed in
      let next_acc = match acc with None -> Some term | Some e -> Some (LC.Add (e, term)) in
      build (i - 1) next_seed next_acc
  in
  match build terms seed None with Some expr -> expr | None -> LC.Const 0

let run_bench_cases () =
  let cases = [ ("rand-100", 100, 17, 5, 8); ("rand-250", 250, 29, 5, 8); ("rand-500", 500, 43, 5, 8) ] in
  List.iter
    (fun (label, terms, seed, x, y) ->
      Printf.printf "Running arith case %s...\n" label;
      Out_channel.flush Stdio.stdout;
      let expr = make_random_expr terms seed in
      ignore (eval_main_expr_with_details expr x y))
    cases

let run () =
  with_outchannel steps_file (fun oc ->
      let write_steps = write_steps_json oc in
      LC.populate_state ();
      let memo = Memo.init_memo () in
      current_memo := Some memo;
      current_write_steps := Some write_steps;
      Fun.protect
        ~finally:(fun () ->
          current_memo := None;
          current_write_steps := None)
        (fun () ->
          test_parser_precedence ();
          test_parser_parentheses ();
          test_expr_equal_structural_true ();
          test_expr_equal_structural_false ();
          test_diffx_simplify ();
          test_diffx_exp_log ();
          test_simplify_fixpoint ();
          test_eval_exp_log ();
          test_eval ();
          run_bench_cases ();
          write_memo_stats_json oc memo));
  print_endline "ArithCEK smoke tests completed."
