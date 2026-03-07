module LC = ArithCEK
module Plain = ArithPlain
module Memo = Ant.Memo
module Word = Ant.Word.Word
module Frontend = ArithFrontend
module Json = Yojson.Safe
module State = Ant.State
module Common = Ant.Common

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

let eval_plain expr x y =
  let plain_expr = plain_expr_of_lc expr in
  Gc.full_major ();
  let _ = Ant.Profile.with_slot eval_plain_slot (fun () -> Plain.eval plain_expr x y) in
  ()

let eval_cek_profile expr x y =
  let seq_expr = LC.from_ocaml_expr expr in
  let seq_x = Memo.from_int x in
  let seq_y = Memo.from_int y in
  Gc.full_major ();
  let _ =
    Ant.Profile.with_slot eval_cek_slot (fun () ->
        Memo.exec_cek_raw
          (Memo.pc_to_exp (Common.int_to_pc 32))
          (Dynarray.of_list [ seq_expr; seq_x; seq_y ])
          (Memo.from_constructor LC.tag_cont_done))
  in
  ()

type eval_details = { value : int; runtime_seconds : float; steps_with_memo : int; steps_without_memo : int }

let eval_expr_with_details expr x y =
  let seq_expr = LC.from_ocaml_expr expr in
  let seq_x = Memo.from_int x in
  let seq_y = Memo.from_int y in
  let start = Unix.gettimeofday () in
  let res =
    match !current_memo with
    | Some memo -> LC.eval memo seq_expr seq_x seq_y
    | None -> with_memo (fun memo -> LC.eval memo seq_expr seq_x seq_y)
  in
  let stop = Unix.gettimeofday () in
  let _ = eval_cek_profile expr x y in
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

let test_expr_eq_commutative () =
  let left = Plain.Add (Plain.Const 1, Plain.Var Plain.X) in
  let right = Plain.Add (Plain.Var Plain.X, Plain.Const 1) in
  expect_true "expr_eq ignores add ordering" (Plain.expr_eq left right)

let test_expr_eq_mul_commutative () =
  let left = Plain.Mul (Plain.Const 2, Plain.Var Plain.Y) in
  let right = Plain.Mul (Plain.Var Plain.Y, Plain.Const 2) in
  expect_true "expr_eq ignores mul ordering" (Plain.expr_eq left right)

let test_diffx_simplify () =
  let expr = Frontend.compile_string "X * X + 3" |> plain_expr_of_lc in
  let derived = Plain.diffx expr |> Plain.simplify_aux in
  let expected = Plain.Add (Plain.Var Plain.X, Plain.Var Plain.X) in
  expect_true "simplify_aux reduces derivative" (Plain.expr_eq derived expected)

let test_diffx_exp_log () =
  let exp_expr = Frontend.compile_string "exp X" |> plain_expr_of_lc in
  let exp_diff = Plain.diffx exp_expr |> Plain.simplify_aux in
  let exp_expected = Plain.Exp (Plain.Var Plain.X) in
  expect_true "diffx handles exp" (Plain.expr_eq exp_diff exp_expected);
  let log_expr = Frontend.compile_string "log X" |> plain_expr_of_lc in
  let log_diff = Plain.diffx log_expr |> Plain.simplify_aux in
  let log_expected = Plain.Exp (Plain.Mul (Plain.Log (Plain.Var Plain.X), Plain.Const (-1))) in
  expect_true "diffx handles log" (Plain.expr_eq log_diff log_expected)

let test_simplify_fixpoint () =
  let expr = Frontend.compile_string "X + 0 + 0" |> plain_expr_of_lc in
  let simplified = Plain.simplify_aux expr in
  let expected = Plain.Var Plain.X in
  expect_true "simplify_aux reaches fixpoint" (Plain.expr_eq simplified expected)

let test_eval_exp_log () =
  let code = "exp 3 + log 8" in
  let result = eval_string ~print_compiled:false code 0 0 in
  expect_int "eval exp/log with integers" 11 result

let test_eval () =
  let code = "X * Y + 2" in
  let result = eval_string ~print_compiled:false code 3 4 in
  expect_int "eval returns integer result" 14 result

let run_bench_cases () =
  let repeat_add n term =
    if n <= 1 then term
    else
      let rec aux i acc = if i <= 0 then acc else aux (i - 1) (acc ^ " + " ^ term) in
      aux (n - 1) term
  in
  let shared = "(exp (X + Y) + log (X + 3) + X * Y + 1)" in
  let poly =
    "(" ^ shared ^ " * " ^ shared ^ " + " ^ shared ^ " * " ^ shared ^ " + " ^ shared ^ " * " ^ shared ^ " + " ^ shared
    ^ " * " ^ shared ^ ")"
  in
  let huge = repeat_add 24 poly in
  let giant = repeat_add 40 poly in
  let mega = repeat_add 120 poly in
  let cases =
    [
      ("small", "X + 2 * Y", 7, 9);
      ("medium", "exp (X + 1) * log (Y + 4) + X * X * Y", 3, 10);
      ("large", "exp (X + Y) * exp (X + 2) + (X * Y + 1) * (X + Y + 3)", 4, 6);
      ("huge", huge, 5, 8);
      ("giant", giant, 5, 8);
      ("mega", mega, 5, 8);
    ]
  in
  List.iter
    (fun (label, code, x, y) ->
      Printf.printf "Running arith case %s...\n" label;
      Out_channel.flush Stdio.stdout;
      ignore (eval_string_with_details ~print_compiled:false code x y))
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
          test_expr_eq_commutative ();
          test_expr_eq_mul_commutative ();
          test_diffx_simplify ();
          test_diffx_exp_log ();
          test_simplify_fixpoint ();
          test_eval_exp_log ();
          test_eval ();
          run_bench_cases ();
          write_memo_stats_json oc memo));
  print_endline "ArithCEK smoke tests completed."
