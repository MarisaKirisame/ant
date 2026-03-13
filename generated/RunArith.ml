module LC = ArithCEK
module Plain = ArithPlain
module Memo = Ant.Memo
module Word = Ant.Word.Word
module Json = Yojson.Safe
module State = Ant.State

let steps_file = "eval_steps_arith.json"

type step_writer = Memo.exec_result -> unit

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

let rec string_of_expr = function
  | LC.Const n -> string_of_int n
  | LC.Var LC.X -> "X"
  | LC.Var LC.Y -> "Y"
  | LC.Add (a, b) -> Printf.sprintf "(%s + %s)" (string_of_expr a) (string_of_expr b)
  | LC.Mul (a, b) -> Printf.sprintf "(%s * %s)" (string_of_expr a) (string_of_expr b)

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

let main_plain_slot = Ant.Profile.register_slot Ant.Profile.plain_profile "arith_plain"
let main_cek_slot = Ant.Profile.register_slot Ant.Profile.cek_profile "arith_cek"

type eval_details = { value : int; runtime_seconds : float; steps_with_memo : int; steps_without_memo : int }

let eval_main_expr_with_details expr =
  let seq_expr = LC.from_ocaml_expr expr in
  let start = Unix.gettimeofday () in
  let res = with_memo (fun memo -> LC.main memo seq_expr) in
  Gc.full_major ();
  let _ =
    Ant.Profile.with_slot main_cek_slot (fun () ->
        LC.to_ocaml_expr
          (Memo.exec_cek_raw
             (Memo.pc_to_exp (Ant.Common.int_to_pc 22))
             (Dynarray.of_list [ seq_expr ])
             (Memo.from_constructor LC.tag_cont_done)))
  in
  Gc.full_major ();
  let stop = Unix.gettimeofday () in
  let plain_expr = plain_expr_of_lc expr in
  let _ = Ant.Profile.with_slot main_plain_slot (fun () -> Plain.main plain_expr) in
  Option.iter (fun write_steps -> write_steps res) !current_write_steps;
  { value = 0; runtime_seconds = stop -. start; steps_with_memo = res.step; steps_without_memo = res.without_memo_step }

let rec make_term size =
  if size == 0 then LC.Const 0
  else if size == 1 then
    match Random.int 3 with
    | 0 -> LC.Var LC.X
    | 1 -> LC.Var LC.Y
    | 2 -> LC.Const (Random.int 8)
    | _ -> failwith "impossible"
  else (
    assert (size > 0);
    let split f =
      let lsize = Random.int size in
      let rsize = size - lsize - 1 in
      f (make_term lsize) (make_term rsize)
    in
    match Random.int 4 with
    | 0 -> split (fun x y -> LC.Add (x, y))
    | 1 | 2 | 3 -> split (fun x y -> LC.Mul (x, y))
    | _ -> failwith "impossible")

let run_bench_cases () =
  let cases =
    [
      ("rand-100", 1000);
      ("rand-200", 2000);
      ("rand-300", 3000);
      ("rand-400", 4000);
      ("rand-500", 5000);
      ("rand-600", 6000);
      ("rand-700", 7000);
      ("rand-800", 8000);
      ("rand-900", 9000);
      ("rand-1000", 10000);
    ]
  in
  List.iter
    (fun (label, size) ->
      Printf.printf "Running arith case %s...\n" label;
      Out_channel.flush Stdio.stdout;
      let expr = make_term size in
      ignore (eval_main_expr_with_details expr))
    cases

let run () =
  with_outchannel steps_file (fun oc ->
      let write_steps = write_steps_json oc in
      LC.populate_state ();
      let memo = Memo.init_memo () in
      current_write_steps := Some write_steps;
      Fun.protect
        ~finally:(fun () -> current_write_steps := None)
        (fun () ->
          run_bench_cases ();
          write_memo_stats_json oc memo))
