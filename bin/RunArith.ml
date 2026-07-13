module LC = ArithCEK
module Plain = ArithPlain
module Memo = Ant.Memo
module Word = Ant.Word.Word
module Json = Yojson.Safe
module State = Ant.State

let default_steps_file = "results/arith/arith.json"
let default_term_size = 400
let default_sample_count = 20

type step_writer = Memo.exec_result -> unit

let current_write_steps : step_writer option ref = ref None

let init_random () =
  match Sys.getenv_opt "ARITH_SEED" with
  | Some seed -> ( match int_of_string_opt seed with Some n -> Random.init n | None -> Random.init 0)
  | None -> Random.init 0

let with_memo f =
  let memo = Memo.init_memo () in
  f memo

let ensure_parent_dir path =
  let parent = Filename.dirname path in
  if (not (String.equal parent ".")) && not (Sys.file_exists parent) then
    let cmd = Printf.sprintf "mkdir -p %s" (Filename.quote parent) in
    if Sys.command cmd <> 0 then failwith (Printf.sprintf "failed to create directory %s" parent)

let with_outchannel steps_path f =
  ensure_parent_dir steps_path;
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_text; Open_wronly ] 0o644 steps_path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

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

let write_steps_json oc ~(input_size : int) ~(sample_index : int) (r : Memo.exec_result) : unit =
  let json_of_profile entries = `List (List.map (fun (name, time) -> `List [ `String name; `Int time ]) entries) in
  let json =
    `Assoc
      [
        ("name", `String "exec_time");
        ("step", `Int r.step);
        ("without_memo_step", `Int r.without_memo_step);
        ("input_size", `Int input_size);
        ("sample_index", `Int sample_index);
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
  let res = with_memo (fun memo -> LC.main ~config:(Memo.memo_config memo) seq_expr) in
  Gc.full_major ();
  let _ =
    Ant.Profile.with_slot main_cek_slot (fun () -> LC.to_ocaml_expr (LC.main ~config:Memo.cek_config seq_expr).words)
  in
  Gc.full_major ();
  let stop = Unix.gettimeofday () in
  let _ = Ant.Profile.with_slot main_plain_slot (fun () -> Plain.main expr) in
  Option.iter (fun write_steps -> write_steps res) !current_write_steps;
  { value = 0; runtime_seconds = stop -. start; steps_with_memo = res.step; steps_without_memo = res.without_memo_step }

let rec make_term size =
  assert (size >= 0);
  if size == 0 then
    match Random.int 5 with
    | 0 -> LC.Var LC.X
    | 1 -> LC.Var LC.Y
    | 2 -> LC.Const 0
    | 3 -> LC.Const 1
    | 4 -> LC.Const (2 + Random.int 3)
    | _ -> failwith "impossible"
  else
    let split f =
      let lsize = Random.int size in
      let rsize = size - lsize - 1 in
      f (make_term lsize) (make_term rsize)
    in
    if Random.bool () then split (fun x y -> LC.Add (x, y)) else split (fun x y -> LC.Mul (x, y))

let run_bench_cases ~term_size ~sample_count =
  List.iter
    (fun sample_index ->
      Out_channel.flush Stdio.stdout;
      let expr = make_term term_size in
      Printf.printf "Running arith case %d/%d at size %d...\n" (sample_index + 1) sample_count term_size;
      ignore (eval_main_expr_with_details expr))
    (List.init sample_count Fun.id)

let run ?(term_size = default_term_size) ?(sample_count = default_sample_count) ?(steps_file = default_steps_file) () =
  if term_size <= 0 then invalid_arg "RunArith.run: term_size must be positive";
  if sample_count <= 0 then invalid_arg "RunArith.run: sample_count must be positive";
  with_outchannel steps_file (fun oc ->
      init_random ();
      LC.populate_state ();
      let memo = Memo.init_memo () in
      let sample_index = ref 0 in
      current_write_steps :=
        Some
          (fun result ->
            write_steps_json oc ~input_size:term_size ~sample_index:!sample_index result;
            incr sample_index);
      Fun.protect
        ~finally:(fun () -> current_write_steps := None)
        (fun () ->
          run_bench_cases ~term_size ~sample_count;
          write_memo_stats_json oc memo))
