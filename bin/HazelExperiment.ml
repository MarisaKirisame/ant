module Common = RunLiveCommon

type dataset = Mk | Th | At
type benchmark = Append | Filter | Map | QS | IS | MS | Pair | Rev

let benchmarks = [ Append; Filter; Map; QS; IS; MS; Pair; Rev ]

let benchmark_key = function
  | Append -> "append"
  | Filter -> "filter"
  | Map -> "map"
  | QS -> "qs"
  | IS -> "is"
  | MS -> "ms"
  | Pair -> "pair"
  | Rev -> "rev"

let benchmark_data_suffix = function
  | Append -> "Append"
  | Filter -> "Filter"
  | Map -> "Map"
  | QS -> "QS"
  | IS -> "IS"
  | MS -> "MS"
  | Pair -> "Pair"
  | Rev -> "Rev"

let dataset_data_prefix = function Mk -> "mk" | Th -> "th" | At -> "at"
let dataset_mode_prefix = function Mk -> "" | Th -> "th_" | At -> "at_"
let mode_name dataset benchmark = dataset_mode_prefix dataset ^ benchmark_key benchmark

let steps_file dataset benchmark =
  match dataset with
  | Mk -> Printf.sprintf "eval_steps_%s.json" (benchmark_key benchmark)
  | Th -> Printf.sprintf "eval_steps_th_%s.json" (benchmark_key benchmark)
  | At -> Printf.sprintf "eval_steps_at_%s.json" (benchmark_key benchmark)

let program_path dataset benchmark =
  Printf.sprintf "data/%s_%s.json" (dataset_data_prefix dataset) (benchmark_data_suffix benchmark)

let list_to_cons_str xs = match xs with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int xs) ^ " :: []"

let input_sizes = [ 50; 100; 150; 200; 250; 300; 350; 400 ]
let repeat_count = 10

let seed_for ~input_size ~repeat_index = Common.experiment_random_seed + (input_size * repeat_count) + repeat_index

let benchmark_test_string benchmark ~input_size ~repeat_index =
  let seed = seed_for ~input_size ~repeat_index in
  let input = Common.make_random_input_list ~seed input_size in
  let left, right = Common.make_random_input_list_pair ~seed input_size in
  match benchmark with
  | Append -> Printf.sprintf {| my_append (%s) (%s) |} (list_to_cons_str left) (list_to_cons_str right)
  | Filter -> Printf.sprintf {| my_filter (fun x -> x > 50) (%s) |} (list_to_cons_str input)
  | Map -> Printf.sprintf {| my_map (fun x -> x + 1) (%s) |} (list_to_cons_str input)
  | QS -> Printf.sprintf {| my_quicksort (%s) |} (list_to_cons_str input)
  | IS -> Printf.sprintf {| my_insertsort (%s) |} (list_to_cons_str input)
  | MS -> Printf.sprintf {| my_mergesort (%s) |} (list_to_cons_str input)
  | Pair -> Printf.sprintf {| my_pair (%s) |} (list_to_cons_str input)
  | Rev -> Printf.sprintf {| my_reverse (%s) |} (list_to_cons_str input)

let benchmark_tests benchmark =
  List.concat_map
    (fun input_size ->
      List.init repeat_count (fun repeat_index ->
          let test = Common.parse_nexpr (benchmark_test_string benchmark ~input_size ~repeat_index) in
          FromHazel.{ test; input_size = Some input_size; repeat_index = Some repeat_index }))
    input_sizes

let run ~dataset ~benchmark =
  FromHazel.run_with_tests ~program_name:(mode_name dataset benchmark) ~program_path:(program_path dataset benchmark)
    ~steps_file:(steps_file dataset benchmark) ~tests:(benchmark_tests benchmark)

let run_mk benchmark = run ~dataset:Mk ~benchmark
let run_th benchmark = run ~dataset:Th ~benchmark
let run_at benchmark = run ~dataset:At ~benchmark

let decode_mode mode =
  let normalized = String.lowercase_ascii mode in
  List.find_map
    (fun benchmark ->
      if String.equal normalized (mode_name Mk benchmark) then Some (Mk, benchmark)
      else if String.equal normalized (mode_name Th benchmark) then Some (Th, benchmark)
      else if String.equal normalized (mode_name At benchmark) then Some (At, benchmark)
      else None)
    benchmarks

let run_mode mode =
  match decode_mode mode with
  | None -> false
  | Some (dataset, benchmark) ->
      run ~dataset ~benchmark;
      true

let all_modes =
  List.concat_map
    (fun benchmark -> [ mode_name Mk benchmark; mode_name Th benchmark; mode_name At benchmark ])
    benchmarks

let usage = Printf.sprintf "Usage: GeneratedMain <%s>" (String.concat "|" all_modes)
