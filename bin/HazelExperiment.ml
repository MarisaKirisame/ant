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

let benchmark_test_string ~input_size =
  let large_input = Common.make_random_input_list input_size in
  let large_left, large_right = Common.make_random_input_list_pair input_size in
  function
  | Append -> Printf.sprintf {| my_append (%s) (%s) |} (list_to_cons_str large_left) (list_to_cons_str large_right)
  | Filter -> Printf.sprintf {| my_filter (fun x -> x > 50) (%s) |} (list_to_cons_str large_input)
  | Map -> Printf.sprintf {| my_map (fun x -> x + 1) (%s) |} (list_to_cons_str large_input)
  | QS -> Printf.sprintf {| my_quicksort (%s) |} (list_to_cons_str large_input)
  | IS -> Printf.sprintf {| my_insertsort (%s) |} (list_to_cons_str large_input)
  | MS -> Printf.sprintf {| my_mergesort (%s) |} (list_to_cons_str large_input)
  | Pair -> Printf.sprintf {| my_pair (%s) |} (list_to_cons_str large_input)
  | Rev -> Printf.sprintf {| my_reverse (%s) |} (list_to_cons_str large_input)

let run ?hazel_compare ?(input_size = Common.experiment_list_length) ?steps_file:steps_file_override ~dataset ~benchmark
    () =
  if input_size <= 0 then invalid_arg "HazelExperiment.run: input_size must be positive";
  let test = Common.parse_nexpr (benchmark_test_string ~input_size benchmark) in
  let steps_file = Option.value steps_file_override ~default:(steps_file dataset benchmark) in
  FromHazel.run_with_test ~program_name:(mode_name dataset benchmark) ~program_path:(program_path dataset benchmark)
    ~steps_file ~input_size ~test ?hazel_compare ()

let run_mk benchmark = run ~dataset:Mk ~benchmark ()
let run_th benchmark = run ~dataset:Th ~benchmark ()
let run_at benchmark = run ~dataset:At ~benchmark ()

let rec find_repo_root dir =
  let marker = Filename.concat dir "hazel/src/CLI/polyfill.js" in
  if Sys.file_exists marker then dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then failwith "could not find Hazel submodule from current directory"
    else find_repo_root parent

let hazel_path path = Filename.concat (find_repo_root (Sys.getcwd ())) path

let hazel_compare_config =
  {
    FromHazel.hazel_cmd =
      Printf.sprintf "node --max-old-space-size=8192 --stack-size=32768 -r %s %s"
        (Filename.quote (hazel_path "hazel/src/CLI/polyfill.js"))
        (Filename.quote (hazel_path "hazel/_build/default/src/CLI/cli.bc.js"));
  }

let run_compare ?(hazel_compare = hazel_compare_config) ~dataset ~benchmark () =
  run ~hazel_compare:(Some hazel_compare) ~dataset ~benchmark ()

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
      run ~dataset ~benchmark ();
      true

let run_scaling_mode ~mode ~input_size ~steps_file =
  match decode_mode mode with
  | None -> false
  | Some (dataset, benchmark) ->
      run ~dataset ~benchmark ~input_size ~steps_file ();
      true

let run_compare_mode mode =
  match decode_mode mode with
  | None -> false
  | Some (dataset, benchmark) ->
      run_compare ~dataset ~benchmark ();
      true

let all_modes =
  List.concat_map
    (fun benchmark -> [ mode_name Mk benchmark; mode_name Th benchmark; mode_name At benchmark ])
    benchmarks

let usage = Printf.sprintf "Usage: GeneratedMain <%s>" (String.concat "|" all_modes)
