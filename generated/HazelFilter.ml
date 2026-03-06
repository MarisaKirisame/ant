let steps_file = "eval_steps_filter.json"
let program_path = "data/mk_Filter.json"

let large_input =
  let rng = Random.State.make [| 42 |] in
  List.init 640 (fun _ -> Random.State.int rng 100)

let large_input_str =
  match large_input with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int large_input) ^ " :: []"

let filter_test_string =
  Printf.sprintf
    {| (my_filter (fun x -> true) ([])) ::
       (my_filter (fun x -> false) (1 :: 2 :: 3 :: [])) ::
       (my_filter (fun x -> x > 2) (1 :: 2 :: 3 :: 4 :: [])) ::
       (my_filter (fun x -> x > 50) (%s)) ::
       [] |}
    large_input_str

let filter_test = RunLiveCommon.parse_nexpr filter_test_string
let run () = FromHazel.run_with_test ~program_name:"filter" ~program_path ~steps_file ~test:filter_test
