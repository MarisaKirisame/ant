let steps_file = "eval_steps_is_sorted.json"
let program_path = "data/mk_program.json"
let large_input = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input

let large_input_str =
  match large_input with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int large_input) ^ " :: []"

let sorted_test_string =
  Printf.sprintf
    {| (sorted ascending (1 :: 2 :: 3 :: [])) ::
       (sorted descending (1 :: 2 :: 3 :: [])) ::
       (sorted ascending (1 :: 3 :: 2 :: [])) ::
       (sorted ascending (1 :: 1 :: [])) ::
       (sorted descending (3 :: 2 :: 1 :: [])) ::
       (sorted ascending (%s)) ::
       [] |}
    large_input_str

let sorted_test = RunLiveCommon.parse_nexpr sorted_test_string
let run () = FromHazel.run_with_test ~program_name:"is_sorted" ~program_path ~steps_file ~test:sorted_test
