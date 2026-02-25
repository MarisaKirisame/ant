let steps_file = "eval_steps_map.json"
let program_path = "data/mk_map.json"
let large_input = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input
let large_input = large_input @ large_input

let large_input_str =
  match large_input with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int large_input) ^ " :: []"

let map_test_string =
  Printf.sprintf
    {| (my_map (fun x -> x + 1) ([])) ::
       (my_map (fun x -> x + 1) (1 :: 2 :: 3 :: [])) ::
       (my_map (fun x -> x + x) (1 :: 2 :: 3 :: [])) ::
       (my_map (fun x -> x + 1) (%s)) ::
       [] |}
    large_input_str

let map_test = RunLiveCommon.parse_nexpr map_test_string
let run () = FromHazel.run_with_test ~program_name:"map" ~program_path ~steps_file ~test:map_test
