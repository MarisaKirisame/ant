let steps_file = "eval_steps_qs.json"
let program_path = "data/mk_QS.json"

let large_input =
  let rng = Random.State.make [| 42 |] in
  List.init 480 (fun _ -> Random.State.int rng 100)

let large_input_str =
  match large_input with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int large_input) ^ " :: []"

let qs_test_string =
  Printf.sprintf
    {| (my_quicksort ([])) ::
       (my_quicksort (3 :: 6 :: 1 :: 4 :: [])) ::
       (my_quicksort (5 :: 5 :: 1 :: 5 :: 2 :: [])) ::
       (my_quicksort (%s)) ::
       [] |}
    large_input_str

let qs_test = RunLiveCommon.parse_nexpr qs_test_string
let run () = FromHazel.run_with_test ~program_name:"qs" ~program_path ~steps_file ~test:qs_test
