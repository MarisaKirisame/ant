let steps_file = "eval_steps_append.json"
let program_path = "data/mk_Append.json"

let large_left, large_right =
  let rng = Random.State.make [| 42 |] in
  (List.init 80 (fun _ -> Random.State.int rng 100), List.init 80 (fun _ -> Random.State.int rng 100))

let list_to_cons_str xs = match xs with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int xs) ^ " :: []"

let append_test_string =
  Printf.sprintf
    {| (my_append ([]) ([])) ::
       (my_append (1 :: 2 :: 3 :: []) ([])) ::
       (my_append ([]) (4 :: 5 :: 6 :: [])) ::
       (my_append (%s) (%s)) ::
       [] |}
    (list_to_cons_str large_left) (list_to_cons_str large_right)

let append_test = RunLiveCommon.parse_nexpr append_test_string
let run () = FromHazel.run_with_test ~program_name:"append" ~program_path ~steps_file ~test:append_test
