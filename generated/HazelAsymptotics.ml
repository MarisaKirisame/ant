
let program_path = "data/mk_map.json"

let list_to_string l = match l with [] -> "[]" | _ -> String.concat " :: " (List.map string_of_int l) ^ " :: []"

let length = 512

(* Random, without structure *)
let random_input =
  let rng = Random.State.make [| 42 |] in
  List.init length (fun _ -> Random.State.int rng 100)


(* Repeating one number, very structured *)

let repeated_input =
  List.init length (Fun.const 1)

(* Low entropy, semi-structured input *)

let low_entropy_input =
  let rng = Random.State.make [| 42 |] in
  let chunks = [
    List.init 7 (fun _ -> Random.State.int rng 100);
    List.init 8 (fun _ -> Random.State.int rng 100);
    List.init 8 (fun _ -> Random.State.int rng 100);
    List.init 9 (fun _ -> Random.State.int rng 100);
  ] in
  let rec make_list chunks_to_concat =
    match chunks_to_concat with
    | 0 -> []
    | n -> List.append (List.nth chunks (Random.State.int rng 4)) (make_list (n - 1))
  in
  make_list (length / 8)

let map_test_string input_str =
  Printf.sprintf
    {| my_map (fun x -> x + 1) (%s) |}
    input_str

let map_test input_str = RunLiveCommon.parse_nexpr (map_test_string input_str)

let test_metadatas = [
  ("eval_steps_asymptotic_random.json", map_test (list_to_string random_input));
  ("eval_steps_asymptotic_repeated.json", map_test (list_to_string repeated_input));
  ("eval_steps_asymptotic_low_entropy.json", map_test (list_to_string low_entropy_input));
]

let run () =
  List.iter
    (fun (steps_file, input_nexpr) ->
      FromHazel.run_with_test
        ~program_name:"map"
        ~program_path
        ~steps_file
        ~test:input_nexpr;
      Ant.Memo.reset ())
    test_metadatas