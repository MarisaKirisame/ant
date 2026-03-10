open Ant
module Test = TestCEK
module Word = Ant.Word.Word

let rec list_to_string l = match l with Test.Nil -> "[]" | Test.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string tl

let rec int_list_of_list = function [] -> Test.Nil | x :: xs -> Test.Cons (x, int_list_of_list xs)

let length = 512

(* Random, without structure *)
let random_input =
  let rng = Random.State.make [| 42 |] in
  List.init length (fun _ -> Random.State.int rng 100)

let rec remove_index l i =
  match l with
  | [] -> []
  | hd::tl -> if i = 0 then tl else hd :: remove_index tl (i - 1)

let random_input_removed =
  let rng = Random.State.make [| 42 |] in
  remove_index random_input (Random.State.int rng length)

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

let json_of_profile entries = `List (List.map (fun (name, time) -> `List [ `String name; `Int time ]) entries)

let run_case ~memo label xs =
  let seq_list = Test.from_ocaml_int_list (int_list_of_list xs) in
  let result = Profile.with_slot RunLiveCommon.eval_cek_slot (fun () -> Test.list_incr memo seq_list) in
  Gc.full_major ();
  let result_ocaml = Test.to_ocaml_int_list result.words in
  let profile_json_dump = Profile.cek_profile |> Profile.dump_profile |> json_of_profile in
  Printf.printf
    "%s -> result=%s (took %d steps, %d without memo), profile=%s\n"
    label
    (list_to_string result_ocaml)
    result.step
    result.without_memo_step
    (Yojson.Safe.to_string profile_json_dump);
  profile_json_dump

let ns_of_result res =
  begin match res with
  | `List inner ->
    begin match (List.hd inner) with
    | `List innerinner ->
      begin match (List.nth innerinner 1) with
      | `Int i -> i
      | `String _ -> failwith "Read profile name when trying to read ns"
      end
    end
  end

let run () =
  Test.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let random_res = run_case ~memo "Random" random_input in

  Test.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let low_entropy_res = run_case ~memo "Low entropy" low_entropy_input in

  Test.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let repeated_res = run_case ~memo "Repeated" repeated_input in

  Test.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let _ = run_case ~memo "Random before remove" random_input in
  let mod_res = run_case ~memo "Random after remove" random_input_removed in
  let random_ns = ns_of_result random_res in
  let low_entropy_ns = ns_of_result low_entropy_res in
  let repeated_ns = ns_of_result repeated_res in
  let mod_ns = ns_of_result mod_res in
  Printf.printf
{|\begin{tabular}{c|c|c|c|c}
        & Random & Low entropy & Modification & Repeated \\ \hline
Memo ns & %i     & %i          & %i     & %i \\ \hline
Ratios  &        & %.3fx        & %.3fx   & %.3fx
\end{tabular}|}
    random_ns
    low_entropy_ns
    mod_ns
    repeated_ns
    (float_of_int random_ns /. float_of_int low_entropy_ns)
    (float_of_int random_ns /. float_of_int mod_ns)
    (float_of_int random_ns /. float_of_int repeated_ns);
  ()