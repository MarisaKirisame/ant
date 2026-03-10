open Ant
module TestCEK = TestCEK
module TestPlain = TestPlain
module Word = Ant.Word.Word

let rec list_to_string_cek l = match l with TestCEK.Nil -> "[]" | TestCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_cek tl
let rec list_to_string_plain l = match l with TestPlain.Nil -> "[]" | TestPlain.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_plain tl

let rec int_list_cek_of_list = function [] -> TestCEK.Nil | x :: xs -> TestCEK.Cons (x, int_list_cek_of_list xs)
let rec int_list_plain_of_list = function [] -> TestPlain.Nil | x :: xs -> TestPlain.Cons (x, int_list_plain_of_list xs)

let length = 65536

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
  let cek_list = TestCEK.from_ocaml_int_list (int_list_cek_of_list xs) in
  let plain_list = int_list_plain_of_list xs in
  Gc.full_major ();
  let result_cek = Profile.with_slot RunLiveCommon.eval_cek_slot (fun () -> TestCEK.list_incr memo cek_list) in
  Gc.full_major ();
  let _ = Profile.with_slot RunLiveCommon.eval_plain_slot (fun () -> TestPlain.list_incr plain_list) in

  let result_ocaml = TestCEK.to_ocaml_int_list result_cek.words in
  let profile_cek_json_dump = Profile.cek_profile |> Profile.dump_profile |> json_of_profile in
  let profile_plain_json_dump = Profile.plain_profile |> Profile.dump_profile |> json_of_profile in
  Printf.printf
    "%s -> result=%s (took %d steps, %d without memo), cek_profile=%s, plain_profile=%s\n"
    label
    (list_to_string_cek result_ocaml)
    result_cek.step
    result_cek.without_memo_step
    (Yojson.Safe.to_string profile_cek_json_dump)
    (Yojson.Safe.to_string profile_plain_json_dump);
  profile_cek_json_dump, profile_plain_json_dump

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
  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let (random_res_cek, random_res_plain) = run_case ~memo "Random" random_input in

  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let (low_entropy_res_cek, low_entropy_res_plain) = run_case ~memo "Low entropy" low_entropy_input in

  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let (repeated_res_cek, repeated_res_plain) = run_case ~memo "Repeated" repeated_input in

  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let _ = run_case ~memo "Random before remove" random_input in
  let (mod_res_cek, mod_res_plain) = run_case ~memo "Random after remove" random_input_removed in
  let (random_ns_cek, random_ns_plain) = (ns_of_result random_res_cek, ns_of_result random_res_plain) in
  let (low_entropy_ns_cek, low_entropy_ns_plain) = (ns_of_result low_entropy_res_cek, ns_of_result low_entropy_res_plain) in
  let (repeated_ns_cek, repeated_ns_plain) = (ns_of_result repeated_res_cek, ns_of_result repeated_res_plain) in
  let (mod_ns_cek, mod_ns_plain) = (ns_of_result mod_res_cek, ns_of_result mod_res_plain) in
  Printf.printf
{|\begin{tabular}{c|c|c|c|c}
            & Random & Low entropy & Modification & Repeated \\ \hline
Baseline ns & %i     & %i          & %i      & %i \\ \hline
Memo ns     & %i     & %i          & %i      & %i \\ \hline
Ratios      &        & %.3fx       & %.3fx   & %.3fx
\end{tabular}|}
    random_ns_plain
    low_entropy_ns_plain
    mod_ns_plain
    repeated_ns_plain
    random_ns_cek
    low_entropy_ns_cek
    mod_ns_cek
    repeated_ns_cek
    (float_of_int random_ns_cek /. float_of_int low_entropy_ns_cek)
    (float_of_int random_ns_cek /. float_of_int mod_ns_cek)
    (float_of_int random_ns_cek /. float_of_int repeated_ns_cek);
  ()