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
  (* Memo *)
  Gc.full_major ();
  let _ = TestCEK.list_incr memo cek_list in
  (* CEK *)
  Gc.full_major ();
  let result_cek = Profile.with_slot RunLiveCommon.eval_cek_slot (fun () ->
    Memo.exec_cek_raw
      (Memo.pc_to_exp (Common.int_to_pc 1))
      (Dynarray.of_list [ cek_list ])
      (Memo.from_constructor TestCEK.tag_cont_done))
  in
  (* Plain *)
  Gc.full_major ();
  let _ = Profile.with_slot RunLiveCommon.eval_plain_slot (fun () -> TestPlain.list_incr plain_list) in
  (* JSONs *)
  let result_ocaml = TestCEK.to_ocaml_int_list result_cek in
  let memo_json = Profile.memo_profile |> Profile.dump_profile |> json_of_profile in
  let cek_json = Profile.cek_profile |> Profile.dump_profile |> json_of_profile in
  let plain_json = Profile.plain_profile |> Profile.dump_profile |> json_of_profile in
  Printf.printf
    "%s -> result=%s, memo_profile=%s, cek_profile=%s, plain_profile=%s\n"
    label
    (list_to_string_cek result_ocaml)
    (Yojson.Safe.to_string memo_json)
    (Yojson.Safe.to_string cek_json)
    (Yojson.Safe.to_string plain_json);
  memo_json, cek_json, plain_json

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

let write_memo_stats_json filename memo =
  RunLiveCommon.with_outchannel filename (fun oc -> RunLiveCommon.write_memo_stats_json oc memo)

let write_steps_json oc (r : Memo.exec_result) : unit =
  let json_of_profile entries = `List (List.map (fun (name, time) -> `List [ `String name; `Int time ]) entries) in
  let json =
    `Assoc
      [
        ("name", `String "exec_time");
        ("step", `Int r.step);
        ("without_memo_step", `Int r.without_memo_step);
        ("memo_profile", Profile.dump_profile Profile.memo_profile |> json_of_profile);
        ("plain_profile", Profile.dump_profile Profile.plain_profile |> json_of_profile);
        ("cek_profile", Profile.dump_profile Profile.cek_profile |> json_of_profile);
      ]
  in
  Yojson.Safe.to_string json |> output_string oc;
  output_char oc '\n';
  flush oc

let write_steps_json filename exec_result =
  RunLiveCommon.with_outchannel filename (fun oc -> RunLiveCommon.write_steps_json oc exec_result)

let run () =
  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let (random_res_memo, random_res_cek) = run_case ~memo "Random" random_input in
  write_memo_stats_json "memo_stats_asymptotic_random.json" memo;

  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let (low_entropy_res_memo, low_entropy_res_cek) = run_case ~memo "Low entropy" low_entropy_input in
  write_memo_stats_json "memo_stats_asymptotic_low_entropy.json" memo;

  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let (repeated_res_memo, repeated_res_cek) = run_case ~memo "Repeated" repeated_input in
  write_memo_stats_json "memo_stats_asymptotic_repeated.json" memo;

  TestCEK.populate_state ();
  let memo = Ant.Memo.init_memo () in
  let _ = run_case ~memo "Random before remove" random_input in
  let (mod_res_memo, mod_res_cek) = run_case ~memo "Random after remove" random_input_removed in
  write_memo_stats_json "memo_stats_asymptotic_mod.json" memo;

  let (random_ns_memo, random_ns_cek) = (random_res_memo, random_res_cek) in
  let (low_entropy_ns_memo, low_entropy_ns_cek) = (low_entropy_res_memo, low_entropy_res_cek) in
  let (repeated_ns_memo, repeated_ns_cek) = (repeated_res_memo, repeated_res_cek) in
  let (mod_ns_memo, mod_ns_cek) = (mod_res_memo, mod_res_cek) in
  Printf.printf
{|\begin{tabular}{c|c|c|c|c}
            & Random & Low entropy & Modification & Repeated \\ \hline
Baseline ns & %i     & %i          & %i      & %i \\ \hline
Memo ns     & %i     & %i          & %i      & %i \\ \hline
Ratios      &        & %.3fx       & %.3fx   & %.3fx
\end{tabular}|}
    random_ns_cek
    low_entropy_ns_cek
    mod_ns_cek
    repeated_ns_cek
    random_ns_memo
    low_entropy_ns_memo
    mod_ns_memo
    repeated_ns_memo
    (float_of_int random_ns_memo /. float_of_int low_entropy_ns_memo)
    (float_of_int random_ns_memo /. float_of_int mod_ns_memo)
    (float_of_int random_ns_memo /. float_of_int repeated_ns_memo);
  ()