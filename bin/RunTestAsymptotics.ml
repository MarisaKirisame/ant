open Ant
module ListCEK = ListCEK
module ListPlain = ListPlain
module Word = Ant.Word.Word

let rec to_listcek = function [] -> ListCEK.Nil | x :: xs -> ListCEK.Cons (x, to_listcek xs)

(* Random, without structure *)
let random_input length =
  let rng = Random.State.make [| 42 |] in
  List.init length (fun _ -> Random.State.int rng 100)

let rec remove_index l i = match l with [] -> [] | hd :: tl -> if i = 0 then tl else hd :: remove_index tl (i - 1)

let random_input_removed input length =
  let rng = Random.State.make [| 42 |] in
  remove_index input (Random.State.int rng length)

(* Repeating one number, very structured *)

let repeated_input length = List.init length (Fun.const 1)

(* Low entropy, semi-structured input *)

let low_entropy_input length =
  let rng = Random.State.make [| 42 |] in
  let block_length = max 1 (length |> float_of_int |> Float.sqrt |> int_of_float) in
  let block_count = max 1 ((length + block_length - 1) / block_length) in
  let blocks = Array.init 2 (fun _ -> List.init block_length (fun _ -> Random.State.int rng 100)) in
  let rec make_list remaining_blocks current_list =
    match remaining_blocks with
    | 0 -> List.take length current_list
    | _ ->
        let block = blocks.(Random.State.int rng 2) in
        make_list (remaining_blocks - 1) (List.append current_list block)
  in
  make_list block_count []

let json_of_profile entries = `List (List.map (fun (name, time) -> `List [ `String name; `Int time ]) entries)

let run_case ~(memo : State.memo) ~(label : string) ~(run_memo : State.memo -> Memo.exec_result)
    ~(run_cek : unit -> Memo.exec_result) ~(run_plain : unit -> 'a) =
  (* Memo *)
  Gc.full_major ();
  let result_memo = run_memo memo in
  (* CEK *)
  Gc.full_major ();
  let _ = Profile.with_slot RunLiveCommon.eval_cek_slot run_cek in
  (* Plain *)
  Gc.full_major ();
  let _ = Profile.with_slot RunLiveCommon.eval_plain_slot (fun () -> run_plain ()) in
  (* JSONs *)
  let memo_step = result_memo.step in
  let without_memo_step = result_memo.without_memo_step in
  let memo_json = Profile.memo_profile |> Profile.dump_profile |> json_of_profile in
  let cek_json = Profile.cek_profile |> Profile.dump_profile |> json_of_profile in
  let plain_json = Profile.plain_profile |> Profile.dump_profile |> json_of_profile in
  (memo_step, without_memo_step, memo_json, cek_json, plain_json)

let total_ns_of_json json =
  begin match json with
  | `List inner ->
      List.fold_left
        (fun sum elt ->
          match elt with
          | `List elt -> (
              match List.nth elt 1 with
              | `Int i -> sum + i
              | `String _ -> failwith "Read profile name in index 1 of json list"))
        0 inner
  end

let write_steps_json oc ~program_name ~input_kind memo_step without_memo_step memo_json cek_json plain_json =
  let json =
    `Assoc
      [
        ("name", `String "exec_time");
        ("step", `Int memo_step);
        ("without_memo_step", `Int without_memo_step);
        ("program_name", `String program_name);
        ("input_kind", `String input_kind);
        ("memo_profile", memo_json);
        ("plain_profile", plain_json);
        ("cek_profile", cek_json);
      ]
  in
  Yojson.Safe.to_string json |> output_string oc;
  output_char oc '\n';
  flush oc

let run_case_then_write ~memo ~program_name ~input_kind ~filename ~run_memo ~run_cek ~run_plain =
  let memo_steps, without_memo_steps, memo_json, cek_json, plain_json =
    run_case ~memo ~label:input_kind ~run_memo ~run_cek ~run_plain
  in
  RunLiveCommon.with_outchannel filename (fun oc ->
      write_steps_json oc ~program_name ~input_kind memo_steps without_memo_steps memo_json cek_json plain_json;
      RunLiveCommon.write_memo_stats_json oc memo);
  (total_ns_of_json memo_json, total_ns_of_json cek_json)

let filename program suffix length = Printf.sprintf "results/entropy/%s/%s/%d.json" program suffix length

let run_all_cases ~program_name ~populate_state ~case_fn ~length =
  let random_input = random_input length in
  let low_entropy_input = low_entropy_input length in
  let repeated_input = repeated_input length in
  let random_input_removed = random_input_removed random_input length in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let random_ns_memo, random_ns_cek = case_fn memo "random" random_input (filename program_name "random" length) in
  let mod_ns_memo, mod_ns_cek = case_fn memo "mod1" random_input_removed (filename program_name "mod1" length) in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let low_entropy_ns_memo, low_entropy_ns_cek =
    case_fn memo "block" low_entropy_input (filename program_name "block" length)
  in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let repeated_ns_memo, repeated_ns_cek = case_fn memo "same" repeated_input (filename program_name "same" length) in

  Printf.printf
    {|\begin{tabular}{c|c|c|c|c}
            & Random & Low entropy & Modification & Repeated \\ \hline
Baseline ns & %i     & %i          & %i      & %i \\ \hline
Memo ns     & %i     & %i          & %i      & %i \\ \hline
Ratios      &        & %.3fx       & %.3fx   & %.3fx
\end{tabular}|}
    random_ns_cek low_entropy_ns_cek mod_ns_cek repeated_ns_cek random_ns_memo low_entropy_ns_memo mod_ns_memo
    repeated_ns_memo
    (float_of_int random_ns_memo /. float_of_int low_entropy_ns_memo)
    (float_of_int random_ns_memo /. float_of_int mod_ns_memo)
    (float_of_int random_ns_memo /. float_of_int repeated_ns_memo);
  Printf.printf "\nProgram: %s (length: %d)\n%!" program_name length;
  ()

let run ~length () =
  if length <= 0 then invalid_arg "RunTestAsymptotics.run: length must be positive";
  run_all_cases ~length ~program_name:"map" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~program_name:"map" ~input_kind:label ~filename
        ~run_memo:(fun memo -> ListCEK.list_incr ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.list_incr ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.list_incr plain_list)));

  run_all_cases ~length ~program_name:"append" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~program_name:"append" ~input_kind:label ~filename
        ~run_memo:(fun memo -> ListCEK.append ~config:(Memo.memo_config memo) cek_list cek_list)
        ~run_cek:(fun () -> ListCEK.append ~config:Memo.cek_config cek_list cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.append plain_list plain_list)));

  run_all_cases ~length ~program_name:"merge_sort" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~program_name:"merge_sort" ~input_kind:label ~filename
        ~run_memo:(fun memo -> ListCEK.mergesort ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.mergesort ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.mergesort plain_list)));

  run_all_cases ~length ~program_name:"quick_sort" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~program_name:"quick_sort" ~input_kind:label ~filename
        ~run_memo:(fun memo -> ListCEK.quicksort ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.quicksort ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.quicksort plain_list)));

  run_all_cases ~length ~program_name:"simple_filter" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~program_name:"simple_filter" ~input_kind:label ~filename
        ~run_memo:(fun memo -> ListCEK.filter_pos ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.filter_pos ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.filter_pos plain_list)));

  run_all_cases ~length ~program_name:"pair" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~program_name:"pair" ~input_kind:label ~filename
        ~run_memo:(fun memo -> ListCEK.pair ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.pair ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.pair plain_list)));
  ()
