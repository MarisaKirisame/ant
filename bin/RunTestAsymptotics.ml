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

  let max_chunk_length = length |> float_of_int |> Float.cbrt |> int_of_float in
  let rec generate_chunks rng n =
    match n with 0 -> [] | n -> (n, List.init n (fun _ -> Random.State.int rng 100)) :: generate_chunks rng (n - 1)
  in
  let chunks = generate_chunks rng max_chunk_length in
  let rec make_list current_size current_list =
    match current_size > length with
    | true -> List.take length current_list
    | false ->
        let random_index = Random.State.int rng max_chunk_length in
        let n, l = List.nth chunks random_index in
        make_list (current_size + n) (List.append current_list l)
  in
  make_list 0 []

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

let write_steps_json oc memo_step without_memo_step memo_json cek_json plain_json =
  let json =
    `Assoc
      [
        ("name", `String "exec_time");
        ("step", `Int memo_step);
        ("without_memo_step", `Int without_memo_step);
        ("memo_profile", memo_json);
        ("plain_profile", plain_json);
        ("cek_profile", cek_json);
      ]
  in
  Yojson.Safe.to_string json |> output_string oc;
  output_char oc '\n';
  flush oc

let run_case_then_write ~memo ~label ~filename ~run_memo ~run_cek ~run_plain =
  let memo_steps, without_memo_steps, memo_json, cek_json, plain_json =
    run_case ~memo ~label ~run_memo ~run_cek ~run_plain
  in
  RunLiveCommon.with_outchannel filename (fun oc ->
      write_steps_json oc memo_steps without_memo_steps memo_json cek_json plain_json;
      RunLiveCommon.write_memo_stats_json oc memo);
  (total_ns_of_json memo_json, total_ns_of_json cek_json)

let filename program suffix = Printf.sprintf "eval_steps_asymptotic_%s_%s.json" program suffix

let run_all_cases ~program_name ~populate_state ~case_fn ~length =
  let random_input = random_input length in
  let low_entropy_input = low_entropy_input length in
  let repeated_input = repeated_input length in
  let random_input_removed = random_input_removed random_input length in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let random_ns_memo, random_ns_cek = case_fn memo "Random" random_input (filename program_name "random") in
  let mod_ns_memo, mod_ns_cek = case_fn memo "Random after remove" random_input_removed (filename program_name "mod") in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let low_entropy_ns_memo, low_entropy_ns_cek =
    case_fn memo "Low entropy" low_entropy_input (filename program_name "low_entropy")
  in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let repeated_ns_memo, repeated_ns_cek = case_fn memo "Repeated" repeated_input (filename program_name "repeated") in

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
  Printf.printf "\nProgram: %s\n%!" program_name;
  ()

let run () =
  run_all_cases ~length:65536 ~program_name:"map" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.list_incr ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.list_incr ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.list_incr plain_list)));

  run_all_cases ~length:65536 ~program_name:"append" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.append ~config:(Memo.memo_config memo) cek_list cek_list)
        ~run_cek:(fun () -> ListCEK.append ~config:Memo.cek_config cek_list cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.append plain_list plain_list)));

  run_all_cases ~length:1024 ~program_name:"insertion_sort" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.insertion_sort ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.insertion_sort ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.insertion_sort plain_list)));

  run_all_cases ~length:8192 ~program_name:"merge_sort" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.mergesort ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.mergesort ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.mergesort plain_list)));

  (*run_all_cases ~length:8192 ~program_name:"quick_sort" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.quicksort ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.quicksort ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.quicksort plain_list)));*)
  run_all_cases ~length:1024 ~program_name:"reverse" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.reverse ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.reverse ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.reverse plain_list)));

  run_all_cases ~length:65536 ~program_name:"simple_filter" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.filter_pos ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.filter_pos ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.filter_pos plain_list)));

  run_all_cases ~length:65536 ~program_name:"pair" ~populate_state:ListCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ListCEK.from_ocaml_int_list (to_listcek xs) in
      let plain_list = to_listcek xs in
      run_case_then_write ~memo ~label ~filename
        ~run_memo:(fun memo -> ListCEK.pair ~config:(Memo.memo_config memo) cek_list)
        ~run_cek:(fun () -> ListCEK.pair ~config:Memo.cek_config cek_list)
        ~run_plain:(fun () -> ignore (ListPlain.pair plain_list)));
  ()
