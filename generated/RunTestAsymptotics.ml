open Ant
module TestCEK = TestCEK
module TestPlain = TestPlain
module AppendCEK = AppendCEK
module AppendPlain = AppendPlain
module InsertionSortCEK = InsertionSortCEK
module InsertionSortPlain = InsertionSortPlain
module MergeSortCEK = MergeSortCEK
module MergeSortPlain = MergeSortPlain
module QuickSortCEK = QuickSortCEK
module QuickSortPlain = QuickSortPlain
module ReverseCEK = ReverseCEK
module ReversePlain = ReversePlain
module SimpleFilterCEK = SimpleFilterCEK
module SimpleFilterPlain = SimpleFilterPlain
module PairCEK = PairCEK
module PairPlain = PairPlain
module Word = Ant.Word.Word

let rec list_to_string_test_cek l =
  match l with TestCEK.Nil -> "[]" | TestCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_test_cek tl

let rec list_to_string_append_cek l =
  match l with
  | AppendCEK.Nil -> "[]"
  | AppendCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_append_cek tl

let rec list_to_string_insertion_sort_cek l =
  match l with
  | InsertionSortCEK.Nil -> "[]"
  | InsertionSortCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_insertion_sort_cek tl

let rec list_to_string_merge_sort_cek l =
  match l with
  | MergeSortCEK.Nil -> "[]"
  | MergeSortCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_merge_sort_cek tl

let rec list_to_string_quick_sort_cek l =
  match l with
  | QuickSortCEK.Nil -> "[]"
  | QuickSortCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_quick_sort_cek tl

let rec list_to_string_reverse_cek l =
  match l with
  | ReverseCEK.Nil -> "[]"
  | ReverseCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_reverse_cek tl

let rec list_to_string_simple_filter_cek l =
  match l with
  | SimpleFilterCEK.Nil -> "[]"
  | SimpleFilterCEK.Cons (hd, tl) -> string_of_int hd ^ " :: " ^ list_to_string_simple_filter_cek tl

let rec list_to_string_pair_cek l =
  match l with
  | PairCEK.NilP -> "[]"
  | PairCEK.ConsP (a, b, tl) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ") :: " ^ list_to_string_pair_cek tl

let rec int_list_test_cek_of_list = function
  | [] -> TestCEK.Nil
  | x :: xs -> TestCEK.Cons (x, int_list_test_cek_of_list xs)

let rec int_list_test_plain_of_list = function
  | [] -> TestPlain.Nil
  | x :: xs -> TestPlain.Cons (x, int_list_test_plain_of_list xs)

let rec int_list_append_cek_of_list = function
  | [] -> AppendCEK.Nil
  | x :: xs -> AppendCEK.Cons (x, int_list_append_cek_of_list xs)

let rec int_list_append_plain_of_list = function
  | [] -> AppendPlain.Nil
  | x :: xs -> AppendPlain.Cons (x, int_list_append_plain_of_list xs)

let rec int_list_insertion_sort_cek_of_list = function
  | [] -> InsertionSortCEK.Nil
  | x :: xs -> InsertionSortCEK.Cons (x, int_list_insertion_sort_cek_of_list xs)

let rec int_list_insertion_sort_plain_of_list = function
  | [] -> InsertionSortPlain.Nil
  | x :: xs -> InsertionSortPlain.Cons (x, int_list_insertion_sort_plain_of_list xs)

let rec int_list_merge_sort_cek_of_list = function
  | [] -> MergeSortCEK.Nil
  | x :: xs -> MergeSortCEK.Cons (x, int_list_merge_sort_cek_of_list xs)

let rec int_list_merge_sort_plain_of_list = function
  | [] -> MergeSortPlain.Nil
  | x :: xs -> MergeSortPlain.Cons (x, int_list_merge_sort_plain_of_list xs)

let rec int_list_quick_sort_cek_of_list = function
  | [] -> QuickSortCEK.Nil
  | x :: xs -> QuickSortCEK.Cons (x, int_list_quick_sort_cek_of_list xs)

let rec int_list_quick_sort_plain_of_list = function
  | [] -> QuickSortPlain.Nil
  | x :: xs -> QuickSortPlain.Cons (x, int_list_quick_sort_plain_of_list xs)

let rec int_list_reverse_cek_of_list = function
  | [] -> ReverseCEK.Nil
  | x :: xs -> ReverseCEK.Cons (x, int_list_reverse_cek_of_list xs)

let rec int_list_reverse_plain_of_list = function
  | [] -> ReversePlain.Nil
  | x :: xs -> ReversePlain.Cons (x, int_list_reverse_plain_of_list xs)

let rec int_list_simple_filter_cek_of_list = function
  | [] -> SimpleFilterCEK.Nil
  | x :: xs -> SimpleFilterCEK.Cons (x, int_list_simple_filter_cek_of_list xs)

let rec int_list_simple_filter_plain_of_list = function
  | [] -> SimpleFilterPlain.Nil
  | x :: xs -> SimpleFilterPlain.Cons (x, int_list_simple_filter_plain_of_list xs)

let rec int_list_pair_cek_of_list = function
  | [] -> PairCEK.Nil
  | x :: xs -> PairCEK.Cons (x, int_list_pair_cek_of_list xs)

let rec int_list_pair_plain_of_list = function
  | [] -> PairPlain.Nil
  | x :: xs -> PairPlain.Cons (x, int_list_pair_plain_of_list xs)

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

(* let rec list_to_string l = match l with [] -> "[]" | hd :: tl -> string_of_int hd ^ " :: " ^ list_to_string tl *)
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

let run_case ~(memo : State.memo) ~(label : string) ~(entry_pc : int) ~(cek_args : Value.value list)
    ~(tag_cont_done : int) ~(run_memo : State.memo -> Memo.exec_result) ~(run_plain : unit -> 'a)
    ~(result_of_cek : Value.value -> 'b) ~(result_to_string : 'b -> string) =
  (* Memo *)
  Gc.full_major ();
  let result_memo = run_memo memo in
  (* CEK *)
  Gc.full_major ();
  let _ =
    Profile.with_slot RunLiveCommon.eval_cek_slot (fun () ->
        Memo.exec_cek_raw
          (Memo.pc_to_exp (Common.int_to_pc entry_pc))
          (Dynarray.of_list cek_args) (Memo.from_constructor tag_cont_done))
  in
  (* Plain *)
  Gc.full_major ();
  let _ = Profile.with_slot RunLiveCommon.eval_plain_slot (fun () -> run_plain ()) in
  (* JSONs *)
  let result_ocaml = result_of_cek result_memo.words in
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

let run_case_then_write ~memo ~label ~filename ~entry_pc ~tag_cont_done ~cek_args ~run_memo ~run_plain ~result_of_cek
    ~result_to_string =
  let memo_steps, without_memo_steps, memo_json, cek_json, plain_json =
    run_case ~memo ~label ~entry_pc ~tag_cont_done ~cek_args ~run_memo ~run_plain ~result_of_cek ~result_to_string
  in
  RunLiveCommon.with_outchannel filename (fun oc ->
      write_steps_json oc memo_steps without_memo_steps memo_json cek_json plain_json;
      RunLiveCommon.write_memo_stats_json oc memo);
  (total_ns_of_json memo_json, total_ns_of_json cek_json)

let filename program suffix = Printf.sprintf "eval_steps_asymptotic_%s_%s.json" program suffix

let run_all_cases ~program_name ~populate_state ~case_fn ~warmup_fn ~length =
  let random_input = random_input length in
  let low_entropy_input = low_entropy_input length in
  let repeated_input = repeated_input length in
  let random_input_removed = random_input_removed random_input length in
  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let random_ns_memo, random_ns_cek = case_fn memo "Random" random_input (filename program_name "random") in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let low_entropy_ns_memo, low_entropy_ns_cek =
    case_fn memo "Low entropy" low_entropy_input (filename program_name "low_entropy")
  in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  let repeated_ns_memo, repeated_ns_cek = case_fn memo "Repeated" repeated_input (filename program_name "repeated") in

  populate_state ();
  let memo = Ant.Memo.init_memo () in
  warmup_fn memo random_input;
  let mod_ns_memo, mod_ns_cek = case_fn memo "Random after remove" random_input_removed (filename program_name "mod") in

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
  run_all_cases ~length:65536 ~program_name:"map" ~populate_state:TestCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = TestCEK.from_ocaml_int_list (int_list_test_cek_of_list xs) in
      let plain_list = int_list_test_plain_of_list xs in
      run_case_then_write ~memo ~label ~filename ~entry_pc:1 ~tag_cont_done:TestCEK.tag_cont_done ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> TestCEK.list_incr memo cek_list)
        ~run_plain:(fun () -> ignore (TestPlain.list_incr plain_list))
        ~result_of_cek:TestCEK.to_ocaml_int_list ~result_to_string:list_to_string_test_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = TestCEK.from_ocaml_int_list (int_list_test_cek_of_list xs) in
      let plain_list = int_list_test_plain_of_list xs in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:1 ~tag_cont_done:TestCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> TestCEK.list_incr memo cek_list)
          ~run_plain:(fun () -> ignore (TestPlain.list_incr plain_list))
          ~result_of_cek:TestCEK.to_ocaml_int_list ~result_to_string:list_to_string_test_cek
      in
      ());

  run_all_cases ~length:65536 ~program_name:"append" ~populate_state:AppendCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = AppendCEK.from_ocaml_int_list (int_list_append_cek_of_list xs) in
      let plain_list = int_list_append_plain_of_list xs in
      let append_value = 1 in
      let append_value_cek = Memo.from_int append_value in
      run_case_then_write ~memo ~label ~filename ~entry_pc:1 ~tag_cont_done:AppendCEK.tag_cont_done
        ~cek_args:[ cek_list; append_value_cek ]
        ~run_memo:(fun memo -> AppendCEK.append memo cek_list append_value_cek)
        ~run_plain:(fun () -> ignore (AppendPlain.append plain_list append_value))
        ~result_of_cek:AppendCEK.to_ocaml_int_list ~result_to_string:list_to_string_append_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = AppendCEK.from_ocaml_int_list (int_list_append_cek_of_list xs) in
      let plain_list = int_list_append_plain_of_list xs in
      let append_value = 1 in
      let append_value_cek = Memo.from_int append_value in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:1 ~tag_cont_done:AppendCEK.tag_cont_done
          ~cek_args:[ cek_list; append_value_cek ]
          ~run_memo:(fun memo -> AppendCEK.append memo cek_list append_value_cek)
          ~run_plain:(fun () -> ignore (AppendPlain.append plain_list append_value))
          ~result_of_cek:AppendCEK.to_ocaml_int_list ~result_to_string:list_to_string_append_cek
      in
      ());

  (*run_all_cases ~length:1024 ~program_name:"insertion_sort" ~populate_state:InsertionSortCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = InsertionSortCEK.from_ocaml_int_list (int_list_insertion_sort_cek_of_list xs) in
      let plain_list = int_list_insertion_sort_plain_of_list xs in
      run_case_then_write ~memo ~label ~filename ~entry_pc:5 ~tag_cont_done:InsertionSortCEK.tag_cont_done
        ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> InsertionSortCEK.insertion_sort memo cek_list)
        ~run_plain:(fun () -> ignore (InsertionSortPlain.insertion_sort plain_list))
        ~result_of_cek:InsertionSortCEK.to_ocaml_int_list ~result_to_string:list_to_string_insertion_sort_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = InsertionSortCEK.from_ocaml_int_list (int_list_insertion_sort_cek_of_list xs) in
      let plain_list = int_list_insertion_sort_plain_of_list xs in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:5 ~tag_cont_done:InsertionSortCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> InsertionSortCEK.insertion_sort memo cek_list)
          ~run_plain:(fun () -> ignore (InsertionSortPlain.insertion_sort plain_list))
          ~result_of_cek:InsertionSortCEK.to_ocaml_int_list ~result_to_string:list_to_string_insertion_sort_cek
      in
      ());

  run_all_cases ~length:1024 ~program_name:"merge_sort" ~populate_state:MergeSortCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = MergeSortCEK.from_ocaml_int_list (int_list_merge_sort_cek_of_list xs) in
      let plain_list = int_list_merge_sort_plain_of_list xs in
      run_case_then_write ~memo ~label ~filename ~entry_pc:11 ~tag_cont_done:MergeSortCEK.tag_cont_done
        ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> MergeSortCEK.mergesort memo cek_list)
        ~run_plain:(fun () -> ignore (MergeSortPlain.mergesort plain_list))
        ~result_of_cek:MergeSortCEK.to_ocaml_int_list ~result_to_string:list_to_string_merge_sort_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = MergeSortCEK.from_ocaml_int_list (int_list_merge_sort_cek_of_list xs) in
      let plain_list = int_list_merge_sort_plain_of_list xs in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:11 ~tag_cont_done:MergeSortCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> MergeSortCEK.mergesort memo cek_list)
          ~run_plain:(fun () -> ignore (MergeSortPlain.mergesort plain_list))
          ~result_of_cek:MergeSortCEK.to_ocaml_int_list ~result_to_string:list_to_string_merge_sort_cek
      in
      ());*)

  (* run_all_cases
    ~program_name:"quick_sort"
    ~populate_state:QuickSortCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = QuickSortCEK.from_ocaml_int_list (int_list_quick_sort_cek_of_list xs) in
      let plain_list = int_list_quick_sort_plain_of_list xs in
      run_case_then_write
        ~memo
        ~label
        ~filename
        ~entry_pc:15
        ~tag_cont_done:QuickSortCEK.tag_cont_done
        ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> QuickSortCEK.quicksort memo cek_list)
        ~run_plain:(fun () -> ignore (QuickSortPlain.quicksort plain_list))
        ~result_of_cek:QuickSortCEK.to_ocaml_int_list
        ~result_to_string:list_to_string_quick_sort_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = QuickSortCEK.from_ocaml_int_list (int_list_quick_sort_cek_of_list xs) in
      let plain_list = int_list_quick_sort_plain_of_list xs in
      let _ =
        run_case
          ~memo
          ~label:"Random before remove"
          ~entry_pc:15
          ~tag_cont_done:QuickSortCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> QuickSortCEK.quicksort memo cek_list)
          ~run_plain:(fun () -> ignore (QuickSortPlain.quicksort plain_list))
          ~result_of_cek:QuickSortCEK.to_ocaml_int_list
          ~result_to_string:list_to_string_quick_sort_cek
      in
      ()); *)

  (*run_all_cases ~length:1024 ~program_name:"reverse" ~populate_state:ReverseCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = ReverseCEK.from_ocaml_int_list (int_list_reverse_cek_of_list xs) in
      let plain_list = int_list_reverse_plain_of_list xs in
      run_case_then_write ~memo ~label ~filename ~entry_pc:3 ~tag_cont_done:ReverseCEK.tag_cont_done
        ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> ReverseCEK.reverse memo cek_list)
        ~run_plain:(fun () -> ignore (ReversePlain.reverse plain_list))
        ~result_of_cek:ReverseCEK.to_ocaml_int_list ~result_to_string:list_to_string_reverse_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = ReverseCEK.from_ocaml_int_list (int_list_reverse_cek_of_list xs) in
      let plain_list = int_list_reverse_plain_of_list xs in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:3 ~tag_cont_done:ReverseCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> ReverseCEK.reverse memo cek_list)
          ~run_plain:(fun () -> ignore (ReversePlain.reverse plain_list))
          ~result_of_cek:ReverseCEK.to_ocaml_int_list ~result_to_string:list_to_string_reverse_cek
      in
      ());*)
  run_all_cases ~length:65536 ~program_name:"simple_filter" ~populate_state:SimpleFilterCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = SimpleFilterCEK.from_ocaml_int_list (int_list_simple_filter_cek_of_list xs) in
      let plain_list = int_list_simple_filter_plain_of_list xs in
      run_case_then_write ~memo ~label ~filename ~entry_pc:1 ~tag_cont_done:SimpleFilterCEK.tag_cont_done
        ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> SimpleFilterCEK.filter_pos memo cek_list)
        ~run_plain:(fun () -> ignore (SimpleFilterPlain.filter_pos plain_list))
        ~result_of_cek:SimpleFilterCEK.to_ocaml_int_list ~result_to_string:list_to_string_simple_filter_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = SimpleFilterCEK.from_ocaml_int_list (int_list_simple_filter_cek_of_list xs) in
      let plain_list = int_list_simple_filter_plain_of_list xs in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:1 ~tag_cont_done:SimpleFilterCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> SimpleFilterCEK.filter_pos memo cek_list)
          ~run_plain:(fun () -> ignore (SimpleFilterPlain.filter_pos plain_list))
          ~result_of_cek:SimpleFilterCEK.to_ocaml_int_list ~result_to_string:list_to_string_simple_filter_cek
      in
      ());

  run_all_cases ~length:65536 ~program_name:"pair" ~populate_state:PairCEK.populate_state
    ~case_fn:(fun memo label xs filename ->
      let cek_list = PairCEK.from_ocaml_int_list (int_list_pair_cek_of_list xs) in
      let plain_list = int_list_pair_plain_of_list xs in
      run_case_then_write ~memo ~label ~filename ~entry_pc:1 ~tag_cont_done:PairCEK.tag_cont_done ~cek_args:[ cek_list ]
        ~run_memo:(fun memo -> PairCEK.pair memo cek_list)
        ~run_plain:(fun () -> ignore (PairPlain.pair plain_list))
        ~result_of_cek:PairCEK.to_ocaml_int_pair_list ~result_to_string:list_to_string_pair_cek)
    ~warmup_fn:(fun memo xs ->
      let cek_list = PairCEK.from_ocaml_int_list (int_list_pair_cek_of_list xs) in
      let plain_list = int_list_pair_plain_of_list xs in
      let _ =
        run_case ~memo ~label:"Random before remove" ~entry_pc:1 ~tag_cont_done:PairCEK.tag_cont_done
          ~cek_args:[ cek_list ]
          ~run_memo:(fun memo -> PairCEK.pair memo cek_list)
          ~run_plain:(fun () -> ignore (PairPlain.pair plain_list))
          ~result_of_cek:PairCEK.to_ocaml_int_pair_list ~result_to_string:list_to_string_pair_cek
      in
      ());
  ()
