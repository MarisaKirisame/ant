open Ant

module TestMonoidHash (M : Hash.MonoidHash) = struct
  let test_hash () =
    let open M in
    let rec print_list = function [] -> "" | e :: l -> (string_of_int @@ hash e) ^ " " ^ print_list l in
    print_endline "Testing MonoidHash";
    print_endline name;
    let h1 = from_int 114514 in
    let h2 = mul h1 unit in
    let h3 = mul unit h1 in
    let init = hash h1 in
    let hlist = [ h1; h2; h3 ] in
    if List.exists (fun i -> hash i <> init) [ h1; h2; h3 ] then failwith ("hash is not idempotent " ^ print_list hlist);
    let l1 = List.init 10000 (fun _ -> Int64.to_int @@ Random.bits64 ()) in
    let l2 = List.init 10000 (fun _ -> Int64.to_int @@ Random.bits64 ()) in
    let list = l1 @ l2 in
    let foldl x = List.fold_left (fun acc i -> mul acc (from_int i)) unit x in
    let foldr x = List.fold_right (fun i acc -> mul (from_int i) acc) x unit in
    let foldx x = List.fold_left (fun acc i -> mul unit (mul (mul acc (from_int i)) unit)) unit x in
    let h4 = foldl list in
    let h5 = foldr list in
    let h6 = mul (foldl l1) (foldl l2) in
    let h7 = mul (foldr l1) (foldr l2) in
    let h8 = mul (foldl l1) (foldr l2) in
    let h9 = foldx list in
    let init = hash h4 in
    let hlist = [ h4; h5; h6; h7; h8; h9 ] in
    if List.exists (fun i -> hash i <> init) hlist then failwith ("hash is not associative " ^ print_list hlist);
    let random_assoc =
      let rec aux left = function
        | [] -> left
        | hd :: tl ->
            let current = from_int hd in
            if Random.bool () then aux (mul left current) tl else mul left (aux current tl)
      in
      aux unit
    in
    let h10 = random_assoc list in
    if hash h10 <> init then failwith "hash is not associative (random)";
    let gen_hlist_for_single x = List.init 1000 (fun _ -> from_int x) in
    if
      List.exists
        (fun i ->
          let hl = gen_hlist_for_single i in
          let init = hash @@ List.hd hl in
          List.exists (fun h -> hash h <> init) hl)
        list
    then failwith "hash is not deterministic"
end

let string_contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  if len_sub = 0 then true else loop 0

let find_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then None
    else if String.sub s i len_sub = sub then Some i
    else loop (i + 1)
  in
  if len_sub = 0 then Some 0 else loop 0

let extract_int_field line field =
  let marker = "\"" ^ field ^ "\":" in
  match find_substring line marker with
  | None -> None
  | Some idx ->
      let start = idx + String.length marker in
      let len = String.length line in
      let rec skip_spaces i =
        if i < len && (line.[i] = ' ' || line.[i] = '\t') then skip_spaces (i + 1) else i
      in
      let rec parse i acc seen =
        if i >= len then if seen then Some acc else None
        else
          let c = line.[i] in
          if c >= '0' && c <= '9' then parse (i + 1) (acc * 10 + (Char.code c - 48)) true
          else if seen then Some acc
          else None
      in
      parse (skip_spaces start) 0 false

let read_lines path =
  In_channel.with_open_text path (fun ic ->
      let rec loop acc =
        match In_channel.input_line ic with None -> List.rev acc | Some line -> loop (line :: acc)
      in
      loop [])

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then dir else find_repo_root parent

let steps_from_file path =
  let root =
    match Sys.getenv_opt "DUNE_SOURCEROOT" with Some root -> root | None -> find_repo_root (Sys.getcwd ())
  in
  let full_path = Filename.concat root path in
  assert (Sys.file_exists full_path);
  let lines = read_lines full_path in
  List.filter_map
    (fun line ->
      if string_contains line "\"name\":\"exec_time\"" then
        match (extract_int_field line "step", extract_int_field line "without_memo_step") with
        | Some step, Some without -> Some (step, without)
        | _ -> None
      else None)
    lines

let assert_first_steps path expected_step expected_without =
  match steps_from_file path with
  | (step, without) :: _ ->
      assert (step = expected_step);
      assert (without = expected_without)
  | [] -> assert false

let assert_contains_steps path expected_step expected_without =
  let steps = steps_from_file path in
  assert (List.exists (fun (step, without) -> step = expected_step && without = expected_without) steps)

let int_of_word_seq seq =
  match Memo.to_word seq with
  | Word.Word.Int v -> v
  | Word.Word.ConstructorTag _ -> failwith "expected int word"

let setup_exec_cek_program () =
  Words.reset ();
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Memo.reset ();
  Memo.add_exp Memo.exec_done 0;
  Memo.add_exp
    (fun w ->
      Memo.assert_env_length w 1;
      Memo.push_env w (Memo.from_int 42);
      w.state.k <- Memo.from_constructor 0;
      Memo.return_n w 2 (Memo.pc_to_exp (Common.int_to_pc 0)))
    1

let test_exec_cek_basic () =
  (* Component test (Memo.exec_cek):
     - Build a tiny CEK program that pushes a constant (42) then halts.
     - Run both exec_cek (memoized) and exec_cek_raw (non-memoized) and
       assert they produce the same final word.
     - Assert the memo counters are exactly (step=1, without_memo_step=1)
       for this fixed program. *)
  setup_exec_cek_program ();
  let c = Memo.pc_to_exp (Common.int_to_pc 1) in
  let e = Stdlib.Dynarray.of_list [ Memo.from_int 7 ] in
  let k = Memo.from_constructor 1 in
  let memo = Memo.init_memo () in
  let result = Memo.exec_cek c e k memo in
  let raw = Memo.exec_cek_raw c (Stdlib.Dynarray.of_list [ Memo.from_int 7 ]) (Memo.from_constructor 1) in
  assert (int_of_word_seq result.words = 42);
  assert (Memo.to_word result.words = Memo.to_word raw);
  assert (result.step = 1);
  assert (result.without_memo_step = 1)

let setup_compose_program () =
  Words.reset ();
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Memo.reset ();
  Memo.add_exp Memo.exec_done 0;
  Memo.add_exp
    (fun w ->
      Memo.assert_env_length w 1;
      Memo.push_env w (Memo.from_int 10);
      w.state.c <- Memo.pc_to_exp (Common.int_to_pc 2))
    1;
  Memo.add_exp
    (fun w ->
      Memo.assert_env_length w 2;
      w.state.k <- Memo.from_constructor 0;
      Memo.return_n w 2 (Memo.pc_to_exp (Common.int_to_pc 0)))
    2

let test_dependency_steps () =
  (* Component test (Dependency.make_step/step_through/compose_step):
     - Execute two concrete CEK steps manually to get two successive states.
     - make_step should capture each transition; step_through should replay it
       to the same concrete state.
     - compose_step should combine those two steps; replaying the composed
       step from the original state should match the two-step final state.
     - The composed step's sc must equal sc1 + sc2. *)
  setup_compose_program ();
  let memo = Memo.init_memo () in
  let start =
    { State.c = Memo.pc_to_exp (Common.int_to_pc 1); e = Stdlib.Dynarray.of_list [ Memo.from_int 0 ]; k = Memo.from_constructor 1 }
  in
  let old1 = State.copy_state start in
  let w1 = State.make_world start memo in
  start.c.step w1;
  let step1 = Dependency.make_step old1 w1.resolved memo in
  let replay1 = Dependency.step_through step1 old1 in
  assert (Dependency.state_equal replay1 w1.state);
  let old2 = State.copy_state w1.state in
  let w2 = State.make_world w1.state memo in
  w1.state.c.step w2;
  let step2 = Dependency.make_step old2 w2.resolved memo in
  let replay2 = Dependency.step_through step2 old2 in
  assert (Dependency.state_equal replay2 w2.state);
  let composed = Dependency.compose_step step1 step2 in
  assert (composed.sc = step1.sc + step2.sc);
  let replay_composed = Dependency.step_through composed old1 in
  assert (Dependency.state_equal replay_composed w2.state)

let test_live_eval_expression () =
  (* Component test (RunLiveCommon.eval_expression):
     - Evaluate the fixed expression (1 + 2) via the Live CEK pipeline.
     - Assert the returned value is VInt 3.
     - Capture the exec_result passed to write_steps and assert it is present.
     - Assert the memo counters are exactly (step=10, without_memo_step=11)
       for this expression with a fresh memo. *)
  RunLiveCommon.LC.populate_state ();
  let memo = Memo.init_memo () in
  let captured = ref None in
  let write_steps r = captured := Some r in
  let expr = RunLiveCommon.LC.EPlus (RunLiveCommon.LC.EInt 1, RunLiveCommon.LC.EInt 2) in
  let value = RunLiveCommon.eval_expression ~memo ~write_steps expr in
  (match value with RunLiveCommon.LC.VInt 3 -> () | _ -> assert false);
  match !captured with
  | None -> assert false
  | Some r ->
      assert (r.step = 10);
      assert (r.without_memo_step = 11)

let test_integration_steps () =
  (* Integration test (eval_steps_*.json fixtures):
     - Read the checked-in make run outputs and assert fixed step counts.
     - We check the first exec_time entry for each file to lock the baseline.
     - We also assert the presence of a specific hazel candidate (33, 1375). *)
  assert_first_steps "eval_steps_simple.json" 2 2;
  assert_first_steps "eval_steps_left_to_right.json" 6 6;
  assert_first_steps "eval_steps_demand_driven.json" 6 6;
  assert_first_steps "eval_steps_from_hazel.json" 74 315;
  assert_contains_steps "eval_steps_from_hazel.json" 33 1375

let _ =
  test_exec_cek_basic ();
  test_dependency_steps ();
  test_live_eval_expression ();
  test_integration_steps ();
  let x = Intmap.create 32 in
  for i = 0 to 9 do
    Intmap.add x i (i + 1)
  done;
  assert (Intmap.mem x 5);
  assert (Intmap.find x 5 = 6);
  assert (Intmap.find_opt x 5 = Some 6);
  assert (Intmap.length x = 10);
  assert (Intmap.fold (fun _k v acc -> acc + v) x 0 = 55);
  assert (Intmap.fold (fun k _v acc -> acc + k) x 0 = 45);
  Intmap.remove x 5;
  assert (not (Intmap.mem x 5));
  assert (Intmap.find_opt x 5 = None);
  assert (Intmap.length x = 9);
  let y = Intmap.create 8 in
  for i = 0 to 4 do
    Intmap.add y i (i * 2)
  done;
  assert (Intmap.length y = 5);
  Intmap.add y 3 99;
  assert (Intmap.find y 3 = 99);
  assert (Intmap.length y = 5);
  let iter_count = ref 0 in
  Intmap.iter (fun _k _v -> iter_count := !iter_count + 1) y;
  assert (!iter_count = 5);
  assert (Intmap.fold (fun _k v acc -> acc + v) y 0 = 113);
  let missing_raises =
    try
      let _ = Intmap.find y 999 in
      false
    with Not_found -> true
  in
  assert missing_raises;
  Intmap.clear y;
  assert (Intmap.length y = 0);
  assert (not (Intmap.mem y 3));
  assert (Intmap.find_opt y 3 = None);
  let iter_called = ref false in
  Intmap.iter (fun _k _v -> iter_called := true) y;
  assert (not !iter_called);
  Intmap.add y 1 10;
  Intmap.reset y;
  assert (Intmap.length y = 0);
  let z = Intmap.create 64 in
  let ref_tbl = Hashtbl.create 64 in
  let st = Random.State.make [| 0x13579 |] in
  let rand_key () = Random.State.int st 200 - 100 in
  let rand_val () = Random.State.int st 2000 - 1000 in
  for _ = 0 to 999 do
    let k = rand_key () in
    match Random.State.int st 4 with
    | 0 ->
        let v = rand_val () in
        Intmap.add z k v;
        Hashtbl.replace ref_tbl k v
    | 1 ->
        Intmap.remove z k;
        Hashtbl.remove ref_tbl k
    | 2 ->
        assert (Intmap.mem z k = Hashtbl.mem ref_tbl k);
        assert (Intmap.find_opt z k = Hashtbl.find_opt ref_tbl k)
    | _ ->
        if Hashtbl.mem ref_tbl k then assert (Intmap.find z k = Hashtbl.find ref_tbl k)
  done;
  assert (Intmap.length z = Hashtbl.length ref_tbl);
  let iter_count = ref 0 in
  Intmap.iter (fun _k _v -> iter_count := !iter_count + 1) z;
  assert (!iter_count = Hashtbl.length ref_tbl);
  Hashtbl.iter (fun k v -> assert (Intmap.find z k = v)) ref_tbl;
  let module SL2 = TestMonoidHash (Hash.SL2) in
  let module SL2Slow = TestMonoidHash (Hash.SL2Slow) in
  let module MCRC32C = TestMonoidHash (Hash.MCRC32C) in
  let module DebugHash = TestMonoidHash (Hash.DebugHash) in
  (* buggy when length > a threshold *)
  SL2.test_hash ();
  SL2Slow.test_hash ();
  MCRC32C.test_hash ();
  DebugHash.test_hash ()
