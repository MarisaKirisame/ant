open Ant

let test_eviction_opt_in () =
  Words.reset ();
  Words.set_constructor_degree 0 1;
  Memo.reset ();
  let done_exp : State.exp = { pc = 0; step = (fun _ -> failwith "already done") } in
  let executed = ref 0 in
  let step (w : State.world) =
    incr executed;
    let word, _ = Memo.resolve w (Reference.Source.E 0) in
    (match word with Word.Word.Int n -> Dynarray.set w.state.e 0 (Memo.from_int (n + 1)) | _ -> assert false);
    w.state.c <- done_exp
  in
  Memo.add_exp done_exp.step 0;
  Memo.add_exp step 1;
  let start = Memo.pc_to_exp (Common.Pc 1) in
  let memo = Memo.init_memo () in
  let run ?(input = 41) config =
    let result = Memo.exec_cek ?config start (Dynarray.of_list [ Memo.from_int input ]) (Memo.from_constructor 0) in
    assert (Memo.to_word result.words = Word.Word.Int (input + 1));
    assert (result.step = 1 && result.without_memo_step = 1)
  in
  run None;
  run (Some Memo.cek_config);
  run (Some (Memo.memo_config memo));
  let executed_after_first = !executed in
  run (Some (Memo.memo_config memo));
  assert (!executed = executed_after_first);
  run (Some (Memo.memo_config memo));
  assert (memo.size > 0 && memo.epoch = 3);
  assert (memo.eviction_state.batch_count = 0);
  assert (memo.eviction_state.evict_count = 0);
  assert (memo.eviction_state.max_tree_size_seen = 0);
  let eviction_memo = Memo.init_memo () in
  run ~input:42 (Some (Memo.memo_config eviction_memo));
  run ~input:43 (Some (Memo.memo_config eviction_memo));
  run (Some (Memo.memo_config ~evict:true eviction_memo));
  assert (eviction_memo.eviction_state.batch_count = 1);
  assert (eviction_memo.eviction_state.evict_count = 1);
  assert (eviction_memo.eviction_state.max_tree_size_seen > 0);
  assert (eviction_memo.size < eviction_memo.eviction_state.max_tree_size_seen);
  (* An ordinary call must not run eviction even after the study opted in. *)
  let batches = eviction_memo.eviction_state.batch_count in
  run (Some (Memo.memo_config eviction_memo));
  assert (eviction_memo.eviction_state.batch_count = batches);
  Memo.reset ();
  Words.reset ()

let assert_same_set expected actual =
  let normalize = List.sort compare in
  assert (normalize expected = normalize actual)

let assert_acyclic_after_removing edges removed =
  let removed = List.fold_left (fun set vertex -> Mfvs.IntSet.add vertex set) Mfvs.IntSet.empty removed in
  let remaining =
    List.filter (fun (src, dst) -> (not (Mfvs.IntSet.mem src removed)) && not (Mfvs.IntSet.mem dst removed)) edges
  in
  assert (Mfvs.find_directed_cycle remaining = None)

let test_mfvs () =
  assert_same_set [] (Mfvs.solve []);
  assert_same_set [ 0 ] (Mfvs.solve [ (0, 0) ]);
  let triangle = [ (0, 1); (1, 2); (2, 0) ] in
  let triangle_solution = Mfvs.solve triangle in
  assert (List.length triangle_solution = 1);
  assert_acyclic_after_removing triangle triangle_solution;
  let shared_hub = [ (0, 1); (1, 2); (2, 0); (0, 3); (3, 4); (4, 0) ] in
  assert_same_set [ 0 ] (Mfvs.solve shared_hub);
  let product_beats_sum = [ (0, 1); (1, 2); (2, 0); (2, 3); (3, 2); (2, 4); (4, 2); (0, 5); (6, 0); (7, 0); (8, 0) ] in
  let by_sum = Mfvs.solve_with Mfvs.degree_sum product_beats_sum in
  let by_product = Mfvs.solve_with Mfvs.degree_product product_beats_sum in
  assert (List.length by_product < List.length by_sum);
  assert_same_set by_product (Mfvs.solve product_beats_sum);
  assert_acyclic_after_removing product_beats_sum by_product

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

let _ =
  test_eviction_opt_in ();
  test_mfvs ();
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
  let module SL2 = TestMonoidHash (Hash.SL2) in
  let module SL2Slow = TestMonoidHash (Hash.SL2Slow) in
  let module MCRC32C = TestMonoidHash (Hash.MCRC32C) in
  let module DebugHash = TestMonoidHash (Hash.DebugHash) in
  (* buggy when length > a threshold *)
  SL2.test_hash ();
  SL2Slow.test_hash ();
  MCRC32C.test_hash ();
  DebugHash.test_hash ()
