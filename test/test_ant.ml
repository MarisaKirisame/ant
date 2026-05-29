open Ant

module Dynarray = Stdlib.Dynarray

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

let singleton_pattern i = Pattern.pattern_cons (Pattern.PCon (Words.from_int i)) BatFingerTree.Generic.empty

let singleton_value i = Value.value_cons (Value.Words (Words.from_int i)) BatFingerTree.Generic.empty

let test_random_memo_eviction () =
  Memo.reset ();
  Memo.add_exp (fun _ -> ()) 0;
  let exp = Memo.pc_to_exp (Common.Pc 0) in
  let memo = Memo.init_memo () in
  for i = 0 to 9 do
    let src : Pattern.pattern State.cek = { c = exp; e = Dynarray.create (); k = singleton_pattern i } in
    let dst : Value.value State.cek = { c = exp; e = Dynarray.create (); k = singleton_value (i + 100) } in
    let step : State.step = { src; dst; sc = i + 1; hit = i; insert_time = i + 10 } in
    Memo.insert_step memo step
  done;
  assert (Memo.memo_size memo = 10);
  let evicted =
    Memo.evict_to_size memo ~random_state:(Stdlib.Random.State.make [| 0x5eed |]) ~size:4
  in
  assert (List.length evicted = 6);
  assert (Memo.memo_size memo = 4);
  assert ((Memo.memo_stats memo).node_counts.stem_nodes = 4);
  List.iter
    (fun (eviction : Memo.eviction) ->
      assert (eviction.evicted_pc = 0);
      assert (eviction.evicted_size = 1);
      assert (eviction.evicted_pvar_length = 0);
      assert (eviction.evicted_sc = eviction.evicted_step.sc);
      assert (eviction.evicted_hit_count = eviction.evicted_step.hit);
      assert (eviction.evicted_insert_time = eviction.evicted_step.insert_time))
    evicted;
  assert (Memo.evict_to_size memo ~size:10 = []);
  assert (Memo.memo_size memo = 4);
  assert (List.length (Memo.evict_to_size memo ~size:0) = 4);
  assert (Memo.memo_size memo = 0)

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
  test_mfvs ();
  test_random_memo_eviction ();
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
