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
    then failwith "hash is not deterministic";
end

let _ =
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
