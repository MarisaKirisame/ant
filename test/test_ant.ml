open Ant

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
  let _ =
    let open Hash.SL2 in
    let hash1 = from_int 114514 in
    let hash2 = mul hash1 unit in
    let hash3 = mul unit hash1 in
    let init = hash hash1 in
    assert (List.for_all (fun i -> hash i = init) [ hash2; hash3 ]);
    let l1 = List.init 200 (fun _ -> Int64.to_int @@ Random.bits64 ()) in
    let l2 = List.init 200 (fun _ -> Int64.to_int @@ Random.bits64 ()) in
    let list = l1 @ l2 in
    let foldl x = List.fold_left (fun acc i -> mul acc (from_int i)) unit x in
    let foldr x = List.fold_right (fun i acc -> mul (from_int i) acc) x unit in
    let hash4 = foldl list in
    let hash5 = foldr list in
    let hash6 = mul (foldl l1) (foldl l2) in
    let hash7 = mul (foldr l1) (foldr l2) in
    let hash8 = mul (foldl l1) (foldr l2) in
    let init = hash hash4 in
    assert (List.for_all (fun i -> hash i = init) [ hash4; hash5; hash6; hash7; hash8 ])
  in
  ()
