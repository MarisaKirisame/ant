type level = { mutable data : int array; mutable len : int }

let make_level capacity = { data = Array.make capacity 0; len = 0 }

type t = { mutable count : int; k : int; rng : Random.State.t; mutable levels : level list }

let rec assert_no_empty_levels = function
  | [] -> ()
  | lvl :: rest ->
      assert (lvl.len > 0);
      assert_no_empty_levels rest

let create_rng = function None -> Random.State.make_self_init () | Some seed -> Random.State.make [| seed |]

let sort_level_inplace (xs : level) : unit =
  let len = xs.len in
  for i = 1 to len - 1 do
    let key = xs.data.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && xs.data.(!j) > key do
      xs.data.(!j + 1) <- xs.data.(!j);
      decr j
    done;
    xs.data.(!j + 1) <- key
  done

let level_add (lvl : level) (x : int) : unit =
  lvl.data.(lvl.len) <- x;
  lvl.len <- lvl.len + 1

let compact_level (rng : Random.State.t) (lvl : level) : int array =
  if lvl.len = 0 then [||]
  else (
    sort_level_inplace lvl;
    let offset = Random.State.int rng 2 in
    let promoted_len = (lvl.len - offset + 1) / 2 in
    let promoted = Array.make promoted_len 0 in
    let out = ref 0 in
    let i = ref offset in
    while !i < lvl.len do
      promoted.(!out) <- lvl.data.(!i);
      incr out;
      i := !i + 2
    done;
    lvl.len <- 0;
    promoted)

let rec insert_level (k : int) (rng : Random.State.t) (x : int) (levels : level list) : level list =
  match levels with
  | [] ->
      let lvl = make_level k in
      level_add lvl x;
      [ lvl ]
  | lvl :: rest ->
      if lvl.len < k then (
        level_add lvl x;
        lvl :: rest)
      else
        let promoted = compact_level rng lvl in
        let rest' = ref rest in
        for i = 0 to Array.length promoted - 1 do
          rest' := insert_level k rng promoted.(i) !rest'
        done;
        level_add lvl x;
        lvl :: !rest'

let create ?(k = 200) ?seed () =
  if k <= 0 then invalid_arg "Kll.create: k must be positive";
  { count = 0; k; rng = create_rng seed; levels = [] }

let add t (x : int) =
  t.count <- t.count + 1;
  t.levels <- insert_level t.k t.rng x t.levels;
  assert_no_empty_levels t.levels

let add_many t (xs : int array) = Array.iter (fun x -> add t x) xs

let quantile t ~q =
  assert (q >= 0.0);
  assert (q <= 1.0);
  if t.count = 0 then None
  else (
    assert_no_empty_levels t.levels;
    match List.rev t.levels with
    | [] -> assert false
    | lvl :: _ ->
        let arr = Array.sub lvl.data 0 lvl.len in
        Array.sort Int.compare arr;
        let pick = int_of_float (q *. float_of_int (lvl.len - 1)) in
        Some arr.(pick))
