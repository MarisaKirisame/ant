open BatFingerTree
open Words

(* A read is an abstraction on patterns.
 * RCon: corresponding patterns must have this constants.
 * RRead: correponding patterns must be constant.
 * RSkip: corresponding patterns can be anything.
 *)
type read = (red, Pattern.measure) Generic.fg
and red = RRead of int | RSkip of int | RCon of words

let monoid = Pattern.monoid

let make_rread n =
  assert (n > 0);
  RRead n

let make_rskip n =
  assert (n > 0);
  RSkip n

let red_measure (r : red) : Pattern.measure =
  match r with
  | RRead n -> { degree = n; max_degree = n }
  | RSkip n -> { degree = n; max_degree = n }
  | RCon c -> { degree = (Words.summary c).degree; max_degree = (Words.summary c).max_degree }

let rec read_valid x : bool =
  match Generic.front x ~monoid ~measure:red_measure with
  | None -> true
  | Some (rest, x) -> (
      match Generic.front rest ~monoid ~measure:red_measure with
      | None -> true
      | Some (_, y) ->
          (match (x, y) with
            | RCon _, RCon _ -> false
            | RCon _, _ -> true
            | RRead _, RRead _ -> false
            | RRead _, _ -> true
            | RSkip _, RSkip _ -> false
            | RSkip _, _ -> true)
          && read_valid rest)

let read_measure (r : read) : Pattern.measure = Generic.measure ~monoid ~measure:red_measure r
let read_is_empty (r : read) : bool = Generic.is_empty r
let read_rear_exn (r : read) : read * red = Generic.rear_exn ~monoid ~measure:red_measure r

let read_front_exn (r : read) : red * read =
  let rest_r, first_r = Generic.front_exn ~monoid ~measure:red_measure r in
  (first_r, rest_r)

let read_cons (p : red) (q : read) : read =
  if Generic.is_empty q then Generic.singleton p
  else
    let qh, qt = read_front_exn q in
    match (p, qh) with
    | RRead p, RRead qh -> Generic.cons ~monoid ~measure:red_measure qt (RRead (p + qh))
    | RSkip p, RSkip qh -> Generic.cons ~monoid ~measure:red_measure qt (RSkip (p + qh))
    | RCon p, RCon qh -> Generic.cons ~monoid ~measure:red_measure qt (RCon (Words.append p qh))
    | _ -> Generic.cons ~monoid ~measure:red_measure q p

let read_snoc (p : read) (q : red) : read =
  if Generic.is_empty p then Generic.singleton q
  else
    let ph, pt = read_rear_exn p in
    match (pt, q) with
    | RRead pt, RRead q -> Generic.snoc ~monoid ~measure:red_measure ph (RRead (pt + q))
    | RSkip pt, RSkip q -> Generic.snoc ~monoid ~measure:red_measure ph (RSkip (pt + q))
    | RCon pt, RCon q -> Generic.snoc ~monoid ~measure:red_measure ph (RCon (Words.append pt q))
    | _ -> Generic.snoc ~monoid ~measure:red_measure p q

let read_append_unsafe (x : read) (y : read) : read = Generic.append ~monoid ~measure:red_measure x y
let read_cons_unsafe (p : red) (q : read) : read = Generic.cons ~monoid ~measure:red_measure q p
let read_snoc_unsafe (p : read) (q : red) : read = Generic.snoc ~monoid ~measure:red_measure p q

let rec read_append (x : read) (y : read) : read =
  if Generic.is_empty x then y
  else if Generic.is_empty y then x
  else
    let rest_x, last_x = read_rear_exn x in
    let first_y, rest_y = read_front_exn y in
    let with_middle middle = read_append_unsafe rest_x (read_cons_unsafe middle rest_y) in
    match (last_x, first_y) with
    | RRead n1, RRead n2 -> with_middle (make_rread (n1 + n2))
    | RSkip n1, RSkip n2 -> with_middle (make_rskip (n1 + n2))
    | RCon c1, RCon c2 -> with_middle (RCon (Words.append c1 c2))
    | _ -> read_append_unsafe x y

let rec read_slice (r : read) (offset : int) : read * read =
  assert (offset >= 0);
  let return x y =
    (*assert ((read_measure x).degree = (read_measure x).max_degree);
    assert ((read_measure x).degree = offset);
    assert (offset + (read_measure y).degree = (read_measure r).degree);*)
    (x, y)
  in
  if offset = 0 then return Generic.empty r
  else
    let x, y = Generic.split ~monoid ~measure:red_measure (fun m -> not (m.max_degree < offset)) r in
    (*assert ((read_measure x).max_degree < offset);*)
    let d = (read_measure x).degree in
    assert (d < offset);
    let needed = offset - d in
    assert (needed > 0);
    let yh, yt = read_front_exn y in
    match yh with
    | RRead n ->
        assert (d + n >= offset);
        assert (needed <= n);
        let left = read_snoc_unsafe x (make_rread needed) in
        let right = if n - needed > 0 then read_cons_unsafe (make_rread (n - needed)) yt else yt in
        return left right
    | RSkip n ->
        assert (d + n >= offset);
        assert (needed <= n);
        let left = read_snoc_unsafe x (make_rskip needed) in
        let right = if n - needed > 0 then read_cons_unsafe (make_rskip (n - needed)) yt else yt in
        return left right
    | RCon c ->
        let cd = (Words.summary c).max_degree in
        assert (d + cd >= offset);
        assert (needed <= cd);
        let c_words, c_children = Words.slice_degree c needed in
        (*assert ((Words.summary c_words).degree = needed);
        assert ((Words.summary c_words).max_degree = needed);*)
        let left = read_snoc_unsafe x (RCon c_words) in
        let right = if not (Generic.is_empty c_children) then read_cons_unsafe (RCon c_children) yt else yt in
        return left right

let read_pop_n (r : read) n : read =
  let x, y = read_slice r n in
  (*assert ((read_measure x).degree = n);
  assert ((read_measure y).degree = (read_measure r).degree - n);*)
  y

let rec unmatch_read (x : read) (y : read) : read =
  if Generic.is_empty x then (
    assert (Generic.is_empty y);
    Generic.empty)
  else
    let xh, xt = read_front_exn x in
    match xh with
    | RSkip n | RRead n ->
        let yh, yt = read_slice y n in
        read_append yh (unmatch_read xt yt)
    | RCon c -> read_cons (RCon c) (unmatch_read xt y)

type join = { result : read; x_rest : read; y_rest : read }

let join_words_lcp_slot = Profile.register_slot Profile.memo_profile "join.words_lcp"

let join (x : read) (x_weaken : bool ref) (y : read) (y_weaken : bool ref) (result_acc : read Lazy.t) : read Lazy.t =
  let return x = x in
  let rec loop xh xt yh yt result_acc =
    let slice slice_length slice_reason =
      let xh, xt = read_slice (read_cons xh xt) slice_length in
      let yh, yt = read_slice (read_cons yh yt) slice_length in
      recurse xt yt (lazy (read_snoc (Lazy.force result_acc) slice_reason))
    in
    match (xh, yh) with
    (* We have to pop them off one of a time, because a small chunk might be masking a larger chunk.*)
    | RSkip x, RSkip y ->
        if x < y then
          let xh, xt = read_front_exn xt in
          let yh, yt = (RSkip (y - x), yt) in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RSkip x))))
        else if x > y then
          let xh, xt = (RSkip (x - y), xt) in
          let yh, yt = read_front_exn yt in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RSkip y))))
        else return (recurse xt yt (lazy (read_snoc (Lazy.force result_acc) (RSkip x))))
    | RSkip x, RRead y ->
        y_weaken := true;
        if x < y then
          let xh, xt = read_front_exn xt in
          let yh, yt = (RRead (y - x), yt) in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RSkip x))))
        else if x > y then
          let xh, xt = (RSkip (x - y), xt) in
          let yh, yt = read_front_exn yt in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RSkip y))))
        else return (recurse xt yt (lazy (read_snoc (Lazy.force result_acc) (RSkip x))))
    | RSkip _, RCon _ ->
        y_weaken := true;
        return (slice 1 (RSkip 1))
    | RRead x, RSkip y ->
        x_weaken := true;
        if x < y then
          let xh, xt = read_front_exn xt in
          let yh, yt = (RSkip (y - x), yt) in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RSkip x))))
        else if x > y then
          let xh, xt = (RRead (x - y), xt) in
          let yh, yt = read_front_exn yt in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RSkip y))))
        else return (recurse xt yt (lazy (read_snoc (Lazy.force result_acc) (RSkip x))))
    | RCon _, RSkip _ ->
        x_weaken := true;
        return (slice 1 (RSkip 1))
    | RRead x, RRead y ->
        if x < y then
          let xh, xt = read_front_exn xt in
          let yh, yt = (RRead (y - x), yt) in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RRead x))))
        else if x > y then
          let xh, xt = (RRead (x - y), xt) in
          let yh, yt = read_front_exn yt in
          return (loop xh xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RRead y))))
        else return (recurse xt yt (lazy (read_snoc (Lazy.force result_acc) (RRead x))))
    | RRead _, RCon _ ->
        y_weaken := true;
        return (slice 1 (RRead 1))
    | RCon _, RRead _ ->
        x_weaken := true;
        return (slice 1 (RRead 1))
    | RCon xh, RCon yh -> (
        let lcp, xh, yh = Profile.with_slot join_words_lcp_slot (fun () -> Words.lcp xh yh) in
        if Generic.is_empty lcp then (
          x_weaken := true;
          y_weaken := true;
          return (slice 1 (RRead 1)))
        else
          match (Generic.is_empty xh, Generic.is_empty yh) with
          | true, true -> return (recurse xt yt (lazy (read_snoc (Lazy.force result_acc) (RCon lcp))))
          | true, false ->
              let xh, xt = read_front_exn xt in
              return (loop xh xt (RCon yh) yt (lazy (read_snoc (Lazy.force result_acc) (RCon lcp))))
          | false, true ->
              let yh, yt = read_front_exn yt in
              return (loop (RCon xh) xt yh yt (lazy (read_snoc (Lazy.force result_acc) (RCon lcp))))
          | false, false ->
              return (loop (RCon xh) xt (RCon yh) yt (lazy (read_snoc (Lazy.force result_acc) (RCon lcp)))))
  and recurse x y result_acc =
    if Generic.is_empty x then (
      assert (Generic.is_empty y);
      result_acc)
    else
      let xh, xt = read_front_exn x in
      let yh, yt = read_front_exn y in
      loop xh xt yh yt result_acc
  in
  recurse x y result_acc

let hash (x : int) (y : int) : int =
  let hash = Hashtbl.hash (x, y) in
  hash

let read_empty : read = Generic.empty

let read_from_pattern (p : Pattern.pattern) : read =
  (*assert (Pattern.pattern_valid p);*)
  Generic.map ~monoid ~measure:red_measure
    (fun pat ->
      match pat with
      | Pattern.PVar n -> make_rskip n
      | Pattern.Words c -> RCon c
      | Pattern.Reference _ -> failwith "read_from_pattern: pattern contains Reference")
    p

let read_equal (x : read) (y : read) : bool =
  Generic.equal
    (fun a b ->
      match (a, b) with
      | RRead n1, RRead n2 -> n1 = n2
      | RSkip n1, RSkip n2 -> n1 = n2
      | RCon c1, RCon c2 -> Words.equal_words c1 c2
      | _ -> false)
    x y
