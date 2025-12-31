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
  | RRead n -> { degree = n; max_degree = n; hole_count = 1 }
  | RSkip n -> { degree = n; max_degree = n; hole_count = 1 }
  | RCon c -> { degree = (Words.summary c).degree; max_degree = (Words.summary c).max_degree; hole_count = 0 }

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
    let ph, pt = read_front_exn p in
    match (ph, q) with
    | RRead ph, RRead q -> Generic.snoc ~monoid ~measure:red_measure pt (RRead (ph + q))
    | RSkip ph, RSkip q -> Generic.snoc ~monoid ~measure:red_measure pt (RSkip (ph + q))
    | RCon ph, RCon q -> Generic.snoc ~monoid ~measure:red_measure pt (RCon (Words.append ph q))
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

let rec join (x : read) (x_weaken : bool ref) (y : read) (y_weaken : bool ref) : join =
  (*assert (read_valid x);
  assert (read_valid y);*)
  let recurse x y = join x x_weaken y y_weaken in
  let return r =
    (*assert ((read_measure r).degree = (read_measure x).degree);
    assert ((read_measure r).max_degree = (read_measure x).max_degree);*)
    r
  in
  (*assert ((read_measure x).degree = (read_measure y).degree);
  assert ((read_measure x).max_degree = (read_measure y).max_degree);*)
  if Generic.is_empty x then (
    assert (Generic.is_empty y);
    return { result = Generic.empty; x_rest = Generic.empty; y_rest = Generic.empty })
  else
    let xh, xt = read_front_exn x in
    let yh, yt = read_front_exn y in
    let slice slice_length slice_reason =
      let xh, xt = read_slice x slice_length in
      let yh, yt = read_slice y slice_length in
      let res = recurse xt yt in
      {
        result = read_cons slice_reason res.result;
        x_rest = read_append xh res.x_rest;
        y_rest = read_append yh res.y_rest;
      }
    in
    match (xh, yh) with
    (* We have to pop them off one of a time, because a small chunk might be masking a larger chunk.
     * A more principled approach would be to calculate the lca.
     *)
    | RSkip x, RSkip y ->
        let m = min x y in
        return (slice m (RSkip m))
    | RSkip x, RRead y ->
        let m = min x y in
        y_weaken := true;
        return (slice m (RSkip m))
    | RSkip _, RCon _ ->
        y_weaken := true;
        return (slice 1 (RSkip 1))
    | RRead x, RSkip y ->
        let m = min x y in
        x_weaken := true;
        return (slice m (RSkip m))
    | RCon _, RSkip _ ->
        x_weaken := true;
        return (slice 1 (RSkip 1))
    | RRead x, RRead y ->
        let m = min x y in
        return (slice m (RRead m))
    | RRead _, RCon _ ->
        y_weaken := true;
        return (slice 1 (RRead 1))
    | RCon _, RRead _ ->
        x_weaken := true;
        return (slice 1 (RRead 1))
    | RCon xh, RCon yh ->
        let lca_length = Words.lca_length xh yh in
        if lca_length = 0 then (
          assert (not (Generic.is_empty xh));
          assert (not (Generic.is_empty yh));
          x_weaken := true;
          y_weaken := true;
          return (slice 1 (RRead 1)))
        else
          let xhh, xht = Words.slice_length xh lca_length in
          let yhh, yht = Words.slice_length yh lca_length in
          assert (Words.equal_words xhh yhh);
          let x = if Generic.is_empty xht then xt else read_cons (RCon xht) xt in
          let y = if Generic.is_empty yht then yt else read_cons (RCon yht) yt in
          let res = recurse x y in
          return { result = read_cons (RCon xhh) res.result; x_rest = res.x_rest; y_rest = res.y_rest }

let hash (x : int) (y : int) : int =
  let hash = Hashtbl.hash (x, y) in
  hash

let read_empty : read = Generic.empty

let read_from_pattern (p : Pattern.pattern) : read =
  (*assert (Pattern.pattern_valid p);*)
  Generic.map ~monoid ~measure:red_measure
    (fun pat -> match pat with Pattern.PVar n -> make_rskip n | Pattern.PCon c -> RCon c)
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
