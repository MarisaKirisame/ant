open BatFingerTree
open Words

(* Both RRead and RSkip correspond to PVar.
 * The difference is that RRead look at the head of the value to distinguish between different cases, while RSkip does not.
 * The read result is a list of int, 
 * which we feed into a hasher (possibly unrelated to monoid hashing) to distinguish between cases.
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
    assert ((read_measure x).degree = (read_measure x).max_degree);
    assert ((read_measure x).degree = offset);
    assert (offset + (read_measure y).degree = (read_measure r).degree);
    (x, y)
  in
  if offset = 0 then return Generic.empty r
  else
    let x, y = Generic.split ~monoid ~measure:red_measure (fun m -> not (m.max_degree < offset)) r in
    assert ((read_measure x).max_degree < offset);
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
        assert ((Words.summary c_words).degree = needed);
        assert ((Words.summary c_words).max_degree = needed);
        let left = read_snoc_unsafe x (RCon c_words) in
        let right = if not (Generic.is_empty c_children) then read_cons_unsafe (RCon c_children) yt else yt in
        return left right

let read_pop_n (r : read) n : read =
  let x, y = read_slice r n in
  assert ((read_measure x).degree = n);
  assert ((read_measure y).degree = (read_measure r).degree - n);
  y

let rec antiunify (x : read) (y : read) : read =
  assert ((read_measure x).degree = (read_measure y).degree);
  assert ((read_measure x).max_degree = (read_measure y).max_degree);
  if Generic.is_empty x then y
  else
    let xh, xt = read_front_exn x in
    let yh, yt = read_front_exn y in
    match (xh, yh) with 
    | RSkip xh, _ -> read_cons (RSkip xh) (antiunify xt (read_pop_n y xh))
    | _, RSkip yh -> read_cons (RSkip yh) (antiunify (read_pop_n x yh) yt)
    | RRead xh, _ -> read_cons (RRead xh) (antiunify xt (read_pop_n y xh))
    | _, RRead yh -> read_cons (RRead yh) (antiunify (read_pop_n x yh) yt)
    | RCon xh, RCon yh -> x
