open Value
open Words
open BatFingerTree

(* Design notes on the pattern/value/word relationship now live in
   docs/internal.md#patterns-as-finger-trees-patternml. *)
(*todo: do we actually need hole_count?*)
type measure = { degree : int; max_degree : int; hole_count : int }

(*Invariant: consecutive constructors should be fused*)
type pattern = (pat, measure) Generic.fg
and pat = PVar of int | PCon of words

let make_pvar n =
  assert (n > 0);
  PVar n

let monoid : measure monoid =
  {
    zero = { degree = 0; max_degree = 0; hole_count = 0 };
    combine =
      (fun x y ->
        {
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
          hole_count = x.hole_count + y.hole_count;
        });
  }

let pat_measure (p : pat) : measure =
  match p with
  | PVar n -> { degree = n; max_degree = n; hole_count = 1 }
  | PCon c -> { degree = (Words.summary c).degree; max_degree = (Words.summary c).max_degree; hole_count = 0 }

let pattern_measure (p : pattern) : measure = Generic.measure ~monoid ~measure:pat_measure p
let pattern_is_empty (x : pattern) : bool = Generic.is_empty x
let pattern_rear_exn (p : pattern) : pattern * pat = Generic.rear_exn ~monoid ~measure:pat_measure p

let pattern_front_exn (p : pattern) : pat * pattern =
  let rest_p, first_p = Generic.front_exn ~monoid ~measure:pat_measure p in
  (first_p, rest_p)

let pattern_cons (p : pat) (q : pattern) : pattern =
  if Generic.is_empty q then Generic.singleton p
  else
    let qh, qt = pattern_front_exn q in
    match (p, qh) with
    | PVar p, PVar qh -> Generic.cons ~monoid ~measure:pat_measure qt (make_pvar (p + qh))
    | PCon p, PCon qh -> Generic.cons ~monoid ~measure:pat_measure qt (PCon (Words.append p qh))
    | PCon _, PVar _ | PVar _, PCon _ -> Generic.cons ~monoid ~measure:pat_measure q p

let pattern_snoc (p : pattern) (q : pat) : pattern =
  if Generic.is_empty p then Generic.singleton q
  else
    let ph, pt = pattern_rear_exn p in
    match (pt, q) with
    | PVar pt, PVar q -> Generic.snoc ~monoid ~measure:pat_measure ph (make_pvar (pt + q))
    | PCon pt, PCon q -> Generic.snoc ~monoid ~measure:pat_measure ph (PCon (Words.append pt q))
    | PCon _, PVar _ | PVar _, PCon _ -> Generic.snoc ~monoid ~measure:pat_measure p q

let pattern_append_unsafe x y = Generic.append ~monoid ~measure:pat_measure x y
let pattern_cons_unsafe x y = Generic.cons ~monoid ~measure:pat_measure y x
let pattern_snoc_unsafe x y = Generic.snoc ~monoid ~measure:pat_measure x y

let rec pattern_append (x : pattern) (y : pattern) : pattern =
  if Generic.is_empty x then y
  else if Generic.is_empty y then x
  else
    let rest_x, last_x = pattern_rear_exn x in
    let first_y, rest_y = pattern_front_exn y in
    let with_middle middle = pattern_append_unsafe rest_x (pattern_cons_unsafe middle rest_y) in
    match (last_x, first_y) with
    | PVar n1, PVar n2 -> with_middle (make_pvar (n1 + n2))
    | PCon c1, PCon c2 -> with_middle (PCon (Words.append c1 c2))
    | _ -> pattern_append_unsafe x y

let pattern_slice (p : pattern) (offset : int) : pattern * pattern =
  assert (offset >= 0);
  let return x y =
    (*assert ((pattern_measure x).degree = (pattern_measure x).max_degree);
    assert ((pattern_measure x).degree = offset);
    assert (offset + (pattern_measure y).degree = (pattern_measure p).degree);*)
    (x, y)
  in
  if offset = 0 then return Generic.empty p
  else
    let x, y = Generic.split ~monoid ~measure:pat_measure (fun m -> not (m.max_degree < offset)) p in
    (*assert ((pattern_measure x).max_degree < offset);*)
    let d = (pattern_measure x).degree in
    assert (d < offset);
    let needed = offset - d in
    assert (needed > 0);
    let yh, yt = pattern_front_exn y in
    match yh with
    | PVar n ->
        assert (d + n >= offset);
        assert (needed <= n);
        let left = pattern_snoc_unsafe x (make_pvar needed) in
        let right = if n - needed > 0 then pattern_cons_unsafe (make_pvar (n - needed)) yt else yt in
        return left right
    | PCon c ->
        let cd = (Words.summary c).max_degree in
        assert (d + cd >= offset);
        assert (needed <= cd);
        let c_words, c_children = Words.slice_degree c needed in
        (*assert ((Words.summary c_words).degree = needed);
        assert ((Words.summary c_words).max_degree = needed);*)
        let left = pattern_snoc_unsafe x (PCon c_words) in
        let right = if not (Generic.is_empty c_children) then pattern_cons_unsafe (PCon c_children) yt else yt in
        return left right

(*todo: rename all pop_n to drop_n*)
let pattern_pop_n (p : pattern) (n : int) : pattern =
  let _, x = pattern_slice p n in
  x

(*let rec pattern_valid x : bool =
  match Generic.front x ~monoid ~measure:pat_measure with
  | None -> true
  | Some (rest, y) -> (
      match Generic.front rest ~monoid ~measure:pat_measure with
      | None -> true
      | Some (_, z) ->
          (match (y, z) with
            | PCon _, PCon _ -> false
            | PCon _, _ -> true
            | PVar _, PVar _ -> false
            | PVar _, _ -> true)
          && pattern_valid rest)*)
