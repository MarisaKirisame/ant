open Value
open Words
open BatFingerTree

(* Patterns are a compact, finger-tree representation of "value-with-holes".
 * Ant uses three related structures:
 *   - words   : fully concrete values (prefix traversals) used at the runtime boundary.
 *   - value   : words extended with References that defer slices of env/cont.
 *   - pattern : holes plus concrete prefixes, used to summarise states for memo hits.
 *
 * The measure tracks degree, max_degree, and hole counts, letting us split and
 * fuse adjacent components in O(log n). Matching walks the pattern left to
 * right, binding skipped slices to anonymous holes; the inverse operation
 * (`compose_pattern`) rebuilds a concrete value from those bindings. These
 * operations underpin dependency matching and step composition in
 * Dependency.make_step/compose_step.
 *)
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

let pattern_snoc (q : pattern) (p : pat) : pattern =
  if Generic.is_empty q then Generic.singleton p
  else
    let qh, qt = pattern_rear_exn q in
    match (qt, p) with
    | PVar qt, PVar p -> Generic.snoc ~monoid ~measure:pat_measure qh (make_pvar (qt + p))
    | PCon qt, PCon p -> Generic.snoc ~monoid ~measure:pat_measure qh (PCon (Words.append qt p))
    | PCon _, PVar _ | PVar _, PCon _ -> Generic.snoc ~monoid ~measure:pat_measure q p

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
    assert ((pattern_measure x).degree = (pattern_measure x).max_degree);
    assert ((pattern_measure x).degree = offset);
    assert (offset + (pattern_measure y).degree = (pattern_measure p).degree);
    (x, y)
  in
  if offset = 0 then return Generic.empty p
  else
    let x, y = Generic.split ~monoid ~measure:pat_measure (fun m -> not (m.max_degree < offset)) p in
    assert ((pattern_measure x).max_degree < offset);
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
        assert ((Words.summary c_words).degree = needed);
        assert ((Words.summary c_words).max_degree = needed);
        let left = pattern_snoc_unsafe x (PCon c_words) in
        let right = if not (Generic.is_empty c_children) then pattern_cons_unsafe (PCon c_children) yt else yt in
        return left right
