open Value
open Words
open BatFingerTree
(* Ant have 3 subtly different data structures, all of which employ finger tree on monoid parsing.
 * Todo: the name sucks. Find better ones.
 * - words: representing a fully concrete value, which is the type user will deal with.
 * - value/seq: representing a value with variables, which we represent results in e.g. memization.
 * - pattern: representing a pattern with holes, which we use to match against Values.
 *)

(* An efficient data structure to represent data. 
 *   This enable pattern matching in O(log(input_size + pattern_size) * holes_count) times.
 * To match a value against a pattern, 
 *   go through the pattern slices left to right, skipping offset and popping off the matching value in the slice.
 * Note that the offsets are consecutive, i.e., the first slice's offset is from the start of the value,
 *   the second slice's offset is from the end of the first slice's value, and so on.
 * All data that got skipped is then bounded to the pattern varables, which we do not give name to for compositionality.
 * There are 4 key operations on patterns, all with similar algorithmic complexity:
 *   - Pattern matching, which we just described.
 *   - Pattern application, the inverse of pattern matching.
 *   - Antiunification, which computes the most specific generalization of two patterns.
 *   - Unification, which computes the most general specialization of a pattern and a value.
 * To use the memo trie, walk down the trie using pattern matching.
 * To insert an entry to the memo trie, walk down the trie with pattern matching and build a leaf node.
 *   If the descend stopped midway, use antiunification to build a branch node holding the two conflicting patterns.
 * The hardest operation is to compose two steps (A[X] -> B[X]) and (C[Y] -> D[Y]).
 *   To do this:
 *   - 0: Unify the seq B with the pattern C, getting substitution map F and a pattern G,
 *       such that B[F[X]] = C[G[X]]
 *   - 1: Build up A'[X] = A[F[X]], D'[X] = D[G[X]]
 *   - 2: The composed step is then A'[X] -> D'[X], which we add to the memo trie.
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
