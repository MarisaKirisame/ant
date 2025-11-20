open Value
open Words
open BatFingerTree

(* Ant have 4 subtly different data structures, all of which employ finger tree on monoid parsing.
 * Todo: the name sucks. Find better ones.
 * - words: representing a fully concrete value, which is the type user will deal with.
 * - value/seq: representing a value with variables, which we represent results in e.g. memization.
 * - pattern: representing a pattern with holes, which we use to match against Values.
 * - read: which return words from one of the four data structures, used for branching in trie.
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
type measure = { degree : int; max_degree : int }

(*Invariant: consecutive constructors should be fused*)
type pattern = (pat, measure) Generic.fg
and pat = PVar of int | PCon of words

let monoid : measure monoid =
  {
    zero = { degree = 0; max_degree = 0 };
    combine = (fun x y -> { degree = x.degree + y.degree; max_degree = max x.max_degree (x.degree + y.max_degree) });
  }

let pat_measure (p : pat) : measure =
  match p with
  | PVar n -> { degree = n; max_degree = n }
  | PCon c -> { degree = (Words.summary c).degree; max_degree = (Words.summary c).max_degree }

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
    | PVar p, PVar qh -> Generic.cons ~monoid ~measure:pat_measure qt (PVar (p + qh))
    | PCon p, PCon qh -> Generic.cons ~monoid ~measure:pat_measure qt (PCon (Words.append p qh))
    | PCon _, PVar _ | PVar _, PCon _ -> Generic.cons ~monoid ~measure:pat_measure q p

let pattern_slice (p : pattern) (offset : int) : pattern * pattern =
  if offset = 0 then (Generic.empty, p)
  else
    let x, y = Generic.split ~monoid ~measure:pat_measure (fun m -> m.max_degree >= offset) p in
    let md = (pattern_measure x).max_degree in
    assert (md < offset);
    let yh, yt = pattern_front_exn y in
    match yh with
    | PVar n ->
        assert (md + n >= offset);
        let needed = offset - md in
        assert (needed > 0 && needed <= n);
        let left = pattern_cons (PVar needed) yt in
        let right = if n - needed > 0 then pattern_cons (PVar (n - needed)) yt else yt in
        (Generic.append ~monoid ~measure:pat_measure x left, right)
    | PCon c ->
        let cd = (Words.summary c).max_degree in
        assert (md + cd >= offset);
        let needed = offset - md in
        assert (needed > 0 && needed <= cd);
        let c_words, c_children = Words.slice c needed in
        let left = pattern_cons (PCon c_words) yt in
        let right = if cd - needed > 0 then pattern_cons (PCon c_children) yt else yt in
        (Generic.append ~monoid ~measure:pat_measure x left, right)

let rec antiunify (x : pattern) (y : pattern) : pattern =
  let xm = pattern_measure x in
  let ym = pattern_measure y in
  assert (xm.degree = ym.degree);
  assert (xm.max_degree = ym.max_degree);
  assert (xm.degree = xm.max_degree);
  if pattern_is_empty x then x
  else
    let xh, xt = pattern_front_exn x in
    let yh, yt = pattern_front_exn y in
    match (xh, yh) with
    | PVar xh, PVar yh ->
        let h = max xh yh in
        pattern_cons (PVar h) (antiunify (snd (pattern_slice x h)) (snd (pattern_slice y h)))
        (* binary search for the longest common prefix, 
         * for the rest (if exist), find max of max_degree, turn into PVar and slice 
         *)
    | PCon xh, PCon yh -> failwith "unimplemented"
    | PVar xh, PCon _ -> pattern_cons (PVar xh) (antiunify xt (snd (pattern_slice y xh)))
    | PCon _, PVar yh -> pattern_cons (PVar yh) (antiunify (snd (pattern_slice x yh)) yt)

type reads = read list
and read = { offset : int }

(*(*lets ignore CEK stuff for now, those codes should be straightforward.*)
type memo =
  (*Note how we are storing two pattern: the outer one is partial (consumed by the trie), and the inner one is complete*)
  | Leaf of { pattern : pattern; step : step }
  | Branch of { reads : reads; children : (fetch_hash, memo) Hashtbl.t; step : step }

and fetch_hash = int
and step = { src : pattern; dst : value; sc : int }

let pat_append x y = Generic.append ~monoid ~measure:pat_measure x y

let rec pattern_append (x : pattern) (y : pattern) : pattern =
  if Generic.is_empty x then y
  else if Generic.is_empty y then x
  else
    let rest_x, last_x = pattern_rear_exn x in
    let first_y, rest_y = pattern_front_exn y in
    let with_middle middle = pat_append (Generic.cons ~monoid ~measure:pat_measure rest_x middle) rest_y in
    match (last_x, first_y) with
    | PVar n1, PVar n2 -> with_middle (PVar (n1 + n2))
    | PCon c1, PCon c2 ->
        with_middle
          (PCon
             {
               words = Words.append c1.words c2.words;
               children = pattern_append c1.children c2.children;
               val_count = c1.val_count + c2.val_count;
             })
    | _ -> pat_append x y

let pattern_cons (x : pat) (y : pattern) : pattern = pattern_append (Generic.singleton x) y

let pattern_option_cons (x : pat) (y : pattern option) : pattern option =
  match y with None -> None | Some y -> Some (pattern_append (Generic.singleton x) y)

(* TODO: redo this function. we have to start by implementing chop/split/length, to take the same slice,
 * given that, use it to refactor the code.
 * pcon also need to take degree/max_degree, as its 'value count' can be <0.
 *)
(*match x with y z -> z*)
let rec subtract (x : pattern) (y : pattern) : pattern option =
  (*We might try to be more lenient with val_count, but that is hard and unnecessary.*)
  if (pattern_measure x).val_count != (pattern_measure y).val_count then None
  else if Generic.is_empty x then (
    assert (Generic.is_empty y);
    Some Generic.empty)
  else (
    assert (not (Generic.is_empty x));
    assert (not (Generic.is_empty y));
    let xh, xt = pattern_front_exn x in
    let yh, yt = pattern_front_exn y in
    match (xh, yh) with
    | PVar xh, PVar yh ->
        if xh < yh then pattern_option_cons (PVar xh) (subtract xt (pattern_cons (PVar (yh - xh)) yt))
        else if xh > yh then pattern_option_cons (PVar yh) (subtract (pattern_cons (PVar (xh - yh)) xt) yt)
        else pattern_option_cons (PVar xh) (subtract xt yt)
    | PCon xh, PCon yh -> failwith "unimplemented"
    | PVar _, PCon _ -> None
    | PCon xh, PVar yh ->
        let xl = Words.length xh.words in
        if xl < yh then pattern_option_cons (PCon xh) (subtract xt (pattern_cons (PVar (yh - xl)) yt))
        else if xl > yh then failwith "unimplemented"
        else pattern_option_cons (PCon xh) (subtract xt yt))

let seq_match (x : seq) (y : pattern) : seq option = failwith "unimplemented"
let walk_seq (m : memo) (s : seq) : memo * seq = failwith "unimplemented"
let walk_pattern (m : memo) (p : pattern) : memo * pattern = failwith "unimplemented"
let to_seq (x : pattern) (y : seq) : seq = failwith "unimplemented"

(*find_step have to walk down, and call to_seq on the holes*)
let find_step (m : memo) (s : seq) : seq * step = failwith "unimplemented"
let raw_step (s : seq) : step = failwith "unimplemented"

(*antiunification. symmetric*)
let antiunify (x : pattern) (y : pattern) : pattern = failwith "unimplemented"
let reads_intersect (x : reads) (y : reads) : reads = failwith "unimplemented"

(* Walk down and insert the step there.
 * One might have failed on a Branch.
 * In that case call antiunify on branch and the remaining pattern and insert new Branch nodes.
 * Insert one final Branch node via reads_intersection to distinguish the two branches.
 *)
let insert_step (m : memo) (step : step) : memo = failwith "unimplemented"

(*concatenation of the two tree. also known as compose. x + y - x = y.*)
let add (x : pattern) (y : pattern) : pattern option = failwith "unimplemented"

type 'a subst_map = int (*todo*)

(*unification, symmetric, form a lattice with antiunification*)
let unify (x : seq) (y : pattern) : pattern subst_map = failwith "unimplemented"
let subst (x : seq) (s : seq subst_map) : seq = failwith "unimplemented"

(*build up the subst_map, and use add to get F', subst to get G'.*)
let compose_step (x : step) (y : step) : step = failwith "unimplemented"
*)
