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
type measure = { val_count : int }

(*Invariant: no consecutive pat of the same constructor*)
type pattern = (pat, measure) Generic.fg

(*Note that val_count stand for the number of values in input, not the number of values in output(children).*)
and pat = PVar of int | PCon of { words : words; children : pattern; val_count : int }

let monoid : measure monoid =
  { zero = { val_count = 0 }; combine = (fun x y -> { val_count = x.val_count + y.val_count }) }

let pat_measure (p : pat) : measure = match p with PVar n -> { val_count = n } | PCon c -> { val_count = c.val_count }
let pattern_measure (p : pattern) : measure = Generic.measure ~monoid ~measure:pat_measure p

(*Invariant: no consecutive read of the same constructor*)
type reads = (read, measure) Generic.fg
and read = RVar of int | RCon of { length : int; children : reads; val_count : int }

let read_measure (r : read) : measure =
  match r with RVar n -> { val_count = n } | RCon c -> { val_count = c.val_count }

let rec pattern_to_reads (p : pattern) : reads =
  Generic.map ~monoid ~measure:read_measure
    (fun x ->
      match x with
      | PVar n -> RVar n
      | PCon c ->
          RCon { length = Words.length c.words; val_count = c.val_count; children = pattern_to_reads c.children })
    p

(*lets ignore CEK stuff for now, those codes should be straightforward.*)
type memo =
  (*Note how we are storing two pattern: the outer one is partial (consumed by the trie), and the inner one is complete*)
  | Leaf of { pattern : pattern; step : step }
  | Branch of { reads : reads; children : (fetch_hash, memo) Hashtbl.t; step : step }

and fetch_hash = int
and step = { src : pattern; dst : value; sc : int }

let pat_append x y = Generic.append ~monoid ~measure:pat_measure x y
let pattern_rear_exn (p : pattern) : pattern * pat = Generic.rear_exn ~monoid ~measure:pat_measure p

let pattern_front_exn (p : pattern) : pat * pattern =
  let rest_p, first_p = Generic.front_exn ~monoid ~measure:pat_measure p in
  (first_p, rest_p)

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
        else if xl > yh then 
          failwith "unimplemented"
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
