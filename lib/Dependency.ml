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

type ('a, 'measure) gap_seq = { head : 'a option; slices : ('a, 'measure) slices; val_count : int }
and ('a, 'measure) slices = ('a slice, measure) Generic.fg
and 'a slice = { offset : int; a : 'a }

type pattern = (match_words, measure) gap_seq
and match_words = { words : words; children : pattern; val_count : int }
and measure = { degree : int; max_degree : int }

let monoid : measure monoid =
  {
    zero = { degree = 0; max_degree = 0 };
    combine = (fun x y -> { degree = x.degree + y.degree; max_degree = max x.max_degree (x.degree + y.max_degree) });
  }

let match_words_measure (slice : match_words slice) : measure =
  { degree = slice.offset + slice.a.val_count; max_degree = slice.offset + slice.a.val_count }

type reads = (read, measure) gap_seq
and read = { length : int;  children : reads; val_count : int }

let pattern_to_reads (p : pattern) : reads list = failwith "unimplemented"

(*lets ignore CEK stuff for now, those codes should be trivial.*)
type memo =
  (*Note how we are storing two pattern: the outer one is partial (consumed by the trie), and the inner one is complete*)
  | Leaf of { pattern : pattern; step : step }
  | Branch of { reads : reads; children : (fetch_hash, memo) Hashtbl.t; step : step }
and fetch_hash = int
and step = {
  src : pattern;
  dst : value;
  sc : int;
}

let seq_to_pattern (s : seq) (r : reads) : pattern = failwith "unimplemented"
(*match x with y z -> z*)
let subtract (x : pattern) (y : pattern) : pattern option = failwith "unimplemented"

let seq_match (x : seq) (y : pattern) : seq option = failwith "unimplemented"

let walk_seq (m : memo) (s : seq) : (memo * seq) = failwith "unimplemented"

let walk_pattern (m : memo) (p : pattern) : (memo * pattern) = failwith "unimplemented"

let to_seq (x : pattern) (y : seq) : seq = failwith "unimplemented"

(*find_step have to walk down, and call to_seq on the holes*)
let find_step (m : memo) (s : seq) : (seq * step) = failwith "unimplemented"

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
