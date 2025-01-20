(* Required reading: Seq.ml
 *
 * This file contian all code that deals with memoiziation.
 * There is multiple mutually recursive component which together enable memoization.
 * However, three of them are the most critical:
 *   The Reference type. It represent fragment of the finger tree that is unavailable, as we're working on fragment of the tree.
 *   The Store. It give meaning to Reference type in the tree.
 *   The memo. It is a fusion between hashtable and trie, somewhat like the patrica trie, to traverse prefix at exponential rate.
 *)
open BatFingerTree
open Word
open SL2

type env = value Dynarray.t and

(* one thing we might want is to have exp as an adt,
 * which allow memoizing exp fragment, which incrementalize the program under program change.
 * however, it
 *   0 - add extra overhead
 *   1 - we dont really need this for eval
 *   2 - is unclear how we actually do this with recursive function
 *         ideally, modifying a recursive function and nothing else
 *         should only cause all reexecution of that recursive function at the changed location.
 *)
exp = {
    (*one step transition. when done throw an exception.*)
    func : state -> state;
    (*pc is an isomorphism to func, and pc -> func is a table lookup.*)
    pc : int;
} and 
kont = value and
state = {
    c : exp;
    e : env;
    k : kont;
} and

(* The Reference
 * Ant memoize a fragment of the current environment, and allow skipping to a point,
 *   where the next evaluation step require a value outside of the fragment.
 * To do so, we need to track fragment that is memoized and unmemoized.
 *   The memoized part can be represented as Word.t,
 *   and the unmemoized part is represented as a Unmatched.  
 * Unmatched contain degree/max_degree, as well as where to fetch the value (a reference).
 *)
 reference = {
    r: source;
    offset: int;
    values_count: int;
} and
(*cannot access, as it is an unmatched reference at the memo caller.*)
barrier = {
    values_count: int
} and
source = E of int | S of int | K and
fg_et = Word of Word.t | Reference of reference | Barrier of reference and
seq = (fg_et, measure) Generic.fg and
measure = {
    degree: int;
    max_degree: int;
    full: full_measure option;
} and
(* measure have this iff fully matched (only Word.t, no reference). *)
full_measure = {
    length: int;
    hash: sl2;
} and

(* The Store
 * When computing under memoization, we need to determine which value is memoized, and which is not.
 *   To do so, each value is paired with a time tag, dictating when is it used.
 *   If the value match with the current time, it is usable.
 *   If it is not usable, we require extra matching to register it.
 * Of course, a match can be partial.
 *   the remaining pieces are appended into the Store.
 *   The Store is an array containing two type of value: the matched and unmatched.
 *   Unmatched contain seq from the memoization caller, and matching on it turn it into a matched
 *     (Note that this is not the same as the unmatched type.)
 *   Matched contain an array index into the remaining prefix and suffix, as well as the sequence matched.
 * Partial matching on consecutive value result in exponentially longer and longer matching length,
 *   done by pairing each entry in the Store a ref of length, aliased on all match of the same origin, growing exponentially.
 * When a memoization run is finished, we need to fix all value for the caller.
 *   fixing the caller-generated values can be done by having a stack of value and popping from the stack.
 *   fixing the callee-generated values can be done by resolving all the references.
 *)
store = {
    (*note that last is not used in the entries, as the whole store is restored at once.*)
    entries: value Dynarray.t Stack.t;
    last: store option;
 } and
value = {
    version: version_t; (*otherwise callee*)
    seq: seq;
    tracker: int ref;
    last: value option;
} and
(* Version
 * 
 *)
version_t = int and
(* The memo
 * The memo is the key data structure that handle all memoization logic.
 *   It contain a match request, which try to match a reference of a length.
 *   The match then is hashed and compared to value in a hashtable.
 *   The value inside a hashtable is a transit function,
 *     which mutate the env and the current value,
 *     alongside an extra memo.
 *   Transit function:
 *     When memo end, he result contain reference which is invalid in the original context, 
 *     and the transit function lift the result to the original context.
 *     It merely look at all the references, resolve them, and finally rebuild the result.
 *     Note that we can lookup memo inside record mode, so the transit function still operate over references.
 *     This would be handled naturally as the execution should only depend on the prefixes.
 *   The caller should traverse down this memo tree until it can not find a match,
 *     then execute the function.
 *)
memo_t = memo_aux Array.t and
memo_aux = {
    request: match_request;
    lookup: (hashkey, memo_aux) Hashtbl.t;
    enter: state -> state;
    exit: state -> state;
} and
match_request = {
    r: source;
    offset: int;
    word_count: int;
} and
hashkey = Todo 