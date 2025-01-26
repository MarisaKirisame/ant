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
open Common
module Hasher = Hash.SL2

type env = value Dynarray.t

(* one thing we might want is to have exp as an adt,
 * which allow memoizing exp fragment to incrementalize the program under changes.
 * however, it
 *   0 - add extra overhead
 *   1 - we dont really need this for eval
 *   2 - is unclear how we actually do this with recursive function
 *         ideally, modifying a recursive function and nothing else
 *         should only cause all reexecution of that recursive function at the changed location.
 *)
and exp = {
  (* One step transition. Throw an exception when done. *)
  step : state -> state;
  (*pc is an isomorphism to func, and pc -> func is a table lookup.*)
  pc : int;
}

and kont = value

and state = {
  c : exp;
  e : env;
  k : kont;
  r : record_context;
  m : memo_t;
}

(* The Reference
 * Ant memoize a fragment of the current environment, and allow skipping to a point,
 *   where the next evaluation step require a value outside of the fragment.
 * To add such entries into the memoization table, we have to run the program in record mode,
 *   which track memoized (and thus usable) fragment, vs unmemoized fragment.
 * The memoized part can be represented as Word.t,
 *   and the unmemoized part is represented as a Unfetched.  
 * Unfetched contain degree/max_degree, as well as where to fetch the value (a reference).
 *)
and reference = { r : source; offset : int; values_count : int }

(* As ant allow recording inside recording, this nested recording form a call stack.
 *   Depth merely denote the depth of this call stack.
 *)
and depth_t = int

(*cannot access, as it is an unfetched reference at the memo caller.*)
and barrier = { values_count : int }
and source = E of int | S of int | K
and fg_et = Word of Word.t | Reference of reference | Barrier of reference
and seq = (fg_et, measure) Generic.fg
and measure = { degree : int; max_degree : int; full : full_measure option }

(* measure have this iff fully fetched (only Word.t, no reference). *)
and full_measure = { length : int; hash : Hasher.t }

(* The Store
 * When computing under memoization, we need to determine which value is memoized, and which is not.
 *   To do so, each value is paired with is depth, indicating when it was constructed/fetched.
 *   If the depth match with the current depth, it is usable.
 *   If it is not usable, we require extra fetching to register it.
 * Of course, a fetch can be partial.
 *   the remaining pieces are appended into the Store.
 *   The Store is an array containing two type of value: the fetched and unfetched.
 *   Unfetched contain seq from the memoization caller, and fetching it turn it into a fetched
 *     (Note that this is not the same as the unfetched type.)
 *   Fetched contain an array index into the remaining prefix and suffix, as well as the sequence fetched.
 * Partial fetching on consecutive value result in exponentially longer and longer fetching length,
 *   done by pairing each entry in the Store a ref of length, aliased on all fetch of the same origin, growing exponentially.
 * When a memoization run is finished, we need to fix all value for the caller.
 *   fixing the caller-generated values can be done by having a stack of value and popping from the stack.
 *   fixing the callee-generated values can be done by resolving all the references.
 *)
and store = {
  (*note that last is not used in the entries, as the whole store is restored at once.*)
  entries : value Dynarray.t;
}

and value = {
  depth : depth_t;
  seq : seq;
  fetch_length : int ref;
}

(* The memo
 * The memo is the key data structure that handle all memoization logic.
 *   It contain a fetch request, which try to fetch a reference of a length.
 *   The segment then is hashed and compared to value in a hashtable.
 *   The value inside a hashtable is a transit function,
 *     which mutate the env and the current value,
 *     alongside an extra memo.
 *   Transit function:
 *     When memo end, he result contain reference which is invalid in the original context, 
 *     and the transit function lift the result to the original context.
 *     It merely look at all the references, resolve them, and finally rebuild the result.
 *     Note that we can lookup memo inside record mode, so the transit function still operate over references.
 *     This would be handled naturally as the execution should only depend on the prefixes.
 *   The caller should traverse down this memo tree until it can not find a fetch,
 *     then execute the function.
 *)
and memo_t = memo_node_t Array.t

and memo_node_t =
  | Need of {
      request : fetch_request;
      lookup : lookup_t;
      (*++depth. caller need to setup the store.*)
      enter : state -> state;
      (*--depth*)
      exit : state -> state;
    }
  | Done

and lookup_t = (fetch_result, memo_node_t) Hashtbl.t
and fetch_request = { r : source; offset : int; word_count : int }
and fetch_result = 
    | FetchPartial of words
    | FetchSuffix of words
    | FetchFull of words

and words = seq (*Just have Word.t. We could make Word a finger tree of Word.t but that would cost lots of conversion between two representation.*)

and record_context =
  | Raw
  | Recording of {
      s : store;
      memo_node : memo_node_t;
      lookup : lookup_t;
      depth : depth_t;
      last : record_context;
    }

(* If it refer to a value from depth-1, it need a value which had not been fetched yet. 
     We can then flush the current state into the Recording record_context, 
       and fetch the value, and register it in memo_t.
     Then the memo_node and lookup field in record_context can be replaced with the adequate result.
 * If it refer to a value from < depth-1, it cannot be fetch. 
     We still flush the state but do not change record_context, but throw an exception instead.*)
let resolve : state * reference -> state * seq = todo "todo"

(*stepping require an unfetched fragment. register the current state.*)
let register_memo_need_unfetched = todo "register_memo"

(*done so no more stepping needed. register the current state.*)
let register_memo_done = todo "register_memo"

(* Path compression

 *)
let fetch_seq (x : seq) (offset : int) (word_count : int): seq * words * seq = todo "fetch_seq"
let shift_et (et : fg_et): fg_et = todo "shift_et"

(*move a value from depth to depth+1*)
(*let shift (x: seq): seq = 
    match x with
    | Generic.Nil -> Generic.Nil
    | Generic.Single et -> Generic.Single (shift_et et)*)

(*move a value from depth to depth-1. if it refer to other value at the current level, unshift them as well.*)
let unshift_et (et : fg_et) = 
    match et with
    | Word w -> w
    | Reference r -> todo "unshift_et_reference" 
    | Barrier r -> todo "unshift_et_barrier"

let unshift_seq = todo "unshift"

let unshift_value = todo "unshift"