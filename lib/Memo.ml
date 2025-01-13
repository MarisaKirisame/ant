(*Required reading: Seq.ml*)
(*This file contian all code that deals with memoiziation.*) 
(*There is multiple mutually recursive component which together enable memoization.*)
(*However, three of them are the most critical:*)
(*The unmatched type. It represent fragment of the finger tree that is unavailable, as we're working on fragment of the tree*)
(*The remain log. It allow quick access for all unmatched fragment of the tree*)
(*The memo. It is a fusion between hashtable and trie, somewhat like the patrica trie, to traverse prefix at exponential rate*)

(* The environment is an array of value*)
type env = Todo

type exp = {
    (*one step transition. when done throw an exception.*)
    func : state -> state;
    (*pc is an isomorphism to func, and pc -> func is a table lookup.*)
    pc : int;
} and 
kont = Todo and
state = {
    c : exp;
    e : env;
    k : kont;
}

(* The Unmatched
 * Ant memoize a fragment of the current environment, and allow skipping to a point,
 *   where the next evaluation step require a value outside of the fragment.
 * To do so, we need to track fragment that is memoized and unmemoized.
 *   The memoized part can be represented as Word.t,
 *   and the unmemoized part is represented as a Unmatched.  
 * Unmatched contain degree/max_degree, as well as where to fetch the value (a reference).
 *)
type unmatched = Todo
type reference = Todo
type value = Todo

(* The match log 
 * When computing under memoization, we need to determine which value is memoized, and which is not.
 *   To do so, each value is paired with a time tag, dictating when is it used.
 *   If the value match with the current time, it is usable.
 *   If it is not usable, we require extra matching to register it.
 * Of course, a match can be partial.
 *   the remaining pieces are appended into the match log.
 *   The match log is an array containing two type of value: the matched and unmatched.
 *   Unmatched contain seq from the memoization caller, and matching on it turn it into a matched
 *     (Note that this is not the same as the unmatched type.)
 *   Matched contain an array index into the remaining prefix and suffix, as well as the sequence matched.
 * Partial matching on consecutive value result in exponentially longer and longer matching length,
 *   done by pairing each entry in the match log a ref of length, aliased on all match of the same origin, growing exponentially.
 * When a memoization run is finished, we need to fix all value for the caller.
 *   fixing the caller-generated values can be done by having a stack of value and popping from the stack.
 *   fixing the callee-generated values can be done by resolving all the references.
 *)
type match_log = Todo

(* The memo
 * The memo is the key data structure that handle all memoization logic.
 *   It contain a match request, which try to match a reference of a length.
 *   The match then is hashed and compared to value in a hashtable.
 *   The value inside a hashtable is a function, which mutate the env and the current value,
 *     alongside an extra memo.
 *   The caller should traverse down this memo tree until it can not find a match,
 *     then execute the function.
 *)
type memo = Todo