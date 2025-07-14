open Value
module Hashtbl = Core.Hashtbl

type env = value Dynarray.t

(* One thing we might want is to have exp as an adt,
 *   which allow memoizing exp fragment to incrementalize the program under changes.
 * However, it
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

(* Record Mode:
 * Adding new entries to the memo require entering record mode,
 * Which track fetched fragment versus unfetched fragment.
 * Fetched fragment can be used normally,
 *   while the unfetched fragment cannot be used(forced).
 * Upon attempt to force an unfetched fragment, the current recording is completed,
 *   which will add a new entry to the memo.
 * Then ant will try to extend the recording by fetching the fragment,
 *   which will again run until stuck, and a new entries will then be added.
 * To extend the fetch require us to keep a old copy of the CEK machine.
 * Additionally, during record mode, ant might enter record mode again,
 *   to record a more fine-grained entries which will not skip as far, but will fire more often.
 * This mean that the CEK machine form a non-empty stack, so ant can extend the recording at any level.
 *
 * The stack can then be assigned a depth, where the root CEK machine (which does not do recording whatsowever)
 *   have a depth of 0, and an extension increase the depth by 1.
 * Values also have their own individual depth, which denote when they are created/last fetched.
 *
 * Todo: the stack is a bad name. Rename to history.
 *)
and state = {
  mutable c : exp;
  mutable e : env;
  mutable k : kont;
  d : depth_t;
  (*step_count*)
  mutable sc : int;
  mutable r : record_state option;
}

(* Needed in Record Mode *)
and record_state = {
  (* s f r die earlier then m so they are separated. *)
  m : state;
  s : store;
  (* instead of mutating the next level, which cannot be undone eaily, path compression work on this lifted value *)
  l : lifted;
  mutable f : fetch_count;
  mutable r : memo_node_t ref option;
}

(* The Store
 * A fetch can be partial, so the remaining fragment need to be fetched again.
 *   they are appended into the Store.
 * Partial fetching on consecutive value result in exponentially longer and longer fetching length,
 *   done by pairing each value a ref of length, aliased on all fetch of the same origin, growing exponentially.
 *)
and store = value Dynarray.t
and lifted = (source, value) Hashtbl.t

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
 *   We are doing some work here to ensure every progress_t advance the execution: 
 *     there is no zero-distance jump.
 *)
and memo_t = memo_node_t ref Array.t

and memo_node_t =
  (* We know transiting need to resolve a fetch_request to continue. *)
  | Need of { next : memo_next_t; progress : progress_t }
  (* We have not made progress. Need more fetching. *)
  | Continue of memo_next_t
  (* The machine evaluate to a value. *)
  | Done of done_t
  (* We know a bit, but the evaluation is ended prematurely. Still it is better to skip to there. *)
  | Halfway of progress_t
  (* We are figuring out this entry. *)
  | BlackHole

and memo_next_t = { request : fetch_request; lookup : lookup_t }
and done_t = { skip : record_state -> state }
and fetch_request = { src : source; offset : int; word_count : int }

(*todo: maybe try janestreet's hashtable. we want lookup to be as fast as possible so it might be worth to ffi some SOTA*)
and lookup_t = (fetch_hash, memo_node_t ref) Hashtbl.t

and progress_t = {
  (* ++depth. *)
  enter : record_state -> state;
  (* --depth.
   * When a memoization run is finished, we need to replace the caller (the machine at last_t) with the current machine.
   *   Doing this require shifting all value of depth x to depth x-1.
   *   This is done by resolving reference to depth x-1.
   *)
  exit : state -> state;
}

and fetch_hash = int
