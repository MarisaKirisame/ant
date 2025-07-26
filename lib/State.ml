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

and state = {
  c : exp;
  e : env;
  k : kont;
  (* step_count *)
  sc : int;
}

and recording = { s : store; u : update }

(* The Store
 * A fetch can be partial, so the remaining fragment need to be fetched again.
 *   they are appended into the Store.
 * Partial fetching on consecutive value result in exponentially longer and longer fetching length,
 *   done by pairing each value a ref of length, aliased on all fetch of the same origin, growing exponentially.
 *)
and store = value Dynarray.t
and update = memo_node_t ref

(* The memo tree is the key data structure that handle all memoization logic.
 *   It contain a fetch request, which try to fetch a reference of a length.
 *   The segment then is hashed and compared to value in a hashtable.
 *   The value inside a hashtable is another cek machine.
 *   We are doing some work here to ensure every progress advance the execution: 
 *     there is no zero-distance jump.
 *)
and memo_t = memo_node_t ref Array.t

(* The node always start from BlackHole, and may become Halfway, and finally be Need, Continue, or Done. *)
and memo_node_t =
  (* We know transiting need to resolve a fetch_request to continue. *)
  | Need of { current : shared; next : memo_next_t }
  (* The machine evaluate to a value. *)
  | Done of shared
  (* We know a bit, but the evaluation is ended prematurely. Still it is better to skip to there. *)
  | Halfway of shared
  (* We are figuring out this entry. *)
  (* Ref: the concept of Haskell's black hole *)
  | BlackHole

and memo_next_t = { request : fetch_request; lookup : lookup_t }
and fetch_request = { src : source; offset : int; word_count : int }

(*todo: maybe try janestreet's hashtable. we want lookup to be as fast as possible so it might be worth to ffi some SOTA*)
and lookup_t = (fetch_hash, memo_node_t ref) Hashtbl.t
and fetch_hash = int
and shared = Shared of state

let copy_state (Shared s) : state =
  let c = s.c in
  let e = Dynarray.map (fun v -> v) s.e in
  let k = s.k in
  let sc = s.sc in
  { c; e; k; sc }
