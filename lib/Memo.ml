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
 *)
and depth_t = int

and machines = {
  c : exp;
  e : env;
  mutable k : kont;
  d : depth_t;
  last : last_t option;
}

(* The Reference
 * To track whether a fragment is fetched or unfetched,
 *   ant extend the seq finger tree to include a Reference Type.
 * For a value at depth x+1, the Reference is an index into the C/E/S/K of the machine at depth x.
 * A key invariant is that a machine at depth x+1 is only able to fetch value at depth x.
 *   If the value is at depth < x, it had not been fetched by the machine at x.
 *   If the value is at depth = x+1, it had been fetched already.
 *   It is impossible for the value to be at depth > x+1.
 *
 * If a value at depth x have a reference which refer to a value at depth x,
 *   It should path-compress lazily, as it had already been fetched, and the reference is pointless.
 *)
and reference = { src : source; offset : int; values_count : int }
and source = E of int | S of int | K

and last_t = {
  (*s and r die earlier then m so they are separated.*)
  mutable f : fetch_count;
  s : store;
  r : record_context;
  m : machines;
}

and fetch_count = int
and state = { mac : machines; mem : memo_t }

(*cannot access, as it is an unfetched reference at the memo caller.*)
and fg_et = Word of Word.t | Reference of reference
and seq = (fg_et, measure_t) Generic.fg
and measure_t = { degree : int; max_degree : int; full : full_measure option }

(* measure have this iff fully fetched (only Word.t, no reference). *)
and full_measure = { length : int; hash : Hasher.t }

(* The Store
 * A fetch can be partial, so the remaining fragment need to be fetched again.
 *   they are appended into the Store.
 * Partial fetching on consecutive value result in exponentially longer and longer fetching length,
 *   done by pairing each value a ref of length, aliased on all fetch of the same origin, growing exponentially.
 *)
and store = value Dynarray.t

and value = {
  seq : seq;
  depth : depth_t;
  fetch_length : int ref;
  (* A value with depth x is path-compressed iff all the reference refer to value with depth x-1.
   * If a value with depth x have it's compressed_since == fetch_count on depth x-1, it is path_compressed.
   *)
  compressed_since : fetch_count;
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
      (*++depth.*)
      enter : state -> state;
      (* --depth.
       * When a memoization run is finished, we need to replace the caller (the machine at last_t) with the current machine.
       *   Doing this require shifting all value of depth x to depth x-1.
       *   This is done by resolving reference to depth x-1.
       *)
      exit : state -> state;
    }
  | Done

and lookup_t = (fetch_result, memo_node_t) Hashtbl.t
and fetch_request = { src : source; offset : int; word_count : int }

(*todo: when the full suffix is fetched, try to extend at front.*)
and fetch_result = { fetched : words; have_prefix : bool; have_suffix : bool }
and words = seq
(*Just have Word.t. We could make Word a finger tree of Word.t but that would cost lots of conversion between two representation.*)

and record_context =
  | Recording of { s : store; memo_node : memo_node_t; lookup : lookup_t }

let monoid : measure_t monoid =
  {
    zero =
      {
        degree = 0;
        max_degree = 0;
        full = Some { length = 0; hash = Hasher.unit };
      };
    combine =
      (fun x y ->
        {
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
          full =
            (match (x.full, y.full) with
            | Some xf, Some yf ->
                Some
                  {
                    length = xf.length + yf.length;
                    hash = Hasher.mul xf.hash yf.hash;
                  }
            | _ -> None);
        });
  }

let constructor_degree_table : int Dynarray.t = Dynarray.create ()

let set_constructor_degree (ctag : int) (degree : int) : unit =
  assert (Dynarray.length constructor_degree_table == ctag);
  Dynarray.add_last constructor_degree_table degree

let measure (et : fg_et) : measure_t =
  match et with
  | Word w ->
      let degree =
        match Word.get_tag w with
        | 0 -> 1
        | 1 -> Dynarray.get constructor_degree_table (Word.get_value w)
        | _ -> panic "unknown tag"
      in
      {
        degree;
        max_degree = degree;
        full = Some { length = 1; hash = Hasher.from_int w };
      }
  | Reference r ->
      { degree = r.values_count; max_degree = r.values_count; full = None }

let pop_n (s : seq) (n : int) : seq * seq =
  if n == 0 then (Generic.empty, s)
  else
    let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
    let w, v = Generic.front_exn ~monoid ~measure y in
    let m = Generic.measure ~monoid ~measure x in
    assert (m.degree == m.max_degree);
    match v with
    | Word v ->
        assert (m.degree + 1 == n);
        let l = Generic.snoc ~monoid ~measure x (Word v) in
        (l, w)
    | Reference v ->
        assert (m.degree < n);
        assert (m.degree + v.values_count >= n);
        let need = n - m.degree in
        let l =
          Generic.snoc ~monoid ~measure x
            (Reference { src = v.src; offset = v.offset; values_count = need })
        in
        if v.values_count == need then (l, w)
        else
          let r =
            Generic.cons ~monoid ~measure w
              (Reference
                 {
                   src = v.src;
                   offset = v.offset + need;
                   values_count = v.values_count - need;
                 })
          in
          (l, r)

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

let get_value (l : last_t) (src : source) : value =
  match src with
  | E i -> Dynarray.get l.m.e i
  | S i -> Dynarray.get l.s i
  | K -> l.m.k

let set_value (l : last_t) (src : source) (v : value) : unit =
  match src with
  | E i -> Dynarray.set l.m.e i v
  | S i -> Dynarray.set l.s i v
  | K -> l.m.k <- v

let path_compress (l : last_t) (src : source) : value =
  let v = get_value l src in
  if (v.depth != l.m.d + 1) || (l.f == v.compressed_since) then v
  else
    let new_v = { seq = todo "path_compress"; compressed_since = l.f; depth = v.depth; fetch_length = v.fetch_length; } in
    set_value l src new_v;
    new_v

let add_to_store (l : last_t) (seq : seq) (fetch_length : int ref) : seq =
  let v = { depth = l.m.d; seq; compressed_since = 0; fetch_length } in
  let r = { src = S (Dynarray.length l.s); offset = 0; values_count = 1 } in
  Dynarray.add_last l.s v;
  Generic.singleton (Reference r)

(*move a value from depth x to depth x+1*)
let fetch_value (l : last_t) (req : fetch_request) : fetch_result =
  let v = get_value l req.src in
  (* Only value at the right depth can be fetched. 
   * If higher depth, it is already fetched so pointless to fetch again.
   * If lower depth, it is not fetched by the last level so we cannot trepass.
   *)
  assert (v.depth == l.m.d);
  let v = path_compress l req.src in
  let x, y = pop_n v.seq req.offset in
  let words, rest =
    Generic.split ~monoid ~measure
      (fun m ->
        not
          (match m.full with
          | None -> false
          | Some m -> req.word_count <= m.length))
      y
  in
  let length =
    (Option.get (Generic.measure ~monoid ~measure words).full).length
  in
  if (not (Generic.is_empty rest)) && length != req.word_count then
    (*we could try to return the shorten fragment and continue. however i doubt it is reusable so we are just cluttering the hashtable*)
    todo "fetch fail, should exit"
  else
    let transformed_x =
      if Generic.is_empty x then Generic.empty
      else add_to_store l x v.fetch_length
    in
    let transformed_rest =
      if Generic.is_empty rest then Generic.empty
        (*todo: match in the reverse direction*)
      else add_to_store l rest v.fetch_length
    in
    l.f <- l.f + 1;
    set_value l req.src
      {
        depth = v.depth + 1;
        fetch_length = v.fetch_length;
        seq =
          Generic.append ~monoid ~measure transformed_x
            (Generic.append ~monoid ~measure words transformed_rest);
        compressed_since = l.f;
      };
    {
      fetched = words;
      have_prefix = Generic.is_empty x;
      have_suffix = Generic.is_empty rest;
    }

let init_fetch_length () : int ref = ref 1

(*assuming this seq is at depth l.m.d+1, convert it to depth l.m.d*)
let rec unshift_seq (l : last_t) (x : seq) : seq =
  let lhs, rhs =
    Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x
  in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with
  | None -> lhs
  | Some (rest, Reference y) ->
      Generic.append ~monoid ~measure lhs
        (Generic.append ~monoid ~measure (unshift_reference l y)
           (unshift_seq l rest))
  | _ -> panic "impossible"

and unshift_reference (l : last_t) (r : reference) : seq =
  let v = unshift_value l r.src in
  if v.depth == l.m.d then
    let _, x = pop_n v.seq r.offset in
    let y, _ = pop_n x r.values_count in
    y
  else Generic.Single (Reference r)

(*move a value from depth x to depth x-1. if it refer to other value at the current level, unshift them as well.*)
and unshift_value (l : last_t) (src : source) : value =
  let v = get_value l src in
  if v.depth > l.m.d then (
    assert (v.depth == l.m.d + 1);
    let new_v =
      {
        seq = unshift_seq l v.seq;
        depth = l.m.d;
        fetch_length = init_fetch_length ();
        compressed_since = 0;
      }
    in
    set_value l src new_v;
    new_v)
  else v
