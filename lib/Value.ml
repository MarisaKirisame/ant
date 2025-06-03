open BatFingerTree
module Hasher = Hash.MCRC32C
open Word

(*todo: have barrier alongside value side by side, to allow force reading with violation*)
(* The Value type.
 * This is the basic value type which will be manipulated by Ant under the hoold.
 * The Value type provide capabilities of that of Seq.ml, as well as *Reference*.
 *
 * Roughly speaking, Reference provide a read barrier over part of the value,
 * so read of those values will be interrupted, and corresponding memo table operation
 * will be carried out.
 *
 * Such capability is provided by a some what Union-Find like design.
 *
 * Note: Value should not alias. Doing so will mess with the fetch_length, which is bad. 
 *)
type value = {
  seq : seq;
  (* Look like we only need a bit dictating fetchness? *)
  depth : depth_t;
  fetch_length : int ref;
  (* A value with depth x is path-compressed iff all the reference refer to value with depth < x.
   * If a value with depth x have it's compressed_since = fetch_count on depth x-1, it is path_compressed.
   *)
  compressed_since : fetch_count;
}

and seq = (fg_et, measure_t) Generic.fg
and fg_et = Word of Word.t | Reference of reference
and depth_t = int
and fetch_count = int

(* measure have this iff fully fetched (only Word.t, no reference). *)
and full_measure = { length : int; hash : Hasher.t }
and measure_t = { degree : int; max_degree : int; full : full_measure option }

(* The Reference
 * To track whether a fragment is fetched or unfetched,
 *   ant extend the seq finger tree to include a Reference Type.
 * For a value with depth x+1, the Reference is an index into the C/E/S/K of the machine at depth x.
 * A key invariant is that a machine at depth x only contain values with depth x or with depth x+1,
 *   and a key collary is that machine at depth x is only able to fetch value at depth x-1:
 *   The machine only contain reference with depth x or x+1, and the latter is already fetched, so cannot be fetched again.
 *
 * If a value at depth x have a reference which refer to a value at depth x,
 *   It should path-compress lazily, as it had already been fetched, and the reference is pointless.
 *)
and reference = { src : source; offset : int; values_count : int }
and source = E of int | S of int | K

let constructor_degree_table : int Dynarray.t = Dynarray.create ()

let set_constructor_degree (ctag : int) (degree : int) : unit =
  assert (Dynarray.length constructor_degree_table = ctag);
  Dynarray.add_last constructor_degree_table degree

let monoid : measure_t monoid =
  {
    zero = { degree = 0; max_degree = 0; full = Some { length = 0; hash = Hasher.unit } };
    combine =
      (fun x y ->
        {
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
          full =
            (match (x.full, y.full) with
            | Some xf, Some yf -> Some { length = xf.length + yf.length; hash = Hasher.mul xf.hash yf.hash }
            | _ -> None);
        });
  }

let measure (et : fg_et) : measure_t =
  match et with
  | Word w ->
      let degree =
        match Word.get_tag w with
        | 0 -> 1
        | 1 -> Dynarray.get constructor_degree_table (Word.get_value w)
        | _ -> failwith "unknown tag"
      in
      { degree; max_degree = degree; full = Some { length = 1; hash = Hasher.from_int w } }
  | Reference r -> { degree = r.values_count; max_degree = r.values_count; full = None }

(* 
   Pop a specific number of elements from the seq represented by a finger tree
   The count is determined by the `n`. All the elements with a `max_degree` less than `n` are popped.
   Return a 2-tuple of
    - a seq consisting of the popped elements *and an element from the head of the rest seq*
      as the last element to check some invariants
    - the remained part of the original seq
*)
let pop_n (s : seq) (n : int) : seq * seq =
  assert (n >= 0);
  if n = 0 then (Generic.empty, s)
  else (
    assert ((Generic.measure ~monoid ~measure s).degree >= n);
    assert ((Generic.measure ~monoid ~measure s).max_degree >= n);
    let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
    let w, v = Generic.front_exn ~monoid ~measure y in
    let m = Generic.measure ~monoid ~measure x in
    assert (m.degree < n);
    match v with
    | Word v ->
        assert (m.degree + 1 = n);
        let l = Generic.snoc ~monoid ~measure x (Word v) in
        (l, w)
    | Reference v ->
        assert (m.degree < n);
        assert (m.degree + v.values_count >= n);
        let need = n - m.degree in
        let l = Generic.snoc ~monoid ~measure x (Reference { src = v.src; offset = v.offset; values_count = need }) in
        if v.values_count = need then (l, w)
        else
          let r =
            Generic.cons ~monoid ~measure w
              (Reference { src = v.src; offset = v.offset + need; values_count = v.values_count - need })
          in
          (l, r))

(* Slice a seq with a given `offset` and `values_count` with `pop_n` *)
let slice (seq : seq) (offset : int) (values_count : int) : seq =
  let m = Generic.measure ~monoid ~measure seq in
  assert (m.degree = m.max_degree);
  if m.degree < offset + values_count then
    print_endline ("slice: degree " ^ string_of_int m.degree ^ " but need " ^ string_of_int (offset + values_count));
  assert (m.degree >= offset + values_count);
  let _, x = pop_n seq offset in
  let y, _ = pop_n x values_count in
  y
