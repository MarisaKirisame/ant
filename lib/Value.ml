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
 * A key invariant is that a machine at depth x only contain value with depth x or with depth x+1,
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
