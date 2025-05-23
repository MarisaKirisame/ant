open BatFingerTree
module Hasher = Hash.MCRC32C
open Word

(* measure have this iff fully fetched (only Word.t, no reference). *)
type full_measure = { length : int; hash : Hasher.t }

type measure_t = { degree : int; max_degree : int; full : full_measure option }

type source = E of int | S of int | K

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
type reference = { src : source; offset : int; values_count : int }

type fg_et = Word of Word.t | Reference of reference
type seq = (fg_et, measure_t) Generic.fg

type depth_t = int
type fetch_count = int

(* Note: Value should not alias. Doing so will mess with the fetch_length, which is bad. *)
type value = {
  seq : seq;
  depth : depth_t;
  fetch_length : int ref;
  (* A value with depth x is path-compressed iff all the reference refer to value with depth < x.
   * If a value with depth x have it's compressed_since = fetch_count on depth x-1, it is path_compressed.
   *)
  compressed_since : fetch_count;
}
