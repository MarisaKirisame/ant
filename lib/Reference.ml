open Base

module Source = struct
  module T = struct
    (* In the state of our abstract machine, we may have a number of environments and stores.
     * Therefore, we need to index the nth environment or the nth store with the extra field. *)
    type t = E of int | S of int | K [@@deriving hash, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type source = Source.t

(* The Reference
 * To track whether a fragment is fetched or unfetched,
 *   and extend the seq finger tree to include a Reference Type.
 * For a value with depth x+1, the Reference is an index into the C/E/S/K of the machine at depth x.
 * A key invariant is that a machine at depth x only contain values with depth x or with depth x+1,
 *   and a key collary is that machine at depth x is only able to fetch value at depth x-1:
 *   The machine only contain reference with depth x or x+1, and the latter is already fetched, so cannot be fetched again.
 *)

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
type reference = { src : source; offset : int; values_count : int }
