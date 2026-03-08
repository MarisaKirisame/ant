open Base

module Source = struct
  module T = struct
    type t = E of int | K [@@deriving eq, hash, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type source = Source.t [@@deriving eq]
type reference = { src : source; hole_idx : int } [@@deriving eq]
