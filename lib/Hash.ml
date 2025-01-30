module type MonoidHash = sig
  type t

  val unit : t
  val mul : t -> t -> t
  val hash : t -> int
  val valid : t -> bool
  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val from_int : int -> t
end

module SL2 : MonoidHash = struct
  type t = floatarray

  (*each sl2 is two int128. additionally to align at 16 bytes we add an extra 16 bytes to ensure enough space.*)
  let allocate_sl2_internal () : t = Array.Floatarray.create ((2 * 4) + 2)

  external sl2_unit_dps : t -> unit = "sl2_unit_stub"

  let unit : t =
    let u = allocate_sl2_internal () in
    sl2_unit_dps u;
    u

  external sl2_mul_dps : t -> t -> t -> unit = "sl2_mul_stub"

  let mul (x : t) (y : t) : t =
    let z = allocate_sl2_internal () in
    sl2_mul_dps z x y;
    z

  external valid : t -> bool = "sl2_valid_stub"
  external eq : t -> t -> bool = "sl2_eq_stub"
  external cmp : t -> t -> int = "sl2_cmp_stub"
  external sl2_from_int_dps : t -> int -> unit = "sl2_from_int_stub"

  let from_int (i : int) : t =
    let x = allocate_sl2_internal () in
    sl2_from_int_dps x i;
    x

  external hash : t -> int = "sl2_hash_stub"
end

module MCRC32C : MonoidHash = struct
  type t = Int64.t

  external __unit : unit -> t = "m_crc32c_unit_stub"
  external mul : t -> t -> t = "m_crc32c_mul_stub"
  external from_int : int -> t = "m_crc32c_from_int_stub"
  external from_char : char -> t = "m_crc32c_from_char_stub"
  external hash : t -> int = "m_crc32c_hash_stub"

  let unit = __unit ()
  let valid _ = true
  let eq x y = Int64.equal x y
  let cmp x y = Int64.compare x y
end
