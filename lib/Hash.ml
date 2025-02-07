module type MonoidHash = sig
  type t

  val unit : t
  val mul : t -> t -> t
  val hash : t -> int
  val valid : t -> bool
  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val from_int : int -> t
  val name : string
end

module SL2 : MonoidHash = struct
  type t

  external __unit : unit -> t = "sl2_unit_stub"
  external mul : t -> t -> t = "sl2_mul_stub"
  external valid : t -> bool = "sl2_valid_stub"
  external eq : t -> t -> bool = "sl2_eq_stub"
  external cmp : t -> t -> int = "sl2_cmp_stub"
  external from_int : int -> t = "sl2_from_int_stub"
  external hash : t -> int = "sl2_hash_stub"

  let unit = __unit ()
  let name = "SL2"
end

module SL2Slow : MonoidHash = struct
  type t

  external __unit : unit -> t = "sl2_slow_unit_stub"
  external mul : t -> t -> t = "sl2_slow_mul_stub"
  external valid : t -> bool = "sl2_slow_valid_stub"
  external eq : t -> t -> bool = "sl2_slow_eq_stub"
  external cmp : t -> t -> int = "sl2_slow_cmp_stub"
  external from_int : int -> t = "sl2_slow_from_int_stub"
  external hash : t -> int = "sl2_slow_hash_stub"

  let unit = __unit ()
  let name = "SL2Slow"
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
  let name = "MCRC32C"
end

module DebugHash : MonoidHash = struct
  type t = Empty | Single of int | Mult of t * t

  let rec to_list_aux (t : t) (acc : int list) =
    match t with Empty -> acc | Single i -> i :: acc | Mult (x, y) -> to_list_aux x (to_list_aux y acc)

  let to_list t = to_list_aux t []
  let unit = Empty
  let from_int i = Single i
  let mul x y = Mult (x, y)
  let valid _ = true

  let hash x =
    let s = Core.Hash.alloc () in
    let s = List.fold_left (fun acc i -> Core.Hash.fold_int acc i) s (to_list x) in
    Core.Hash.get_hash_value s

  let eq x y = to_list x = to_list y
  let cmp x y = compare (to_list x) (to_list y)
  let name = "DebugHash"
end
