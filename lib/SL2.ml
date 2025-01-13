(*todo: we should use [@@unbox] and [@@noallo]*)
type sl2 = floatarray

(*each sl2 is two int128. additionally to align at 16 bytes we add an extra 16 bytes to ensure enough space.*)
let allocate_sl2_internal () : sl2 = Array.Floatarray.create (2 * 4 + 2)

external sl2_unit_dps : sl2 -> unit = "sl2_unit_stub"

let sl2_unit : sl2 = 
  let u = allocate_sl2_internal () in
  sl2_unit_dps u;
  u

external sl2_mul_dps : sl2 -> sl2 -> sl2 -> unit = "sl2_mul_stub"

let sl2_mul (x: sl2) (y: sl2): sl2 = 
  let z = allocate_sl2_internal () in
  sl2_mul_dps z x y;
  z

external sl2_valid : sl2 -> bool = "sl2_valid_stub"

external sl2_eq : sl2 -> sl2 -> bool = "sl2_eq_stub"

external sl2_cmp : sl2 -> sl2 -> int = "sl2_cmp_stub"

external sl2_from_int_dps : sl2 -> int -> unit = "sl2_from_int_stub"

let sl2_from_int (i: int): sl2 = 
  let x = allocate_sl2_internal () in
  sl2_from_int_dps x i;
  x

external sl2_hash : sl2 -> int = "sl2_hash_stub"
