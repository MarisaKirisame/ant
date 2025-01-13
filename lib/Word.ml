
(*The smallest unit in our value representation.
We steal the 1 bit after the sign bit for tagging.
Note that ocaml already steal 1 bit, so we are left with 63 - 1 = 62 bits.
To simplify conversion to/from other representation, they ignore the top bit.
Conversion then is a single bitwise logical and/or.
This does sadly mean non-int value representation have 63 - 1 - 1 = 61 bits.

Tag = 0:
  The value is an (signed) int.
  Note how this setup allow conversion to/from int as an noop.

Tag = 1:
  The value is a constructor.
*)
module Word = struct
  type t = int
  type tag = (*unsigned*) int

  let tag_width = 1

  type value = int

  let max_tag = (1 lsl tag_width) - 1
  let int_tag = 0
  let constructor_tag = 1
  let tag_bitmask_distance = Sys.int_size - 1 - tag_width
  let tag_to_bitmask (t : tag) : t = t lsl tag_bitmask_distance
  let bitmask_to_tag (t : tag) : t = t lsr tag_bitmask_distance
  let make (t : tag) (v : value) : t = v lor tag_to_bitmask t
  let get_bitmask (t : t) : tag = t land tag_to_bitmask max_tag
  let get_tag (t : t) : tag = bitmask_to_tag (get_bitmask t)
  let get_value (t : t) : value = t land lnot (tag_to_bitmask max_tag)

  (*fast path to/from int*)
  let from_int (t : t) : int = t
  let to_int (i : int) : t = i
end