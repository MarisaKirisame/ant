(*The smallest unit in our value representation.
We steal the 1 bit after the sign bit for tagging.
Note that ocaml already steal 1 bit, so we are left with 63 - 1 = 62 bits.
To simplify conversion to/from other representation, they ignore the top bit.
Conversion then is a single bitwise logical and/or.
This does sadly mean non-int value representation have 63 - 1 - 1 = 61 bits.

TODO: support negative int

Tag = 0:
  The value is an (signed) int.
  Note how this setup allow conversion to/from int as an noop.

Tag = 1:
  The value is a constructor.
*)
module Word = struct
  type value = int
  type t = Int of value | ConstructorTag of value

  let get_value (t : t) : int =
    match t with
    | Int value -> value
    | ConstructorTag value -> value

  (* Returns a hashable representation of a Word.t. *)
  let raw_repr (t : t) : int * int =
    match t with
    | Int value -> (0, value)
    | ConstructorTag value -> (1, value)

  let to_string (t : t) : string =
    match t with
    | Int value -> "Int(" ^ string_of_int value ^ ")"
    | ConstructorTag value -> "ConstructorTag(" ^ string_of_int value ^ ")"
end
