module Word = struct
  type value = int
  and t = Int of value | ConstructorTag of value [@@deriving eq]

  let get_value (t : t) : int = match t with Int value -> value | ConstructorTag value -> value

  (* Returns a hashable representation of a Word.t. *)
  let raw_repr (t : t) : int * int = match t with Int value -> (0, value) | ConstructorTag value -> (1, value)

  let to_string (t : t) : string =
    match t with
    | Int value -> "I(" ^ string_of_int value ^ ")"
    | ConstructorTag value -> "C(" ^ string_of_int value ^ ")"
end
