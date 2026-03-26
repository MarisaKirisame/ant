module Word = struct
  type value = int
  and t = Int of value | ConstructorTag of value [@@deriving eq]

  let hash (t : t) : int =
    match t with Int value -> Hashtbl.hash (0, value) | ConstructorTag value -> Hashtbl.hash (1, value)

  let get_value (t : t) : int = match t with Int value -> value | ConstructorTag value -> value

  let to_string (t : t) : string =
    match t with
    | Int value -> "I(" ^ string_of_int value ^ ")"
    | ConstructorTag value -> "C(" ^ string_of_int value ^ ")"
end
