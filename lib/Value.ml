open Word
include Reference

(* Tree values with optional references to CEK sources. *)
type value = Node of Word.t * value list | Reference of reference
and seq = value

let pack_tag = -1
let node (label : Word.t) (children : value list) : value = Node (label, children)

let pack (children : value list) : value =
  match children with
  | [] -> Node (Word.ConstructorTag pack_tag, [])
  | [ v ] -> v
  | _ -> Node (Word.ConstructorTag pack_tag, children)

let is_pack = function Node (ConstructorTag tag, _) when tag = pack_tag -> true | _ -> false

let unpack_seq (v : value) : value list =
  match v with Node (ConstructorTag tag, children) when tag = pack_tag -> children | _ -> [ v ]

let string_of_src (src : source) : string =
  match src with Source.E i -> "E(" ^ string_of_int i ^ ")" | Source.K -> "K"

let string_of_reference (r : reference) : string =
  let str = string_of_src r.src in
  if r.hole_idx = 0 then str else str ^ "@" ^ string_of_int r.hole_idx

let rec string_of_value (v : value) : string =
  match v with
  | Reference r -> string_of_reference r
  | Node (label, children) ->
      let label_str = Word.to_string label in
      if children = [] then label_str else label_str ^ "[" ^ String.concat "," (List.map string_of_value children) ^ "]"

let rec equal_value (x : value) (y : value) : bool =
  match (x, y) with
  | Reference r1, Reference r2 -> Reference.equal_reference r1 r2
  | Node (l1, c1), Node (l2, c2) ->
      Word.equal l1 l2 && List.length c1 = List.length c2 && List.for_all2 equal_value c1 c2
  | _ -> false
