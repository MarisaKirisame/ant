open Value
open Word

(* Tree-shaped patterns with holes. *)
type pattern = PVar of int | PNode of Word.t * pattern list

let make_pvar n =
  assert (n > 0);
  PVar n

let pack (children : pattern list) : pattern = PNode (Word.ConstructorTag Value.pack_tag, children)

let rec pattern_pvar_count = function
  | PVar _ -> 1
  | PNode (_, children) -> List.fold_left (fun acc p -> acc + pattern_pvar_count p) 0 children

let rec pattern_pvar_length = function
  | PVar n -> n
  | PNode (_, children) -> List.fold_left (fun acc p -> acc + pattern_pvar_length p) 0 children

let rec pattern_size = function
  | PVar _ -> 1
  | PNode (_, children) -> 1 + List.fold_left (fun acc p -> acc + pattern_size p) 0 children

let rec string_of_pattern p =
  match p with
  | PVar n -> "PVar(" ^ string_of_int n ^ ")"
  | PNode (label, children) ->
      let label_str = Word.to_string label in
      if children = [] then label_str
      else label_str ^ "[" ^ String.concat "," (List.map string_of_pattern children) ^ "]"
