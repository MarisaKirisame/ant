open BatFingerTree
open Word
open Common
module Hasher = Hash.MCRC32C

(* Design rationale for the monoid parsing representation lives in
   docs/internal.md#monoid-parsing-wordsml. *)

type measure = {
  length : int;
  (* Degree semantics are described in docs/internal.md#monoid-parsing-wordsml. *)
  degree : int;
  (*To pop a value off the string, find the earliest place where degree = 1.
  Sadly that is non-monotonic, so searching is not log time.
  But max_degree < 1 is, and the following character constitute the shortest string with degree = 1*)
  max_degree : int;
  hash : Hasher.t;
}

let monoid : measure monoid =
  {
    zero = { length = 0; degree = 0; max_degree = 0; hash = Hasher.unit };
    combine =
      (fun x y ->
        {
          length = x.length + y.length;
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
          hash = Hasher.mul x.hash y.hash;
        });
  }

let constructor_degree_table : int Dynarray.t = Dynarray.create ()

let set_constructor_degree (ctag : int) (degree : int) : unit =
  assert (Dynarray.length constructor_degree_table = ctag);
  Dynarray.add_last constructor_degree_table degree

let measure (w : Word.t) : measure =
  let degree = match w with Int _ -> 1 | ConstructorTag value -> Dynarray.get constructor_degree_table value in
  let w_repr_tag, w_repr_value = Word.raw_repr w in
  {
    length = 1;
    degree;
    max_degree = degree;
    hash = Hasher.mul (Hasher.from_int w_repr_tag) (Hasher.from_int w_repr_value);
  }

type words = (Word.t, measure) Generic.fg

let from_constructor (ctag : int) : words = Generic.singleton (Word.ConstructorTag ctag)
let from_int (i : int) : words = Generic.singleton (Word.Int i)

let to_word (s : words) : Word.t =
  assert (Generic.size s = 1);
  Generic.head_exn s

let summary (s : words) : measure = Generic.measure ~monoid ~measure s
let length (s : words) : int = (summary s).length
let degree (s : words) : int = (summary s).degree
let max_degree (s : words) : int = (summary s).max_degree
let is_empty (s : words) = Generic.is_empty s
let empty : words = Generic.empty
let append (x : words) (y : words) : words = Generic.append ~monoid ~measure x y
let appends (x : words list) : words = List.fold_right append x empty
let cons (x : Word.t) (y : words) : words = Generic.cons ~monoid ~measure y x

let list_match (x : words) : (Word.t * words) option =
  Option.map (fun (x, y) -> (y, x)) (Generic.front ~monoid ~measure x)

let pop_n (s : words) (n : int) : words * words =
  let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
  let r, w = Generic.front_exn ~monoid ~measure y in
  let l = Generic.snoc ~monoid ~measure x w in
  (l, r)

let slice_degree (s : words) (n : int) : words * words = pop_n s n
let equal (x : words) (y : words) : bool = (summary x).hash = (summary y).hash
let pop (s : words) = pop_n s 1

let slice_length (s : words) (l : int) : words * words =
  let x, y = Generic.split ~monoid ~measure (fun m -> m.length > l) s in
  assert ((summary x).length = l);
  (x, y)

let rec splits (x : words) : words list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t
