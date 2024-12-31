open BatFingerTree

exception EXN of string

let panic msg = raise (EXN msg)
let todo msg = panic ("todo: " ^ msg)

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
  let get_bitmask (t : t) : tag = t lor tag_to_bitmask max_tag
  let get_tag (t : t) : tag = bitmask_to_tag (get_bitmask t)
  let get_value (t : t) : value = t land lnot (tag_to_bitmask max_tag)

  (*fast path to/from int*)
  let from_int (t : t) : int = t
  let to_int (i : int) : t = i
end

type measure = {
  length : int;
  (*A degree is the number of value a string generate. 
  It can be 0 or negative due to unfinished constructors.
  
  A base type such as int have a degree of 1,
  and a constructor have a degree of 1 - number of arguments, 
  as after that number of values being fed in the degree is 1.
  *)
  degree : int;
  (*To pop a value off the string, find the earliest place where degree = 1.
  Sadly that is non-monotonic, so searching is not log time.
  But max_degree < 1 is, and the following character constitute the shortest string with degree = 1*)
  max_degree : int;
}

let monoid : measure monoid =
  {
    zero = { length = 0; degree = 0; max_degree = 0 };
    combine =
      (fun x y ->
        {
          length = x.length + y.length;
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
        });
  }

let constructor_degree_table : int Dynarray.t = Dynarray.create ()

let set_constructor_degree (ctag : int) (degree : int) : unit =
  assert (Dynarray.length constructor_degree_table == ctag);
  Dynarray.add_last constructor_degree_table degree

let measure (w : Word.t) : measure =
  let degree =
    match Word.get_tag w with
    | 0 -> 1
    | 1 -> Dynarray.get constructor_degree_table (Word.get_value w)
    | _ -> panic "unknown tag"
  in
  { length = 1; degree; max_degree = degree }

type seq = (Word.t, measure) Generic.fg

let from_constructor (ctag : int) : seq =
  Generic.singleton (Word.make Word.constructor_tag ctag)

let from_int (i : int) : seq = Generic.singleton (Word.make Word.int_tag i)

let to_int (s : seq) : int =
  assert (Generic.size s == 1);
  Generic.head_exn s

let summary (s : seq) : measure = Generic.measure ~monoid ~measure s
let length (s : seq) : int = (summary s).length
let degree (s : seq) : int = (summary s).degree
let max_degree (s : seq) : int = (summary s).max_degree
let is_empty (s : seq) = Generic.is_empty s
let empty : seq = Generic.empty
let append (x : seq) (y : seq) : seq = Generic.append ~monoid ~measure x y
let appends (x : seq list) : seq = List.fold_right append x empty
let cons (x : Word.t) (y : seq) : seq = Generic.cons ~monoid ~measure y x

let list_match (x : seq) : (Word.t * seq) option =
  Option.map (fun (x, y) -> (y, x)) (Generic.front ~monoid ~measure x)

let pop_n (s : seq) (n : int) : seq * seq =
  let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree < n) s in
  let r, w = Generic.front_exn ~monoid ~measure y in
  (Generic.snoc ~monoid ~measure x w, r)

let pop (s : seq) = pop_n s 1

let split (s : seq) (l : int) : seq * seq =
  Generic.split ~monoid ~measure (fun m -> m.length < l) s

let rec splits (x : seq) : seq list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t
