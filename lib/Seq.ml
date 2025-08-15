open BatFingerTree
open Word
open Common

(* This file implement monoid parsing, a key technique in Ant. 
 * The key problem is:
 *   We need to treat values (ADTs) as strings of atomic size,
 *   and we need to be able to reason about partial strings - a substring of a string which represent a value.
 *
 * To this end, we define the string repesentation of a value to be the prefix traversal of that value.
 * For example, the list (Cons 0 (Cons 1 (Cons 2 Empty))) will be represented as 'Cons 0 Cons 1 Cons 2 Empty'.
 * Like wise, the tree (Node (Node (Leaf 0) (Leaf 1)) (Node (Leaf 2) (Leaf 3))) is represented as 
 *   'Node Node Leaf 0 Leaf 1 Node Leaf 2 Leaf 3'
 *
 * Essentially, we are stripping off all parenthesis from the value, and this is fine because
 *   Each constructor have a fixed arity, so the parenthesis can always be recovered, although we never explicitly do so.
 *
 * With this representation, now we can talk about substrings of values, such as 'Node Node Leaf'.
 * Such strings can be of 3 kinds:
 * - A string representing exactly a single value, such as 'Node Leaf 0 Leaf 1'.
 * - A string representing multiple values, such as 'Leaf 0 Leaf 1', which represent two values.
 * - A string representing an incomplete value, such as 'Node', which need two more values to complete and produce a value.
 *     In some sense, this string contain '-1' value: after adding 2 more values, it will have a value of 1.
 *
 * Because negative values dont really make sense, we will be calling them 'degree'.
 * A atomic value have a degree of 1, and a constructor have a degree of 1 minus the number of arguments it take.
 * For example, 'Node' have a degree of -1, and 'Leaf' have a degree of 0.
 * The degree of a string is the sum of the degrees of its components.
 * For example, 'Node Leaf 0 Leaf 1' have a degree of 0, as it have 1 constructor and 2 atomic values.
 * This allow us to pop a value from a string of degree >= 1: 
 *   we just need to find the earliest place where the prefix have a degree of 1.
 *
 * A key insight is that pattern matching can be carried out using only a partial value.
 * That is, given only the first word of the string, we can obtain the constructor tag,
 *   and thus can decide which branch to take.
 * Note that we had not obtained the value of the constructor arguments yet, but that is fine.
 * We can represent them as 'references', which point to some location in some string, to resolve them later.
 *)

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
  assert (Dynarray.length constructor_degree_table = ctag);
  Dynarray.add_last constructor_degree_table degree

let measure (w : Word.t) : measure =
  let degree =
    match Word.get_tag w with
    | 0 -> 1
    | 1 -> Dynarray.get constructor_degree_table (Word.get_value w)
    | _ -> failwith "unknown tag"
  in
  { length = 1; degree; max_degree = degree }

type seq = (Word.t, measure) Generic.fg

let from_constructor (ctag : int) : seq = Generic.singleton (Word.make Word.constructor_tag ctag)
let from_int (i : int) : seq = Generic.singleton (Word.make Word.int_tag i)

let to_int (s : seq) : int =
  assert (Generic.size s = 1);
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
let list_match (x : seq) : (Word.t * seq) option = Option.map (fun (x, y) -> (y, x)) (Generic.front ~monoid ~measure x)

let pop_n (s : seq) (n : int) : seq * seq =
  let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
  let r, w = Generic.front_exn ~monoid ~measure y in
  let l = Generic.snoc ~monoid ~measure x w in
  (l, r)

let pop (s : seq) = pop_n s 1
let split (s : seq) (l : int) : seq * seq = Generic.split ~monoid ~measure (fun m -> m.length > l) s

let rec splits (x : seq) : seq list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t
