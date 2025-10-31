open BatFingerTree
module Hasher = Hash.MCRC32C
open Word
include Reference

(* The Value type.
 * This is the basic value type which will be manipulated by Ant under the hoold.
 * The Value type provide capabilities of that of Seq.ml, as well as *Reference*.
 *
 * Roughly speaking, Reference provide a read barrier over part of the value,
 * so read of those values will be interrupted, and corresponding memo table operation
 * will be carried out.
 *
 * Note: Value should not alias. Doing so will mess with the fetch_length, which is bad. 
 *)
type value = (fg_et, measure_t) Generic.fg
and seq = value
and fg_et = Word of Word.t | Reference of reference
and measure_t = { degree : int; max_degree : int; full : full_measure_t option }
and full_measure_t = { length : int; hash : Hasher.t }

let constructor_degree_table : int Dynarray.t = Dynarray.create ()

let set_constructor_degree (ctag : int) (degree : int) : unit =
  assert (Dynarray.length constructor_degree_table = ctag);
  Dynarray.add_last constructor_degree_table degree

let monoid : measure_t monoid =
  {
    zero = { degree = 0; max_degree = 0; full = Some { length = 0; hash = Hasher.unit } };
    combine =
      (fun x y ->
        {
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
          full =
            (match (x.full, y.full) with
            | Some fx, Some fy -> Some { length = fx.length + fy.length; hash = Hasher.mul fx.hash fy.hash }
            | _ -> None);
        });
  }

let measure (et : fg_et) : measure_t =
  match et with
  | Word w ->
      let degree =
        match w with
        | Int _ -> 1
        | ConstructorTag value -> Dynarray.get constructor_degree_table value
      in
      let (w_repr_tag, w_repr_value) = Word.raw_repr w in
      {
        degree;
        max_degree = degree;
        full = Some {
          length = 1;
          hash = Hasher.mul (Hasher.from_int w_repr_tag) (Hasher.from_int w_repr_value)
        }
      }
  | Reference r -> { degree = r.values_count; max_degree = r.values_count; full = None }

(* Split at the values level, not the finger tree node level
   Pop a specific number of elements from the seq represented by a finger tree
   The count is determined by the `n`. All the elements with a `max_degree` less than `n` are popped.
   Return a 2-tuple of
    - a seq consisting of the popped elements *and an element from the head of the rest seq*
      as the last element to check some invariants
    - the remained part of the original seq
*)
let rec pop_n (s : seq) (n : int) : seq * seq =
  assert (n >= 0);
  if n = 0 then (Generic.empty, s)
  else (
    assert ((Generic.measure ~monoid ~measure s).degree >= n);
    assert ((Generic.measure ~monoid ~measure s).max_degree >= n);
    let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
    let w, v = Generic.front_exn ~monoid ~measure y in
    let m = Generic.measure ~monoid ~measure x in
    assert (m.degree < n);
    match v with
    | Word v ->
        assert (m.degree + 1 = n);
        let l = Generic.snoc ~monoid ~measure x (Word v) in
        (l, w)
    | Reference v ->
        assert (m.degree < n);
        assert (m.degree + v.values_count >= n);
        let need = n - m.degree in
        let l = Generic.snoc ~monoid ~measure x (Reference { src = v.src; offset = v.offset; values_count = need }) in
        if v.values_count = need then (l, w)
        else
          let r =
            Generic.cons ~monoid ~measure w
              (Reference { src = v.src; offset = v.offset + need; values_count = v.values_count - need })
          in
          (l, r))

(* Slice a seq with a given `offset` and `values_count` with `pop_n` *)
let slice (seq : seq) (offset : int) (values_count : int) : seq =
  let m = Generic.measure ~monoid ~measure seq in
  assert (m.degree = m.max_degree);
  if m.degree < offset + values_count then
    print_endline ("slice: degree " ^ string_of_int m.degree ^ " but need " ^ string_of_int (offset + values_count));
  assert (m.degree >= offset + values_count);
  let _, x = pop_n seq offset in
  let y, _ = pop_n x values_count in
  y

let string_of_src (src : source) : string =
  match src with
  | Source.E i -> "E(" ^ string_of_int i ^ ")"
  | Source.S i -> "S(" ^ string_of_int i ^ ")"
  | Source.K -> "K"

let string_of_reference (r : reference) : string =
  "Reference(src: " ^ string_of_src r.src ^ ", offset: " ^ string_of_int r.offset ^ ", values_count: "
  ^ string_of_int r.values_count ^ ")"

let string_of_fg_et (et : fg_et) : string =
  match et with Word w -> "Word(" ^ Word.to_string w ^ ")" | Reference r -> string_of_reference r

let rec string_of_value (v : value) : string =
  if Generic.is_empty v then ""
  else
    let v, w = Generic.front_exn ~monoid ~measure v in
    string_of_fg_et w ^ string_of_value v
