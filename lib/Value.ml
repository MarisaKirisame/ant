open BatFingerTree
module Hasher = Hash.MCRC32C
open Word
include Reference

(* Values extend the word finger tree with References so we can represent
 * partially materialised environments/continuations.
 *)
type fg_et = Word of Word.t | Reference of reference [@@deriving eq]

type value = (fg_et, measure_t) Generic.fg
and seq = value
and measure_t = { degree : int; max_degree : int; full : full_measure_t option }
and full_measure_t = { length : int; hash : Hasher.t }

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
        match w with Int _ -> 1 | ConstructorTag value -> Dynarray.get Words.constructor_degree_table value
      in
      let w_repr_tag, w_repr_value = Word.raw_repr w in
      {
        degree;
        (*todo: this should be 0. fix and rerun.*)
        max_degree = degree;
        full = Some { length = 1; hash = Hasher.mul (Hasher.from_int w_repr_tag) (Hasher.from_int w_repr_value) };
      }
  | Reference r -> { degree = r.values_count; max_degree = r.values_count; full = None }

let summary x = Generic.measure ~monoid ~measure x
let append (x : seq) (y : seq) : seq = Generic.append ~monoid ~measure x y

(* pop_n semantics are documented in docs/internal.md#value-slicing-semantics-valueml. *)
let rec pop_n (s : seq) (n : int) : seq * seq =
  assert (n >= 0);
  if n = 0 then (Generic.empty, s)
  else (
    assert ((summary s).degree >= n);
    assert ((summary s).max_degree >= n);
    (* split stops at the first node whose max_degree reaches the target;
       this guarantees the head of [y] holds the boundary element we need to
       include in the left slice. *)
    let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
    let w, v = Generic.front_exn ~monoid ~measure y in
    let m = summary x in
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
        let l = Generic.snoc ~monoid ~measure x (Reference { v with values_count = need }) in
        if v.values_count = need then (l, w)
        else
          let r =
            Generic.cons ~monoid ~measure w
              (Reference { v with offset = v.offset + need; values_count = v.values_count - need })
          in
          (l, r))

(* Slice a seq with a given `offset` and `values_count` with `pop_n` *)
let slice (seq : seq) (offset : int) (values_count : int) : seq =
  let m = summary seq in
  assert (m.degree = m.max_degree);
  if m.degree < offset + values_count then
    print_endline ("slice: degree " ^ string_of_int m.degree ^ " but need " ^ string_of_int (offset + values_count));
  assert (m.degree >= offset + values_count);
  let _, x = pop_n seq offset in
  let y, _ = pop_n x values_count in
  y

let string_of_src (src : source) : string =
  match src with Source.E i -> "E(" ^ string_of_int i ^ ")" | Source.K -> "K"

let string_of_reference (r : reference) : string =
  let str = string_of_src r.src in
  let str = if r.hole_idx = 0 then str else str ^ "@" ^ string_of_int r.hole_idx in
  let str = if r.offset = 0 then str else str ^ "+" ^ string_of_int r.offset in
  let str = if r.values_count = 1 then str else str ^ ":" ^ string_of_int r.values_count in
  str

let string_of_fg_et (et : fg_et) : string =
  match et with Word w -> Word.to_string w | Reference r -> string_of_reference r

let rec string_of_value_aux (v : value) : string =
  if Generic.is_empty v then ""
  else
    let v, w = Generic.front_exn ~monoid ~measure v in
    string_of_fg_et w ^ string_of_value_aux v

let string_of_value (v : value) : string = string_of_value_aux v ^ "(degree=" ^ string_of_int (summary v).degree ^ ")"
