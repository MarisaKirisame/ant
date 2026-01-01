open BatFingerTree
module Hasher = Hash.MCRC32C
open Word
include Reference

(* Values extend the word finger tree with References so we can represent
 * partially materialised environments/continuations.
 *)
type fg_et = Words of Words.words | Reference of reference [@@deriving eq]

type value = (fg_et, measure_t) Generic.fg
and seq = value
and measure_t = { degree : int; max_degree : int }
and full_measure_t = { length : int; hash : Hasher.t }

let monoid : measure_t monoid =
  {
    zero = { degree = 0; max_degree = 0 };
    combine = (fun x y -> { degree = x.degree + y.degree; max_degree = max x.max_degree (x.degree + y.max_degree) });
  }
let measure (et : fg_et) : measure_t =
  match et with
  | Words w ->
      let m = Words.summary w in
      { degree = m.degree; max_degree = m.max_degree }
  | Reference r -> { degree = r.values_count; max_degree = r.values_count }
(*let rec value_valid x : bool =
  match Generic.front x ~monoid ~measure with
  | None -> true
  | Some (rest, x) -> (
      match Generic.front rest ~monoid ~measure with
      | None -> true
      | Some (_, y) ->
          (match (x, y) with
            | Reference _, Reference _ -> true
            | Reference _, Words _ -> true
            | Words _, Words _ -> false
            | Words _, Reference _ -> true)
          && value_valid rest)*)
let summary x = Generic.measure ~monoid ~measure x
let append (x : seq) (y : seq) : seq =
  if Generic.is_empty x then y
  else if Generic.is_empty y then x
  else
    let xh, xt = Generic.rear_exn ~monoid ~measure x in
    let yt, yh = Generic.front_exn ~monoid ~measure y in
    match (xt, yh) with
    | Words xt, Words yh ->
        Generic.append ~monoid ~measure xh (Generic.cons ~monoid ~measure yt (Words (Words.append xt yh)))
    | _ -> Generic.append ~monoid ~measure x y

let value_cons (et : fg_et) (v : seq) : seq =
  if Generic.is_empty v then Generic.singleton et
  else
    let vt, vh = Generic.front_exn ~monoid ~measure v in
    match (et, vh) with
    | Words et, Words vh -> Generic.cons ~monoid ~measure vt (Words (Words.append et vh))
    | _ -> Generic.cons ~monoid ~measure v et

(* pop_n semantics are documented in docs/internal.md#value-slicing-semantics-valueml. *)
let rec pop_n (s : seq) (n : int) : seq * seq =
  (*assert (value_valid s);*)
  assert (n >= 0);
  if n = 0 then (Generic.empty, s)
  else
    (*assert ((summary s).degree >= n);
    assert ((summary s).max_degree >= n);*)
    (* split stops at the first node whose max_degree reaches the target;
       this guarantees the head of [y] holds the boundary element we need to
       include in the left slice. *)
    let x, y = Generic.split ~monoid ~measure (fun m -> m.max_degree >= n) s in
    let m = summary x in
    assert (m.degree < n);
    let w, v = Generic.front_exn ~monoid ~measure y in
    match v with
    | Words v ->
        assert (m.degree + (Words.summary v).max_degree >= n);
        let vh, vt = Words.slice_degree v (n - m.degree) in
        let l = append x (value_cons (Words vh) Generic.empty) in
        let r = if Words.is_empty vt then w else value_cons (Words vt) w in
        (l, r)
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
          (l, r)

(* Slice a seq with a given `offset` and `values_count` with `pop_n` *)
let slice (seq : seq) (offset : int) (values_count : int) : seq =
  (*let m = summary seq in
  assert (m.degree = m.max_degree);
  if m.degree < offset + values_count then
    print_endline ("slice: degree " ^ string_of_int m.degree ^ " but need " ^ string_of_int (offset + values_count));
  assert (m.degree >= offset + values_count);*)
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
  match et with Words w -> Words.string_of_words w | Reference r -> string_of_reference r
let rec string_of_value_aux (v : value) : string =
  if Generic.is_empty v then ""
  else
    let v, w = Generic.front_exn ~monoid ~measure v in
    string_of_fg_et w ^ string_of_value_aux v

let string_of_value (v : value) : string = string_of_value_aux v ^ "(degree=" ^ string_of_int (summary v).degree ^ ")"
let front_exn (v : value) : fg_et * value =
  let w, v = Generic.front_exn ~monoid ~measure v in
  (v, w)
let unwords (v : value) (w : Words.words) : value option =
  (*assert (value_valid v);*)
  let vt, vh = Generic.front_exn ~monoid ~measure v in
  match vh with
  | Words x -> (
      let xrest = Words.unwords x w in
      match xrest with
      | Some xrest ->
          let ret = if Generic.is_empty xrest then vt else Generic.cons ~monoid ~measure vt (Words xrest) in
          (*assert (value_valid ret);*)
          Some ret
      | None -> None)
  | Reference _ -> None
