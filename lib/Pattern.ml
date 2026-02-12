open Value
open Words
open BatFingerTree

(* Design notes on the pattern/value/word relationship now live in
   docs/internal.md#patterns-as-finger-trees-patternml. *)
(*todo: do we actually need hole_count?*)
type measure = Value.measure_t

(*Invariant: consecutive constructors should be fused*)
type pattern = Value.value

type pat = Value.fg_et = Words of words | Reference of reference | PVar of int

let make_pvar n =
  assert (n > 0);
  PVar n

let monoid : measure monoid = Value.monoid
let pat_measure (p : pat) : measure = Value.measure p
let pattern_measure (p : pattern) : measure = Value.summary p
let pattern_is_empty (x : pattern) : bool = Generic.is_empty x
let pattern_rear_exn (p : pattern) : pattern * pat = Generic.rear_exn ~monoid ~measure:pat_measure p
let pattern_front_exn (p : pattern) : pat * pattern = Value.front_exn p
let pattern_cons (p : pat) (q : pattern) : pattern = Value.value_cons p q
let pattern_snoc (p : pattern) (q : pat) : pattern = Value.value_snoc p q
let pattern_append_unsafe x y = Generic.append ~monoid ~measure:pat_measure x y
let pattern_cons_unsafe x y = Value.value_cons_unsafe x y
let pattern_snoc_unsafe x y = Value.value_snoc_unsafe x y
let pattern_append (x : pattern) (y : pattern) : pattern = Value.append x y
let pattern_slice (p : pattern) (offset : int) : pattern * pattern = Value.pop_n p offset

(*todo: rename all pop_n to drop_n*)
let pattern_pop_n (p : pattern) (n : int) : pattern =
  let _, x = pattern_slice p n in
  x

(*let rec pattern_valid x : bool =
  match Generic.front x ~monoid ~measure:pat_measure with
  | None -> true
  | Some (rest, y) -> (
      match Generic.front rest ~monoid ~measure:pat_measure with
      | None -> true
      | Some (_, z) -> (
          match (y, z) with
          | Words _, Words _ -> false
          | Words _, _ -> true
          | PVar _, PVar _ -> false
          | PVar _, _ -> true
          | Reference _, _ -> true)
          && pattern_valid rest)*)
