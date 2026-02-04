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
  hash : Hasher.t Lazy.t;
}

let monoid : measure monoid =
  {
    zero = { length = 0; degree = 0; max_degree = 0; hash = lazy Hasher.unit };
    combine =
      (fun x y ->
        {
          length = x.length + y.length;
          degree = x.degree + y.degree;
          max_degree = max x.max_degree (x.degree + y.max_degree);
          hash = lazy (Hasher.mul (Lazy.force x.hash) (Lazy.force y.hash));
        });
  }

let constructor_degree_table : int Dynarray.t = Dynarray.create ()
let reset () = Dynarray.clear constructor_degree_table

let set_constructor_degree (ctag : int) (degree : int) : unit =
  assert (Dynarray.length constructor_degree_table = ctag);
  Dynarray.add_last constructor_degree_table degree

let measure (w : Word.t) : measure =
  let degree = match w with Int _ -> 1 | ConstructorTag value -> Dynarray.get constructor_degree_table value in
  { length = 1; degree; max_degree = degree; hash = lazy (Hasher.from_int (Word.hash w)) }

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
let hash_slot = Profile.register_slot Profile.memo_profile "hash"
let hash (s : words) : Hasher.t = Profile.with_slot hash_slot (fun _ -> Lazy.force (summary s).hash)
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
let equal_words (x : words) (y : words) : bool = hash x = hash y
let pop (s : words) = pop_n s 1

let slice_length (s : words) (l : int) : words * words =
  let x, y = Generic.split ~monoid ~measure (fun m -> m.length > l) s in
  (*assert ((summary x).length = l);*)
  (x, y)

let rec splits (x : words) : words list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t

let lcp_length (x : words) (y : words) : int =
  let return n =
    (*let xl, _ = slice_length x n in
    let yl, _ = slice_length y n in
    assert (equal_words xl yl);
    (if n < min (length x) (length y) then
       let xl1, _ = slice_length x (n + 1) in
       let yl1, _ = slice_length y (n + 1) in
       assert (not (equal_words xl1 yl1)));*)
    n
  in
  let max_common_len = min (length x) (length y) in
  (*[0, lo) is a common prefix, and we havent look at [lo, hi) yet.*)
  let rec search lo hi =
    if lo = hi then lo
    else
      let mid = (lo + hi + 1) / 2 in
      let x_prefix, _ = slice_length x mid in
      let y_prefix, _ = slice_length y mid in
      if equal_words x_prefix y_prefix then search mid hi else search lo (mid - 1)
  in
  return (search 0 max_common_len)

let words_front_exn (s : words) : Word.t * words =
  let w, v = Generic.front_exn ~monoid ~measure s in
  (v, w)

let lcp (x : words) (y : words) : words * words * words =
  (* Invariants:
   *   hi = max length of common prefix remaining to search
   * Return:
   *   (lcp of x and y, suffix of x ++ x_rest, suffix of y ++ y_rest)
   *)
  let rec search hi common x x_rest y y_rest =
    if hi = 0 then (common, append x x_rest, append y y_rest)
    else if hi < 8 then
      let xh, xt = words_front_exn x in
      let yh, yt = words_front_exn y in
      if Word.equal xh yh then search (hi - 1) (append common (singleton xh)) xt x_rest yt y_rest
      else (common, append x x_rest, append y y_rest)
    else
      let mid = (hi + 1) / 2 in
      let x_prefix, x_suffix = slice_length x mid in
      let y_prefix, y_suffix = slice_length y mid in
      if equal_words x_prefix y_prefix then search (hi - mid) (append common x_prefix) x_suffix x_rest y_suffix y_rest
      else search (mid - 1) common x_prefix (append x_suffix x_rest) y_prefix (append y_suffix y_rest)
  in
  let lx = length x in
  let ly = length y in
  let hi = min lx ly in
  let x, x_rest = (x, Generic.empty) in
  let y, y_rest = (y, Generic.empty) in
  if false && lx = ly && equal_words x y then (x, Generic.empty, Generic.empty)
  else search hi Generic.empty x x_rest y y_rest

let unwords (v : words) (w : words) : words option =
  let wl = length w in
  let vh, vt = Generic.split ~monoid ~measure (fun m -> not (m.length <= wl)) v in
  let m = summary vh in
  if m.length < wl then None
  else (
    assert (m.length = wl);
    if Lazy.force m.hash = hash w then Some vt else None)

let string_of_words (w : words) : string = Generic.to_list w |> List.map Word.to_string |> String.concat ""
