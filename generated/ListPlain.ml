open ListCEK

type int_list = ListCEK.int_list
type int_pair_list = ListCEK.int_pair_list

let rec pair =
 fun x ->
  match x with
  | Nil -> NilP
  | Cons (x1, xt1) -> ( match xt1 with Nil -> NilP | Cons (x2, xt2) -> ConsP (x1, x2, pair xt1))

let rec list_incr = fun x -> match x with Nil -> Nil | Cons (xh, xt) -> Cons (xh + 1, list_incr xt)

let rec filter_pos =
 fun x -> match x with Nil -> Nil | Cons (xh, xt) -> if xh > 50 then Cons (xh, filter_pos xt) else filter_pos xt

let rec append = fun x y -> match x with Nil -> y | Cons (xh, xt) -> Cons (xh, append xt y)
let rec reverse = fun x -> match x with Nil -> Nil | Cons (xh, xt) -> append (reverse xt) (Cons (xh, Nil))

let rec insert =
 fun elt x ->
  match x with Nil -> Cons (elt, Nil) | Cons (xh, xt) -> if elt <= xh then Cons (elt, x) else Cons (xh, insert elt xt)

let rec insertion_sort = fun x -> match x with Nil -> Nil | Cons (xh, xt) -> insert xh (insertion_sort xt)

type pair_int_lists = ListCEK.pair_int_lists

let rec my_split_aux =
 fun x y ->
  match y with
  | Nil -> P (Nil, x)
  | Cons (y0, yt) -> (
      match yt with
      | Nil -> P (Nil, x)
      | Cons (y1, ys) -> (
          match x with
          | Nil -> P (Nil, Nil)
          | Cons (xh, xt) -> ( match my_split_aux xt ys with P (l, r) -> P (Cons (xh, l), r))))

let rec my_split = fun x -> my_split_aux x x

let rec my_merge =
 fun x y ->
  match x with
  | Nil -> y
  | Cons (xh, xt) -> (
      match y with Nil -> x | Cons (yh, yt) -> if xh < yh then Cons (xh, my_merge xt y) else Cons (yh, my_merge x yt))

let rec mergesort =
 fun x ->
  match x with
  | Nil -> Nil
  | Cons (x1, xt) -> (
      match xt with
      | Nil -> x
      | Cons (x2, xt2) -> ( match my_split x with P (l, r) -> my_merge (mergesort l) (mergesort r)))

let rec filter_gt =
 fun x pivot ->
  match x with Nil -> Nil | Cons (xh, xt) -> if xh > pivot then Cons (xh, filter_gt xt pivot) else filter_gt xt pivot

let rec filter_eq =
 fun x pivot ->
  match x with Nil -> Nil | Cons (xh, xt) -> if xh = pivot then Cons (xh, filter_eq xt pivot) else filter_eq xt pivot

let rec filter_lt =
 fun x pivot ->
  match x with Nil -> Nil | Cons (xh, xt) -> if xh < pivot then Cons (xh, filter_lt xt pivot) else filter_lt xt pivot

let rec quicksort =
 fun x ->
  match x with
  | Nil -> Nil
  | Cons (xh, xt) -> append (quicksort (filter_lt x xh)) (append (filter_eq x xh) (quicksort (filter_gt x xh)))
