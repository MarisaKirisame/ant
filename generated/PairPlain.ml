type int_list = Nil | Cons of int * int_list
type int_pair_list = NilP | ConsP of int * int * int_pair_list

let rec pair =
 fun x ->
  match x with
  | Nil -> NilP
  | Cons (x1, xt1) -> ( match xt1 with Nil -> NilP | Cons (x2, xt2) -> ConsP (x1, x2, pair xt1))
