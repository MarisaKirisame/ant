type int_list = Nil | Cons of int * int_list

let rec list_incr =
 fun x ->
  match x with
  | Nil ->
      let _0 = Nil in
      _0
  | Cons (xh, xt) ->
      let _1 = 1 in
      let _2 = xh + _1 in
      let _3 = list_incr xt in
      let _4 = Cons (_2, _3) in
      _4
