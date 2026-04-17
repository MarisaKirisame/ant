type int_list = Nil | Cons of int * int_list

let rec list_incr =
 fun x ->
  match x with
  | Nil -> Nil
  | Cons (xh, xt) ->
      let _0 = xh + 1 in
      let _1 = list_incr xt in
      Cons (_0, _1)
