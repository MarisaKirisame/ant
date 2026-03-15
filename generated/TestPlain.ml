type int_list = Nil | Cons of int * int_list

let rec list_incr =
 fun x ->
  match x with
  | Nil -> Nil
  | Cons (xh, xt) ->
      let _'anf0 = xh + 1 in
      let _'anf1 = list_incr xt in
      Cons (_'anf0, _'anf1)
