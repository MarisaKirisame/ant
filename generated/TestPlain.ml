type int_list = Nil | Cons of int * int_list

let rec list_incr = fun x -> match x with Nil -> Nil | Cons (xh, xt) -> Cons (xh + 1, list_incr xt)
