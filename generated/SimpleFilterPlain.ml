type int_list = Nil | Cons of int * int_list

let rec filter_pos =
 fun x -> match x with Nil -> Nil | Cons (xh, xt) -> if xh > 50 then Cons (xh, filter_pos xt) else filter_pos xt
