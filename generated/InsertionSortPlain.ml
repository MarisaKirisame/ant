type int_list = Nil | Cons of int * int_list

let rec insert =
 fun elt x ->
  match x with Nil -> Cons (elt, Nil) | Cons (xh, xt) -> if elt <= xh then Cons (elt, x) else Cons (xh, insert elt xt)

let rec insertion_sort = fun x -> match x with Nil -> Nil | Cons (xh, xt) -> insert xh (insertion_sort xt)
