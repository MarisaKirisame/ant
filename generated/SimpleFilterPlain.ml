type  int_list  = Nil| Cons of (int) * (int_list);;

let rec (filter_pos) = fun (x) -> (match x with (Nil) -> (Nil)| (Cons (xh, xt)) -> (if ((xh)>(0)) then (x) else (xt)))