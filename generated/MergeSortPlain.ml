type  int_list  = Nil| Cons of (int) * (int_list);;

type  pair_int_lists  = P of (int_list) * (int_list);;

let rec (my_split_aux) = fun (x) (y) -> (match y with (Nil) -> (P ((Nil), (x)))| (Cons (y0, yt)) -> (match yt with (Nil) -> (P ((Nil), (x)))| (Cons (y1, ys)) -> (match x with (Nil) -> (P ((Nil), (Nil)))| (Cons (xh, xt)) -> (match (my_split_aux) (xt) (ys) with (P (l, r)) -> (P ((Cons ((xh), (l))), (r)))))));;

let rec (my_split) = fun (x) -> ((my_split_aux) (x) (x));;

let rec (my_merge) = fun (x) (y) -> (match x with (Nil) -> (y)| (Cons (xh, xt)) -> (match y with (Nil) -> (x)| (Cons (yh, yt)) -> (if ((xh)<(yh)) then (Cons ((xh), ((my_merge) (xt) (y)))) else (Cons ((yh), ((my_merge) (x) (yt)))))));;

let rec (mergesort) = fun (x) -> (match x with (Nil) -> (Nil)| (Cons (x1, xt)) -> (match xt with (Nil) -> (x)| (Cons (x2, xt2)) -> (match (my_split) (x) with (P (l, r)) -> ((my_merge) ((mergesort) (l)) ((mergesort) (r))))))