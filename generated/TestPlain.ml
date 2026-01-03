type 'a list  = Nil| Cons of ('a) * (('a) list);;

type 'a 'b pair  = Pair of ('a) * ('b);;

type 'a wrap  = Wrap of ('a);;

let rec (nil) = fun (_) -> (Nil);;

let rec (cons) = fun (a) (b) -> (Cons ((a), (b)));;

let rec (test_op) = fun (a) (b) -> (((a)+(b)));;

let rec (test_const_op) = fun (_) -> (let (x) = (((1)+(2))) in (x));;

let rec (test_nested_let) = fun (x) -> (let (y) = (let (z) = (((1)+(2))) in (z)) in (y));;

let rec (test_if) = fun (x) -> (if ((x)<(0)) then (1) else (2));;

let rec (test_nested_if) = fun (x) -> (if ((if ((x)<(0)) then (1) else (0))<(1)) then (1) else (2));;

let rec (test_match) = fun (x) -> (match x with (Nil) -> (0)| (Cons (a, b)) -> (1));;

let rec (test_match2) = fun (x) -> (match x with (Pair (a, b)) -> (((a)+(b))));;

let rec (test_wrap) = fun (x) -> (match x with (Wrap (a)) -> (a));;

let rec (test_all) = fun (_) -> (let (a) = ((nil) (0)) in (let (_1) = ((cons) (a) (a)) in (let (_2) = ((test_op) (1) (2)) in (let (_3) = ((test_const_op) (0)) in (let (_4) = ((test_nested_let) (0)) in (let (_5) = ((test_if) (0)) in (let (_6) = ((test_nested_if) (0)) in (let (_7) = ((test_match) (Cons ((0), (Nil)))) in (let (_8) = ((test_match2) (Pair ((1), (2)))) in (let (_9) = ((test_wrap) (Wrap ((0)))) in (0)))))))))))