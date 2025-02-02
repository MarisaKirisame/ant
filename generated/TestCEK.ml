open Ant
open Word
open Memo
open Common
type ocaml_int_list = Nil | Cons of int * Memo.seq
let int_list_Nil : Memo.seq = Memo.appends [Memo.from_constructor 1]
let int_list_Cons x0 x1: Memo.seq = Memo.appends [Memo.from_constructor 2;Memo.from_int x0;x1]
let from_ocaml_int_list x = match x with | Nil -> int_list_Nil  | Cons(x0, x1) -> int_list_Cons x0 x1
let to_ocaml_int_list x = let (h, t) = Option.get (Memo.list_match x) in match (Word.get_value h) with | 1 -> Nil | 2 -> let [x0;x1] = Memo.splits t in Cons(Memo.to_int(x0),x1)
let rec list_incr (x0 : seq): seq = todo "meow"; exec_cek (pc_to_exp 2)(Dynarray.of_list[(x0)])(Memo.from_constructor 0);;
let () = add_exp (fun x -> todo "return last element in env by applying the continuation"; x)
let () = add_exp (fun x -> todo "pp_cases"; x)
let () = add_exp (fun x -> todo "extend env with location, set pc"; x)
let () = Memo.set_constructor_degree 0 (1)
let () = Memo.set_constructor_degree 1 (1)
let () = Memo.set_constructor_degree 2 (-1)
