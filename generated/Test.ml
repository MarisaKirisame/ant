[@@@warning "-8"]

open Ant
open Word

type ocaml_int_list = Nil | Cons of int * Seq.seq

let () = Seq.set_constructor_degree 0 1
let int_list_Nil : Seq.seq = Seq.appends [ Seq.from_constructor 0 ]
let () = Seq.set_constructor_degree 1 (-1)
let int_list_Cons x0 x1 : Seq.seq = Seq.appends [ Seq.from_constructor 1; Seq.from_int x0; x1 ]
let from_ocaml_int_list x = match x with Nil -> int_list_Nil | Cons (x0, x1) -> int_list_Cons x0 x1

let to_ocaml_int_list x =
  let h, t = Option.get (Seq.list_match x) in
  match Word.get_value h with
  | 0 -> Nil
  | 1 ->
      let [ x0; x1 ] = Seq.splits t in
      Cons (Seq.to_int x0, x1)

let rec list_incr =
 fun l -> match to_ocaml_int_list l with Nil -> int_list_Nil | Cons (x, l) -> int_list_Cons (x + 1) (list_incr l)
