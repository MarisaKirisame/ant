open Ant
module Word = Seq.Word

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
  | _ -> failwith "unreachable"

let rec list_incr =
 fun x -> match to_ocaml_int_list x with Nil -> int_list_Nil | Cons xhxt -> int_list_Cons (xh + 1, list_incr xt)
