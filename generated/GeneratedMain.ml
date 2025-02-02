open TestCEK
open Ant
open Common

let rec gen i aux = if i == 0 then aux else gen (i - 1) (int_list_Cons i aux)

let rec to_ocaml x =
  match to_ocaml_int_list x with Nil -> [] | Cons (x, l) -> x :: to_ocaml l

let input_list = gen 100 int_list_Nil
let _ = todo "here"

let _ =
  print_endline
    (String.concat " "
       (List.map string_of_int (to_ocaml (list_incr input_list))))
