open TestCEK
open Ant
open Common

let rec to_ocaml x = match to_ocaml_int_list x with Nil -> [] | Cons (x, l) -> x :: to_ocaml l
let rec from_ocaml x = match x with [] -> int_list_Nil | xh :: xt -> int_list_Cons xh (from_ocaml xt)
let run x = List.map string_of_int (to_ocaml (list_incr (from_ocaml x)))
let _ = run (List.init 1000 (fun i -> 0))
(*let _ = run (List.init 1000 (fun i -> i + 1))
let _ = run (List.init 1002 (fun i -> i))*)
