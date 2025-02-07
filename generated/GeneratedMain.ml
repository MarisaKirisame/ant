open TestCEK
open Ant
open Common

let rec to_ocaml x = match to_ocaml_int_list x with Nil -> [] | Cons (x, l) -> x :: to_ocaml l
let rec from_ocaml x = match x with [] -> int_list_Nil | xh :: xt -> int_list_Cons xh (from_ocaml xt)
let run x = print_endline (String.concat " " (List.map string_of_int (to_ocaml (list_incr (from_ocaml x)))))
let _ = run (List.init 200 (fun i -> 0))
let _ = run (List.init 201 (fun i -> 0))
let _ = run (List.init 202 (fun i -> 0))
let _ = run (List.init 203 (fun i -> 0))
let _ = run (List.init 100 (fun i -> i + 1))
let _ = run (List.init 102 (fun i -> i + 1))
