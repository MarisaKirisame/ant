open TestCEK
open Ant
open Common

let rec to_ocaml x = match to_ocaml_int_list x with Nil -> [] | Cons (x, l) -> x :: to_ocaml l
let rec from_ocaml x = match x with [] -> int_list_Nil | xh :: xt -> int_list_Cons xh (from_ocaml xt)
let run x = List.map string_of_int (to_ocaml (list_incr (from_ocaml x)))
let list_length = 100 (*todo: error when 100*)

(* Run the test with a large input to ensure it works correctly *)
let rec loop i =
  if i < 30 then
    let _ = run (List.init list_length (fun i -> i + 5)) in
    loop (i + 1)

(*todo: negative number is broken*)
let _ = print_endline "warmup..."
let _ = loop 0
let _ = print_endline "warmup done!"
let _ = print_endline "testing insert on the end..."
let _ = run (List.init list_length (fun i -> i + 10))
let _ = run (List.init list_length (fun i -> i + 10))
let _ = print_endline "testing insert on the end done!"
let _ = print_endline "testing insert on the front..."
let _ = run (List.init list_length (fun i -> i))
let _ = run (List.init list_length (fun i -> i))
let _ = print_endline "testing insert on the front done!"
