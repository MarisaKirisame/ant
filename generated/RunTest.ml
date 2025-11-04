open Ant
module Seq = Ant.Seq
module Word = Ant.Word.Word

type ocaml_int_list = Nil | Cons of int * Seq.seq

let () = Seq.set_constructor_degree 0 1
let word_list_Nil : Seq.seq = Seq.appends [ Seq.from_constructor 0 ]
let () = Seq.set_constructor_degree 1 (-1)
let word_list_Cons x0 x1 : Seq.seq = Seq.appends [ Seq.from_constructor 1; Seq.from_int x0; x1 ]
let from_ocaml_int_list = function Nil -> word_list_Nil | Cons (x0, x1) -> word_list_Cons x0 x1

let to_ocaml_int_list x =
  let h, t = Option.get (Seq.list_match x) in
  match Word.get_value h with
  | 0 -> Nil
  | 1 ->
      let [ x0; x1 ] = Seq.splits t in
      Cons (Word.get_value (Seq.to_word x0), x1)
  | _ -> failwith "unreachable"

let rec list_incr x =
  match to_ocaml_int_list x with Nil -> word_list_Nil | Cons (xh, xt) -> word_list_Cons (xh + 1) (list_incr xt)

let rec to_ocaml x = match to_ocaml_int_list x with Nil -> [] | Cons (xh, xt) -> xh :: to_ocaml xt
let rec from_ocaml x = match x with [] -> word_list_Nil | xh :: xt -> word_list_Cons xh (from_ocaml xt)
let run_once x = List.map string_of_int (to_ocaml (list_incr (from_ocaml x)))
let list_length = 100

let rec loop i =
  if i < 30 then
    let _ = run_once (List.init list_length (fun i -> i + 5)) in
    loop (i + 1)

let run () : unit =
  print_endline "warmup...";
  loop 0;
  print_endline "warmup done!";
  print_endline "testing insert on the end...";
  let _ = run_once (List.init list_length (fun i -> i + 10)) in
  let _ = run_once (List.init list_length (fun i -> i + 10)) in
  print_endline "testing insert on the end done!";
  print_endline "testing insert on the front...";
  let _ = run_once (List.init list_length (fun i -> i)) in
  let _ = run_once (List.init list_length (fun i -> i)) in
  print_endline "testing insert on the front done!"
