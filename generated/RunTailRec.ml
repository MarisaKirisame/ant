(* open Ant
module Tail = TailRecCEK
module Word = Ant.Word.Word

let rec int_list_of_list = function [] -> Tail.Nil | x :: xs -> Tail.Cons (x, int_list_of_list xs)

let run_case label xs =
  let seq_list = Tail.from_ocaml_int_list (int_list_of_list xs) in
  let seq_acc = Memo.from_int 0 in
  let result = Tail.sum seq_list seq_acc in
  let total = Word.get_value (Memo.to_word result.words) in
  Printf.printf "%s -> sum=%d (took %d steps, %d without memo)\n" label total result.step result.without_memo_step *)

let run () = print_endline "commented out."
(* run_case "empty" [];
  run_case "short" [ 1; 2; 3; 4 ];
  run_case "long" (List.init 100 (fun i -> i + 1)) *)
