open Ant
open Word
open Memo
open Value
open Common
let words_runtime = Words.create_runtime ()
let memo_runtime = Memo.create_runtime ()
let with_runtime_ f = Words.with_runtime words_runtime (fun () -> Memo.with_runtime memo_runtime f)
module Words0 = Words
module Words = struct include Words0
  let set_constructor_degree ctag degree = with_runtime_ (fun () -> Words0.set_constructor_degree ctag degree)
end
module Memo0 = Memo
module Memo = struct include Memo0
  let add_exp f pc = with_runtime_ (fun () -> Memo0.add_exp f pc)
  let pc_to_exp pc = with_runtime_ (fun () -> Memo0.pc_to_exp pc)
  let init_memo () = with_runtime_ Memo0.init_memo
end
let add_exp = Memo.add_exp
let pc_to_exp = Memo.pc_to_exp
let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
type int_list = | Nil | Cons of int * int_list;;
let rec from_ocaml_int_list x =
  match x with
  | Nil ->
    Memo.appends [Memo.from_constructor tag_Nil]
  | Cons (x0, x1) ->
    Memo.appends [Memo.from_constructor tag_Cons; (Memo.from_int x0); from_ocaml_int_list x1]
let rec to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with| 1 (* tag_Nil *) ->
    Nil| 2 (* tag_Cons *) ->
    let (x0, x1) = Memo.splits_2 t in
    Cons ((Word.get_value (Memo.to_word x0)), to_ocaml_int_list x1)
  | _ -> failwith "unreachable"
let rec sum memo (x0 : Value.seq) (x1 : Value.seq): exec_result = (exec_cek (pc_to_exp (int_to_pc 1))(Dynarray.of_list[(x0);(x1)])((Memo.from_constructor tag_cont_done)) memo)
let () = add_exp (fun w_3 -> ((assert_env_length w_3 1);let (hd_0, tl_0) = (resolve w_3 K) in (match (Word.get_value hd_0) with | c_1 when c_1 = tag_cont_done -> (exec_done w_3)| _ -> failwith "unreachable (0)"))) 0
let () = add_exp (fun w_0 -> ((assert_env_length w_0 2);(push_env w_0 (Dynarray.get ((w_0.state).e) 0));((w_0.state).c <- (pc_to_exp (int_to_pc 3))))) 1
let () = add_exp (fun w_2 -> ((assert_env_length w_2 7);let x0_0 = (resolve w_2 (Source.E 5)) in let x1_0 = (resolve w_2 (Source.E 6)) in ((ignore (pop_env w_2));(ignore (pop_env w_2));(push_env w_2 (Memo.from_int ((Word.get_value (fst x0_0)) + (Word.get_value (fst x1_0)))));(assert_env_length w_2 6);(ignore (env_call w_2 [] 2));((w_2.state).c <- (pc_to_exp (int_to_pc 1)))))) 2
let () = add_exp (fun w_1 -> ((assert_env_length w_1 3);let last_0 = (Source.E 2) in let x_0 = (resolve w_1 last_0) in (match (Word.get_value (fst x_0)) with | c_0 when c_0 = tag_Nil -> ((ignore (pop_env w_1));(assert_env_length w_1 2);(push_env w_1 (Dynarray.get ((w_1.state).e) 1));(assert_env_length w_1 3);(return_n w_1 3 (pc_to_exp (int_to_pc 0))))| c_0 when c_0 = tag_Cons -> let splits_0 = (Memo.splits (snd x_0)) in let split0_0 = (List.nth splits_0 0) in let split1_0 = (List.nth splits_0 1) in ((ignore (pop_env w_1));(push_env w_1 split0_0);(push_env w_1 split1_0);(assert_env_length w_1 4);(push_env w_1 (Dynarray.get ((w_1.state).e) 3));(assert_env_length w_1 5);(push_env w_1 (Dynarray.get ((w_1.state).e) 2));(assert_env_length w_1 6);(push_env w_1 (Dynarray.get ((w_1.state).e) 1));((w_1.state).c <- (pc_to_exp (int_to_pc 2))))| _ -> failwith "unreachable (3)"))) 3
let () = Words.set_constructor_degree 0 (1)
let () = Words.set_constructor_degree 1 (1)
let () = Words.set_constructor_degree 2 (-1)