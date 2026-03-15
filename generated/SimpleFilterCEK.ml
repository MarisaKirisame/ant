open Ant
open Word
open Memo
open Value
open Common
let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
type int_list =
| Nil
| Cons of int * int_list
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
let rec filter_pos memo (x0 : Value.seq): exec_result = (exec_cek (pc_to_exp (int_to_pc 1))(Dynarray.of_list[(x0)])((Memo.from_constructor tag_cont_done)) memo)
let populate_state () =
  Memo.reset ();
  Words.reset ();add_exp (fun w_3 -> ((assert_env_length w_3 1);let (hd_0, tl_0) = (resolve w_3 K) in (match (Word.get_value hd_0) with | 0 (* tag_cont_done *) -> (exec_done w_3)| _ -> failwith "unreachable (0)"))) 0;
add_exp (fun w_0 -> ((assert_env_length w_0 1);(push_env w_0 (Dynarray.get ((w_0.state).e) 0));((w_0.state).c <- (pc_to_exp (int_to_pc 3))))) 1;
add_exp (fun w_2 -> ((assert_env_length w_2 5);let x0_0 = (resolve w_2 (Source.E 3)) in let x1_0 = (resolve w_2 (Source.E 4)) in ((ignore (pop_env w_2));(ignore (pop_env w_2));(push_env w_2 (Memo.from_int ((if (Word.get_value (fst x0_0)) > (Word.get_value (fst x1_0)) then 1 else 0))));(assert_env_length w_2 4);let cond_0 = (resolve w_2 (Source.E 3)) in ((ignore (pop_env w_2));let if_kont_0 = ((fun _ -> ((assert_env_length w_2 4);(drop_n w_2 4 2);(assert_env_length w_2 2);(return_n w_2 2 (pc_to_exp (int_to_pc 0)))))) in if ((Word.get_value (fst cond_0)) <> 0) then ((assert_env_length w_2 3);(push_env w_2 (Dynarray.get ((w_2.state).e) 0));(if_kont_0 ())) else ((assert_env_length w_2 3);(push_env w_2 (Dynarray.get ((w_2.state).e) 2));(if_kont_0 ())))))) 2;
add_exp (fun w_1 -> ((assert_env_length w_1 2);let last_0 = (Source.E 1) in let x_0 = (resolve w_1 last_0) in (match (Word.get_value (fst x_0)) with | 1 (* tag_Nil *) -> ((ignore (pop_env w_1));(assert_env_length w_1 1);(push_env w_1 (Memo.from_constructor tag_Nil));(assert_env_length w_1 2);(return_n w_1 2 (pc_to_exp (int_to_pc 0))))| 2 (* tag_Cons *) -> let splits_0 = (Memo.splits (snd x_0)) in let split0_0 = (List.nth splits_0 0) in let split1_0 = (List.nth splits_0 1) in ((ignore (pop_env w_1));(push_env w_1 split0_0);(push_env w_1 split1_0);(assert_env_length w_1 3);(push_env w_1 (Dynarray.get ((w_1.state).e) 1));(assert_env_length w_1 4);(push_env w_1 (Memo.from_int 0));((w_1.state).c <- (pc_to_exp (int_to_pc 2))))| _ -> failwith "unreachable (3)"))) 3;
Words.set_constructor_degree 0 (1);
Words.set_constructor_degree 1 (1);
Words.set_constructor_degree 2 (-1);;