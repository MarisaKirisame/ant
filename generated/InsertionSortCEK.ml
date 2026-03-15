open Ant
open Word
open Memo
open Value
open Common
let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_cont_1 = 3
let tag_cont_2 = 4
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
let rec insert memo (x0 : Value.seq) (x1 : Value.seq): exec_result = (exec_cek (pc_to_exp (int_to_pc 1))(Dynarray.of_list[(x0);(x1)])((Memo.from_constructor tag_cont_done)) memo)
let rec insertion_sort memo (x0 : Value.seq): exec_result = (exec_cek (pc_to_exp (int_to_pc 4))(Dynarray.of_list[(x0)])((Memo.from_constructor tag_cont_done)) memo)
let populate_state () =
  Memo.reset ();
  Words.reset ();add_exp (fun w_5 -> ((assert_env_length w_5 1);let (hd_0, tl_0) = (resolve w_5 K) in (match (Word.get_value hd_0) with | 0 (* tag_cont_done *) -> (exec_done w_5)| 3 (* tag_cont_1 *) -> (((w_5.state).k <- (get_next_cont tl_0));(restore_env w_5 1 tl_0);(assert_env_length w_5 2);let ctor_arg_4 = (pop_env w_5) in let ctor_arg_5 = (pop_env w_5) in ((push_env w_5 (Memo.appends [(Memo.from_constructor tag_Cons);ctor_arg_5;ctor_arg_4]));(if_kont_0 ())))| 4 (* tag_cont_2 *) -> (((w_5.state).k <- (get_next_cont tl_0));(restore_env w_5 1 tl_0);(assert_env_length w_5 2);(ignore (env_call w_5 [] 2));((w_5.state).c <- (pc_to_exp (int_to_pc 1))))| _ -> failwith "unreachable (0)"))) 0;
add_exp (fun w_0 -> ((assert_env_length w_0 2);(push_env w_0 (Dynarray.get ((w_0.state).e) 1));((w_0.state).c <- (pc_to_exp (int_to_pc 3))))) 1;
add_exp (fun w_2 -> ((assert_env_length w_2 6);let x0_0 = (resolve w_2 (Source.E 4)) in let x1_0 = (resolve w_2 (Source.E 5)) in ((ignore (pop_env w_2));(ignore (pop_env w_2));(push_env w_2 (Memo.from_int ((if (Word.get_value (fst x0_0)) <= (Word.get_value (fst x1_0)) then 1 else 0))));(assert_env_length w_2 5);let cond_0 = (resolve w_2 (Source.E 4)) in ((ignore (pop_env w_2));let if_kont_0 = ((fun _ -> ((assert_env_length w_2 5);(drop_n w_2 5 2);(assert_env_length w_2 3);(return_n w_2 3 (pc_to_exp (int_to_pc 0)))))) in if ((Word.get_value (fst cond_0)) <> 0) then ((assert_env_length w_2 4);(push_env w_2 (Dynarray.get ((w_2.state).e) 0));(assert_env_length w_2 5);(push_env w_2 (Dynarray.get ((w_2.state).e) 1));(assert_env_length w_2 6);let ctor_arg_2 = (pop_env w_2) in let ctor_arg_3 = (pop_env w_2) in ((push_env w_2 (Memo.appends [(Memo.from_constructor tag_Cons);ctor_arg_3;ctor_arg_2]));(if_kont_0 ()))) else ((assert_env_length w_2 4);(push_env w_2 (Dynarray.get ((w_2.state).e) 2));(assert_env_length w_2 5);(push_env w_2 (Dynarray.get ((w_2.state).e) 0));(assert_env_length w_2 6);(push_env w_2 (Dynarray.get ((w_2.state).e) 3));(assert_env_length w_2 7);let keep_0 = (env_call w_2 [4] 2) in ((w_2.state).k <- (Memo.appends [(Memo.from_constructor tag_cont_1);keep_0;((w_2.state).k)]));((w_2.state).c <- (pc_to_exp (int_to_pc 1)))))))) 2;
add_exp (fun w_1 -> ((assert_env_length w_1 3);let last_0 = (Source.E 2) in let x_0 = (resolve w_1 last_0) in (match (Word.get_value (fst x_0)) with | 1 (* tag_Nil *) -> ((ignore (pop_env w_1));(assert_env_length w_1 2);(push_env w_1 (Dynarray.get ((w_1.state).e) 0));(assert_env_length w_1 3);(push_env w_1 (Memo.from_constructor tag_Nil));(assert_env_length w_1 4);let ctor_arg_0 = (pop_env w_1) in let ctor_arg_1 = (pop_env w_1) in ((push_env w_1 (Memo.appends [(Memo.from_constructor tag_Cons);ctor_arg_1;ctor_arg_0]));(assert_env_length w_1 3);(return_n w_1 3 (pc_to_exp (int_to_pc 0)))))| 2 (* tag_Cons *) -> let splits_0 = (Memo.splits (snd x_0)) in let split0_0 = (List.nth splits_0 0) in let split1_0 = (List.nth splits_0 1) in ((ignore (pop_env w_1));(push_env w_1 split0_0);(push_env w_1 split1_0);(assert_env_length w_1 4);(push_env w_1 (Dynarray.get ((w_1.state).e) 0));(assert_env_length w_1 5);(push_env w_1 (Dynarray.get ((w_1.state).e) 2));((w_1.state).c <- (pc_to_exp (int_to_pc 2))))| _ -> failwith "unreachable (3)"))) 3;
add_exp (fun w_3 -> ((assert_env_length w_3 1);(push_env w_3 (Dynarray.get ((w_3.state).e) 0));((w_3.state).c <- (pc_to_exp (int_to_pc 5))))) 4;
add_exp (fun w_4 -> ((assert_env_length w_4 2);let last_1 = (Source.E 1) in let x_1 = (resolve w_4 last_1) in (match (Word.get_value (fst x_1)) with | 1 (* tag_Nil *) -> ((ignore (pop_env w_4));(assert_env_length w_4 1);(push_env w_4 (Memo.from_constructor tag_Nil));(assert_env_length w_4 2);(return_n w_4 2 (pc_to_exp (int_to_pc 0))))| 2 (* tag_Cons *) -> let splits_1 = (Memo.splits (snd x_1)) in let split0_1 = (List.nth splits_1 0) in let split1_1 = (List.nth splits_1 1) in ((ignore (pop_env w_4));(push_env w_4 split0_1);(push_env w_4 split1_1);(assert_env_length w_4 3);(push_env w_4 (Dynarray.get ((w_4.state).e) 1));(assert_env_length w_4 4);(push_env w_4 (Dynarray.get ((w_4.state).e) 2));(assert_env_length w_4 5);let keep_1 = (env_call w_4 [3] 1) in ((w_4.state).k <- (Memo.appends [(Memo.from_constructor tag_cont_2);keep_1;((w_4.state).k)]));((w_4.state).c <- (pc_to_exp (int_to_pc 4))))| _ -> failwith "unreachable (5)"))) 5;
Words.set_constructor_degree 0 (1);
Words.set_constructor_degree 1 (1);
Words.set_constructor_degree 2 (-1);
Words.set_constructor_degree 3 (-1);
Words.set_constructor_degree 4 (-1);;