open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0

let rec f memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_2 ->
      assert_env_length w_2 1;
      let hd_0, tl_0 = resolve w_2 K in
      match Word.get_value hd_0 with c_0 when c_0 = tag_cont_done -> exec_done w_2 | _ -> failwith "unreachable (0)")
    0

let () =
  add_exp
    (fun w_0 ->
      grow_env (* program return value *) w_0 1;
      grow_env (* let *) w_0 1;
      grow_env (* op *) w_0 2;
      set_env w_0 4 (Memo.from_int 1);
      w_0.state.c <- pc_to_exp (int_to_pc 2))
    1

let () =
  add_exp
    (fun w_1 ->
      let x0_0 = resolve w_1 (Source.E 0) in
      let x1_0 = resolve w_1 (Source.E 4) in
      let r_0 = Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)) in
      set_env w_1 2 r_0;
      shrink_env w_1 2;
      set_env w_1 2 (Dynarray.get w_1.state.e 2);
      set_env w_1 1 (Dynarray.get w_1.state.e 2);
      shrink_env w_1 1;
      return_n_with w_1 2 (Dynarray.get w_1.state.e 1) (pc_to_exp (int_to_pc 0)))
    2

let () = Words.set_constructor_degree 0 1
