open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2

type int_list = Nil | Cons of int * int_list

let rec from_ocaml_int_list x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Cons; Memo.from_int x0; from_ocaml_int_list x1 ]

let rec to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 (* tag_Nil *) -> Nil
  | 2 (* tag_Cons *) ->
      let x0, x1 = Memo.splits_2 t in
      Cons (Word.get_value (Memo.to_word x0), to_ocaml_int_list x1)
  | _ -> failwith "unreachable"

let rec sum memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      let hd_0, tl_0 = resolve w_3 K in
      match Word.get_value hd_0 with 0 (* tag_cont_done *) -> exec_done w_3 | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 2;
      grow_env (* return value *) w_0 1;
      assert_env_length w_0 3;
      grow_env (* match *) w_0 2;
      assert_env_length w_0 5;
      set_env w_0 3 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 3))
    1;
  add_exp
    (fun w_2 ->
      let x0_0 = resolve w_2 (Source.E 5) in
      let x1_0 = resolve w_2 (Source.E 1) in
      let r_0 = Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)) in
      set_env w_2 8 r_0;
      assert_env_length w_2 11;
      shrink_env w_2 2;
      assert_env_length w_2 9;
      ignore (env_call w_2 [] 2);
      w_2.state.c <- pc_to_exp (int_to_pc 1))
    2;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 5;
      let x_0 = resolve w_1 (Source.E 3) in
      match Word.get_value (fst x_0) with
      | 1 (* tag_Nil *) ->
          assert_env_length w_1 5;
          set_env w_1 4 (Dynarray.get w_1.state.e 1);
          set_env w_1 2 (Dynarray.get w_1.state.e 4);
          assert_env_length w_1 5;
          shrink_env w_1 2;
          return_n_with w_1 3 (Dynarray.get w_1.state.e 2) (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          assert_env_length w_1 5;
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 7;
          grow_env (* tail call args *) w_1 2;
          assert_env_length w_1 9;
          set_env w_1 7 (Dynarray.get w_1.state.e 6);
          assert_env_length w_1 9;
          grow_env (* op *) w_1 2;
          w_1.state.c <- pc_to_exp (int_to_pc 2)
      | _ -> failwith "unreachable (3)")
    3;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1)
