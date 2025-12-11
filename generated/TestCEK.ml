open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_cont_1 = 3

type int_list = Nil | Cons of int * int_list

let rec from_ocaml_int_list x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Cons; Memo.from_int x0; from_ocaml_int_list x1 ]

let rec to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_Nil -> Nil
  | c when c = tag_Cons ->
      let x0, x1 = Memo.splits_2 t in
      Cons (Word.get_value (Memo.to_word x0), to_ocaml_int_list x1)
  | _ -> failwith "unreachable"

let rec list_incr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      let hd_0, tl_0 = resolve w_3 K in
      match Word.get_value hd_0 with
      | c_1 when c_1 = tag_cont_done -> exec_done w_3
      | c_1 when c_1 = tag_cont_1 ->
          w_3.state.k <- get_next_cont tl_0;
          restore_env w_3 1 tl_0;
          assert_env_length w_3 2;
          let ctor_arg_0 = pop_env w_3 in
          let ctor_arg_1 = pop_env w_3 in
          push_env w_3 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_1; ctor_arg_0 ]);
          assert_env_length w_3 1;
          drop_n w_3 1 0;
          assert_env_length w_3 1;
          return_n w_3 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (0)")
    0

let () =
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 3))
    1

let () =
  add_exp
    (fun w_2 ->
      assert_env_length w_2 5;
      let x0_0 = resolve w_2 (Source.E 3) in
      let x1_0 = resolve w_2 (Source.E 4) in
      ignore (pop_env w_2);
      ignore (pop_env w_2);
      push_env w_2 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_2 4;
      push_env w_2 (Dynarray.get w_2.state.e 2);
      assert_env_length w_2 5;
      let keep_0 = env_call w_2 [ 3 ] 1 in
      w_2.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_2.state.k ];
      w_2.state.c <- pc_to_exp (int_to_pc 1))
    2

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      let last_0 = Source.E 1 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | c_0 when c_0 = tag_Nil ->
          ignore (pop_env w_1);
          assert_env_length w_1 1;
          push_env w_1 (Memo.from_constructor tag_Nil);
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | c_0 when c_0 = tag_Cons ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          ignore (pop_env w_1);
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 3;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          assert_env_length w_1 4;
          push_env w_1 (Memo.from_int 1);
          w_1.state.c <- pc_to_exp (int_to_pc 2)
      | _ -> failwith "unreachable (3)")
    3

let () = Words.set_constructor_degree 0 1
let () = Words.set_constructor_degree 1 1
let () = Words.set_constructor_degree 2 (-1)
let () = Words.set_constructor_degree 3 (-1)
