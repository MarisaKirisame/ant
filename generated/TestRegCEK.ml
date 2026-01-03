open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_Pair = 3
let tag_Wrap = 4
let tag_cont_1 = 5
let tag_cont_2 = 6
let tag_cont_3 = 7
let tag_cont_4 = 8
let tag_cont_5 = 9
let tag_cont_6 = 10
let tag_cont_7 = 11
let tag_cont_8 = 12
let tag_cont_9 = 13
let tag_cont_10 = 14

type 'a list = Nil | Cons of 'a * 'a list

let rec from_ocaml_list from_generic_a x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) ->
      Memo.appends [ Memo.from_constructor tag_Cons; from_generic_a x0; from_ocaml_list (fun x -> from_generic_a x) x1 ]

let rec to_ocaml_list to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 (* tag_Nil *) -> Nil
  | 2 (* tag_Cons *) ->
      let x0, x1 = Memo.splits_2 t in
      Cons (to_generic_a x0, to_ocaml_list (fun x -> to_generic_a x) x1)
  | _ -> failwith "unreachable"

type ('a, 'b) pair = Pair of 'a * 'b

let rec from_ocaml_pair from_generic_a from_generic_b x =
  match x with Pair (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Pair; from_generic_a x0; from_generic_b x1 ]

let rec to_ocaml_pair to_generic_a to_generic_b x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 3 (* tag_Pair *) ->
      let x0, x1 = Memo.splits_2 t in
      Pair (to_generic_a x0, to_generic_b x1)
  | _ -> failwith "unreachable"

type 'a wrap = Wrap of 'a

let rec from_ocaml_wrap from_generic_a x =
  match x with Wrap x0 -> Memo.appends [ Memo.from_constructor tag_Wrap; from_generic_a x0 ]

let rec to_ocaml_wrap to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 4 (* tag_Wrap *) ->
      let x0 = Memo.splits_1 t in
      Wrap (to_generic_a x0)
  | _ -> failwith "unreachable"

let rec nil memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cons memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 2)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_op memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_const_op memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_nested_let memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 7)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_if memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 9)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_nested_if memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 11)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_match memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 15)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_match2 memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 17)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_wrap memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 20)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_all memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 22)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_22 ->
      assert_env_length w_22 1;
      let hd_0, tl_0 = resolve w_22 K in
      match Word.get_value hd_0 with
      | c_0 when c_0 = tag_cont_done -> exec_done w_22
      | c_0 when c_0 = tag_cont_1 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          set_env w_22 2 (Dynarray.get w_22.state.e 3);
          assert_env_length w_22 4;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 5;
          grow_env (* call args *) w_22 2;
          assert_env_length w_22 7;
          set_env w_22 5 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 7;
          set_env w_22 6 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 7;
          let keep_1 = env_call w_22 [ 1; 3; 4 ] 2 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 2)
      | c_0 when c_0 = tag_cont_2 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 3 tl_0;
          set_env w_22 4 (Dynarray.get w_22.state.e 5);
          assert_env_length w_22 6;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 7;
          grow_env (* call args *) w_22 2;
          set_env w_22 7 (Memo.from_int 1);
          set_env w_22 8 (Memo.from_int 2);
          assert_env_length w_22 9;
          let keep_2 = env_call w_22 [ 1; 3; 5; 6 ] 2 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 3)
      | c_0 when c_0 = tag_cont_3 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 4 tl_0;
          set_env w_22 6 (Dynarray.get w_22.state.e 7);
          assert_env_length w_22 8;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 9;
          grow_env (* call args *) w_22 1;
          set_env w_22 9 (Memo.from_int 0);
          assert_env_length w_22 10;
          let keep_3 = env_call w_22 [ 1; 3; 5; 7; 8 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 5)
      | c_0 when c_0 = tag_cont_4 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 5 tl_0;
          set_env w_22 8 (Dynarray.get w_22.state.e 9);
          assert_env_length w_22 10;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 11;
          grow_env (* call args *) w_22 1;
          set_env w_22 11 (Memo.from_int 0);
          assert_env_length w_22 12;
          let keep_4 = env_call w_22 [ 1; 3; 5; 7; 9; 10 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 7)
      | c_0 when c_0 = tag_cont_5 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 6 tl_0;
          set_env w_22 10 (Dynarray.get w_22.state.e 11);
          assert_env_length w_22 12;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 13;
          grow_env (* call args *) w_22 1;
          set_env w_22 13 (Memo.from_int 0);
          assert_env_length w_22 14;
          let keep_5 = env_call w_22 [ 1; 3; 5; 7; 9; 11; 12 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 9)
      | c_0 when c_0 = tag_cont_6 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 7 tl_0;
          set_env w_22 12 (Dynarray.get w_22.state.e 13);
          assert_env_length w_22 14;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 15;
          grow_env (* call args *) w_22 1;
          set_env w_22 15 (Memo.from_int 0);
          assert_env_length w_22 16;
          let keep_6 = env_call w_22 [ 1; 3; 5; 7; 9; 11; 13; 14 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 11)
      | c_0 when c_0 = tag_cont_7 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 8 tl_0;
          set_env w_22 14 (Dynarray.get w_22.state.e 15);
          assert_env_length w_22 16;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 17;
          grow_env (* call args *) w_22 1;
          assert_env_length w_22 18;
          grow_env (* call to constructor *) w_22 2;
          set_env w_22 18 (Memo.from_int 0);
          set_env w_22 19 (Memo.from_constructor tag_Nil);
          set_env w_22 17
            (Memo.appends
               [ Memo.from_constructor tag_Cons; Dynarray.get w_22.state.e 18; Dynarray.get w_22.state.e 19 ]);
          assert_env_length w_22 20;
          shrink_env w_22 2;
          assert_env_length w_22 18;
          let keep_7 = env_call w_22 [ 1; 3; 5; 7; 9; 11; 13; 15; 16 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 15)
      | c_0 when c_0 = tag_cont_8 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 9 tl_0;
          set_env w_22 16 (Dynarray.get w_22.state.e 17);
          assert_env_length w_22 18;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 19;
          grow_env (* call args *) w_22 1;
          assert_env_length w_22 20;
          grow_env (* call to constructor *) w_22 2;
          set_env w_22 20 (Memo.from_int 1);
          set_env w_22 21 (Memo.from_int 2);
          set_env w_22 19
            (Memo.appends
               [ Memo.from_constructor tag_Pair; Dynarray.get w_22.state.e 20; Dynarray.get w_22.state.e 21 ]);
          assert_env_length w_22 22;
          shrink_env w_22 2;
          assert_env_length w_22 20;
          let keep_8 = env_call w_22 [ 1; 3; 5; 7; 9; 11; 13; 15; 17; 18 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 17)
      | c_0 when c_0 = tag_cont_9 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 10 tl_0;
          set_env w_22 18 (Dynarray.get w_22.state.e 19);
          assert_env_length w_22 20;
          grow_env (* let *) w_22 1;
          assert_env_length w_22 21;
          grow_env (* call args *) w_22 1;
          assert_env_length w_22 22;
          grow_env (* call to constructor *) w_22 1;
          set_env w_22 22 (Memo.from_int 0);
          set_env w_22 21 (Memo.appends [ Memo.from_constructor tag_Wrap; Dynarray.get w_22.state.e 22 ]);
          assert_env_length w_22 23;
          shrink_env w_22 1;
          assert_env_length w_22 22;
          let keep_9 = env_call w_22 [ 1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 20 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 20)
      | c_0 when c_0 = tag_cont_10 ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 11 tl_0;
          set_env w_22 20 (Dynarray.get w_22.state.e 21);
          set_env w_22 20 (Memo.from_int 0);
          set_env w_22 18 (Dynarray.get w_22.state.e 20);
          assert_env_length w_22 22;
          shrink_env w_22 1;
          set_env w_22 16 (Dynarray.get w_22.state.e 18);
          assert_env_length w_22 21;
          shrink_env w_22 1;
          set_env w_22 14 (Dynarray.get w_22.state.e 16);
          assert_env_length w_22 20;
          shrink_env w_22 1;
          set_env w_22 12 (Dynarray.get w_22.state.e 14);
          assert_env_length w_22 19;
          shrink_env w_22 1;
          set_env w_22 10 (Dynarray.get w_22.state.e 12);
          assert_env_length w_22 18;
          shrink_env w_22 1;
          set_env w_22 8 (Dynarray.get w_22.state.e 10);
          assert_env_length w_22 17;
          shrink_env w_22 1;
          set_env w_22 6 (Dynarray.get w_22.state.e 8);
          assert_env_length w_22 16;
          shrink_env w_22 1;
          set_env w_22 4 (Dynarray.get w_22.state.e 6);
          assert_env_length w_22 15;
          shrink_env w_22 1;
          set_env w_22 2 (Dynarray.get w_22.state.e 4);
          assert_env_length w_22 14;
          shrink_env w_22 1;
          set_env w_22 1 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 13;
          shrink_env w_22 1;
          return_n_with w_22 12 (Dynarray.get w_22.state.e 1) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (0)")
    0

let () =
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      grow_env (* return value *) w_0 1;
      set_env w_0 1 (Memo.from_constructor tag_Nil);
      return_n_with w_0 2 (Dynarray.get w_0.state.e 1) (pc_to_exp (int_to_pc 0)))
    1

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      grow_env (* return value *) w_1 1;
      assert_env_length w_1 3;
      grow_env (* call to constructor *) w_1 2;
      set_env w_1 2
        (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_1.state.e 0; Dynarray.get w_1.state.e 1 ]);
      assert_env_length w_1 5;
      shrink_env w_1 2;
      return_n_with w_1 3 (Dynarray.get w_1.state.e 2) (pc_to_exp (int_to_pc 0)))
    2

let () =
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      grow_env (* return value *) w_2 1;
      assert_env_length w_2 3;
      grow_env (* op *) w_2 2;
      w_2.state.c <- pc_to_exp (int_to_pc 4))
    3

let () =
  add_exp
    (fun w_3 ->
      let x0_0 = resolve w_3 (Source.E 0) in
      let x1_0 = resolve w_3 (Source.E 1) in
      let r_0 = Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)) in
      set_env w_3 2 r_0;
      assert_env_length w_3 5;
      shrink_env w_3 2;
      return_n_with w_3 3 (Dynarray.get w_3.state.e 2) (pc_to_exp (int_to_pc 0)))
    4

let () =
  add_exp
    (fun w_4 ->
      assert_env_length w_4 1;
      grow_env (* return value *) w_4 1;
      assert_env_length w_4 2;
      grow_env (* let *) w_4 1;
      assert_env_length w_4 3;
      grow_env (* op *) w_4 2;
      set_env w_4 3 (Memo.from_int 1);
      set_env w_4 4 (Memo.from_int 2);
      w_4.state.c <- pc_to_exp (int_to_pc 6))
    5

let () =
  add_exp
    (fun w_5 ->
      let x0_1 = resolve w_5 (Source.E 3) in
      let x1_1 = resolve w_5 (Source.E 4) in
      let r_1 = Memo.from_int (Word.get_value (fst x0_1) + Word.get_value (fst x1_1)) in
      set_env w_5 2 r_1;
      assert_env_length w_5 5;
      shrink_env w_5 2;
      assert_env_length w_5 3;
      set_env w_5 2 (Dynarray.get w_5.state.e 2);
      set_env w_5 1 (Dynarray.get w_5.state.e 2);
      assert_env_length w_5 3;
      shrink_env w_5 1;
      return_n_with w_5 2 (Dynarray.get w_5.state.e 1) (pc_to_exp (int_to_pc 0)))
    6

let () =
  add_exp
    (fun w_6 ->
      assert_env_length w_6 1;
      grow_env (* return value *) w_6 1;
      assert_env_length w_6 2;
      grow_env (* let *) w_6 1;
      assert_env_length w_6 3;
      grow_env (* let *) w_6 1;
      assert_env_length w_6 4;
      grow_env (* op *) w_6 2;
      set_env w_6 4 (Memo.from_int 1);
      set_env w_6 5 (Memo.from_int 2);
      w_6.state.c <- pc_to_exp (int_to_pc 8))
    7

let () =
  add_exp
    (fun w_7 ->
      let x0_2 = resolve w_7 (Source.E 4) in
      let x1_2 = resolve w_7 (Source.E 5) in
      let r_2 = Memo.from_int (Word.get_value (fst x0_2) + Word.get_value (fst x1_2)) in
      set_env w_7 3 r_2;
      assert_env_length w_7 6;
      shrink_env w_7 2;
      assert_env_length w_7 4;
      set_env w_7 3 (Dynarray.get w_7.state.e 3);
      set_env w_7 2 (Dynarray.get w_7.state.e 3);
      assert_env_length w_7 4;
      shrink_env w_7 1;
      assert_env_length w_7 3;
      set_env w_7 2 (Dynarray.get w_7.state.e 2);
      set_env w_7 1 (Dynarray.get w_7.state.e 2);
      assert_env_length w_7 3;
      shrink_env w_7 1;
      return_n_with w_7 2 (Dynarray.get w_7.state.e 1) (pc_to_exp (int_to_pc 0)))
    8

let () =
  add_exp
    (fun w_8 ->
      assert_env_length w_8 1;
      grow_env (* return value *) w_8 1;
      assert_env_length w_8 2;
      grow_env (* if *) w_8 2;
      assert_env_length w_8 4;
      grow_env (* op *) w_8 2;
      set_env w_8 5 (Memo.from_int 0);
      w_8.state.c <- pc_to_exp (int_to_pc 10))
    9

let () =
  add_exp
    (fun w_9 ->
      let x0_3 = resolve w_9 (Source.E 0) in
      let x1_3 = resolve w_9 (Source.E 5) in
      let r_3 = Memo.from_int (if Word.get_value (fst x0_3) < Word.get_value (fst x1_3) then 1 else 0) in
      set_env w_9 2 r_3;
      assert_env_length w_9 6;
      shrink_env w_9 2;
      assert_env_length w_9 4;
      let cond_0 = resolve w_9 (Source.E 2) in
      if Word.get_value (fst cond_0) <> 0 then (
        set_env w_9 3 (Memo.from_int 1);
        set_env w_9 1 (Dynarray.get w_9.state.e 3);
        assert_env_length w_9 4;
        shrink_env w_9 2;
        return_n_with w_9 2 (Dynarray.get w_9.state.e 1) (pc_to_exp (int_to_pc 0)))
      else (
        set_env w_9 3 (Memo.from_int 2);
        set_env w_9 1 (Dynarray.get w_9.state.e 3);
        assert_env_length w_9 4;
        shrink_env w_9 2;
        return_n_with w_9 2 (Dynarray.get w_9.state.e 1) (pc_to_exp (int_to_pc 0))))
    10

let () =
  add_exp
    (fun w_10 ->
      assert_env_length w_10 1;
      grow_env (* return value *) w_10 1;
      assert_env_length w_10 2;
      grow_env (* if *) w_10 2;
      assert_env_length w_10 4;
      grow_env (* op *) w_10 2;
      assert_env_length w_10 6;
      grow_env (* if *) w_10 2;
      assert_env_length w_10 8;
      grow_env (* op *) w_10 2;
      set_env w_10 9 (Memo.from_int 0);
      w_10.state.c <- pc_to_exp (int_to_pc 14))
    11

let () =
  add_exp
    (fun w_12 ->
      let x0_5 = resolve w_12 (Source.E 4) in
      let x1_5 = resolve w_12 (Source.E 5) in
      let r_5 = Memo.from_int (if Word.get_value (fst x0_5) < Word.get_value (fst x1_5) then 1 else 0) in
      set_env w_12 2 r_5;
      assert_env_length w_12 6;
      shrink_env w_12 2;
      assert_env_length w_12 4;
      let cond_1 = resolve w_12 (Source.E 2) in
      if Word.get_value (fst cond_1) <> 0 then (
        set_env w_12 3 (Memo.from_int 1);
        set_env w_12 1 (Dynarray.get w_12.state.e 3);
        assert_env_length w_12 4;
        shrink_env w_12 2;
        return_n_with w_12 2 (Dynarray.get w_12.state.e 1) (pc_to_exp (int_to_pc 0)))
      else (
        set_env w_12 3 (Memo.from_int 2);
        set_env w_12 1 (Dynarray.get w_12.state.e 3);
        assert_env_length w_12 4;
        shrink_env w_12 2;
        return_n_with w_12 2 (Dynarray.get w_12.state.e 1) (pc_to_exp (int_to_pc 0))))
    12

let () =
  add_exp
    (fun w_13 ->
      let x0_6 = resolve w_13 (Source.E 4) in
      let x1_6 = resolve w_13 (Source.E 5) in
      let r_6 = Memo.from_int (if Word.get_value (fst x0_6) < Word.get_value (fst x1_6) then 1 else 0) in
      set_env w_13 2 r_6;
      assert_env_length w_13 6;
      shrink_env w_13 2;
      assert_env_length w_13 4;
      let cond_1 = resolve w_13 (Source.E 2) in
      if Word.get_value (fst cond_1) <> 0 then (
        set_env w_13 3 (Memo.from_int 1);
        set_env w_13 1 (Dynarray.get w_13.state.e 3);
        assert_env_length w_13 4;
        shrink_env w_13 2;
        return_n_with w_13 2 (Dynarray.get w_13.state.e 1) (pc_to_exp (int_to_pc 0)))
      else (
        set_env w_13 3 (Memo.from_int 2);
        set_env w_13 1 (Dynarray.get w_13.state.e 3);
        assert_env_length w_13 4;
        shrink_env w_13 2;
        return_n_with w_13 2 (Dynarray.get w_13.state.e 1) (pc_to_exp (int_to_pc 0))))
    13

let () =
  add_exp
    (fun w_11 ->
      let x0_4 = resolve w_11 (Source.E 0) in
      let x1_4 = resolve w_11 (Source.E 9) in
      let r_4 = Memo.from_int (if Word.get_value (fst x0_4) < Word.get_value (fst x1_4) then 1 else 0) in
      set_env w_11 6 r_4;
      assert_env_length w_11 10;
      shrink_env w_11 2;
      assert_env_length w_11 8;
      let cond_2 = resolve w_11 (Source.E 6) in
      if Word.get_value (fst cond_2) <> 0 then (
        set_env w_11 7 (Memo.from_int 1);
        set_env w_11 4 (Dynarray.get w_11.state.e 7);
        assert_env_length w_11 8;
        shrink_env w_11 2;
        set_env w_11 5 (Memo.from_int 1);
        w_11.state.c <- pc_to_exp (int_to_pc 12))
      else (
        set_env w_11 7 (Memo.from_int 0);
        set_env w_11 4 (Dynarray.get w_11.state.e 7);
        assert_env_length w_11 8;
        shrink_env w_11 2;
        set_env w_11 5 (Memo.from_int 1);
        w_11.state.c <- pc_to_exp (int_to_pc 13)))
    14

let () =
  add_exp
    (fun w_14 ->
      assert_env_length w_14 1;
      grow_env (* return value *) w_14 1;
      assert_env_length w_14 2;
      grow_env (* match *) w_14 2;
      assert_env_length w_14 4;
      set_env w_14 2 (Dynarray.get w_14.state.e 0);
      w_14.state.c <- pc_to_exp (int_to_pc 16))
    15

let () =
  add_exp
    (fun w_15 ->
      assert_env_length w_15 4;
      let x_0 = resolve w_15 (Source.E 2) in
      match Word.get_value (fst x_0) with
      | 1 (* tag_Nil *) ->
          set_env w_15 3 (Memo.from_int 0);
          set_env w_15 1 (Dynarray.get w_15.state.e 3);
          assert_env_length w_15 4;
          shrink_env w_15 2;
          return_n_with w_15 2 (Dynarray.get w_15.state.e 1) (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          assert_env_length w_15 4;
          push_env w_15 split0_0;
          push_env w_15 split1_0;
          set_env w_15 3 (Memo.from_int 1);
          assert_env_length w_15 6;
          shrink_env w_15 2;
          set_env w_15 1 (Dynarray.get w_15.state.e 3);
          assert_env_length w_15 4;
          shrink_env w_15 2;
          return_n_with w_15 2 (Dynarray.get w_15.state.e 1) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (16)")
    16

let () =
  add_exp
    (fun w_16 ->
      assert_env_length w_16 1;
      grow_env (* return value *) w_16 1;
      assert_env_length w_16 2;
      grow_env (* match *) w_16 2;
      assert_env_length w_16 4;
      set_env w_16 2 (Dynarray.get w_16.state.e 0);
      w_16.state.c <- pc_to_exp (int_to_pc 19))
    17

let () =
  add_exp
    (fun w_18 ->
      let x0_7 = resolve w_18 (Source.E 4) in
      let x1_7 = resolve w_18 (Source.E 5) in
      let r_7 = Memo.from_int (Word.get_value (fst x0_7) + Word.get_value (fst x1_7)) in
      set_env w_18 3 r_7;
      assert_env_length w_18 8;
      shrink_env w_18 2;
      assert_env_length w_18 6;
      shrink_env w_18 2;
      set_env w_18 1 (Dynarray.get w_18.state.e 3);
      assert_env_length w_18 4;
      shrink_env w_18 2;
      return_n_with w_18 2 (Dynarray.get w_18.state.e 1) (pc_to_exp (int_to_pc 0)))
    18

let () =
  add_exp
    (fun w_17 ->
      assert_env_length w_17 4;
      let x_1 = resolve w_17 (Source.E 2) in
      match Word.get_value (fst x_1) with
      | 3 (* tag_Pair *) ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          let split1_1 = List.nth splits_1 1 in
          assert_env_length w_17 4;
          push_env w_17 split0_1;
          push_env w_17 split1_1;
          assert_env_length w_17 6;
          grow_env (* op *) w_17 2;
          w_17.state.c <- pc_to_exp (int_to_pc 18)
      | _ -> failwith "unreachable (19)")
    19

let () =
  add_exp
    (fun w_19 ->
      assert_env_length w_19 1;
      grow_env (* return value *) w_19 1;
      assert_env_length w_19 2;
      grow_env (* match *) w_19 2;
      assert_env_length w_19 4;
      set_env w_19 2 (Dynarray.get w_19.state.e 0);
      w_19.state.c <- pc_to_exp (int_to_pc 21))
    20

let () =
  add_exp
    (fun w_20 ->
      assert_env_length w_20 4;
      let x_2 = resolve w_20 (Source.E 2) in
      match Word.get_value (fst x_2) with
      | 4 (* tag_Wrap *) ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          assert_env_length w_20 4;
          push_env w_20 split0_2;
          assert_env_length w_20 5;
          set_env w_20 3 (Dynarray.get w_20.state.e 4);
          assert_env_length w_20 5;
          shrink_env w_20 1;
          set_env w_20 1 (Dynarray.get w_20.state.e 3);
          assert_env_length w_20 4;
          shrink_env w_20 2;
          return_n_with w_20 2 (Dynarray.get w_20.state.e 1) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (21)")
    21

let () =
  add_exp
    (fun w_21 ->
      assert_env_length w_21 1;
      grow_env (* return value *) w_21 1;
      assert_env_length w_21 2;
      grow_env (* let *) w_21 1;
      assert_env_length w_21 3;
      grow_env (* call args *) w_21 1;
      set_env w_21 3 (Memo.from_int 0);
      assert_env_length w_21 4;
      let keep_0 = env_call w_21 [ 1; 2 ] 1 in
      w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_21.state.k ];
      w_21.state.c <- pc_to_exp (int_to_pc 1))
    22

let () = Words.set_constructor_degree 0 1
let () = Words.set_constructor_degree 1 1
let () = Words.set_constructor_degree 2 (-1)
let () = Words.set_constructor_degree 3 (-1)
let () = Words.set_constructor_degree 4 0
let () = Words.set_constructor_degree 5 (-2)
let () = Words.set_constructor_degree 6 (-3)
let () = Words.set_constructor_degree 7 (-4)
let () = Words.set_constructor_degree 8 (-5)
let () = Words.set_constructor_degree 9 (-6)
let () = Words.set_constructor_degree 10 (-7)
let () = Words.set_constructor_degree 11 (-8)
let () = Words.set_constructor_degree 12 (-9)
let () = Words.set_constructor_degree 13 (-10)
let () = Words.set_constructor_degree 14 (-11)
