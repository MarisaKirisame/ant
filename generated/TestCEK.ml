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
  exec_cek (pc_to_exp (int_to_pc 14)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_match2 memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 16)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_wrap memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 19)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_all memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 21)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_21 ->
      assert_env_length w_21 1;
      let hd_0, tl_0 = resolve w_21 K in
      match Word.get_value hd_0 with
      | c_3 when c_3 = tag_cont_done -> exec_done w_21
      | c_3 when c_3 = tag_cont_1 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Dynarray.get w_21.state.e 0);
          assert_env_length w_21 2;
          push_env w_21 (Dynarray.get w_21.state.e 0);
          assert_env_length w_21 3;
          let keep_1 = env_call w_21 [] 2 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 2)
      | c_3 when c_3 = tag_cont_2 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 1);
          assert_env_length w_21 2;
          push_env w_21 (Memo.from_int 2);
          assert_env_length w_21 3;
          let keep_2 = env_call w_21 [] 2 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 3)
      | c_3 when c_3 = tag_cont_3 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          let keep_3 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 5)
      | c_3 when c_3 = tag_cont_4 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          let keep_4 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 7)
      | c_3 when c_3 = tag_cont_5 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          let keep_5 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 9)
      | c_3 when c_3 = tag_cont_6 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          let keep_6 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 11)
      | c_3 when c_3 = tag_cont_7 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          push_env w_21 (Memo.from_constructor tag_Nil);
          assert_env_length w_21 3;
          let ctor_arg_2 = pop_env w_21 in
          let ctor_arg_3 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_3; ctor_arg_2 ]);
          assert_env_length w_21 2;
          let keep_7 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 14)
      | c_3 when c_3 = tag_cont_8 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 1);
          assert_env_length w_21 2;
          push_env w_21 (Memo.from_int 2);
          assert_env_length w_21 3;
          let ctor_arg_4 = pop_env w_21 in
          let ctor_arg_5 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_Pair; ctor_arg_5; ctor_arg_4 ]);
          assert_env_length w_21 2;
          let keep_8 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 16)
      | c_3 when c_3 = tag_cont_9 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          let ctor_arg_6 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_Wrap; ctor_arg_6 ]);
          assert_env_length w_21 2;
          let keep_9 = env_call w_21 [] 1 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 19)
      | c_3 when c_3 = tag_cont_10 ->
          w_21.state.k <- get_next_cont tl_0;
          restore_env w_21 0 tl_0;
          assert_env_length w_21 1;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 2;
          drop_n w_21 2 1;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          return_n w_21 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      push_env w_0 (Memo.from_constructor tag_Nil);
      assert_env_length w_0 2;
      return_n w_0 2 (pc_to_exp (int_to_pc 0)))
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      push_env w_1 (Dynarray.get w_1.state.e 0);
      assert_env_length w_1 3;
      push_env w_1 (Dynarray.get w_1.state.e 1);
      assert_env_length w_1 4;
      let ctor_arg_0 = pop_env w_1 in
      let ctor_arg_1 = pop_env w_1 in
      push_env w_1 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_1; ctor_arg_0 ]);
      assert_env_length w_1 3;
      return_n w_1 3 (pc_to_exp (int_to_pc 0)))
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      push_env w_2 (Dynarray.get w_2.state.e 0);
      assert_env_length w_2 3;
      push_env w_2 (Dynarray.get w_2.state.e 1);
      w_2.state.c <- pc_to_exp (int_to_pc 4))
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 4;
      let x0_0 = resolve w_3 (Source.E 2) in
      let x1_0 = resolve w_3 (Source.E 3) in
      ignore (pop_env w_3);
      ignore (pop_env w_3);
      push_env w_3 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_3 3;
      return_n w_3 3 (pc_to_exp (int_to_pc 0)))
    4;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 1;
      push_env w_4 (Memo.from_int 1);
      assert_env_length w_4 2;
      push_env w_4 (Memo.from_int 2);
      w_4.state.c <- pc_to_exp (int_to_pc 6))
    5;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 3;
      let x0_1 = resolve w_5 (Source.E 1) in
      let x1_1 = resolve w_5 (Source.E 2) in
      ignore (pop_env w_5);
      ignore (pop_env w_5);
      push_env w_5 (Memo.from_int (Word.get_value (fst x0_1) + Word.get_value (fst x1_1)));
      assert_env_length w_5 2;
      push_env w_5 (Dynarray.get w_5.state.e 1);
      assert_env_length w_5 3;
      drop_n w_5 3 1;
      assert_env_length w_5 2;
      return_n w_5 2 (pc_to_exp (int_to_pc 0)))
    6;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 1;
      push_env w_6 (Memo.from_int 1);
      assert_env_length w_6 2;
      push_env w_6 (Memo.from_int 2);
      w_6.state.c <- pc_to_exp (int_to_pc 8))
    7;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 3;
      let x0_2 = resolve w_7 (Source.E 1) in
      let x1_2 = resolve w_7 (Source.E 2) in
      ignore (pop_env w_7);
      ignore (pop_env w_7);
      push_env w_7 (Memo.from_int (Word.get_value (fst x0_2) + Word.get_value (fst x1_2)));
      assert_env_length w_7 2;
      push_env w_7 (Dynarray.get w_7.state.e 1);
      assert_env_length w_7 3;
      drop_n w_7 3 1;
      assert_env_length w_7 2;
      push_env w_7 (Dynarray.get w_7.state.e 1);
      assert_env_length w_7 3;
      drop_n w_7 3 1;
      assert_env_length w_7 2;
      return_n w_7 2 (pc_to_exp (int_to_pc 0)))
    8;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 1;
      push_env w_8 (Dynarray.get w_8.state.e 0);
      assert_env_length w_8 2;
      push_env w_8 (Memo.from_int 0);
      w_8.state.c <- pc_to_exp (int_to_pc 10))
    9;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 3;
      let x0_3 = resolve w_9 (Source.E 1) in
      let x1_3 = resolve w_9 (Source.E 2) in
      ignore (pop_env w_9);
      ignore (pop_env w_9);
      push_env w_9 (Memo.from_int (if Word.get_value (fst x0_3) < Word.get_value (fst x1_3) then 1 else 0));
      assert_env_length w_9 2;
      let cond_0 = resolve w_9 (Source.E 1) in
      ignore (pop_env w_9);
      let if_kont_0 =
       fun _ ->
        assert_env_length w_9 2;
        return_n w_9 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_9 1;
        push_env w_9 (Memo.from_int 1);
        if_kont_0 ())
      else (
        assert_env_length w_9 1;
        push_env w_9 (Memo.from_int 2);
        if_kont_0 ()))
    10;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 1;
      push_env w_10 (Dynarray.get w_10.state.e 0);
      assert_env_length w_10 2;
      push_env w_10 (Memo.from_int 0);
      w_10.state.c <- pc_to_exp (int_to_pc 13))
    11;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 3;
      let x0_5 = resolve w_12 (Source.E 1) in
      let x1_5 = resolve w_12 (Source.E 2) in
      ignore (pop_env w_12);
      ignore (pop_env w_12);
      push_env w_12 (Memo.from_int (if Word.get_value (fst x0_5) < Word.get_value (fst x1_5) then 1 else 0));
      assert_env_length w_12 2;
      let cond_2 = resolve w_12 (Source.E 1) in
      ignore (pop_env w_12);
      let if_kont_1 =
       fun _ ->
        assert_env_length w_12 2;
        return_n w_12 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_12 1;
        push_env w_12 (Memo.from_int 1);
        if_kont_1 ())
      else (
        assert_env_length w_12 1;
        push_env w_12 (Memo.from_int 2);
        if_kont_1 ()))
    12;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 3;
      let x0_4 = resolve w_11 (Source.E 1) in
      let x1_4 = resolve w_11 (Source.E 2) in
      ignore (pop_env w_11);
      ignore (pop_env w_11);
      push_env w_11 (Memo.from_int (if Word.get_value (fst x0_4) < Word.get_value (fst x1_4) then 1 else 0));
      assert_env_length w_11 2;
      let cond_1 = resolve w_11 (Source.E 1) in
      ignore (pop_env w_11);
      let if_kont_2 =
       fun _ ->
        assert_env_length w_11 2;
        push_env w_11 (Memo.from_int 1);
        w_11.state.c <- pc_to_exp (int_to_pc 12)
      in
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_11 1;
        push_env w_11 (Memo.from_int 1);
        if_kont_2 ())
      else (
        assert_env_length w_11 1;
        push_env w_11 (Memo.from_int 0);
        if_kont_2 ()))
    13;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 1;
      push_env w_13 (Dynarray.get w_13.state.e 0);
      w_13.state.c <- pc_to_exp (int_to_pc 15))
    14;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 2;
      let last_0 = Source.E 1 in
      let x_0 = resolve w_14 last_0 in
      match Word.get_value (fst x_0) with
      | c_0 when c_0 = tag_Nil ->
          ignore (pop_env w_14);
          assert_env_length w_14 1;
          push_env w_14 (Memo.from_int 0);
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | c_0 when c_0 = tag_Cons ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          ignore (pop_env w_14);
          push_env w_14 split0_0;
          push_env w_14 split1_0;
          assert_env_length w_14 3;
          push_env w_14 (Memo.from_int 1);
          assert_env_length w_14 4;
          drop_n w_14 4 2;
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (15)")
    15;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 1;
      push_env w_15 (Dynarray.get w_15.state.e 0);
      w_15.state.c <- pc_to_exp (int_to_pc 18))
    16;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 5;
      let x0_6 = resolve w_17 (Source.E 3) in
      let x1_6 = resolve w_17 (Source.E 4) in
      ignore (pop_env w_17);
      ignore (pop_env w_17);
      push_env w_17 (Memo.from_int (Word.get_value (fst x0_6) + Word.get_value (fst x1_6)));
      assert_env_length w_17 4;
      drop_n w_17 4 2;
      assert_env_length w_17 2;
      return_n w_17 2 (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 2;
      let last_1 = Source.E 1 in
      let x_1 = resolve w_16 last_1 in
      match Word.get_value (fst x_1) with
      | c_1 when c_1 = tag_Pair ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          let split1_1 = List.nth splits_1 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_1;
          push_env w_16 split1_1;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          assert_env_length w_16 4;
          push_env w_16 (Dynarray.get w_16.state.e 2);
          w_16.state.c <- pc_to_exp (int_to_pc 17)
      | _ -> failwith "unreachable (18)")
    18;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 1;
      push_env w_18 (Dynarray.get w_18.state.e 0);
      w_18.state.c <- pc_to_exp (int_to_pc 20))
    19;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 2;
      let last_2 = Source.E 1 in
      let x_2 = resolve w_19 last_2 in
      match Word.get_value (fst x_2) with
      | c_2 when c_2 = tag_Wrap ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_2;
          assert_env_length w_19 2;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          assert_env_length w_19 3;
          drop_n w_19 3 1;
          assert_env_length w_19 2;
          return_n w_19 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (20)")
    20;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 1;
      push_env w_20 (Memo.from_int 0);
      assert_env_length w_20 2;
      let keep_0 = env_call w_20 [] 1 in
      w_20.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_20.state.k ];
      w_20.state.c <- pc_to_exp (int_to_pc 1))
    21;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1);
  Words.set_constructor_degree 3 (-1);
  Words.set_constructor_degree 4 0;
  Words.set_constructor_degree 5 0;
  Words.set_constructor_degree 6 0;
  Words.set_constructor_degree 7 0;
  Words.set_constructor_degree 8 0;
  Words.set_constructor_degree 9 0;
  Words.set_constructor_degree 10 0;
  Words.set_constructor_degree 11 0;
  Words.set_constructor_degree 12 0;
  Words.set_constructor_degree 13 0;
  Words.set_constructor_degree 14 0
