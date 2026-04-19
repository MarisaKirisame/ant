open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_NilP = 3
let tag_ConsP = 4
let tag_cont_1 = 5
let tag_cont_2 = 6
let tag_cont_3 = 7
let tag_cont_4 = 8
let tag_cont_5 = 9
let tag_cont_6 = 10
let tag_cont_7 = 11
let tag_P = 12
let tag_cont_8 = 13
let tag_cont_9 = 14
let tag_cont_10 = 15
let tag_cont_11 = 16
let tag_cont_12 = 17
let tag_cont_13 = 18
let tag_cont_14 = 19
let tag_cont_15 = 20
let tag_cont_16 = 21
let tag_cont_17 = 22
let tag_cont_18 = 23
let tag_cont_19 = 24
let tag_cont_20 = 25
let tag_cont_21 = 26
let tag_cont_22 = 27

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

type int_pair_list = NilP | ConsP of int * int * int_pair_list

let rec from_ocaml_int_pair_list x =
  match x with
  | NilP -> Memo.appends [ Memo.from_constructor tag_NilP ]
  | ConsP (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_ConsP; Memo.from_int x0; Memo.from_int x1; from_ocaml_int_pair_list x2 ]

let rec to_ocaml_int_pair_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 3 (* tag_NilP *) -> NilP
  | 4 (* tag_ConsP *) ->
      let x0, x1, x2 = Memo.splits_3 t in
      ConsP (Word.get_value (Memo.to_word x0), Word.get_value (Memo.to_word x1), to_ocaml_int_pair_list x2)
  | _ -> failwith "unreachable"

let rec pair memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec list_incr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec filter_pos memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 7)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec append memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 11)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec reverse memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 13)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec insert memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 15)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec insertion_sort memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 19)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

type pair_int_lists = P of int_list * int_list

let rec from_ocaml_pair_int_lists x =
  match x with
  | P (x0, x1) -> Memo.appends [ Memo.from_constructor tag_P; from_ocaml_int_list x0; from_ocaml_int_list x1 ]

let rec to_ocaml_pair_int_lists x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 12 (* tag_P *) ->
      let x0, x1 = Memo.splits_2 t in
      P (to_ocaml_int_list x0, to_ocaml_int_list x1)
  | _ -> failwith "unreachable"

let rec my_split_aux memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 21)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec my_split memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 25)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec my_merge memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 26)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec mergesort memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 31)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec filter_gt memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 34)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec filter_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 38)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec filter_lt memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 42)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec quicksort memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 46)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_47 ->
      assert_env_length w_47 1;
      let hd_0, tl_0 = resolve w_47 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_47
      | 5 (* tag_cont_1 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 2 tl_0;
          assert_env_length w_47 3;
          let ctor_arg_10 = pop_env w_47 in
          let ctor_arg_11 = pop_env w_47 in
          let ctor_arg_12 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_ConsP; ctor_arg_12; ctor_arg_11; ctor_arg_10 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_cont_2 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_13 = pop_env w_47 in
          let ctor_arg_14 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_14; ctor_arg_13 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_cont_3 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_15 = pop_env w_47 in
          let ctor_arg_16 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_16; ctor_arg_15 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_cont_4 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_17 = pop_env w_47 in
          let ctor_arg_18 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_18; ctor_arg_17 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_cont_5 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          push_env w_47 (Dynarray.get w_47.state.e 0);
          assert_env_length w_47 3;
          push_env w_47 (Memo.from_constructor tag_Nil);
          assert_env_length w_47 4;
          let ctor_arg_19 = pop_env w_47 in
          let ctor_arg_20 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_20; ctor_arg_19 ]);
          assert_env_length w_47 3;
          ignore (env_call w_47 [] 2);
          w_47.state.c <- pc_to_exp (int_to_pc 11)
      | 10 (* tag_cont_6 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_21 = pop_env w_47 in
          let ctor_arg_22 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_22; ctor_arg_21 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 11 (* tag_cont_7 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          ignore (env_call w_47 [] 2);
          w_47.state.c <- pc_to_exp (int_to_pc 15)
      | 13 (* tag_cont_8 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          w_47.state.c <- pc_to_exp (int_to_pc 48)
      | 14 (* tag_cont_9 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_27 = pop_env w_47 in
          let ctor_arg_28 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_28; ctor_arg_27 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 15 (* tag_cont_10 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_29 = pop_env w_47 in
          let ctor_arg_30 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_30; ctor_arg_29 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 16 (* tag_cont_11 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 0 tl_0;
          w_47.state.c <- pc_to_exp (int_to_pc 49)
      | 17 (* tag_cont_12 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_31 = pop_env w_47 in
          let ctor_arg_32 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_32; ctor_arg_31 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 18 (* tag_cont_13 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_33 = pop_env w_47 in
          let ctor_arg_34 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_34; ctor_arg_33 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 19 (* tag_cont_14 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          let ctor_arg_35 = pop_env w_47 in
          let ctor_arg_36 = pop_env w_47 in
          push_env w_47 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_36; ctor_arg_35 ]);
          assert_env_length w_47 1;
          drop_n w_47 1 0;
          assert_env_length w_47 1;
          return_n w_47 1 (pc_to_exp (int_to_pc 0))
      | 20 (* tag_cont_15 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 2 tl_0;
          assert_env_length w_47 3;
          push_env w_47 (Dynarray.get w_47.state.e 0);
          assert_env_length w_47 4;
          push_env w_47 (Dynarray.get w_47.state.e 1);
          assert_env_length w_47 5;
          let keep_16 = env_call w_47 [ 0; 1; 2 ] 2 in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_16; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 38)
      | 21 (* tag_cont_16 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 2 tl_0;
          assert_env_length w_47 3;
          let keep_17 = env_call w_47 [ 0; 1 ] 1 in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_17; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 46)
      | 22 (* tag_cont_17 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          push_env w_47 (Dynarray.get w_47.state.e 0);
          assert_env_length w_47 3;
          let keep_18 = env_call w_47 [ 1 ] 1 in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_18; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 31)
      | 23 (* tag_cont_18 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          ignore (env_call w_47 [] 2);
          w_47.state.c <- pc_to_exp (int_to_pc 11)
      | 24 (* tag_cont_19 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 3 tl_0;
          assert_env_length w_47 4;
          push_env w_47 (Dynarray.get w_47.state.e 0);
          assert_env_length w_47 5;
          push_env w_47 (Dynarray.get w_47.state.e 1);
          assert_env_length w_47 6;
          let keep_19 = env_call w_47 [ 2; 3 ] 2 in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_19; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 34)
      | 25 (* tag_cont_20 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 1 tl_0;
          assert_env_length w_47 2;
          ignore (env_call w_47 [] 2);
          w_47.state.c <- pc_to_exp (int_to_pc 26)
      | 26 (* tag_cont_21 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 2 tl_0;
          assert_env_length w_47 3;
          let keep_20 = env_call w_47 [ 2 ] 2 in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_20; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 11)
      | 27 (* tag_cont_22 *) ->
          w_47.state.k <- get_next_cont tl_0;
          restore_env w_47 2 tl_0;
          assert_env_length w_47 3;
          let keep_21 = env_call w_47 [ 2; 3 ] 1 in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_21; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 46)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 3))
    1;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 4;
      let last_1 = Source.E 3 in
      let x_1 = resolve w_2 last_1 in
      match Word.get_value (fst x_1) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_2);
          assert_env_length w_2 3;
          push_env w_2 (Memo.from_constructor tag_NilP);
          assert_env_length w_2 4;
          drop_n w_2 4 2;
          assert_env_length w_2 2;
          return_n w_2 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          let split1_1 = List.nth splits_1 1 in
          ignore (pop_env w_2);
          push_env w_2 split0_1;
          push_env w_2 split1_1;
          assert_env_length w_2 5;
          push_env w_2 (Dynarray.get w_2.state.e 1);
          assert_env_length w_2 6;
          push_env w_2 (Dynarray.get w_2.state.e 3);
          assert_env_length w_2 7;
          push_env w_2 (Dynarray.get w_2.state.e 2);
          assert_env_length w_2 8;
          let keep_0 = env_call w_2 [ 5; 6 ] 1 in
          w_2.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_2.state.k ];
          w_2.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      let last_0 = Source.E 1 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_1);
          assert_env_length w_1 1;
          push_env w_1 (Memo.from_constructor tag_NilP);
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          ignore (pop_env w_1);
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 3;
          push_env w_1 (Dynarray.get w_1.state.e 2);
          w_1.state.c <- pc_to_exp (int_to_pc 2)
      | _ -> failwith "unreachable (3)")
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      push_env w_3 (Dynarray.get w_3.state.e 0);
      w_3.state.c <- pc_to_exp (int_to_pc 6))
    4;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 5;
      let x0_0 = resolve w_5 (Source.E 3) in
      let x1_0 = resolve w_5 (Source.E 4) in
      ignore (pop_env w_5);
      ignore (pop_env w_5);
      push_env w_5 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_5 4;
      push_env w_5 (Dynarray.get w_5.state.e 2);
      assert_env_length w_5 5;
      let keep_1 = env_call w_5 [ 3 ] 1 in
      w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_5.state.k ];
      w_5.state.c <- pc_to_exp (int_to_pc 4))
    5;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 2;
      let last_2 = Source.E 1 in
      let x_2 = resolve w_4 last_2 in
      match Word.get_value (fst x_2) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_4);
          assert_env_length w_4 1;
          push_env w_4 (Memo.from_constructor tag_Nil);
          assert_env_length w_4 2;
          return_n w_4 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          let split1_2 = List.nth splits_2 1 in
          ignore (pop_env w_4);
          push_env w_4 split0_2;
          push_env w_4 split1_2;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 4;
          push_env w_4 (Memo.from_int 1);
          w_4.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (6)")
    6;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 1;
      push_env w_6 (Dynarray.get w_6.state.e 0);
      w_6.state.c <- pc_to_exp (int_to_pc 10))
    7;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 4;
      let cond_0 = resolve w_9 (Source.E 3) in
      ignore (pop_env w_9);
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_9 3;
        push_env w_9 (Dynarray.get w_9.state.e 1);
        assert_env_length w_9 4;
        push_env w_9 (Dynarray.get w_9.state.e 2);
        assert_env_length w_9 5;
        let keep_2 = env_call w_9 [ 3 ] 1 in
        w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_9.state.k ];
        w_9.state.c <- pc_to_exp (int_to_pc 7))
      else (
        assert_env_length w_9 3;
        push_env w_9 (Dynarray.get w_9.state.e 2);
        assert_env_length w_9 4;
        ignore (env_call w_9 [] 1);
        w_9.state.c <- pc_to_exp (int_to_pc 7)))
    8;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 5;
      let x0_1 = resolve w_8 (Source.E 3) in
      let x1_1 = resolve w_8 (Source.E 4) in
      ignore (pop_env w_8);
      ignore (pop_env w_8);
      push_env w_8 (Memo.from_int (if Word.get_value (fst x0_1) > Word.get_value (fst x1_1) then 1 else 0));
      w_8.state.c <- pc_to_exp (int_to_pc 8))
    9;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 2;
      let last_3 = Source.E 1 in
      let x_3 = resolve w_7 last_3 in
      match Word.get_value (fst x_3) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_7);
          assert_env_length w_7 1;
          push_env w_7 (Memo.from_constructor tag_Nil);
          assert_env_length w_7 2;
          return_n w_7 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_3 = Memo.splits (snd x_3) in
          let split0_3 = List.nth splits_3 0 in
          let split1_3 = List.nth splits_3 1 in
          ignore (pop_env w_7);
          push_env w_7 split0_3;
          push_env w_7 split1_3;
          assert_env_length w_7 3;
          push_env w_7 (Dynarray.get w_7.state.e 1);
          assert_env_length w_7 4;
          push_env w_7 (Memo.from_int 50);
          w_7.state.c <- pc_to_exp (int_to_pc 9)
      | _ -> failwith "unreachable (10)")
    10;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 2;
      push_env w_10 (Dynarray.get w_10.state.e 0);
      w_10.state.c <- pc_to_exp (int_to_pc 12))
    11;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 3;
      let last_4 = Source.E 2 in
      let x_4 = resolve w_11 last_4 in
      match Word.get_value (fst x_4) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_11);
          assert_env_length w_11 2;
          push_env w_11 (Dynarray.get w_11.state.e 1);
          assert_env_length w_11 3;
          return_n w_11 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_4 = Memo.splits (snd x_4) in
          let split0_4 = List.nth splits_4 0 in
          let split1_4 = List.nth splits_4 1 in
          ignore (pop_env w_11);
          push_env w_11 split0_4;
          push_env w_11 split1_4;
          assert_env_length w_11 4;
          push_env w_11 (Dynarray.get w_11.state.e 2);
          assert_env_length w_11 5;
          push_env w_11 (Dynarray.get w_11.state.e 3);
          assert_env_length w_11 6;
          push_env w_11 (Dynarray.get w_11.state.e 1);
          assert_env_length w_11 7;
          let keep_3 = env_call w_11 [ 4 ] 2 in
          w_11.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_11.state.k ];
          w_11.state.c <- pc_to_exp (int_to_pc 11)
      | _ -> failwith "unreachable (12)")
    12;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 1;
      push_env w_12 (Dynarray.get w_12.state.e 0);
      w_12.state.c <- pc_to_exp (int_to_pc 14))
    13;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 2;
      let last_5 = Source.E 1 in
      let x_5 = resolve w_13 last_5 in
      match Word.get_value (fst x_5) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_13);
          assert_env_length w_13 1;
          push_env w_13 (Memo.from_constructor tag_Nil);
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_5 = Memo.splits (snd x_5) in
          let split0_5 = List.nth splits_5 0 in
          let split1_5 = List.nth splits_5 1 in
          ignore (pop_env w_13);
          push_env w_13 split0_5;
          push_env w_13 split1_5;
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 2);
          assert_env_length w_13 4;
          let keep_4 = env_call w_13 [ 1 ] 1 in
          w_13.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_13.state.k ];
          w_13.state.c <- pc_to_exp (int_to_pc 13)
      | _ -> failwith "unreachable (14)")
    14;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 2;
      push_env w_14 (Dynarray.get w_14.state.e 1);
      w_14.state.c <- pc_to_exp (int_to_pc 18))
    15;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 5;
      let cond_1 = resolve w_17 (Source.E 4) in
      ignore (pop_env w_17);
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_17 4;
        push_env w_17 (Dynarray.get w_17.state.e 0);
        assert_env_length w_17 5;
        push_env w_17 (Dynarray.get w_17.state.e 1);
        assert_env_length w_17 6;
        let ctor_arg_2 = pop_env w_17 in
        let ctor_arg_3 = pop_env w_17 in
        push_env w_17 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_3; ctor_arg_2 ]);
        assert_env_length w_17 5;
        drop_n w_17 5 2;
        assert_env_length w_17 3;
        return_n w_17 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_17 4;
        push_env w_17 (Dynarray.get w_17.state.e 2);
        assert_env_length w_17 5;
        push_env w_17 (Dynarray.get w_17.state.e 0);
        assert_env_length w_17 6;
        push_env w_17 (Dynarray.get w_17.state.e 3);
        assert_env_length w_17 7;
        let keep_5 = env_call w_17 [ 4 ] 2 in
        w_17.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_17.state.k ];
        w_17.state.c <- pc_to_exp (int_to_pc 15)))
    16;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 6;
      let x0_2 = resolve w_16 (Source.E 4) in
      let x1_2 = resolve w_16 (Source.E 5) in
      ignore (pop_env w_16);
      ignore (pop_env w_16);
      push_env w_16 (Memo.from_int (if Word.get_value (fst x0_2) <= Word.get_value (fst x1_2) then 1 else 0));
      w_16.state.c <- pc_to_exp (int_to_pc 16))
    17;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 3;
      let last_6 = Source.E 2 in
      let x_6 = resolve w_15 last_6 in
      match Word.get_value (fst x_6) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_15);
          assert_env_length w_15 2;
          push_env w_15 (Dynarray.get w_15.state.e 0);
          assert_env_length w_15 3;
          push_env w_15 (Memo.from_constructor tag_Nil);
          assert_env_length w_15 4;
          let ctor_arg_0 = pop_env w_15 in
          let ctor_arg_1 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_1; ctor_arg_0 ]);
          assert_env_length w_15 3;
          return_n w_15 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_6 = Memo.splits (snd x_6) in
          let split0_6 = List.nth splits_6 0 in
          let split1_6 = List.nth splits_6 1 in
          ignore (pop_env w_15);
          push_env w_15 split0_6;
          push_env w_15 split1_6;
          assert_env_length w_15 4;
          push_env w_15 (Dynarray.get w_15.state.e 0);
          assert_env_length w_15 5;
          push_env w_15 (Dynarray.get w_15.state.e 2);
          w_15.state.c <- pc_to_exp (int_to_pc 17)
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
      let last_7 = Source.E 1 in
      let x_7 = resolve w_19 last_7 in
      match Word.get_value (fst x_7) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_19);
          assert_env_length w_19 1;
          push_env w_19 (Memo.from_constructor tag_Nil);
          assert_env_length w_19 2;
          return_n w_19 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_7 = Memo.splits (snd x_7) in
          let split0_7 = List.nth splits_7 0 in
          let split1_7 = List.nth splits_7 1 in
          ignore (pop_env w_19);
          push_env w_19 split0_7;
          push_env w_19 split1_7;
          assert_env_length w_19 3;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 2);
          assert_env_length w_19 5;
          let keep_6 = env_call w_19 [ 3 ] 1 in
          w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_19.state.k ];
          w_19.state.c <- pc_to_exp (int_to_pc 19)
      | _ -> failwith "unreachable (20)")
    20;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 2;
      push_env w_20 (Dynarray.get w_20.state.e 1);
      w_20.state.c <- pc_to_exp (int_to_pc 24))
    21;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 7;
      let last_10 = Source.E 6 in
      let x_10 = resolve w_23 last_10 in
      match Word.get_value (fst x_10) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_23);
          assert_env_length w_23 6;
          push_env w_23 (Memo.from_constructor tag_Nil);
          assert_env_length w_23 7;
          push_env w_23 (Memo.from_constructor tag_Nil);
          assert_env_length w_23 8;
          let ctor_arg_8 = pop_env w_23 in
          let ctor_arg_9 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_P; ctor_arg_9; ctor_arg_8 ]);
          assert_env_length w_23 7;
          drop_n w_23 7 2;
          assert_env_length w_23 5;
          drop_n w_23 5 2;
          assert_env_length w_23 3;
          return_n w_23 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_10 = Memo.splits (snd x_10) in
          let split0_10 = List.nth splits_10 0 in
          let split1_10 = List.nth splits_10 1 in
          ignore (pop_env w_23);
          push_env w_23 split0_10;
          push_env w_23 split1_10;
          assert_env_length w_23 8;
          push_env w_23 (Dynarray.get w_23.state.e 7);
          assert_env_length w_23 9;
          push_env w_23 (Dynarray.get w_23.state.e 5);
          assert_env_length w_23 10;
          let keep_7 = env_call w_23 [ 6 ] 2 in
          w_23.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_23.state.k ];
          w_23.state.c <- pc_to_exp (int_to_pc 21)
      | _ -> failwith "unreachable (22)")
    22;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 5;
      let last_9 = Source.E 4 in
      let x_9 = resolve w_22 last_9 in
      match Word.get_value (fst x_9) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_22);
          assert_env_length w_22 4;
          push_env w_22 (Memo.from_constructor tag_Nil);
          assert_env_length w_22 5;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 6;
          let ctor_arg_6 = pop_env w_22 in
          let ctor_arg_7 = pop_env w_22 in
          push_env w_22 (Memo.appends [ Memo.from_constructor tag_P; ctor_arg_7; ctor_arg_6 ]);
          assert_env_length w_22 5;
          drop_n w_22 5 2;
          assert_env_length w_22 3;
          return_n w_22 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_9 = Memo.splits (snd x_9) in
          let split0_9 = List.nth splits_9 0 in
          let split1_9 = List.nth splits_9 1 in
          ignore (pop_env w_22);
          push_env w_22 split0_9;
          push_env w_22 split1_9;
          assert_env_length w_22 6;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          w_22.state.c <- pc_to_exp (int_to_pc 22)
      | _ -> failwith "unreachable (23)")
    23;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 3;
      let last_8 = Source.E 2 in
      let x_8 = resolve w_21 last_8 in
      match Word.get_value (fst x_8) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_21);
          assert_env_length w_21 2;
          push_env w_21 (Memo.from_constructor tag_Nil);
          assert_env_length w_21 3;
          push_env w_21 (Dynarray.get w_21.state.e 0);
          assert_env_length w_21 4;
          let ctor_arg_4 = pop_env w_21 in
          let ctor_arg_5 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_P; ctor_arg_5; ctor_arg_4 ]);
          assert_env_length w_21 3;
          return_n w_21 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_8 = Memo.splits (snd x_8) in
          let split0_8 = List.nth splits_8 0 in
          let split1_8 = List.nth splits_8 1 in
          ignore (pop_env w_21);
          push_env w_21 split0_8;
          push_env w_21 split1_8;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 3);
          w_21.state.c <- pc_to_exp (int_to_pc 23)
      | _ -> failwith "unreachable (24)")
    24;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 1;
      push_env w_24 (Dynarray.get w_24.state.e 0);
      assert_env_length w_24 2;
      push_env w_24 (Dynarray.get w_24.state.e 0);
      assert_env_length w_24 3;
      ignore (env_call w_24 [] 2);
      w_24.state.c <- pc_to_exp (int_to_pc 21))
    25;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 2;
      push_env w_25 (Dynarray.get w_25.state.e 0);
      w_25.state.c <- pc_to_exp (int_to_pc 30))
    26;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 7;
      let cond_2 = resolve w_29 (Source.E 6) in
      ignore (pop_env w_29);
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_29 6;
        push_env w_29 (Dynarray.get w_29.state.e 2);
        assert_env_length w_29 7;
        push_env w_29 (Dynarray.get w_29.state.e 3);
        assert_env_length w_29 8;
        push_env w_29 (Dynarray.get w_29.state.e 1);
        assert_env_length w_29 9;
        let keep_9 = env_call w_29 [ 6 ] 2 in
        w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_29.state.k ];
        w_29.state.c <- pc_to_exp (int_to_pc 26))
      else (
        assert_env_length w_29 6;
        push_env w_29 (Dynarray.get w_29.state.e 4);
        assert_env_length w_29 7;
        push_env w_29 (Dynarray.get w_29.state.e 0);
        assert_env_length w_29 8;
        push_env w_29 (Dynarray.get w_29.state.e 5);
        assert_env_length w_29 9;
        let keep_8 = env_call w_29 [ 6 ] 2 in
        w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_29.state.k ];
        w_29.state.c <- pc_to_exp (int_to_pc 26)))
    27;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 8;
      let x0_3 = resolve w_28 (Source.E 6) in
      let x1_3 = resolve w_28 (Source.E 7) in
      ignore (pop_env w_28);
      ignore (pop_env w_28);
      push_env w_28 (Memo.from_int (if Word.get_value (fst x0_3) < Word.get_value (fst x1_3) then 1 else 0));
      w_28.state.c <- pc_to_exp (int_to_pc 27))
    28;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 5;
      let last_12 = Source.E 4 in
      let x_12 = resolve w_27 last_12 in
      match Word.get_value (fst x_12) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_27);
          assert_env_length w_27 4;
          push_env w_27 (Dynarray.get w_27.state.e 0);
          assert_env_length w_27 5;
          drop_n w_27 5 2;
          assert_env_length w_27 3;
          return_n w_27 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_12 = Memo.splits (snd x_12) in
          let split0_12 = List.nth splits_12 0 in
          let split1_12 = List.nth splits_12 1 in
          ignore (pop_env w_27);
          push_env w_27 split0_12;
          push_env w_27 split1_12;
          assert_env_length w_27 6;
          push_env w_27 (Dynarray.get w_27.state.e 2);
          assert_env_length w_27 7;
          push_env w_27 (Dynarray.get w_27.state.e 4);
          w_27.state.c <- pc_to_exp (int_to_pc 28)
      | _ -> failwith "unreachable (29)")
    29;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 3;
      let last_11 = Source.E 2 in
      let x_11 = resolve w_26 last_11 in
      match Word.get_value (fst x_11) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_26);
          assert_env_length w_26 2;
          push_env w_26 (Dynarray.get w_26.state.e 1);
          assert_env_length w_26 3;
          return_n w_26 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_11 = Memo.splits (snd x_11) in
          let split0_11 = List.nth splits_11 0 in
          let split1_11 = List.nth splits_11 1 in
          ignore (pop_env w_26);
          push_env w_26 split0_11;
          push_env w_26 split1_11;
          assert_env_length w_26 4;
          push_env w_26 (Dynarray.get w_26.state.e 1);
          w_26.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (30)")
    30;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 1;
      push_env w_30 (Dynarray.get w_30.state.e 0);
      w_30.state.c <- pc_to_exp (int_to_pc 33))
    31;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 4;
      let last_14 = Source.E 3 in
      let x_14 = resolve w_32 last_14 in
      match Word.get_value (fst x_14) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_32);
          assert_env_length w_32 3;
          push_env w_32 (Dynarray.get w_32.state.e 0);
          assert_env_length w_32 4;
          drop_n w_32 4 2;
          assert_env_length w_32 2;
          return_n w_32 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_14 = Memo.splits (snd x_14) in
          let split0_14 = List.nth splits_14 0 in
          let split1_14 = List.nth splits_14 1 in
          ignore (pop_env w_32);
          push_env w_32 split0_14;
          push_env w_32 split1_14;
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 0);
          assert_env_length w_32 6;
          let keep_10 = env_call w_32 [] 1 in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 25)
      | _ -> failwith "unreachable (32)")
    32;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 2;
      let last_13 = Source.E 1 in
      let x_13 = resolve w_31 last_13 in
      match Word.get_value (fst x_13) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_31);
          assert_env_length w_31 1;
          push_env w_31 (Memo.from_constructor tag_Nil);
          assert_env_length w_31 2;
          return_n w_31 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_13 = Memo.splits (snd x_13) in
          let split0_13 = List.nth splits_13 0 in
          let split1_13 = List.nth splits_13 1 in
          ignore (pop_env w_31);
          push_env w_31 split0_13;
          push_env w_31 split1_13;
          assert_env_length w_31 3;
          push_env w_31 (Dynarray.get w_31.state.e 2);
          w_31.state.c <- pc_to_exp (int_to_pc 32)
      | _ -> failwith "unreachable (33)")
    33;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 2;
      push_env w_33 (Dynarray.get w_33.state.e 0);
      w_33.state.c <- pc_to_exp (int_to_pc 37))
    34;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 5;
      let cond_3 = resolve w_36 (Source.E 4) in
      ignore (pop_env w_36);
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_36 4;
        push_env w_36 (Dynarray.get w_36.state.e 2);
        assert_env_length w_36 5;
        push_env w_36 (Dynarray.get w_36.state.e 3);
        assert_env_length w_36 6;
        push_env w_36 (Dynarray.get w_36.state.e 1);
        assert_env_length w_36 7;
        let keep_11 = env_call w_36 [ 4 ] 2 in
        w_36.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_36.state.k ];
        w_36.state.c <- pc_to_exp (int_to_pc 34))
      else (
        assert_env_length w_36 4;
        push_env w_36 (Dynarray.get w_36.state.e 3);
        assert_env_length w_36 5;
        push_env w_36 (Dynarray.get w_36.state.e 1);
        assert_env_length w_36 6;
        ignore (env_call w_36 [] 2);
        w_36.state.c <- pc_to_exp (int_to_pc 34)))
    35;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 6;
      let x0_4 = resolve w_35 (Source.E 4) in
      let x1_4 = resolve w_35 (Source.E 5) in
      ignore (pop_env w_35);
      ignore (pop_env w_35);
      push_env w_35 (Memo.from_int (if Word.get_value (fst x0_4) > Word.get_value (fst x1_4) then 1 else 0));
      w_35.state.c <- pc_to_exp (int_to_pc 35))
    36;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 3;
      let last_15 = Source.E 2 in
      let x_15 = resolve w_34 last_15 in
      match Word.get_value (fst x_15) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_34);
          assert_env_length w_34 2;
          push_env w_34 (Memo.from_constructor tag_Nil);
          assert_env_length w_34 3;
          return_n w_34 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_15 = Memo.splits (snd x_15) in
          let split0_15 = List.nth splits_15 0 in
          let split1_15 = List.nth splits_15 1 in
          ignore (pop_env w_34);
          push_env w_34 split0_15;
          push_env w_34 split1_15;
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 5;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          w_34.state.c <- pc_to_exp (int_to_pc 36)
      | _ -> failwith "unreachable (37)")
    37;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 2;
      push_env w_37 (Dynarray.get w_37.state.e 0);
      w_37.state.c <- pc_to_exp (int_to_pc 41))
    38;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 5;
      let cond_4 = resolve w_40 (Source.E 4) in
      ignore (pop_env w_40);
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_40 4;
        push_env w_40 (Dynarray.get w_40.state.e 2);
        assert_env_length w_40 5;
        push_env w_40 (Dynarray.get w_40.state.e 3);
        assert_env_length w_40 6;
        push_env w_40 (Dynarray.get w_40.state.e 1);
        assert_env_length w_40 7;
        let keep_12 = env_call w_40 [ 4 ] 2 in
        w_40.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_40.state.k ];
        w_40.state.c <- pc_to_exp (int_to_pc 38))
      else (
        assert_env_length w_40 4;
        push_env w_40 (Dynarray.get w_40.state.e 3);
        assert_env_length w_40 5;
        push_env w_40 (Dynarray.get w_40.state.e 1);
        assert_env_length w_40 6;
        ignore (env_call w_40 [] 2);
        w_40.state.c <- pc_to_exp (int_to_pc 38)))
    39;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 6;
      let x0_5 = resolve w_39 (Source.E 4) in
      let x1_5 = resolve w_39 (Source.E 5) in
      ignore (pop_env w_39);
      ignore (pop_env w_39);
      push_env w_39 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      w_39.state.c <- pc_to_exp (int_to_pc 39))
    40;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 3;
      let last_16 = Source.E 2 in
      let x_16 = resolve w_38 last_16 in
      match Word.get_value (fst x_16) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_38);
          assert_env_length w_38 2;
          push_env w_38 (Memo.from_constructor tag_Nil);
          assert_env_length w_38 3;
          return_n w_38 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_16 = Memo.splits (snd x_16) in
          let split0_16 = List.nth splits_16 0 in
          let split1_16 = List.nth splits_16 1 in
          ignore (pop_env w_38);
          push_env w_38 split0_16;
          push_env w_38 split1_16;
          assert_env_length w_38 4;
          push_env w_38 (Dynarray.get w_38.state.e 2);
          assert_env_length w_38 5;
          push_env w_38 (Dynarray.get w_38.state.e 1);
          w_38.state.c <- pc_to_exp (int_to_pc 40)
      | _ -> failwith "unreachable (41)")
    41;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 2;
      push_env w_41 (Dynarray.get w_41.state.e 0);
      w_41.state.c <- pc_to_exp (int_to_pc 45))
    42;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 5;
      let cond_5 = resolve w_44 (Source.E 4) in
      ignore (pop_env w_44);
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_44 4;
        push_env w_44 (Dynarray.get w_44.state.e 2);
        assert_env_length w_44 5;
        push_env w_44 (Dynarray.get w_44.state.e 3);
        assert_env_length w_44 6;
        push_env w_44 (Dynarray.get w_44.state.e 1);
        assert_env_length w_44 7;
        let keep_13 = env_call w_44 [ 4 ] 2 in
        w_44.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_44.state.k ];
        w_44.state.c <- pc_to_exp (int_to_pc 42))
      else (
        assert_env_length w_44 4;
        push_env w_44 (Dynarray.get w_44.state.e 3);
        assert_env_length w_44 5;
        push_env w_44 (Dynarray.get w_44.state.e 1);
        assert_env_length w_44 6;
        ignore (env_call w_44 [] 2);
        w_44.state.c <- pc_to_exp (int_to_pc 42)))
    43;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 6;
      let x0_6 = resolve w_43 (Source.E 4) in
      let x1_6 = resolve w_43 (Source.E 5) in
      ignore (pop_env w_43);
      ignore (pop_env w_43);
      push_env w_43 (Memo.from_int (if Word.get_value (fst x0_6) < Word.get_value (fst x1_6) then 1 else 0));
      w_43.state.c <- pc_to_exp (int_to_pc 43))
    44;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 3;
      let last_17 = Source.E 2 in
      let x_17 = resolve w_42 last_17 in
      match Word.get_value (fst x_17) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_42);
          assert_env_length w_42 2;
          push_env w_42 (Memo.from_constructor tag_Nil);
          assert_env_length w_42 3;
          return_n w_42 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_17 = Memo.splits (snd x_17) in
          let split0_17 = List.nth splits_17 0 in
          let split1_17 = List.nth splits_17 1 in
          ignore (pop_env w_42);
          push_env w_42 split0_17;
          push_env w_42 split1_17;
          assert_env_length w_42 4;
          push_env w_42 (Dynarray.get w_42.state.e 2);
          assert_env_length w_42 5;
          push_env w_42 (Dynarray.get w_42.state.e 1);
          w_42.state.c <- pc_to_exp (int_to_pc 44)
      | _ -> failwith "unreachable (45)")
    45;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 1;
      push_env w_45 (Dynarray.get w_45.state.e 0);
      w_45.state.c <- pc_to_exp (int_to_pc 47))
    46;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 2;
      let last_18 = Source.E 1 in
      let x_18 = resolve w_46 last_18 in
      match Word.get_value (fst x_18) with
      | 1 (* tag_Nil *) ->
          ignore (pop_env w_46);
          assert_env_length w_46 1;
          push_env w_46 (Memo.from_constructor tag_Nil);
          assert_env_length w_46 2;
          return_n w_46 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_18 = Memo.splits (snd x_18) in
          let split0_18 = List.nth splits_18 0 in
          let split1_18 = List.nth splits_18 1 in
          ignore (pop_env w_46);
          push_env w_46 split0_18;
          push_env w_46 split1_18;
          assert_env_length w_46 3;
          push_env w_46 (Dynarray.get w_46.state.e 0);
          assert_env_length w_46 4;
          push_env w_46 (Dynarray.get w_46.state.e 1);
          assert_env_length w_46 5;
          let keep_14 = env_call w_46 [ 0; 1 ] 2 in
          w_46.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_14; w_46.state.k ];
          w_46.state.c <- pc_to_exp (int_to_pc 42)
      | _ -> failwith "unreachable (47)")
    47;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 2;
      let last_19 = Source.E 1 in
      let x_19 = resolve w_48 last_19 in
      match Word.get_value (fst x_19) with
      | 12 (* tag_P *) ->
          let splits_19 = Memo.splits (snd x_19) in
          let split0_19 = List.nth splits_19 0 in
          let split1_19 = List.nth splits_19 1 in
          ignore (pop_env w_48);
          push_env w_48 split0_19;
          push_env w_48 split1_19;
          assert_env_length w_48 3;
          push_env w_48 (Dynarray.get w_48.state.e 0);
          assert_env_length w_48 4;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          assert_env_length w_48 5;
          let ctor_arg_23 = pop_env w_48 in
          let ctor_arg_24 = pop_env w_48 in
          push_env w_48 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_24; ctor_arg_23 ]);
          assert_env_length w_48 4;
          push_env w_48 (Dynarray.get w_48.state.e 2);
          assert_env_length w_48 5;
          let ctor_arg_25 = pop_env w_48 in
          let ctor_arg_26 = pop_env w_48 in
          push_env w_48 (Memo.appends [ Memo.from_constructor tag_P; ctor_arg_26; ctor_arg_25 ]);
          assert_env_length w_48 4;
          drop_n w_48 4 2;
          assert_env_length w_48 2;
          drop_n w_48 2 1;
          assert_env_length w_48 1;
          drop_n w_48 1 0;
          assert_env_length w_48 1;
          drop_n w_48 1 0;
          assert_env_length w_48 1;
          return_n w_48 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (48)")
    48;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 1;
      let last_20 = Source.E 0 in
      let x_20 = resolve w_49 last_20 in
      match Word.get_value (fst x_20) with
      | 12 (* tag_P *) ->
          let splits_20 = Memo.splits (snd x_20) in
          let split0_20 = List.nth splits_20 0 in
          let split1_20 = List.nth splits_20 1 in
          ignore (pop_env w_49);
          push_env w_49 split0_20;
          push_env w_49 split1_20;
          assert_env_length w_49 2;
          push_env w_49 (Dynarray.get w_49.state.e 0);
          assert_env_length w_49 3;
          let keep_15 = env_call w_49 [ 1 ] 1 in
          w_49.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_15; w_49.state.k ];
          w_49.state.c <- pc_to_exp (int_to_pc 31)
      | _ -> failwith "unreachable (49)")
    49;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1);
  Words.set_constructor_degree 3 1;
  Words.set_constructor_degree 4 (-2);
  Words.set_constructor_degree 5 (-2);
  Words.set_constructor_degree 6 (-1);
  Words.set_constructor_degree 7 (-1);
  Words.set_constructor_degree 8 (-1);
  Words.set_constructor_degree 9 (-1);
  Words.set_constructor_degree 10 (-1);
  Words.set_constructor_degree 11 (-1);
  Words.set_constructor_degree 12 (-1);
  Words.set_constructor_degree 13 (-1);
  Words.set_constructor_degree 14 (-1);
  Words.set_constructor_degree 15 (-1);
  Words.set_constructor_degree 16 0;
  Words.set_constructor_degree 17 (-1);
  Words.set_constructor_degree 18 (-1);
  Words.set_constructor_degree 19 (-1);
  Words.set_constructor_degree 20 (-2);
  Words.set_constructor_degree 21 (-2);
  Words.set_constructor_degree 22 (-1);
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 (-3);
  Words.set_constructor_degree 25 (-1);
  Words.set_constructor_degree 26 (-2);
  Words.set_constructor_degree 27 (-2)
