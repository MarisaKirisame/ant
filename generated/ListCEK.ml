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
let tag_cont_8 = 12
let tag_P = 13
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
let tag_cont_23 = 28
let tag_cont_24 = 29
let tag_cont_25 = 30
let tag_cont_26 = 31
let tag_cont_27 = 32
let tag_cont_28 = 33
let tag_cont_29 = 34
let tag_cont_30 = 35
let tag_cont_31 = 36

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

let rec pair ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec list_incr ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec filter_pos ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec append ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 8)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec reverse ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 9)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec insert ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 10)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec insertion_sort ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 13)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

type pair_int_lists = P of int_list * int_list

let rec from_ocaml_pair_int_lists x =
  match x with
  | P (x0, x1) -> Memo.appends [ Memo.from_constructor tag_P; from_ocaml_int_list x0; from_ocaml_int_list x1 ]

let rec to_ocaml_pair_int_lists x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 13 (* tag_P *) ->
      let x0, x1 = Memo.splits_2 t in
      P (to_ocaml_int_list x0, to_ocaml_int_list x1)
  | _ -> failwith "unreachable"

let rec my_split_aux ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 14)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec my_split ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 17)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec my_merge ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 18)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec mergesort ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 22)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec filter_gt ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 24)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec filter_eq ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 27)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec filter_lt ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 30)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec quicksort ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 33)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_33 ->
      assert_env_length w_33 1;
      let hd_0, tl_0 = resolve w_33 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_33
      | 5 (* tag_cont_1 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 2 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends
               [
                 Memo.from_constructor tag_ConsP;
                 Dynarray.get w_33.state.e 0;
                 Dynarray.get w_33.state.e 1;
                 Dynarray.get w_33.state.e 2;
               ]);
          assert_env_length w_33 3;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_cont_2 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_cont_3 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_cont_4 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_cont_5 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 10 (* tag_cont_6 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          push_env w_33 (Memo.from_constructor tag_Nil);
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 2 ]);
          assert_env_length w_33 3;
          let keep_20 = env_call w_33 [] [ 1; 0 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_20; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 8)
      | 11 (* tag_cont_7 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_cont_8 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          assert_env_length w_33 2;
          let keep_21 = env_call w_33 [] [ 0; 1 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_21; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 10)
      | 14 (* tag_cont_9 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          assert_env_length w_33 2;
          w_33.state.c <- pc_to_exp (int_to_pc 34)
      | 15 (* tag_cont_10 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 16 (* tag_cont_11 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 17 (* tag_cont_12 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 18 (* tag_cont_13 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          w_33.state.c <- pc_to_exp (int_to_pc 35)
      | 19 (* tag_cont_14 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 20 (* tag_cont_15 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 21 (* tag_cont_16 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 22 (* tag_cont_17 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 23 (* tag_cont_18 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          Dynarray.set w_33.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
          assert_env_length w_33 2;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 24 (* tag_cont_19 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 25 (* tag_cont_20 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 2 tl_0;
          assert_env_length w_33 3;
          let keep_23 = env_call w_33 [ 0; 1 ] [ 2 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_23; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 33)
      | 26 (* tag_cont_21 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 27 (* tag_cont_22 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 28 (* tag_cont_23 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          assert_env_length w_33 2;
          let keep_24 = env_call w_33 [ 1 ] [ 0 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_24; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 22)
      | 29 (* tag_cont_24 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 2 tl_0;
          assert_env_length w_33 3;
          let keep_25 = env_call w_33 [ 0; 1; 2 ] [ 0; 1 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_25; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 27)
      | 30 (* tag_cont_25 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          assert_env_length w_33 2;
          let keep_26 = env_call w_33 [] [ 0; 1 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_26; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 18)
      | 31 (* tag_cont_26 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 3 tl_0;
          assert_env_length w_33 4;
          let keep_27 = env_call w_33 [ 2; 3 ] [ 0; 1 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_27; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 24)
      | 32 (* tag_cont_27 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | 33 (* tag_cont_28 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 2 tl_0;
          assert_env_length w_33 3;
          let keep_28 = env_call w_33 [ 0; 1 ] [ 2 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_28; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 33)
      | 34 (* tag_cont_29 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 2 tl_0;
          assert_env_length w_33 3;
          let keep_29 = env_call w_33 [ 0 ] [ 1; 2 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_29; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 8)
      | 35 (* tag_cont_30 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 1 tl_0;
          assert_env_length w_33 2;
          let keep_30 = env_call w_33 [] [ 0; 1 ] in
          w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_30; w_33.state.k ];
          w_33.state.c <- pc_to_exp (int_to_pc 8)
      | 36 (* tag_cont_31 *) ->
          w_33.state.k <- get_next_cont tl_0;
          restore_env w_33 0 tl_0;
          assert_env_length w_33 1;
          return_n w_33 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      let x_0 = resolve w_0 (Source.E 0) in
      match Word.get_value (fst x_0) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_0.state.e 0 (Memo.from_constructor tag_NilP);
          assert_env_length w_0 1;
          return_n w_0 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          Dynarray.set w_0.state.e 0 split0_0;
          push_env w_0 split1_0;
          assert_env_length w_0 2;
          w_0.state.c <- pc_to_exp (int_to_pc 2)
      | _ -> failwith "unreachable (3)")
    1;
  add_exp
    (fun w_1 ->
      let x_1 = resolve w_1 (Source.E 1) in
      match Word.get_value (fst x_1) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_1.state.e 0 (Memo.from_constructor tag_NilP);
          assert_env_length w_1 2;
          return_n w_1 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          let split1_1 = List.nth splits_1 1 in
          push_env w_1 split0_1;
          push_env w_1 split1_1;
          assert_env_length w_1 4;
          let keep_0 = env_call w_1 [ 0; 2 ] [ 1 ] in
          w_1.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_1.state.k ];
          w_1.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 1;
      let x_2 = resolve w_2 (Source.E 0) in
      match Word.get_value (fst x_2) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_2.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_2 1;
          return_n w_2 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          let split1_2 = List.nth splits_2 1 in
          Dynarray.set w_2.state.e 0 split0_2;
          push_env w_2 split1_2;
          push_env w_2 (Memo.from_int 1);
          w_2.state.c <- pc_to_exp (int_to_pc 4)
      | _ -> failwith "unreachable (5)")
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 3;
      let x0_0 = resolve w_3 (Source.E 0) in
      let x1_0 = resolve w_3 (Source.E 2) in
      Dynarray.set w_3.state.e 0 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_3 3;
      let keep_1 = env_call w_3 [ 0 ] [ 1 ] in
      w_3.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_3.state.k ];
      w_3.state.c <- pc_to_exp (int_to_pc 3))
    4;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 1;
      let x_3 = resolve w_4 (Source.E 0) in
      match Word.get_value (fst x_3) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_4.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_4 1;
          return_n w_4 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_3 = Memo.splits (snd x_3) in
          let split0_3 = List.nth splits_3 0 in
          let split1_3 = List.nth splits_3 1 in
          Dynarray.set w_4.state.e 0 split0_3;
          push_env w_4 split1_3;
          push_env w_4 (Memo.from_int 50);
          w_4.state.c <- pc_to_exp (int_to_pc 7)
      | _ -> failwith "unreachable (8)")
    5;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 3;
      let cond_0 = resolve w_6 (Source.E 2) in
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_6 3;
        let keep_2 = env_call w_6 [ 0 ] [ 1 ] in
        w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_6.state.k ];
        w_6.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_6 3;
        let keep_3 = env_call w_6 [] [ 1 ] in
        w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_6.state.k ];
        w_6.state.c <- pc_to_exp (int_to_pc 5)))
    6;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 3;
      let x0_1 = resolve w_5 (Source.E 0) in
      let x1_1 = resolve w_5 (Source.E 2) in
      Dynarray.set w_5.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_1) > Word.get_value (fst x1_1) then 1 else 0));
      w_5.state.c <- pc_to_exp (int_to_pc 6))
    7;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 2;
      let x_4 = resolve w_7 (Source.E 0) in
      match Word.get_value (fst x_4) with
      | 1 (* tag_Nil *) ->
          assert_env_length w_7 2;
          return_n w_7 1 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_4 = Memo.splits (snd x_4) in
          let split0_4 = List.nth splits_4 0 in
          let split1_4 = List.nth splits_4 1 in
          Dynarray.set w_7.state.e 0 split0_4;
          push_env w_7 split1_4;
          assert_env_length w_7 3;
          let keep_4 = env_call w_7 [ 0 ] [ 2; 1 ] in
          w_7.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_7.state.k ];
          w_7.state.c <- pc_to_exp (int_to_pc 8)
      | _ -> failwith "unreachable (9)")
    8;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 1;
      let x_5 = resolve w_8 (Source.E 0) in
      match Word.get_value (fst x_5) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_8.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_8 1;
          return_n w_8 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_5 = Memo.splits (snd x_5) in
          let split0_5 = List.nth splits_5 0 in
          let split1_5 = List.nth splits_5 1 in
          Dynarray.set w_8.state.e 0 split0_5;
          push_env w_8 split1_5;
          assert_env_length w_8 2;
          let keep_5 = env_call w_8 [ 0 ] [ 1 ] in
          w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_8.state.k ];
          w_8.state.c <- pc_to_exp (int_to_pc 9)
      | _ -> failwith "unreachable (10)")
    9;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 2;
      let x_6 = resolve w_9 (Source.E 1) in
      match Word.get_value (fst x_6) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_9.state.e 1 (Memo.from_constructor tag_Nil);
          Dynarray.set w_9.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_9.state.e 0; Dynarray.get w_9.state.e 1 ]);
          assert_env_length w_9 2;
          return_n w_9 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_6 = Memo.splits (snd x_6) in
          let split0_6 = List.nth splits_6 0 in
          let split1_6 = List.nth splits_6 1 in
          push_env w_9 split0_6;
          push_env w_9 split1_6;
          w_9.state.c <- pc_to_exp (int_to_pc 12)
      | _ -> failwith "unreachable (13)")
    10;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 5;
      let cond_1 = resolve w_11 (Source.E 4) in
      if Word.get_value (fst cond_1) <> 0 then (
        Dynarray.set w_11.state.e 0
          (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_11.state.e 0; Dynarray.get w_11.state.e 1 ]);
        assert_env_length w_11 5;
        return_n w_11 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_11 5;
        let keep_6 = env_call w_11 [ 2 ] [ 0; 3 ] in
        w_11.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_11.state.k ];
        w_11.state.c <- pc_to_exp (int_to_pc 10)))
    11;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      let x0_2 = resolve w_10 (Source.E 0) in
      let x1_2 = resolve w_10 (Source.E 2) in
      push_env w_10 (Memo.from_int (if Word.get_value (fst x0_2) <= Word.get_value (fst x1_2) then 1 else 0));
      w_10.state.c <- pc_to_exp (int_to_pc 11))
    12;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 1;
      let x_7 = resolve w_12 (Source.E 0) in
      match Word.get_value (fst x_7) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_12.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_12 1;
          return_n w_12 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_7 = Memo.splits (snd x_7) in
          let split0_7 = List.nth splits_7 0 in
          let split1_7 = List.nth splits_7 1 in
          Dynarray.set w_12.state.e 0 split0_7;
          push_env w_12 split1_7;
          assert_env_length w_12 2;
          let keep_7 = env_call w_12 [ 0 ] [ 1 ] in
          w_12.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_12.state.k ];
          w_12.state.c <- pc_to_exp (int_to_pc 13)
      | _ -> failwith "unreachable (14)")
    13;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 2;
      let x_8 = resolve w_13 (Source.E 1) in
      match Word.get_value (fst x_8) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_13.state.e 1 (Memo.from_constructor tag_Nil);
          Dynarray.set w_13.state.e 0
            (Memo.appends [ Memo.from_constructor tag_P; Dynarray.get w_13.state.e 1; Dynarray.get w_13.state.e 0 ]);
          assert_env_length w_13 2;
          return_n w_13 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_8 = Memo.splits (snd x_8) in
          let split0_8 = List.nth splits_8 0 in
          let split1_8 = List.nth splits_8 1 in
          Dynarray.set w_13.state.e 1 split0_8;
          push_env w_13 split1_8;
          assert_env_length w_13 3;
          w_13.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (17)")
    14;
  add_exp
    (fun w_15 ->
      let x_10 = resolve w_15 (Source.E 0) in
      match Word.get_value (fst x_10) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_15.state.e 0 (Memo.from_constructor tag_Nil);
          Dynarray.set w_15.state.e 1 (Memo.from_constructor tag_Nil);
          Dynarray.set w_15.state.e 0
            (Memo.appends [ Memo.from_constructor tag_P; Dynarray.get w_15.state.e 0; Dynarray.get w_15.state.e 1 ]);
          assert_env_length w_15 3;
          return_n w_15 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_10 = Memo.splits (snd x_10) in
          let split0_10 = List.nth splits_10 0 in
          let split1_10 = List.nth splits_10 1 in
          Dynarray.set w_15.state.e 0 split0_10;
          Dynarray.set w_15.state.e 1 split1_10;
          assert_env_length w_15 3;
          let keep_8 = env_call w_15 [ 0 ] [ 1; 2 ] in
          w_15.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_15.state.k ];
          w_15.state.c <- pc_to_exp (int_to_pc 14)
      | _ -> failwith "unreachable (15)")
    15;
  add_exp
    (fun w_14 ->
      let x_9 = resolve w_14 (Source.E 2) in
      match Word.get_value (fst x_9) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_14.state.e 1 (Memo.from_constructor tag_Nil);
          Dynarray.set w_14.state.e 0
            (Memo.appends [ Memo.from_constructor tag_P; Dynarray.get w_14.state.e 1; Dynarray.get w_14.state.e 0 ]);
          assert_env_length w_14 3;
          return_n w_14 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_9 = Memo.splits (snd x_9) in
          let split0_9 = List.nth splits_9 0 in
          let split1_9 = List.nth splits_9 1 in
          Dynarray.set w_14.state.e 1 split0_9;
          Dynarray.set w_14.state.e 2 split1_9;
          assert_env_length w_14 3;
          w_14.state.c <- pc_to_exp (int_to_pc 15)
      | _ -> failwith "unreachable (16)")
    16;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 1;
      let keep_9 = env_call w_16 [] [ 0; 0 ] in
      w_16.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_16.state.k ];
      w_16.state.c <- pc_to_exp (int_to_pc 14))
    17;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 2;
      let x_11 = resolve w_17 (Source.E 0) in
      match Word.get_value (fst x_11) with
      | 1 (* tag_Nil *) ->
          assert_env_length w_17 2;
          return_n w_17 1 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_11 = Memo.splits (snd x_11) in
          let split0_11 = List.nth splits_11 0 in
          let split1_11 = List.nth splits_11 1 in
          push_env w_17 split0_11;
          push_env w_17 split1_11;
          assert_env_length w_17 4;
          w_17.state.c <- pc_to_exp (int_to_pc 21)
      | _ -> failwith "unreachable (22)")
    18;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 7;
      let cond_2 = resolve w_20 (Source.E 6) in
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_20 7;
        let keep_10 = env_call w_20 [ 2 ] [ 3; 1 ] in
        w_20.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_20.state.k ];
        w_20.state.c <- pc_to_exp (int_to_pc 18))
      else (
        assert_env_length w_20 7;
        let keep_11 = env_call w_20 [ 4 ] [ 0; 5 ] in
        w_20.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_20.state.k ];
        w_20.state.c <- pc_to_exp (int_to_pc 18)))
    19;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 6;
      let x0_3 = resolve w_19 (Source.E 2) in
      let x1_3 = resolve w_19 (Source.E 4) in
      push_env w_19 (Memo.from_int (if Word.get_value (fst x0_3) < Word.get_value (fst x1_3) then 1 else 0));
      w_19.state.c <- pc_to_exp (int_to_pc 19))
    20;
  add_exp
    (fun w_18 ->
      let x_12 = resolve w_18 (Source.E 1) in
      match Word.get_value (fst x_12) with
      | 1 (* tag_Nil *) ->
          assert_env_length w_18 4;
          return_n w_18 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_12 = Memo.splits (snd x_12) in
          let split0_12 = List.nth splits_12 0 in
          let split1_12 = List.nth splits_12 1 in
          push_env w_18 split0_12;
          push_env w_18 split1_12;
          w_18.state.c <- pc_to_exp (int_to_pc 20)
      | _ -> failwith "unreachable (21)")
    21;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 1;
      let x_13 = resolve w_21 (Source.E 0) in
      match Word.get_value (fst x_13) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_21.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_21 1;
          return_n w_21 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_13 = Memo.splits (snd x_13) in
          let split0_13 = List.nth splits_13 0 in
          let split1_13 = List.nth splits_13 1 in
          push_env w_21 split0_13;
          push_env w_21 split1_13;
          assert_env_length w_21 3;
          w_21.state.c <- pc_to_exp (int_to_pc 23)
      | _ -> failwith "unreachable (24)")
    22;
  add_exp
    (fun w_22 ->
      let x_14 = resolve w_22 (Source.E 2) in
      match Word.get_value (fst x_14) with
      | 1 (* tag_Nil *) ->
          assert_env_length w_22 3;
          return_n w_22 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_14 = Memo.splits (snd x_14) in
          let split0_14 = List.nth splits_14 0 in
          let split1_14 = List.nth splits_14 1 in
          Dynarray.set w_22.state.e 1 split0_14;
          Dynarray.set w_22.state.e 2 split1_14;
          assert_env_length w_22 3;
          let keep_12 = env_call w_22 [] [ 0 ] in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 17)
      | _ -> failwith "unreachable (23)")
    23;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 2;
      let x_15 = resolve w_23 (Source.E 0) in
      match Word.get_value (fst x_15) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_23.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_23 2;
          return_n w_23 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_15 = Memo.splits (snd x_15) in
          let split0_15 = List.nth splits_15 0 in
          let split1_15 = List.nth splits_15 1 in
          Dynarray.set w_23.state.e 0 split0_15;
          push_env w_23 split1_15;
          w_23.state.c <- pc_to_exp (int_to_pc 26)
      | _ -> failwith "unreachable (27)")
    24;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 4;
      let cond_3 = resolve w_25 (Source.E 3) in
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_25 4;
        let keep_13 = env_call w_25 [ 0 ] [ 2; 1 ] in
        w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_25.state.k ];
        w_25.state.c <- pc_to_exp (int_to_pc 24))
      else (
        assert_env_length w_25 4;
        let keep_14 = env_call w_25 [] [ 2; 1 ] in
        w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_25.state.k ];
        w_25.state.c <- pc_to_exp (int_to_pc 24)))
    25;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 3;
      let x0_4 = resolve w_24 (Source.E 0) in
      let x1_4 = resolve w_24 (Source.E 1) in
      push_env w_24 (Memo.from_int (if Word.get_value (fst x0_4) > Word.get_value (fst x1_4) then 1 else 0));
      w_24.state.c <- pc_to_exp (int_to_pc 25))
    26;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 2;
      let x_16 = resolve w_26 (Source.E 0) in
      match Word.get_value (fst x_16) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_26.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_26 2;
          return_n w_26 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_16 = Memo.splits (snd x_16) in
          let split0_16 = List.nth splits_16 0 in
          let split1_16 = List.nth splits_16 1 in
          Dynarray.set w_26.state.e 0 split0_16;
          push_env w_26 split1_16;
          w_26.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (30)")
    27;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 4;
      let cond_4 = resolve w_28 (Source.E 3) in
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_28 4;
        let keep_15 = env_call w_28 [ 0 ] [ 2; 1 ] in
        w_28.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_28.state.k ];
        w_28.state.c <- pc_to_exp (int_to_pc 27))
      else (
        assert_env_length w_28 4;
        let keep_16 = env_call w_28 [] [ 2; 1 ] in
        w_28.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_28.state.k ];
        w_28.state.c <- pc_to_exp (int_to_pc 27)))
    28;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 3;
      let x0_5 = resolve w_27 (Source.E 0) in
      let x1_5 = resolve w_27 (Source.E 1) in
      push_env w_27 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      w_27.state.c <- pc_to_exp (int_to_pc 28))
    29;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 2;
      let x_17 = resolve w_29 (Source.E 0) in
      match Word.get_value (fst x_17) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_29.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_29 2;
          return_n w_29 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_17 = Memo.splits (snd x_17) in
          let split0_17 = List.nth splits_17 0 in
          let split1_17 = List.nth splits_17 1 in
          Dynarray.set w_29.state.e 0 split0_17;
          push_env w_29 split1_17;
          w_29.state.c <- pc_to_exp (int_to_pc 32)
      | _ -> failwith "unreachable (33)")
    30;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 4;
      let cond_5 = resolve w_31 (Source.E 3) in
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_31 4;
        let keep_17 = env_call w_31 [ 0 ] [ 2; 1 ] in
        w_31.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_31.state.k ];
        w_31.state.c <- pc_to_exp (int_to_pc 30))
      else (
        assert_env_length w_31 4;
        let keep_18 = env_call w_31 [] [ 2; 1 ] in
        w_31.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_31.state.k ];
        w_31.state.c <- pc_to_exp (int_to_pc 30)))
    31;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 3;
      let x0_6 = resolve w_30 (Source.E 0) in
      let x1_6 = resolve w_30 (Source.E 1) in
      push_env w_30 (Memo.from_int (if Word.get_value (fst x0_6) < Word.get_value (fst x1_6) then 1 else 0));
      w_30.state.c <- pc_to_exp (int_to_pc 31))
    32;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 1;
      let x_18 = resolve w_32 (Source.E 0) in
      match Word.get_value (fst x_18) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_32.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_32 1;
          return_n w_32 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_18 = Memo.splits (snd x_18) in
          let split0_18 = List.nth splits_18 0 in
          let split1_18 = List.nth splits_18 1 in
          push_env w_32 split0_18;
          push_env w_32 split1_18;
          assert_env_length w_32 3;
          let keep_19 = env_call w_32 [ 0; 1 ] [ 0; 1 ] in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_19; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (34)")
    33;
  add_exp
    (fun w_34 ->
      let x_19 = resolve w_34 (Source.E 1) in
      match Word.get_value (fst x_19) with
      | 13 (* tag_P *) ->
          let splits_19 = Memo.splits (snd x_19) in
          let split0_19 = List.nth splits_19 0 in
          let split1_19 = List.nth splits_19 1 in
          Dynarray.set w_34.state.e 1 split0_19;
          push_env w_34 split1_19;
          Dynarray.set w_34.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_34.state.e 0; Dynarray.get w_34.state.e 1 ]);
          Dynarray.set w_34.state.e 0
            (Memo.appends [ Memo.from_constructor tag_P; Dynarray.get w_34.state.e 0; Dynarray.get w_34.state.e 2 ]);
          assert_env_length w_34 3;
          return_n w_34 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (34)")
    34;
  add_exp
    (fun w_35 ->
      let x_20 = resolve w_35 (Source.E 0) in
      match Word.get_value (fst x_20) with
      | 13 (* tag_P *) ->
          let splits_20 = Memo.splits (snd x_20) in
          let split0_20 = List.nth splits_20 0 in
          let split1_20 = List.nth splits_20 1 in
          Dynarray.set w_35.state.e 0 split0_20;
          push_env w_35 split1_20;
          assert_env_length w_35 2;
          let keep_22 = env_call w_35 [ 1 ] [ 0 ] in
          w_35.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_22; w_35.state.k ];
          w_35.state.c <- pc_to_exp (int_to_pc 22)
      | _ -> failwith "unreachable (35)")
    35;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1);
  Words.set_constructor_degree 3 1;
  Words.set_constructor_degree 4 (-2);
  Words.set_constructor_degree 5 (-2);
  Words.set_constructor_degree 6 (-1);
  Words.set_constructor_degree 7 (-1);
  Words.set_constructor_degree 8 0;
  Words.set_constructor_degree 9 (-1);
  Words.set_constructor_degree 10 (-1);
  Words.set_constructor_degree 11 (-1);
  Words.set_constructor_degree 12 (-1);
  Words.set_constructor_degree 13 (-1);
  Words.set_constructor_degree 14 (-1);
  Words.set_constructor_degree 15 0;
  Words.set_constructor_degree 16 (-1);
  Words.set_constructor_degree 17 (-1);
  Words.set_constructor_degree 18 0;
  Words.set_constructor_degree 19 (-1);
  Words.set_constructor_degree 20 0;
  Words.set_constructor_degree 21 (-1);
  Words.set_constructor_degree 22 0;
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 0;
  Words.set_constructor_degree 25 (-2);
  Words.set_constructor_degree 26 0;
  Words.set_constructor_degree 27 0;
  Words.set_constructor_degree 28 (-1);
  Words.set_constructor_degree 29 (-2);
  Words.set_constructor_degree 30 (-1);
  Words.set_constructor_degree 31 (-3);
  Words.set_constructor_degree 32 0;
  Words.set_constructor_degree 33 (-2);
  Words.set_constructor_degree 34 (-2);
  Words.set_constructor_degree 35 (-1);
  Words.set_constructor_degree 36 0
