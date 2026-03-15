open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_X = 1
let tag_Y = 2
let tag_Const = 3
let tag_Var = 4
let tag_Add = 5
let tag_Mul = 6
let tag_cont_1 = 7
let tag_cont_2 = 8
let tag_cont_3 = 9
let tag_cont_4 = 10
let tag_cont_5 = 11
let tag_cont_6 = 12
let tag_cont_7 = 13
let tag_cont_8 = 14
let tag_cont_9 = 15
let tag_cont_10 = 16
let tag_cont_11 = 17
let tag_cont_12 = 18
let tag_cont_13 = 19
let tag_cont_14 = 20
let tag_cont_15 = 21
let tag_cont_16 = 22
let tag_cont_17 = 23
let tag_cont_18 = 24
let tag_cont_19 = 25
let tag_cont_20 = 26
let tag_cont_21 = 27
let tag_cont_22 = 28
let tag_cont_23 = 29
let tag_cont_24 = 30
let tag_cont_25 = 31
let tag_cont_26 = 32
let tag_cont_27 = 33
let tag_cont_28 = 34
let tag_cont_29 = 35

type var = X | Y

let rec from_ocaml_var x =
  match x with X -> Memo.appends [ Memo.from_constructor tag_X ] | Y -> Memo.appends [ Memo.from_constructor tag_Y ]

let rec to_ocaml_var x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with 1 (* tag_X *) -> X | 2 (* tag_Y *) -> Y | _ -> failwith "unreachable"

type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr

let rec from_ocaml_expr x =
  match x with
  | Const x0 -> Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int x0 ]
  | Var x0 -> Memo.appends [ Memo.from_constructor tag_Var; from_ocaml_var x0 ]
  | Add (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Add; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | Mul (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Mul; from_ocaml_expr x0; from_ocaml_expr x1 ]

let rec to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 3 (* tag_Const *) ->
      let x0 = Memo.splits_1 t in
      Const (Word.get_value (Memo.to_word x0))
  | 4 (* tag_Var *) ->
      let x0 = Memo.splits_1 t in
      Var (to_ocaml_var x0)
  | 5 (* tag_Add *) ->
      let x0, x1 = Memo.splits_2 t in
      Add (to_ocaml_expr x0, to_ocaml_expr x1)
  | 6 (* tag_Mul *) ->
      let x0, x1 = Memo.splits_2 t in
      Mul (to_ocaml_expr x0, to_ocaml_expr x1)
  | _ -> failwith "unreachable"

let rec var_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec compare_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_equal memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 6)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 13)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec simplify_aux memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 15)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec diffx memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 16)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 19)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec main memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 22)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_22 ->
      assert_env_length w_22 1;
      let hd_0, tl_0 = resolve w_22 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_22
      | 7 (* tag_cont_1 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 4;
          let keep_12 = env_call w_22 [ 0; 1; 2 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 3)
      | 8 (* tag_cont_2 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          let keep_13 = env_call w_22 [ 1 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 1)
      | 9 (* tag_cont_3 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 5;
          let keep_14 = env_call w_22 [ 2 ] 2 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 6)
      | 10 (* tag_cont_4 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 5;
          let keep_15 = env_call w_22 [ 2 ] 2 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 6)
      | 11 (* tag_cont_5 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          let keep_16 = env_call w_22 [ 1 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 13)
      | 12 (* tag_cont_6 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          let keep_17 = env_call w_22 [ 0; 1 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 13)
      | 13 (* tag_cont_7 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 4;
          let keep_18 = env_call w_22 [ 1 ] 2 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 6)
      | 14 (* tag_cont_8 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          let keep_19 = env_call w_22 [ 1 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_19; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 16)
      | 15 (* tag_cont_9 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 4;
          let ctor_arg_3 = pop_env w_22 in
          let ctor_arg_4 = pop_env w_22 in
          push_env w_22 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_4; ctor_arg_3 ]);
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 5;
          let keep_20 = env_call w_22 [ 2; 3 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_20; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 16)
      | 16 (* tag_cont_10 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 3 tl_0;
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 5;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 6;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 7;
          let keep_21 = env_call w_22 [ 3 ] 3 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_21; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 19)
      | 17 (* tag_cont_11 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 3 tl_0;
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 5;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 6;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 7;
          let keep_22 = env_call w_22 [ 3 ] 3 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_22; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 19)
      | 18 (* tag_cont_12 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 0 tl_0;
          assert_env_length w_22 1;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 2;
          let keep_23 = env_call w_22 [] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_23; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 16)
      | 19 (* tag_cont_13 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 3 tl_0;
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 5;
          push_env w_22 (Dynarray.get w_22.state.e 3);
          w_22.state.c <- pc_to_exp (int_to_pc 37)
      | 20 (* tag_cont_14 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 38)
      | 21 (* tag_cont_15 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 39)
      | 22 (* tag_cont_16 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 40)
      | 23 (* tag_cont_17 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 4;
          let keep_27 = env_call w_22 [ 0; 1 ] 2 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_27; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 6)
      | 24 (* tag_cont_18 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          w_22.state.c <- pc_to_exp (int_to_pc 56)
      | 25 (* tag_cont_19 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 57)
      | 26 (* tag_cont_20 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          let ctor_arg_31 = pop_env w_22 in
          let ctor_arg_32 = pop_env w_22 in
          push_env w_22 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_32; ctor_arg_31 ]);
          assert_env_length w_22 1;
          drop_n w_22 1 0;
          assert_env_length w_22 1;
          return_n w_22 1 (pc_to_exp (int_to_pc 0))
      | 27 (* tag_cont_21 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          let ctor_arg_33 = pop_env w_22 in
          let ctor_arg_34 = pop_env w_22 in
          push_env w_22 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_34; ctor_arg_33 ]);
          assert_env_length w_22 2;
          let ctor_arg_35 = pop_env w_22 in
          let ctor_arg_36 = pop_env w_22 in
          push_env w_22 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_36; ctor_arg_35 ]);
          assert_env_length w_22 1;
          drop_n w_22 1 0;
          assert_env_length w_22 1;
          return_n w_22 1 (pc_to_exp (int_to_pc 0))
      | 28 (* tag_cont_22 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 58)
      | 29 (* tag_cont_23 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 59)
      | 30 (* tag_cont_24 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 0 tl_0;
          assert_env_length w_22 1;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 2;
          ignore (env_call w_22 [] 1);
          w_22.state.c <- pc_to_exp (int_to_pc 15)
      | 31 (* tag_cont_25 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          let keep_28 = env_call w_22 [ 1 ] 1 in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_28; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 1)
      | 32 (* tag_cont_26 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 4;
          push_env w_22 (Memo.from_int 0);
          w_22.state.c <- pc_to_exp (int_to_pc 61)
      | 33 (* tag_cont_27 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 4;
          push_env w_22 (Memo.from_int 0);
          w_22.state.c <- pc_to_exp (int_to_pc 63)
      | 34 (* tag_cont_28 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 2 tl_0;
          w_22.state.c <- pc_to_exp (int_to_pc 76)
      | 35 (* tag_cont_29 *) ->
          w_22.state.k <- get_next_cont tl_0;
          restore_env w_22 1 tl_0;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 0);
          assert_env_length w_22 3;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          w_22.state.c <- pc_to_exp (int_to_pc 81)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 2))
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      let last_0 = Source.E 1 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | 1 (* tag_X *) ->
          ignore (pop_env w_1);
          assert_env_length w_1 1;
          push_env w_1 (Memo.from_int 0);
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Y *) ->
          ignore (pop_env w_1);
          assert_env_length w_1 1;
          push_env w_1 (Memo.from_int 1);
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 1;
      push_env w_2 (Dynarray.get w_2.state.e 0);
      w_2.state.c <- pc_to_exp (int_to_pc 4))
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      let last_1 = Source.E 1 in
      let x_1 = resolve w_3 last_1 in
      match Word.get_value (fst x_1) with
      | 3 (* tag_Const *) ->
          let splits_0 = Memo.splits (snd x_1) in
          let split0_0 = List.nth splits_0 0 in
          ignore (pop_env w_3);
          push_env w_3 split0_0;
          assert_env_length w_3 2;
          push_env w_3 (Memo.from_int 0);
          assert_env_length w_3 3;
          drop_n w_3 3 1;
          assert_env_length w_3 2;
          return_n w_3 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          ignore (pop_env w_3);
          push_env w_3 split0_1;
          assert_env_length w_3 2;
          push_env w_3 (Memo.from_int 1);
          assert_env_length w_3 3;
          drop_n w_3 3 1;
          assert_env_length w_3 2;
          return_n w_3 2 (pc_to_exp (int_to_pc 0))
      | 5 (* tag_Add *) ->
          let splits_2 = Memo.splits (snd x_1) in
          let split0_2 = List.nth splits_2 0 in
          let split1_0 = List.nth splits_2 1 in
          ignore (pop_env w_3);
          push_env w_3 split0_2;
          push_env w_3 split1_0;
          assert_env_length w_3 3;
          push_env w_3 (Memo.from_int 2);
          assert_env_length w_3 4;
          drop_n w_3 4 2;
          assert_env_length w_3 2;
          return_n w_3 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Mul *) ->
          let splits_3 = Memo.splits (snd x_1) in
          let split0_3 = List.nth splits_3 0 in
          let split1_1 = List.nth splits_3 1 in
          ignore (pop_env w_3);
          push_env w_3 split0_3;
          push_env w_3 split1_1;
          assert_env_length w_3 3;
          push_env w_3 (Memo.from_int 3);
          assert_env_length w_3 4;
          drop_n w_3 4 2;
          assert_env_length w_3 2;
          return_n w_3 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (4)")
    4;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 2;
      push_env w_4 (Dynarray.get w_4.state.e 0);
      assert_env_length w_4 3;
      let keep_0 = env_call w_4 [ 0; 1 ] 1 in
      w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_4.state.k ];
      w_4.state.c <- pc_to_exp (int_to_pc 3))
    5;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 2;
      push_env w_5 (Dynarray.get w_5.state.e 0);
      w_5.state.c <- pc_to_exp (int_to_pc 12))
    6;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 6;
      let x0_0 = resolve w_8 (Source.E 4) in
      let x1_0 = resolve w_8 (Source.E 5) in
      ignore (pop_env w_8);
      ignore (pop_env w_8);
      push_env w_8 (Memo.from_int (if Word.get_value (fst x0_0) = Word.get_value (fst x1_0) then 1 else 0));
      assert_env_length w_8 5;
      drop_n w_8 5 1;
      assert_env_length w_8 4;
      drop_n w_8 4 1;
      assert_env_length w_8 3;
      return_n w_8 3 (pc_to_exp (int_to_pc 0)))
    7;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 4;
      let last_3 = Source.E 3 in
      let x_3 = resolve w_7 last_3 in
      match Word.get_value (fst x_3) with
      | 3 (* tag_Const *) ->
          let splits_5 = Memo.splits (snd x_3) in
          let split0_5 = List.nth splits_5 0 in
          ignore (pop_env w_7);
          push_env w_7 split0_5;
          assert_env_length w_7 4;
          push_env w_7 (Dynarray.get w_7.state.e 2);
          assert_env_length w_7 5;
          push_env w_7 (Dynarray.get w_7.state.e 3);
          w_7.state.c <- pc_to_exp (int_to_pc 7)
      | _ ->
          ignore (pop_env w_7);
          assert_env_length w_7 3;
          push_env w_7 (Memo.from_int 0);
          assert_env_length w_7 4;
          drop_n w_7 4 1;
          assert_env_length w_7 3;
          return_n w_7 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (8)")
    8;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 4;
      let last_4 = Source.E 3 in
      let x_4 = resolve w_9 last_4 in
      match Word.get_value (fst x_4) with
      | 4 (* tag_Var *) ->
          let splits_7 = Memo.splits (snd x_4) in
          let split0_7 = List.nth splits_7 0 in
          ignore (pop_env w_9);
          push_env w_9 split0_7;
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 2);
          assert_env_length w_9 5;
          let keep_1 = env_call w_9 [ 3 ] 1 in
          w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_9.state.k ];
          w_9.state.c <- pc_to_exp (int_to_pc 1)
      | _ ->
          ignore (pop_env w_9);
          assert_env_length w_9 3;
          push_env w_9 (Memo.from_int 0);
          assert_env_length w_9 4;
          drop_n w_9 4 1;
          assert_env_length w_9 3;
          return_n w_9 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (9)")
    9;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 5;
      let last_5 = Source.E 4 in
      let x_5 = resolve w_10 last_5 in
      match Word.get_value (fst x_5) with
      | 5 (* tag_Add *) ->
          let splits_9 = Memo.splits (snd x_5) in
          let split0_9 = List.nth splits_9 0 in
          let split1_3 = List.nth splits_9 1 in
          ignore (pop_env w_10);
          push_env w_10 split0_9;
          push_env w_10 split1_3;
          assert_env_length w_10 6;
          push_env w_10 (Dynarray.get w_10.state.e 2);
          assert_env_length w_10 7;
          push_env w_10 (Dynarray.get w_10.state.e 4);
          assert_env_length w_10 8;
          let keep_2 = env_call w_10 [ 3; 5 ] 2 in
          w_10.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_10.state.k ];
          w_10.state.c <- pc_to_exp (int_to_pc 6)
      | _ ->
          ignore (pop_env w_10);
          assert_env_length w_10 4;
          push_env w_10 (Memo.from_int 0);
          assert_env_length w_10 5;
          drop_n w_10 5 2;
          assert_env_length w_10 3;
          return_n w_10 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (10)")
    10;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 5;
      let last_6 = Source.E 4 in
      let x_6 = resolve w_11 last_6 in
      match Word.get_value (fst x_6) with
      | 6 (* tag_Mul *) ->
          let splits_11 = Memo.splits (snd x_6) in
          let split0_11 = List.nth splits_11 0 in
          let split1_5 = List.nth splits_11 1 in
          ignore (pop_env w_11);
          push_env w_11 split0_11;
          push_env w_11 split1_5;
          assert_env_length w_11 6;
          push_env w_11 (Dynarray.get w_11.state.e 2);
          assert_env_length w_11 7;
          push_env w_11 (Dynarray.get w_11.state.e 4);
          assert_env_length w_11 8;
          let keep_3 = env_call w_11 [ 3; 5 ] 2 in
          w_11.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_11.state.k ];
          w_11.state.c <- pc_to_exp (int_to_pc 6)
      | _ ->
          ignore (pop_env w_11);
          assert_env_length w_11 4;
          push_env w_11 (Memo.from_int 0);
          assert_env_length w_11 5;
          drop_n w_11 5 2;
          assert_env_length w_11 3;
          return_n w_11 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (11)")
    11;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 3;
      let last_2 = Source.E 2 in
      let x_2 = resolve w_6 last_2 in
      match Word.get_value (fst x_2) with
      | 3 (* tag_Const *) ->
          let splits_4 = Memo.splits (snd x_2) in
          let split0_4 = List.nth splits_4 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_4;
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 8)
      | 4 (* tag_Var *) ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_6;
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 9)
      | 5 (* tag_Add *) ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          let split1_2 = List.nth splits_8 1 in
          ignore (pop_env w_6);
          push_env w_6 split0_8;
          push_env w_6 split1_2;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 10)
      | 6 (* tag_Mul *) ->
          let splits_10 = Memo.splits (snd x_2) in
          let split0_10 = List.nth splits_10 0 in
          let split1_4 = List.nth splits_10 1 in
          ignore (pop_env w_6);
          push_env w_6 split0_10;
          push_env w_6 split1_4;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 11)
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
      let last_7 = Source.E 1 in
      let x_7 = resolve w_13 last_7 in
      match Word.get_value (fst x_7) with
      | 3 (* tag_Const *) ->
          let splits_12 = Memo.splits (snd x_7) in
          let split0_12 = List.nth splits_12 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_12;
          assert_env_length w_13 2;
          push_env w_13 (Dynarray.get w_13.state.e 0);
          assert_env_length w_13 3;
          drop_n w_13 3 1;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_13 = Memo.splits (snd x_7) in
          let split0_13 = List.nth splits_13 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_13;
          assert_env_length w_13 2;
          push_env w_13 (Dynarray.get w_13.state.e 0);
          assert_env_length w_13 3;
          drop_n w_13 3 1;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | 5 (* tag_Add *) ->
          let splits_14 = Memo.splits (snd x_7) in
          let split0_14 = List.nth splits_14 0 in
          let split1_6 = List.nth splits_14 1 in
          ignore (pop_env w_13);
          push_env w_13 split0_14;
          push_env w_13 split1_6;
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 4;
          let keep_4 = env_call w_13 [ 2 ] 1 in
          w_13.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_13.state.k ];
          w_13.state.c <- pc_to_exp (int_to_pc 13)
      | 6 (* tag_Mul *) ->
          let splits_15 = Memo.splits (snd x_7) in
          let split0_15 = List.nth splits_15 0 in
          let split1_7 = List.nth splits_15 1 in
          ignore (pop_env w_13);
          push_env w_13 split0_15;
          push_env w_13 split1_7;
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 4;
          let keep_5 = env_call w_13 [ 2 ] 1 in
          w_13.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_13.state.k ];
          w_13.state.c <- pc_to_exp (int_to_pc 13)
      | _ -> failwith "unreachable (14)")
    14;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 1;
      push_env w_14 (Dynarray.get w_14.state.e 0);
      assert_env_length w_14 2;
      let keep_6 = env_call w_14 [ 0 ] 1 in
      w_14.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_14.state.k ];
      w_14.state.c <- pc_to_exp (int_to_pc 13))
    15;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 1;
      push_env w_15 (Dynarray.get w_15.state.e 0);
      w_15.state.c <- pc_to_exp (int_to_pc 18))
    16;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 3;
      let last_9 = Source.E 2 in
      let x_9 = resolve w_17 last_9 in
      match Word.get_value (fst x_9) with
      | 1 (* tag_X *) ->
          ignore (pop_env w_17);
          assert_env_length w_17 2;
          push_env w_17 (Memo.from_int 1);
          assert_env_length w_17 3;
          let ctor_arg_1 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_1 ]);
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          return_n w_17 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Y *) ->
          ignore (pop_env w_17);
          assert_env_length w_17 2;
          push_env w_17 (Memo.from_int 0);
          assert_env_length w_17 3;
          let ctor_arg_2 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_2 ]);
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          return_n w_17 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (17)")
    17;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 2;
      let last_8 = Source.E 1 in
      let x_8 = resolve w_16 last_8 in
      match Word.get_value (fst x_8) with
      | 3 (* tag_Const *) ->
          let splits_16 = Memo.splits (snd x_8) in
          let split0_16 = List.nth splits_16 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_16;
          assert_env_length w_16 2;
          push_env w_16 (Memo.from_int 0);
          assert_env_length w_16 3;
          let ctor_arg_0 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_0 ]);
          assert_env_length w_16 3;
          drop_n w_16 3 1;
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_17 = Memo.splits (snd x_8) in
          let split0_17 = List.nth splits_17 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_17;
          assert_env_length w_16 2;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 17)
      | 5 (* tag_Add *) ->
          let splits_18 = Memo.splits (snd x_8) in
          let split0_18 = List.nth splits_18 0 in
          let split1_8 = List.nth splits_18 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_18;
          push_env w_16 split1_8;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          assert_env_length w_16 4;
          let keep_7 = env_call w_16 [ 2 ] 1 in
          w_16.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_16.state.k ];
          w_16.state.c <- pc_to_exp (int_to_pc 16)
      | 6 (* tag_Mul *) ->
          let splits_19 = Memo.splits (snd x_8) in
          let split0_19 = List.nth splits_19 0 in
          let split1_9 = List.nth splits_19 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_19;
          push_env w_16 split1_9;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          assert_env_length w_16 4;
          let keep_8 = env_call w_16 [ 1; 2 ] 1 in
          w_16.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_16.state.k ];
          w_16.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (18)")
    18;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 3;
      push_env w_18 (Dynarray.get w_18.state.e 0);
      w_18.state.c <- pc_to_exp (int_to_pc 21))
    19;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 5;
      let last_11 = Source.E 4 in
      let x_11 = resolve w_20 last_11 in
      match Word.get_value (fst x_11) with
      | 1 (* tag_X *) ->
          ignore (pop_env w_20);
          assert_env_length w_20 4;
          push_env w_20 (Dynarray.get w_20.state.e 1);
          assert_env_length w_20 5;
          drop_n w_20 5 1;
          assert_env_length w_20 4;
          return_n w_20 4 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Y *) ->
          ignore (pop_env w_20);
          assert_env_length w_20 4;
          push_env w_20 (Dynarray.get w_20.state.e 2);
          assert_env_length w_20 5;
          drop_n w_20 5 1;
          assert_env_length w_20 4;
          return_n w_20 4 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (20)")
    20;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 4;
      let last_10 = Source.E 3 in
      let x_10 = resolve w_19 last_10 in
      match Word.get_value (fst x_10) with
      | 3 (* tag_Const *) ->
          let splits_20 = Memo.splits (snd x_10) in
          let split0_20 = List.nth splits_20 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_20;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          assert_env_length w_19 5;
          drop_n w_19 5 1;
          assert_env_length w_19 4;
          return_n w_19 4 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_21 = Memo.splits (snd x_10) in
          let split0_21 = List.nth splits_21 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_21;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          w_19.state.c <- pc_to_exp (int_to_pc 20)
      | 5 (* tag_Add *) ->
          let splits_22 = Memo.splits (snd x_10) in
          let split0_22 = List.nth splits_22 0 in
          let split1_10 = List.nth splits_22 1 in
          ignore (pop_env w_19);
          push_env w_19 split0_22;
          push_env w_19 split1_10;
          assert_env_length w_19 5;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          assert_env_length w_19 6;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          assert_env_length w_19 7;
          push_env w_19 (Dynarray.get w_19.state.e 2);
          assert_env_length w_19 8;
          let keep_9 = env_call w_19 [ 1; 2; 4 ] 3 in
          w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_19.state.k ];
          w_19.state.c <- pc_to_exp (int_to_pc 19)
      | 6 (* tag_Mul *) ->
          let splits_23 = Memo.splits (snd x_10) in
          let split0_23 = List.nth splits_23 0 in
          let split1_11 = List.nth splits_23 1 in
          ignore (pop_env w_19);
          push_env w_19 split0_23;
          push_env w_19 split1_11;
          assert_env_length w_19 5;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          assert_env_length w_19 6;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          assert_env_length w_19 7;
          push_env w_19 (Dynarray.get w_19.state.e 2);
          assert_env_length w_19 8;
          let keep_10 = env_call w_19 [ 1; 2; 4 ] 3 in
          w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_19.state.k ];
          w_19.state.c <- pc_to_exp (int_to_pc 19)
      | _ -> failwith "unreachable (21)")
    21;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 1;
      push_env w_21 (Dynarray.get w_21.state.e 0);
      assert_env_length w_21 2;
      let keep_11 = env_call w_21 [] 1 in
      w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_21.state.k ];
      w_21.state.c <- pc_to_exp (int_to_pc 16))
    22;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 7;
      let cond_3 = resolve w_32 (Source.E 6) in
      ignore (pop_env w_32);
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_32 6;
        push_env w_32 (Memo.from_int 1);
        assert_env_length w_32 7;
        drop_n w_32 7 1;
        assert_env_length w_32 6;
        drop_n w_32 6 1;
        assert_env_length w_32 5;
        drop_n w_32 5 1;
        assert_env_length w_32 4;
        drop_n w_32 4 1;
        assert_env_length w_32 3;
        return_n w_32 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_32 6;
        push_env w_32 (Memo.from_int 0);
        assert_env_length w_32 7;
        drop_n w_32 7 1;
        assert_env_length w_32 6;
        drop_n w_32 6 1;
        assert_env_length w_32 5;
        drop_n w_32 5 1;
        assert_env_length w_32 4;
        drop_n w_32 4 1;
        assert_env_length w_32 3;
        return_n w_32 3 (pc_to_exp (int_to_pc 0))))
    23;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 8;
      let x0_4 = resolve w_31 (Source.E 6) in
      let x1_4 = resolve w_31 (Source.E 7) in
      ignore (pop_env w_31);
      ignore (pop_env w_31);
      push_env w_31 (Memo.from_int (if Word.get_value (fst x0_4) > Word.get_value (fst x1_4) then 1 else 0));
      w_31.state.c <- pc_to_exp (int_to_pc 23))
    24;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 8;
      let x0_5 = resolve w_33 (Source.E 6) in
      let x1_5 = resolve w_33 (Source.E 7) in
      ignore (pop_env w_33);
      ignore (pop_env w_33);
      push_env w_33 (Memo.from_int (Word.get_value (fst x0_5) - Word.get_value (fst x1_5)));
      assert_env_length w_33 7;
      drop_n w_33 7 1;
      assert_env_length w_33 6;
      drop_n w_33 6 1;
      assert_env_length w_33 5;
      drop_n w_33 5 1;
      assert_env_length w_33 4;
      drop_n w_33 4 1;
      assert_env_length w_33 3;
      return_n w_33 3 (pc_to_exp (int_to_pc 0)))
    25;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 7;
      let cond_2 = resolve w_30 (Source.E 6) in
      ignore (pop_env w_30);
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_30 6;
        push_env w_30 (Memo.from_int 0);
        assert_env_length w_30 7;
        push_env w_30 (Memo.from_int 1);
        w_30.state.c <- pc_to_exp (int_to_pc 25))
      else (
        assert_env_length w_30 6;
        push_env w_30 (Dynarray.get w_30.state.e 4);
        assert_env_length w_30 7;
        push_env w_30 (Dynarray.get w_30.state.e 5);
        w_30.state.c <- pc_to_exp (int_to_pc 24)))
    26;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 8;
      let x0_3 = resolve w_29 (Source.E 6) in
      let x1_3 = resolve w_29 (Source.E 7) in
      ignore (pop_env w_29);
      ignore (pop_env w_29);
      push_env w_29 (Memo.from_int (if Word.get_value (fst x0_3) < Word.get_value (fst x1_3) then 1 else 0));
      w_29.state.c <- pc_to_exp (int_to_pc 26))
    27;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 6;
      let last_13 = Source.E 5 in
      let x_13 = resolve w_28 last_13 in
      match Word.get_value (fst x_13) with
      | 3 (* tag_Const *) ->
          let splits_25 = Memo.splits (snd x_13) in
          let split0_25 = List.nth splits_25 0 in
          ignore (pop_env w_28);
          push_env w_28 split0_25;
          assert_env_length w_28 6;
          push_env w_28 (Dynarray.get w_28.state.e 4);
          assert_env_length w_28 7;
          push_env w_28 (Dynarray.get w_28.state.e 5);
          w_28.state.c <- pc_to_exp (int_to_pc 27)
      | _ -> failwith "unreachable (28)")
    28;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 6;
      let last_14 = Source.E 5 in
      let x_14 = resolve w_34 last_14 in
      match Word.get_value (fst x_14) with
      | 4 (* tag_Var *) ->
          let splits_27 = Memo.splits (snd x_14) in
          let split0_27 = List.nth splits_27 0 in
          ignore (pop_env w_34);
          push_env w_34 split0_27;
          assert_env_length w_34 6;
          push_env w_34 (Dynarray.get w_34.state.e 4);
          assert_env_length w_34 7;
          let keep_24 = env_call w_34 [ 5 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_24; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (29)")
    29;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 7;
      let last_15 = Source.E 6 in
      let x_15 = resolve w_35 last_15 in
      match Word.get_value (fst x_15) with
      | 5 (* tag_Add *) ->
          let splits_29 = Memo.splits (snd x_15) in
          let split0_29 = List.nth splits_29 0 in
          let split1_13 = List.nth splits_29 1 in
          ignore (pop_env w_35);
          push_env w_35 split0_29;
          push_env w_35 split1_13;
          assert_env_length w_35 8;
          push_env w_35 (Dynarray.get w_35.state.e 4);
          assert_env_length w_35 9;
          push_env w_35 (Dynarray.get w_35.state.e 6);
          assert_env_length w_35 10;
          let keep_25 = env_call w_35 [ 5; 7 ] 2 in
          w_35.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_25; w_35.state.k ];
          w_35.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (30)")
    30;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 7;
      let last_16 = Source.E 6 in
      let x_16 = resolve w_36 last_16 in
      match Word.get_value (fst x_16) with
      | 6 (* tag_Mul *) ->
          let splits_31 = Memo.splits (snd x_16) in
          let split0_31 = List.nth splits_31 0 in
          let split1_15 = List.nth splits_31 1 in
          ignore (pop_env w_36);
          push_env w_36 split0_31;
          push_env w_36 split1_15;
          assert_env_length w_36 8;
          push_env w_36 (Dynarray.get w_36.state.e 4);
          assert_env_length w_36 9;
          push_env w_36 (Dynarray.get w_36.state.e 6);
          assert_env_length w_36 10;
          let keep_26 = env_call w_36 [ 5; 7 ] 2 in
          w_36.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_26; w_36.state.k ];
          w_36.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (31)")
    31;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 5;
      let last_12 = Source.E 4 in
      let x_12 = resolve w_27 last_12 in
      match Word.get_value (fst x_12) with
      | 3 (* tag_Const *) ->
          let splits_24 = Memo.splits (snd x_12) in
          let split0_24 = List.nth splits_24 0 in
          ignore (pop_env w_27);
          push_env w_27 split0_24;
          assert_env_length w_27 5;
          push_env w_27 (Dynarray.get w_27.state.e 1);
          w_27.state.c <- pc_to_exp (int_to_pc 28)
      | 4 (* tag_Var *) ->
          let splits_26 = Memo.splits (snd x_12) in
          let split0_26 = List.nth splits_26 0 in
          ignore (pop_env w_27);
          push_env w_27 split0_26;
          assert_env_length w_27 5;
          push_env w_27 (Dynarray.get w_27.state.e 1);
          w_27.state.c <- pc_to_exp (int_to_pc 29)
      | 5 (* tag_Add *) ->
          let splits_28 = Memo.splits (snd x_12) in
          let split0_28 = List.nth splits_28 0 in
          let split1_12 = List.nth splits_28 1 in
          ignore (pop_env w_27);
          push_env w_27 split0_28;
          push_env w_27 split1_12;
          assert_env_length w_27 6;
          push_env w_27 (Dynarray.get w_27.state.e 1);
          w_27.state.c <- pc_to_exp (int_to_pc 30)
      | 6 (* tag_Mul *) ->
          let splits_30 = Memo.splits (snd x_12) in
          let split0_30 = List.nth splits_30 0 in
          let split1_14 = List.nth splits_30 1 in
          ignore (pop_env w_27);
          push_env w_27 split0_30;
          push_env w_27 split1_14;
          assert_env_length w_27 6;
          push_env w_27 (Dynarray.get w_27.state.e 1);
          w_27.state.c <- pc_to_exp (int_to_pc 31)
      | _ -> failwith "unreachable (32)")
    32;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 5;
      let cond_1 = resolve w_26 (Source.E 4) in
      ignore (pop_env w_26);
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_26 4;
        push_env w_26 (Memo.from_int 1);
        assert_env_length w_26 5;
        drop_n w_26 5 1;
        assert_env_length w_26 4;
        drop_n w_26 4 1;
        assert_env_length w_26 3;
        return_n w_26 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_26 4;
        push_env w_26 (Dynarray.get w_26.state.e 0);
        w_26.state.c <- pc_to_exp (int_to_pc 32)))
    33;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 6;
      let x0_2 = resolve w_25 (Source.E 4) in
      let x1_2 = resolve w_25 (Source.E 5) in
      ignore (pop_env w_25);
      ignore (pop_env w_25);
      push_env w_25 (Memo.from_int (if Word.get_value (fst x0_2) > Word.get_value (fst x1_2) then 1 else 0));
      w_25.state.c <- pc_to_exp (int_to_pc 33))
    34;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 6;
      let x0_6 = resolve w_37 (Source.E 4) in
      let x1_6 = resolve w_37 (Source.E 5) in
      ignore (pop_env w_37);
      ignore (pop_env w_37);
      push_env w_37 (Memo.from_int (Word.get_value (fst x0_6) - Word.get_value (fst x1_6)));
      assert_env_length w_37 5;
      drop_n w_37 5 1;
      assert_env_length w_37 4;
      drop_n w_37 4 1;
      assert_env_length w_37 3;
      return_n w_37 3 (pc_to_exp (int_to_pc 0)))
    35;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 5;
      let cond_0 = resolve w_24 (Source.E 4) in
      ignore (pop_env w_24);
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_24 4;
        push_env w_24 (Memo.from_int 0);
        assert_env_length w_24 5;
        push_env w_24 (Memo.from_int 1);
        w_24.state.c <- pc_to_exp (int_to_pc 35))
      else (
        assert_env_length w_24 4;
        push_env w_24 (Dynarray.get w_24.state.e 2);
        assert_env_length w_24 5;
        push_env w_24 (Dynarray.get w_24.state.e 3);
        w_24.state.c <- pc_to_exp (int_to_pc 34)))
    36;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 6;
      let x0_1 = resolve w_23 (Source.E 4) in
      let x1_1 = resolve w_23 (Source.E 5) in
      ignore (pop_env w_23);
      ignore (pop_env w_23);
      push_env w_23 (Memo.from_int (if Word.get_value (fst x0_1) < Word.get_value (fst x1_1) then 1 else 0));
      w_23.state.c <- pc_to_exp (int_to_pc 36))
    37;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 2;
      let x0_7 = resolve w_38 (Source.E 0) in
      let x1_7 = resolve w_38 (Source.E 1) in
      ignore (pop_env w_38);
      ignore (pop_env w_38);
      push_env w_38 (Memo.from_int (if Word.get_value (fst x0_7) = Word.get_value (fst x1_7) then 1 else 0));
      assert_env_length w_38 1;
      drop_n w_38 1 0;
      assert_env_length w_38 1;
      drop_n w_38 1 0;
      assert_env_length w_38 1;
      return_n w_38 1 (pc_to_exp (int_to_pc 0)))
    38;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 2;
      let x0_8 = resolve w_39 (Source.E 0) in
      let x1_8 = resolve w_39 (Source.E 1) in
      ignore (pop_env w_39);
      ignore (pop_env w_39);
      push_env w_39 (Memo.from_int (if Word.get_value (fst x0_8) <> 0 && Word.get_value (fst x1_8) <> 0 then 1 else 0));
      assert_env_length w_39 1;
      drop_n w_39 1 0;
      assert_env_length w_39 1;
      drop_n w_39 1 0;
      assert_env_length w_39 1;
      return_n w_39 1 (pc_to_exp (int_to_pc 0)))
    39;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 2;
      let x0_9 = resolve w_40 (Source.E 0) in
      let x1_9 = resolve w_40 (Source.E 1) in
      ignore (pop_env w_40);
      ignore (pop_env w_40);
      push_env w_40 (Memo.from_int (if Word.get_value (fst x0_9) <> 0 && Word.get_value (fst x1_9) <> 0 then 1 else 0));
      assert_env_length w_40 1;
      drop_n w_40 1 0;
      assert_env_length w_40 1;
      drop_n w_40 1 0;
      assert_env_length w_40 1;
      return_n w_40 1 (pc_to_exp (int_to_pc 0)))
    40;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 7;
      let x0_14 = resolve w_51 (Source.E 5) in
      let x1_14 = resolve w_51 (Source.E 6) in
      ignore (pop_env w_51);
      ignore (pop_env w_51);
      push_env w_51 (Memo.from_int (Word.get_value (fst x0_14) * Word.get_value (fst x1_14)));
      assert_env_length w_51 6;
      let ctor_arg_5 = pop_env w_51 in
      push_env w_51 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_5 ]);
      assert_env_length w_51 6;
      drop_n w_51 6 1;
      assert_env_length w_51 5;
      drop_n w_51 5 1;
      assert_env_length w_51 4;
      drop_n w_51 4 1;
      assert_env_length w_51 3;
      drop_n w_51 3 1;
      assert_env_length w_51 2;
      drop_n w_51 2 1;
      assert_env_length w_51 1;
      return_n w_51 1 (pc_to_exp (int_to_pc 0)))
    41;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 6;
      let cond_7 = resolve w_50 (Source.E 5) in
      ignore (pop_env w_50);
      if Word.get_value (fst cond_7) <> 0 then (
        assert_env_length w_50 5;
        push_env w_50 (Dynarray.get w_50.state.e 1);
        assert_env_length w_50 6;
        drop_n w_50 6 1;
        assert_env_length w_50 5;
        drop_n w_50 5 1;
        assert_env_length w_50 4;
        drop_n w_50 4 1;
        assert_env_length w_50 3;
        drop_n w_50 3 1;
        assert_env_length w_50 2;
        drop_n w_50 2 1;
        assert_env_length w_50 1;
        return_n w_50 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_50 5;
        push_env w_50 (Dynarray.get w_50.state.e 3);
        assert_env_length w_50 6;
        push_env w_50 (Dynarray.get w_50.state.e 4);
        w_50.state.c <- pc_to_exp (int_to_pc 41)))
    42;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 7;
      let x0_13 = resolve w_49 (Source.E 5) in
      let x1_13 = resolve w_49 (Source.E 6) in
      ignore (pop_env w_49);
      ignore (pop_env w_49);
      push_env w_49 (Memo.from_int (if Word.get_value (fst x0_13) = Word.get_value (fst x1_13) then 1 else 0));
      w_49.state.c <- pc_to_exp (int_to_pc 42))
    43;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 6;
      let cond_6 = resolve w_48 (Source.E 5) in
      ignore (pop_env w_48);
      if Word.get_value (fst cond_6) <> 0 then (
        assert_env_length w_48 5;
        push_env w_48 (Memo.from_int 0);
        assert_env_length w_48 6;
        let ctor_arg_6 = pop_env w_48 in
        push_env w_48 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_6 ]);
        assert_env_length w_48 6;
        drop_n w_48 6 1;
        assert_env_length w_48 5;
        drop_n w_48 5 1;
        assert_env_length w_48 4;
        drop_n w_48 4 1;
        assert_env_length w_48 3;
        drop_n w_48 3 1;
        assert_env_length w_48 2;
        drop_n w_48 2 1;
        assert_env_length w_48 1;
        return_n w_48 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_48 5;
        push_env w_48 (Dynarray.get w_48.state.e 4);
        assert_env_length w_48 6;
        push_env w_48 (Memo.from_int 1);
        w_48.state.c <- pc_to_exp (int_to_pc 43)))
    44;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 7;
      let x0_12 = resolve w_47 (Source.E 5) in
      let x1_12 = resolve w_47 (Source.E 6) in
      ignore (pop_env w_47);
      ignore (pop_env w_47);
      push_env w_47 (Memo.from_int (if Word.get_value (fst x0_12) = Word.get_value (fst x1_12) then 1 else 0));
      w_47.state.c <- pc_to_exp (int_to_pc 44))
    45;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 5;
      let last_18 = Source.E 4 in
      let x_18 = resolve w_46 last_18 in
      match Word.get_value (fst x_18) with
      | 3 (* tag_Const *) ->
          let splits_33 = Memo.splits (snd x_18) in
          let split0_33 = List.nth splits_33 0 in
          ignore (pop_env w_46);
          push_env w_46 split0_33;
          assert_env_length w_46 5;
          push_env w_46 (Dynarray.get w_46.state.e 4);
          assert_env_length w_46 6;
          push_env w_46 (Memo.from_int 0);
          w_46.state.c <- pc_to_exp (int_to_pc 45)
      | _ ->
          ignore (pop_env w_46);
          assert_env_length w_46 4;
          push_env w_46 (Dynarray.get w_46.state.e 1);
          assert_env_length w_46 5;
          push_env w_46 (Dynarray.get w_46.state.e 2);
          assert_env_length w_46 6;
          let ctor_arg_7 = pop_env w_46 in
          let ctor_arg_8 = pop_env w_46 in
          push_env w_46 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_8; ctor_arg_7 ]);
          assert_env_length w_46 5;
          drop_n w_46 5 1;
          assert_env_length w_46 4;
          drop_n w_46 4 1;
          assert_env_length w_46 3;
          drop_n w_46 3 1;
          assert_env_length w_46 2;
          drop_n w_46 2 1;
          assert_env_length w_46 1;
          return_n w_46 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (46)")
    46;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 5;
      let cond_5 = resolve w_45 (Source.E 4) in
      ignore (pop_env w_45);
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_45 4;
        push_env w_45 (Dynarray.get w_45.state.e 2);
        assert_env_length w_45 5;
        drop_n w_45 5 1;
        assert_env_length w_45 4;
        drop_n w_45 4 1;
        assert_env_length w_45 3;
        drop_n w_45 3 1;
        assert_env_length w_45 2;
        drop_n w_45 2 1;
        assert_env_length w_45 1;
        return_n w_45 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_45 4;
        push_env w_45 (Dynarray.get w_45.state.e 2);
        w_45.state.c <- pc_to_exp (int_to_pc 46)))
    47;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 6;
      let x0_11 = resolve w_44 (Source.E 4) in
      let x1_11 = resolve w_44 (Source.E 5) in
      ignore (pop_env w_44);
      ignore (pop_env w_44);
      push_env w_44 (Memo.from_int (if Word.get_value (fst x0_11) = Word.get_value (fst x1_11) then 1 else 0));
      w_44.state.c <- pc_to_exp (int_to_pc 47))
    48;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 5;
      let cond_4 = resolve w_43 (Source.E 4) in
      ignore (pop_env w_43);
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_43 4;
        push_env w_43 (Memo.from_int 0);
        assert_env_length w_43 5;
        let ctor_arg_9 = pop_env w_43 in
        push_env w_43 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_9 ]);
        assert_env_length w_43 5;
        drop_n w_43 5 1;
        assert_env_length w_43 4;
        drop_n w_43 4 1;
        assert_env_length w_43 3;
        drop_n w_43 3 1;
        assert_env_length w_43 2;
        drop_n w_43 2 1;
        assert_env_length w_43 1;
        return_n w_43 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_43 4;
        push_env w_43 (Dynarray.get w_43.state.e 3);
        assert_env_length w_43 5;
        push_env w_43 (Memo.from_int 1);
        w_43.state.c <- pc_to_exp (int_to_pc 48)))
    49;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 6;
      let x0_10 = resolve w_42 (Source.E 4) in
      let x1_10 = resolve w_42 (Source.E 5) in
      ignore (pop_env w_42);
      ignore (pop_env w_42);
      push_env w_42 (Memo.from_int (if Word.get_value (fst x0_10) = Word.get_value (fst x1_10) then 1 else 0));
      w_42.state.c <- pc_to_exp (int_to_pc 49))
    50;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 5;
      let cond_9 = resolve w_56 (Source.E 4) in
      ignore (pop_env w_56);
      if Word.get_value (fst cond_9) <> 0 then (
        assert_env_length w_56 4;
        push_env w_56 (Dynarray.get w_56.state.e 1);
        assert_env_length w_56 5;
        drop_n w_56 5 1;
        assert_env_length w_56 4;
        drop_n w_56 4 1;
        assert_env_length w_56 3;
        drop_n w_56 3 1;
        assert_env_length w_56 2;
        drop_n w_56 2 1;
        assert_env_length w_56 1;
        return_n w_56 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_56 4;
        push_env w_56 (Dynarray.get w_56.state.e 2);
        assert_env_length w_56 5;
        push_env w_56 (Dynarray.get w_56.state.e 1);
        assert_env_length w_56 6;
        let ctor_arg_16 = pop_env w_56 in
        let ctor_arg_17 = pop_env w_56 in
        push_env w_56 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_17; ctor_arg_16 ]);
        assert_env_length w_56 5;
        drop_n w_56 5 1;
        assert_env_length w_56 4;
        drop_n w_56 4 1;
        assert_env_length w_56 3;
        drop_n w_56 3 1;
        assert_env_length w_56 2;
        drop_n w_56 2 1;
        assert_env_length w_56 1;
        return_n w_56 1 (pc_to_exp (int_to_pc 0))))
    51;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 6;
      let x0_16 = resolve w_55 (Source.E 4) in
      let x1_16 = resolve w_55 (Source.E 5) in
      ignore (pop_env w_55);
      ignore (pop_env w_55);
      push_env w_55 (Memo.from_int (if Word.get_value (fst x0_16) = Word.get_value (fst x1_16) then 1 else 0));
      w_55.state.c <- pc_to_exp (int_to_pc 51))
    52;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 5;
      let cond_8 = resolve w_54 (Source.E 4) in
      ignore (pop_env w_54);
      if Word.get_value (fst cond_8) <> 0 then (
        assert_env_length w_54 4;
        push_env w_54 (Memo.from_int 0);
        assert_env_length w_54 5;
        let ctor_arg_18 = pop_env w_54 in
        push_env w_54 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_18 ]);
        assert_env_length w_54 5;
        drop_n w_54 5 1;
        assert_env_length w_54 4;
        drop_n w_54 4 1;
        assert_env_length w_54 3;
        drop_n w_54 3 1;
        assert_env_length w_54 2;
        drop_n w_54 2 1;
        assert_env_length w_54 1;
        return_n w_54 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_54 4;
        push_env w_54 (Dynarray.get w_54.state.e 3);
        assert_env_length w_54 5;
        push_env w_54 (Memo.from_int 1);
        w_54.state.c <- pc_to_exp (int_to_pc 52)))
    53;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 6;
      let x0_15 = resolve w_53 (Source.E 4) in
      let x1_15 = resolve w_53 (Source.E 5) in
      ignore (pop_env w_53);
      ignore (pop_env w_53);
      push_env w_53 (Memo.from_int (if Word.get_value (fst x0_15) = Word.get_value (fst x1_15) then 1 else 0));
      w_53.state.c <- pc_to_exp (int_to_pc 53))
    54;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 4;
      let last_19 = Source.E 3 in
      let x_19 = resolve w_52 last_19 in
      match Word.get_value (fst x_19) with
      | 3 (* tag_Const *) ->
          let splits_35 = Memo.splits (snd x_19) in
          let split0_35 = List.nth splits_35 0 in
          ignore (pop_env w_52);
          push_env w_52 split0_35;
          assert_env_length w_52 4;
          push_env w_52 (Dynarray.get w_52.state.e 3);
          assert_env_length w_52 5;
          push_env w_52 (Memo.from_int 0);
          w_52.state.c <- pc_to_exp (int_to_pc 54)
      | 5 (* tag_Add *) ->
          let splits_36 = Memo.splits (snd x_19) in
          let split0_36 = List.nth splits_36 0 in
          let split1_17 = List.nth splits_36 1 in
          ignore (pop_env w_52);
          push_env w_52 split0_36;
          push_env w_52 split1_17;
          assert_env_length w_52 5;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          assert_env_length w_52 6;
          push_env w_52 (Dynarray.get w_52.state.e 3);
          assert_env_length w_52 7;
          let ctor_arg_19 = pop_env w_52 in
          let ctor_arg_20 = pop_env w_52 in
          push_env w_52 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_20; ctor_arg_19 ]);
          assert_env_length w_52 6;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          assert_env_length w_52 7;
          push_env w_52 (Dynarray.get w_52.state.e 4);
          assert_env_length w_52 8;
          let ctor_arg_21 = pop_env w_52 in
          let ctor_arg_22 = pop_env w_52 in
          push_env w_52 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_22; ctor_arg_21 ]);
          assert_env_length w_52 7;
          let ctor_arg_23 = pop_env w_52 in
          let ctor_arg_24 = pop_env w_52 in
          push_env w_52 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_24; ctor_arg_23 ]);
          assert_env_length w_52 6;
          drop_n w_52 6 2;
          assert_env_length w_52 4;
          drop_n w_52 4 1;
          assert_env_length w_52 3;
          drop_n w_52 3 1;
          assert_env_length w_52 2;
          drop_n w_52 2 1;
          assert_env_length w_52 1;
          return_n w_52 1 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Mul *) ->
          let splits_37 = Memo.splits (snd x_19) in
          let split0_37 = List.nth splits_37 0 in
          let split1_18 = List.nth splits_37 1 in
          ignore (pop_env w_52);
          push_env w_52 split0_37;
          push_env w_52 split1_18;
          assert_env_length w_52 5;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          assert_env_length w_52 6;
          push_env w_52 (Dynarray.get w_52.state.e 3);
          assert_env_length w_52 7;
          push_env w_52 (Dynarray.get w_52.state.e 4);
          assert_env_length w_52 8;
          let ctor_arg_25 = pop_env w_52 in
          let ctor_arg_26 = pop_env w_52 in
          push_env w_52 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_26; ctor_arg_25 ]);
          assert_env_length w_52 7;
          let ctor_arg_27 = pop_env w_52 in
          let ctor_arg_28 = pop_env w_52 in
          push_env w_52 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_28; ctor_arg_27 ]);
          assert_env_length w_52 6;
          drop_n w_52 6 2;
          assert_env_length w_52 4;
          drop_n w_52 4 1;
          assert_env_length w_52 3;
          drop_n w_52 3 1;
          assert_env_length w_52 2;
          drop_n w_52 2 1;
          assert_env_length w_52 1;
          return_n w_52 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_52);
          assert_env_length w_52 3;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          assert_env_length w_52 4;
          push_env w_52 (Dynarray.get w_52.state.e 2);
          assert_env_length w_52 5;
          let ctor_arg_29 = pop_env w_52 in
          let ctor_arg_30 = pop_env w_52 in
          push_env w_52 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_30; ctor_arg_29 ]);
          assert_env_length w_52 4;
          drop_n w_52 4 1;
          assert_env_length w_52 3;
          drop_n w_52 3 1;
          assert_env_length w_52 2;
          drop_n w_52 2 1;
          assert_env_length w_52 1;
          return_n w_52 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (55)")
    55;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 4;
      let last_17 = Source.E 3 in
      let x_17 = resolve w_41 last_17 in
      match Word.get_value (fst x_17) with
      | 3 (* tag_Const *) ->
          let splits_32 = Memo.splits (snd x_17) in
          let split0_32 = List.nth splits_32 0 in
          ignore (pop_env w_41);
          push_env w_41 split0_32;
          assert_env_length w_41 4;
          push_env w_41 (Dynarray.get w_41.state.e 3);
          assert_env_length w_41 5;
          push_env w_41 (Memo.from_int 0);
          w_41.state.c <- pc_to_exp (int_to_pc 50)
      | 5 (* tag_Add *) ->
          let splits_34 = Memo.splits (snd x_17) in
          let split0_34 = List.nth splits_34 0 in
          let split1_16 = List.nth splits_34 1 in
          ignore (pop_env w_41);
          push_env w_41 split0_34;
          push_env w_41 split1_16;
          assert_env_length w_41 5;
          push_env w_41 (Dynarray.get w_41.state.e 3);
          assert_env_length w_41 6;
          push_env w_41 (Dynarray.get w_41.state.e 0);
          assert_env_length w_41 7;
          let ctor_arg_10 = pop_env w_41 in
          let ctor_arg_11 = pop_env w_41 in
          push_env w_41 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_11; ctor_arg_10 ]);
          assert_env_length w_41 6;
          push_env w_41 (Dynarray.get w_41.state.e 4);
          assert_env_length w_41 7;
          push_env w_41 (Dynarray.get w_41.state.e 0);
          assert_env_length w_41 8;
          let ctor_arg_12 = pop_env w_41 in
          let ctor_arg_13 = pop_env w_41 in
          push_env w_41 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_13; ctor_arg_12 ]);
          assert_env_length w_41 7;
          let ctor_arg_14 = pop_env w_41 in
          let ctor_arg_15 = pop_env w_41 in
          push_env w_41 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_15; ctor_arg_14 ]);
          assert_env_length w_41 6;
          drop_n w_41 6 2;
          assert_env_length w_41 4;
          drop_n w_41 4 1;
          assert_env_length w_41 3;
          drop_n w_41 3 1;
          assert_env_length w_41 2;
          drop_n w_41 2 1;
          assert_env_length w_41 1;
          return_n w_41 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_41);
          assert_env_length w_41 3;
          push_env w_41 (Dynarray.get w_41.state.e 2);
          w_41.state.c <- pc_to_exp (int_to_pc 55)
      | _ -> failwith "unreachable (56)")
    56;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 2;
      let cond_10 = resolve w_57 (Source.E 1) in
      ignore (pop_env w_57);
      if Word.get_value (fst cond_10) <> 0 then (
        assert_env_length w_57 1;
        push_env w_57 (Dynarray.get w_57.state.e 0);
        assert_env_length w_57 2;
        drop_n w_57 2 1;
        assert_env_length w_57 1;
        return_n w_57 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_57 1;
        push_env w_57 (Dynarray.get w_57.state.e 0);
        assert_env_length w_57 2;
        ignore (env_call w_57 [] 1);
        w_57.state.c <- pc_to_exp (int_to_pc 15)))
    57;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 2;
      let x0_17 = resolve w_58 (Source.E 0) in
      let x1_17 = resolve w_58 (Source.E 1) in
      ignore (pop_env w_58);
      ignore (pop_env w_58);
      push_env w_58 (Memo.from_int (Word.get_value (fst x0_17) + Word.get_value (fst x1_17)));
      assert_env_length w_58 1;
      drop_n w_58 1 0;
      assert_env_length w_58 1;
      return_n w_58 1 (pc_to_exp (int_to_pc 0)))
    58;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 2;
      let x0_18 = resolve w_59 (Source.E 0) in
      let x1_18 = resolve w_59 (Source.E 1) in
      ignore (pop_env w_59);
      ignore (pop_env w_59);
      push_env w_59 (Memo.from_int (Word.get_value (fst x0_18) * Word.get_value (fst x1_18)));
      assert_env_length w_59 1;
      drop_n w_59 1 0;
      assert_env_length w_59 1;
      return_n w_59 1 (pc_to_exp (int_to_pc 0)))
    59;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 4;
      let cond_11 = resolve w_61 (Source.E 3) in
      ignore (pop_env w_61);
      if Word.get_value (fst cond_11) <> 0 then (
        assert_env_length w_61 3;
        push_env w_61 (Dynarray.get w_61.state.e 0);
        assert_env_length w_61 4;
        push_env w_61 (Dynarray.get w_61.state.e 1);
        assert_env_length w_61 5;
        ignore (env_call w_61 [] 2);
        w_61.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_61 3;
        push_env w_61 (Dynarray.get w_61.state.e 2);
        assert_env_length w_61 4;
        drop_n w_61 4 1;
        assert_env_length w_61 3;
        drop_n w_61 3 1;
        assert_env_length w_61 2;
        drop_n w_61 2 1;
        assert_env_length w_61 1;
        drop_n w_61 1 0;
        assert_env_length w_61 1;
        drop_n w_61 1 0;
        assert_env_length w_61 1;
        return_n w_61 1 (pc_to_exp (int_to_pc 0))))
    60;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 5;
      let x0_19 = resolve w_60 (Source.E 3) in
      let x1_19 = resolve w_60 (Source.E 4) in
      ignore (pop_env w_60);
      ignore (pop_env w_60);
      push_env w_60 (Memo.from_int (if Word.get_value (fst x0_19) = Word.get_value (fst x1_19) then 1 else 0));
      w_60.state.c <- pc_to_exp (int_to_pc 60))
    61;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 4;
      let cond_12 = resolve w_63 (Source.E 3) in
      ignore (pop_env w_63);
      if Word.get_value (fst cond_12) <> 0 then (
        assert_env_length w_63 3;
        push_env w_63 (Dynarray.get w_63.state.e 0);
        assert_env_length w_63 4;
        push_env w_63 (Dynarray.get w_63.state.e 1);
        assert_env_length w_63 5;
        ignore (env_call w_63 [] 2);
        w_63.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_63 3;
        push_env w_63 (Dynarray.get w_63.state.e 2);
        assert_env_length w_63 4;
        drop_n w_63 4 1;
        assert_env_length w_63 3;
        drop_n w_63 3 1;
        assert_env_length w_63 2;
        drop_n w_63 2 1;
        assert_env_length w_63 1;
        drop_n w_63 1 0;
        assert_env_length w_63 1;
        drop_n w_63 1 0;
        assert_env_length w_63 1;
        return_n w_63 1 (pc_to_exp (int_to_pc 0))))
    62;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 5;
      let x0_20 = resolve w_62 (Source.E 3) in
      let x1_20 = resolve w_62 (Source.E 4) in
      ignore (pop_env w_62);
      ignore (pop_env w_62);
      push_env w_62 (Memo.from_int (if Word.get_value (fst x0_20) = Word.get_value (fst x1_20) then 1 else 0));
      w_62.state.c <- pc_to_exp (int_to_pc 62))
    63;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 6;
      let x0_22 = resolve w_69 (Source.E 4) in
      let x1_22 = resolve w_69 (Source.E 5) in
      ignore (pop_env w_69);
      ignore (pop_env w_69);
      push_env w_69 (Memo.from_int (Word.get_value (fst x0_22) + Word.get_value (fst x1_22)));
      assert_env_length w_69 5;
      let ctor_arg_37 = pop_env w_69 in
      push_env w_69 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_37 ]);
      assert_env_length w_69 5;
      drop_n w_69 5 1;
      assert_env_length w_69 4;
      drop_n w_69 4 1;
      assert_env_length w_69 3;
      drop_n w_69 3 1;
      assert_env_length w_69 2;
      drop_n w_69 2 1;
      assert_env_length w_69 1;
      drop_n w_69 1 0;
      assert_env_length w_69 1;
      return_n w_69 1 (pc_to_exp (int_to_pc 0)))
    64;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 4;
      let last_21 = Source.E 3 in
      let x_21 = resolve w_68 last_21 in
      match Word.get_value (fst x_21) with
      | 3 (* tag_Const *) ->
          let splits_39 = Memo.splits (snd x_21) in
          let split0_39 = List.nth splits_39 0 in
          ignore (pop_env w_68);
          push_env w_68 split0_39;
          assert_env_length w_68 4;
          push_env w_68 (Dynarray.get w_68.state.e 2);
          assert_env_length w_68 5;
          push_env w_68 (Dynarray.get w_68.state.e 3);
          w_68.state.c <- pc_to_exp (int_to_pc 64)
      | _ ->
          ignore (pop_env w_68);
          assert_env_length w_68 3;
          push_env w_68 (Dynarray.get w_68.state.e 0);
          assert_env_length w_68 4;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          assert_env_length w_68 5;
          let ctor_arg_38 = pop_env w_68 in
          let ctor_arg_39 = pop_env w_68 in
          push_env w_68 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_39; ctor_arg_38 ]);
          assert_env_length w_68 4;
          drop_n w_68 4 1;
          assert_env_length w_68 3;
          drop_n w_68 3 1;
          assert_env_length w_68 2;
          drop_n w_68 2 1;
          assert_env_length w_68 1;
          drop_n w_68 1 0;
          assert_env_length w_68 1;
          return_n w_68 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (65)")
    65;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 4;
      let cond_14 = resolve w_67 (Source.E 3) in
      ignore (pop_env w_67);
      if Word.get_value (fst cond_14) <> 0 then (
        assert_env_length w_67 3;
        push_env w_67 (Dynarray.get w_67.state.e 1);
        assert_env_length w_67 4;
        drop_n w_67 4 1;
        assert_env_length w_67 3;
        drop_n w_67 3 1;
        assert_env_length w_67 2;
        drop_n w_67 2 1;
        assert_env_length w_67 1;
        drop_n w_67 1 0;
        assert_env_length w_67 1;
        return_n w_67 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_67 3;
        push_env w_67 (Dynarray.get w_67.state.e 1);
        w_67.state.c <- pc_to_exp (int_to_pc 65)))
    66;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 5;
      let x0_21 = resolve w_66 (Source.E 3) in
      let x1_21 = resolve w_66 (Source.E 4) in
      ignore (pop_env w_66);
      ignore (pop_env w_66);
      push_env w_66 (Memo.from_int (if Word.get_value (fst x0_21) = Word.get_value (fst x1_21) then 1 else 0));
      w_66.state.c <- pc_to_exp (int_to_pc 66))
    67;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 10;
      let x0_23 = resolve w_73 (Source.E 8) in
      let x1_23 = resolve w_73 (Source.E 9) in
      ignore (pop_env w_73);
      ignore (pop_env w_73);
      push_env w_73 (Memo.from_int (Word.get_value (fst x0_23) + Word.get_value (fst x1_23)));
      assert_env_length w_73 9;
      let ctor_arg_40 = pop_env w_73 in
      push_env w_73 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_40 ]);
      assert_env_length w_73 9;
      push_env w_73 (Dynarray.get w_73.state.e 3);
      assert_env_length w_73 10;
      push_env w_73 (Dynarray.get w_73.state.e 5);
      assert_env_length w_73 11;
      let ctor_arg_41 = pop_env w_73 in
      let ctor_arg_42 = pop_env w_73 in
      push_env w_73 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_42; ctor_arg_41 ]);
      assert_env_length w_73 10;
      let ctor_arg_43 = pop_env w_73 in
      let ctor_arg_44 = pop_env w_73 in
      push_env w_73 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_44; ctor_arg_43 ]);
      assert_env_length w_73 9;
      drop_n w_73 9 1;
      assert_env_length w_73 8;
      drop_n w_73 8 1;
      assert_env_length w_73 7;
      drop_n w_73 7 2;
      assert_env_length w_73 5;
      drop_n w_73 5 2;
      assert_env_length w_73 3;
      drop_n w_73 3 1;
      assert_env_length w_73 2;
      drop_n w_73 2 1;
      assert_env_length w_73 1;
      drop_n w_73 1 0;
      assert_env_length w_73 1;
      return_n w_73 1 (pc_to_exp (int_to_pc 0)))
    68;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 8;
      let last_24 = Source.E 7 in
      let x_24 = resolve w_72 last_24 in
      match Word.get_value (fst x_24) with
      | 3 (* tag_Const *) ->
          let splits_43 = Memo.splits (snd x_24) in
          let split0_43 = List.nth splits_43 0 in
          ignore (pop_env w_72);
          push_env w_72 split0_43;
          assert_env_length w_72 8;
          push_env w_72 (Dynarray.get w_72.state.e 6);
          assert_env_length w_72 9;
          push_env w_72 (Dynarray.get w_72.state.e 7);
          w_72.state.c <- pc_to_exp (int_to_pc 68)
      | _ ->
          ignore (pop_env w_72);
          assert_env_length w_72 7;
          push_env w_72 (Dynarray.get w_72.state.e 2);
          assert_env_length w_72 8;
          push_env w_72 (Dynarray.get w_72.state.e 3);
          assert_env_length w_72 9;
          let ctor_arg_45 = pop_env w_72 in
          let ctor_arg_46 = pop_env w_72 in
          push_env w_72 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_46; ctor_arg_45 ]);
          assert_env_length w_72 8;
          push_env w_72 (Dynarray.get w_72.state.e 4);
          assert_env_length w_72 9;
          let ctor_arg_47 = pop_env w_72 in
          let ctor_arg_48 = pop_env w_72 in
          push_env w_72 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_48; ctor_arg_47 ]);
          assert_env_length w_72 8;
          push_env w_72 (Dynarray.get w_72.state.e 5);
          assert_env_length w_72 9;
          let ctor_arg_49 = pop_env w_72 in
          let ctor_arg_50 = pop_env w_72 in
          push_env w_72 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_50; ctor_arg_49 ]);
          assert_env_length w_72 8;
          drop_n w_72 8 1;
          assert_env_length w_72 7;
          drop_n w_72 7 2;
          assert_env_length w_72 5;
          drop_n w_72 5 2;
          assert_env_length w_72 3;
          drop_n w_72 3 1;
          assert_env_length w_72 2;
          drop_n w_72 2 1;
          assert_env_length w_72 1;
          drop_n w_72 1 0;
          assert_env_length w_72 1;
          return_n w_72 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (69)")
    69;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 7;
      let last_23 = Source.E 6 in
      let x_23 = resolve w_71 last_23 in
      match Word.get_value (fst x_23) with
      | 3 (* tag_Const *) ->
          let splits_42 = Memo.splits (snd x_23) in
          let split0_42 = List.nth splits_42 0 in
          ignore (pop_env w_71);
          push_env w_71 split0_42;
          assert_env_length w_71 7;
          push_env w_71 (Dynarray.get w_71.state.e 4);
          w_71.state.c <- pc_to_exp (int_to_pc 69)
      | _ ->
          ignore (pop_env w_71);
          assert_env_length w_71 6;
          push_env w_71 (Dynarray.get w_71.state.e 2);
          assert_env_length w_71 7;
          push_env w_71 (Dynarray.get w_71.state.e 3);
          assert_env_length w_71 8;
          let ctor_arg_51 = pop_env w_71 in
          let ctor_arg_52 = pop_env w_71 in
          push_env w_71 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_52; ctor_arg_51 ]);
          assert_env_length w_71 7;
          push_env w_71 (Dynarray.get w_71.state.e 4);
          assert_env_length w_71 8;
          let ctor_arg_53 = pop_env w_71 in
          let ctor_arg_54 = pop_env w_71 in
          push_env w_71 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_54; ctor_arg_53 ]);
          assert_env_length w_71 7;
          push_env w_71 (Dynarray.get w_71.state.e 5);
          assert_env_length w_71 8;
          let ctor_arg_55 = pop_env w_71 in
          let ctor_arg_56 = pop_env w_71 in
          push_env w_71 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_56; ctor_arg_55 ]);
          assert_env_length w_71 7;
          drop_n w_71 7 2;
          assert_env_length w_71 5;
          drop_n w_71 5 2;
          assert_env_length w_71 3;
          drop_n w_71 3 1;
          assert_env_length w_71 2;
          drop_n w_71 2 1;
          assert_env_length w_71 1;
          drop_n w_71 1 0;
          assert_env_length w_71 1;
          return_n w_71 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (70)")
    70;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 5;
      let last_22 = Source.E 4 in
      let x_22 = resolve w_70 last_22 in
      match Word.get_value (fst x_22) with
      | 5 (* tag_Add *) ->
          let splits_41 = Memo.splits (snd x_22) in
          let split0_41 = List.nth splits_41 0 in
          let split1_20 = List.nth splits_41 1 in
          ignore (pop_env w_70);
          push_env w_70 split0_41;
          push_env w_70 split1_20;
          assert_env_length w_70 6;
          push_env w_70 (Dynarray.get w_70.state.e 2);
          w_70.state.c <- pc_to_exp (int_to_pc 70)
      | _ ->
          ignore (pop_env w_70);
          assert_env_length w_70 4;
          push_env w_70 (Dynarray.get w_70.state.e 0);
          assert_env_length w_70 5;
          push_env w_70 (Dynarray.get w_70.state.e 1);
          assert_env_length w_70 6;
          let ctor_arg_57 = pop_env w_70 in
          let ctor_arg_58 = pop_env w_70 in
          push_env w_70 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_58; ctor_arg_57 ]);
          assert_env_length w_70 5;
          drop_n w_70 5 2;
          assert_env_length w_70 3;
          drop_n w_70 3 1;
          assert_env_length w_70 2;
          drop_n w_70 2 1;
          assert_env_length w_70 1;
          drop_n w_70 1 0;
          assert_env_length w_70 1;
          return_n w_70 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (71)")
    71;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 4;
      let cond_15 = resolve w_76 (Source.E 3) in
      ignore (pop_env w_76);
      if Word.get_value (fst cond_15) <> 0 then (
        assert_env_length w_76 3;
        push_env w_76 (Dynarray.get w_76.state.e 0);
        assert_env_length w_76 4;
        drop_n w_76 4 1;
        assert_env_length w_76 3;
        drop_n w_76 3 1;
        assert_env_length w_76 2;
        drop_n w_76 2 1;
        assert_env_length w_76 1;
        drop_n w_76 1 0;
        assert_env_length w_76 1;
        return_n w_76 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_76 3;
        push_env w_76 (Dynarray.get w_76.state.e 1);
        assert_env_length w_76 4;
        push_env w_76 (Dynarray.get w_76.state.e 0);
        assert_env_length w_76 5;
        let ctor_arg_59 = pop_env w_76 in
        let ctor_arg_60 = pop_env w_76 in
        push_env w_76 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_60; ctor_arg_59 ]);
        assert_env_length w_76 4;
        drop_n w_76 4 1;
        assert_env_length w_76 3;
        drop_n w_76 3 1;
        assert_env_length w_76 2;
        drop_n w_76 2 1;
        assert_env_length w_76 1;
        drop_n w_76 1 0;
        assert_env_length w_76 1;
        return_n w_76 1 (pc_to_exp (int_to_pc 0))))
    72;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 5;
      let x0_24 = resolve w_75 (Source.E 3) in
      let x1_24 = resolve w_75 (Source.E 4) in
      ignore (pop_env w_75);
      ignore (pop_env w_75);
      push_env w_75 (Memo.from_int (if Word.get_value (fst x0_24) = Word.get_value (fst x1_24) then 1 else 0));
      w_75.state.c <- pc_to_exp (int_to_pc 72))
    73;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 3;
      let last_25 = Source.E 2 in
      let x_25 = resolve w_74 last_25 in
      match Word.get_value (fst x_25) with
      | 3 (* tag_Const *) ->
          let splits_44 = Memo.splits (snd x_25) in
          let split0_44 = List.nth splits_44 0 in
          ignore (pop_env w_74);
          push_env w_74 split0_44;
          assert_env_length w_74 3;
          push_env w_74 (Dynarray.get w_74.state.e 2);
          assert_env_length w_74 4;
          push_env w_74 (Memo.from_int 0);
          w_74.state.c <- pc_to_exp (int_to_pc 73)
      | 5 (* tag_Add *) ->
          let splits_45 = Memo.splits (snd x_25) in
          let split0_45 = List.nth splits_45 0 in
          let split1_21 = List.nth splits_45 1 in
          ignore (pop_env w_74);
          push_env w_74 split0_45;
          push_env w_74 split1_21;
          assert_env_length w_74 4;
          push_env w_74 (Dynarray.get w_74.state.e 0);
          assert_env_length w_74 5;
          push_env w_74 (Dynarray.get w_74.state.e 2);
          assert_env_length w_74 6;
          let ctor_arg_61 = pop_env w_74 in
          let ctor_arg_62 = pop_env w_74 in
          push_env w_74 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_62; ctor_arg_61 ]);
          assert_env_length w_74 5;
          push_env w_74 (Dynarray.get w_74.state.e 3);
          assert_env_length w_74 6;
          let ctor_arg_63 = pop_env w_74 in
          let ctor_arg_64 = pop_env w_74 in
          push_env w_74 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_64; ctor_arg_63 ]);
          assert_env_length w_74 5;
          drop_n w_74 5 2;
          assert_env_length w_74 3;
          drop_n w_74 3 1;
          assert_env_length w_74 2;
          drop_n w_74 2 1;
          assert_env_length w_74 1;
          drop_n w_74 1 0;
          assert_env_length w_74 1;
          return_n w_74 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_74);
          assert_env_length w_74 2;
          push_env w_74 (Dynarray.get w_74.state.e 0);
          assert_env_length w_74 3;
          push_env w_74 (Dynarray.get w_74.state.e 1);
          assert_env_length w_74 4;
          let ctor_arg_65 = pop_env w_74 in
          let ctor_arg_66 = pop_env w_74 in
          push_env w_74 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_66; ctor_arg_65 ]);
          assert_env_length w_74 3;
          drop_n w_74 3 1;
          assert_env_length w_74 2;
          drop_n w_74 2 1;
          assert_env_length w_74 1;
          drop_n w_74 1 0;
          assert_env_length w_74 1;
          return_n w_74 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (74)")
    74;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 3;
      let last_20 = Source.E 2 in
      let x_20 = resolve w_65 last_20 in
      match Word.get_value (fst x_20) with
      | 3 (* tag_Const *) ->
          let splits_38 = Memo.splits (snd x_20) in
          let split0_38 = List.nth splits_38 0 in
          ignore (pop_env w_65);
          push_env w_65 split0_38;
          assert_env_length w_65 3;
          push_env w_65 (Dynarray.get w_65.state.e 2);
          assert_env_length w_65 4;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 67)
      | 5 (* tag_Add *) ->
          let splits_40 = Memo.splits (snd x_20) in
          let split0_40 = List.nth splits_40 0 in
          let split1_19 = List.nth splits_40 1 in
          ignore (pop_env w_65);
          push_env w_65 split0_40;
          push_env w_65 split1_19;
          assert_env_length w_65 4;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 71)
      | _ ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 74)
      | _ -> failwith "unreachable (75)")
    75;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 3;
      let cond_13 = resolve w_64 (Source.E 2) in
      ignore (pop_env w_64);
      if Word.get_value (fst cond_13) <> 0 then (
        assert_env_length w_64 2;
        push_env w_64 (Memo.from_int 2);
        assert_env_length w_64 3;
        let ctor_arg_67 = pop_env w_64 in
        push_env w_64 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_67 ]);
        assert_env_length w_64 3;
        push_env w_64 (Dynarray.get w_64.state.e 0);
        assert_env_length w_64 4;
        let ctor_arg_68 = pop_env w_64 in
        let ctor_arg_69 = pop_env w_64 in
        push_env w_64 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_69; ctor_arg_68 ]);
        assert_env_length w_64 3;
        drop_n w_64 3 1;
        assert_env_length w_64 2;
        drop_n w_64 2 1;
        assert_env_length w_64 1;
        drop_n w_64 1 0;
        assert_env_length w_64 1;
        return_n w_64 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_64 2;
        push_env w_64 (Dynarray.get w_64.state.e 0);
        w_64.state.c <- pc_to_exp (int_to_pc 75)))
    76;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 3;
      let cond_17 = resolve w_80 (Source.E 2) in
      ignore (pop_env w_80);
      if Word.get_value (fst cond_17) <> 0 then (
        assert_env_length w_80 2;
        push_env w_80 (Memo.from_int 1);
        assert_env_length w_80 3;
        drop_n w_80 3 1;
        assert_env_length w_80 2;
        drop_n w_80 2 1;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        return_n w_80 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_80 2;
        push_env w_80 (Memo.from_int 0);
        assert_env_length w_80 3;
        drop_n w_80 3 1;
        assert_env_length w_80 2;
        drop_n w_80 2 1;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        drop_n w_80 1 0;
        assert_env_length w_80 1;
        return_n w_80 1 (pc_to_exp (int_to_pc 0))))
    77;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 4;
      let x0_26 = resolve w_79 (Source.E 2) in
      let x1_26 = resolve w_79 (Source.E 3) in
      ignore (pop_env w_79);
      ignore (pop_env w_79);
      push_env w_79 (Memo.from_int (if Word.get_value (fst x0_26) > Word.get_value (fst x1_26) then 1 else 0));
      w_79.state.c <- pc_to_exp (int_to_pc 77))
    78;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 4;
      let x0_27 = resolve w_81 (Source.E 2) in
      let x1_27 = resolve w_81 (Source.E 3) in
      ignore (pop_env w_81);
      ignore (pop_env w_81);
      push_env w_81 (Memo.from_int (Word.get_value (fst x0_27) - Word.get_value (fst x1_27)));
      assert_env_length w_81 3;
      drop_n w_81 3 1;
      assert_env_length w_81 2;
      drop_n w_81 2 1;
      assert_env_length w_81 1;
      drop_n w_81 1 0;
      assert_env_length w_81 1;
      drop_n w_81 1 0;
      assert_env_length w_81 1;
      drop_n w_81 1 0;
      assert_env_length w_81 1;
      drop_n w_81 1 0;
      assert_env_length w_81 1;
      return_n w_81 1 (pc_to_exp (int_to_pc 0)))
    79;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 3;
      let cond_16 = resolve w_78 (Source.E 2) in
      ignore (pop_env w_78);
      if Word.get_value (fst cond_16) <> 0 then (
        assert_env_length w_78 2;
        push_env w_78 (Memo.from_int 0);
        assert_env_length w_78 3;
        push_env w_78 (Memo.from_int 1);
        w_78.state.c <- pc_to_exp (int_to_pc 79))
      else (
        assert_env_length w_78 2;
        push_env w_78 (Dynarray.get w_78.state.e 0);
        assert_env_length w_78 3;
        push_env w_78 (Dynarray.get w_78.state.e 1);
        w_78.state.c <- pc_to_exp (int_to_pc 78)))
    80;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 4;
      let x0_25 = resolve w_77 (Source.E 2) in
      let x1_25 = resolve w_77 (Source.E 3) in
      ignore (pop_env w_77);
      ignore (pop_env w_77);
      push_env w_77 (Memo.from_int (if Word.get_value (fst x0_25) < Word.get_value (fst x1_25) then 1 else 0));
      w_77.state.c <- pc_to_exp (int_to_pc 80))
    81;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 1;
  Words.set_constructor_degree 3 0;
  Words.set_constructor_degree 4 0;
  Words.set_constructor_degree 5 (-1);
  Words.set_constructor_degree 6 (-1);
  Words.set_constructor_degree 7 (-2);
  Words.set_constructor_degree 8 (-1);
  Words.set_constructor_degree 9 (-2);
  Words.set_constructor_degree 10 (-2);
  Words.set_constructor_degree 11 (-1);
  Words.set_constructor_degree 12 (-1);
  Words.set_constructor_degree 13 (-1);
  Words.set_constructor_degree 14 (-1);
  Words.set_constructor_degree 15 (-2);
  Words.set_constructor_degree 16 (-3);
  Words.set_constructor_degree 17 (-3);
  Words.set_constructor_degree 18 0;
  Words.set_constructor_degree 19 (-3);
  Words.set_constructor_degree 20 (-1);
  Words.set_constructor_degree 21 (-1);
  Words.set_constructor_degree 22 (-1);
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 (-2);
  Words.set_constructor_degree 25 (-1);
  Words.set_constructor_degree 26 (-1);
  Words.set_constructor_degree 27 (-2);
  Words.set_constructor_degree 28 (-1);
  Words.set_constructor_degree 29 (-1);
  Words.set_constructor_degree 30 0;
  Words.set_constructor_degree 31 (-1);
  Words.set_constructor_degree 32 (-2);
  Words.set_constructor_degree 33 (-2);
  Words.set_constructor_degree 34 (-2);
  Words.set_constructor_degree 35 (-1)
