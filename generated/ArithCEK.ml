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
let tag_Exp = 7
let tag_Log = 8
let tag_cont_1 = 9
let tag_cont_2 = 10
let tag_cont_3 = 11
let tag_cont_4 = 12
let tag_cont_5 = 13
let tag_cont_6 = 14
let tag_cont_7 = 15
let tag_cont_8 = 16
let tag_cont_9 = 17
let tag_cont_10 = 18
let tag_cont_11 = 19
let tag_cont_12 = 20
let tag_cont_13 = 21
let tag_cont_14 = 22
let tag_cont_15 = 23
let tag_cont_16 = 24
let tag_cont_17 = 25
let tag_cont_18 = 26
let tag_cont_19 = 27
let tag_cont_20 = 28
let tag_cont_21 = 29
let tag_cont_22 = 30
let tag_cont_23 = 31
let tag_cont_24 = 32
let tag_cont_25 = 33
let tag_cont_26 = 34
let tag_cont_27 = 35
let tag_cont_28 = 36
let tag_cont_29 = 37
let tag_cont_30 = 38
let tag_cont_31 = 39
let tag_cont_32 = 40
let tag_cont_33 = 41
let tag_cont_34 = 42
let tag_cont_35 = 43
let tag_cont_36 = 44
let tag_cont_37 = 45
let tag_cont_38 = 46
let tag_cont_39 = 47
let tag_cont_40 = 48
let tag_cont_41 = 49
let tag_cont_42 = 50
let tag_cont_43 = 51
let tag_cont_44 = 52
let tag_cont_45 = 53
let tag_cont_46 = 54
let tag_cont_47 = 55
let tag_cont_48 = 56

type var = X | Y

let rec from_ocaml_var x =
  match x with X -> Memo.appends [ Memo.from_constructor tag_X ] | Y -> Memo.appends [ Memo.from_constructor tag_Y ]

let rec to_ocaml_var x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with 1 (* tag_X *) -> X | 2 (* tag_Y *) -> Y | _ -> failwith "unreachable"

type expr = Const of int | Var of var | Add of expr * expr | Mul of expr * expr | Exp of expr | Log of expr

let rec from_ocaml_expr x =
  match x with
  | Const x0 -> Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int x0 ]
  | Var x0 -> Memo.appends [ Memo.from_constructor tag_Var; from_ocaml_var x0 ]
  | Add (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Add; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | Mul (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Mul; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | Exp x0 -> Memo.appends [ Memo.from_constructor tag_Exp; from_ocaml_expr x0 ]
  | Log x0 -> Memo.appends [ Memo.from_constructor tag_Log; from_ocaml_expr x0 ]

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
  | 7 (* tag_Exp *) ->
      let x0 = Memo.splits_1 t in
      Exp (to_ocaml_expr x0)
  | 8 (* tag_Log *) ->
      let x0 = Memo.splits_1 t in
      Log (to_ocaml_expr x0)
  | _ -> failwith "unreachable"

let rec var_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec compare_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec int_exp memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 6)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec int_log memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 12)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_eq_struct memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 16)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 25)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 27)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec simplify_aux memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 28)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec diffx memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 29)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 32)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_34 ->
      assert_env_length w_34 1;
      let hd_0, tl_0 = resolve w_34 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_34
      | 9 (* tag_cont_1 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 4;
          let keep_vals_20 = env_call w_34 [ 0; 1; 2 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_vals_20; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 3)
      | 10 (* tag_cont_2 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 35)
      | 11 (* tag_cont_3 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 36)
      | 12 (* tag_cont_4 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let keep_vals_21 = env_call w_34 [ 1 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_vals_21; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 1)
      | 13 (* tag_cont_5 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 5;
          let keep_vals_22 = env_call w_34 [ 2 ] 2 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_vals_22; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 16)
      | 14 (* tag_cont_6 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 5;
          let keep_vals_23 = env_call w_34 [ 2 ] 2 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_vals_23; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 16)
      | 15 (* tag_cont_7 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let keep_vals_24 = env_call w_34 [ 1 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_vals_24; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 25)
      | 16 (* tag_cont_8 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let keep_vals_25 = env_call w_34 [ 1 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_vals_25; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 25)
      | 17 (* tag_cont_9 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 0 tl_0;
          assert_env_length w_34 1;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          w_34.state.c <- pc_to_exp (int_to_pc 39)
      | 18 (* tag_cont_10 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 0 tl_0;
          assert_env_length w_34 1;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          w_34.state.c <- pc_to_exp (int_to_pc 42)
      | 19 (* tag_cont_11 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let keep_vals_28 = env_call w_34 [ 1 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_vals_28; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 25)
      | 20 (* tag_cont_12 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 4;
          let keep_vals_29 = env_call w_34 [ 1 ] 2 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_vals_29; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 27)
      | 21 (* tag_cont_13 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let keep_vals_30 = env_call w_34 [ 1 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_vals_30; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 29)
      | 22 (* tag_cont_14 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 4;
          let ctor_arg_8 = pop_env w_34 in
          let ctor_arg_9 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_9; ctor_arg_8 ]);
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 5;
          let keep_vals_31 = env_call w_34 [ 2; 3 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_vals_31; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 29)
      | 23 (* tag_cont_15 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          let ctor_arg_10 = pop_env w_34 in
          let ctor_arg_11 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_11; ctor_arg_10 ]);
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          return_n w_34 1 (pc_to_exp (int_to_pc 0))
      | 24 (* tag_cont_16 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let ctor_arg_12 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Log; ctor_arg_12 ]);
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          assert_env_length w_34 4;
          push_env w_34 (Memo.from_int 1);
          w_34.state.c <- pc_to_exp (int_to_pc 43)
      | 25 (* tag_cont_17 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 3 tl_0;
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 5;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 6;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 7;
          let keep_vals_32 = env_call w_34 [ 3 ] 3 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_vals_32; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 32)
      | 26 (* tag_cont_18 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 3 tl_0;
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 5;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 6;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 7;
          let keep_vals_33 = env_call w_34 [ 3 ] 3 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_vals_33; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 32)
      | 27 (* tag_cont_19 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 0 tl_0;
          assert_env_length w_34 1;
          ignore (env_call w_34 [] 1);
          w_34.state.c <- pc_to_exp (int_to_pc 6)
      | 28 (* tag_cont_20 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 0 tl_0;
          assert_env_length w_34 1;
          ignore (env_call w_34 [] 1);
          w_34.state.c <- pc_to_exp (int_to_pc 12)
      | 29 (* tag_cont_21 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 3 tl_0;
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 5;
          push_env w_34 (Dynarray.get w_34.state.e 3);
          w_34.state.c <- pc_to_exp (int_to_pc 60)
      | 30 (* tag_cont_22 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 61)
      | 31 (* tag_cont_23 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 62)
      | 32 (* tag_cont_24 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 63)
      | 33 (* tag_cont_25 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          w_34.state.c <- pc_to_exp (int_to_pc 71)
      | 34 (* tag_cont_26 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          w_34.state.c <- pc_to_exp (int_to_pc 87)
      | 35 (* tag_cont_27 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 0 tl_0;
          assert_env_length w_34 1;
          let ctor_arg_24 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_24 ]);
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          return_n w_34 1 (pc_to_exp (int_to_pc 0))
      | 36 (* tag_cont_28 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 0 tl_0;
          assert_env_length w_34 1;
          let ctor_arg_25 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_25 ]);
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          return_n w_34 1 (pc_to_exp (int_to_pc 0))
      | 37 (* tag_cont_29 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          assert_env_length w_34 4;
          ignore (env_call w_34 [] 2);
          w_34.state.c <- pc_to_exp (int_to_pc 16)
      | 38 (* tag_cont_30 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 88)
      | 39 (* tag_cont_31 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          let ctor_arg_26 = pop_env w_34 in
          let ctor_arg_27 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_27; ctor_arg_26 ]);
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          return_n w_34 1 (pc_to_exp (int_to_pc 0))
      | 40 (* tag_cont_32 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          let ctor_arg_28 = pop_env w_34 in
          let ctor_arg_29 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_29; ctor_arg_28 ]);
          assert_env_length w_34 2;
          let ctor_arg_30 = pop_env w_34 in
          let ctor_arg_31 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_31; ctor_arg_30 ]);
          assert_env_length w_34 1;
          drop_n w_34 1 0;
          assert_env_length w_34 1;
          return_n w_34 1 (pc_to_exp (int_to_pc 0))
      | 41 (* tag_cont_33 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 89)
      | 42 (* tag_cont_34 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 90)
      | 43 (* tag_cont_35 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          let keep_vals_43 = env_call w_34 [ 1 ] 1 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_vals_43; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 1)
      | 44 (* tag_cont_36 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 4;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 92)
      | 45 (* tag_cont_37 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 4;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 94)
      | 46 (* tag_cont_38 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 98)
      | 47 (* tag_cont_39 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 100)
      | 48 (* tag_cont_40 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 104)
      | 49 (* tag_cont_41 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 106)
      | 50 (* tag_cont_42 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 108)
      | 51 (* tag_cont_43 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_int 0);
          w_34.state.c <- pc_to_exp (int_to_pc 110)
      | 52 (* tag_cont_44 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 1 tl_0;
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          w_34.state.c <- pc_to_exp (int_to_pc 115)
      | 53 (* tag_cont_45 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 116)
      | 54 (* tag_cont_46 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 117)
      | 55 (* tag_cont_47 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 118)
      | 56 (* tag_cont_48 *) ->
          w_34.state.k <- get_next_cont tl_0;
          restore_env w_34 2 tl_0;
          w_34.state.c <- pc_to_exp (int_to_pc 119)
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
      | 7 (* tag_Exp *) ->
          let splits_4 = Memo.splits (snd x_1) in
          let split0_4 = List.nth splits_4 0 in
          ignore (pop_env w_3);
          push_env w_3 split0_4;
          assert_env_length w_3 2;
          push_env w_3 (Memo.from_int 4);
          assert_env_length w_3 3;
          drop_n w_3 3 1;
          assert_env_length w_3 2;
          return_n w_3 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Log *) ->
          let splits_5 = Memo.splits (snd x_1) in
          let split0_5 = List.nth splits_5 0 in
          ignore (pop_env w_3);
          push_env w_3 split0_5;
          assert_env_length w_3 2;
          push_env w_3 (Memo.from_int 5);
          assert_env_length w_3 3;
          drop_n w_3 3 1;
          assert_env_length w_3 2;
          return_n w_3 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (4)")
    4;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 2;
      push_env w_4 (Dynarray.get w_4.state.e 0);
      assert_env_length w_4 3;
      let keep_vals_0 = env_call w_4 [ 0; 1 ] 1 in
      w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_vals_0; w_4.state.k ];
      w_4.state.c <- pc_to_exp (int_to_pc 3))
    5;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 1;
      push_env w_5 (Dynarray.get w_5.state.e 0);
      assert_env_length w_5 2;
      push_env w_5 (Memo.from_int 0);
      w_5.state.c <- pc_to_exp (int_to_pc 11))
    6;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      let x0_2 = resolve w_10 (Source.E 2) in
      let x1_2 = resolve w_10 (Source.E 3) in
      ignore (pop_env w_10);
      ignore (pop_env w_10);
      push_env w_10 (Memo.from_int (Word.get_value (fst x0_2) - Word.get_value (fst x1_2)));
      assert_env_length w_10 3;
      let keep_vals_1 = env_call w_10 [ 1 ] 1 in
      w_10.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_vals_1; w_10.state.k ];
      w_10.state.c <- pc_to_exp (int_to_pc 6))
    7;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 2;
      let cond_1 = resolve w_9 (Source.E 1) in
      ignore (pop_env w_9);
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_9 1;
        push_env w_9 (Memo.from_int 1);
        assert_env_length w_9 2;
        return_n w_9 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_9 1;
        push_env w_9 (Memo.from_int 2);
        assert_env_length w_9 2;
        push_env w_9 (Dynarray.get w_9.state.e 0);
        assert_env_length w_9 3;
        push_env w_9 (Memo.from_int 1);
        w_9.state.c <- pc_to_exp (int_to_pc 7)))
    8;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 3;
      let x0_1 = resolve w_8 (Source.E 1) in
      let x1_1 = resolve w_8 (Source.E 2) in
      ignore (pop_env w_8);
      ignore (pop_env w_8);
      push_env w_8 (Memo.from_int (if Word.get_value (fst x0_1) = Word.get_value (fst x1_1) then 1 else 0));
      w_8.state.c <- pc_to_exp (int_to_pc 8))
    9;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 2;
      let cond_0 = resolve w_7 (Source.E 1) in
      ignore (pop_env w_7);
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_7 1;
        push_env w_7 (Memo.from_int 0);
        assert_env_length w_7 2;
        return_n w_7 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_7 1;
        push_env w_7 (Dynarray.get w_7.state.e 0);
        assert_env_length w_7 2;
        push_env w_7 (Memo.from_int 0);
        w_7.state.c <- pc_to_exp (int_to_pc 9)))
    10;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 3;
      let x0_0 = resolve w_6 (Source.E 1) in
      let x1_0 = resolve w_6 (Source.E 2) in
      ignore (pop_env w_6);
      ignore (pop_env w_6);
      push_env w_6 (Memo.from_int (if Word.get_value (fst x0_0) < Word.get_value (fst x1_0) then 1 else 0));
      w_6.state.c <- pc_to_exp (int_to_pc 10))
    11;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 1;
      push_env w_11 (Dynarray.get w_11.state.e 0);
      assert_env_length w_11 2;
      push_env w_11 (Memo.from_int 1);
      w_11.state.c <- pc_to_exp (int_to_pc 15))
    12;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 4;
      let x0_4 = resolve w_14 (Source.E 2) in
      let x1_4 = resolve w_14 (Source.E 3) in
      ignore (pop_env w_14);
      ignore (pop_env w_14);
      push_env w_14 (Memo.from_int (Word.get_value (fst x0_4) / Word.get_value (fst x1_4)));
      assert_env_length w_14 3;
      let keep_vals_2 = env_call w_14 [ 1 ] 1 in
      w_14.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_vals_2; w_14.state.k ];
      w_14.state.c <- pc_to_exp (int_to_pc 12))
    13;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 2;
      let cond_2 = resolve w_13 (Source.E 1) in
      ignore (pop_env w_13);
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_13 1;
        push_env w_13 (Memo.from_int 0);
        assert_env_length w_13 2;
        return_n w_13 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_13 1;
        push_env w_13 (Memo.from_int 1);
        assert_env_length w_13 2;
        push_env w_13 (Dynarray.get w_13.state.e 0);
        assert_env_length w_13 3;
        push_env w_13 (Memo.from_int 2);
        w_13.state.c <- pc_to_exp (int_to_pc 13)))
    14;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 3;
      let x0_3 = resolve w_12 (Source.E 1) in
      let x1_3 = resolve w_12 (Source.E 2) in
      ignore (pop_env w_12);
      ignore (pop_env w_12);
      push_env w_12 (Memo.from_int (if Word.get_value (fst x0_3) <= Word.get_value (fst x1_3) then 1 else 0));
      w_12.state.c <- pc_to_exp (int_to_pc 14))
    15;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 2;
      push_env w_15 (Dynarray.get w_15.state.e 0);
      w_15.state.c <- pc_to_exp (int_to_pc 24))
    16;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 6;
      let x0_5 = resolve w_18 (Source.E 4) in
      let x1_5 = resolve w_18 (Source.E 5) in
      ignore (pop_env w_18);
      ignore (pop_env w_18);
      push_env w_18 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      assert_env_length w_18 5;
      drop_n w_18 5 1;
      assert_env_length w_18 4;
      drop_n w_18 4 1;
      assert_env_length w_18 3;
      return_n w_18 3 (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 4;
      let last_3 = Source.E 3 in
      let x_3 = resolve w_17 last_3 in
      match Word.get_value (fst x_3) with
      | 3 (* tag_Const *) ->
          let splits_7 = Memo.splits (snd x_3) in
          let split0_7 = List.nth splits_7 0 in
          ignore (pop_env w_17);
          push_env w_17 split0_7;
          assert_env_length w_17 4;
          push_env w_17 (Dynarray.get w_17.state.e 2);
          assert_env_length w_17 5;
          push_env w_17 (Dynarray.get w_17.state.e 3);
          w_17.state.c <- pc_to_exp (int_to_pc 17)
      | _ ->
          ignore (pop_env w_17);
          assert_env_length w_17 3;
          push_env w_17 (Memo.from_int 0);
          assert_env_length w_17 4;
          drop_n w_17 4 1;
          assert_env_length w_17 3;
          return_n w_17 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (18)")
    18;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 4;
      let last_4 = Source.E 3 in
      let x_4 = resolve w_19 last_4 in
      match Word.get_value (fst x_4) with
      | 4 (* tag_Var *) ->
          let splits_9 = Memo.splits (snd x_4) in
          let split0_9 = List.nth splits_9 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_9;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 2);
          assert_env_length w_19 5;
          let keep_vals_3 = env_call w_19 [ 3 ] 1 in
          w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_vals_3; w_19.state.k ];
          w_19.state.c <- pc_to_exp (int_to_pc 1)
      | _ ->
          ignore (pop_env w_19);
          assert_env_length w_19 3;
          push_env w_19 (Memo.from_int 0);
          assert_env_length w_19 4;
          drop_n w_19 4 1;
          assert_env_length w_19 3;
          return_n w_19 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (19)")
    19;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 5;
      let last_5 = Source.E 4 in
      let x_5 = resolve w_20 last_5 in
      match Word.get_value (fst x_5) with
      | 5 (* tag_Add *) ->
          let splits_11 = Memo.splits (snd x_5) in
          let split0_11 = List.nth splits_11 0 in
          let split1_3 = List.nth splits_11 1 in
          ignore (pop_env w_20);
          push_env w_20 split0_11;
          push_env w_20 split1_3;
          assert_env_length w_20 6;
          push_env w_20 (Dynarray.get w_20.state.e 2);
          assert_env_length w_20 7;
          push_env w_20 (Dynarray.get w_20.state.e 4);
          assert_env_length w_20 8;
          let keep_vals_4 = env_call w_20 [ 3; 5 ] 2 in
          w_20.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_vals_4; w_20.state.k ];
          w_20.state.c <- pc_to_exp (int_to_pc 16)
      | _ ->
          ignore (pop_env w_20);
          assert_env_length w_20 4;
          push_env w_20 (Memo.from_int 0);
          assert_env_length w_20 5;
          drop_n w_20 5 2;
          assert_env_length w_20 3;
          return_n w_20 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (20)")
    20;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 5;
      let last_6 = Source.E 4 in
      let x_6 = resolve w_21 last_6 in
      match Word.get_value (fst x_6) with
      | 6 (* tag_Mul *) ->
          let splits_13 = Memo.splits (snd x_6) in
          let split0_13 = List.nth splits_13 0 in
          let split1_5 = List.nth splits_13 1 in
          ignore (pop_env w_21);
          push_env w_21 split0_13;
          push_env w_21 split1_5;
          assert_env_length w_21 6;
          push_env w_21 (Dynarray.get w_21.state.e 2);
          assert_env_length w_21 7;
          push_env w_21 (Dynarray.get w_21.state.e 4);
          assert_env_length w_21 8;
          let keep_vals_5 = env_call w_21 [ 3; 5 ] 2 in
          w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_vals_5; w_21.state.k ];
          w_21.state.c <- pc_to_exp (int_to_pc 16)
      | _ ->
          ignore (pop_env w_21);
          assert_env_length w_21 4;
          push_env w_21 (Memo.from_int 0);
          assert_env_length w_21 5;
          drop_n w_21 5 2;
          assert_env_length w_21 3;
          return_n w_21 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (21)")
    21;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 4;
      let last_7 = Source.E 3 in
      let x_7 = resolve w_22 last_7 in
      match Word.get_value (fst x_7) with
      | 7 (* tag_Exp *) ->
          let splits_15 = Memo.splits (snd x_7) in
          let split0_15 = List.nth splits_15 0 in
          ignore (pop_env w_22);
          push_env w_22 split0_15;
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 5;
          push_env w_22 (Dynarray.get w_22.state.e 3);
          assert_env_length w_22 6;
          ignore (env_call w_22 [] 2);
          w_22.state.c <- pc_to_exp (int_to_pc 16)
      | _ ->
          ignore (pop_env w_22);
          assert_env_length w_22 3;
          push_env w_22 (Memo.from_int 0);
          assert_env_length w_22 4;
          drop_n w_22 4 1;
          assert_env_length w_22 3;
          return_n w_22 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (22)")
    22;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 4;
      let last_8 = Source.E 3 in
      let x_8 = resolve w_23 last_8 in
      match Word.get_value (fst x_8) with
      | 8 (* tag_Log *) ->
          let splits_17 = Memo.splits (snd x_8) in
          let split0_17 = List.nth splits_17 0 in
          ignore (pop_env w_23);
          push_env w_23 split0_17;
          assert_env_length w_23 4;
          push_env w_23 (Dynarray.get w_23.state.e 2);
          assert_env_length w_23 5;
          push_env w_23 (Dynarray.get w_23.state.e 3);
          assert_env_length w_23 6;
          ignore (env_call w_23 [] 2);
          w_23.state.c <- pc_to_exp (int_to_pc 16)
      | _ ->
          ignore (pop_env w_23);
          assert_env_length w_23 3;
          push_env w_23 (Memo.from_int 0);
          assert_env_length w_23 4;
          drop_n w_23 4 1;
          assert_env_length w_23 3;
          return_n w_23 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (23)")
    23;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 3;
      let last_2 = Source.E 2 in
      let x_2 = resolve w_16 last_2 in
      match Word.get_value (fst x_2) with
      | 3 (* tag_Const *) ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_6;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 18)
      | 4 (* tag_Var *) ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_8;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 19)
      | 5 (* tag_Add *) ->
          let splits_10 = Memo.splits (snd x_2) in
          let split0_10 = List.nth splits_10 0 in
          let split1_2 = List.nth splits_10 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_10;
          push_env w_16 split1_2;
          assert_env_length w_16 4;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 20)
      | 6 (* tag_Mul *) ->
          let splits_12 = Memo.splits (snd x_2) in
          let split0_12 = List.nth splits_12 0 in
          let split1_4 = List.nth splits_12 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_12;
          push_env w_16 split1_4;
          assert_env_length w_16 4;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 21)
      | 7 (* tag_Exp *) ->
          let splits_14 = Memo.splits (snd x_2) in
          let split0_14 = List.nth splits_14 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_14;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 22)
      | 8 (* tag_Log *) ->
          let splits_16 = Memo.splits (snd x_2) in
          let split0_16 = List.nth splits_16 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_16;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 23)
      | _ -> failwith "unreachable (24)")
    24;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 1;
      push_env w_24 (Dynarray.get w_24.state.e 0);
      w_24.state.c <- pc_to_exp (int_to_pc 26))
    25;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 2;
      let last_9 = Source.E 1 in
      let x_9 = resolve w_25 last_9 in
      match Word.get_value (fst x_9) with
      | 3 (* tag_Const *) ->
          let splits_18 = Memo.splits (snd x_9) in
          let split0_18 = List.nth splits_18 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_18;
          assert_env_length w_25 2;
          push_env w_25 (Dynarray.get w_25.state.e 0);
          assert_env_length w_25 3;
          drop_n w_25 3 1;
          assert_env_length w_25 2;
          return_n w_25 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_19 = Memo.splits (snd x_9) in
          let split0_19 = List.nth splits_19 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_19;
          assert_env_length w_25 2;
          push_env w_25 (Dynarray.get w_25.state.e 0);
          assert_env_length w_25 3;
          drop_n w_25 3 1;
          assert_env_length w_25 2;
          return_n w_25 2 (pc_to_exp (int_to_pc 0))
      | 5 (* tag_Add *) ->
          let splits_20 = Memo.splits (snd x_9) in
          let split0_20 = List.nth splits_20 0 in
          let split1_6 = List.nth splits_20 1 in
          ignore (pop_env w_25);
          push_env w_25 split0_20;
          push_env w_25 split1_6;
          assert_env_length w_25 3;
          push_env w_25 (Dynarray.get w_25.state.e 1);
          assert_env_length w_25 4;
          let keep_vals_6 = env_call w_25 [ 2 ] 1 in
          w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_vals_6; w_25.state.k ];
          w_25.state.c <- pc_to_exp (int_to_pc 25)
      | 6 (* tag_Mul *) ->
          let splits_21 = Memo.splits (snd x_9) in
          let split0_21 = List.nth splits_21 0 in
          let split1_7 = List.nth splits_21 1 in
          ignore (pop_env w_25);
          push_env w_25 split0_21;
          push_env w_25 split1_7;
          assert_env_length w_25 3;
          push_env w_25 (Dynarray.get w_25.state.e 1);
          assert_env_length w_25 4;
          let keep_vals_7 = env_call w_25 [ 2 ] 1 in
          w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_vals_7; w_25.state.k ];
          w_25.state.c <- pc_to_exp (int_to_pc 25)
      | 7 (* tag_Exp *) ->
          let splits_22 = Memo.splits (snd x_9) in
          let split0_22 = List.nth splits_22 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_22;
          assert_env_length w_25 2;
          push_env w_25 (Dynarray.get w_25.state.e 1);
          assert_env_length w_25 3;
          let keep_vals_8 = env_call w_25 [] 1 in
          w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_vals_8; w_25.state.k ];
          w_25.state.c <- pc_to_exp (int_to_pc 25)
      | 8 (* tag_Log *) ->
          let splits_23 = Memo.splits (snd x_9) in
          let split0_23 = List.nth splits_23 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_23;
          assert_env_length w_25 2;
          push_env w_25 (Dynarray.get w_25.state.e 1);
          assert_env_length w_25 3;
          let keep_vals_9 = env_call w_25 [] 1 in
          w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_vals_9; w_25.state.k ];
          w_25.state.c <- pc_to_exp (int_to_pc 25)
      | _ -> failwith "unreachable (26)")
    26;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 2;
      push_env w_26 (Dynarray.get w_26.state.e 0);
      assert_env_length w_26 3;
      let keep_vals_10 = env_call w_26 [ 1 ] 1 in
      w_26.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_vals_10; w_26.state.k ];
      w_26.state.c <- pc_to_exp (int_to_pc 25))
    27;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 1;
      push_env w_27 (Dynarray.get w_27.state.e 0);
      assert_env_length w_27 2;
      let keep_vals_11 = env_call w_27 [ 0 ] 1 in
      w_27.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_vals_11; w_27.state.k ];
      w_27.state.c <- pc_to_exp (int_to_pc 25))
    28;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 1;
      push_env w_28 (Dynarray.get w_28.state.e 0);
      w_28.state.c <- pc_to_exp (int_to_pc 31))
    29;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 3;
      let last_11 = Source.E 2 in
      let x_11 = resolve w_30 last_11 in
      match Word.get_value (fst x_11) with
      | 1 (* tag_X *) ->
          ignore (pop_env w_30);
          assert_env_length w_30 2;
          push_env w_30 (Memo.from_int 1);
          assert_env_length w_30 3;
          let ctor_arg_1 = pop_env w_30 in
          push_env w_30 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_1 ]);
          assert_env_length w_30 3;
          drop_n w_30 3 1;
          assert_env_length w_30 2;
          return_n w_30 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Y *) ->
          ignore (pop_env w_30);
          assert_env_length w_30 2;
          push_env w_30 (Memo.from_int 0);
          assert_env_length w_30 3;
          let ctor_arg_2 = pop_env w_30 in
          push_env w_30 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_2 ]);
          assert_env_length w_30 3;
          drop_n w_30 3 1;
          assert_env_length w_30 2;
          return_n w_30 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (30)")
    30;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 2;
      let last_10 = Source.E 1 in
      let x_10 = resolve w_29 last_10 in
      match Word.get_value (fst x_10) with
      | 3 (* tag_Const *) ->
          let splits_24 = Memo.splits (snd x_10) in
          let split0_24 = List.nth splits_24 0 in
          ignore (pop_env w_29);
          push_env w_29 split0_24;
          assert_env_length w_29 2;
          push_env w_29 (Memo.from_int 0);
          assert_env_length w_29 3;
          let ctor_arg_0 = pop_env w_29 in
          push_env w_29 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_0 ]);
          assert_env_length w_29 3;
          drop_n w_29 3 1;
          assert_env_length w_29 2;
          return_n w_29 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_25 = Memo.splits (snd x_10) in
          let split0_25 = List.nth splits_25 0 in
          ignore (pop_env w_29);
          push_env w_29 split0_25;
          assert_env_length w_29 2;
          push_env w_29 (Dynarray.get w_29.state.e 1);
          w_29.state.c <- pc_to_exp (int_to_pc 30)
      | 5 (* tag_Add *) ->
          let splits_26 = Memo.splits (snd x_10) in
          let split0_26 = List.nth splits_26 0 in
          let split1_8 = List.nth splits_26 1 in
          ignore (pop_env w_29);
          push_env w_29 split0_26;
          push_env w_29 split1_8;
          assert_env_length w_29 3;
          push_env w_29 (Dynarray.get w_29.state.e 1);
          assert_env_length w_29 4;
          let keep_vals_12 = env_call w_29 [ 2 ] 1 in
          w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_vals_12; w_29.state.k ];
          w_29.state.c <- pc_to_exp (int_to_pc 29)
      | 6 (* tag_Mul *) ->
          let splits_27 = Memo.splits (snd x_10) in
          let split0_27 = List.nth splits_27 0 in
          let split1_9 = List.nth splits_27 1 in
          ignore (pop_env w_29);
          push_env w_29 split0_27;
          push_env w_29 split1_9;
          assert_env_length w_29 3;
          push_env w_29 (Dynarray.get w_29.state.e 1);
          assert_env_length w_29 4;
          let keep_vals_13 = env_call w_29 [ 1; 2 ] 1 in
          w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_vals_13; w_29.state.k ];
          w_29.state.c <- pc_to_exp (int_to_pc 29)
      | 7 (* tag_Exp *) ->
          let splits_28 = Memo.splits (snd x_10) in
          let split0_28 = List.nth splits_28 0 in
          ignore (pop_env w_29);
          push_env w_29 split0_28;
          assert_env_length w_29 2;
          push_env w_29 (Dynarray.get w_29.state.e 1);
          assert_env_length w_29 3;
          let ctor_arg_3 = pop_env w_29 in
          push_env w_29 (Memo.appends [ Memo.from_constructor tag_Exp; ctor_arg_3 ]);
          assert_env_length w_29 3;
          push_env w_29 (Dynarray.get w_29.state.e 1);
          assert_env_length w_29 4;
          let keep_vals_14 = env_call w_29 [ 2 ] 1 in
          w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_vals_14; w_29.state.k ];
          w_29.state.c <- pc_to_exp (int_to_pc 29)
      | 8 (* tag_Log *) ->
          let splits_29 = Memo.splits (snd x_10) in
          let split0_29 = List.nth splits_29 0 in
          ignore (pop_env w_29);
          push_env w_29 split0_29;
          assert_env_length w_29 2;
          push_env w_29 (Dynarray.get w_29.state.e 1);
          assert_env_length w_29 3;
          let keep_vals_15 = env_call w_29 [ 1 ] 1 in
          w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_vals_15; w_29.state.k ];
          w_29.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (31)")
    31;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 3;
      push_env w_31 (Dynarray.get w_31.state.e 0);
      w_31.state.c <- pc_to_exp (int_to_pc 34))
    32;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 5;
      let last_13 = Source.E 4 in
      let x_13 = resolve w_33 last_13 in
      match Word.get_value (fst x_13) with
      | 1 (* tag_X *) ->
          ignore (pop_env w_33);
          assert_env_length w_33 4;
          push_env w_33 (Dynarray.get w_33.state.e 1);
          assert_env_length w_33 5;
          drop_n w_33 5 1;
          assert_env_length w_33 4;
          return_n w_33 4 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Y *) ->
          ignore (pop_env w_33);
          assert_env_length w_33 4;
          push_env w_33 (Dynarray.get w_33.state.e 2);
          assert_env_length w_33 5;
          drop_n w_33 5 1;
          assert_env_length w_33 4;
          return_n w_33 4 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (33)")
    33;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 4;
      let last_12 = Source.E 3 in
      let x_12 = resolve w_32 last_12 in
      match Word.get_value (fst x_12) with
      | 3 (* tag_Const *) ->
          let splits_30 = Memo.splits (snd x_12) in
          let split0_30 = List.nth splits_30 0 in
          ignore (pop_env w_32);
          push_env w_32 split0_30;
          assert_env_length w_32 4;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 5;
          drop_n w_32 5 1;
          assert_env_length w_32 4;
          return_n w_32 4 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Var *) ->
          let splits_31 = Memo.splits (snd x_12) in
          let split0_31 = List.nth splits_31 0 in
          ignore (pop_env w_32);
          push_env w_32 split0_31;
          assert_env_length w_32 4;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          w_32.state.c <- pc_to_exp (int_to_pc 33)
      | 5 (* tag_Add *) ->
          let splits_32 = Memo.splits (snd x_12) in
          let split0_32 = List.nth splits_32 0 in
          let split1_10 = List.nth splits_32 1 in
          ignore (pop_env w_32);
          push_env w_32 split0_32;
          push_env w_32 split1_10;
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 6;
          push_env w_32 (Dynarray.get w_32.state.e 1);
          assert_env_length w_32 7;
          push_env w_32 (Dynarray.get w_32.state.e 2);
          assert_env_length w_32 8;
          let keep_vals_16 = env_call w_32 [ 1; 2; 4 ] 3 in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_vals_16; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 32)
      | 6 (* tag_Mul *) ->
          let splits_33 = Memo.splits (snd x_12) in
          let split0_33 = List.nth splits_33 0 in
          let split1_11 = List.nth splits_33 1 in
          ignore (pop_env w_32);
          push_env w_32 split0_33;
          push_env w_32 split1_11;
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 6;
          push_env w_32 (Dynarray.get w_32.state.e 1);
          assert_env_length w_32 7;
          push_env w_32 (Dynarray.get w_32.state.e 2);
          assert_env_length w_32 8;
          let keep_vals_17 = env_call w_32 [ 1; 2; 4 ] 3 in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_vals_17; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 32)
      | 7 (* tag_Exp *) ->
          let splits_34 = Memo.splits (snd x_12) in
          let split0_34 = List.nth splits_34 0 in
          ignore (pop_env w_32);
          push_env w_32 split0_34;
          assert_env_length w_32 4;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 1);
          assert_env_length w_32 6;
          push_env w_32 (Dynarray.get w_32.state.e 2);
          assert_env_length w_32 7;
          let keep_vals_18 = env_call w_32 [] 3 in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_vals_18; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 32)
      | 8 (* tag_Log *) ->
          let splits_35 = Memo.splits (snd x_12) in
          let split0_35 = List.nth splits_35 0 in
          ignore (pop_env w_32);
          push_env w_32 split0_35;
          assert_env_length w_32 4;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 1);
          assert_env_length w_32 6;
          push_env w_32 (Dynarray.get w_32.state.e 2);
          assert_env_length w_32 7;
          let keep_vals_19 = env_call w_32 [] 3 in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_vals_19; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 32)
      | _ -> failwith "unreachable (34)")
    34;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 2;
      let x0_6 = resolve w_35 (Source.E 0) in
      let x1_6 = resolve w_35 (Source.E 1) in
      ignore (pop_env w_35);
      ignore (pop_env w_35);
      push_env w_35 (Memo.from_int (Word.get_value (fst x0_6) * Word.get_value (fst x1_6)));
      assert_env_length w_35 1;
      return_n w_35 1 (pc_to_exp (int_to_pc 0)))
    35;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 2;
      let x0_7 = resolve w_36 (Source.E 0) in
      let x1_7 = resolve w_36 (Source.E 1) in
      ignore (pop_env w_36);
      ignore (pop_env w_36);
      push_env w_36 (Memo.from_int (Word.get_value (fst x0_7) + Word.get_value (fst x1_7)));
      assert_env_length w_36 1;
      return_n w_36 1 (pc_to_exp (int_to_pc 0)))
    36;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 3;
      let cond_3 = resolve w_39 (Source.E 2) in
      ignore (pop_env w_39);
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_39 2;
        push_env w_39 (Memo.from_int 1);
        assert_env_length w_39 3;
        let ctor_arg_4 = pop_env w_39 in
        push_env w_39 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_4 ]);
        assert_env_length w_39 3;
        drop_n w_39 3 1;
        assert_env_length w_39 2;
        drop_n w_39 2 1;
        assert_env_length w_39 1;
        drop_n w_39 1 0;
        assert_env_length w_39 1;
        return_n w_39 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_39 2;
        push_env w_39 (Dynarray.get w_39.state.e 1);
        assert_env_length w_39 3;
        let keep_vals_26 = env_call w_39 [] 1 in
        w_39.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_vals_26; w_39.state.k ];
        w_39.state.c <- pc_to_exp (int_to_pc 6)))
    37;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 4;
      let x0_8 = resolve w_38 (Source.E 2) in
      let x1_8 = resolve w_38 (Source.E 3) in
      ignore (pop_env w_38);
      ignore (pop_env w_38);
      push_env w_38 (Memo.from_int (if Word.get_value (fst x0_8) = Word.get_value (fst x1_8) then 1 else 0));
      w_38.state.c <- pc_to_exp (int_to_pc 37))
    38;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 2;
      let last_14 = Source.E 1 in
      let x_14 = resolve w_37 last_14 in
      match Word.get_value (fst x_14) with
      | 3 (* tag_Const *) ->
          let splits_36 = Memo.splits (snd x_14) in
          let split0_36 = List.nth splits_36 0 in
          ignore (pop_env w_37);
          push_env w_37 split0_36;
          assert_env_length w_37 2;
          push_env w_37 (Dynarray.get w_37.state.e 1);
          assert_env_length w_37 3;
          push_env w_37 (Memo.from_int 0);
          w_37.state.c <- pc_to_exp (int_to_pc 38)
      | _ ->
          ignore (pop_env w_37);
          assert_env_length w_37 1;
          push_env w_37 (Dynarray.get w_37.state.e 0);
          assert_env_length w_37 2;
          let ctor_arg_5 = pop_env w_37 in
          push_env w_37 (Memo.appends [ Memo.from_constructor tag_Exp; ctor_arg_5 ]);
          assert_env_length w_37 2;
          drop_n w_37 2 1;
          assert_env_length w_37 1;
          drop_n w_37 1 0;
          assert_env_length w_37 1;
          return_n w_37 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (39)")
    39;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 3;
      let cond_4 = resolve w_42 (Source.E 2) in
      ignore (pop_env w_42);
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_42 2;
        push_env w_42 (Memo.from_int 0);
        assert_env_length w_42 3;
        let ctor_arg_6 = pop_env w_42 in
        push_env w_42 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_6 ]);
        assert_env_length w_42 3;
        drop_n w_42 3 1;
        assert_env_length w_42 2;
        drop_n w_42 2 1;
        assert_env_length w_42 1;
        drop_n w_42 1 0;
        assert_env_length w_42 1;
        return_n w_42 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_42 2;
        push_env w_42 (Dynarray.get w_42.state.e 1);
        assert_env_length w_42 3;
        let keep_vals_27 = env_call w_42 [] 1 in
        w_42.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_vals_27; w_42.state.k ];
        w_42.state.c <- pc_to_exp (int_to_pc 12)))
    40;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 4;
      let x0_9 = resolve w_41 (Source.E 2) in
      let x1_9 = resolve w_41 (Source.E 3) in
      ignore (pop_env w_41);
      ignore (pop_env w_41);
      push_env w_41 (Memo.from_int (if Word.get_value (fst x0_9) = Word.get_value (fst x1_9) then 1 else 0));
      w_41.state.c <- pc_to_exp (int_to_pc 40))
    41;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 2;
      let last_15 = Source.E 1 in
      let x_15 = resolve w_40 last_15 in
      match Word.get_value (fst x_15) with
      | 3 (* tag_Const *) ->
          let splits_37 = Memo.splits (snd x_15) in
          let split0_37 = List.nth splits_37 0 in
          ignore (pop_env w_40);
          push_env w_40 split0_37;
          assert_env_length w_40 2;
          push_env w_40 (Dynarray.get w_40.state.e 1);
          assert_env_length w_40 3;
          push_env w_40 (Memo.from_int 1);
          w_40.state.c <- pc_to_exp (int_to_pc 41)
      | _ ->
          ignore (pop_env w_40);
          assert_env_length w_40 1;
          push_env w_40 (Dynarray.get w_40.state.e 0);
          assert_env_length w_40 2;
          let ctor_arg_7 = pop_env w_40 in
          push_env w_40 (Memo.appends [ Memo.from_constructor tag_Log; ctor_arg_7 ]);
          assert_env_length w_40 2;
          drop_n w_40 2 1;
          assert_env_length w_40 1;
          drop_n w_40 1 0;
          assert_env_length w_40 1;
          return_n w_40 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (42)")
    42;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 5;
      let x0_10 = resolve w_43 (Source.E 3) in
      let x1_10 = resolve w_43 (Source.E 4) in
      ignore (pop_env w_43);
      ignore (pop_env w_43);
      push_env w_43 (Memo.from_int (Word.get_value (fst x0_10) - Word.get_value (fst x1_10)));
      assert_env_length w_43 4;
      let ctor_arg_13 = pop_env w_43 in
      push_env w_43 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_13 ]);
      assert_env_length w_43 4;
      let ctor_arg_14 = pop_env w_43 in
      let ctor_arg_15 = pop_env w_43 in
      push_env w_43 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_15; ctor_arg_14 ]);
      assert_env_length w_43 3;
      let ctor_arg_16 = pop_env w_43 in
      push_env w_43 (Memo.appends [ Memo.from_constructor tag_Exp; ctor_arg_16 ]);
      assert_env_length w_43 3;
      let ctor_arg_17 = pop_env w_43 in
      let ctor_arg_18 = pop_env w_43 in
      push_env w_43 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_18; ctor_arg_17 ]);
      assert_env_length w_43 2;
      drop_n w_43 2 1;
      assert_env_length w_43 1;
      return_n w_43 1 (pc_to_exp (int_to_pc 0)))
    43;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 7;
      let cond_8 = resolve w_53 (Source.E 6) in
      ignore (pop_env w_53);
      if Word.get_value (fst cond_8) <> 0 then (
        assert_env_length w_53 6;
        push_env w_53 (Memo.from_int 1);
        assert_env_length w_53 7;
        drop_n w_53 7 1;
        assert_env_length w_53 6;
        drop_n w_53 6 1;
        assert_env_length w_53 5;
        drop_n w_53 5 1;
        assert_env_length w_53 4;
        drop_n w_53 4 1;
        assert_env_length w_53 3;
        return_n w_53 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_53 6;
        push_env w_53 (Memo.from_int 0);
        assert_env_length w_53 7;
        drop_n w_53 7 1;
        assert_env_length w_53 6;
        drop_n w_53 6 1;
        assert_env_length w_53 5;
        drop_n w_53 5 1;
        assert_env_length w_53 4;
        drop_n w_53 4 1;
        assert_env_length w_53 3;
        return_n w_53 3 (pc_to_exp (int_to_pc 0))))
    44;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 8;
      let x0_14 = resolve w_52 (Source.E 6) in
      let x1_14 = resolve w_52 (Source.E 7) in
      ignore (pop_env w_52);
      ignore (pop_env w_52);
      push_env w_52 (Memo.from_int (if Word.get_value (fst x0_14) > Word.get_value (fst x1_14) then 1 else 0));
      w_52.state.c <- pc_to_exp (int_to_pc 44))
    45;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 8;
      let x0_15 = resolve w_54 (Source.E 6) in
      let x1_15 = resolve w_54 (Source.E 7) in
      ignore (pop_env w_54);
      ignore (pop_env w_54);
      push_env w_54 (Memo.from_int (Word.get_value (fst x0_15) - Word.get_value (fst x1_15)));
      assert_env_length w_54 7;
      drop_n w_54 7 1;
      assert_env_length w_54 6;
      drop_n w_54 6 1;
      assert_env_length w_54 5;
      drop_n w_54 5 1;
      assert_env_length w_54 4;
      drop_n w_54 4 1;
      assert_env_length w_54 3;
      return_n w_54 3 (pc_to_exp (int_to_pc 0)))
    46;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 7;
      let cond_7 = resolve w_51 (Source.E 6) in
      ignore (pop_env w_51);
      if Word.get_value (fst cond_7) <> 0 then (
        assert_env_length w_51 6;
        push_env w_51 (Memo.from_int 0);
        assert_env_length w_51 7;
        push_env w_51 (Memo.from_int 1);
        w_51.state.c <- pc_to_exp (int_to_pc 46))
      else (
        assert_env_length w_51 6;
        push_env w_51 (Dynarray.get w_51.state.e 4);
        assert_env_length w_51 7;
        push_env w_51 (Dynarray.get w_51.state.e 5);
        w_51.state.c <- pc_to_exp (int_to_pc 45)))
    47;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 8;
      let x0_13 = resolve w_50 (Source.E 6) in
      let x1_13 = resolve w_50 (Source.E 7) in
      ignore (pop_env w_50);
      ignore (pop_env w_50);
      push_env w_50 (Memo.from_int (if Word.get_value (fst x0_13) < Word.get_value (fst x1_13) then 1 else 0));
      w_50.state.c <- pc_to_exp (int_to_pc 47))
    48;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 6;
      let last_17 = Source.E 5 in
      let x_17 = resolve w_49 last_17 in
      match Word.get_value (fst x_17) with
      | 3 (* tag_Const *) ->
          let splits_39 = Memo.splits (snd x_17) in
          let split0_39 = List.nth splits_39 0 in
          ignore (pop_env w_49);
          push_env w_49 split0_39;
          assert_env_length w_49 6;
          push_env w_49 (Dynarray.get w_49.state.e 4);
          assert_env_length w_49 7;
          push_env w_49 (Dynarray.get w_49.state.e 5);
          w_49.state.c <- pc_to_exp (int_to_pc 48)
      | _ -> failwith "unreachable (49)")
    49;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 6;
      let last_18 = Source.E 5 in
      let x_18 = resolve w_55 last_18 in
      match Word.get_value (fst x_18) with
      | 4 (* tag_Var *) ->
          let splits_41 = Memo.splits (snd x_18) in
          let split0_41 = List.nth splits_41 0 in
          ignore (pop_env w_55);
          push_env w_55 split0_41;
          assert_env_length w_55 6;
          push_env w_55 (Dynarray.get w_55.state.e 4);
          assert_env_length w_55 7;
          let keep_vals_34 = env_call w_55 [ 5 ] 1 in
          w_55.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_vals_34; w_55.state.k ];
          w_55.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (50)")
    50;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 7;
      let last_19 = Source.E 6 in
      let x_19 = resolve w_56 last_19 in
      match Word.get_value (fst x_19) with
      | 5 (* tag_Add *) ->
          let splits_43 = Memo.splits (snd x_19) in
          let split0_43 = List.nth splits_43 0 in
          let split1_13 = List.nth splits_43 1 in
          ignore (pop_env w_56);
          push_env w_56 split0_43;
          push_env w_56 split1_13;
          assert_env_length w_56 8;
          push_env w_56 (Dynarray.get w_56.state.e 4);
          assert_env_length w_56 9;
          push_env w_56 (Dynarray.get w_56.state.e 6);
          assert_env_length w_56 10;
          let keep_vals_35 = env_call w_56 [ 5; 7 ] 2 in
          w_56.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_vals_35; w_56.state.k ];
          w_56.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (51)")
    51;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 7;
      let last_20 = Source.E 6 in
      let x_20 = resolve w_57 last_20 in
      match Word.get_value (fst x_20) with
      | 6 (* tag_Mul *) ->
          let splits_45 = Memo.splits (snd x_20) in
          let split0_45 = List.nth splits_45 0 in
          let split1_15 = List.nth splits_45 1 in
          ignore (pop_env w_57);
          push_env w_57 split0_45;
          push_env w_57 split1_15;
          assert_env_length w_57 8;
          push_env w_57 (Dynarray.get w_57.state.e 4);
          assert_env_length w_57 9;
          push_env w_57 (Dynarray.get w_57.state.e 6);
          assert_env_length w_57 10;
          let keep_vals_36 = env_call w_57 [ 5; 7 ] 2 in
          w_57.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_vals_36; w_57.state.k ];
          w_57.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (52)")
    52;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 6;
      let last_21 = Source.E 5 in
      let x_21 = resolve w_58 last_21 in
      match Word.get_value (fst x_21) with
      | 7 (* tag_Exp *) ->
          let splits_47 = Memo.splits (snd x_21) in
          let split0_47 = List.nth splits_47 0 in
          ignore (pop_env w_58);
          push_env w_58 split0_47;
          assert_env_length w_58 6;
          push_env w_58 (Dynarray.get w_58.state.e 4);
          assert_env_length w_58 7;
          push_env w_58 (Dynarray.get w_58.state.e 5);
          assert_env_length w_58 8;
          ignore (env_call w_58 [] 2);
          w_58.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (53)")
    53;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 6;
      let last_22 = Source.E 5 in
      let x_22 = resolve w_59 last_22 in
      match Word.get_value (fst x_22) with
      | 8 (* tag_Log *) ->
          let splits_49 = Memo.splits (snd x_22) in
          let split0_49 = List.nth splits_49 0 in
          ignore (pop_env w_59);
          push_env w_59 split0_49;
          assert_env_length w_59 6;
          push_env w_59 (Dynarray.get w_59.state.e 4);
          assert_env_length w_59 7;
          push_env w_59 (Dynarray.get w_59.state.e 5);
          assert_env_length w_59 8;
          ignore (env_call w_59 [] 2);
          w_59.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (54)")
    54;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 5;
      let last_16 = Source.E 4 in
      let x_16 = resolve w_48 last_16 in
      match Word.get_value (fst x_16) with
      | 3 (* tag_Const *) ->
          let splits_38 = Memo.splits (snd x_16) in
          let split0_38 = List.nth splits_38 0 in
          ignore (pop_env w_48);
          push_env w_48 split0_38;
          assert_env_length w_48 5;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 49)
      | 4 (* tag_Var *) ->
          let splits_40 = Memo.splits (snd x_16) in
          let split0_40 = List.nth splits_40 0 in
          ignore (pop_env w_48);
          push_env w_48 split0_40;
          assert_env_length w_48 5;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 50)
      | 5 (* tag_Add *) ->
          let splits_42 = Memo.splits (snd x_16) in
          let split0_42 = List.nth splits_42 0 in
          let split1_12 = List.nth splits_42 1 in
          ignore (pop_env w_48);
          push_env w_48 split0_42;
          push_env w_48 split1_12;
          assert_env_length w_48 6;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 51)
      | 6 (* tag_Mul *) ->
          let splits_44 = Memo.splits (snd x_16) in
          let split0_44 = List.nth splits_44 0 in
          let split1_14 = List.nth splits_44 1 in
          ignore (pop_env w_48);
          push_env w_48 split0_44;
          push_env w_48 split1_14;
          assert_env_length w_48 6;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 52)
      | 7 (* tag_Exp *) ->
          let splits_46 = Memo.splits (snd x_16) in
          let split0_46 = List.nth splits_46 0 in
          ignore (pop_env w_48);
          push_env w_48 split0_46;
          assert_env_length w_48 5;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 53)
      | 8 (* tag_Log *) ->
          let splits_48 = Memo.splits (snd x_16) in
          let split0_48 = List.nth splits_48 0 in
          ignore (pop_env w_48);
          push_env w_48 split0_48;
          assert_env_length w_48 5;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 54)
      | _ -> failwith "unreachable (55)")
    55;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 5;
      let cond_6 = resolve w_47 (Source.E 4) in
      ignore (pop_env w_47);
      if Word.get_value (fst cond_6) <> 0 then (
        assert_env_length w_47 4;
        push_env w_47 (Memo.from_int 1);
        assert_env_length w_47 5;
        drop_n w_47 5 1;
        assert_env_length w_47 4;
        drop_n w_47 4 1;
        assert_env_length w_47 3;
        return_n w_47 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_47 4;
        push_env w_47 (Dynarray.get w_47.state.e 0);
        w_47.state.c <- pc_to_exp (int_to_pc 55)))
    56;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 6;
      let x0_12 = resolve w_46 (Source.E 4) in
      let x1_12 = resolve w_46 (Source.E 5) in
      ignore (pop_env w_46);
      ignore (pop_env w_46);
      push_env w_46 (Memo.from_int (if Word.get_value (fst x0_12) > Word.get_value (fst x1_12) then 1 else 0));
      w_46.state.c <- pc_to_exp (int_to_pc 56))
    57;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 6;
      let x0_16 = resolve w_60 (Source.E 4) in
      let x1_16 = resolve w_60 (Source.E 5) in
      ignore (pop_env w_60);
      ignore (pop_env w_60);
      push_env w_60 (Memo.from_int (Word.get_value (fst x0_16) - Word.get_value (fst x1_16)));
      assert_env_length w_60 5;
      drop_n w_60 5 1;
      assert_env_length w_60 4;
      drop_n w_60 4 1;
      assert_env_length w_60 3;
      return_n w_60 3 (pc_to_exp (int_to_pc 0)))
    58;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 5;
      let cond_5 = resolve w_45 (Source.E 4) in
      ignore (pop_env w_45);
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_45 4;
        push_env w_45 (Memo.from_int 0);
        assert_env_length w_45 5;
        push_env w_45 (Memo.from_int 1);
        w_45.state.c <- pc_to_exp (int_to_pc 58))
      else (
        assert_env_length w_45 4;
        push_env w_45 (Dynarray.get w_45.state.e 2);
        assert_env_length w_45 5;
        push_env w_45 (Dynarray.get w_45.state.e 3);
        w_45.state.c <- pc_to_exp (int_to_pc 57)))
    59;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 6;
      let x0_11 = resolve w_44 (Source.E 4) in
      let x1_11 = resolve w_44 (Source.E 5) in
      ignore (pop_env w_44);
      ignore (pop_env w_44);
      push_env w_44 (Memo.from_int (if Word.get_value (fst x0_11) < Word.get_value (fst x1_11) then 1 else 0));
      w_44.state.c <- pc_to_exp (int_to_pc 59))
    60;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 2;
      let x0_17 = resolve w_61 (Source.E 0) in
      let x1_17 = resolve w_61 (Source.E 1) in
      ignore (pop_env w_61);
      ignore (pop_env w_61);
      push_env w_61 (Memo.from_int (if Word.get_value (fst x0_17) = Word.get_value (fst x1_17) then 1 else 0));
      assert_env_length w_61 1;
      drop_n w_61 1 0;
      assert_env_length w_61 1;
      drop_n w_61 1 0;
      assert_env_length w_61 1;
      return_n w_61 1 (pc_to_exp (int_to_pc 0)))
    61;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 2;
      let x0_18 = resolve w_62 (Source.E 0) in
      let x1_18 = resolve w_62 (Source.E 1) in
      ignore (pop_env w_62);
      ignore (pop_env w_62);
      push_env w_62
        (Memo.from_int (if Word.get_value (fst x0_18) <> 0 && Word.get_value (fst x1_18) <> 0 then 1 else 0));
      assert_env_length w_62 1;
      drop_n w_62 1 0;
      assert_env_length w_62 1;
      drop_n w_62 1 0;
      assert_env_length w_62 1;
      return_n w_62 1 (pc_to_exp (int_to_pc 0)))
    62;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 2;
      let x0_19 = resolve w_63 (Source.E 0) in
      let x1_19 = resolve w_63 (Source.E 1) in
      ignore (pop_env w_63);
      ignore (pop_env w_63);
      push_env w_63
        (Memo.from_int (if Word.get_value (fst x0_19) <> 0 && Word.get_value (fst x1_19) <> 0 then 1 else 0));
      assert_env_length w_63 1;
      drop_n w_63 1 0;
      assert_env_length w_63 1;
      drop_n w_63 1 0;
      assert_env_length w_63 1;
      return_n w_63 1 (pc_to_exp (int_to_pc 0)))
    63;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 6;
      let x0_21 = resolve w_68 (Source.E 4) in
      let x1_21 = resolve w_68 (Source.E 5) in
      ignore (pop_env w_68);
      ignore (pop_env w_68);
      push_env w_68 (Memo.from_int (Word.get_value (fst x0_21) + Word.get_value (fst x1_21)));
      assert_env_length w_68 5;
      let ctor_arg_19 = pop_env w_68 in
      push_env w_68 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_19 ]);
      assert_env_length w_68 5;
      drop_n w_68 5 1;
      assert_env_length w_68 4;
      drop_n w_68 4 1;
      assert_env_length w_68 3;
      drop_n w_68 3 1;
      assert_env_length w_68 2;
      drop_n w_68 2 1;
      assert_env_length w_68 1;
      drop_n w_68 1 0;
      assert_env_length w_68 1;
      return_n w_68 1 (pc_to_exp (int_to_pc 0)))
    64;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 4;
      let last_24 = Source.E 3 in
      let x_24 = resolve w_67 last_24 in
      match Word.get_value (fst x_24) with
      | 3 (* tag_Const *) ->
          let splits_51 = Memo.splits (snd x_24) in
          let split0_51 = List.nth splits_51 0 in
          ignore (pop_env w_67);
          push_env w_67 split0_51;
          assert_env_length w_67 4;
          push_env w_67 (Dynarray.get w_67.state.e 2);
          assert_env_length w_67 5;
          push_env w_67 (Dynarray.get w_67.state.e 3);
          w_67.state.c <- pc_to_exp (int_to_pc 64)
      | _ ->
          ignore (pop_env w_67);
          assert_env_length w_67 3;
          push_env w_67 (Dynarray.get w_67.state.e 0);
          assert_env_length w_67 4;
          push_env w_67 (Dynarray.get w_67.state.e 1);
          assert_env_length w_67 5;
          let keep_vals_37 = env_call w_67 [ 0; 1 ] 2 in
          w_67.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_vals_37; w_67.state.k ];
          w_67.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (65)")
    65;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 4;
      let cond_9 = resolve w_66 (Source.E 3) in
      ignore (pop_env w_66);
      if Word.get_value (fst cond_9) <> 0 then (
        assert_env_length w_66 3;
        push_env w_66 (Dynarray.get w_66.state.e 1);
        assert_env_length w_66 4;
        drop_n w_66 4 1;
        assert_env_length w_66 3;
        drop_n w_66 3 1;
        assert_env_length w_66 2;
        drop_n w_66 2 1;
        assert_env_length w_66 1;
        drop_n w_66 1 0;
        assert_env_length w_66 1;
        return_n w_66 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_66 3;
        push_env w_66 (Dynarray.get w_66.state.e 1);
        w_66.state.c <- pc_to_exp (int_to_pc 65)))
    66;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 5;
      let x0_20 = resolve w_65 (Source.E 3) in
      let x1_20 = resolve w_65 (Source.E 4) in
      ignore (pop_env w_65);
      ignore (pop_env w_65);
      push_env w_65 (Memo.from_int (if Word.get_value (fst x0_20) = Word.get_value (fst x1_20) then 1 else 0));
      w_65.state.c <- pc_to_exp (int_to_pc 66))
    67;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 4;
      let cond_10 = resolve w_71 (Source.E 3) in
      ignore (pop_env w_71);
      if Word.get_value (fst cond_10) <> 0 then (
        assert_env_length w_71 3;
        push_env w_71 (Dynarray.get w_71.state.e 0);
        assert_env_length w_71 4;
        drop_n w_71 4 1;
        assert_env_length w_71 3;
        drop_n w_71 3 1;
        assert_env_length w_71 2;
        drop_n w_71 2 1;
        assert_env_length w_71 1;
        drop_n w_71 1 0;
        assert_env_length w_71 1;
        return_n w_71 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_71 3;
        push_env w_71 (Dynarray.get w_71.state.e 0);
        assert_env_length w_71 4;
        push_env w_71 (Dynarray.get w_71.state.e 1);
        assert_env_length w_71 5;
        let keep_vals_38 = env_call w_71 [ 0; 1 ] 2 in
        w_71.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_vals_38; w_71.state.k ];
        w_71.state.c <- pc_to_exp (int_to_pc 5)))
    68;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 5;
      let x0_22 = resolve w_70 (Source.E 3) in
      let x1_22 = resolve w_70 (Source.E 4) in
      ignore (pop_env w_70);
      ignore (pop_env w_70);
      push_env w_70 (Memo.from_int (if Word.get_value (fst x0_22) = Word.get_value (fst x1_22) then 1 else 0));
      w_70.state.c <- pc_to_exp (int_to_pc 68))
    69;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 3;
      let last_25 = Source.E 2 in
      let x_25 = resolve w_69 last_25 in
      match Word.get_value (fst x_25) with
      | 3 (* tag_Const *) ->
          let splits_52 = Memo.splits (snd x_25) in
          let split0_52 = List.nth splits_52 0 in
          ignore (pop_env w_69);
          push_env w_69 split0_52;
          assert_env_length w_69 3;
          push_env w_69 (Dynarray.get w_69.state.e 2);
          assert_env_length w_69 4;
          push_env w_69 (Memo.from_int 0);
          w_69.state.c <- pc_to_exp (int_to_pc 69)
      | _ ->
          ignore (pop_env w_69);
          assert_env_length w_69 2;
          push_env w_69 (Dynarray.get w_69.state.e 0);
          assert_env_length w_69 3;
          push_env w_69 (Dynarray.get w_69.state.e 1);
          assert_env_length w_69 4;
          let keep_vals_39 = env_call w_69 [ 0; 1 ] 2 in
          w_69.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_vals_39; w_69.state.k ];
          w_69.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (70)")
    70;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 3;
      let last_23 = Source.E 2 in
      let x_23 = resolve w_64 last_23 in
      match Word.get_value (fst x_23) with
      | 3 (* tag_Const *) ->
          let splits_50 = Memo.splits (snd x_23) in
          let split0_50 = List.nth splits_50 0 in
          ignore (pop_env w_64);
          push_env w_64 split0_50;
          assert_env_length w_64 3;
          push_env w_64 (Dynarray.get w_64.state.e 2);
          assert_env_length w_64 4;
          push_env w_64 (Memo.from_int 0);
          w_64.state.c <- pc_to_exp (int_to_pc 67)
      | _ ->
          ignore (pop_env w_64);
          assert_env_length w_64 2;
          push_env w_64 (Dynarray.get w_64.state.e 1);
          w_64.state.c <- pc_to_exp (int_to_pc 70)
      | _ -> failwith "unreachable (71)")
    71;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 6;
      let x0_27 = resolve w_82 (Source.E 4) in
      let x1_27 = resolve w_82 (Source.E 5) in
      ignore (pop_env w_82);
      ignore (pop_env w_82);
      push_env w_82 (Memo.from_int (Word.get_value (fst x0_27) * Word.get_value (fst x1_27)));
      assert_env_length w_82 5;
      let ctor_arg_20 = pop_env w_82 in
      push_env w_82 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_20 ]);
      assert_env_length w_82 5;
      drop_n w_82 5 1;
      assert_env_length w_82 4;
      drop_n w_82 4 1;
      assert_env_length w_82 3;
      drop_n w_82 3 1;
      assert_env_length w_82 2;
      drop_n w_82 2 1;
      assert_env_length w_82 1;
      drop_n w_82 1 0;
      assert_env_length w_82 1;
      return_n w_82 1 (pc_to_exp (int_to_pc 0)))
    72;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 5;
      let cond_14 = resolve w_81 (Source.E 4) in
      ignore (pop_env w_81);
      if Word.get_value (fst cond_14) <> 0 then (
        assert_env_length w_81 4;
        push_env w_81 (Dynarray.get w_81.state.e 0);
        assert_env_length w_81 5;
        drop_n w_81 5 1;
        assert_env_length w_81 4;
        drop_n w_81 4 1;
        assert_env_length w_81 3;
        drop_n w_81 3 1;
        assert_env_length w_81 2;
        drop_n w_81 2 1;
        assert_env_length w_81 1;
        drop_n w_81 1 0;
        assert_env_length w_81 1;
        return_n w_81 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_81 4;
        push_env w_81 (Dynarray.get w_81.state.e 2);
        assert_env_length w_81 5;
        push_env w_81 (Dynarray.get w_81.state.e 3);
        w_81.state.c <- pc_to_exp (int_to_pc 72)))
    73;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 6;
      let x0_26 = resolve w_80 (Source.E 4) in
      let x1_26 = resolve w_80 (Source.E 5) in
      ignore (pop_env w_80);
      ignore (pop_env w_80);
      push_env w_80 (Memo.from_int (if Word.get_value (fst x0_26) = Word.get_value (fst x1_26) then 1 else 0));
      w_80.state.c <- pc_to_exp (int_to_pc 73))
    74;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 5;
      let cond_13 = resolve w_79 (Source.E 4) in
      ignore (pop_env w_79);
      if Word.get_value (fst cond_13) <> 0 then (
        assert_env_length w_79 4;
        push_env w_79 (Memo.from_int 0);
        assert_env_length w_79 5;
        let ctor_arg_21 = pop_env w_79 in
        push_env w_79 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_21 ]);
        assert_env_length w_79 5;
        drop_n w_79 5 1;
        assert_env_length w_79 4;
        drop_n w_79 4 1;
        assert_env_length w_79 3;
        drop_n w_79 3 1;
        assert_env_length w_79 2;
        drop_n w_79 2 1;
        assert_env_length w_79 1;
        drop_n w_79 1 0;
        assert_env_length w_79 1;
        return_n w_79 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_79 4;
        push_env w_79 (Dynarray.get w_79.state.e 3);
        assert_env_length w_79 5;
        push_env w_79 (Memo.from_int 1);
        w_79.state.c <- pc_to_exp (int_to_pc 74)))
    75;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 6;
      let x0_25 = resolve w_78 (Source.E 4) in
      let x1_25 = resolve w_78 (Source.E 5) in
      ignore (pop_env w_78);
      ignore (pop_env w_78);
      push_env w_78 (Memo.from_int (if Word.get_value (fst x0_25) = Word.get_value (fst x1_25) then 1 else 0));
      w_78.state.c <- pc_to_exp (int_to_pc 75))
    76;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 4;
      let last_27 = Source.E 3 in
      let x_27 = resolve w_77 last_27 in
      match Word.get_value (fst x_27) with
      | 3 (* tag_Const *) ->
          let splits_54 = Memo.splits (snd x_27) in
          let split0_54 = List.nth splits_54 0 in
          ignore (pop_env w_77);
          push_env w_77 split0_54;
          assert_env_length w_77 4;
          push_env w_77 (Dynarray.get w_77.state.e 3);
          assert_env_length w_77 5;
          push_env w_77 (Memo.from_int 0);
          w_77.state.c <- pc_to_exp (int_to_pc 76)
      | _ ->
          ignore (pop_env w_77);
          assert_env_length w_77 3;
          push_env w_77 (Dynarray.get w_77.state.e 0);
          assert_env_length w_77 4;
          push_env w_77 (Dynarray.get w_77.state.e 1);
          assert_env_length w_77 5;
          let keep_vals_40 = env_call w_77 [ 0; 1 ] 2 in
          w_77.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_vals_40; w_77.state.k ];
          w_77.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (77)")
    77;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 4;
      let cond_12 = resolve w_76 (Source.E 3) in
      ignore (pop_env w_76);
      if Word.get_value (fst cond_12) <> 0 then (
        assert_env_length w_76 3;
        push_env w_76 (Dynarray.get w_76.state.e 1);
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
        w_76.state.c <- pc_to_exp (int_to_pc 77)))
    78;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 5;
      let x0_24 = resolve w_75 (Source.E 3) in
      let x1_24 = resolve w_75 (Source.E 4) in
      ignore (pop_env w_75);
      ignore (pop_env w_75);
      push_env w_75 (Memo.from_int (if Word.get_value (fst x0_24) = Word.get_value (fst x1_24) then 1 else 0));
      w_75.state.c <- pc_to_exp (int_to_pc 78))
    79;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 4;
      let cond_11 = resolve w_74 (Source.E 3) in
      ignore (pop_env w_74);
      if Word.get_value (fst cond_11) <> 0 then (
        assert_env_length w_74 3;
        push_env w_74 (Memo.from_int 0);
        assert_env_length w_74 4;
        let ctor_arg_22 = pop_env w_74 in
        push_env w_74 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_22 ]);
        assert_env_length w_74 4;
        drop_n w_74 4 1;
        assert_env_length w_74 3;
        drop_n w_74 3 1;
        assert_env_length w_74 2;
        drop_n w_74 2 1;
        assert_env_length w_74 1;
        drop_n w_74 1 0;
        assert_env_length w_74 1;
        return_n w_74 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_74 3;
        push_env w_74 (Dynarray.get w_74.state.e 2);
        assert_env_length w_74 4;
        push_env w_74 (Memo.from_int 1);
        w_74.state.c <- pc_to_exp (int_to_pc 79)))
    80;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 5;
      let x0_23 = resolve w_73 (Source.E 3) in
      let x1_23 = resolve w_73 (Source.E 4) in
      ignore (pop_env w_73);
      ignore (pop_env w_73);
      push_env w_73 (Memo.from_int (if Word.get_value (fst x0_23) = Word.get_value (fst x1_23) then 1 else 0));
      w_73.state.c <- pc_to_exp (int_to_pc 80))
    81;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 4;
      let cond_16 = resolve w_87 (Source.E 3) in
      ignore (pop_env w_87);
      if Word.get_value (fst cond_16) <> 0 then (
        assert_env_length w_87 3;
        push_env w_87 (Dynarray.get w_87.state.e 0);
        assert_env_length w_87 4;
        drop_n w_87 4 1;
        assert_env_length w_87 3;
        drop_n w_87 3 1;
        assert_env_length w_87 2;
        drop_n w_87 2 1;
        assert_env_length w_87 1;
        drop_n w_87 1 0;
        assert_env_length w_87 1;
        return_n w_87 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_87 3;
        push_env w_87 (Dynarray.get w_87.state.e 0);
        assert_env_length w_87 4;
        push_env w_87 (Dynarray.get w_87.state.e 1);
        assert_env_length w_87 5;
        let keep_vals_41 = env_call w_87 [ 0; 1 ] 2 in
        w_87.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_vals_41; w_87.state.k ];
        w_87.state.c <- pc_to_exp (int_to_pc 5)))
    82;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 5;
      let x0_29 = resolve w_86 (Source.E 3) in
      let x1_29 = resolve w_86 (Source.E 4) in
      ignore (pop_env w_86);
      ignore (pop_env w_86);
      push_env w_86 (Memo.from_int (if Word.get_value (fst x0_29) = Word.get_value (fst x1_29) then 1 else 0));
      w_86.state.c <- pc_to_exp (int_to_pc 82))
    83;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 4;
      let cond_15 = resolve w_85 (Source.E 3) in
      ignore (pop_env w_85);
      if Word.get_value (fst cond_15) <> 0 then (
        assert_env_length w_85 3;
        push_env w_85 (Memo.from_int 0);
        assert_env_length w_85 4;
        let ctor_arg_23 = pop_env w_85 in
        push_env w_85 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_23 ]);
        assert_env_length w_85 4;
        drop_n w_85 4 1;
        assert_env_length w_85 3;
        drop_n w_85 3 1;
        assert_env_length w_85 2;
        drop_n w_85 2 1;
        assert_env_length w_85 1;
        drop_n w_85 1 0;
        assert_env_length w_85 1;
        return_n w_85 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_85 3;
        push_env w_85 (Dynarray.get w_85.state.e 2);
        assert_env_length w_85 4;
        push_env w_85 (Memo.from_int 1);
        w_85.state.c <- pc_to_exp (int_to_pc 83)))
    84;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 5;
      let x0_28 = resolve w_84 (Source.E 3) in
      let x1_28 = resolve w_84 (Source.E 4) in
      ignore (pop_env w_84);
      ignore (pop_env w_84);
      push_env w_84 (Memo.from_int (if Word.get_value (fst x0_28) = Word.get_value (fst x1_28) then 1 else 0));
      w_84.state.c <- pc_to_exp (int_to_pc 84))
    85;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 3;
      let last_28 = Source.E 2 in
      let x_28 = resolve w_83 last_28 in
      match Word.get_value (fst x_28) with
      | 3 (* tag_Const *) ->
          let splits_55 = Memo.splits (snd x_28) in
          let split0_55 = List.nth splits_55 0 in
          ignore (pop_env w_83);
          push_env w_83 split0_55;
          assert_env_length w_83 3;
          push_env w_83 (Dynarray.get w_83.state.e 2);
          assert_env_length w_83 4;
          push_env w_83 (Memo.from_int 0);
          w_83.state.c <- pc_to_exp (int_to_pc 85)
      | _ ->
          ignore (pop_env w_83);
          assert_env_length w_83 2;
          push_env w_83 (Dynarray.get w_83.state.e 0);
          assert_env_length w_83 3;
          push_env w_83 (Dynarray.get w_83.state.e 1);
          assert_env_length w_83 4;
          let keep_vals_42 = env_call w_83 [ 0; 1 ] 2 in
          w_83.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_vals_42; w_83.state.k ];
          w_83.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (86)")
    86;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 3;
      let last_26 = Source.E 2 in
      let x_26 = resolve w_72 last_26 in
      match Word.get_value (fst x_26) with
      | 3 (* tag_Const *) ->
          let splits_53 = Memo.splits (snd x_26) in
          let split0_53 = List.nth splits_53 0 in
          ignore (pop_env w_72);
          push_env w_72 split0_53;
          assert_env_length w_72 3;
          push_env w_72 (Dynarray.get w_72.state.e 2);
          assert_env_length w_72 4;
          push_env w_72 (Memo.from_int 0);
          w_72.state.c <- pc_to_exp (int_to_pc 81)
      | _ ->
          ignore (pop_env w_72);
          assert_env_length w_72 2;
          push_env w_72 (Dynarray.get w_72.state.e 1);
          w_72.state.c <- pc_to_exp (int_to_pc 86)
      | _ -> failwith "unreachable (87)")
    87;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 2;
      let cond_17 = resolve w_88 (Source.E 1) in
      ignore (pop_env w_88);
      if Word.get_value (fst cond_17) <> 0 then (
        assert_env_length w_88 1;
        push_env w_88 (Dynarray.get w_88.state.e 0);
        assert_env_length w_88 2;
        drop_n w_88 2 1;
        assert_env_length w_88 1;
        return_n w_88 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_88 1;
        push_env w_88 (Dynarray.get w_88.state.e 0);
        assert_env_length w_88 2;
        ignore (env_call w_88 [] 1);
        w_88.state.c <- pc_to_exp (int_to_pc 28)))
    88;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 2;
      let x0_30 = resolve w_89 (Source.E 0) in
      let x1_30 = resolve w_89 (Source.E 1) in
      ignore (pop_env w_89);
      ignore (pop_env w_89);
      push_env w_89 (Memo.from_int (Word.get_value (fst x0_30) + Word.get_value (fst x1_30)));
      assert_env_length w_89 1;
      drop_n w_89 1 0;
      assert_env_length w_89 1;
      return_n w_89 1 (pc_to_exp (int_to_pc 0)))
    89;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 2;
      let x0_31 = resolve w_90 (Source.E 0) in
      let x1_31 = resolve w_90 (Source.E 1) in
      ignore (pop_env w_90);
      ignore (pop_env w_90);
      push_env w_90 (Memo.from_int (Word.get_value (fst x0_31) * Word.get_value (fst x1_31)));
      assert_env_length w_90 1;
      drop_n w_90 1 0;
      assert_env_length w_90 1;
      return_n w_90 1 (pc_to_exp (int_to_pc 0)))
    90;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 4;
      let cond_18 = resolve w_92 (Source.E 3) in
      ignore (pop_env w_92);
      if Word.get_value (fst cond_18) <> 0 then (
        assert_env_length w_92 3;
        push_env w_92 (Dynarray.get w_92.state.e 0);
        assert_env_length w_92 4;
        push_env w_92 (Dynarray.get w_92.state.e 1);
        assert_env_length w_92 5;
        ignore (env_call w_92 [] 2);
        w_92.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_92 3;
        push_env w_92 (Dynarray.get w_92.state.e 2);
        assert_env_length w_92 4;
        drop_n w_92 4 1;
        assert_env_length w_92 3;
        drop_n w_92 3 1;
        assert_env_length w_92 2;
        drop_n w_92 2 1;
        assert_env_length w_92 1;
        drop_n w_92 1 0;
        assert_env_length w_92 1;
        drop_n w_92 1 0;
        assert_env_length w_92 1;
        return_n w_92 1 (pc_to_exp (int_to_pc 0))))
    91;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 5;
      let x0_32 = resolve w_91 (Source.E 3) in
      let x1_32 = resolve w_91 (Source.E 4) in
      ignore (pop_env w_91);
      ignore (pop_env w_91);
      push_env w_91 (Memo.from_int (if Word.get_value (fst x0_32) = Word.get_value (fst x1_32) then 1 else 0));
      w_91.state.c <- pc_to_exp (int_to_pc 91))
    92;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 4;
      let cond_19 = resolve w_94 (Source.E 3) in
      ignore (pop_env w_94);
      if Word.get_value (fst cond_19) <> 0 then (
        assert_env_length w_94 3;
        push_env w_94 (Dynarray.get w_94.state.e 0);
        assert_env_length w_94 4;
        push_env w_94 (Dynarray.get w_94.state.e 1);
        assert_env_length w_94 5;
        ignore (env_call w_94 [] 2);
        w_94.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_94 3;
        push_env w_94 (Dynarray.get w_94.state.e 2);
        assert_env_length w_94 4;
        drop_n w_94 4 1;
        assert_env_length w_94 3;
        drop_n w_94 3 1;
        assert_env_length w_94 2;
        drop_n w_94 2 1;
        assert_env_length w_94 1;
        drop_n w_94 1 0;
        assert_env_length w_94 1;
        drop_n w_94 1 0;
        assert_env_length w_94 1;
        return_n w_94 1 (pc_to_exp (int_to_pc 0))))
    93;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 5;
      let x0_33 = resolve w_93 (Source.E 3) in
      let x1_33 = resolve w_93 (Source.E 4) in
      ignore (pop_env w_93);
      ignore (pop_env w_93);
      push_env w_93 (Memo.from_int (if Word.get_value (fst x0_33) = Word.get_value (fst x1_33) then 1 else 0));
      w_93.state.c <- pc_to_exp (int_to_pc 93))
    94;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 4;
      let last_29 = Source.E 3 in
      let x_29 = resolve w_97 last_29 in
      match Word.get_value (fst x_29) with
      | 5 (* tag_Add *) ->
          let splits_56 = Memo.splits (snd x_29) in
          let split0_56 = List.nth splits_56 0 in
          let split1_16 = List.nth splits_56 1 in
          ignore (pop_env w_97);
          push_env w_97 split0_56;
          push_env w_97 split1_16;
          assert_env_length w_97 5;
          push_env w_97 (Dynarray.get w_97.state.e 3);
          assert_env_length w_97 6;
          push_env w_97 (Dynarray.get w_97.state.e 4);
          assert_env_length w_97 7;
          let keep_vals_44 = env_call w_97 [ 2; 3 ] 2 in
          w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_vals_44; w_97.state.k ];
          w_97.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (95)")
    95;
  add_exp
    (fun w_98 ->
      assert_env_length w_98 4;
      let last_30 = Source.E 3 in
      let x_30 = resolve w_98 last_30 in
      match Word.get_value (fst x_30) with
      | 5 (* tag_Add *) ->
          let splits_57 = Memo.splits (snd x_30) in
          let split0_57 = List.nth splits_57 0 in
          let split1_17 = List.nth splits_57 1 in
          ignore (pop_env w_98);
          push_env w_98 split0_57;
          push_env w_98 split1_17;
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 3);
          assert_env_length w_98 6;
          push_env w_98 (Dynarray.get w_98.state.e 4);
          assert_env_length w_98 7;
          let keep_vals_45 = env_call w_98 [ 2; 3 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_vals_45; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (96)")
    96;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 3;
      let cond_20 = resolve w_96 (Source.E 2) in
      ignore (pop_env w_96);
      if Word.get_value (fst cond_20) <> 0 then (
        assert_env_length w_96 2;
        push_env w_96 (Dynarray.get w_96.state.e 0);
        assert_env_length w_96 3;
        push_env w_96 (Dynarray.get w_96.state.e 1);
        assert_env_length w_96 4;
        let ctor_arg_34 = pop_env w_96 in
        let ctor_arg_35 = pop_env w_96 in
        push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_35; ctor_arg_34 ]);
        assert_env_length w_96 3;
        push_env w_96 (Dynarray.get w_96.state.e 2);
        w_96.state.c <- pc_to_exp (int_to_pc 96))
      else (
        assert_env_length w_96 2;
        push_env w_96 (Dynarray.get w_96.state.e 1);
        assert_env_length w_96 3;
        push_env w_96 (Dynarray.get w_96.state.e 0);
        assert_env_length w_96 4;
        let ctor_arg_32 = pop_env w_96 in
        let ctor_arg_33 = pop_env w_96 in
        push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_33; ctor_arg_32 ]);
        assert_env_length w_96 3;
        push_env w_96 (Dynarray.get w_96.state.e 2);
        w_96.state.c <- pc_to_exp (int_to_pc 95)))
    97;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 4;
      let x0_34 = resolve w_95 (Source.E 2) in
      let x1_34 = resolve w_95 (Source.E 3) in
      ignore (pop_env w_95);
      ignore (pop_env w_95);
      push_env w_95 (Memo.from_int (if Word.get_value (fst x0_34) <= Word.get_value (fst x1_34) then 1 else 0));
      w_95.state.c <- pc_to_exp (int_to_pc 97))
    98;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 3;
      let cond_21 = resolve w_100 (Source.E 2) in
      ignore (pop_env w_100);
      if Word.get_value (fst cond_21) <> 0 then (
        assert_env_length w_100 2;
        push_env w_100 (Dynarray.get w_100.state.e 0);
        assert_env_length w_100 3;
        push_env w_100 (Dynarray.get w_100.state.e 1);
        assert_env_length w_100 4;
        let ctor_arg_38 = pop_env w_100 in
        let ctor_arg_39 = pop_env w_100 in
        push_env w_100 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_39; ctor_arg_38 ]);
        assert_env_length w_100 3;
        drop_n w_100 3 0;
        assert_env_length w_100 3;
        drop_n w_100 3 1;
        assert_env_length w_100 2;
        drop_n w_100 2 1;
        assert_env_length w_100 1;
        drop_n w_100 1 0;
        assert_env_length w_100 1;
        return_n w_100 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_100 2;
        push_env w_100 (Dynarray.get w_100.state.e 1);
        assert_env_length w_100 3;
        push_env w_100 (Dynarray.get w_100.state.e 0);
        assert_env_length w_100 4;
        let ctor_arg_36 = pop_env w_100 in
        let ctor_arg_37 = pop_env w_100 in
        push_env w_100 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_37; ctor_arg_36 ]);
        assert_env_length w_100 3;
        drop_n w_100 3 0;
        assert_env_length w_100 3;
        drop_n w_100 3 1;
        assert_env_length w_100 2;
        drop_n w_100 2 1;
        assert_env_length w_100 1;
        drop_n w_100 1 0;
        assert_env_length w_100 1;
        return_n w_100 1 (pc_to_exp (int_to_pc 0))))
    99;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 4;
      let x0_35 = resolve w_99 (Source.E 2) in
      let x1_35 = resolve w_99 (Source.E 3) in
      ignore (pop_env w_99);
      ignore (pop_env w_99);
      push_env w_99 (Memo.from_int (if Word.get_value (fst x0_35) <= Word.get_value (fst x1_35) then 1 else 0));
      w_99.state.c <- pc_to_exp (int_to_pc 99))
    100;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 4;
      let last_31 = Source.E 3 in
      let x_31 = resolve w_103 last_31 in
      match Word.get_value (fst x_31) with
      | 5 (* tag_Add *) ->
          let splits_58 = Memo.splits (snd x_31) in
          let split0_58 = List.nth splits_58 0 in
          let split1_18 = List.nth splits_58 1 in
          ignore (pop_env w_103);
          push_env w_103 split0_58;
          push_env w_103 split1_18;
          assert_env_length w_103 5;
          push_env w_103 (Dynarray.get w_103.state.e 3);
          assert_env_length w_103 6;
          push_env w_103 (Dynarray.get w_103.state.e 4);
          assert_env_length w_103 7;
          let keep_vals_46 = env_call w_103 [ 2; 3 ] 2 in
          w_103.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_vals_46; w_103.state.k ];
          w_103.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (101)")
    101;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 4;
      let last_32 = Source.E 3 in
      let x_32 = resolve w_104 last_32 in
      match Word.get_value (fst x_32) with
      | 5 (* tag_Add *) ->
          let splits_59 = Memo.splits (snd x_32) in
          let split0_59 = List.nth splits_59 0 in
          let split1_19 = List.nth splits_59 1 in
          ignore (pop_env w_104);
          push_env w_104 split0_59;
          push_env w_104 split1_19;
          assert_env_length w_104 5;
          push_env w_104 (Dynarray.get w_104.state.e 3);
          assert_env_length w_104 6;
          push_env w_104 (Dynarray.get w_104.state.e 4);
          assert_env_length w_104 7;
          let keep_vals_47 = env_call w_104 [ 2; 3 ] 2 in
          w_104.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_vals_47; w_104.state.k ];
          w_104.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (102)")
    102;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 3;
      let cond_22 = resolve w_102 (Source.E 2) in
      ignore (pop_env w_102);
      if Word.get_value (fst cond_22) <> 0 then (
        assert_env_length w_102 2;
        push_env w_102 (Dynarray.get w_102.state.e 0);
        assert_env_length w_102 3;
        push_env w_102 (Dynarray.get w_102.state.e 1);
        assert_env_length w_102 4;
        let ctor_arg_42 = pop_env w_102 in
        let ctor_arg_43 = pop_env w_102 in
        push_env w_102 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_43; ctor_arg_42 ]);
        assert_env_length w_102 3;
        push_env w_102 (Dynarray.get w_102.state.e 2);
        w_102.state.c <- pc_to_exp (int_to_pc 102))
      else (
        assert_env_length w_102 2;
        push_env w_102 (Dynarray.get w_102.state.e 1);
        assert_env_length w_102 3;
        push_env w_102 (Dynarray.get w_102.state.e 0);
        assert_env_length w_102 4;
        let ctor_arg_40 = pop_env w_102 in
        let ctor_arg_41 = pop_env w_102 in
        push_env w_102 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_41; ctor_arg_40 ]);
        assert_env_length w_102 3;
        push_env w_102 (Dynarray.get w_102.state.e 2);
        w_102.state.c <- pc_to_exp (int_to_pc 101)))
    103;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 4;
      let x0_36 = resolve w_101 (Source.E 2) in
      let x1_36 = resolve w_101 (Source.E 3) in
      ignore (pop_env w_101);
      ignore (pop_env w_101);
      push_env w_101 (Memo.from_int (if Word.get_value (fst x0_36) <= Word.get_value (fst x1_36) then 1 else 0));
      w_101.state.c <- pc_to_exp (int_to_pc 103))
    104;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 3;
      let cond_23 = resolve w_106 (Source.E 2) in
      ignore (pop_env w_106);
      if Word.get_value (fst cond_23) <> 0 then (
        assert_env_length w_106 2;
        push_env w_106 (Dynarray.get w_106.state.e 0);
        assert_env_length w_106 3;
        push_env w_106 (Dynarray.get w_106.state.e 1);
        assert_env_length w_106 4;
        let ctor_arg_46 = pop_env w_106 in
        let ctor_arg_47 = pop_env w_106 in
        push_env w_106 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_47; ctor_arg_46 ]);
        assert_env_length w_106 3;
        drop_n w_106 3 0;
        assert_env_length w_106 3;
        drop_n w_106 3 1;
        assert_env_length w_106 2;
        drop_n w_106 2 1;
        assert_env_length w_106 1;
        drop_n w_106 1 0;
        assert_env_length w_106 1;
        return_n w_106 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_106 2;
        push_env w_106 (Dynarray.get w_106.state.e 1);
        assert_env_length w_106 3;
        push_env w_106 (Dynarray.get w_106.state.e 0);
        assert_env_length w_106 4;
        let ctor_arg_44 = pop_env w_106 in
        let ctor_arg_45 = pop_env w_106 in
        push_env w_106 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_45; ctor_arg_44 ]);
        assert_env_length w_106 3;
        drop_n w_106 3 0;
        assert_env_length w_106 3;
        drop_n w_106 3 1;
        assert_env_length w_106 2;
        drop_n w_106 2 1;
        assert_env_length w_106 1;
        drop_n w_106 1 0;
        assert_env_length w_106 1;
        return_n w_106 1 (pc_to_exp (int_to_pc 0))))
    105;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 4;
      let x0_37 = resolve w_105 (Source.E 2) in
      let x1_37 = resolve w_105 (Source.E 3) in
      ignore (pop_env w_105);
      ignore (pop_env w_105);
      push_env w_105 (Memo.from_int (if Word.get_value (fst x0_37) <= Word.get_value (fst x1_37) then 1 else 0));
      w_105.state.c <- pc_to_exp (int_to_pc 105))
    106;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 3;
      let cond_24 = resolve w_108 (Source.E 2) in
      ignore (pop_env w_108);
      if Word.get_value (fst cond_24) <> 0 then (
        assert_env_length w_108 2;
        push_env w_108 (Dynarray.get w_108.state.e 0);
        assert_env_length w_108 3;
        push_env w_108 (Dynarray.get w_108.state.e 1);
        assert_env_length w_108 4;
        let ctor_arg_50 = pop_env w_108 in
        let ctor_arg_51 = pop_env w_108 in
        push_env w_108 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_51; ctor_arg_50 ]);
        assert_env_length w_108 3;
        drop_n w_108 3 0;
        assert_env_length w_108 3;
        drop_n w_108 3 1;
        assert_env_length w_108 2;
        drop_n w_108 2 1;
        assert_env_length w_108 1;
        drop_n w_108 1 0;
        assert_env_length w_108 1;
        return_n w_108 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_108 2;
        push_env w_108 (Dynarray.get w_108.state.e 1);
        assert_env_length w_108 3;
        push_env w_108 (Dynarray.get w_108.state.e 0);
        assert_env_length w_108 4;
        let ctor_arg_48 = pop_env w_108 in
        let ctor_arg_49 = pop_env w_108 in
        push_env w_108 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_49; ctor_arg_48 ]);
        assert_env_length w_108 3;
        drop_n w_108 3 0;
        assert_env_length w_108 3;
        drop_n w_108 3 1;
        assert_env_length w_108 2;
        drop_n w_108 2 1;
        assert_env_length w_108 1;
        drop_n w_108 1 0;
        assert_env_length w_108 1;
        return_n w_108 1 (pc_to_exp (int_to_pc 0))))
    107;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 4;
      let x0_38 = resolve w_107 (Source.E 2) in
      let x1_38 = resolve w_107 (Source.E 3) in
      ignore (pop_env w_107);
      ignore (pop_env w_107);
      push_env w_107 (Memo.from_int (if Word.get_value (fst x0_38) <= Word.get_value (fst x1_38) then 1 else 0));
      w_107.state.c <- pc_to_exp (int_to_pc 107))
    108;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 3;
      let cond_25 = resolve w_110 (Source.E 2) in
      ignore (pop_env w_110);
      if Word.get_value (fst cond_25) <> 0 then (
        assert_env_length w_110 2;
        push_env w_110 (Dynarray.get w_110.state.e 0);
        assert_env_length w_110 3;
        push_env w_110 (Dynarray.get w_110.state.e 1);
        assert_env_length w_110 4;
        let ctor_arg_54 = pop_env w_110 in
        let ctor_arg_55 = pop_env w_110 in
        push_env w_110 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_55; ctor_arg_54 ]);
        assert_env_length w_110 3;
        drop_n w_110 3 1;
        assert_env_length w_110 2;
        drop_n w_110 2 1;
        assert_env_length w_110 1;
        drop_n w_110 1 0;
        assert_env_length w_110 1;
        return_n w_110 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_110 2;
        push_env w_110 (Dynarray.get w_110.state.e 1);
        assert_env_length w_110 3;
        push_env w_110 (Dynarray.get w_110.state.e 0);
        assert_env_length w_110 4;
        let ctor_arg_52 = pop_env w_110 in
        let ctor_arg_53 = pop_env w_110 in
        push_env w_110 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_53; ctor_arg_52 ]);
        assert_env_length w_110 3;
        drop_n w_110 3 1;
        assert_env_length w_110 2;
        drop_n w_110 2 1;
        assert_env_length w_110 1;
        drop_n w_110 1 0;
        assert_env_length w_110 1;
        return_n w_110 1 (pc_to_exp (int_to_pc 0))))
    109;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 4;
      let x0_39 = resolve w_109 (Source.E 2) in
      let x1_39 = resolve w_109 (Source.E 3) in
      ignore (pop_env w_109);
      ignore (pop_env w_109);
      push_env w_109 (Memo.from_int (if Word.get_value (fst x0_39) <= Word.get_value (fst x1_39) then 1 else 0));
      w_109.state.c <- pc_to_exp (int_to_pc 109))
    110;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 3;
      let cond_27 = resolve w_114 (Source.E 2) in
      ignore (pop_env w_114);
      if Word.get_value (fst cond_27) <> 0 then (
        assert_env_length w_114 2;
        push_env w_114 (Memo.from_int 1);
        assert_env_length w_114 3;
        drop_n w_114 3 1;
        assert_env_length w_114 2;
        drop_n w_114 2 1;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        return_n w_114 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_114 2;
        push_env w_114 (Memo.from_int 0);
        assert_env_length w_114 3;
        drop_n w_114 3 1;
        assert_env_length w_114 2;
        drop_n w_114 2 1;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        drop_n w_114 1 0;
        assert_env_length w_114 1;
        return_n w_114 1 (pc_to_exp (int_to_pc 0))))
    111;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 4;
      let x0_41 = resolve w_113 (Source.E 2) in
      let x1_41 = resolve w_113 (Source.E 3) in
      ignore (pop_env w_113);
      ignore (pop_env w_113);
      push_env w_113 (Memo.from_int (if Word.get_value (fst x0_41) > Word.get_value (fst x1_41) then 1 else 0));
      w_113.state.c <- pc_to_exp (int_to_pc 111))
    112;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 4;
      let x0_42 = resolve w_115 (Source.E 2) in
      let x1_42 = resolve w_115 (Source.E 3) in
      ignore (pop_env w_115);
      ignore (pop_env w_115);
      push_env w_115 (Memo.from_int (Word.get_value (fst x0_42) - Word.get_value (fst x1_42)));
      assert_env_length w_115 3;
      drop_n w_115 3 1;
      assert_env_length w_115 2;
      drop_n w_115 2 1;
      assert_env_length w_115 1;
      drop_n w_115 1 0;
      assert_env_length w_115 1;
      drop_n w_115 1 0;
      assert_env_length w_115 1;
      drop_n w_115 1 0;
      assert_env_length w_115 1;
      drop_n w_115 1 0;
      assert_env_length w_115 1;
      return_n w_115 1 (pc_to_exp (int_to_pc 0)))
    113;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 3;
      let cond_26 = resolve w_112 (Source.E 2) in
      ignore (pop_env w_112);
      if Word.get_value (fst cond_26) <> 0 then (
        assert_env_length w_112 2;
        push_env w_112 (Memo.from_int 0);
        assert_env_length w_112 3;
        push_env w_112 (Memo.from_int 1);
        w_112.state.c <- pc_to_exp (int_to_pc 113))
      else (
        assert_env_length w_112 2;
        push_env w_112 (Dynarray.get w_112.state.e 0);
        assert_env_length w_112 3;
        push_env w_112 (Dynarray.get w_112.state.e 1);
        w_112.state.c <- pc_to_exp (int_to_pc 112)))
    114;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 4;
      let x0_40 = resolve w_111 (Source.E 2) in
      let x1_40 = resolve w_111 (Source.E 3) in
      ignore (pop_env w_111);
      ignore (pop_env w_111);
      push_env w_111 (Memo.from_int (if Word.get_value (fst x0_40) < Word.get_value (fst x1_40) then 1 else 0));
      w_111.state.c <- pc_to_exp (int_to_pc 114))
    115;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 3;
      let cond_28 = resolve w_116 (Source.E 2) in
      ignore (pop_env w_116);
      if Word.get_value (fst cond_28) <> 0 then (
        assert_env_length w_116 2;
        push_env w_116 (Memo.from_int 2);
        assert_env_length w_116 3;
        let ctor_arg_56 = pop_env w_116 in
        push_env w_116 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_56 ]);
        assert_env_length w_116 3;
        push_env w_116 (Dynarray.get w_116.state.e 1);
        assert_env_length w_116 4;
        let ctor_arg_57 = pop_env w_116 in
        let ctor_arg_58 = pop_env w_116 in
        push_env w_116 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_58; ctor_arg_57 ]);
        assert_env_length w_116 3;
        drop_n w_116 3 1;
        assert_env_length w_116 2;
        drop_n w_116 2 1;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        return_n w_116 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_116 2;
        push_env w_116 (Dynarray.get w_116.state.e 0);
        assert_env_length w_116 3;
        drop_n w_116 3 1;
        assert_env_length w_116 2;
        drop_n w_116 2 1;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        drop_n w_116 1 0;
        assert_env_length w_116 1;
        return_n w_116 1 (pc_to_exp (int_to_pc 0))))
    116;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 3;
      let cond_29 = resolve w_117 (Source.E 2) in
      ignore (pop_env w_117);
      if Word.get_value (fst cond_29) <> 0 then (
        assert_env_length w_117 2;
        push_env w_117 (Memo.from_int 2);
        assert_env_length w_117 3;
        let ctor_arg_59 = pop_env w_117 in
        push_env w_117 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_59 ]);
        assert_env_length w_117 3;
        push_env w_117 (Dynarray.get w_117.state.e 1);
        assert_env_length w_117 4;
        let ctor_arg_60 = pop_env w_117 in
        let ctor_arg_61 = pop_env w_117 in
        push_env w_117 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_61; ctor_arg_60 ]);
        assert_env_length w_117 3;
        drop_n w_117 3 1;
        assert_env_length w_117 2;
        drop_n w_117 2 1;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        return_n w_117 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_117 2;
        push_env w_117 (Dynarray.get w_117.state.e 0);
        assert_env_length w_117 3;
        drop_n w_117 3 1;
        assert_env_length w_117 2;
        drop_n w_117 2 1;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        drop_n w_117 1 0;
        assert_env_length w_117 1;
        return_n w_117 1 (pc_to_exp (int_to_pc 0))))
    117;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 3;
      let cond_30 = resolve w_118 (Source.E 2) in
      ignore (pop_env w_118);
      if Word.get_value (fst cond_30) <> 0 then (
        assert_env_length w_118 2;
        push_env w_118 (Memo.from_int 2);
        assert_env_length w_118 3;
        let ctor_arg_62 = pop_env w_118 in
        push_env w_118 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_62 ]);
        assert_env_length w_118 3;
        push_env w_118 (Dynarray.get w_118.state.e 1);
        assert_env_length w_118 4;
        let ctor_arg_63 = pop_env w_118 in
        let ctor_arg_64 = pop_env w_118 in
        push_env w_118 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_64; ctor_arg_63 ]);
        assert_env_length w_118 3;
        drop_n w_118 3 1;
        assert_env_length w_118 2;
        drop_n w_118 2 1;
        assert_env_length w_118 1;
        drop_n w_118 1 0;
        assert_env_length w_118 1;
        drop_n w_118 1 0;
        assert_env_length w_118 1;
        drop_n w_118 1 0;
        assert_env_length w_118 1;
        return_n w_118 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_118 2;
        push_env w_118 (Dynarray.get w_118.state.e 0);
        assert_env_length w_118 3;
        drop_n w_118 3 1;
        assert_env_length w_118 2;
        drop_n w_118 2 1;
        assert_env_length w_118 1;
        drop_n w_118 1 0;
        assert_env_length w_118 1;
        drop_n w_118 1 0;
        assert_env_length w_118 1;
        drop_n w_118 1 0;
        assert_env_length w_118 1;
        return_n w_118 1 (pc_to_exp (int_to_pc 0))))
    118;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 3;
      let cond_31 = resolve w_119 (Source.E 2) in
      ignore (pop_env w_119);
      if Word.get_value (fst cond_31) <> 0 then (
        assert_env_length w_119 2;
        push_env w_119 (Memo.from_int 2);
        assert_env_length w_119 3;
        let ctor_arg_65 = pop_env w_119 in
        push_env w_119 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_65 ]);
        assert_env_length w_119 3;
        push_env w_119 (Dynarray.get w_119.state.e 1);
        assert_env_length w_119 4;
        let ctor_arg_66 = pop_env w_119 in
        let ctor_arg_67 = pop_env w_119 in
        push_env w_119 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_67; ctor_arg_66 ]);
        assert_env_length w_119 3;
        drop_n w_119 3 1;
        assert_env_length w_119 2;
        drop_n w_119 2 1;
        assert_env_length w_119 1;
        drop_n w_119 1 0;
        assert_env_length w_119 1;
        drop_n w_119 1 0;
        assert_env_length w_119 1;
        drop_n w_119 1 0;
        assert_env_length w_119 1;
        return_n w_119 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_119 2;
        push_env w_119 (Dynarray.get w_119.state.e 0);
        assert_env_length w_119 3;
        drop_n w_119 3 1;
        assert_env_length w_119 2;
        drop_n w_119 2 1;
        assert_env_length w_119 1;
        drop_n w_119 1 0;
        assert_env_length w_119 1;
        drop_n w_119 1 0;
        assert_env_length w_119 1;
        drop_n w_119 1 0;
        assert_env_length w_119 1;
        return_n w_119 1 (pc_to_exp (int_to_pc 0))))
    119;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 1;
  Words.set_constructor_degree 3 0;
  Words.set_constructor_degree 4 0;
  Words.set_constructor_degree 5 (-1);
  Words.set_constructor_degree 6 (-1);
  Words.set_constructor_degree 7 0;
  Words.set_constructor_degree 8 0;
  Words.set_constructor_degree 9 (-2);
  Words.set_constructor_degree 10 (-1);
  Words.set_constructor_degree 11 (-1);
  Words.set_constructor_degree 12 (-1);
  Words.set_constructor_degree 13 (-2);
  Words.set_constructor_degree 14 (-2);
  Words.set_constructor_degree 15 (-1);
  Words.set_constructor_degree 16 (-1);
  Words.set_constructor_degree 17 0;
  Words.set_constructor_degree 18 0;
  Words.set_constructor_degree 19 (-1);
  Words.set_constructor_degree 20 (-1);
  Words.set_constructor_degree 21 (-1);
  Words.set_constructor_degree 22 (-2);
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 (-1);
  Words.set_constructor_degree 25 (-3);
  Words.set_constructor_degree 26 (-3);
  Words.set_constructor_degree 27 0;
  Words.set_constructor_degree 28 0;
  Words.set_constructor_degree 29 (-3);
  Words.set_constructor_degree 30 (-1);
  Words.set_constructor_degree 31 (-1);
  Words.set_constructor_degree 32 (-1);
  Words.set_constructor_degree 33 (-1);
  Words.set_constructor_degree 34 (-1);
  Words.set_constructor_degree 35 0;
  Words.set_constructor_degree 36 0;
  Words.set_constructor_degree 37 (-1);
  Words.set_constructor_degree 38 (-1);
  Words.set_constructor_degree 39 (-1);
  Words.set_constructor_degree 40 (-2);
  Words.set_constructor_degree 41 (-1);
  Words.set_constructor_degree 42 (-1);
  Words.set_constructor_degree 43 (-1);
  Words.set_constructor_degree 44 (-2);
  Words.set_constructor_degree 45 (-2);
  Words.set_constructor_degree 46 (-2);
  Words.set_constructor_degree 47 (-2);
  Words.set_constructor_degree 48 (-2);
  Words.set_constructor_degree 49 (-2);
  Words.set_constructor_degree 50 (-2);
  Words.set_constructor_degree 51 (-2);
  Words.set_constructor_degree 52 (-1);
  Words.set_constructor_degree 53 (-2);
  Words.set_constructor_degree 54 (-2);
  Words.set_constructor_degree 55 (-2);
  Words.set_constructor_degree 56 (-2)
