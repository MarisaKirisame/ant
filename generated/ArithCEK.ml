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
let tag_cont_0 = 7
let tag_cont_1 = 8
let tag_cont_2 = 9
let tag_cont_3 = 10
let tag_cont_4 = 11
let tag_cont_5 = 12
let tag_cont_6 = 13
let tag_cont_7 = 14
let tag_cont_8 = 15
let tag_cont_9 = 16
let tag_cont_10 = 17
let tag_cont_11 = 18
let tag_cont_12 = 19
let tag_cont_13 = 20
let tag_cont_14 = 21
let tag_cont_15 = 22
let tag_cont_16 = 23
let tag_cont_17 = 24
let tag_cont_18 = 25
let tag_cont_19 = 26
let tag_cont_20 = 27
let tag_cont_21 = 28
let tag_cont_22 = 29
let tag_cont_23 = 30
let tag_cont_24 = 31
let tag_cont_25 = 32
let tag_cont_26 = 33
let tag_cont_27 = 34
let tag_cont_28 = 35

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

let var_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let expr_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 8)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let compare_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 9)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let expr_equal memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 46)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let normalize memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 92)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let simplify_aux memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 93)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let diffx memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 102)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 109)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let main memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 110)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_139 ->
      assert_env_length w_139 1;
      let hd_0, tl_0 = resolve w_139 K in
      match Word.get_value hd_0 with
      | 7 (* tag_cont_0 *) ->
          let ret_0 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 1 ] tl_0;
          set_env_slot w_139 0 ret_0;
          w_139.state.c <- pc_to_exp (int_to_pc 112)
      | 8 (* tag_cont_1 *) ->
          let ret_1 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0 ] tl_0;
          set_env_slot w_139 1 ret_1;
          w_139.state.c <- pc_to_exp (int_to_pc 111)
      | 9 (* tag_cont_2 *) ->
          let ret_2 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 2 ] tl_0;
          set_env_slot w_139 0 ret_2;
          w_139.state.c <- pc_to_exp (int_to_pc 114)
      | 10 (* tag_cont_3 *) ->
          let ret_3 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1 ] tl_0;
          set_env_slot w_139 2 ret_3;
          w_139.state.c <- pc_to_exp (int_to_pc 113)
      | 11 (* tag_cont_4 *) ->
          let ret_4 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 2 ] tl_0;
          set_env_slot w_139 0 ret_4;
          w_139.state.c <- pc_to_exp (int_to_pc 116)
      | 12 (* tag_cont_5 *) ->
          let ret_5 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1 ] tl_0;
          set_env_slot w_139 2 ret_5;
          w_139.state.c <- pc_to_exp (int_to_pc 115)
      | 13 (* tag_cont_6 *) ->
          let ret_6 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 2 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0 ] tl_0;
          set_env_slot w_139 1 ret_6;
          w_139.state.c <- pc_to_exp (int_to_pc 118)
      | 14 (* tag_cont_7 *) ->
          let ret_7 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 2 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 1 ] tl_0;
          set_env_slot w_139 0 ret_7;
          w_139.state.c <- pc_to_exp (int_to_pc 117)
      | 15 (* tag_cont_8 *) ->
          let ret_8 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 1 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [] tl_0;
          set_env_slot w_139 0 ret_8;
          w_139.state.c <- pc_to_exp (int_to_pc 120)
      | 16 (* tag_cont_9 *) ->
          let ret_9 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 1 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [] tl_0;
          set_env_slot w_139 0 ret_9;
          w_139.state.c <- pc_to_exp (int_to_pc 119)
      | 17 (* tag_cont_10 *) ->
          let ret_10 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 3 ] tl_0;
          set_env_slot w_139 0 ret_10;
          w_139.state.c <- pc_to_exp (int_to_pc 122)
      | 18 (* tag_cont_11 *) ->
          let ret_11 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1; 2 ] tl_0;
          set_env_slot w_139 3 ret_11;
          w_139.state.c <- pc_to_exp (int_to_pc 121)
      | 19 (* tag_cont_12 *) ->
          let ret_12 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 3 ] tl_0;
          set_env_slot w_139 0 ret_12;
          w_139.state.c <- pc_to_exp (int_to_pc 124)
      | 20 (* tag_cont_13 *) ->
          let ret_13 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 4 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1; 2 ] tl_0;
          set_env_slot w_139 3 ret_13;
          w_139.state.c <- pc_to_exp (int_to_pc 123)
      | 21 (* tag_cont_14 *) ->
          let ret_14 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 2 ] tl_0;
          set_env_slot w_139 1 ret_14;
          w_139.state.c <- pc_to_exp (int_to_pc 127)
      | 22 (* tag_cont_15 *) ->
          let ret_15 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0 ] tl_0;
          set_env_slot w_139 2 ret_15;
          w_139.state.c <- pc_to_exp (int_to_pc 126)
      | 23 (* tag_cont_16 *) ->
          let ret_16 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 1 ] tl_0;
          set_env_slot w_139 0 ret_16;
          w_139.state.c <- pc_to_exp (int_to_pc 125)
      | 24 (* tag_cont_17 *) ->
          let ret_17 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 2 ] tl_0;
          set_env_slot w_139 1 ret_17;
          w_139.state.c <- pc_to_exp (int_to_pc 129)
      | 25 (* tag_cont_18 *) ->
          let ret_18 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 2 ] tl_0;
          set_env_slot w_139 0 ret_18;
          w_139.state.c <- pc_to_exp (int_to_pc 128)
      | 26 (* tag_cont_19 *) ->
          let ret_19 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1; 2 ] tl_0;
          set_env_slot w_139 3 ret_19;
          w_139.state.c <- pc_to_exp (int_to_pc 131)
      | 27 (* tag_cont_20 *) ->
          let ret_20 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1 ] tl_0;
          set_env_slot w_139 2 ret_20;
          w_139.state.c <- pc_to_exp (int_to_pc 130)
      | 28 (* tag_cont_21 *) ->
          let ret_21 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0 ] tl_0;
          set_env_slot w_139 1 ret_21;
          w_139.state.c <- pc_to_exp (int_to_pc 133)
      | 29 (* tag_cont_22 *) ->
          let ret_22 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 2 ] tl_0;
          set_env_slot w_139 0 ret_22;
          w_139.state.c <- pc_to_exp (int_to_pc 132)
      | 30 (* tag_cont_23 *) ->
          let ret_23 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1 ] tl_0;
          set_env_slot w_139 2 ret_23;
          w_139.state.c <- pc_to_exp (int_to_pc 134)
      | 31 (* tag_cont_24 *) ->
          let ret_24 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 5 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1 ] tl_0;
          set_env_slot w_139 2 ret_24;
          w_139.state.c <- pc_to_exp (int_to_pc 135)
      | 32 (* tag_cont_25 *) ->
          let ret_25 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 3 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 1 ] tl_0;
          set_env_slot w_139 0 ret_25;
          w_139.state.c <- pc_to_exp (int_to_pc 137)
      | 33 (* tag_cont_26 *) ->
          let ret_26 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 3 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0 ] tl_0;
          set_env_slot w_139 1 ret_26;
          w_139.state.c <- pc_to_exp (int_to_pc 136)
      | 34 (* tag_cont_27 *) ->
          let ret_27 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 3 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 1 ] tl_0;
          set_env_slot w_139 2 ret_27;
          w_139.state.c <- pc_to_exp (int_to_pc 139)
      | 35 (* tag_cont_28 *) ->
          let ret_28 = get_env_slot w_139 0 in
          assert_env_length w_139 1;
          w_139.state.k <- get_next_cont tl_0;
          init_frame w_139 3 (Memo.from_constructor tag_cont_done);
          restore_env_slots w_139 [ 0; 2 ] tl_0;
          set_env_slot w_139 1 ret_28;
          w_139.state.c <- pc_to_exp (int_to_pc 138)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 1;
      return_value w_94 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    1;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 1;
      return_value w_95 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    2;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 1;
      assert_env_length w_96 1;
      match Memo.list_match (get_env_slot w_96 0) with
      | None -> failwith "unreachable (3)"
      | Some pair_18 -> (
          let tag_18 = Word.get_value (fst pair_18) in
          match tag_18 with
          | 1 (* tag_X *) -> w_96.state.c <- pc_to_exp (int_to_pc 1)
          | 2 (* tag_Y *) -> w_96.state.c <- pc_to_exp (int_to_pc 2)
          | _ -> failwith "unreachable (3)"))
    3;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 1;
      return_value w_38 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    4;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 1;
      return_value w_39 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    5;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 1;
      return_value w_40 (Memo.from_int 2) (pc_to_exp (int_to_pc 0)))
    6;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 1;
      return_value w_41 (Memo.from_int 3) (pc_to_exp (int_to_pc 0)))
    7;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 1;
      assert_env_length w_42 1;
      match Memo.list_match (get_env_slot w_42 0) with
      | None -> failwith "unreachable (8)"
      | Some pair_7 -> (
          let tag_7 = Word.get_value (fst pair_7) in
          match tag_7 with
          | 3 (* tag_Const *) ->
              let parts_12 = Memo.splits (snd pair_7) in
              if List.length parts_12 = 1 then
                let part0_12 = List.nth parts_12 0 in
                w_42.state.c <- pc_to_exp (int_to_pc 4)
              else failwith "unreachable (8)"
          | 4 (* tag_Var *) ->
              let parts_13 = Memo.splits (snd pair_7) in
              if List.length parts_13 = 1 then
                let part0_13 = List.nth parts_13 0 in
                w_42.state.c <- pc_to_exp (int_to_pc 5)
              else failwith "unreachable (8)"
          | 5 (* tag_Add *) ->
              let parts_14 = Memo.splits (snd pair_7) in
              if List.length parts_14 = 2 then
                let part0_14 = List.nth parts_14 0 in
                let part1_6 = List.nth parts_14 1 in
                w_42.state.c <- pc_to_exp (int_to_pc 6)
              else failwith "unreachable (8)"
          | 6 (* tag_Mul *) ->
              let parts_15 = Memo.splits (snd pair_7) in
              if List.length parts_15 = 2 then
                let part0_15 = List.nth parts_15 0 in
                let part1_7 = List.nth parts_15 1 in
                w_42.state.c <- pc_to_exp (int_to_pc 7)
              else failwith "unreachable (8)"
          | _ -> failwith "unreachable (8)"))
    8;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 5;
      let arg0_21 = get_env_slot w_97 1 in
      assert_env_length w_97 5;
      w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; collect_env_slots w_97 [ 0; 1 ]; w_97.state.k ];
      init_frame w_97 1 (Memo.from_constructor tag_cont_done);
      set_env_slot w_97 0 arg0_21;
      w_97.state.c <- pc_to_exp (int_to_pc 8))
    9;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 5;
      assert_env_length w_100 5;
      let lhs_18 = Memo.to_word (Memo.from_int 0) in
      let rhs_18 = Memo.to_word (Memo.from_int 1) in
      set_env_slot w_100 0 (Memo.from_int (Word.get_value lhs_18 - Word.get_value rhs_18));
      return_value w_100 (get_env_slot w_100 0) (pc_to_exp (int_to_pc 0)))
    10;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 5;
      assert_env_length w_101 5;
      let lhs_19 = Memo.to_word (get_env_slot w_101 2) in
      let rhs_19 = Memo.to_word (get_env_slot w_101 3) in
      set_env_slot w_101 2 (Memo.from_int (if Word.get_value lhs_19 > Word.get_value rhs_19 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_101 2)) <> 0 then w_101.state.c <- pc_to_exp (int_to_pc 12)
      else w_101.state.c <- pc_to_exp (int_to_pc 33))
    11;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 5;
      return_value w_102 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    12;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 5;
      assert_env_length w_103 5;
      let lhs_20 = Memo.to_word (get_env_slot w_103 1) in
      let rhs_20 = Memo.to_word (get_env_slot w_103 0) in
      set_env_slot w_103 2 (Memo.from_int (if Word.get_value lhs_20 < Word.get_value rhs_20 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_103 2)) <> 0 then w_103.state.c <- pc_to_exp (int_to_pc 14)
      else w_103.state.c <- pc_to_exp (int_to_pc 15))
    13;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 5;
      assert_env_length w_104 5;
      let lhs_21 = Memo.to_word (Memo.from_int 0) in
      let rhs_21 = Memo.to_word (Memo.from_int 1) in
      set_env_slot w_104 0 (Memo.from_int (Word.get_value lhs_21 - Word.get_value rhs_21));
      return_value w_104 (get_env_slot w_104 0) (pc_to_exp (int_to_pc 0)))
    14;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 5;
      assert_env_length w_105 5;
      let lhs_22 = Memo.to_word (get_env_slot w_105 1) in
      let rhs_22 = Memo.to_word (get_env_slot w_105 0) in
      set_env_slot w_105 0 (Memo.from_int (if Word.get_value lhs_22 > Word.get_value rhs_22 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_105 0)) <> 0 then w_105.state.c <- pc_to_exp (int_to_pc 16)
      else w_105.state.c <- pc_to_exp (int_to_pc 17))
    15;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 5;
      return_value w_106 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    16;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 5;
      return_value w_107 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 5;
      assert_env_length w_108 5;
      match Memo.list_match (get_env_slot w_108 0) with
      | None -> failwith "unreachable (18)"
      | Some pair_19 -> (
          let tag_19 = Word.get_value (fst pair_19) in
          match tag_19 with
          | 3 (* tag_Const *) ->
              let parts_34 = Memo.splits (snd pair_19) in
              if List.length parts_34 = 1 then (
                let part0_34 = List.nth parts_34 0 in
                set_env_slot w_108 0 part0_34;
                w_108.state.c <- pc_to_exp (int_to_pc 13))
              else failwith "unreachable (18)"
          | _ -> failwith "unreachable (18)"))
    18;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 5;
      let arg0_23 = get_env_slot w_109 1 in
      assert_env_length w_109 5;
      w_109.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; collect_env_slots w_109 [ 2 ]; w_109.state.k ];
      init_frame w_109 1 (Memo.from_constructor tag_cont_done);
      set_env_slot w_109 0 arg0_23;
      w_109.state.c <- pc_to_exp (int_to_pc 3))
    19;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 5;
      assert_env_length w_112 5;
      let lhs_24 = Memo.to_word (Memo.from_int 0) in
      let rhs_24 = Memo.to_word (Memo.from_int 1) in
      set_env_slot w_112 0 (Memo.from_int (Word.get_value lhs_24 - Word.get_value rhs_24));
      return_value w_112 (get_env_slot w_112 0) (pc_to_exp (int_to_pc 0)))
    20;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 5;
      assert_env_length w_113 5;
      let lhs_25 = Memo.to_word (get_env_slot w_113 0) in
      let rhs_25 = Memo.to_word (get_env_slot w_113 1) in
      set_env_slot w_113 0 (Memo.from_int (if Word.get_value lhs_25 > Word.get_value rhs_25 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_113 0)) <> 0 then w_113.state.c <- pc_to_exp (int_to_pc 22)
      else w_113.state.c <- pc_to_exp (int_to_pc 23))
    21;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 5;
      return_value w_114 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    22;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 5;
      return_value w_115 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    23;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 5;
      assert_env_length w_116 5;
      match Memo.list_match (get_env_slot w_116 0) with
      | None -> failwith "unreachable (24)"
      | Some pair_20 -> (
          let tag_20 = Word.get_value (fst pair_20) in
          match tag_20 with
          | 4 (* tag_Var *) ->
              let parts_35 = Memo.splits (snd pair_20) in
              if List.length parts_35 = 1 then (
                let part0_35 = List.nth parts_35 0 in
                set_env_slot w_116 2 part0_35;
                w_116.state.c <- pc_to_exp (int_to_pc 19))
              else failwith "unreachable (24)"
          | _ -> failwith "unreachable (24)"))
    24;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 5;
      let arg0_25 = get_env_slot w_117 2 in
      let arg1_10 = get_env_slot w_117 3 in
      assert_env_length w_117 5;
      w_117.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_23; collect_env_slots w_117 [ 0; 1 ]; w_117.state.k ];
      init_frame w_117 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_117 1 arg0_25;
      set_env_slot w_117 0 arg1_10;
      w_117.state.c <- pc_to_exp (int_to_pc 9))
    25;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 5;
      let arg0_26 = get_env_slot w_119 1 in
      let arg1_11 = get_env_slot w_119 0 in
      assert_env_length w_119 5;
      init_frame w_119 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_119 1 arg0_26;
      set_env_slot w_119 0 arg1_11;
      w_119.state.c <- pc_to_exp (int_to_pc 9))
    26;
  add_exp
    (fun w_120 ->
      assert_env_length w_120 5;
      return_value w_120 (get_env_slot w_120 2) (pc_to_exp (int_to_pc 0)))
    27;
  add_exp
    (fun w_121 ->
      assert_env_length w_121 5;
      assert_env_length w_121 5;
      match Memo.list_match (get_env_slot w_121 0) with
      | None -> failwith "unreachable (28)"
      | Some pair_21 -> (
          let tag_21 = Word.get_value (fst pair_21) in
          match tag_21 with
          | 5 (* tag_Add *) ->
              let parts_36 = Memo.splits (snd pair_21) in
              if List.length parts_36 = 2 then (
                let part0_36 = List.nth parts_36 0 in
                let part1_16 = List.nth parts_36 1 in
                set_env_slot w_121 3 part0_36;
                set_env_slot w_121 0 part1_16;
                w_121.state.c <- pc_to_exp (int_to_pc 25))
              else failwith "unreachable (28)"
          | _ -> failwith "unreachable (28)"))
    28;
  add_exp
    (fun w_122 ->
      assert_env_length w_122 5;
      let arg0_27 = get_env_slot w_122 2 in
      let arg1_12 = get_env_slot w_122 3 in
      assert_env_length w_122 5;
      w_122.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_24; collect_env_slots w_122 [ 0; 1 ]; w_122.state.k ];
      init_frame w_122 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_122 1 arg0_27;
      set_env_slot w_122 0 arg1_12;
      w_122.state.c <- pc_to_exp (int_to_pc 9))
    29;
  add_exp
    (fun w_124 ->
      assert_env_length w_124 5;
      let arg0_28 = get_env_slot w_124 1 in
      let arg1_13 = get_env_slot w_124 0 in
      assert_env_length w_124 5;
      init_frame w_124 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_124 1 arg0_28;
      set_env_slot w_124 0 arg1_13;
      w_124.state.c <- pc_to_exp (int_to_pc 9))
    30;
  add_exp
    (fun w_125 ->
      assert_env_length w_125 5;
      return_value w_125 (get_env_slot w_125 2) (pc_to_exp (int_to_pc 0)))
    31;
  add_exp
    (fun w_126 ->
      assert_env_length w_126 5;
      assert_env_length w_126 5;
      match Memo.list_match (get_env_slot w_126 0) with
      | None -> failwith "unreachable (32)"
      | Some pair_22 -> (
          let tag_22 = Word.get_value (fst pair_22) in
          match tag_22 with
          | 6 (* tag_Mul *) ->
              let parts_37 = Memo.splits (snd pair_22) in
              if List.length parts_37 = 2 then (
                let part0_37 = List.nth parts_37 0 in
                let part1_17 = List.nth parts_37 1 in
                set_env_slot w_126 3 part0_37;
                set_env_slot w_126 0 part1_17;
                w_126.state.c <- pc_to_exp (int_to_pc 29))
              else failwith "unreachable (32)"
          | _ -> failwith "unreachable (32)"))
    32;
  add_exp
    (fun w_127 ->
      assert_env_length w_127 5;
      assert_env_length w_127 5;
      match Memo.list_match (get_env_slot w_127 1) with
      | None -> failwith "unreachable (33)"
      | Some pair_23 -> (
          let tag_23 = Word.get_value (fst pair_23) in
          match tag_23 with
          | 3 (* tag_Const *) ->
              let parts_38 = Memo.splits (snd pair_23) in
              if List.length parts_38 = 1 then (
                let part0_38 = List.nth parts_38 0 in
                set_env_slot w_127 1 part0_38;
                w_127.state.c <- pc_to_exp (int_to_pc 18))
              else failwith "unreachable (33)"
          | 4 (* tag_Var *) ->
              let parts_39 = Memo.splits (snd pair_23) in
              if List.length parts_39 = 1 then (
                let part0_39 = List.nth parts_39 0 in
                set_env_slot w_127 1 part0_39;
                w_127.state.c <- pc_to_exp (int_to_pc 24))
              else failwith "unreachable (33)"
          | 5 (* tag_Add *) ->
              let parts_40 = Memo.splits (snd pair_23) in
              if List.length parts_40 = 2 then (
                let part0_40 = List.nth parts_40 0 in
                let part1_18 = List.nth parts_40 1 in
                set_env_slot w_127 2 part0_40;
                set_env_slot w_127 1 part1_18;
                w_127.state.c <- pc_to_exp (int_to_pc 28))
              else failwith "unreachable (33)"
          | 6 (* tag_Mul *) ->
              let parts_41 = Memo.splits (snd pair_23) in
              if List.length parts_41 = 2 then (
                let part0_41 = List.nth parts_41 0 in
                let part1_19 = List.nth parts_41 1 in
                set_env_slot w_127 2 part0_41;
                set_env_slot w_127 1 part1_19;
                w_127.state.c <- pc_to_exp (int_to_pc 32))
              else failwith "unreachable (33)"
          | _ -> failwith "unreachable (33)"))
    33;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 4;
      assert_env_length w_0 4;
      let lhs_0 = Memo.to_word (get_env_slot w_0 1) in
      let rhs_0 = Memo.to_word (get_env_slot w_0 0) in
      set_env_slot w_0 0 (Memo.from_int (if Word.get_value lhs_0 = Word.get_value rhs_0 then 1 else 0));
      return_value w_0 (get_env_slot w_0 0) (pc_to_exp (int_to_pc 0)))
    34;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 4;
      return_value w_1 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    35;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 4;
      assert_env_length w_2 4;
      match Memo.list_match (get_env_slot w_2 0) with
      | None -> w_2.state.c <- pc_to_exp (int_to_pc 35)
      | Some pair_0 -> (
          let tag_0 = Word.get_value (fst pair_0) in
          match tag_0 with
          | 3 (* tag_Const *) ->
              let parts_0 = Memo.splits (snd pair_0) in
              if List.length parts_0 = 1 then (
                let part0_0 = List.nth parts_0 0 in
                set_env_slot w_2 0 part0_0;
                w_2.state.c <- pc_to_exp (int_to_pc 34))
              else w_2.state.c <- pc_to_exp (int_to_pc 35)
          | _ -> w_2.state.c <- pc_to_exp (int_to_pc 35)))
    36;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 4;
      let arg0_0 = get_env_slot w_3 1 in
      assert_env_length w_3 4;
      w_3.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; collect_env_slots w_3 [ 0 ]; w_3.state.k ];
      init_frame w_3 1 (Memo.from_constructor tag_cont_done);
      set_env_slot w_3 0 arg0_0;
      w_3.state.c <- pc_to_exp (int_to_pc 3))
    37;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 4;
      return_value w_6 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    38;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 4;
      assert_env_length w_7 4;
      match Memo.list_match (get_env_slot w_7 0) with
      | None -> w_7.state.c <- pc_to_exp (int_to_pc 38)
      | Some pair_1 -> (
          let tag_1 = Word.get_value (fst pair_1) in
          match tag_1 with
          | 4 (* tag_Var *) ->
              let parts_1 = Memo.splits (snd pair_1) in
              if List.length parts_1 = 1 then (
                let part0_1 = List.nth parts_1 0 in
                set_env_slot w_7 0 part0_1;
                w_7.state.c <- pc_to_exp (int_to_pc 37))
              else w_7.state.c <- pc_to_exp (int_to_pc 38)
          | _ -> w_7.state.c <- pc_to_exp (int_to_pc 38)))
    39;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 4;
      let arg0_2 = get_env_slot w_8 2 in
      let arg1_0 = get_env_slot w_8 3 in
      assert_env_length w_8 4;
      w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; collect_env_slots w_8 [ 0; 1 ]; w_8.state.k ];
      init_frame w_8 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_8 1 arg0_2;
      set_env_slot w_8 0 arg1_0;
      w_8.state.c <- pc_to_exp (int_to_pc 46))
    40;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 4;
      return_value w_11 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    41;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 4;
      assert_env_length w_12 4;
      match Memo.list_match (get_env_slot w_12 0) with
      | None -> w_12.state.c <- pc_to_exp (int_to_pc 41)
      | Some pair_2 -> (
          let tag_2 = Word.get_value (fst pair_2) in
          match tag_2 with
          | 5 (* tag_Add *) ->
              let parts_2 = Memo.splits (snd pair_2) in
              if List.length parts_2 = 2 then (
                let part0_2 = List.nth parts_2 0 in
                let part1_0 = List.nth parts_2 1 in
                set_env_slot w_12 3 part0_2;
                set_env_slot w_12 0 part1_0;
                w_12.state.c <- pc_to_exp (int_to_pc 40))
              else w_12.state.c <- pc_to_exp (int_to_pc 41)
          | _ -> w_12.state.c <- pc_to_exp (int_to_pc 41)))
    42;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 4;
      let arg0_4 = get_env_slot w_13 2 in
      let arg1_2 = get_env_slot w_13 3 in
      assert_env_length w_13 4;
      w_13.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; collect_env_slots w_13 [ 0; 1 ]; w_13.state.k ];
      init_frame w_13 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_13 1 arg0_4;
      set_env_slot w_13 0 arg1_2;
      w_13.state.c <- pc_to_exp (int_to_pc 46))
    43;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 4;
      return_value w_16 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
    44;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 4;
      assert_env_length w_17 4;
      match Memo.list_match (get_env_slot w_17 0) with
      | None -> w_17.state.c <- pc_to_exp (int_to_pc 44)
      | Some pair_3 -> (
          let tag_3 = Word.get_value (fst pair_3) in
          match tag_3 with
          | 6 (* tag_Mul *) ->
              let parts_3 = Memo.splits (snd pair_3) in
              if List.length parts_3 = 2 then (
                let part0_3 = List.nth parts_3 0 in
                let part1_1 = List.nth parts_3 1 in
                set_env_slot w_17 3 part0_3;
                set_env_slot w_17 0 part1_1;
                w_17.state.c <- pc_to_exp (int_to_pc 43))
              else w_17.state.c <- pc_to_exp (int_to_pc 44)
          | _ -> w_17.state.c <- pc_to_exp (int_to_pc 44)))
    45;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 4;
      assert_env_length w_18 4;
      match Memo.list_match (get_env_slot w_18 1) with
      | None -> failwith "unreachable (46)"
      | Some pair_4 -> (
          let tag_4 = Word.get_value (fst pair_4) in
          match tag_4 with
          | 3 (* tag_Const *) ->
              let parts_4 = Memo.splits (snd pair_4) in
              if List.length parts_4 = 1 then (
                let part0_4 = List.nth parts_4 0 in
                set_env_slot w_18 1 part0_4;
                w_18.state.c <- pc_to_exp (int_to_pc 36))
              else failwith "unreachable (46)"
          | 4 (* tag_Var *) ->
              let parts_5 = Memo.splits (snd pair_4) in
              if List.length parts_5 = 1 then (
                let part0_5 = List.nth parts_5 0 in
                set_env_slot w_18 1 part0_5;
                w_18.state.c <- pc_to_exp (int_to_pc 39))
              else failwith "unreachable (46)"
          | 5 (* tag_Add *) ->
              let parts_6 = Memo.splits (snd pair_4) in
              if List.length parts_6 = 2 then (
                let part0_6 = List.nth parts_6 0 in
                let part1_2 = List.nth parts_6 1 in
                set_env_slot w_18 2 part0_6;
                set_env_slot w_18 1 part1_2;
                w_18.state.c <- pc_to_exp (int_to_pc 42))
              else failwith "unreachable (46)"
          | 6 (* tag_Mul *) ->
              let parts_7 = Memo.splits (snd pair_4) in
              if List.length parts_7 = 2 then (
                let part0_7 = List.nth parts_7 0 in
                let part1_3 = List.nth parts_7 1 in
                set_env_slot w_18 2 part0_7;
                set_env_slot w_18 1 part1_3;
                w_18.state.c <- pc_to_exp (int_to_pc 45))
              else failwith "unreachable (46)"
          | _ -> failwith "unreachable (46)"))
    46;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 5;
      return_value w_43 (get_env_slot w_43 0) (pc_to_exp (int_to_pc 0)))
    47;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 5;
      return_value w_44 (get_env_slot w_44 0) (pc_to_exp (int_to_pc 0)))
    48;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 5;
      let arg0_16 = get_env_slot w_45 0 in
      assert_env_length w_45 5;
      w_45.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; collect_env_slots w_45 [ 1 ]; w_45.state.k ];
      init_frame w_45 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_45 0 arg0_16;
      w_45.state.c <- pc_to_exp (int_to_pc 92))
    49;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 5;
      assert_env_length w_49 5;
      set_env_slot w_49 1 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 2 ]);
      assert_env_length w_49 5;
      set_env_slot w_49 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_49 1; get_env_slot w_49 0 ]);
      return_value w_49 (get_env_slot w_49 0) (pc_to_exp (int_to_pc 0)))
    50;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 5;
      assert_env_length w_50 5;
      let lhs_6 = Memo.to_word (get_env_slot w_50 1) in
      let rhs_6 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_50 3 (Memo.from_int (if Word.get_value lhs_6 = Word.get_value rhs_6 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_50 3)) <> 0 then w_50.state.c <- pc_to_exp (int_to_pc 52)
      else w_50.state.c <- pc_to_exp (int_to_pc 55))
    51;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 5;
      return_value w_51 (get_env_slot w_51 2) (pc_to_exp (int_to_pc 0)))
    52;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 5;
      assert_env_length w_52 5;
      let lhs_7 = Memo.to_word (get_env_slot w_52 1) in
      let rhs_7 = Memo.to_word (get_env_slot w_52 0) in
      set_env_slot w_52 0 (Memo.from_int (Word.get_value lhs_7 + Word.get_value rhs_7));
      assert_env_length w_52 5;
      set_env_slot w_52 0 (Memo.appends [ Memo.from_constructor tag_Const; get_env_slot w_52 0 ]);
      return_value w_52 (get_env_slot w_52 0) (pc_to_exp (int_to_pc 0)))
    53;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 5;
      assert_env_length w_53 5;
      set_env_slot w_53 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_53 0; get_env_slot w_53 2 ]);
      return_value w_53 (get_env_slot w_53 0) (pc_to_exp (int_to_pc 0)))
    54;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 5;
      assert_env_length w_54 5;
      match Memo.list_match (get_env_slot w_54 2) with
      | None -> w_54.state.c <- pc_to_exp (int_to_pc 54)
      | Some pair_8 -> (
          let tag_8 = Word.get_value (fst pair_8) in
          match tag_8 with
          | 3 (* tag_Const *) ->
              let parts_16 = Memo.splits (snd pair_8) in
              if List.length parts_16 = 1 then (
                let part0_16 = List.nth parts_16 0 in
                set_env_slot w_54 0 part0_16;
                w_54.state.c <- pc_to_exp (int_to_pc 53))
              else w_54.state.c <- pc_to_exp (int_to_pc 54)
          | _ -> w_54.state.c <- pc_to_exp (int_to_pc 54)))
    55;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 5;
      assert_env_length w_55 5;
      let lhs_8 = Memo.to_word (get_env_slot w_55 4) in
      let rhs_8 = Memo.to_word (get_env_slot w_55 2) in
      set_env_slot w_55 2 (Memo.from_int (Word.get_value lhs_8 + Word.get_value rhs_8));
      assert_env_length w_55 5;
      set_env_slot w_55 2 (Memo.appends [ Memo.from_constructor tag_Const; get_env_slot w_55 2 ]);
      assert_env_length w_55 5;
      set_env_slot w_55 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_55 1; get_env_slot w_55 0 ]);
      assert_env_length w_55 5;
      set_env_slot w_55 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_55 2; get_env_slot w_55 0 ]);
      return_value w_55 (get_env_slot w_55 0) (pc_to_exp (int_to_pc 0)))
    56;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 5;
      assert_env_length w_56 5;
      set_env_slot w_56 1 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_56 3; get_env_slot w_56 1 ]);
      assert_env_length w_56 5;
      set_env_slot w_56 1 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_56 1; get_env_slot w_56 2 ]);
      assert_env_length w_56 5;
      set_env_slot w_56 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_56 1; get_env_slot w_56 0 ]);
      return_value w_56 (get_env_slot w_56 0) (pc_to_exp (int_to_pc 0)))
    57;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 5;
      assert_env_length w_57 5;
      match Memo.list_match (get_env_slot w_57 2) with
      | None -> w_57.state.c <- pc_to_exp (int_to_pc 57)
      | Some pair_9 -> (
          let tag_9 = Word.get_value (fst pair_9) in
          match tag_9 with
          | 3 (* tag_Const *) ->
              let parts_17 = Memo.splits (snd pair_9) in
              if List.length parts_17 = 1 then (
                let part0_17 = List.nth parts_17 0 in
                set_env_slot w_57 2 part0_17;
                w_57.state.c <- pc_to_exp (int_to_pc 56))
              else w_57.state.c <- pc_to_exp (int_to_pc 57)
          | _ -> w_57.state.c <- pc_to_exp (int_to_pc 57)))
    58;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 5;
      assert_env_length w_58 5;
      set_env_slot w_58 1 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_58 3; get_env_slot w_58 1 ]);
      assert_env_length w_58 5;
      set_env_slot w_58 1 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_58 1; get_env_slot w_58 2 ]);
      assert_env_length w_58 5;
      set_env_slot w_58 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_58 1; get_env_slot w_58 0 ]);
      return_value w_58 (get_env_slot w_58 0) (pc_to_exp (int_to_pc 0)))
    59;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 5;
      assert_env_length w_59 5;
      match Memo.list_match (get_env_slot w_59 3) with
      | None -> w_59.state.c <- pc_to_exp (int_to_pc 59)
      | Some pair_10 -> (
          let tag_10 = Word.get_value (fst pair_10) in
          match tag_10 with
          | 3 (* tag_Const *) ->
              let parts_18 = Memo.splits (snd pair_10) in
              if List.length parts_18 = 1 then (
                let part0_18 = List.nth parts_18 0 in
                set_env_slot w_59 4 part0_18;
                w_59.state.c <- pc_to_exp (int_to_pc 58))
              else w_59.state.c <- pc_to_exp (int_to_pc 59)
          | _ -> w_59.state.c <- pc_to_exp (int_to_pc 59)))
    60;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 5;
      assert_env_length w_60 5;
      set_env_slot w_60 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_60 0; get_env_slot w_60 2 ]);
      return_value w_60 (get_env_slot w_60 0) (pc_to_exp (int_to_pc 0)))
    61;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 5;
      assert_env_length w_61 5;
      match Memo.list_match (get_env_slot w_61 2) with
      | None -> w_61.state.c <- pc_to_exp (int_to_pc 61)
      | Some pair_11 -> (
          let tag_11 = Word.get_value (fst pair_11) in
          match tag_11 with
          | 5 (* tag_Add *) ->
              let parts_19 = Memo.splits (snd pair_11) in
              if List.length parts_19 = 2 then (
                let part0_19 = List.nth parts_19 0 in
                let part1_8 = List.nth parts_19 1 in
                set_env_slot w_61 2 part0_19;
                set_env_slot w_61 0 part1_8;
                w_61.state.c <- pc_to_exp (int_to_pc 60))
              else w_61.state.c <- pc_to_exp (int_to_pc 61)
          | _ -> w_61.state.c <- pc_to_exp (int_to_pc 61)))
    62;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 5;
      assert_env_length w_62 5;
      let lhs_9 = Memo.to_word (get_env_slot w_62 1) in
      let rhs_9 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_62 1 (Memo.from_int (if Word.get_value lhs_9 = Word.get_value rhs_9 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_62 1)) <> 0 then w_62.state.c <- pc_to_exp (int_to_pc 64)
      else w_62.state.c <- pc_to_exp (int_to_pc 65))
    63;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 5;
      return_value w_63 (get_env_slot w_63 0) (pc_to_exp (int_to_pc 0)))
    64;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 5;
      assert_env_length w_64 5;
      set_env_slot w_64 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_64 2; get_env_slot w_64 0 ]);
      return_value w_64 (get_env_slot w_64 0) (pc_to_exp (int_to_pc 0)))
    65;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 5;
      assert_env_length w_65 5;
      set_env_slot w_65 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_65 0; get_env_slot w_65 2 ]);
      assert_env_length w_65 5;
      set_env_slot w_65 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_65 0; get_env_slot w_65 1 ]);
      return_value w_65 (get_env_slot w_65 0) (pc_to_exp (int_to_pc 0)))
    66;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 5;
      assert_env_length w_66 5;
      set_env_slot w_66 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_66 0; get_env_slot w_66 2 ]);
      return_value w_66 (get_env_slot w_66 0) (pc_to_exp (int_to_pc 0)))
    67;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 5;
      assert_env_length w_67 5;
      match Memo.list_match (get_env_slot w_67 2) with
      | None -> w_67.state.c <- pc_to_exp (int_to_pc 67)
      | Some pair_12 -> (
          let tag_12 = Word.get_value (fst pair_12) in
          match tag_12 with
          | 3 (* tag_Const *) ->
              let parts_20 = Memo.splits (snd pair_12) in
              if List.length parts_20 = 1 then (
                let part0_20 = List.nth parts_20 0 in
                set_env_slot w_67 1 part0_20;
                w_67.state.c <- pc_to_exp (int_to_pc 63))
              else w_67.state.c <- pc_to_exp (int_to_pc 67)
          | 5 (* tag_Add *) ->
              let parts_21 = Memo.splits (snd pair_12) in
              if List.length parts_21 = 2 then (
                let part0_21 = List.nth parts_21 0 in
                let part1_9 = List.nth parts_21 1 in
                set_env_slot w_67 2 part0_21;
                set_env_slot w_67 1 part1_9;
                w_67.state.c <- pc_to_exp (int_to_pc 66))
              else w_67.state.c <- pc_to_exp (int_to_pc 67)
          | _ -> w_67.state.c <- pc_to_exp (int_to_pc 67)))
    68;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 5;
      assert_env_length w_68 5;
      match Memo.list_match (get_env_slot w_68 0) with
      | None -> w_68.state.c <- pc_to_exp (int_to_pc 68)
      | Some pair_13 -> (
          let tag_13 = Word.get_value (fst pair_13) in
          match tag_13 with
          | 3 (* tag_Const *) ->
              let parts_22 = Memo.splits (snd pair_13) in
              if List.length parts_22 = 1 then (
                let part0_22 = List.nth parts_22 0 in
                set_env_slot w_68 1 part0_22;
                w_68.state.c <- pc_to_exp (int_to_pc 51))
              else w_68.state.c <- pc_to_exp (int_to_pc 68)
          | 5 (* tag_Add *) ->
              let parts_23 = Memo.splits (snd pair_13) in
              if List.length parts_23 = 2 then (
                let part0_23 = List.nth parts_23 0 in
                let part1_10 = List.nth parts_23 1 in
                set_env_slot w_68 3 part0_23;
                set_env_slot w_68 1 part1_10;
                w_68.state.c <- pc_to_exp (int_to_pc 62))
              else w_68.state.c <- pc_to_exp (int_to_pc 68)
          | _ -> w_68.state.c <- pc_to_exp (int_to_pc 68)))
    69;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 5;
      assert_env_length w_69 5;
      let lhs_10 = Memo.to_word (get_env_slot w_69 2) in
      let rhs_10 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_69 3 (Memo.from_int (if Word.get_value lhs_10 = Word.get_value rhs_10 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_69 3)) <> 0 then w_69.state.c <- pc_to_exp (int_to_pc 71)
      else w_69.state.c <- pc_to_exp (int_to_pc 72))
    70;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 5;
      assert_env_length w_70 5;
      set_env_slot w_70 0 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
      return_value w_70 (get_env_slot w_70 0) (pc_to_exp (int_to_pc 0)))
    71;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 5;
      assert_env_length w_71 5;
      let lhs_11 = Memo.to_word (get_env_slot w_71 2) in
      let rhs_11 = Memo.to_word (Memo.from_int 1) in
      set_env_slot w_71 3 (Memo.from_int (if Word.get_value lhs_11 = Word.get_value rhs_11 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_71 3)) <> 0 then w_71.state.c <- pc_to_exp (int_to_pc 73)
      else w_71.state.c <- pc_to_exp (int_to_pc 80))
    72;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 5;
      return_value w_72 (get_env_slot w_72 1) (pc_to_exp (int_to_pc 0)))
    73;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 5;
      assert_env_length w_73 5;
      let lhs_12 = Memo.to_word (get_env_slot w_73 1) in
      let rhs_12 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_73 3 (Memo.from_int (if Word.get_value lhs_12 = Word.get_value rhs_12 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_73 3)) <> 0 then w_73.state.c <- pc_to_exp (int_to_pc 75)
      else w_73.state.c <- pc_to_exp (int_to_pc 76))
    74;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 5;
      assert_env_length w_74 5;
      set_env_slot w_74 0 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
      return_value w_74 (get_env_slot w_74 0) (pc_to_exp (int_to_pc 0)))
    75;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 5;
      assert_env_length w_75 5;
      let lhs_13 = Memo.to_word (get_env_slot w_75 1) in
      let rhs_13 = Memo.to_word (Memo.from_int 1) in
      set_env_slot w_75 3 (Memo.from_int (if Word.get_value lhs_13 = Word.get_value rhs_13 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_75 3)) <> 0 then w_75.state.c <- pc_to_exp (int_to_pc 77)
      else w_75.state.c <- pc_to_exp (int_to_pc 78))
    76;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 5;
      return_value w_76 (get_env_slot w_76 0) (pc_to_exp (int_to_pc 0)))
    77;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 5;
      assert_env_length w_77 5;
      let lhs_14 = Memo.to_word (get_env_slot w_77 2) in
      let rhs_14 = Memo.to_word (get_env_slot w_77 1) in
      set_env_slot w_77 0 (Memo.from_int (Word.get_value lhs_14 * Word.get_value rhs_14));
      assert_env_length w_77 5;
      set_env_slot w_77 0 (Memo.appends [ Memo.from_constructor tag_Const; get_env_slot w_77 0 ]);
      return_value w_77 (get_env_slot w_77 0) (pc_to_exp (int_to_pc 0)))
    78;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 5;
      assert_env_length w_78 5;
      set_env_slot w_78 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_78 0; get_env_slot w_78 1 ]);
      return_value w_78 (get_env_slot w_78 0) (pc_to_exp (int_to_pc 0)))
    79;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 5;
      assert_env_length w_79 5;
      match Memo.list_match (get_env_slot w_79 1) with
      | None -> w_79.state.c <- pc_to_exp (int_to_pc 79)
      | Some pair_14 -> (
          let tag_14 = Word.get_value (fst pair_14) in
          match tag_14 with
          | 3 (* tag_Const *) ->
              let parts_24 = Memo.splits (snd pair_14) in
              if List.length parts_24 = 1 then (
                let part0_24 = List.nth parts_24 0 in
                set_env_slot w_79 1 part0_24;
                w_79.state.c <- pc_to_exp (int_to_pc 74))
              else w_79.state.c <- pc_to_exp (int_to_pc 79)
          | _ -> w_79.state.c <- pc_to_exp (int_to_pc 79)))
    80;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 5;
      assert_env_length w_80 5;
      set_env_slot w_80 1 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_80 1; get_env_slot w_80 2 ]);
      assert_env_length w_80 5;
      set_env_slot w_80 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_80 0; get_env_slot w_80 2 ]);
      assert_env_length w_80 5;
      set_env_slot w_80 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_80 1; get_env_slot w_80 0 ]);
      return_value w_80 (get_env_slot w_80 0) (pc_to_exp (int_to_pc 0)))
    81;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 5;
      assert_env_length w_81 5;
      let lhs_15 = Memo.to_word (get_env_slot w_81 2) in
      let rhs_15 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_81 3 (Memo.from_int (if Word.get_value lhs_15 = Word.get_value rhs_15 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_81 3)) <> 0 then w_81.state.c <- pc_to_exp (int_to_pc 83)
      else w_81.state.c <- pc_to_exp (int_to_pc 84))
    82;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 5;
      assert_env_length w_82 5;
      set_env_slot w_82 0 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
      return_value w_82 (get_env_slot w_82 0) (pc_to_exp (int_to_pc 0)))
    83;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 5;
      assert_env_length w_83 5;
      let lhs_16 = Memo.to_word (get_env_slot w_83 2) in
      let rhs_16 = Memo.to_word (Memo.from_int 1) in
      set_env_slot w_83 2 (Memo.from_int (if Word.get_value lhs_16 = Word.get_value rhs_16 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_83 2)) <> 0 then w_83.state.c <- pc_to_exp (int_to_pc 85)
      else w_83.state.c <- pc_to_exp (int_to_pc 86))
    84;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 5;
      return_value w_84 (get_env_slot w_84 0) (pc_to_exp (int_to_pc 0)))
    85;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 5;
      assert_env_length w_85 5;
      set_env_slot w_85 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_85 1; get_env_slot w_85 0 ]);
      return_value w_85 (get_env_slot w_85 0) (pc_to_exp (int_to_pc 0)))
    86;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 5;
      assert_env_length w_86 5;
      set_env_slot w_86 2 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_86 0; get_env_slot w_86 2 ]);
      assert_env_length w_86 5;
      set_env_slot w_86 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_86 0; get_env_slot w_86 1 ]);
      assert_env_length w_86 5;
      set_env_slot w_86 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_86 2; get_env_slot w_86 0 ]);
      return_value w_86 (get_env_slot w_86 0) (pc_to_exp (int_to_pc 0)))
    87;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 5;
      assert_env_length w_87 5;
      set_env_slot w_87 1 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_87 1; get_env_slot w_87 2 ]);
      assert_env_length w_87 5;
      set_env_slot w_87 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_87 0; get_env_slot w_87 1 ]);
      return_value w_87 (get_env_slot w_87 0) (pc_to_exp (int_to_pc 0)))
    88;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 5;
      assert_env_length w_88 5;
      set_env_slot w_88 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_88 0; get_env_slot w_88 1 ]);
      return_value w_88 (get_env_slot w_88 0) (pc_to_exp (int_to_pc 0)))
    89;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 5;
      assert_env_length w_89 5;
      match Memo.list_match (get_env_slot w_89 1) with
      | None -> w_89.state.c <- pc_to_exp (int_to_pc 89)
      | Some pair_15 -> (
          let tag_15 = Word.get_value (fst pair_15) in
          match tag_15 with
          | 3 (* tag_Const *) ->
              let parts_25 = Memo.splits (snd pair_15) in
              if List.length parts_25 = 1 then (
                let part0_25 = List.nth parts_25 0 in
                set_env_slot w_89 2 part0_25;
                w_89.state.c <- pc_to_exp (int_to_pc 82))
              else w_89.state.c <- pc_to_exp (int_to_pc 89)
          | 5 (* tag_Add *) ->
              let parts_26 = Memo.splits (snd pair_15) in
              if List.length parts_26 = 2 then (
                let part0_26 = List.nth parts_26 0 in
                let part1_11 = List.nth parts_26 1 in
                set_env_slot w_89 2 part0_26;
                set_env_slot w_89 1 part1_11;
                w_89.state.c <- pc_to_exp (int_to_pc 87))
              else w_89.state.c <- pc_to_exp (int_to_pc 89)
          | 6 (* tag_Mul *) ->
              let parts_27 = Memo.splits (snd pair_15) in
              if List.length parts_27 = 2 then (
                let part0_27 = List.nth parts_27 0 in
                let part1_12 = List.nth parts_27 1 in
                set_env_slot w_89 1 part0_27;
                set_env_slot w_89 2 part1_12;
                w_89.state.c <- pc_to_exp (int_to_pc 88))
              else w_89.state.c <- pc_to_exp (int_to_pc 89)
          | _ -> w_89.state.c <- pc_to_exp (int_to_pc 89)))
    90;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 5;
      let arg0_19 = get_env_slot w_90 0 in
      assert_env_length w_90 5;
      w_90.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; collect_env_slots w_90 [ 2 ]; w_90.state.k ];
      init_frame w_90 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_90 0 arg0_19;
      w_90.state.c <- pc_to_exp (int_to_pc 92))
    91;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 5;
      assert_env_length w_93 5;
      match Memo.list_match (get_env_slot w_93 0) with
      | None -> failwith "unreachable (92)"
      | Some pair_17 -> (
          let tag_17 = Word.get_value (fst pair_17) in
          match tag_17 with
          | 3 (* tag_Const *) ->
              let parts_30 = Memo.splits (snd pair_17) in
              if List.length parts_30 = 1 then
                let part0_30 = List.nth parts_30 0 in
                w_93.state.c <- pc_to_exp (int_to_pc 47)
              else failwith "unreachable (92)"
          | 4 (* tag_Var *) ->
              let parts_31 = Memo.splits (snd pair_17) in
              if List.length parts_31 = 1 then
                let part0_31 = List.nth parts_31 0 in
                w_93.state.c <- pc_to_exp (int_to_pc 48)
              else failwith "unreachable (92)"
          | 5 (* tag_Add *) ->
              let parts_32 = Memo.splits (snd pair_17) in
              if List.length parts_32 = 2 then (
                let part0_32 = List.nth parts_32 0 in
                let part1_14 = List.nth parts_32 1 in
                set_env_slot w_93 0 part0_32;
                set_env_slot w_93 1 part1_14;
                w_93.state.c <- pc_to_exp (int_to_pc 49))
              else failwith "unreachable (92)"
          | 6 (* tag_Mul *) ->
              let parts_33 = Memo.splits (snd pair_17) in
              if List.length parts_33 = 2 then (
                let part0_33 = List.nth parts_33 0 in
                let part1_15 = List.nth parts_33 1 in
                set_env_slot w_93 0 part0_33;
                set_env_slot w_93 2 part1_15;
                w_93.state.c <- pc_to_exp (int_to_pc 91))
              else failwith "unreachable (92)"
          | _ -> failwith "unreachable (92)"))
    92;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 2;
      let arg0_6 = get_env_slot w_19 1 in
      assert_env_length w_19 2;
      w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; collect_env_slots w_19 [ 1 ]; w_19.state.k ];
      init_frame w_19 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_19 0 arg0_6;
      w_19.state.c <- pc_to_exp (int_to_pc 92))
    93;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 2;
      return_value w_22 (get_env_slot w_22 0) (pc_to_exp (int_to_pc 0)))
    94;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 2;
      let arg0_8 = get_env_slot w_23 0 in
      assert_env_length w_23 2;
      init_frame w_23 2 (Memo.from_constructor tag_cont_done);
      set_env_slot w_23 1 arg0_8;
      w_23.state.c <- pc_to_exp (int_to_pc 93))
    95;
  add_exp
    (fun w_128 ->
      assert_env_length w_128 3;
      assert_env_length w_128 3;
      set_env_slot w_128 0 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
      return_value w_128 (get_env_slot w_128 0) (pc_to_exp (int_to_pc 0)))
    96;
  add_exp
    (fun w_129 ->
      assert_env_length w_129 3;
      assert_env_length w_129 3;
      set_env_slot w_129 0 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 1 ]);
      return_value w_129 (get_env_slot w_129 0) (pc_to_exp (int_to_pc 0)))
    97;
  add_exp
    (fun w_130 ->
      assert_env_length w_130 3;
      assert_env_length w_130 3;
      set_env_slot w_130 0 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
      return_value w_130 (get_env_slot w_130 0) (pc_to_exp (int_to_pc 0)))
    98;
  add_exp
    (fun w_131 ->
      assert_env_length w_131 3;
      assert_env_length w_131 3;
      match Memo.list_match (get_env_slot w_131 0) with
      | None -> failwith "unreachable (99)"
      | Some pair_24 -> (
          let tag_24 = Word.get_value (fst pair_24) in
          match tag_24 with
          | 1 (* tag_X *) -> w_131.state.c <- pc_to_exp (int_to_pc 97)
          | 2 (* tag_Y *) -> w_131.state.c <- pc_to_exp (int_to_pc 98)
          | _ -> failwith "unreachable (99)"))
    99;
  add_exp
    (fun w_132 ->
      assert_env_length w_132 3;
      let arg0_29 = get_env_slot w_132 1 in
      assert_env_length w_132 3;
      w_132.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; collect_env_slots w_132 [ 0 ]; w_132.state.k ];
      init_frame w_132 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_132 0 arg0_29;
      w_132.state.c <- pc_to_exp (int_to_pc 102))
    100;
  add_exp
    (fun w_135 ->
      assert_env_length w_135 3;
      let arg0_31 = get_env_slot w_135 0 in
      assert_env_length w_135 3;
      w_135.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_28; collect_env_slots w_135 [ 0; 2 ]; w_135.state.k ];
      init_frame w_135 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_135 0 arg0_31;
      w_135.state.c <- pc_to_exp (int_to_pc 102))
    101;
  add_exp
    (fun w_138 ->
      assert_env_length w_138 3;
      assert_env_length w_138 3;
      match Memo.list_match (get_env_slot w_138 0) with
      | None -> failwith "unreachable (102)"
      | Some pair_25 -> (
          let tag_25 = Word.get_value (fst pair_25) in
          match tag_25 with
          | 3 (* tag_Const *) ->
              let parts_42 = Memo.splits (snd pair_25) in
              if List.length parts_42 = 1 then
                let part0_42 = List.nth parts_42 0 in
                w_138.state.c <- pc_to_exp (int_to_pc 96)
              else failwith "unreachable (102)"
          | 4 (* tag_Var *) ->
              let parts_43 = Memo.splits (snd pair_25) in
              if List.length parts_43 = 1 then (
                let part0_43 = List.nth parts_43 0 in
                set_env_slot w_138 0 part0_43;
                w_138.state.c <- pc_to_exp (int_to_pc 99))
              else failwith "unreachable (102)"
          | 5 (* tag_Add *) ->
              let parts_44 = Memo.splits (snd pair_25) in
              if List.length parts_44 = 2 then (
                let part0_44 = List.nth parts_44 0 in
                let part1_20 = List.nth parts_44 1 in
                set_env_slot w_138 1 part0_44;
                set_env_slot w_138 0 part1_20;
                w_138.state.c <- pc_to_exp (int_to_pc 100))
              else failwith "unreachable (102)"
          | 6 (* tag_Mul *) ->
              let parts_45 = Memo.splits (snd pair_25) in
              if List.length parts_45 = 2 then (
                let part0_45 = List.nth parts_45 0 in
                let part1_21 = List.nth parts_45 1 in
                set_env_slot w_138 0 part0_45;
                set_env_slot w_138 2 part1_21;
                w_138.state.c <- pc_to_exp (int_to_pc 101))
              else failwith "unreachable (102)"
          | _ -> failwith "unreachable (102)"))
    102;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 4;
      return_value w_27 (get_env_slot w_27 0) (pc_to_exp (int_to_pc 0)))
    103;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 4;
      return_value w_28 (get_env_slot w_28 0) (pc_to_exp (int_to_pc 0)))
    104;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 4;
      return_value w_29 (get_env_slot w_29 1) (pc_to_exp (int_to_pc 0)))
    105;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 4;
      assert_env_length w_30 4;
      match Memo.list_match (get_env_slot w_30 2) with
      | None -> failwith "unreachable (106)"
      | Some pair_5 -> (
          let tag_5 = Word.get_value (fst pair_5) in
          match tag_5 with
          | 1 (* tag_X *) -> w_30.state.c <- pc_to_exp (int_to_pc 104)
          | 2 (* tag_Y *) -> w_30.state.c <- pc_to_exp (int_to_pc 105)
          | _ -> failwith "unreachable (106)"))
    106;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 4;
      let arg0_12 = get_env_slot w_31 3 in
      let arg1_5 = get_env_slot w_31 0 in
      let arg2_0 = get_env_slot w_31 1 in
      assert_env_length w_31 4;
      w_31.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_11; collect_env_slots w_31 [ 0; 1; 2 ]; w_31.state.k ];
      init_frame w_31 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_31 2 arg0_12;
      set_env_slot w_31 0 arg1_5;
      set_env_slot w_31 1 arg2_0;
      w_31.state.c <- pc_to_exp (int_to_pc 109))
    107;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 4;
      let arg0_14 = get_env_slot w_34 3 in
      let arg1_7 = get_env_slot w_34 0 in
      let arg2_2 = get_env_slot w_34 1 in
      assert_env_length w_34 4;
      w_34.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_13; collect_env_slots w_34 [ 0; 1; 2 ]; w_34.state.k ];
      init_frame w_34 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_34 2 arg0_14;
      set_env_slot w_34 0 arg1_7;
      set_env_slot w_34 1 arg2_2;
      w_34.state.c <- pc_to_exp (int_to_pc 109))
    108;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 4;
      assert_env_length w_37 4;
      match Memo.list_match (get_env_slot w_37 2) with
      | None -> failwith "unreachable (109)"
      | Some pair_6 -> (
          let tag_6 = Word.get_value (fst pair_6) in
          match tag_6 with
          | 3 (* tag_Const *) ->
              let parts_8 = Memo.splits (snd pair_6) in
              if List.length parts_8 = 1 then (
                let part0_8 = List.nth parts_8 0 in
                set_env_slot w_37 0 part0_8;
                w_37.state.c <- pc_to_exp (int_to_pc 103))
              else failwith "unreachable (109)"
          | 4 (* tag_Var *) ->
              let parts_9 = Memo.splits (snd pair_6) in
              if List.length parts_9 = 1 then (
                let part0_9 = List.nth parts_9 0 in
                set_env_slot w_37 2 part0_9;
                w_37.state.c <- pc_to_exp (int_to_pc 106))
              else failwith "unreachable (109)"
          | 5 (* tag_Add *) ->
              let parts_10 = Memo.splits (snd pair_6) in
              if List.length parts_10 = 2 then (
                let part0_10 = List.nth parts_10 0 in
                let part1_4 = List.nth parts_10 1 in
                set_env_slot w_37 3 part0_10;
                set_env_slot w_37 2 part1_4;
                w_37.state.c <- pc_to_exp (int_to_pc 107))
              else failwith "unreachable (109)"
          | 6 (* tag_Mul *) ->
              let parts_11 = Memo.splits (snd pair_6) in
              if List.length parts_11 = 2 then (
                let part0_11 = List.nth parts_11 0 in
                let part1_5 = List.nth parts_11 1 in
                set_env_slot w_37 3 part0_11;
                set_env_slot w_37 2 part1_5;
                w_37.state.c <- pc_to_exp (int_to_pc 108))
              else failwith "unreachable (109)"
          | _ -> failwith "unreachable (109)"))
    109;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 1;
      let arg0_9 = get_env_slot w_24 0 in
      assert_env_length w_24 1;
      w_24.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; collect_env_slots w_24 []; w_24.state.k ];
      init_frame w_24 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_24 0 arg0_9;
      w_24.state.c <- pc_to_exp (int_to_pc 102))
    110;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 4;
      let arg0_1 = get_env_slot w_4 0 in
      assert_env_length w_4 4;
      w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_0; collect_env_slots w_4 [ 1 ]; w_4.state.k ];
      init_frame w_4 1 (Memo.from_constructor tag_cont_done);
      set_env_slot w_4 0 arg0_1;
      w_4.state.c <- pc_to_exp (int_to_pc 3))
    111;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 4;
      assert_env_length w_5 4;
      let lhs_1 = Memo.to_word (get_env_slot w_5 1) in
      let rhs_1 = Memo.to_word (get_env_slot w_5 0) in
      set_env_slot w_5 0 (Memo.from_int (if Word.get_value lhs_1 = Word.get_value rhs_1 then 1 else 0));
      return_value w_5 (get_env_slot w_5 0) (pc_to_exp (int_to_pc 0)))
    112;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 4;
      let arg0_3 = get_env_slot w_9 1 in
      let arg1_1 = get_env_slot w_9 0 in
      assert_env_length w_9 4;
      w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; collect_env_slots w_9 [ 2 ]; w_9.state.k ];
      init_frame w_9 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_9 1 arg0_3;
      set_env_slot w_9 0 arg1_1;
      w_9.state.c <- pc_to_exp (int_to_pc 46))
    113;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      assert_env_length w_10 4;
      let lhs_2 = Memo.to_word (get_env_slot w_10 2) in
      let rhs_2 = Memo.to_word (get_env_slot w_10 0) in
      set_env_slot w_10 0 (Memo.from_int (if Word.get_value lhs_2 <> 0 && Word.get_value rhs_2 <> 0 then 1 else 0));
      return_value w_10 (get_env_slot w_10 0) (pc_to_exp (int_to_pc 0)))
    114;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 4;
      let arg0_5 = get_env_slot w_14 1 in
      let arg1_3 = get_env_slot w_14 0 in
      assert_env_length w_14 4;
      w_14.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; collect_env_slots w_14 [ 2 ]; w_14.state.k ];
      init_frame w_14 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_14 1 arg0_5;
      set_env_slot w_14 0 arg1_3;
      w_14.state.c <- pc_to_exp (int_to_pc 46))
    115;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 4;
      assert_env_length w_15 4;
      let lhs_3 = Memo.to_word (get_env_slot w_15 2) in
      let rhs_3 = Memo.to_word (get_env_slot w_15 0) in
      set_env_slot w_15 0 (Memo.from_int (if Word.get_value lhs_3 <> 0 && Word.get_value rhs_3 <> 0 then 1 else 0));
      return_value w_15 (get_env_slot w_15 0) (pc_to_exp (int_to_pc 0)))
    116;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 2;
      let arg0_7 = get_env_slot w_20 1 in
      let arg1_4 = get_env_slot w_20 0 in
      assert_env_length w_20 2;
      w_20.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; collect_env_slots w_20 [ 0 ]; w_20.state.k ];
      init_frame w_20 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_20 1 arg0_7;
      set_env_slot w_20 0 arg1_4;
      w_20.state.c <- pc_to_exp (int_to_pc 46))
    117;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 2;
      if Word.get_value (Memo.to_word (get_env_slot w_21 1)) <> 0 then w_21.state.c <- pc_to_exp (int_to_pc 94)
      else w_21.state.c <- pc_to_exp (int_to_pc 95))
    118;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 1;
      let arg0_10 = get_env_slot w_25 0 in
      assert_env_length w_25 1;
      w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; collect_env_slots w_25 []; w_25.state.k ];
      init_frame w_25 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_25 0 arg0_10;
      w_25.state.c <- pc_to_exp (int_to_pc 102))
    119;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 1;
      let arg0_11 = get_env_slot w_26 0 in
      assert_env_length w_26 1;
      init_frame w_26 2 (Memo.from_constructor tag_cont_done);
      set_env_slot w_26 1 arg0_11;
      w_26.state.c <- pc_to_exp (int_to_pc 93))
    120;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 4;
      let arg0_13 = get_env_slot w_32 2 in
      let arg1_6 = get_env_slot w_32 0 in
      let arg2_1 = get_env_slot w_32 1 in
      assert_env_length w_32 4;
      w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; collect_env_slots w_32 [ 3 ]; w_32.state.k ];
      init_frame w_32 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_32 2 arg0_13;
      set_env_slot w_32 0 arg1_6;
      set_env_slot w_32 1 arg2_1;
      w_32.state.c <- pc_to_exp (int_to_pc 109))
    121;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 4;
      assert_env_length w_33 4;
      let lhs_4 = Memo.to_word (get_env_slot w_33 3) in
      let rhs_4 = Memo.to_word (get_env_slot w_33 0) in
      set_env_slot w_33 0 (Memo.from_int (Word.get_value lhs_4 + Word.get_value rhs_4));
      return_value w_33 (get_env_slot w_33 0) (pc_to_exp (int_to_pc 0)))
    122;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 4;
      let arg0_15 = get_env_slot w_35 2 in
      let arg1_8 = get_env_slot w_35 0 in
      let arg2_3 = get_env_slot w_35 1 in
      assert_env_length w_35 4;
      w_35.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; collect_env_slots w_35 [ 3 ]; w_35.state.k ];
      init_frame w_35 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_35 2 arg0_15;
      set_env_slot w_35 0 arg1_8;
      set_env_slot w_35 1 arg2_3;
      w_35.state.c <- pc_to_exp (int_to_pc 109))
    123;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 4;
      assert_env_length w_36 4;
      let lhs_5 = Memo.to_word (get_env_slot w_36 3) in
      let rhs_5 = Memo.to_word (get_env_slot w_36 0) in
      set_env_slot w_36 0 (Memo.from_int (Word.get_value lhs_5 * Word.get_value rhs_5));
      return_value w_36 (get_env_slot w_36 0) (pc_to_exp (int_to_pc 0)))
    124;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 5;
      let arg0_17 = get_env_slot w_46 1 in
      assert_env_length w_46 5;
      w_46.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; collect_env_slots w_46 [ 0 ]; w_46.state.k ];
      init_frame w_46 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_46 0 arg0_17;
      w_46.state.c <- pc_to_exp (int_to_pc 92))
    125;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 5;
      let arg0_18 = get_env_slot w_47 0 in
      let arg1_9 = get_env_slot w_47 2 in
      assert_env_length w_47 5;
      w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; collect_env_slots w_47 [ 0; 2 ]; w_47.state.k ];
      init_frame w_47 4 (Memo.from_constructor tag_cont_done);
      set_env_slot w_47 1 arg0_18;
      set_env_slot w_47 0 arg1_9;
      w_47.state.c <- pc_to_exp (int_to_pc 46))
    126;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 5;
      if Word.get_value (Memo.to_word (get_env_slot w_48 1)) <> 0 then w_48.state.c <- pc_to_exp (int_to_pc 50)
      else w_48.state.c <- pc_to_exp (int_to_pc 69))
    127;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 5;
      let arg0_20 = get_env_slot w_91 2 in
      assert_env_length w_91 5;
      w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; collect_env_slots w_91 [ 0; 2 ]; w_91.state.k ];
      init_frame w_91 5 (Memo.from_constructor tag_cont_done);
      set_env_slot w_91 0 arg0_20;
      w_91.state.c <- pc_to_exp (int_to_pc 92))
    128;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 5;
      assert_env_length w_92 5;
      match Memo.list_match (get_env_slot w_92 0) with
      | None -> w_92.state.c <- pc_to_exp (int_to_pc 90)
      | Some pair_16 -> (
          let tag_16 = Word.get_value (fst pair_16) in
          match tag_16 with
          | 3 (* tag_Const *) ->
              let parts_28 = Memo.splits (snd pair_16) in
              if List.length parts_28 = 1 then (
                let part0_28 = List.nth parts_28 0 in
                set_env_slot w_92 2 part0_28;
                w_92.state.c <- pc_to_exp (int_to_pc 70))
              else w_92.state.c <- pc_to_exp (int_to_pc 90)
          | 5 (* tag_Add *) ->
              let parts_29 = Memo.splits (snd pair_16) in
              if List.length parts_29 = 2 then (
                let part0_29 = List.nth parts_29 0 in
                let part1_13 = List.nth parts_29 1 in
                set_env_slot w_92 1 part0_29;
                set_env_slot w_92 0 part1_13;
                w_92.state.c <- pc_to_exp (int_to_pc 81))
              else w_92.state.c <- pc_to_exp (int_to_pc 90)
          | _ -> w_92.state.c <- pc_to_exp (int_to_pc 90)))
    129;
  add_exp
    (fun w_98 ->
      assert_env_length w_98 5;
      let arg0_22 = get_env_slot w_98 0 in
      assert_env_length w_98 5;
      w_98.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_19; collect_env_slots w_98 [ 0; 1; 2 ]; w_98.state.k ];
      init_frame w_98 1 (Memo.from_constructor tag_cont_done);
      set_env_slot w_98 0 arg0_22;
      w_98.state.c <- pc_to_exp (int_to_pc 8))
    130;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 5;
      assert_env_length w_99 5;
      let lhs_17 = Memo.to_word (get_env_slot w_99 2) in
      let rhs_17 = Memo.to_word (get_env_slot w_99 3) in
      set_env_slot w_99 4 (Memo.from_int (if Word.get_value lhs_17 < Word.get_value rhs_17 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_99 4)) <> 0 then w_99.state.c <- pc_to_exp (int_to_pc 10)
      else w_99.state.c <- pc_to_exp (int_to_pc 11))
    131;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 5;
      let arg0_24 = get_env_slot w_110 2 in
      assert_env_length w_110 5;
      w_110.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; collect_env_slots w_110 [ 0 ]; w_110.state.k ];
      init_frame w_110 1 (Memo.from_constructor tag_cont_done);
      set_env_slot w_110 0 arg0_24;
      w_110.state.c <- pc_to_exp (int_to_pc 3))
    132;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 5;
      assert_env_length w_111 5;
      let lhs_23 = Memo.to_word (get_env_slot w_111 0) in
      let rhs_23 = Memo.to_word (get_env_slot w_111 1) in
      set_env_slot w_111 2 (Memo.from_int (if Word.get_value lhs_23 < Word.get_value rhs_23 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_111 2)) <> 0 then w_111.state.c <- pc_to_exp (int_to_pc 20)
      else w_111.state.c <- pc_to_exp (int_to_pc 21))
    133;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 5;
      assert_env_length w_118 5;
      let lhs_26 = Memo.to_word (get_env_slot w_118 2) in
      let rhs_26 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_118 3 (Memo.from_int (if Word.get_value lhs_26 = Word.get_value rhs_26 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_118 3)) <> 0 then w_118.state.c <- pc_to_exp (int_to_pc 26)
      else w_118.state.c <- pc_to_exp (int_to_pc 27))
    134;
  add_exp
    (fun w_123 ->
      assert_env_length w_123 5;
      assert_env_length w_123 5;
      let lhs_27 = Memo.to_word (get_env_slot w_123 2) in
      let rhs_27 = Memo.to_word (Memo.from_int 0) in
      set_env_slot w_123 3 (Memo.from_int (if Word.get_value lhs_27 = Word.get_value rhs_27 then 1 else 0));
      if Word.get_value (Memo.to_word (get_env_slot w_123 3)) <> 0 then w_123.state.c <- pc_to_exp (int_to_pc 30)
      else w_123.state.c <- pc_to_exp (int_to_pc 31))
    135;
  add_exp
    (fun w_133 ->
      assert_env_length w_133 3;
      let arg0_30 = get_env_slot w_133 0 in
      assert_env_length w_133 3;
      w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; collect_env_slots w_133 [ 1 ]; w_133.state.k ];
      init_frame w_133 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_133 0 arg0_30;
      w_133.state.c <- pc_to_exp (int_to_pc 102))
    136;
  add_exp
    (fun w_134 ->
      assert_env_length w_134 3;
      assert_env_length w_134 3;
      set_env_slot w_134 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_134 1; get_env_slot w_134 0 ]);
      return_value w_134 (get_env_slot w_134 0) (pc_to_exp (int_to_pc 0)))
    137;
  add_exp
    (fun w_136 ->
      assert_env_length w_136 3;
      assert_env_length w_136 3;
      set_env_slot w_136 1 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_136 1; get_env_slot w_136 2 ]);
      let arg0_32 = get_env_slot w_136 2 in
      assert_env_length w_136 3;
      w_136.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_27; collect_env_slots w_136 [ 0; 1 ]; w_136.state.k ];
      init_frame w_136 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_136 0 arg0_32;
      w_136.state.c <- pc_to_exp (int_to_pc 102))
    138;
  add_exp
    (fun w_137 ->
      assert_env_length w_137 3;
      assert_env_length w_137 3;
      set_env_slot w_137 0 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_137 0; get_env_slot w_137 2 ]);
      assert_env_length w_137 3;
      set_env_slot w_137 0 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_137 1; get_env_slot w_137 0 ]);
      return_value w_137 (get_env_slot w_137 0) (pc_to_exp (int_to_pc 0)))
    139;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 1;
  Words.set_constructor_degree 3 0;
  Words.set_constructor_degree 4 0;
  Words.set_constructor_degree 5 (-1);
  Words.set_constructor_degree 6 (-1);
  Words.set_constructor_degree 7 (-1);
  Words.set_constructor_degree 8 (-1);
  Words.set_constructor_degree 9 (-1);
  Words.set_constructor_degree 10 (-2);
  Words.set_constructor_degree 11 (-1);
  Words.set_constructor_degree 12 (-2);
  Words.set_constructor_degree 13 (-1);
  Words.set_constructor_degree 14 (-1);
  Words.set_constructor_degree 15 0;
  Words.set_constructor_degree 16 0;
  Words.set_constructor_degree 17 (-1);
  Words.set_constructor_degree 18 (-3);
  Words.set_constructor_degree 19 (-1);
  Words.set_constructor_degree 20 (-3);
  Words.set_constructor_degree 21 (-2);
  Words.set_constructor_degree 22 (-1);
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 (-2);
  Words.set_constructor_degree 25 (-1);
  Words.set_constructor_degree 26 (-3);
  Words.set_constructor_degree 27 (-2);
  Words.set_constructor_degree 28 (-1);
  Words.set_constructor_degree 29 (-1);
  Words.set_constructor_degree 30 (-2);
  Words.set_constructor_degree 31 (-2);
  Words.set_constructor_degree 32 (-1);
  Words.set_constructor_degree 33 (-1);
  Words.set_constructor_degree 34 (-2);
  Words.set_constructor_degree 35 (-2)
