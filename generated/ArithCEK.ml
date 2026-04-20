open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Z = 1
let tag_S = 2
let tag_X = 3
let tag_Y = 4
let tag_Const = 5
let tag_Var = 6
let tag_Add = 7
let tag_Mul = 8
let tag_Missing = 9
let tag_Found = 10
let tag_ENil = 11
let tag_ECons = 12
let tag_NoPick = 13
let tag_Pick = 14
let tag_cont_1 = 15
let tag_cont_2 = 16
let tag_cont_3 = 17
let tag_cont_4 = 18
let tag_cont_5 = 19
let tag_cont_6 = 20
let tag_cont_7 = 21
let tag_cont_8 = 22
let tag_cont_9 = 23
let tag_cont_10 = 24
let tag_cont_11 = 25
let tag_cont_12 = 26
let tag_cont_13 = 27
let tag_cont_14 = 28
let tag_cont_15 = 29
let tag_cont_16 = 30
let tag_cont_17 = 31
let tag_cont_18 = 32
let tag_cont_19 = 33
let tag_cont_20 = 34
let tag_cont_21 = 35
let tag_cont_22 = 36
let tag_cont_23 = 37
let tag_cont_24 = 38
let tag_cont_25 = 39
let tag_cont_26 = 40
let tag_cont_27 = 41
let tag_cont_28 = 42
let tag_cont_29 = 43
let tag_cont_30 = 44
let tag_cont_31 = 45
let tag_cont_32 = 46
let tag_cont_33 = 47
let tag_cont_34 = 48
let tag_cont_35 = 49
let tag_cont_36 = 50
let tag_cont_37 = 51
let tag_cont_38 = 52
let tag_cont_39 = 53
let tag_cont_40 = 54
let tag_cont_41 = 55
let tag_cont_42 = 56
let tag_cont_43 = 57
let tag_cont_44 = 58
let tag_cont_45 = 59
let tag_cont_46 = 60
let tag_cont_47 = 61
let tag_cont_48 = 62
let tag_cont_49 = 63
let tag_cont_50 = 64
let tag_cont_51 = 65
let tag_cont_52 = 66
let tag_cont_53 = 67
let tag_cont_54 = 68
let tag_cont_55 = 69
let tag_cont_56 = 70
let tag_cont_57 = 71
let tag_cont_58 = 72
let tag_cont_59 = 73
let tag_cont_60 = 74
let tag_cont_61 = 75
let tag_cont_62 = 76
let tag_cont_63 = 77
let tag_cont_64 = 78
let tag_cont_65 = 79
let tag_cont_66 = 80
let tag_cont_67 = 81
let tag_cont_68 = 82
let tag_cont_69 = 83
let tag_cont_70 = 84
let tag_cont_71 = 85
let tag_cont_72 = 86
let tag_cont_73 = 87
let tag_cont_74 = 88
let tag_cont_75 = 89
let tag_cont_76 = 90
let tag_cont_77 = 91
let tag_cont_78 = 92
let tag_cont_79 = 93
let tag_cont_80 = 94
let tag_cont_81 = 95
let tag_cont_82 = 96
let tag_cont_83 = 97
let tag_cont_84 = 98
let tag_cont_85 = 99
let tag_cont_86 = 100
let tag_cont_87 = 101
let tag_cont_88 = 102
let tag_cont_89 = 103
let tag_cont_90 = 104
let tag_cont_91 = 105
let tag_cont_92 = 106
let tag_cont_93 = 107
let tag_cont_94 = 108
let tag_cont_95 = 109
let tag_cont_96 = 110
let tag_cont_97 = 111
let tag_cont_98 = 112
let tag_cont_99 = 113
let tag_cont_100 = 114
let tag_cont_101 = 115
let tag_cont_102 = 116
let tag_cont_103 = 117
let tag_cont_104 = 118
let tag_cont_105 = 119
let tag_cont_106 = 120
let tag_cont_107 = 121
let tag_cont_108 = 122
let tag_cont_109 = 123
let tag_cont_110 = 124
let tag_cont_111 = 125
let tag_cont_112 = 126
let tag_cont_113 = 127
let tag_cont_114 = 128
let tag_cont_115 = 129
let tag_cont_116 = 130
let tag_cont_117 = 131
let tag_cont_118 = 132

type nat = Z | S of nat

let rec from_ocaml_nat x =
  match x with
  | Z -> Memo.appends [ Memo.from_constructor tag_Z ]
  | S x0 -> Memo.appends [ Memo.from_constructor tag_S; from_ocaml_nat x0 ]

let rec to_ocaml_nat x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 (* tag_Z *) -> Z
  | 2 (* tag_S *) ->
      let x0 = Memo.splits_1 t in
      S (to_ocaml_nat x0)
  | _ -> failwith "unreachable"

type var = X | Y

let rec from_ocaml_var x =
  match x with X -> Memo.appends [ Memo.from_constructor tag_X ] | Y -> Memo.appends [ Memo.from_constructor tag_Y ]

let rec to_ocaml_var x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with 3 (* tag_X *) -> X | 4 (* tag_Y *) -> Y | _ -> failwith "unreachable"

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
  | 5 (* tag_Const *) ->
      let x0 = Memo.splits_1 t in
      Const (Word.get_value (Memo.to_word x0))
  | 6 (* tag_Var *) ->
      let x0 = Memo.splits_1 t in
      Var (to_ocaml_var x0)
  | 7 (* tag_Add *) ->
      let x0, x1 = Memo.splits_2 t in
      Add (to_ocaml_expr x0, to_ocaml_expr x1)
  | 8 (* tag_Mul *) ->
      let x0, x1 = Memo.splits_2 t in
      Mul (to_ocaml_expr x0, to_ocaml_expr x1)
  | _ -> failwith "unreachable"

type factor_result = Missing | Found of expr

let rec from_ocaml_factor_result x =
  match x with
  | Missing -> Memo.appends [ Memo.from_constructor tag_Missing ]
  | Found x0 -> Memo.appends [ Memo.from_constructor tag_Found; from_ocaml_expr x0 ]

let rec to_ocaml_factor_result x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 9 (* tag_Missing *) -> Missing
  | 10 (* tag_Found *) ->
      let x0 = Memo.splits_1 t in
      Found (to_ocaml_expr x0)
  | _ -> failwith "unreachable"

type expr_list = ENil | ECons of expr * expr_list

let rec from_ocaml_expr_list x =
  match x with
  | ENil -> Memo.appends [ Memo.from_constructor tag_ENil ]
  | ECons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ECons; from_ocaml_expr x0; from_ocaml_expr_list x1 ]

let rec to_ocaml_expr_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 11 (* tag_ENil *) -> ENil
  | 12 (* tag_ECons *) ->
      let x0, x1 = Memo.splits_2 t in
      ECons (to_ocaml_expr x0, to_ocaml_expr_list x1)
  | _ -> failwith "unreachable"

type pick_result = NoPick | Pick of expr * expr_list

let rec from_ocaml_pick_result x =
  match x with
  | NoPick -> Memo.appends [ Memo.from_constructor tag_NoPick ]
  | Pick (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Pick; from_ocaml_expr x0; from_ocaml_expr_list x1 ]

let rec to_ocaml_pick_result x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 13 (* tag_NoPick *) -> NoPick
  | 14 (* tag_Pick *) ->
      let x0, x1 = Memo.splits_2 t in
      Pick (to_ocaml_expr x0, to_ocaml_expr_list x1)
  | _ -> failwith "unreachable"

let rec var_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec compare_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_equal memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 6)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_size memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 13)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec better_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 15)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec scale memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 16)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec coeff_value memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 23)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec coeff_base memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 26)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec extract_factor memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 29)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_factor memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 30)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec append_exprs memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 32)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec insert_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 34)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec sort_exprs memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 36)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec compare_add_term memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 38)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec insert_add_term memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 39)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec sort_add_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 41)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec reverse_exprs_aux memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 43)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec reverse_exprs memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 45)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec flatten_add memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 46)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec flatten_mul memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 50)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_coeff memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 52)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_base memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 55)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_total_coeff memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 58)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_bases memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 60)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec build_mul memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 62)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize_mul_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 65)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec combine_like_terms_acc memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 66)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec combine_like_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 70)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec factor_adjacent memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 72)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec pick_factored memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 75)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 77)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec build_add memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 79)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_round memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 82)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize_add_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 83)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_opt memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 84)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 87)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec simplify_aux memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 89)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec diffx memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 90)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 93)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec main memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 96)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_96 ->
      assert_env_length w_96 1;
      let hd_0, tl_0 = resolve w_96 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_96
      | 15 (* tag_cont_1 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let keep_40 = env_call w_96 [ 0; 1; 2 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_40; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 3)
      | 16 (* tag_cont_2 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_41 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_41; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 1)
      | 17 (* tag_cont_3 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_42 = env_call w_96 [ 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_42; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 18 (* tag_cont_4 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_43 = env_call w_96 [ 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_43; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 19 (* tag_cont_5 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 97)
      | 20 (* tag_cont_6 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 98)
      | 21 (* tag_cont_7 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let keep_46 = env_call w_96 [ 0; 1; 2 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; keep_46; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 13)
      | 22 (* tag_cont_8 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 100)
      | 23 (* tag_cont_9 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 4);
          w_96.state.c <- pc_to_exp (int_to_pc 101)
      | 24 (* tag_cont_10 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_34 = pop_env w_96 in
          let ctor_arg_35 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_35; ctor_arg_34 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 25 (* tag_cont_11 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 103)
      | 26 (* tag_cont_12 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 34)
      | 27 (* tag_cont_13 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let keep_50 = env_call w_96 [ 0; 1; 2 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; keep_50; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 26)
      | 28 (* tag_cont_14 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 105)
      | 29 (* tag_cont_15 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 39)
      | 30 (* tag_cont_16 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_52 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; keep_52; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 46)
      | 31 (* tag_cont_17 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_53 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_56; keep_53; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 50)
      | 32 (* tag_cont_18 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_54 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; keep_54; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 58)
      | 33 (* tag_cont_19 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 108)
      | 34 (* tag_cont_20 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_40 = pop_env w_96 in
          let ctor_arg_41 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_41; ctor_arg_40 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 35 (* tag_cont_21 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_57 = env_call w_96 [ 0 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_60; keep_57; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 58)
      | 36 (* tag_cont_22 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_58 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_61; keep_58; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 50)
      | 37 (* tag_cont_23 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Memo.from_constructor tag_ENil);
          assert_env_length w_96 2;
          let ctor_arg_42 = pop_env w_96 in
          let ctor_arg_43 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_43; ctor_arg_42 ]);
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 38 (* tag_cont_24 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 6;
          let keep_59 = env_call w_96 [ 0; 1; 3; 4 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_62; keep_59; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 23)
      | 39 (* tag_cont_25 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 4;
          let keep_60 = env_call w_96 [ 1; 2 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_63; keep_60; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 23)
      | 40 (* tag_cont_26 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 4);
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 7;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 8;
          let ctor_arg_44 = pop_env w_96 in
          let ctor_arg_45 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_45; ctor_arg_44 ]);
          assert_env_length w_96 7;
          let keep_61 = env_call w_96 [ 0; 1; 3; 4 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_64; keep_61; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 41 (* tag_cont_27 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 7;
          let ctor_arg_46 = pop_env w_96 in
          let ctor_arg_47 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_47; ctor_arg_46 ]);
          assert_env_length w_96 6;
          let keep_62 = env_call w_96 [ 0; 1; 2; 3 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_65; keep_62; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 42 (* tag_cont_28 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 109)
      | 43 (* tag_cont_29 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_48 = pop_env w_96 in
          let ctor_arg_49 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_49; ctor_arg_48 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 44 (* tag_cont_30 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_65 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_68; keep_65; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 77)
      | 45 (* tag_cont_31 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_66 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_69; keep_66; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 41)
      | 46 (* tag_cont_32 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_67 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_70; keep_67; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 46)
      | 47 (* tag_cont_33 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_68 = env_call w_96 [ 0; 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_71; keep_68; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 84)
      | 48 (* tag_cont_34 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_69 = env_call w_96 [ 0; 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_72; keep_69; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 84)
      | 49 (* tag_cont_35 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_70 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_73; keep_70; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 87)
      | 50 (* tag_cont_36 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_71 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_74; keep_71; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 87)
      | 51 (* tag_cont_37 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let keep_72 = env_call w_96 [ 1 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_75; keep_72; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 52 (* tag_cont_38 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_73 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_76; keep_73; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 90)
      | 53 (* tag_cont_39 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let ctor_arg_50 = pop_env w_96 in
          let ctor_arg_51 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_51; ctor_arg_50 ]);
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_74 = env_call w_96 [ 2; 3 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_77; keep_74; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 90)
      | 54 (* tag_cont_40 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 7;
          let keep_75 = env_call w_96 [ 3 ] 3 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_78; keep_75; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 93)
      | 55 (* tag_cont_41 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 7;
          let keep_76 = env_call w_96 [ 3 ] 3 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_79; keep_76; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 93)
      | 56 (* tag_cont_42 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_77 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_80; keep_77; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 90)
      | 57 (* tag_cont_43 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          w_96.state.c <- pc_to_exp (int_to_pc 124)
      | 58 (* tag_cont_44 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 125)
      | 59 (* tag_cont_45 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 126)
      | 60 (* tag_cont_46 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 127)
      | 61 (* tag_cont_47 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 128)
      | 62 (* tag_cont_48 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 129)
      | 63 (* tag_cont_49 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          w_96.state.c <- pc_to_exp (int_to_pc 133)
      | 64 (* tag_cont_50 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          w_96.state.c <- pc_to_exp (int_to_pc 134)
      | 65 (* tag_cont_51 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 4);
          w_96.state.c <- pc_to_exp (int_to_pc 135)
      | 66 (* tag_cont_52 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_61 = pop_env w_96 in
          let ctor_arg_62 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_62; ctor_arg_61 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 67 (* tag_cont_53 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          assert_env_length w_96 6;
          let keep_83 = env_call w_96 [ 0; 1 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_86; keep_83; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 5)
      | 68 (* tag_cont_54 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_63 = pop_env w_96 in
          let ctor_arg_64 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_64; ctor_arg_63 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 69 (* tag_cont_55 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 32)
      | 70 (* tag_cont_56 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 32)
      | 71 (* tag_cont_57 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 136)
      | 72 (* tag_cont_58 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 34)
      | 73 (* tag_cont_59 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 34)
      | 74 (* tag_cont_60 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 3;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 138)
      | 75 (* tag_cont_61 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let keep_85 = env_call w_96 [] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_85; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 32)
      | 76 (* tag_cont_62 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          assert_env_length w_96 7;
          let keep_86 = env_call w_96 [ 0; 1; 2; 3; 4 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_88; keep_86; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 77 (* tag_cont_63 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 6;
          ignore (env_call w_96 [] 3);
          w_96.state.c <- pc_to_exp (int_to_pc 66)
      | 78 (* tag_cont_64 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 139)
      | 79 (* tag_cont_65 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 4 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 140)
      | 80 (* tag_cont_66 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_68 = pop_env w_96 in
          let ctor_arg_69 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_69; ctor_arg_68 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 81 (* tag_cont_67 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          ignore (env_call w_96 [] 1);
          w_96.state.c <- pc_to_exp (int_to_pc 77)
      | 82 (* tag_cont_68 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_90 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_92; keep_90; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 72)
      | 83 (* tag_cont_69 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_91 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_93; keep_91; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 70)
      | 84 (* tag_cont_70 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let keep_92 = env_call w_96 [] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_92; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 32)
      | 85 (* tag_cont_71 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          let keep_93 = env_call w_96 [ 0; 1; 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_94; keep_93; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 83)
      | 86 (* tag_cont_72 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          let keep_94 = env_call w_96 [ 0; 1; 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_95; keep_94; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 65)
      | 87 (* tag_cont_73 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          w_96.state.c <- pc_to_exp (int_to_pc 143)
      | 88 (* tag_cont_74 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          w_96.state.c <- pc_to_exp (int_to_pc 148)
      | 89 (* tag_cont_75 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 149)
      | 90 (* tag_cont_76 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_70 = pop_env w_96 in
          let ctor_arg_71 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_71; ctor_arg_70 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 91 (* tag_cont_77 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          let ctor_arg_72 = pop_env w_96 in
          let ctor_arg_73 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_73; ctor_arg_72 ]);
          assert_env_length w_96 2;
          let ctor_arg_74 = pop_env w_96 in
          let ctor_arg_75 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_75; ctor_arg_74 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 92 (* tag_cont_78 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 150)
      | 93 (* tag_cont_79 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 151)
      | 94 (* tag_cont_80 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 1);
          w_96.state.c <- pc_to_exp (int_to_pc 89)
      | 95 (* tag_cont_81 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_95 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_96; keep_95; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 1)
      | 96 (* tag_cont_82 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 153)
      | 97 (* tag_cont_83 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 155)
      | 98 (* tag_cont_84 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 157)
      | 99 (* tag_cont_85 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 158)
      | 100 (* tag_cont_86 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 160)
      | 101 (* tag_cont_87 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 3;
          let keep_97 = env_call w_96 [ 0 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_98; keep_97; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 62)
      | 102 (* tag_cont_88 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 5 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 162)
      | 103 (* tag_cont_89 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          ignore (env_call w_96 [] 1);
          w_96.state.c <- pc_to_exp (int_to_pc 72)
      | 104 (* tag_cont_90 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let ctor_arg_79 = pop_env w_96 in
          let ctor_arg_80 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_80; ctor_arg_79 ]);
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          drop_n w_96 1 0;
          assert_env_length w_96 1;
          return_n w_96 1 (pc_to_exp (int_to_pc 0))
      | 105 (* tag_cont_91 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 163)
      | 106 (* tag_cont_92 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 1);
          w_96.state.c <- pc_to_exp (int_to_pc 77)
      | 107 (* tag_cont_93 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_99 = env_call w_96 [ 0 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_100; keep_99; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 82)
      | 108 (* tag_cont_94 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 6;
          let keep_100 = env_call w_96 [ 0; 3 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_101; keep_100; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 83)
      | 109 (* tag_cont_95 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 6;
          let keep_101 = env_call w_96 [ 0; 3 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_102; keep_101; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 65)
      | 110 (* tag_cont_96 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 168)
      | 111 (* tag_cont_97 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let keep_102 = env_call w_96 [ 0; 1; 2 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_103; keep_102; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 23)
      | 112 (* tag_cont_98 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          push_env w_96 (Memo.from_int 1);
          w_96.state.c <- pc_to_exp (int_to_pc 170)
      | 113 (* tag_cont_99 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          push_env w_96 (Memo.from_int 0);
          w_96.state.c <- pc_to_exp (int_to_pc 172)
      | 114 (* tag_cont_100 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_104 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_106; keep_104; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 45)
      | 115 (* tag_cont_101 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          let keep_105 = env_call w_96 [ 0; 1 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_107; keep_105; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 83)
      | 116 (* tag_cont_102 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          let keep_106 = env_call w_96 [ 0; 1 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_108; keep_106; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 65)
      | 117 (* tag_cont_103 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          w_96.state.c <- pc_to_exp (int_to_pc 177)
      | 118 (* tag_cont_104 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          ignore (env_call w_96 [] 2);
          w_96.state.c <- pc_to_exp (int_to_pc 39)
      | 119 (* tag_cont_105 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_107 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_109; keep_107; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 79)
      | 120 (* tag_cont_106 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          let keep_108 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_105; keep_108; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 82)
      | 121 (* tag_cont_107 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_109 = env_call w_96 [ 0; 1 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_110; keep_109; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 83)
      | 122 (* tag_cont_108 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_110 = env_call w_96 [ 0; 1 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_111; keep_110; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 65)
      | 123 (* tag_cont_109 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          let keep_111 = env_call w_96 [ 1 ] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_112; keep_111; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 79)
      | 124 (* tag_cont_110 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_112 = env_call w_96 [ 0; 1; 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_113; keep_112; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 125 (* tag_cont_111 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 2 tl_0;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          let keep_113 = env_call w_96 [ 0; 1; 2 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_114; keep_113; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 6)
      | 126 (* tag_cont_112 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 1 tl_0;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 4;
          let keep_114 = env_call w_96 [] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_115; keep_114; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 15)
      | 127 (* tag_cont_113 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 178)
      | 128 (* tag_cont_114 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 3 tl_0;
          w_96.state.c <- pc_to_exp (int_to_pc 179)
      | 129 (* tag_cont_115 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          let keep_115 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_118; keep_115; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 46)
      | 130 (* tag_cont_116 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          push_env w_96 (Dynarray.get w_96.state.e 0);
          assert_env_length w_96 2;
          ignore (env_call w_96 [] 1);
          w_96.state.c <- pc_to_exp (int_to_pc 79)
      | 131 (* tag_cont_117 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          let keep_116 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_116; keep_116; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 70)
      | 132 (* tag_cont_118 *) ->
          w_96.state.k <- get_next_cont tl_0;
          restore_env w_96 0 tl_0;
          assert_env_length w_96 1;
          let keep_117 = env_call w_96 [] 1 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_117; keep_117; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 41)
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
      | 3 (* tag_X *) ->
          ignore (pop_env w_1);
          assert_env_length w_1 1;
          push_env w_1 (Memo.from_int 0);
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
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
      | 5 (* tag_Const *) ->
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
      | 6 (* tag_Var *) ->
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
      | 7 (* tag_Add *) ->
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
      | 8 (* tag_Mul *) ->
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
      | 5 (* tag_Const *) ->
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
      | 6 (* tag_Var *) ->
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
      | 7 (* tag_Add *) ->
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
      | 8 (* tag_Mul *) ->
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
      | 5 (* tag_Const *) ->
          let splits_4 = Memo.splits (snd x_2) in
          let split0_4 = List.nth splits_4 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_4;
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 8)
      | 6 (* tag_Var *) ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_6;
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 9)
      | 7 (* tag_Add *) ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          let split1_2 = List.nth splits_8 1 in
          ignore (pop_env w_6);
          push_env w_6 split0_8;
          push_env w_6 split1_2;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          w_6.state.c <- pc_to_exp (int_to_pc 10)
      | 8 (* tag_Mul *) ->
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
      | 5 (* tag_Const *) ->
          let splits_12 = Memo.splits (snd x_7) in
          let split0_12 = List.nth splits_12 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_12;
          assert_env_length w_13 2;
          push_env w_13 (Memo.from_int 1);
          assert_env_length w_13 3;
          drop_n w_13 3 1;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_13 = Memo.splits (snd x_7) in
          let split0_13 = List.nth splits_13 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_13;
          assert_env_length w_13 2;
          push_env w_13 (Memo.from_int 1);
          assert_env_length w_13 3;
          drop_n w_13 3 1;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_14 = Memo.splits (snd x_7) in
          let split0_14 = List.nth splits_14 0 in
          let split1_6 = List.nth splits_14 1 in
          ignore (pop_env w_13);
          push_env w_13 split0_14;
          push_env w_13 split1_6;
          assert_env_length w_13 3;
          push_env w_13 (Memo.from_int 1);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 5;
          let keep_4 = env_call w_13 [ 2; 3 ] 1 in
          w_13.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_13.state.k ];
          w_13.state.c <- pc_to_exp (int_to_pc 13)
      | 8 (* tag_Mul *) ->
          let splits_15 = Memo.splits (snd x_7) in
          let split0_15 = List.nth splits_15 0 in
          let split1_7 = List.nth splits_15 1 in
          ignore (pop_env w_13);
          push_env w_13 split0_15;
          push_env w_13 split1_7;
          assert_env_length w_13 3;
          push_env w_13 (Memo.from_int 1);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 5;
          let keep_5 = env_call w_13 [ 2; 3 ] 1 in
          w_13.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_13.state.k ];
          w_13.state.c <- pc_to_exp (int_to_pc 13)
      | _ -> failwith "unreachable (14)")
    14;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 2;
      push_env w_14 (Dynarray.get w_14.state.e 0);
      assert_env_length w_14 3;
      let keep_6 = env_call w_14 [ 0; 1 ] 1 in
      w_14.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_14.state.k ];
      w_14.state.c <- pc_to_exp (int_to_pc 13))
    15;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 2;
      push_env w_15 (Dynarray.get w_15.state.e 0);
      assert_env_length w_15 3;
      push_env w_15 (Memo.from_int 0);
      w_15.state.c <- pc_to_exp (int_to_pc 22))
    16;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 5;
      let x0_2 = resolve w_19 (Source.E 3) in
      let x1_2 = resolve w_19 (Source.E 4) in
      ignore (pop_env w_19);
      ignore (pop_env w_19);
      push_env w_19 (Memo.from_int (Word.get_value (fst x0_2) * Word.get_value (fst x1_2)));
      assert_env_length w_19 4;
      let ctor_arg_0 = pop_env w_19 in
      push_env w_19 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_0 ]);
      assert_env_length w_19 4;
      drop_n w_19 4 1;
      assert_env_length w_19 3;
      return_n w_19 3 (pc_to_exp (int_to_pc 0)))
    17;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 3;
      let cond_1 = resolve w_21 (Source.E 2) in
      ignore (pop_env w_21);
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_21 2;
        push_env w_21 (Dynarray.get w_21.state.e 1);
        assert_env_length w_21 3;
        return_n w_21 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_21 2;
        push_env w_21 (Dynarray.get w_21.state.e 0);
        assert_env_length w_21 3;
        let ctor_arg_1 = pop_env w_21 in
        push_env w_21 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_1 ]);
        assert_env_length w_21 3;
        push_env w_21 (Dynarray.get w_21.state.e 1);
        assert_env_length w_21 4;
        let ctor_arg_2 = pop_env w_21 in
        let ctor_arg_3 = pop_env w_21 in
        push_env w_21 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_3; ctor_arg_2 ]);
        assert_env_length w_21 3;
        return_n w_21 3 (pc_to_exp (int_to_pc 0))))
    18;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 4;
      let x0_3 = resolve w_20 (Source.E 2) in
      let x1_3 = resolve w_20 (Source.E 3) in
      ignore (pop_env w_20);
      ignore (pop_env w_20);
      push_env w_20 (Memo.from_int (if Word.get_value (fst x0_3) = Word.get_value (fst x1_3) then 1 else 0));
      w_20.state.c <- pc_to_exp (int_to_pc 18))
    19;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 3;
      let last_8 = Source.E 2 in
      let x_8 = resolve w_18 last_8 in
      match Word.get_value (fst x_8) with
      | 5 (* tag_Const *) ->
          let splits_16 = Memo.splits (snd x_8) in
          let split0_16 = List.nth splits_16 0 in
          ignore (pop_env w_18);
          push_env w_18 split0_16;
          assert_env_length w_18 3;
          push_env w_18 (Dynarray.get w_18.state.e 0);
          assert_env_length w_18 4;
          push_env w_18 (Dynarray.get w_18.state.e 2);
          w_18.state.c <- pc_to_exp (int_to_pc 17)
      | _ ->
          ignore (pop_env w_18);
          assert_env_length w_18 2;
          push_env w_18 (Dynarray.get w_18.state.e 0);
          assert_env_length w_18 3;
          push_env w_18 (Memo.from_int 1);
          w_18.state.c <- pc_to_exp (int_to_pc 19)
      | _ -> failwith "unreachable (20)")
    20;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 3;
      let cond_0 = resolve w_17 (Source.E 2) in
      ignore (pop_env w_17);
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_17 2;
        push_env w_17 (Memo.from_int 0);
        assert_env_length w_17 3;
        let ctor_arg_4 = pop_env w_17 in
        push_env w_17 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_4 ]);
        assert_env_length w_17 3;
        return_n w_17 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_17 2;
        push_env w_17 (Dynarray.get w_17.state.e 1);
        w_17.state.c <- pc_to_exp (int_to_pc 20)))
    21;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 4;
      let x0_1 = resolve w_16 (Source.E 2) in
      let x1_1 = resolve w_16 (Source.E 3) in
      ignore (pop_env w_16);
      ignore (pop_env w_16);
      push_env w_16 (Memo.from_int (if Word.get_value (fst x0_1) = Word.get_value (fst x1_1) then 1 else 0));
      w_16.state.c <- pc_to_exp (int_to_pc 21))
    22;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 1;
      push_env w_22 (Dynarray.get w_22.state.e 0);
      w_22.state.c <- pc_to_exp (int_to_pc 25))
    23;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 4;
      let last_10 = Source.E 3 in
      let x_10 = resolve w_24 last_10 in
      match Word.get_value (fst x_10) with
      | 5 (* tag_Const *) ->
          let splits_19 = Memo.splits (snd x_10) in
          let split0_19 = List.nth splits_19 0 in
          ignore (pop_env w_24);
          push_env w_24 split0_19;
          assert_env_length w_24 4;
          push_env w_24 (Dynarray.get w_24.state.e 3);
          assert_env_length w_24 5;
          drop_n w_24 5 1;
          assert_env_length w_24 4;
          drop_n w_24 4 2;
          assert_env_length w_24 2;
          return_n w_24 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_24);
          assert_env_length w_24 3;
          push_env w_24 (Memo.from_int 1);
          assert_env_length w_24 4;
          drop_n w_24 4 2;
          assert_env_length w_24 2;
          return_n w_24 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (24)")
    24;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 2;
      let last_9 = Source.E 1 in
      let x_9 = resolve w_23 last_9 in
      match Word.get_value (fst x_9) with
      | 5 (* tag_Const *) ->
          let splits_17 = Memo.splits (snd x_9) in
          let split0_17 = List.nth splits_17 0 in
          ignore (pop_env w_23);
          push_env w_23 split0_17;
          assert_env_length w_23 2;
          push_env w_23 (Dynarray.get w_23.state.e 1);
          assert_env_length w_23 3;
          drop_n w_23 3 1;
          assert_env_length w_23 2;
          return_n w_23 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_18 = Memo.splits (snd x_9) in
          let split0_18 = List.nth splits_18 0 in
          let split1_8 = List.nth splits_18 1 in
          ignore (pop_env w_23);
          push_env w_23 split0_18;
          push_env w_23 split1_8;
          assert_env_length w_23 3;
          push_env w_23 (Dynarray.get w_23.state.e 1);
          w_23.state.c <- pc_to_exp (int_to_pc 24)
      | _ ->
          ignore (pop_env w_23);
          assert_env_length w_23 1;
          push_env w_23 (Memo.from_int 1);
          assert_env_length w_23 2;
          return_n w_23 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (25)")
    25;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 1;
      push_env w_25 (Dynarray.get w_25.state.e 0);
      w_25.state.c <- pc_to_exp (int_to_pc 28))
    26;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 4;
      let last_12 = Source.E 3 in
      let x_12 = resolve w_27 last_12 in
      match Word.get_value (fst x_12) with
      | 5 (* tag_Const *) ->
          let splits_22 = Memo.splits (snd x_12) in
          let split0_22 = List.nth splits_22 0 in
          ignore (pop_env w_27);
          push_env w_27 split0_22;
          assert_env_length w_27 4;
          push_env w_27 (Dynarray.get w_27.state.e 2);
          assert_env_length w_27 5;
          drop_n w_27 5 1;
          assert_env_length w_27 4;
          drop_n w_27 4 2;
          assert_env_length w_27 2;
          return_n w_27 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_27);
          assert_env_length w_27 3;
          push_env w_27 (Dynarray.get w_27.state.e 0);
          assert_env_length w_27 4;
          drop_n w_27 4 2;
          assert_env_length w_27 2;
          return_n w_27 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (27)")
    27;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 2;
      let last_11 = Source.E 1 in
      let x_11 = resolve w_26 last_11 in
      match Word.get_value (fst x_11) with
      | 5 (* tag_Const *) ->
          let splits_20 = Memo.splits (snd x_11) in
          let split0_20 = List.nth splits_20 0 in
          ignore (pop_env w_26);
          push_env w_26 split0_20;
          assert_env_length w_26 2;
          push_env w_26 (Memo.from_int 1);
          assert_env_length w_26 3;
          let ctor_arg_5 = pop_env w_26 in
          push_env w_26 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_5 ]);
          assert_env_length w_26 3;
          drop_n w_26 3 1;
          assert_env_length w_26 2;
          return_n w_26 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_21 = Memo.splits (snd x_11) in
          let split0_21 = List.nth splits_21 0 in
          let split1_9 = List.nth splits_21 1 in
          ignore (pop_env w_26);
          push_env w_26 split0_21;
          push_env w_26 split1_9;
          assert_env_length w_26 3;
          push_env w_26 (Dynarray.get w_26.state.e 1);
          w_26.state.c <- pc_to_exp (int_to_pc 27)
      | _ ->
          ignore (pop_env w_26);
          assert_env_length w_26 1;
          push_env w_26 (Dynarray.get w_26.state.e 0);
          assert_env_length w_26 2;
          return_n w_26 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (28)")
    28;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 2;
      push_env w_28 (Dynarray.get w_28.state.e 0);
      assert_env_length w_28 3;
      push_env w_28 (Dynarray.get w_28.state.e 1);
      assert_env_length w_28 4;
      let keep_7 = env_call w_28 [ 0; 1 ] 2 in
      w_28.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_28.state.k ];
      w_28.state.c <- pc_to_exp (int_to_pc 6))
    29;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 2;
      push_env w_29 (Dynarray.get w_29.state.e 0);
      w_29.state.c <- pc_to_exp (int_to_pc 31))
    30;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 3;
      let last_13 = Source.E 2 in
      let x_13 = resolve w_30 last_13 in
      match Word.get_value (fst x_13) with
      | 8 (* tag_Mul *) ->
          let splits_23 = Memo.splits (snd x_13) in
          let split0_23 = List.nth splits_23 0 in
          let split1_10 = List.nth splits_23 1 in
          ignore (pop_env w_30);
          push_env w_30 split0_23;
          push_env w_30 split1_10;
          assert_env_length w_30 4;
          push_env w_30 (Dynarray.get w_30.state.e 2);
          assert_env_length w_30 5;
          push_env w_30 (Dynarray.get w_30.state.e 1);
          assert_env_length w_30 6;
          let keep_8 = env_call w_30 [ 0; 1; 2; 3 ] 2 in
          w_30.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_30.state.k ];
          w_30.state.c <- pc_to_exp (int_to_pc 29)
      | _ ->
          ignore (pop_env w_30);
          assert_env_length w_30 2;
          push_env w_30 (Dynarray.get w_30.state.e 0);
          assert_env_length w_30 3;
          push_env w_30 (Dynarray.get w_30.state.e 1);
          assert_env_length w_30 4;
          let ctor_arg_6 = pop_env w_30 in
          let ctor_arg_7 = pop_env w_30 in
          push_env w_30 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_7; ctor_arg_6 ]);
          assert_env_length w_30 3;
          return_n w_30 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (31)")
    31;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 2;
      push_env w_31 (Dynarray.get w_31.state.e 0);
      w_31.state.c <- pc_to_exp (int_to_pc 33))
    32;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 3;
      let last_14 = Source.E 2 in
      let x_14 = resolve w_32 last_14 in
      match Word.get_value (fst x_14) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_32);
          assert_env_length w_32 2;
          push_env w_32 (Dynarray.get w_32.state.e 1);
          assert_env_length w_32 3;
          return_n w_32 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_24 = Memo.splits (snd x_14) in
          let split0_24 = List.nth splits_24 0 in
          let split1_11 = List.nth splits_24 1 in
          ignore (pop_env w_32);
          push_env w_32 split0_24;
          push_env w_32 split1_11;
          assert_env_length w_32 4;
          push_env w_32 (Dynarray.get w_32.state.e 2);
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 6;
          push_env w_32 (Dynarray.get w_32.state.e 1);
          assert_env_length w_32 7;
          let keep_9 = env_call w_32 [ 4 ] 2 in
          w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_32.state.k ];
          w_32.state.c <- pc_to_exp (int_to_pc 32)
      | _ -> failwith "unreachable (33)")
    33;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 2;
      push_env w_33 (Dynarray.get w_33.state.e 1);
      w_33.state.c <- pc_to_exp (int_to_pc 35))
    34;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 3;
      let last_15 = Source.E 2 in
      let x_15 = resolve w_34 last_15 in
      match Word.get_value (fst x_15) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_34);
          assert_env_length w_34 2;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 3;
          push_env w_34 (Memo.from_constructor tag_ENil);
          assert_env_length w_34 4;
          let ctor_arg_8 = pop_env w_34 in
          let ctor_arg_9 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_9; ctor_arg_8 ]);
          assert_env_length w_34 3;
          return_n w_34 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_25 = Memo.splits (snd x_15) in
          let split0_25 = List.nth splits_25 0 in
          let split1_12 = List.nth splits_25 1 in
          ignore (pop_env w_34);
          push_env w_34 split0_25;
          push_env w_34 split1_12;
          assert_env_length w_34 4;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 5;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          assert_env_length w_34 6;
          let keep_10 = env_call w_34 [ 0; 1; 2; 3 ] 2 in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (35)")
    35;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 1;
      push_env w_35 (Dynarray.get w_35.state.e 0);
      w_35.state.c <- pc_to_exp (int_to_pc 37))
    36;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 2;
      let last_16 = Source.E 1 in
      let x_16 = resolve w_36 last_16 in
      match Word.get_value (fst x_16) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_36);
          assert_env_length w_36 1;
          push_env w_36 (Memo.from_constructor tag_ENil);
          assert_env_length w_36 2;
          return_n w_36 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_26 = Memo.splits (snd x_16) in
          let split0_26 = List.nth splits_26 0 in
          let split1_13 = List.nth splits_26 1 in
          ignore (pop_env w_36);
          push_env w_36 split0_26;
          push_env w_36 split1_13;
          assert_env_length w_36 3;
          push_env w_36 (Dynarray.get w_36.state.e 1);
          assert_env_length w_36 4;
          push_env w_36 (Dynarray.get w_36.state.e 2);
          assert_env_length w_36 5;
          let keep_11 = env_call w_36 [ 3 ] 1 in
          w_36.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_36.state.k ];
          w_36.state.c <- pc_to_exp (int_to_pc 36)
      | _ -> failwith "unreachable (37)")
    37;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 2;
      push_env w_37 (Dynarray.get w_37.state.e 0);
      assert_env_length w_37 3;
      let keep_12 = env_call w_37 [ 0; 1 ] 1 in
      w_37.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_37.state.k ];
      w_37.state.c <- pc_to_exp (int_to_pc 26))
    38;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 2;
      push_env w_38 (Dynarray.get w_38.state.e 1);
      w_38.state.c <- pc_to_exp (int_to_pc 40))
    39;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 3;
      let last_17 = Source.E 2 in
      let x_17 = resolve w_39 last_17 in
      match Word.get_value (fst x_17) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_39);
          assert_env_length w_39 2;
          push_env w_39 (Dynarray.get w_39.state.e 0);
          assert_env_length w_39 3;
          push_env w_39 (Memo.from_constructor tag_ENil);
          assert_env_length w_39 4;
          let ctor_arg_10 = pop_env w_39 in
          let ctor_arg_11 = pop_env w_39 in
          push_env w_39 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_11; ctor_arg_10 ]);
          assert_env_length w_39 3;
          return_n w_39 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_27 = Memo.splits (snd x_17) in
          let split0_27 = List.nth splits_27 0 in
          let split1_14 = List.nth splits_27 1 in
          ignore (pop_env w_39);
          push_env w_39 split0_27;
          push_env w_39 split1_14;
          assert_env_length w_39 4;
          push_env w_39 (Dynarray.get w_39.state.e 0);
          assert_env_length w_39 5;
          push_env w_39 (Dynarray.get w_39.state.e 2);
          assert_env_length w_39 6;
          let keep_13 = env_call w_39 [ 0; 1; 2; 3 ] 2 in
          w_39.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_39.state.k ];
          w_39.state.c <- pc_to_exp (int_to_pc 38)
      | _ -> failwith "unreachable (40)")
    40;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 1;
      push_env w_40 (Dynarray.get w_40.state.e 0);
      w_40.state.c <- pc_to_exp (int_to_pc 42))
    41;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 2;
      let last_18 = Source.E 1 in
      let x_18 = resolve w_41 last_18 in
      match Word.get_value (fst x_18) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_41);
          assert_env_length w_41 1;
          push_env w_41 (Memo.from_constructor tag_ENil);
          assert_env_length w_41 2;
          return_n w_41 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_28 = Memo.splits (snd x_18) in
          let split0_28 = List.nth splits_28 0 in
          let split1_15 = List.nth splits_28 1 in
          ignore (pop_env w_41);
          push_env w_41 split0_28;
          push_env w_41 split1_15;
          assert_env_length w_41 3;
          push_env w_41 (Dynarray.get w_41.state.e 1);
          assert_env_length w_41 4;
          push_env w_41 (Dynarray.get w_41.state.e 2);
          assert_env_length w_41 5;
          let keep_14 = env_call w_41 [ 3 ] 1 in
          w_41.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_41.state.k ];
          w_41.state.c <- pc_to_exp (int_to_pc 41)
      | _ -> failwith "unreachable (42)")
    42;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 2;
      push_env w_42 (Dynarray.get w_42.state.e 0);
      w_42.state.c <- pc_to_exp (int_to_pc 44))
    43;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 3;
      let last_19 = Source.E 2 in
      let x_19 = resolve w_43 last_19 in
      match Word.get_value (fst x_19) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_43);
          assert_env_length w_43 2;
          push_env w_43 (Dynarray.get w_43.state.e 1);
          assert_env_length w_43 3;
          return_n w_43 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_29 = Memo.splits (snd x_19) in
          let split0_29 = List.nth splits_29 0 in
          let split1_16 = List.nth splits_29 1 in
          ignore (pop_env w_43);
          push_env w_43 split0_29;
          push_env w_43 split1_16;
          assert_env_length w_43 4;
          push_env w_43 (Dynarray.get w_43.state.e 3);
          assert_env_length w_43 5;
          push_env w_43 (Dynarray.get w_43.state.e 2);
          assert_env_length w_43 6;
          push_env w_43 (Dynarray.get w_43.state.e 1);
          assert_env_length w_43 7;
          let ctor_arg_12 = pop_env w_43 in
          let ctor_arg_13 = pop_env w_43 in
          push_env w_43 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_13; ctor_arg_12 ]);
          assert_env_length w_43 6;
          ignore (env_call w_43 [] 2);
          w_43.state.c <- pc_to_exp (int_to_pc 43)
      | _ -> failwith "unreachable (44)")
    44;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 1;
      push_env w_44 (Dynarray.get w_44.state.e 0);
      assert_env_length w_44 2;
      push_env w_44 (Memo.from_constructor tag_ENil);
      assert_env_length w_44 3;
      ignore (env_call w_44 [] 2);
      w_44.state.c <- pc_to_exp (int_to_pc 43))
    45;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 1;
      push_env w_45 (Dynarray.get w_45.state.e 0);
      w_45.state.c <- pc_to_exp (int_to_pc 49))
    46;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 3;
      let cond_2 = resolve w_48 (Source.E 2) in
      ignore (pop_env w_48);
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_48 2;
        push_env w_48 (Memo.from_constructor tag_ENil);
        assert_env_length w_48 3;
        drop_n w_48 3 1;
        assert_env_length w_48 2;
        return_n w_48 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_48 2;
        push_env w_48 (Dynarray.get w_48.state.e 0);
        assert_env_length w_48 3;
        push_env w_48 (Memo.from_constructor tag_ENil);
        assert_env_length w_48 4;
        let ctor_arg_14 = pop_env w_48 in
        let ctor_arg_15 = pop_env w_48 in
        push_env w_48 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_15; ctor_arg_14 ]);
        assert_env_length w_48 3;
        drop_n w_48 3 1;
        assert_env_length w_48 2;
        return_n w_48 2 (pc_to_exp (int_to_pc 0))))
    47;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 4;
      let x0_4 = resolve w_47 (Source.E 2) in
      let x1_4 = resolve w_47 (Source.E 3) in
      ignore (pop_env w_47);
      ignore (pop_env w_47);
      push_env w_47 (Memo.from_int (if Word.get_value (fst x0_4) = Word.get_value (fst x1_4) then 1 else 0));
      w_47.state.c <- pc_to_exp (int_to_pc 47))
    48;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 2;
      let last_20 = Source.E 1 in
      let x_20 = resolve w_46 last_20 in
      match Word.get_value (fst x_20) with
      | 7 (* tag_Add *) ->
          let splits_30 = Memo.splits (snd x_20) in
          let split0_30 = List.nth splits_30 0 in
          let split1_17 = List.nth splits_30 1 in
          ignore (pop_env w_46);
          push_env w_46 split0_30;
          push_env w_46 split1_17;
          assert_env_length w_46 3;
          push_env w_46 (Dynarray.get w_46.state.e 1);
          assert_env_length w_46 4;
          let keep_15 = env_call w_46 [ 2 ] 1 in
          w_46.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_46.state.k ];
          w_46.state.c <- pc_to_exp (int_to_pc 46)
      | 5 (* tag_Const *) ->
          let splits_31 = Memo.splits (snd x_20) in
          let split0_31 = List.nth splits_31 0 in
          ignore (pop_env w_46);
          push_env w_46 split0_31;
          assert_env_length w_46 2;
          push_env w_46 (Dynarray.get w_46.state.e 1);
          assert_env_length w_46 3;
          push_env w_46 (Memo.from_int 0);
          w_46.state.c <- pc_to_exp (int_to_pc 48)
      | _ ->
          ignore (pop_env w_46);
          assert_env_length w_46 1;
          push_env w_46 (Dynarray.get w_46.state.e 0);
          assert_env_length w_46 2;
          push_env w_46 (Memo.from_constructor tag_ENil);
          assert_env_length w_46 3;
          let ctor_arg_16 = pop_env w_46 in
          let ctor_arg_17 = pop_env w_46 in
          push_env w_46 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_17; ctor_arg_16 ]);
          assert_env_length w_46 2;
          return_n w_46 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (49)")
    49;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 1;
      push_env w_49 (Dynarray.get w_49.state.e 0);
      w_49.state.c <- pc_to_exp (int_to_pc 51))
    50;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 2;
      let last_21 = Source.E 1 in
      let x_21 = resolve w_50 last_21 in
      match Word.get_value (fst x_21) with
      | 8 (* tag_Mul *) ->
          let splits_32 = Memo.splits (snd x_21) in
          let split0_32 = List.nth splits_32 0 in
          let split1_18 = List.nth splits_32 1 in
          ignore (pop_env w_50);
          push_env w_50 split0_32;
          push_env w_50 split1_18;
          assert_env_length w_50 3;
          push_env w_50 (Dynarray.get w_50.state.e 1);
          assert_env_length w_50 4;
          let keep_16 = env_call w_50 [ 2 ] 1 in
          w_50.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_50.state.k ];
          w_50.state.c <- pc_to_exp (int_to_pc 50)
      | _ ->
          ignore (pop_env w_50);
          assert_env_length w_50 1;
          push_env w_50 (Dynarray.get w_50.state.e 0);
          assert_env_length w_50 2;
          push_env w_50 (Memo.from_constructor tag_ENil);
          assert_env_length w_50 3;
          let ctor_arg_18 = pop_env w_50 in
          let ctor_arg_19 = pop_env w_50 in
          push_env w_50 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_19; ctor_arg_18 ]);
          assert_env_length w_50 2;
          return_n w_50 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (51)")
    51;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 1;
      push_env w_51 (Dynarray.get w_51.state.e 0);
      w_51.state.c <- pc_to_exp (int_to_pc 54))
    52;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 4;
      let last_23 = Source.E 3 in
      let x_23 = resolve w_53 last_23 in
      match Word.get_value (fst x_23) with
      | 5 (* tag_Const *) ->
          let splits_35 = Memo.splits (snd x_23) in
          let split0_35 = List.nth splits_35 0 in
          ignore (pop_env w_53);
          push_env w_53 split0_35;
          assert_env_length w_53 4;
          push_env w_53 (Dynarray.get w_53.state.e 3);
          assert_env_length w_53 5;
          drop_n w_53 5 1;
          assert_env_length w_53 4;
          drop_n w_53 4 2;
          assert_env_length w_53 2;
          return_n w_53 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_53);
          assert_env_length w_53 3;
          push_env w_53 (Memo.from_int 1);
          assert_env_length w_53 4;
          drop_n w_53 4 2;
          assert_env_length w_53 2;
          return_n w_53 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (53)")
    53;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 2;
      let last_22 = Source.E 1 in
      let x_22 = resolve w_52 last_22 in
      match Word.get_value (fst x_22) with
      | 5 (* tag_Const *) ->
          let splits_33 = Memo.splits (snd x_22) in
          let split0_33 = List.nth splits_33 0 in
          ignore (pop_env w_52);
          push_env w_52 split0_33;
          assert_env_length w_52 2;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          assert_env_length w_52 3;
          drop_n w_52 3 1;
          assert_env_length w_52 2;
          return_n w_52 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_34 = Memo.splits (snd x_22) in
          let split0_34 = List.nth splits_34 0 in
          let split1_19 = List.nth splits_34 1 in
          ignore (pop_env w_52);
          push_env w_52 split0_34;
          push_env w_52 split1_19;
          assert_env_length w_52 3;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          w_52.state.c <- pc_to_exp (int_to_pc 53)
      | _ ->
          ignore (pop_env w_52);
          assert_env_length w_52 1;
          push_env w_52 (Memo.from_int 1);
          assert_env_length w_52 2;
          return_n w_52 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (54)")
    54;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 1;
      push_env w_54 (Dynarray.get w_54.state.e 0);
      w_54.state.c <- pc_to_exp (int_to_pc 57))
    55;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 4;
      let last_25 = Source.E 3 in
      let x_25 = resolve w_56 last_25 in
      match Word.get_value (fst x_25) with
      | 5 (* tag_Const *) ->
          let splits_38 = Memo.splits (snd x_25) in
          let split0_38 = List.nth splits_38 0 in
          ignore (pop_env w_56);
          push_env w_56 split0_38;
          assert_env_length w_56 4;
          push_env w_56 (Dynarray.get w_56.state.e 2);
          assert_env_length w_56 5;
          drop_n w_56 5 1;
          assert_env_length w_56 4;
          drop_n w_56 4 2;
          assert_env_length w_56 2;
          return_n w_56 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_56);
          assert_env_length w_56 3;
          push_env w_56 (Dynarray.get w_56.state.e 0);
          assert_env_length w_56 4;
          drop_n w_56 4 2;
          assert_env_length w_56 2;
          return_n w_56 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (56)")
    56;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 2;
      let last_24 = Source.E 1 in
      let x_24 = resolve w_55 last_24 in
      match Word.get_value (fst x_24) with
      | 5 (* tag_Const *) ->
          let splits_36 = Memo.splits (snd x_24) in
          let split0_36 = List.nth splits_36 0 in
          ignore (pop_env w_55);
          push_env w_55 split0_36;
          assert_env_length w_55 2;
          push_env w_55 (Memo.from_int 1);
          assert_env_length w_55 3;
          let ctor_arg_20 = pop_env w_55 in
          push_env w_55 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_20 ]);
          assert_env_length w_55 3;
          drop_n w_55 3 1;
          assert_env_length w_55 2;
          return_n w_55 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_37 = Memo.splits (snd x_24) in
          let split0_37 = List.nth splits_37 0 in
          let split1_20 = List.nth splits_37 1 in
          ignore (pop_env w_55);
          push_env w_55 split0_37;
          push_env w_55 split1_20;
          assert_env_length w_55 3;
          push_env w_55 (Dynarray.get w_55.state.e 1);
          w_55.state.c <- pc_to_exp (int_to_pc 56)
      | _ ->
          ignore (pop_env w_55);
          assert_env_length w_55 1;
          push_env w_55 (Dynarray.get w_55.state.e 0);
          assert_env_length w_55 2;
          return_n w_55 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (57)")
    57;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 1;
      push_env w_57 (Dynarray.get w_57.state.e 0);
      w_57.state.c <- pc_to_exp (int_to_pc 59))
    58;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 2;
      let last_26 = Source.E 1 in
      let x_26 = resolve w_58 last_26 in
      match Word.get_value (fst x_26) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_58);
          assert_env_length w_58 1;
          push_env w_58 (Memo.from_int 1);
          assert_env_length w_58 2;
          return_n w_58 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_39 = Memo.splits (snd x_26) in
          let split0_39 = List.nth splits_39 0 in
          let split1_21 = List.nth splits_39 1 in
          ignore (pop_env w_58);
          push_env w_58 split0_39;
          push_env w_58 split1_21;
          assert_env_length w_58 3;
          push_env w_58 (Dynarray.get w_58.state.e 1);
          assert_env_length w_58 4;
          let keep_17 = env_call w_58 [ 2 ] 1 in
          w_58.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_58.state.k ];
          w_58.state.c <- pc_to_exp (int_to_pc 52)
      | _ -> failwith "unreachable (59)")
    59;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 1;
      push_env w_59 (Dynarray.get w_59.state.e 0);
      w_59.state.c <- pc_to_exp (int_to_pc 61))
    60;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 2;
      let last_27 = Source.E 1 in
      let x_27 = resolve w_60 last_27 in
      match Word.get_value (fst x_27) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_60);
          assert_env_length w_60 1;
          push_env w_60 (Memo.from_constructor tag_ENil);
          assert_env_length w_60 2;
          return_n w_60 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_40 = Memo.splits (snd x_27) in
          let split0_40 = List.nth splits_40 0 in
          let split1_22 = List.nth splits_40 1 in
          ignore (pop_env w_60);
          push_env w_60 split0_40;
          push_env w_60 split1_22;
          assert_env_length w_60 3;
          push_env w_60 (Dynarray.get w_60.state.e 1);
          assert_env_length w_60 4;
          let keep_18 = env_call w_60 [ 2 ] 1 in
          w_60.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_60.state.k ];
          w_60.state.c <- pc_to_exp (int_to_pc 55)
      | _ -> failwith "unreachable (61)")
    61;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 1;
      push_env w_61 (Dynarray.get w_61.state.e 0);
      w_61.state.c <- pc_to_exp (int_to_pc 64))
    62;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 4;
      let last_29 = Source.E 3 in
      let x_29 = resolve w_63 last_29 in
      match Word.get_value (fst x_29) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_63);
          assert_env_length w_63 3;
          push_env w_63 (Dynarray.get w_63.state.e 1);
          assert_env_length w_63 4;
          drop_n w_63 4 2;
          assert_env_length w_63 2;
          return_n w_63 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_63);
          assert_env_length w_63 3;
          push_env w_63 (Dynarray.get w_63.state.e 1);
          assert_env_length w_63 4;
          push_env w_63 (Dynarray.get w_63.state.e 2);
          assert_env_length w_63 5;
          let keep_19 = env_call w_63 [ 3 ] 1 in
          w_63.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_19; w_63.state.k ];
          w_63.state.c <- pc_to_exp (int_to_pc 62)
      | _ -> failwith "unreachable (63)")
    63;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 2;
      let last_28 = Source.E 1 in
      let x_28 = resolve w_62 last_28 in
      match Word.get_value (fst x_28) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_62);
          assert_env_length w_62 1;
          push_env w_62 (Memo.from_int 1);
          assert_env_length w_62 2;
          let ctor_arg_21 = pop_env w_62 in
          push_env w_62 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_21 ]);
          assert_env_length w_62 2;
          return_n w_62 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_41 = Memo.splits (snd x_28) in
          let split0_41 = List.nth splits_41 0 in
          let split1_23 = List.nth splits_41 1 in
          ignore (pop_env w_62);
          push_env w_62 split0_41;
          push_env w_62 split1_23;
          assert_env_length w_62 3;
          push_env w_62 (Dynarray.get w_62.state.e 2);
          w_62.state.c <- pc_to_exp (int_to_pc 63)
      | _ -> failwith "unreachable (64)")
    64;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 2;
      push_env w_64 (Dynarray.get w_64.state.e 0);
      assert_env_length w_64 3;
      let keep_20 = env_call w_64 [ 1 ] 1 in
      w_64.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_20; w_64.state.k ];
      w_64.state.c <- pc_to_exp (int_to_pc 50))
    65;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 3;
      push_env w_65 (Dynarray.get w_65.state.e 2);
      w_65.state.c <- pc_to_exp (int_to_pc 69))
    66;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 4;
      let cond_3 = resolve w_68 (Source.E 3) in
      ignore (pop_env w_68);
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_68 3;
        push_env w_68 (Memo.from_constructor tag_ENil);
        assert_env_length w_68 4;
        return_n w_68 4 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_68 3;
        push_env w_68 (Dynarray.get w_68.state.e 1);
        assert_env_length w_68 4;
        push_env w_68 (Dynarray.get w_68.state.e 0);
        assert_env_length w_68 5;
        let keep_21 = env_call w_68 [] 2 in
        w_68.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_21; w_68.state.k ];
        w_68.state.c <- pc_to_exp (int_to_pc 16)))
    67;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 5;
      let x0_5 = resolve w_67 (Source.E 3) in
      let x1_5 = resolve w_67 (Source.E 4) in
      ignore (pop_env w_67);
      ignore (pop_env w_67);
      push_env w_67 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      w_67.state.c <- pc_to_exp (int_to_pc 67))
    68;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 4;
      let last_30 = Source.E 3 in
      let x_30 = resolve w_66 last_30 in
      match Word.get_value (fst x_30) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_66);
          assert_env_length w_66 3;
          push_env w_66 (Dynarray.get w_66.state.e 1);
          assert_env_length w_66 4;
          push_env w_66 (Memo.from_int 0);
          w_66.state.c <- pc_to_exp (int_to_pc 68)
      | 12 (* tag_ECons *) ->
          let splits_42 = Memo.splits (snd x_30) in
          let split0_42 = List.nth splits_42 0 in
          let split1_24 = List.nth splits_42 1 in
          ignore (pop_env w_66);
          push_env w_66 split0_42;
          push_env w_66 split1_24;
          assert_env_length w_66 5;
          push_env w_66 (Dynarray.get w_66.state.e 3);
          assert_env_length w_66 6;
          let keep_22 = env_call w_66 [ 0; 1; 3; 4 ] 1 in
          w_66.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_22; w_66.state.k ];
          w_66.state.c <- pc_to_exp (int_to_pc 26)
      | _ -> failwith "unreachable (69)")
    69;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 1;
      push_env w_69 (Dynarray.get w_69.state.e 0);
      w_69.state.c <- pc_to_exp (int_to_pc 71))
    70;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 2;
      let last_31 = Source.E 1 in
      let x_31 = resolve w_70 last_31 in
      match Word.get_value (fst x_31) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_70);
          assert_env_length w_70 1;
          push_env w_70 (Memo.from_constructor tag_ENil);
          assert_env_length w_70 2;
          return_n w_70 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_43 = Memo.splits (snd x_31) in
          let split0_43 = List.nth splits_43 0 in
          let split1_25 = List.nth splits_43 1 in
          ignore (pop_env w_70);
          push_env w_70 split0_43;
          push_env w_70 split1_25;
          assert_env_length w_70 3;
          push_env w_70 (Dynarray.get w_70.state.e 1);
          assert_env_length w_70 4;
          let keep_23 = env_call w_70 [ 1; 2 ] 1 in
          w_70.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_23; w_70.state.k ];
          w_70.state.c <- pc_to_exp (int_to_pc 26)
      | _ -> failwith "unreachable (71)")
    71;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 1;
      push_env w_71 (Dynarray.get w_71.state.e 0);
      w_71.state.c <- pc_to_exp (int_to_pc 74))
    72;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 4;
      let last_33 = Source.E 3 in
      let x_33 = resolve w_73 last_33 in
      match Word.get_value (fst x_33) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_73);
          assert_env_length w_73 3;
          push_env w_73 (Dynarray.get w_73.state.e 1);
          assert_env_length w_73 4;
          push_env w_73 (Memo.from_constructor tag_ENil);
          assert_env_length w_73 5;
          let ctor_arg_22 = pop_env w_73 in
          let ctor_arg_23 = pop_env w_73 in
          push_env w_73 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_23; ctor_arg_22 ]);
          assert_env_length w_73 4;
          drop_n w_73 4 2;
          assert_env_length w_73 2;
          return_n w_73 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_45 = Memo.splits (snd x_33) in
          let split0_45 = List.nth splits_45 0 in
          let split1_27 = List.nth splits_45 1 in
          ignore (pop_env w_73);
          push_env w_73 split0_45;
          push_env w_73 split1_27;
          assert_env_length w_73 5;
          push_env w_73 (Dynarray.get w_73.state.e 1);
          assert_env_length w_73 6;
          push_env w_73 (Dynarray.get w_73.state.e 3);
          assert_env_length w_73 7;
          let keep_24 = env_call w_73 [ 1; 2; 3; 4 ] 2 in
          w_73.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_24; w_73.state.k ];
          w_73.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (73)")
    73;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 2;
      let last_32 = Source.E 1 in
      let x_32 = resolve w_72 last_32 in
      match Word.get_value (fst x_32) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_72);
          assert_env_length w_72 1;
          push_env w_72 (Memo.from_constructor tag_ENil);
          assert_env_length w_72 2;
          return_n w_72 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_44 = Memo.splits (snd x_32) in
          let split0_44 = List.nth splits_44 0 in
          let split1_26 = List.nth splits_44 1 in
          ignore (pop_env w_72);
          push_env w_72 split0_44;
          push_env w_72 split1_26;
          assert_env_length w_72 3;
          push_env w_72 (Dynarray.get w_72.state.e 2);
          w_72.state.c <- pc_to_exp (int_to_pc 73)
      | _ -> failwith "unreachable (74)")
    74;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 2;
      push_env w_74 (Dynarray.get w_74.state.e 1);
      w_74.state.c <- pc_to_exp (int_to_pc 76))
    75;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 3;
      let last_34 = Source.E 2 in
      let x_34 = resolve w_75 last_34 in
      match Word.get_value (fst x_34) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_75);
          assert_env_length w_75 2;
          push_env w_75 (Memo.from_constructor tag_NoPick);
          assert_env_length w_75 3;
          return_n w_75 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_46 = Memo.splits (snd x_34) in
          let split0_46 = List.nth splits_46 0 in
          let split1_28 = List.nth splits_46 1 in
          ignore (pop_env w_75);
          push_env w_75 split0_46;
          push_env w_75 split1_28;
          assert_env_length w_75 4;
          push_env w_75 (Dynarray.get w_75.state.e 0);
          assert_env_length w_75 5;
          push_env w_75 (Dynarray.get w_75.state.e 2);
          assert_env_length w_75 6;
          let keep_25 = env_call w_75 [ 0; 2; 3 ] 2 in
          w_75.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_25; w_75.state.k ];
          w_75.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (76)")
    76;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 1;
      push_env w_76 (Dynarray.get w_76.state.e 0);
      w_76.state.c <- pc_to_exp (int_to_pc 78))
    77;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 2;
      let last_35 = Source.E 1 in
      let x_35 = resolve w_77 last_35 in
      match Word.get_value (fst x_35) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_77);
          assert_env_length w_77 1;
          push_env w_77 (Memo.from_constructor tag_ENil);
          assert_env_length w_77 2;
          return_n w_77 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_47 = Memo.splits (snd x_35) in
          let split0_47 = List.nth splits_47 0 in
          let split1_29 = List.nth splits_47 1 in
          ignore (pop_env w_77);
          push_env w_77 split0_47;
          push_env w_77 split1_29;
          assert_env_length w_77 3;
          push_env w_77 (Dynarray.get w_77.state.e 1);
          assert_env_length w_77 4;
          push_env w_77 (Dynarray.get w_77.state.e 2);
          assert_env_length w_77 5;
          let keep_26 = env_call w_77 [ 1; 2 ] 2 in
          w_77.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_26; w_77.state.k ];
          w_77.state.c <- pc_to_exp (int_to_pc 75)
      | _ -> failwith "unreachable (78)")
    78;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 1;
      push_env w_78 (Dynarray.get w_78.state.e 0);
      w_78.state.c <- pc_to_exp (int_to_pc 81))
    79;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 4;
      let last_37 = Source.E 3 in
      let x_37 = resolve w_80 last_37 in
      match Word.get_value (fst x_37) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_80);
          assert_env_length w_80 3;
          push_env w_80 (Dynarray.get w_80.state.e 1);
          assert_env_length w_80 4;
          drop_n w_80 4 2;
          assert_env_length w_80 2;
          return_n w_80 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_80);
          assert_env_length w_80 3;
          push_env w_80 (Dynarray.get w_80.state.e 1);
          assert_env_length w_80 4;
          push_env w_80 (Dynarray.get w_80.state.e 2);
          assert_env_length w_80 5;
          let keep_27 = env_call w_80 [ 3 ] 1 in
          w_80.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_27; w_80.state.k ];
          w_80.state.c <- pc_to_exp (int_to_pc 79)
      | _ -> failwith "unreachable (80)")
    80;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 2;
      let last_36 = Source.E 1 in
      let x_36 = resolve w_79 last_36 in
      match Word.get_value (fst x_36) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_79);
          assert_env_length w_79 1;
          push_env w_79 (Memo.from_int 0);
          assert_env_length w_79 2;
          let ctor_arg_24 = pop_env w_79 in
          push_env w_79 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_24 ]);
          assert_env_length w_79 2;
          return_n w_79 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_48 = Memo.splits (snd x_36) in
          let split0_48 = List.nth splits_48 0 in
          let split1_30 = List.nth splits_48 1 in
          ignore (pop_env w_79);
          push_env w_79 split0_48;
          push_env w_79 split1_30;
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 2);
          w_79.state.c <- pc_to_exp (int_to_pc 80)
      | _ -> failwith "unreachable (81)")
    81;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 1;
      push_env w_81 (Dynarray.get w_81.state.e 0);
      assert_env_length w_81 2;
      let keep_28 = env_call w_81 [] 1 in
      w_81.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_28; w_81.state.k ];
      w_81.state.c <- pc_to_exp (int_to_pc 72))
    82;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 2;
      push_env w_82 (Dynarray.get w_82.state.e 0);
      assert_env_length w_82 3;
      let keep_29 = env_call w_82 [ 1 ] 1 in
      w_82.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_29; w_82.state.k ];
      w_82.state.c <- pc_to_exp (int_to_pc 46))
    83;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 2;
      push_env w_83 (Dynarray.get w_83.state.e 0);
      w_83.state.c <- pc_to_exp (int_to_pc 86))
    84;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 4;
      let last_39 = Source.E 3 in
      let x_39 = resolve w_85 last_39 in
      match Word.get_value (fst x_39) with
      | 7 (* tag_Add *) ->
          let splits_50 = Memo.splits (snd x_39) in
          let split0_50 = List.nth splits_50 0 in
          let split1_31 = List.nth splits_50 1 in
          ignore (pop_env w_85);
          push_env w_85 split0_50;
          push_env w_85 split1_31;
          assert_env_length w_85 5;
          push_env w_85 (Dynarray.get w_85.state.e 2);
          assert_env_length w_85 6;
          push_env w_85 (Dynarray.get w_85.state.e 3);
          assert_env_length w_85 7;
          let keep_30 = env_call w_85 [ 2; 4 ] 2 in
          w_85.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_30; w_85.state.k ];
          w_85.state.c <- pc_to_exp (int_to_pc 84)
      | 8 (* tag_Mul *) ->
          let splits_51 = Memo.splits (snd x_39) in
          let split0_51 = List.nth splits_51 0 in
          let split1_32 = List.nth splits_51 1 in
          ignore (pop_env w_85);
          push_env w_85 split0_51;
          push_env w_85 split1_32;
          assert_env_length w_85 5;
          push_env w_85 (Dynarray.get w_85.state.e 2);
          assert_env_length w_85 6;
          push_env w_85 (Dynarray.get w_85.state.e 3);
          assert_env_length w_85 7;
          let keep_31 = env_call w_85 [ 2; 4 ] 2 in
          w_85.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_31; w_85.state.k ];
          w_85.state.c <- pc_to_exp (int_to_pc 84)
      | _ ->
          ignore (pop_env w_85);
          assert_env_length w_85 3;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          assert_env_length w_85 4;
          drop_n w_85 4 1;
          assert_env_length w_85 3;
          return_n w_85 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (85)")
    85;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 3;
      let last_38 = Source.E 2 in
      let x_38 = resolve w_84 last_38 in
      match Word.get_value (fst x_38) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_84);
          assert_env_length w_84 2;
          push_env w_84 (Dynarray.get w_84.state.e 1);
          assert_env_length w_84 3;
          return_n w_84 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_49 = Memo.splits (snd x_38) in
          let split0_49 = List.nth splits_49 0 in
          ignore (pop_env w_84);
          push_env w_84 split0_49;
          assert_env_length w_84 3;
          push_env w_84 (Dynarray.get w_84.state.e 1);
          w_84.state.c <- pc_to_exp (int_to_pc 85)
      | _ -> failwith "unreachable (86)")
    86;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 1;
      push_env w_86 (Dynarray.get w_86.state.e 0);
      w_86.state.c <- pc_to_exp (int_to_pc 88))
    87;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 2;
      let last_40 = Source.E 1 in
      let x_40 = resolve w_87 last_40 in
      match Word.get_value (fst x_40) with
      | 5 (* tag_Const *) ->
          let splits_52 = Memo.splits (snd x_40) in
          let split0_52 = List.nth splits_52 0 in
          ignore (pop_env w_87);
          push_env w_87 split0_52;
          assert_env_length w_87 2;
          push_env w_87 (Dynarray.get w_87.state.e 0);
          assert_env_length w_87 3;
          drop_n w_87 3 1;
          assert_env_length w_87 2;
          return_n w_87 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_53 = Memo.splits (snd x_40) in
          let split0_53 = List.nth splits_53 0 in
          ignore (pop_env w_87);
          push_env w_87 split0_53;
          assert_env_length w_87 2;
          push_env w_87 (Dynarray.get w_87.state.e 0);
          assert_env_length w_87 3;
          drop_n w_87 3 1;
          assert_env_length w_87 2;
          return_n w_87 2 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_54 = Memo.splits (snd x_40) in
          let split0_54 = List.nth splits_54 0 in
          let split1_33 = List.nth splits_54 1 in
          ignore (pop_env w_87);
          push_env w_87 split0_54;
          push_env w_87 split1_33;
          assert_env_length w_87 3;
          push_env w_87 (Dynarray.get w_87.state.e 1);
          assert_env_length w_87 4;
          let keep_32 = env_call w_87 [ 2 ] 1 in
          w_87.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_32; w_87.state.k ];
          w_87.state.c <- pc_to_exp (int_to_pc 87)
      | 8 (* tag_Mul *) ->
          let splits_55 = Memo.splits (snd x_40) in
          let split0_55 = List.nth splits_55 0 in
          let split1_34 = List.nth splits_55 1 in
          ignore (pop_env w_87);
          push_env w_87 split0_55;
          push_env w_87 split1_34;
          assert_env_length w_87 3;
          push_env w_87 (Dynarray.get w_87.state.e 1);
          assert_env_length w_87 4;
          let keep_33 = env_call w_87 [ 2 ] 1 in
          w_87.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_33; w_87.state.k ];
          w_87.state.c <- pc_to_exp (int_to_pc 87)
      | _ -> failwith "unreachable (88)")
    88;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 1;
      push_env w_88 (Dynarray.get w_88.state.e 0);
      assert_env_length w_88 2;
      let keep_34 = env_call w_88 [ 0 ] 1 in
      w_88.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_34; w_88.state.k ];
      w_88.state.c <- pc_to_exp (int_to_pc 87))
    89;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 1;
      push_env w_89 (Dynarray.get w_89.state.e 0);
      w_89.state.c <- pc_to_exp (int_to_pc 92))
    90;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 3;
      let last_42 = Source.E 2 in
      let x_42 = resolve w_91 last_42 in
      match Word.get_value (fst x_42) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_91);
          assert_env_length w_91 2;
          push_env w_91 (Memo.from_int 1);
          assert_env_length w_91 3;
          let ctor_arg_26 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_26 ]);
          assert_env_length w_91 3;
          drop_n w_91 3 1;
          assert_env_length w_91 2;
          return_n w_91 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_91);
          assert_env_length w_91 2;
          push_env w_91 (Memo.from_int 0);
          assert_env_length w_91 3;
          let ctor_arg_27 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_27 ]);
          assert_env_length w_91 3;
          drop_n w_91 3 1;
          assert_env_length w_91 2;
          return_n w_91 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (91)")
    91;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 2;
      let last_41 = Source.E 1 in
      let x_41 = resolve w_90 last_41 in
      match Word.get_value (fst x_41) with
      | 5 (* tag_Const *) ->
          let splits_56 = Memo.splits (snd x_41) in
          let split0_56 = List.nth splits_56 0 in
          ignore (pop_env w_90);
          push_env w_90 split0_56;
          assert_env_length w_90 2;
          push_env w_90 (Memo.from_int 0);
          assert_env_length w_90 3;
          let ctor_arg_25 = pop_env w_90 in
          push_env w_90 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_25 ]);
          assert_env_length w_90 3;
          drop_n w_90 3 1;
          assert_env_length w_90 2;
          return_n w_90 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_57 = Memo.splits (snd x_41) in
          let split0_57 = List.nth splits_57 0 in
          ignore (pop_env w_90);
          push_env w_90 split0_57;
          assert_env_length w_90 2;
          push_env w_90 (Dynarray.get w_90.state.e 1);
          w_90.state.c <- pc_to_exp (int_to_pc 91)
      | 7 (* tag_Add *) ->
          let splits_58 = Memo.splits (snd x_41) in
          let split0_58 = List.nth splits_58 0 in
          let split1_35 = List.nth splits_58 1 in
          ignore (pop_env w_90);
          push_env w_90 split0_58;
          push_env w_90 split1_35;
          assert_env_length w_90 3;
          push_env w_90 (Dynarray.get w_90.state.e 1);
          assert_env_length w_90 4;
          let keep_35 = env_call w_90 [ 2 ] 1 in
          w_90.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_35; w_90.state.k ];
          w_90.state.c <- pc_to_exp (int_to_pc 90)
      | 8 (* tag_Mul *) ->
          let splits_59 = Memo.splits (snd x_41) in
          let split0_59 = List.nth splits_59 0 in
          let split1_36 = List.nth splits_59 1 in
          ignore (pop_env w_90);
          push_env w_90 split0_59;
          push_env w_90 split1_36;
          assert_env_length w_90 3;
          push_env w_90 (Dynarray.get w_90.state.e 1);
          assert_env_length w_90 4;
          let keep_36 = env_call w_90 [ 1; 2 ] 1 in
          w_90.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_36; w_90.state.k ];
          w_90.state.c <- pc_to_exp (int_to_pc 90)
      | _ -> failwith "unreachable (92)")
    92;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 3;
      push_env w_92 (Dynarray.get w_92.state.e 0);
      w_92.state.c <- pc_to_exp (int_to_pc 95))
    93;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 5;
      let last_44 = Source.E 4 in
      let x_44 = resolve w_94 last_44 in
      match Word.get_value (fst x_44) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_94);
          assert_env_length w_94 4;
          push_env w_94 (Dynarray.get w_94.state.e 1);
          assert_env_length w_94 5;
          drop_n w_94 5 1;
          assert_env_length w_94 4;
          return_n w_94 4 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_94);
          assert_env_length w_94 4;
          push_env w_94 (Dynarray.get w_94.state.e 2);
          assert_env_length w_94 5;
          drop_n w_94 5 1;
          assert_env_length w_94 4;
          return_n w_94 4 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (94)")
    94;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 4;
      let last_43 = Source.E 3 in
      let x_43 = resolve w_93 last_43 in
      match Word.get_value (fst x_43) with
      | 5 (* tag_Const *) ->
          let splits_60 = Memo.splits (snd x_43) in
          let split0_60 = List.nth splits_60 0 in
          ignore (pop_env w_93);
          push_env w_93 split0_60;
          assert_env_length w_93 4;
          push_env w_93 (Dynarray.get w_93.state.e 3);
          assert_env_length w_93 5;
          drop_n w_93 5 1;
          assert_env_length w_93 4;
          return_n w_93 4 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_61 = Memo.splits (snd x_43) in
          let split0_61 = List.nth splits_61 0 in
          ignore (pop_env w_93);
          push_env w_93 split0_61;
          assert_env_length w_93 4;
          push_env w_93 (Dynarray.get w_93.state.e 3);
          w_93.state.c <- pc_to_exp (int_to_pc 94)
      | 7 (* tag_Add *) ->
          let splits_62 = Memo.splits (snd x_43) in
          let split0_62 = List.nth splits_62 0 in
          let split1_37 = List.nth splits_62 1 in
          ignore (pop_env w_93);
          push_env w_93 split0_62;
          push_env w_93 split1_37;
          assert_env_length w_93 5;
          push_env w_93 (Dynarray.get w_93.state.e 3);
          assert_env_length w_93 6;
          push_env w_93 (Dynarray.get w_93.state.e 1);
          assert_env_length w_93 7;
          push_env w_93 (Dynarray.get w_93.state.e 2);
          assert_env_length w_93 8;
          let keep_37 = env_call w_93 [ 1; 2; 4 ] 3 in
          w_93.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_37; w_93.state.k ];
          w_93.state.c <- pc_to_exp (int_to_pc 93)
      | 8 (* tag_Mul *) ->
          let splits_63 = Memo.splits (snd x_43) in
          let split0_63 = List.nth splits_63 0 in
          let split1_38 = List.nth splits_63 1 in
          ignore (pop_env w_93);
          push_env w_93 split0_63;
          push_env w_93 split1_38;
          assert_env_length w_93 5;
          push_env w_93 (Dynarray.get w_93.state.e 3);
          assert_env_length w_93 6;
          push_env w_93 (Dynarray.get w_93.state.e 1);
          assert_env_length w_93 7;
          push_env w_93 (Dynarray.get w_93.state.e 2);
          assert_env_length w_93 8;
          let keep_38 = env_call w_93 [ 1; 2; 4 ] 3 in
          w_93.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_38; w_93.state.k ];
          w_93.state.c <- pc_to_exp (int_to_pc 93)
      | _ -> failwith "unreachable (95)")
    95;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 1;
      push_env w_95 (Dynarray.get w_95.state.e 0);
      assert_env_length w_95 2;
      let keep_39 = env_call w_95 [] 1 in
      w_95.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_39; w_95.state.k ];
      w_95.state.c <- pc_to_exp (int_to_pc 90))
    96;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 3;
      let x0_6 = resolve w_97 (Source.E 1) in
      let x1_6 = resolve w_97 (Source.E 2) in
      ignore (pop_env w_97);
      ignore (pop_env w_97);
      push_env w_97 (Memo.from_int (Word.get_value (fst x0_6) + Word.get_value (fst x1_6)));
      assert_env_length w_97 2;
      push_env w_97 (Dynarray.get w_97.state.e 0);
      assert_env_length w_97 3;
      let keep_44 = env_call w_97 [ 1 ] 1 in
      w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_44; w_97.state.k ];
      w_97.state.c <- pc_to_exp (int_to_pc 13))
    97;
  add_exp
    (fun w_98 ->
      assert_env_length w_98 3;
      let x0_7 = resolve w_98 (Source.E 1) in
      let x1_7 = resolve w_98 (Source.E 2) in
      ignore (pop_env w_98);
      ignore (pop_env w_98);
      push_env w_98 (Memo.from_int (Word.get_value (fst x0_7) + Word.get_value (fst x1_7)));
      assert_env_length w_98 2;
      push_env w_98 (Dynarray.get w_98.state.e 0);
      assert_env_length w_98 3;
      let keep_45 = env_call w_98 [ 1 ] 1 in
      w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_45; w_98.state.k ];
      w_98.state.c <- pc_to_exp (int_to_pc 13))
    98;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 3;
      let last_45 = Source.E 2 in
      let x_45 = resolve w_100 last_45 in
      match Word.get_value (fst x_45) with
      | 8 (* tag_Mul *) ->
          let splits_64 = Memo.splits (snd x_45) in
          let split0_64 = List.nth splits_64 0 in
          let split1_39 = List.nth splits_64 1 in
          ignore (pop_env w_100);
          push_env w_100 split0_64;
          push_env w_100 split1_39;
          assert_env_length w_100 4;
          push_env w_100 (Dynarray.get w_100.state.e 0);
          assert_env_length w_100 5;
          push_env w_100 (Dynarray.get w_100.state.e 2);
          assert_env_length w_100 6;
          let keep_47 = env_call w_100 [ 0; 2; 3 ] 2 in
          w_100.state.k <- Memo.appends [ Memo.from_constructor tag_cont_50; keep_47; w_100.state.k ];
          w_100.state.c <- pc_to_exp (int_to_pc 29)
      | _ ->
          ignore (pop_env w_100);
          assert_env_length w_100 2;
          push_env w_100 (Memo.from_constructor tag_Missing);
          assert_env_length w_100 3;
          return_n w_100 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (99)")
    99;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 3;
      let cond_4 = resolve w_99 (Source.E 2) in
      ignore (pop_env w_99);
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_99 2;
        push_env w_99 (Memo.from_int 1);
        assert_env_length w_99 3;
        let ctor_arg_28 = pop_env w_99 in
        push_env w_99 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_28 ]);
        assert_env_length w_99 3;
        let ctor_arg_29 = pop_env w_99 in
        push_env w_99 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_29 ]);
        assert_env_length w_99 3;
        return_n w_99 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_99 2;
        push_env w_99 (Dynarray.get w_99.state.e 1);
        w_99.state.c <- pc_to_exp (int_to_pc 99)))
    100;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 6;
      let last_46 = Source.E 5 in
      let x_46 = resolve w_101 last_46 in
      match Word.get_value (fst x_46) with
      | 10 (* tag_Found *) ->
          let splits_65 = Memo.splits (snd x_46) in
          let split0_65 = List.nth splits_65 0 in
          ignore (pop_env w_101);
          push_env w_101 split0_65;
          assert_env_length w_101 6;
          push_env w_101 (Dynarray.get w_101.state.e 2);
          assert_env_length w_101 7;
          push_env w_101 (Dynarray.get w_101.state.e 3);
          assert_env_length w_101 8;
          push_env w_101 (Dynarray.get w_101.state.e 5);
          assert_env_length w_101 9;
          let ctor_arg_30 = pop_env w_101 in
          let ctor_arg_31 = pop_env w_101 in
          push_env w_101 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_31; ctor_arg_30 ]);
          assert_env_length w_101 8;
          let ctor_arg_32 = pop_env w_101 in
          let ctor_arg_33 = pop_env w_101 in
          push_env w_101 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_33; ctor_arg_32 ]);
          assert_env_length w_101 7;
          drop_n w_101 7 1;
          assert_env_length w_101 6;
          drop_n w_101 6 1;
          assert_env_length w_101 5;
          drop_n w_101 5 2;
          assert_env_length w_101 3;
          return_n w_101 3 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_101);
          assert_env_length w_101 5;
          push_env w_101 (Dynarray.get w_101.state.e 3);
          assert_env_length w_101 6;
          push_env w_101 (Dynarray.get w_101.state.e 1);
          assert_env_length w_101 7;
          let keep_48 = env_call w_101 [ 0; 1; 2; 3 ] 2 in
          w_101.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; keep_48; w_101.state.k ];
          w_101.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (101)")
    101;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 5;
      let cond_5 = resolve w_103 (Source.E 4) in
      ignore (pop_env w_103);
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_103 4;
        push_env w_103 (Dynarray.get w_103.state.e 0);
        assert_env_length w_103 5;
        push_env w_103 (Dynarray.get w_103.state.e 1);
        assert_env_length w_103 6;
        let ctor_arg_36 = pop_env w_103 in
        let ctor_arg_37 = pop_env w_103 in
        push_env w_103 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_37; ctor_arg_36 ]);
        assert_env_length w_103 5;
        drop_n w_103 5 2;
        assert_env_length w_103 3;
        return_n w_103 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_103 4;
        push_env w_103 (Dynarray.get w_103.state.e 2);
        assert_env_length w_103 5;
        push_env w_103 (Dynarray.get w_103.state.e 0);
        assert_env_length w_103 6;
        push_env w_103 (Dynarray.get w_103.state.e 3);
        assert_env_length w_103 7;
        let keep_49 = env_call w_103 [ 4 ] 2 in
        w_103.state.k <- Memo.appends [ Memo.from_constructor tag_cont_52; keep_49; w_103.state.k ];
        w_103.state.c <- pc_to_exp (int_to_pc 34)))
    102;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 6;
      let x0_8 = resolve w_102 (Source.E 4) in
      let x1_8 = resolve w_102 (Source.E 5) in
      ignore (pop_env w_102);
      ignore (pop_env w_102);
      push_env w_102 (Memo.from_int (if Word.get_value (fst x0_8) <= Word.get_value (fst x1_8) then 1 else 0));
      w_102.state.c <- pc_to_exp (int_to_pc 102))
    103;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 5;
      let cond_6 = resolve w_105 (Source.E 4) in
      ignore (pop_env w_105);
      if Word.get_value (fst cond_6) <> 0 then (
        assert_env_length w_105 4;
        push_env w_105 (Dynarray.get w_105.state.e 0);
        assert_env_length w_105 5;
        push_env w_105 (Dynarray.get w_105.state.e 1);
        assert_env_length w_105 6;
        let ctor_arg_38 = pop_env w_105 in
        let ctor_arg_39 = pop_env w_105 in
        push_env w_105 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_39; ctor_arg_38 ]);
        assert_env_length w_105 5;
        drop_n w_105 5 2;
        assert_env_length w_105 3;
        return_n w_105 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_105 4;
        push_env w_105 (Dynarray.get w_105.state.e 2);
        assert_env_length w_105 5;
        push_env w_105 (Dynarray.get w_105.state.e 0);
        assert_env_length w_105 6;
        push_env w_105 (Dynarray.get w_105.state.e 3);
        assert_env_length w_105 7;
        let keep_51 = env_call w_105 [ 4 ] 2 in
        w_105.state.k <- Memo.appends [ Memo.from_constructor tag_cont_54; keep_51; w_105.state.k ];
        w_105.state.c <- pc_to_exp (int_to_pc 39)))
    104;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 6;
      let x0_9 = resolve w_104 (Source.E 4) in
      let x1_9 = resolve w_104 (Source.E 5) in
      ignore (pop_env w_104);
      ignore (pop_env w_104);
      push_env w_104 (Memo.from_int (if Word.get_value (fst x0_9) <= Word.get_value (fst x1_9) then 1 else 0));
      w_104.state.c <- pc_to_exp (int_to_pc 104))
    105;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 4;
      let cond_7 = resolve w_108 (Source.E 3) in
      ignore (pop_env w_108);
      if Word.get_value (fst cond_7) <> 0 then (
        assert_env_length w_108 3;
        push_env w_108 (Dynarray.get w_108.state.e 0);
        assert_env_length w_108 4;
        ignore (env_call w_108 [] 1);
        w_108.state.c <- pc_to_exp (int_to_pc 60))
      else (
        assert_env_length w_108 3;
        push_env w_108 (Dynarray.get w_108.state.e 1);
        assert_env_length w_108 4;
        push_env w_108 (Dynarray.get w_108.state.e 0);
        assert_env_length w_108 5;
        let keep_55 = env_call w_108 [ 3 ] 1 in
        w_108.state.k <- Memo.appends [ Memo.from_constructor tag_cont_58; keep_55; w_108.state.k ];
        w_108.state.c <- pc_to_exp (int_to_pc 60)))
    106;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 5;
      let x0_10 = resolve w_107 (Source.E 3) in
      let x1_10 = resolve w_107 (Source.E 4) in
      ignore (pop_env w_107);
      ignore (pop_env w_107);
      push_env w_107 (Memo.from_int (if Word.get_value (fst x0_10) = Word.get_value (fst x1_10) then 1 else 0));
      w_107.state.c <- pc_to_exp (int_to_pc 106))
    107;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 3;
      let last_47 = Source.E 2 in
      let x_47 = resolve w_106 last_47 in
      match Word.get_value (fst x_47) with
      | 5 (* tag_Const *) ->
          let splits_66 = Memo.splits (snd x_47) in
          let split0_66 = List.nth splits_66 0 in
          ignore (pop_env w_106);
          push_env w_106 split0_66;
          assert_env_length w_106 3;
          push_env w_106 (Dynarray.get w_106.state.e 2);
          assert_env_length w_106 4;
          push_env w_106 (Memo.from_int 1);
          w_106.state.c <- pc_to_exp (int_to_pc 107)
      | _ ->
          ignore (pop_env w_106);
          assert_env_length w_106 2;
          push_env w_106 (Dynarray.get w_106.state.e 1);
          assert_env_length w_106 3;
          push_env w_106 (Dynarray.get w_106.state.e 0);
          assert_env_length w_106 4;
          let keep_56 = env_call w_106 [ 2 ] 1 in
          w_106.state.k <- Memo.appends [ Memo.from_constructor tag_cont_59; keep_56; w_106.state.k ];
          w_106.state.c <- pc_to_exp (int_to_pc 60)
      | _ -> failwith "unreachable (108)")
    108;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 3;
      let last_48 = Source.E 2 in
      let x_48 = resolve w_109 last_48 in
      match Word.get_value (fst x_48) with
      | 13 (* tag_NoPick *) ->
          ignore (pop_env w_109);
          assert_env_length w_109 2;
          push_env w_109 (Dynarray.get w_109.state.e 0);
          assert_env_length w_109 3;
          push_env w_109 (Dynarray.get w_109.state.e 1);
          assert_env_length w_109 4;
          let keep_63 = env_call w_109 [ 2 ] 1 in
          w_109.state.k <- Memo.appends [ Memo.from_constructor tag_cont_66; keep_63; w_109.state.k ];
          w_109.state.c <- pc_to_exp (int_to_pc 77)
      | 14 (* tag_Pick *) ->
          let splits_67 = Memo.splits (snd x_48) in
          let split0_67 = List.nth splits_67 0 in
          let split1_40 = List.nth splits_67 1 in
          ignore (pop_env w_109);
          push_env w_109 split0_67;
          push_env w_109 split1_40;
          assert_env_length w_109 4;
          push_env w_109 (Dynarray.get w_109.state.e 2);
          assert_env_length w_109 5;
          push_env w_109 (Dynarray.get w_109.state.e 3);
          assert_env_length w_109 6;
          let keep_64 = env_call w_109 [] 2 in
          w_109.state.k <- Memo.appends [ Memo.from_constructor tag_cont_67; keep_64; w_109.state.k ];
          w_109.state.c <- pc_to_exp (int_to_pc 39)
      | _ -> failwith "unreachable (109)")
    109;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 7;
      let cond_11 = resolve w_119 (Source.E 6) in
      ignore (pop_env w_119);
      if Word.get_value (fst cond_11) <> 0 then (
        assert_env_length w_119 6;
        push_env w_119 (Memo.from_int 1);
        assert_env_length w_119 7;
        drop_n w_119 7 1;
        assert_env_length w_119 6;
        drop_n w_119 6 1;
        assert_env_length w_119 5;
        drop_n w_119 5 1;
        assert_env_length w_119 4;
        drop_n w_119 4 1;
        assert_env_length w_119 3;
        return_n w_119 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_119 6;
        push_env w_119 (Memo.from_int 0);
        assert_env_length w_119 7;
        drop_n w_119 7 1;
        assert_env_length w_119 6;
        drop_n w_119 6 1;
        assert_env_length w_119 5;
        drop_n w_119 5 1;
        assert_env_length w_119 4;
        drop_n w_119 4 1;
        assert_env_length w_119 3;
        return_n w_119 3 (pc_to_exp (int_to_pc 0))))
    110;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 8;
      let x0_14 = resolve w_118 (Source.E 6) in
      let x1_14 = resolve w_118 (Source.E 7) in
      ignore (pop_env w_118);
      ignore (pop_env w_118);
      push_env w_118 (Memo.from_int (if Word.get_value (fst x0_14) > Word.get_value (fst x1_14) then 1 else 0));
      w_118.state.c <- pc_to_exp (int_to_pc 110))
    111;
  add_exp
    (fun w_120 ->
      assert_env_length w_120 8;
      let x0_15 = resolve w_120 (Source.E 6) in
      let x1_15 = resolve w_120 (Source.E 7) in
      ignore (pop_env w_120);
      ignore (pop_env w_120);
      push_env w_120 (Memo.from_int (Word.get_value (fst x0_15) - Word.get_value (fst x1_15)));
      assert_env_length w_120 7;
      drop_n w_120 7 1;
      assert_env_length w_120 6;
      drop_n w_120 6 1;
      assert_env_length w_120 5;
      drop_n w_120 5 1;
      assert_env_length w_120 4;
      drop_n w_120 4 1;
      assert_env_length w_120 3;
      return_n w_120 3 (pc_to_exp (int_to_pc 0)))
    112;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 7;
      let cond_10 = resolve w_117 (Source.E 6) in
      ignore (pop_env w_117);
      if Word.get_value (fst cond_10) <> 0 then (
        assert_env_length w_117 6;
        push_env w_117 (Memo.from_int 0);
        assert_env_length w_117 7;
        push_env w_117 (Memo.from_int 1);
        w_117.state.c <- pc_to_exp (int_to_pc 112))
      else (
        assert_env_length w_117 6;
        push_env w_117 (Dynarray.get w_117.state.e 4);
        assert_env_length w_117 7;
        push_env w_117 (Dynarray.get w_117.state.e 5);
        w_117.state.c <- pc_to_exp (int_to_pc 111)))
    113;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 8;
      let x0_13 = resolve w_116 (Source.E 6) in
      let x1_13 = resolve w_116 (Source.E 7) in
      ignore (pop_env w_116);
      ignore (pop_env w_116);
      push_env w_116 (Memo.from_int (if Word.get_value (fst x0_13) < Word.get_value (fst x1_13) then 1 else 0));
      w_116.state.c <- pc_to_exp (int_to_pc 113))
    114;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 6;
      let last_50 = Source.E 5 in
      let x_50 = resolve w_115 last_50 in
      match Word.get_value (fst x_50) with
      | 5 (* tag_Const *) ->
          let splits_69 = Memo.splits (snd x_50) in
          let split0_69 = List.nth splits_69 0 in
          ignore (pop_env w_115);
          push_env w_115 split0_69;
          assert_env_length w_115 6;
          push_env w_115 (Dynarray.get w_115.state.e 4);
          assert_env_length w_115 7;
          push_env w_115 (Dynarray.get w_115.state.e 5);
          w_115.state.c <- pc_to_exp (int_to_pc 114)
      | _ -> failwith "unreachable (115)")
    115;
  add_exp
    (fun w_121 ->
      assert_env_length w_121 6;
      let last_51 = Source.E 5 in
      let x_51 = resolve w_121 last_51 in
      match Word.get_value (fst x_51) with
      | 6 (* tag_Var *) ->
          let splits_71 = Memo.splits (snd x_51) in
          let split0_71 = List.nth splits_71 0 in
          ignore (pop_env w_121);
          push_env w_121 split0_71;
          assert_env_length w_121 6;
          push_env w_121 (Dynarray.get w_121.state.e 4);
          assert_env_length w_121 7;
          let keep_78 = env_call w_121 [ 5 ] 1 in
          w_121.state.k <- Memo.appends [ Memo.from_constructor tag_cont_81; keep_78; w_121.state.k ];
          w_121.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (116)")
    116;
  add_exp
    (fun w_122 ->
      assert_env_length w_122 7;
      let last_52 = Source.E 6 in
      let x_52 = resolve w_122 last_52 in
      match Word.get_value (fst x_52) with
      | 7 (* tag_Add *) ->
          let splits_73 = Memo.splits (snd x_52) in
          let split0_73 = List.nth splits_73 0 in
          let split1_42 = List.nth splits_73 1 in
          ignore (pop_env w_122);
          push_env w_122 split0_73;
          push_env w_122 split1_42;
          assert_env_length w_122 8;
          push_env w_122 (Dynarray.get w_122.state.e 4);
          assert_env_length w_122 9;
          push_env w_122 (Dynarray.get w_122.state.e 6);
          assert_env_length w_122 10;
          let keep_79 = env_call w_122 [ 5; 7 ] 2 in
          w_122.state.k <- Memo.appends [ Memo.from_constructor tag_cont_82; keep_79; w_122.state.k ];
          w_122.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (117)")
    117;
  add_exp
    (fun w_123 ->
      assert_env_length w_123 7;
      let last_53 = Source.E 6 in
      let x_53 = resolve w_123 last_53 in
      match Word.get_value (fst x_53) with
      | 8 (* tag_Mul *) ->
          let splits_75 = Memo.splits (snd x_53) in
          let split0_75 = List.nth splits_75 0 in
          let split1_44 = List.nth splits_75 1 in
          ignore (pop_env w_123);
          push_env w_123 split0_75;
          push_env w_123 split1_44;
          assert_env_length w_123 8;
          push_env w_123 (Dynarray.get w_123.state.e 4);
          assert_env_length w_123 9;
          push_env w_123 (Dynarray.get w_123.state.e 6);
          assert_env_length w_123 10;
          let keep_80 = env_call w_123 [ 5; 7 ] 2 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_83; keep_80; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (118)")
    118;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 5;
      let last_49 = Source.E 4 in
      let x_49 = resolve w_114 last_49 in
      match Word.get_value (fst x_49) with
      | 5 (* tag_Const *) ->
          let splits_68 = Memo.splits (snd x_49) in
          let split0_68 = List.nth splits_68 0 in
          ignore (pop_env w_114);
          push_env w_114 split0_68;
          assert_env_length w_114 5;
          push_env w_114 (Dynarray.get w_114.state.e 1);
          w_114.state.c <- pc_to_exp (int_to_pc 115)
      | 6 (* tag_Var *) ->
          let splits_70 = Memo.splits (snd x_49) in
          let split0_70 = List.nth splits_70 0 in
          ignore (pop_env w_114);
          push_env w_114 split0_70;
          assert_env_length w_114 5;
          push_env w_114 (Dynarray.get w_114.state.e 1);
          w_114.state.c <- pc_to_exp (int_to_pc 116)
      | 7 (* tag_Add *) ->
          let splits_72 = Memo.splits (snd x_49) in
          let split0_72 = List.nth splits_72 0 in
          let split1_41 = List.nth splits_72 1 in
          ignore (pop_env w_114);
          push_env w_114 split0_72;
          push_env w_114 split1_41;
          assert_env_length w_114 6;
          push_env w_114 (Dynarray.get w_114.state.e 1);
          w_114.state.c <- pc_to_exp (int_to_pc 117)
      | 8 (* tag_Mul *) ->
          let splits_74 = Memo.splits (snd x_49) in
          let split0_74 = List.nth splits_74 0 in
          let split1_43 = List.nth splits_74 1 in
          ignore (pop_env w_114);
          push_env w_114 split0_74;
          push_env w_114 split1_43;
          assert_env_length w_114 6;
          push_env w_114 (Dynarray.get w_114.state.e 1);
          w_114.state.c <- pc_to_exp (int_to_pc 118)
      | _ -> failwith "unreachable (119)")
    119;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 5;
      let cond_9 = resolve w_113 (Source.E 4) in
      ignore (pop_env w_113);
      if Word.get_value (fst cond_9) <> 0 then (
        assert_env_length w_113 4;
        push_env w_113 (Memo.from_int 1);
        assert_env_length w_113 5;
        drop_n w_113 5 1;
        assert_env_length w_113 4;
        drop_n w_113 4 1;
        assert_env_length w_113 3;
        return_n w_113 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_113 4;
        push_env w_113 (Dynarray.get w_113.state.e 0);
        w_113.state.c <- pc_to_exp (int_to_pc 119)))
    120;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 6;
      let x0_12 = resolve w_112 (Source.E 4) in
      let x1_12 = resolve w_112 (Source.E 5) in
      ignore (pop_env w_112);
      ignore (pop_env w_112);
      push_env w_112 (Memo.from_int (if Word.get_value (fst x0_12) > Word.get_value (fst x1_12) then 1 else 0));
      w_112.state.c <- pc_to_exp (int_to_pc 120))
    121;
  add_exp
    (fun w_124 ->
      assert_env_length w_124 6;
      let x0_16 = resolve w_124 (Source.E 4) in
      let x1_16 = resolve w_124 (Source.E 5) in
      ignore (pop_env w_124);
      ignore (pop_env w_124);
      push_env w_124 (Memo.from_int (Word.get_value (fst x0_16) - Word.get_value (fst x1_16)));
      assert_env_length w_124 5;
      drop_n w_124 5 1;
      assert_env_length w_124 4;
      drop_n w_124 4 1;
      assert_env_length w_124 3;
      return_n w_124 3 (pc_to_exp (int_to_pc 0)))
    122;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 5;
      let cond_8 = resolve w_111 (Source.E 4) in
      ignore (pop_env w_111);
      if Word.get_value (fst cond_8) <> 0 then (
        assert_env_length w_111 4;
        push_env w_111 (Memo.from_int 0);
        assert_env_length w_111 5;
        push_env w_111 (Memo.from_int 1);
        w_111.state.c <- pc_to_exp (int_to_pc 122))
      else (
        assert_env_length w_111 4;
        push_env w_111 (Dynarray.get w_111.state.e 2);
        assert_env_length w_111 5;
        push_env w_111 (Dynarray.get w_111.state.e 3);
        w_111.state.c <- pc_to_exp (int_to_pc 121)))
    123;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 6;
      let x0_11 = resolve w_110 (Source.E 4) in
      let x1_11 = resolve w_110 (Source.E 5) in
      ignore (pop_env w_110);
      ignore (pop_env w_110);
      push_env w_110 (Memo.from_int (if Word.get_value (fst x0_11) < Word.get_value (fst x1_11) then 1 else 0));
      w_110.state.c <- pc_to_exp (int_to_pc 123))
    124;
  add_exp
    (fun w_125 ->
      assert_env_length w_125 2;
      let x0_17 = resolve w_125 (Source.E 0) in
      let x1_17 = resolve w_125 (Source.E 1) in
      ignore (pop_env w_125);
      ignore (pop_env w_125);
      push_env w_125 (Memo.from_int (if Word.get_value (fst x0_17) = Word.get_value (fst x1_17) then 1 else 0));
      assert_env_length w_125 1;
      drop_n w_125 1 0;
      assert_env_length w_125 1;
      drop_n w_125 1 0;
      assert_env_length w_125 1;
      return_n w_125 1 (pc_to_exp (int_to_pc 0)))
    125;
  add_exp
    (fun w_126 ->
      assert_env_length w_126 2;
      let x0_18 = resolve w_126 (Source.E 0) in
      let x1_18 = resolve w_126 (Source.E 1) in
      ignore (pop_env w_126);
      ignore (pop_env w_126);
      push_env w_126
        (Memo.from_int (if Word.get_value (fst x0_18) <> 0 && Word.get_value (fst x1_18) <> 0 then 1 else 0));
      assert_env_length w_126 1;
      drop_n w_126 1 0;
      assert_env_length w_126 1;
      drop_n w_126 1 0;
      assert_env_length w_126 1;
      return_n w_126 1 (pc_to_exp (int_to_pc 0)))
    126;
  add_exp
    (fun w_127 ->
      assert_env_length w_127 2;
      let x0_19 = resolve w_127 (Source.E 0) in
      let x1_19 = resolve w_127 (Source.E 1) in
      ignore (pop_env w_127);
      ignore (pop_env w_127);
      push_env w_127
        (Memo.from_int (if Word.get_value (fst x0_19) <> 0 && Word.get_value (fst x1_19) <> 0 then 1 else 0));
      assert_env_length w_127 1;
      drop_n w_127 1 0;
      assert_env_length w_127 1;
      drop_n w_127 1 0;
      assert_env_length w_127 1;
      return_n w_127 1 (pc_to_exp (int_to_pc 0)))
    127;
  add_exp
    (fun w_128 ->
      assert_env_length w_128 2;
      let x0_20 = resolve w_128 (Source.E 0) in
      let x1_20 = resolve w_128 (Source.E 1) in
      ignore (pop_env w_128);
      ignore (pop_env w_128);
      push_env w_128 (Memo.from_int (Word.get_value (fst x0_20) + Word.get_value (fst x1_20)));
      assert_env_length w_128 1;
      drop_n w_128 1 0;
      assert_env_length w_128 1;
      return_n w_128 1 (pc_to_exp (int_to_pc 0)))
    128;
  add_exp
    (fun w_129 ->
      assert_env_length w_129 2;
      let x0_21 = resolve w_129 (Source.E 0) in
      let x1_21 = resolve w_129 (Source.E 1) in
      ignore (pop_env w_129);
      ignore (pop_env w_129);
      push_env w_129 (Memo.from_int (Word.get_value (fst x0_21) + Word.get_value (fst x1_21)));
      assert_env_length w_129 1;
      drop_n w_129 1 0;
      assert_env_length w_129 1;
      return_n w_129 1 (pc_to_exp (int_to_pc 0)))
    129;
  add_exp
    (fun w_133 ->
      assert_env_length w_133 5;
      let cond_13 = resolve w_133 (Source.E 4) in
      ignore (pop_env w_133);
      if Word.get_value (fst cond_13) <> 0 then (
        assert_env_length w_133 4;
        push_env w_133 (Dynarray.get w_133.state.e 1);
        assert_env_length w_133 5;
        drop_n w_133 5 1;
        assert_env_length w_133 4;
        drop_n w_133 4 1;
        assert_env_length w_133 3;
        return_n w_133 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_133 4;
        push_env w_133 (Dynarray.get w_133.state.e 0);
        assert_env_length w_133 5;
        push_env w_133 (Dynarray.get w_133.state.e 1);
        assert_env_length w_133 6;
        let keep_81 = env_call w_133 [ 0; 1 ] 2 in
        w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_84; keep_81; w_133.state.k ];
        w_133.state.c <- pc_to_exp (int_to_pc 5)))
    130;
  add_exp
    (fun w_132 ->
      assert_env_length w_132 6;
      let x0_23 = resolve w_132 (Source.E 4) in
      let x1_23 = resolve w_132 (Source.E 5) in
      ignore (pop_env w_132);
      ignore (pop_env w_132);
      push_env w_132 (Memo.from_int (if Word.get_value (fst x0_23) < Word.get_value (fst x1_23) then 1 else 0));
      w_132.state.c <- pc_to_exp (int_to_pc 130))
    131;
  add_exp
    (fun w_131 ->
      assert_env_length w_131 5;
      let cond_12 = resolve w_131 (Source.E 4) in
      ignore (pop_env w_131);
      if Word.get_value (fst cond_12) <> 0 then (
        assert_env_length w_131 4;
        push_env w_131 (Dynarray.get w_131.state.e 0);
        assert_env_length w_131 5;
        drop_n w_131 5 1;
        assert_env_length w_131 4;
        drop_n w_131 4 1;
        assert_env_length w_131 3;
        return_n w_131 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_131 4;
        push_env w_131 (Dynarray.get w_131.state.e 3);
        assert_env_length w_131 5;
        push_env w_131 (Dynarray.get w_131.state.e 2);
        w_131.state.c <- pc_to_exp (int_to_pc 131)))
    132;
  add_exp
    (fun w_130 ->
      assert_env_length w_130 6;
      let x0_22 = resolve w_130 (Source.E 4) in
      let x1_22 = resolve w_130 (Source.E 5) in
      ignore (pop_env w_130);
      ignore (pop_env w_130);
      push_env w_130 (Memo.from_int (if Word.get_value (fst x0_22) < Word.get_value (fst x1_22) then 1 else 0));
      w_130.state.c <- pc_to_exp (int_to_pc 132))
    133;
  add_exp
    (fun w_134 ->
      assert_env_length w_134 5;
      let last_54 = Source.E 4 in
      let x_54 = resolve w_134 last_54 in
      match Word.get_value (fst x_54) with
      | 10 (* tag_Found *) ->
          let splits_76 = Memo.splits (snd x_54) in
          let split0_76 = List.nth splits_76 0 in
          ignore (pop_env w_134);
          push_env w_134 split0_76;
          assert_env_length w_134 5;
          push_env w_134 (Dynarray.get w_134.state.e 4);
          assert_env_length w_134 6;
          push_env w_134 (Dynarray.get w_134.state.e 2);
          assert_env_length w_134 7;
          let ctor_arg_52 = pop_env w_134 in
          let ctor_arg_53 = pop_env w_134 in
          push_env w_134 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_53; ctor_arg_52 ]);
          assert_env_length w_134 6;
          let ctor_arg_54 = pop_env w_134 in
          push_env w_134 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_54 ]);
          assert_env_length w_134 6;
          drop_n w_134 6 1;
          assert_env_length w_134 5;
          drop_n w_134 5 1;
          assert_env_length w_134 4;
          drop_n w_134 4 2;
          assert_env_length w_134 2;
          return_n w_134 2 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_134);
          assert_env_length w_134 4;
          push_env w_134 (Dynarray.get w_134.state.e 0);
          assert_env_length w_134 5;
          push_env w_134 (Dynarray.get w_134.state.e 2);
          assert_env_length w_134 6;
          let keep_82 = env_call w_134 [ 1 ] 2 in
          w_134.state.k <- Memo.appends [ Memo.from_constructor tag_cont_85; keep_82; w_134.state.k ];
          w_134.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (134)")
    134;
  add_exp
    (fun w_135 ->
      assert_env_length w_135 6;
      let last_55 = Source.E 5 in
      let x_55 = resolve w_135 last_55 in
      match Word.get_value (fst x_55) with
      | 10 (* tag_Found *) ->
          let splits_77 = Memo.splits (snd x_55) in
          let split0_77 = List.nth splits_77 0 in
          ignore (pop_env w_135);
          push_env w_135 split0_77;
          assert_env_length w_135 6;
          push_env w_135 (Dynarray.get w_135.state.e 3);
          assert_env_length w_135 7;
          push_env w_135 (Dynarray.get w_135.state.e 2);
          assert_env_length w_135 8;
          push_env w_135 (Dynarray.get w_135.state.e 5);
          assert_env_length w_135 9;
          let ctor_arg_55 = pop_env w_135 in
          let ctor_arg_56 = pop_env w_135 in
          push_env w_135 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_56; ctor_arg_55 ]);
          assert_env_length w_135 8;
          let ctor_arg_57 = pop_env w_135 in
          let ctor_arg_58 = pop_env w_135 in
          push_env w_135 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_58; ctor_arg_57 ]);
          assert_env_length w_135 7;
          drop_n w_135 7 1;
          assert_env_length w_135 6;
          drop_n w_135 6 1;
          assert_env_length w_135 5;
          drop_n w_135 5 0;
          assert_env_length w_135 5;
          drop_n w_135 5 2;
          assert_env_length w_135 3;
          return_n w_135 3 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_135);
          assert_env_length w_135 5;
          push_env w_135 (Dynarray.get w_135.state.e 0);
          assert_env_length w_135 6;
          push_env w_135 (Dynarray.get w_135.state.e 1);
          assert_env_length w_135 7;
          let ctor_arg_59 = pop_env w_135 in
          let ctor_arg_60 = pop_env w_135 in
          push_env w_135 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_60; ctor_arg_59 ]);
          assert_env_length w_135 6;
          drop_n w_135 6 1;
          assert_env_length w_135 5;
          drop_n w_135 5 0;
          assert_env_length w_135 5;
          drop_n w_135 5 2;
          assert_env_length w_135 3;
          return_n w_135 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (135)")
    135;
  add_exp
    (fun w_136 ->
      assert_env_length w_136 2;
      let x0_24 = resolve w_136 (Source.E 0) in
      let x1_24 = resolve w_136 (Source.E 1) in
      ignore (pop_env w_136);
      ignore (pop_env w_136);
      push_env w_136 (Memo.from_int (Word.get_value (fst x0_24) * Word.get_value (fst x1_24)));
      assert_env_length w_136 1;
      drop_n w_136 1 0;
      assert_env_length w_136 1;
      return_n w_136 1 (pc_to_exp (int_to_pc 0)))
    136;
  add_exp
    (fun w_138 ->
      assert_env_length w_138 3;
      let cond_14 = resolve w_138 (Source.E 2) in
      ignore (pop_env w_138);
      if Word.get_value (fst cond_14) <> 0 then (
        assert_env_length w_138 2;
        push_env w_138 (Memo.from_int 0);
        assert_env_length w_138 3;
        let ctor_arg_65 = pop_env w_138 in
        push_env w_138 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_65 ]);
        assert_env_length w_138 3;
        drop_n w_138 3 1;
        assert_env_length w_138 2;
        drop_n w_138 2 1;
        assert_env_length w_138 1;
        return_n w_138 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_138 2;
        push_env w_138 (Dynarray.get w_138.state.e 0);
        assert_env_length w_138 3;
        let keep_84 = env_call w_138 [ 1 ] 1 in
        w_138.state.k <- Memo.appends [ Memo.from_constructor tag_cont_87; keep_84; w_138.state.k ];
        w_138.state.c <- pc_to_exp (int_to_pc 60)))
    137;
  add_exp
    (fun w_137 ->
      assert_env_length w_137 4;
      let x0_25 = resolve w_137 (Source.E 2) in
      let x1_25 = resolve w_137 (Source.E 3) in
      ignore (pop_env w_137);
      ignore (pop_env w_137);
      push_env w_137 (Memo.from_int (if Word.get_value (fst x0_25) = Word.get_value (fst x1_25) then 1 else 0));
      w_137.state.c <- pc_to_exp (int_to_pc 137))
    138;
  add_exp
    (fun w_139 ->
      assert_env_length w_139 5;
      let cond_15 = resolve w_139 (Source.E 4) in
      ignore (pop_env w_139);
      if Word.get_value (fst cond_15) <> 0 then (
        assert_env_length w_139 4;
        push_env w_139 (Dynarray.get w_139.state.e 0);
        assert_env_length w_139 5;
        push_env w_139 (Dynarray.get w_139.state.e 1);
        assert_env_length w_139 6;
        let keep_88 = env_call w_139 [ 4 ] 1 in
        w_139.state.k <- Memo.appends [ Memo.from_constructor tag_cont_90; keep_88; w_139.state.k ];
        w_139.state.c <- pc_to_exp (int_to_pc 72))
      else (
        assert_env_length w_139 4;
        push_env w_139 (Dynarray.get w_139.state.e 3);
        assert_env_length w_139 5;
        push_env w_139 (Dynarray.get w_139.state.e 2);
        assert_env_length w_139 6;
        let keep_87 = env_call w_139 [] 2 in
        w_139.state.k <- Memo.appends [ Memo.from_constructor tag_cont_89; keep_87; w_139.state.k ];
        w_139.state.c <- pc_to_exp (int_to_pc 39)))
    139;
  add_exp
    (fun w_140 ->
      assert_env_length w_140 5;
      let cond_16 = resolve w_140 (Source.E 4) in
      ignore (pop_env w_140);
      if Word.get_value (fst cond_16) <> 0 then (
        assert_env_length w_140 4;
        push_env w_140 (Dynarray.get w_140.state.e 0);
        assert_env_length w_140 5;
        push_env w_140 (Dynarray.get w_140.state.e 2);
        assert_env_length w_140 6;
        let keep_89 = env_call w_140 [ 1 ] 2 in
        w_140.state.k <- Memo.appends [ Memo.from_constructor tag_cont_91; keep_89; w_140.state.k ];
        w_140.state.c <- pc_to_exp (int_to_pc 75))
      else (
        assert_env_length w_140 4;
        push_env w_140 (Dynarray.get w_140.state.e 3);
        assert_env_length w_140 5;
        push_env w_140 (Dynarray.get w_140.state.e 2);
        assert_env_length w_140 6;
        let ctor_arg_66 = pop_env w_140 in
        let ctor_arg_67 = pop_env w_140 in
        push_env w_140 (Memo.appends [ Memo.from_constructor tag_Pick; ctor_arg_67; ctor_arg_66 ]);
        assert_env_length w_140 5;
        drop_n w_140 5 1;
        assert_env_length w_140 4;
        drop_n w_140 4 2;
        assert_env_length w_140 2;
        return_n w_140 2 (pc_to_exp (int_to_pc 0))))
    140;
  add_exp
    (fun w_143 ->
      assert_env_length w_143 4;
      let cond_17 = resolve w_143 (Source.E 3) in
      ignore (pop_env w_143);
      if Word.get_value (fst cond_17) <> 0 then (
        assert_env_length w_143 3;
        push_env w_143 (Dynarray.get w_143.state.e 1);
        assert_env_length w_143 4;
        drop_n w_143 4 1;
        assert_env_length w_143 3;
        drop_n w_143 3 1;
        assert_env_length w_143 2;
        drop_n w_143 2 1;
        assert_env_length w_143 1;
        drop_n w_143 1 0;
        assert_env_length w_143 1;
        return_n w_143 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_143 3;
        push_env w_143 (Dynarray.get w_143.state.e 0);
        assert_env_length w_143 4;
        push_env w_143 (Dynarray.get w_143.state.e 1);
        assert_env_length w_143 5;
        ignore (env_call w_143 [] 2);
        w_143.state.c <- pc_to_exp (int_to_pc 83)))
    141;
  add_exp
    (fun w_142 ->
      assert_env_length w_142 5;
      let x0_26 = resolve w_142 (Source.E 3) in
      let x1_26 = resolve w_142 (Source.E 4) in
      ignore (pop_env w_142);
      ignore (pop_env w_142);
      push_env w_142 (Memo.from_int (if Word.get_value (fst x0_26) = Word.get_value (fst x1_26) then 1 else 0));
      w_142.state.c <- pc_to_exp (int_to_pc 141))
    142;
  add_exp
    (fun w_141 ->
      assert_env_length w_141 3;
      let last_56 = Source.E 2 in
      let x_56 = resolve w_141 last_56 in
      match Word.get_value (fst x_56) with
      | 5 (* tag_Const *) ->
          let splits_78 = Memo.splits (snd x_56) in
          let split0_78 = List.nth splits_78 0 in
          ignore (pop_env w_141);
          push_env w_141 split0_78;
          assert_env_length w_141 3;
          push_env w_141 (Dynarray.get w_141.state.e 2);
          assert_env_length w_141 4;
          push_env w_141 (Memo.from_int 0);
          w_141.state.c <- pc_to_exp (int_to_pc 142)
      | _ ->
          ignore (pop_env w_141);
          assert_env_length w_141 2;
          push_env w_141 (Dynarray.get w_141.state.e 0);
          assert_env_length w_141 3;
          push_env w_141 (Dynarray.get w_141.state.e 1);
          assert_env_length w_141 4;
          ignore (env_call w_141 [] 2);
          w_141.state.c <- pc_to_exp (int_to_pc 83)
      | _ -> failwith "unreachable (143)")
    143;
  add_exp
    (fun w_148 ->
      assert_env_length w_148 4;
      let cond_19 = resolve w_148 (Source.E 3) in
      ignore (pop_env w_148);
      if Word.get_value (fst cond_19) <> 0 then (
        assert_env_length w_148 3;
        push_env w_148 (Dynarray.get w_148.state.e 1);
        assert_env_length w_148 4;
        drop_n w_148 4 1;
        assert_env_length w_148 3;
        drop_n w_148 3 1;
        assert_env_length w_148 2;
        drop_n w_148 2 1;
        assert_env_length w_148 1;
        drop_n w_148 1 0;
        assert_env_length w_148 1;
        return_n w_148 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_148 3;
        push_env w_148 (Dynarray.get w_148.state.e 0);
        assert_env_length w_148 4;
        push_env w_148 (Dynarray.get w_148.state.e 1);
        assert_env_length w_148 5;
        ignore (env_call w_148 [] 2);
        w_148.state.c <- pc_to_exp (int_to_pc 65)))
    144;
  add_exp
    (fun w_147 ->
      assert_env_length w_147 5;
      let x0_28 = resolve w_147 (Source.E 3) in
      let x1_28 = resolve w_147 (Source.E 4) in
      ignore (pop_env w_147);
      ignore (pop_env w_147);
      push_env w_147 (Memo.from_int (if Word.get_value (fst x0_28) = Word.get_value (fst x1_28) then 1 else 0));
      w_147.state.c <- pc_to_exp (int_to_pc 144))
    145;
  add_exp
    (fun w_146 ->
      assert_env_length w_146 4;
      let cond_18 = resolve w_146 (Source.E 3) in
      ignore (pop_env w_146);
      if Word.get_value (fst cond_18) <> 0 then (
        assert_env_length w_146 3;
        push_env w_146 (Dynarray.get w_146.state.e 0);
        assert_env_length w_146 4;
        drop_n w_146 4 1;
        assert_env_length w_146 3;
        drop_n w_146 3 1;
        assert_env_length w_146 2;
        drop_n w_146 2 1;
        assert_env_length w_146 1;
        drop_n w_146 1 0;
        assert_env_length w_146 1;
        return_n w_146 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_146 3;
        push_env w_146 (Dynarray.get w_146.state.e 2);
        assert_env_length w_146 4;
        push_env w_146 (Memo.from_int 1);
        w_146.state.c <- pc_to_exp (int_to_pc 145)))
    146;
  add_exp
    (fun w_145 ->
      assert_env_length w_145 5;
      let x0_27 = resolve w_145 (Source.E 3) in
      let x1_27 = resolve w_145 (Source.E 4) in
      ignore (pop_env w_145);
      ignore (pop_env w_145);
      push_env w_145 (Memo.from_int (if Word.get_value (fst x0_27) = Word.get_value (fst x1_27) then 1 else 0));
      w_145.state.c <- pc_to_exp (int_to_pc 146))
    147;
  add_exp
    (fun w_144 ->
      assert_env_length w_144 3;
      let last_57 = Source.E 2 in
      let x_57 = resolve w_144 last_57 in
      match Word.get_value (fst x_57) with
      | 5 (* tag_Const *) ->
          let splits_79 = Memo.splits (snd x_57) in
          let split0_79 = List.nth splits_79 0 in
          ignore (pop_env w_144);
          push_env w_144 split0_79;
          assert_env_length w_144 3;
          push_env w_144 (Dynarray.get w_144.state.e 2);
          assert_env_length w_144 4;
          push_env w_144 (Memo.from_int 0);
          w_144.state.c <- pc_to_exp (int_to_pc 147)
      | _ ->
          ignore (pop_env w_144);
          assert_env_length w_144 2;
          push_env w_144 (Dynarray.get w_144.state.e 0);
          assert_env_length w_144 3;
          push_env w_144 (Dynarray.get w_144.state.e 1);
          assert_env_length w_144 4;
          ignore (env_call w_144 [] 2);
          w_144.state.c <- pc_to_exp (int_to_pc 65)
      | _ -> failwith "unreachable (148)")
    148;
  add_exp
    (fun w_149 ->
      assert_env_length w_149 2;
      let cond_20 = resolve w_149 (Source.E 1) in
      ignore (pop_env w_149);
      if Word.get_value (fst cond_20) <> 0 then (
        assert_env_length w_149 1;
        push_env w_149 (Dynarray.get w_149.state.e 0);
        assert_env_length w_149 2;
        drop_n w_149 2 1;
        assert_env_length w_149 1;
        return_n w_149 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_149 1;
        push_env w_149 (Dynarray.get w_149.state.e 0);
        assert_env_length w_149 2;
        ignore (env_call w_149 [] 1);
        w_149.state.c <- pc_to_exp (int_to_pc 89)))
    149;
  add_exp
    (fun w_150 ->
      assert_env_length w_150 2;
      let x0_29 = resolve w_150 (Source.E 0) in
      let x1_29 = resolve w_150 (Source.E 1) in
      ignore (pop_env w_150);
      ignore (pop_env w_150);
      push_env w_150 (Memo.from_int (Word.get_value (fst x0_29) + Word.get_value (fst x1_29)));
      assert_env_length w_150 1;
      drop_n w_150 1 0;
      assert_env_length w_150 1;
      return_n w_150 1 (pc_to_exp (int_to_pc 0)))
    150;
  add_exp
    (fun w_151 ->
      assert_env_length w_151 2;
      let x0_30 = resolve w_151 (Source.E 0) in
      let x1_30 = resolve w_151 (Source.E 1) in
      ignore (pop_env w_151);
      ignore (pop_env w_151);
      push_env w_151 (Memo.from_int (Word.get_value (fst x0_30) * Word.get_value (fst x1_30)));
      assert_env_length w_151 1;
      drop_n w_151 1 0;
      assert_env_length w_151 1;
      return_n w_151 1 (pc_to_exp (int_to_pc 0)))
    151;
  add_exp
    (fun w_153 ->
      assert_env_length w_153 4;
      let cond_21 = resolve w_153 (Source.E 3) in
      ignore (pop_env w_153);
      if Word.get_value (fst cond_21) <> 0 then (
        assert_env_length w_153 3;
        push_env w_153 (Dynarray.get w_153.state.e 0);
        assert_env_length w_153 4;
        push_env w_153 (Dynarray.get w_153.state.e 1);
        assert_env_length w_153 5;
        ignore (env_call w_153 [] 2);
        w_153.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_153 3;
        push_env w_153 (Dynarray.get w_153.state.e 2);
        assert_env_length w_153 4;
        drop_n w_153 4 1;
        assert_env_length w_153 3;
        drop_n w_153 3 1;
        assert_env_length w_153 2;
        drop_n w_153 2 1;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        return_n w_153 1 (pc_to_exp (int_to_pc 0))))
    152;
  add_exp
    (fun w_152 ->
      assert_env_length w_152 5;
      let x0_31 = resolve w_152 (Source.E 3) in
      let x1_31 = resolve w_152 (Source.E 4) in
      ignore (pop_env w_152);
      ignore (pop_env w_152);
      push_env w_152 (Memo.from_int (if Word.get_value (fst x0_31) = Word.get_value (fst x1_31) then 1 else 0));
      w_152.state.c <- pc_to_exp (int_to_pc 152))
    153;
  add_exp
    (fun w_155 ->
      assert_env_length w_155 4;
      let cond_22 = resolve w_155 (Source.E 3) in
      ignore (pop_env w_155);
      if Word.get_value (fst cond_22) <> 0 then (
        assert_env_length w_155 3;
        push_env w_155 (Dynarray.get w_155.state.e 0);
        assert_env_length w_155 4;
        push_env w_155 (Dynarray.get w_155.state.e 1);
        assert_env_length w_155 5;
        ignore (env_call w_155 [] 2);
        w_155.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_155 3;
        push_env w_155 (Dynarray.get w_155.state.e 2);
        assert_env_length w_155 4;
        drop_n w_155 4 1;
        assert_env_length w_155 3;
        drop_n w_155 3 1;
        assert_env_length w_155 2;
        drop_n w_155 2 1;
        assert_env_length w_155 1;
        drop_n w_155 1 0;
        assert_env_length w_155 1;
        drop_n w_155 1 0;
        assert_env_length w_155 1;
        return_n w_155 1 (pc_to_exp (int_to_pc 0))))
    154;
  add_exp
    (fun w_154 ->
      assert_env_length w_154 5;
      let x0_32 = resolve w_154 (Source.E 3) in
      let x1_32 = resolve w_154 (Source.E 4) in
      ignore (pop_env w_154);
      ignore (pop_env w_154);
      push_env w_154 (Memo.from_int (if Word.get_value (fst x0_32) = Word.get_value (fst x1_32) then 1 else 0));
      w_154.state.c <- pc_to_exp (int_to_pc 154))
    155;
  add_exp
    (fun w_157 ->
      assert_env_length w_157 3;
      let cond_23 = resolve w_157 (Source.E 2) in
      ignore (pop_env w_157);
      if Word.get_value (fst cond_23) <> 0 then (
        assert_env_length w_157 2;
        push_env w_157 (Dynarray.get w_157.state.e 0);
        assert_env_length w_157 3;
        drop_n w_157 3 0;
        assert_env_length w_157 3;
        drop_n w_157 3 0;
        assert_env_length w_157 3;
        return_n w_157 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_157 2;
        push_env w_157 (Dynarray.get w_157.state.e 1);
        assert_env_length w_157 3;
        drop_n w_157 3 0;
        assert_env_length w_157 3;
        drop_n w_157 3 0;
        assert_env_length w_157 3;
        return_n w_157 3 (pc_to_exp (int_to_pc 0))))
    156;
  add_exp
    (fun w_156 ->
      assert_env_length w_156 4;
      let x0_33 = resolve w_156 (Source.E 2) in
      let x1_33 = resolve w_156 (Source.E 3) in
      ignore (pop_env w_156);
      ignore (pop_env w_156);
      push_env w_156 (Memo.from_int (if Word.get_value (fst x0_33) <= Word.get_value (fst x1_33) then 1 else 0));
      w_156.state.c <- pc_to_exp (int_to_pc 156))
    157;
  add_exp
    (fun w_158 ->
      assert_env_length w_158 3;
      let last_58 = Source.E 2 in
      let x_58 = resolve w_158 last_58 in
      match Word.get_value (fst x_58) with
      | 10 (* tag_Found *) ->
          let splits_80 = Memo.splits (snd x_58) in
          let split0_80 = List.nth splits_80 0 in
          ignore (pop_env w_158);
          push_env w_158 split0_80;
          assert_env_length w_158 3;
          push_env w_158 (Dynarray.get w_158.state.e 0);
          assert_env_length w_158 4;
          push_env w_158 (Dynarray.get w_158.state.e 2);
          assert_env_length w_158 5;
          let ctor_arg_76 = pop_env w_158 in
          let ctor_arg_77 = pop_env w_158 in
          push_env w_158 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_77; ctor_arg_76 ]);
          assert_env_length w_158 4;
          let ctor_arg_78 = pop_env w_158 in
          push_env w_158 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_78 ]);
          assert_env_length w_158 4;
          drop_n w_158 4 1;
          assert_env_length w_158 3;
          drop_n w_158 3 1;
          assert_env_length w_158 2;
          drop_n w_158 2 0;
          assert_env_length w_158 2;
          drop_n w_158 2 1;
          assert_env_length w_158 1;
          return_n w_158 1 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_158);
          assert_env_length w_158 2;
          push_env w_158 (Memo.from_constructor tag_Missing);
          assert_env_length w_158 3;
          drop_n w_158 3 1;
          assert_env_length w_158 2;
          drop_n w_158 2 0;
          assert_env_length w_158 2;
          drop_n w_158 2 1;
          assert_env_length w_158 1;
          return_n w_158 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (158)")
    158;
  add_exp
    (fun w_160 ->
      assert_env_length w_160 4;
      let cond_24 = resolve w_160 (Source.E 3) in
      ignore (pop_env w_160);
      if Word.get_value (fst cond_24) <> 0 then (
        assert_env_length w_160 3;
        push_env w_160 (Dynarray.get w_160.state.e 0);
        assert_env_length w_160 4;
        let keep_96 = env_call w_160 [ 0; 1 ] 1 in
        w_160.state.k <- Memo.appends [ Memo.from_constructor tag_cont_97; keep_96; w_160.state.k ];
        w_160.state.c <- pc_to_exp (int_to_pc 23))
      else (
        assert_env_length w_160 3;
        push_env w_160 (Dynarray.get w_160.state.e 2);
        assert_env_length w_160 4;
        drop_n w_160 4 1;
        assert_env_length w_160 3;
        drop_n w_160 3 0;
        assert_env_length w_160 3;
        drop_n w_160 3 0;
        assert_env_length w_160 3;
        return_n w_160 3 (pc_to_exp (int_to_pc 0))))
    159;
  add_exp
    (fun w_159 ->
      assert_env_length w_159 5;
      let x0_34 = resolve w_159 (Source.E 3) in
      let x1_34 = resolve w_159 (Source.E 4) in
      ignore (pop_env w_159);
      ignore (pop_env w_159);
      push_env w_159 (Memo.from_int (if Word.get_value (fst x0_34) = Word.get_value (fst x1_34) then 1 else 0));
      w_159.state.c <- pc_to_exp (int_to_pc 159))
    160;
  add_exp
    (fun w_162 ->
      assert_env_length w_162 8;
      let x0_35 = resolve w_162 (Source.E 6) in
      let x1_35 = resolve w_162 (Source.E 7) in
      ignore (pop_env w_162);
      ignore (pop_env w_162);
      push_env w_162 (Memo.from_int (Word.get_value (fst x0_35) + Word.get_value (fst x1_35)));
      assert_env_length w_162 7;
      push_env w_162 (Dynarray.get w_162.state.e 2);
      assert_env_length w_162 8;
      ignore (env_call w_162 [] 3);
      w_162.state.c <- pc_to_exp (int_to_pc 66))
    161;
  add_exp
    (fun w_161 ->
      assert_env_length w_161 6;
      let cond_25 = resolve w_161 (Source.E 5) in
      ignore (pop_env w_161);
      if Word.get_value (fst cond_25) <> 0 then (
        assert_env_length w_161 5;
        push_env w_161 (Dynarray.get w_161.state.e 0);
        assert_env_length w_161 6;
        push_env w_161 (Dynarray.get w_161.state.e 1);
        assert_env_length w_161 7;
        push_env w_161 (Dynarray.get w_161.state.e 4);
        w_161.state.c <- pc_to_exp (int_to_pc 161))
      else (
        assert_env_length w_161 5;
        push_env w_161 (Dynarray.get w_161.state.e 3);
        assert_env_length w_161 6;
        push_env w_161 (Dynarray.get w_161.state.e 4);
        assert_env_length w_161 7;
        push_env w_161 (Dynarray.get w_161.state.e 2);
        assert_env_length w_161 8;
        let keep_98 = env_call w_161 [ 0; 1 ] 3 in
        w_161.state.k <- Memo.appends [ Memo.from_constructor tag_cont_99; keep_98; w_161.state.k ];
        w_161.state.c <- pc_to_exp (int_to_pc 66)))
    162;
  add_exp
    (fun w_163 ->
      assert_env_length w_163 2;
      let last_59 = Source.E 1 in
      let x_59 = resolve w_163 last_59 in
      match Word.get_value (fst x_59) with
      | 13 (* tag_NoPick *) ->
          ignore (pop_env w_163);
          assert_env_length w_163 1;
          push_env w_163 (Memo.from_constructor tag_NoPick);
          assert_env_length w_163 2;
          drop_n w_163 2 0;
          assert_env_length w_163 2;
          drop_n w_163 2 1;
          assert_env_length w_163 1;
          return_n w_163 1 (pc_to_exp (int_to_pc 0))
      | 14 (* tag_Pick *) ->
          let splits_81 = Memo.splits (snd x_59) in
          let split0_81 = List.nth splits_81 0 in
          let split1_45 = List.nth splits_81 1 in
          ignore (pop_env w_163);
          push_env w_163 split0_81;
          push_env w_163 split1_45;
          assert_env_length w_163 3;
          push_env w_163 (Dynarray.get w_163.state.e 1);
          assert_env_length w_163 4;
          push_env w_163 (Dynarray.get w_163.state.e 0);
          assert_env_length w_163 5;
          push_env w_163 (Dynarray.get w_163.state.e 2);
          assert_env_length w_163 6;
          let ctor_arg_81 = pop_env w_163 in
          let ctor_arg_82 = pop_env w_163 in
          push_env w_163 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_82; ctor_arg_81 ]);
          assert_env_length w_163 5;
          let ctor_arg_83 = pop_env w_163 in
          let ctor_arg_84 = pop_env w_163 in
          push_env w_163 (Memo.appends [ Memo.from_constructor tag_Pick; ctor_arg_84; ctor_arg_83 ]);
          assert_env_length w_163 4;
          drop_n w_163 4 2;
          assert_env_length w_163 2;
          drop_n w_163 2 0;
          assert_env_length w_163 2;
          drop_n w_163 2 1;
          assert_env_length w_163 1;
          return_n w_163 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (163)")
    163;
  add_exp
    (fun w_167 ->
      assert_env_length w_167 3;
      let cond_27 = resolve w_167 (Source.E 2) in
      ignore (pop_env w_167);
      if Word.get_value (fst cond_27) <> 0 then (
        assert_env_length w_167 2;
        push_env w_167 (Memo.from_int 1);
        assert_env_length w_167 3;
        drop_n w_167 3 1;
        assert_env_length w_167 2;
        drop_n w_167 2 1;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        return_n w_167 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_167 2;
        push_env w_167 (Memo.from_int 0);
        assert_env_length w_167 3;
        drop_n w_167 3 1;
        assert_env_length w_167 2;
        drop_n w_167 2 1;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        drop_n w_167 1 0;
        assert_env_length w_167 1;
        return_n w_167 1 (pc_to_exp (int_to_pc 0))))
    164;
  add_exp
    (fun w_166 ->
      assert_env_length w_166 4;
      let x0_37 = resolve w_166 (Source.E 2) in
      let x1_37 = resolve w_166 (Source.E 3) in
      ignore (pop_env w_166);
      ignore (pop_env w_166);
      push_env w_166 (Memo.from_int (if Word.get_value (fst x0_37) > Word.get_value (fst x1_37) then 1 else 0));
      w_166.state.c <- pc_to_exp (int_to_pc 164))
    165;
  add_exp
    (fun w_168 ->
      assert_env_length w_168 4;
      let x0_38 = resolve w_168 (Source.E 2) in
      let x1_38 = resolve w_168 (Source.E 3) in
      ignore (pop_env w_168);
      ignore (pop_env w_168);
      push_env w_168 (Memo.from_int (Word.get_value (fst x0_38) - Word.get_value (fst x1_38)));
      assert_env_length w_168 3;
      drop_n w_168 3 1;
      assert_env_length w_168 2;
      drop_n w_168 2 1;
      assert_env_length w_168 1;
      drop_n w_168 1 0;
      assert_env_length w_168 1;
      drop_n w_168 1 0;
      assert_env_length w_168 1;
      drop_n w_168 1 0;
      assert_env_length w_168 1;
      drop_n w_168 1 0;
      assert_env_length w_168 1;
      return_n w_168 1 (pc_to_exp (int_to_pc 0)))
    166;
  add_exp
    (fun w_165 ->
      assert_env_length w_165 3;
      let cond_26 = resolve w_165 (Source.E 2) in
      ignore (pop_env w_165);
      if Word.get_value (fst cond_26) <> 0 then (
        assert_env_length w_165 2;
        push_env w_165 (Memo.from_int 0);
        assert_env_length w_165 3;
        push_env w_165 (Memo.from_int 1);
        w_165.state.c <- pc_to_exp (int_to_pc 166))
      else (
        assert_env_length w_165 2;
        push_env w_165 (Dynarray.get w_165.state.e 0);
        assert_env_length w_165 3;
        push_env w_165 (Dynarray.get w_165.state.e 1);
        w_165.state.c <- pc_to_exp (int_to_pc 165)))
    167;
  add_exp
    (fun w_164 ->
      assert_env_length w_164 4;
      let x0_36 = resolve w_164 (Source.E 2) in
      let x1_36 = resolve w_164 (Source.E 3) in
      ignore (pop_env w_164);
      ignore (pop_env w_164);
      push_env w_164 (Memo.from_int (if Word.get_value (fst x0_36) < Word.get_value (fst x1_36) then 1 else 0));
      w_164.state.c <- pc_to_exp (int_to_pc 167))
    168;
  add_exp
    (fun w_170 ->
      assert_env_length w_170 3;
      let cond_28 = resolve w_170 (Source.E 2) in
      ignore (pop_env w_170);
      if Word.get_value (fst cond_28) <> 0 then (
        assert_env_length w_170 2;
        push_env w_170 (Dynarray.get w_170.state.e 1);
        assert_env_length w_170 3;
        drop_n w_170 3 1;
        assert_env_length w_170 2;
        drop_n w_170 2 0;
        assert_env_length w_170 2;
        drop_n w_170 2 1;
        assert_env_length w_170 1;
        drop_n w_170 1 0;
        assert_env_length w_170 1;
        return_n w_170 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_170 2;
        push_env w_170 (Dynarray.get w_170.state.e 0);
        assert_env_length w_170 3;
        push_env w_170 (Dynarray.get w_170.state.e 1);
        assert_env_length w_170 4;
        ignore (env_call w_170 [] 2);
        w_170.state.c <- pc_to_exp (int_to_pc 16)))
    169;
  add_exp
    (fun w_169 ->
      assert_env_length w_169 4;
      let x0_39 = resolve w_169 (Source.E 2) in
      let x1_39 = resolve w_169 (Source.E 3) in
      ignore (pop_env w_169);
      ignore (pop_env w_169);
      push_env w_169 (Memo.from_int (if Word.get_value (fst x0_39) = Word.get_value (fst x1_39) then 1 else 0));
      w_169.state.c <- pc_to_exp (int_to_pc 169))
    170;
  add_exp
    (fun w_172 ->
      assert_env_length w_172 4;
      let cond_29 = resolve w_172 (Source.E 3) in
      ignore (pop_env w_172);
      if Word.get_value (fst cond_29) <> 0 then (
        assert_env_length w_172 3;
        push_env w_172 (Dynarray.get w_172.state.e 2);
        assert_env_length w_172 4;
        drop_n w_172 4 1;
        assert_env_length w_172 3;
        drop_n w_172 3 0;
        assert_env_length w_172 3;
        drop_n w_172 3 0;
        assert_env_length w_172 3;
        drop_n w_172 3 0;
        assert_env_length w_172 3;
        return_n w_172 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_172 3;
        push_env w_172 (Dynarray.get w_172.state.e 1);
        assert_env_length w_172 4;
        push_env w_172 (Dynarray.get w_172.state.e 0);
        assert_env_length w_172 5;
        let keep_103 = env_call w_172 [ 2 ] 2 in
        w_172.state.k <- Memo.appends [ Memo.from_constructor tag_cont_104; keep_103; w_172.state.k ];
        w_172.state.c <- pc_to_exp (int_to_pc 16)))
    171;
  add_exp
    (fun w_171 ->
      assert_env_length w_171 5;
      let x0_40 = resolve w_171 (Source.E 3) in
      let x1_40 = resolve w_171 (Source.E 4) in
      ignore (pop_env w_171);
      ignore (pop_env w_171);
      push_env w_171 (Memo.from_int (if Word.get_value (fst x0_40) = Word.get_value (fst x1_40) then 1 else 0));
      w_171.state.c <- pc_to_exp (int_to_pc 171))
    172;
  add_exp
    (fun w_176 ->
      assert_env_length w_176 5;
      let cond_31 = resolve w_176 (Source.E 4) in
      ignore (pop_env w_176);
      if Word.get_value (fst cond_31) <> 0 then (
        assert_env_length w_176 4;
        push_env w_176 (Memo.from_int 1);
        assert_env_length w_176 5;
        drop_n w_176 5 1;
        assert_env_length w_176 4;
        drop_n w_176 4 1;
        assert_env_length w_176 3;
        drop_n w_176 3 0;
        assert_env_length w_176 3;
        drop_n w_176 3 0;
        assert_env_length w_176 3;
        drop_n w_176 3 0;
        assert_env_length w_176 3;
        return_n w_176 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_176 4;
        push_env w_176 (Dynarray.get w_176.state.e 0);
        assert_env_length w_176 5;
        push_env w_176 (Dynarray.get w_176.state.e 1);
        assert_env_length w_176 6;
        ignore (env_call w_176 [] 2);
        w_176.state.c <- pc_to_exp (int_to_pc 5)))
    173;
  add_exp
    (fun w_175 ->
      assert_env_length w_175 6;
      let x0_42 = resolve w_175 (Source.E 4) in
      let x1_42 = resolve w_175 (Source.E 5) in
      ignore (pop_env w_175);
      ignore (pop_env w_175);
      push_env w_175 (Memo.from_int (if Word.get_value (fst x0_42) > Word.get_value (fst x1_42) then 1 else 0));
      w_175.state.c <- pc_to_exp (int_to_pc 173))
    174;
  add_exp
    (fun w_177 ->
      assert_env_length w_177 6;
      let x0_43 = resolve w_177 (Source.E 4) in
      let x1_43 = resolve w_177 (Source.E 5) in
      ignore (pop_env w_177);
      ignore (pop_env w_177);
      push_env w_177 (Memo.from_int (Word.get_value (fst x0_43) - Word.get_value (fst x1_43)));
      assert_env_length w_177 5;
      drop_n w_177 5 1;
      assert_env_length w_177 4;
      drop_n w_177 4 1;
      assert_env_length w_177 3;
      drop_n w_177 3 0;
      assert_env_length w_177 3;
      drop_n w_177 3 0;
      assert_env_length w_177 3;
      drop_n w_177 3 0;
      assert_env_length w_177 3;
      return_n w_177 3 (pc_to_exp (int_to_pc 0)))
    175;
  add_exp
    (fun w_174 ->
      assert_env_length w_174 5;
      let cond_30 = resolve w_174 (Source.E 4) in
      ignore (pop_env w_174);
      if Word.get_value (fst cond_30) <> 0 then (
        assert_env_length w_174 4;
        push_env w_174 (Memo.from_int 0);
        assert_env_length w_174 5;
        push_env w_174 (Memo.from_int 1);
        w_174.state.c <- pc_to_exp (int_to_pc 175))
      else (
        assert_env_length w_174 4;
        push_env w_174 (Dynarray.get w_174.state.e 2);
        assert_env_length w_174 5;
        push_env w_174 (Dynarray.get w_174.state.e 3);
        w_174.state.c <- pc_to_exp (int_to_pc 174)))
    176;
  add_exp
    (fun w_173 ->
      assert_env_length w_173 6;
      let x0_41 = resolve w_173 (Source.E 4) in
      let x1_41 = resolve w_173 (Source.E 5) in
      ignore (pop_env w_173);
      ignore (pop_env w_173);
      push_env w_173 (Memo.from_int (if Word.get_value (fst x0_41) < Word.get_value (fst x1_41) then 1 else 0));
      w_173.state.c <- pc_to_exp (int_to_pc 176))
    177;
  add_exp
    (fun w_178 ->
      assert_env_length w_178 4;
      let cond_32 = resolve w_178 (Source.E 3) in
      ignore (pop_env w_178);
      if Word.get_value (fst cond_32) <> 0 then (
        assert_env_length w_178 3;
        push_env w_178 (Dynarray.get w_178.state.e 1);
        assert_env_length w_178 4;
        drop_n w_178 4 1;
        assert_env_length w_178 3;
        drop_n w_178 3 0;
        assert_env_length w_178 3;
        drop_n w_178 3 0;
        assert_env_length w_178 3;
        drop_n w_178 3 1;
        assert_env_length w_178 2;
        drop_n w_178 2 0;
        assert_env_length w_178 2;
        drop_n w_178 2 0;
        assert_env_length w_178 2;
        drop_n w_178 2 0;
        assert_env_length w_178 2;
        drop_n w_178 2 1;
        assert_env_length w_178 1;
        return_n w_178 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_178 3;
        push_env w_178 (Dynarray.get w_178.state.e 0);
        assert_env_length w_178 4;
        push_env w_178 (Dynarray.get w_178.state.e 2);
        assert_env_length w_178 5;
        ignore (env_call w_178 [] 2);
        w_178.state.c <- pc_to_exp (int_to_pc 84)))
    178;
  add_exp
    (fun w_179 ->
      assert_env_length w_179 4;
      let cond_33 = resolve w_179 (Source.E 3) in
      ignore (pop_env w_179);
      if Word.get_value (fst cond_33) <> 0 then (
        assert_env_length w_179 3;
        push_env w_179 (Dynarray.get w_179.state.e 1);
        assert_env_length w_179 4;
        drop_n w_179 4 1;
        assert_env_length w_179 3;
        drop_n w_179 3 0;
        assert_env_length w_179 3;
        drop_n w_179 3 0;
        assert_env_length w_179 3;
        drop_n w_179 3 1;
        assert_env_length w_179 2;
        drop_n w_179 2 0;
        assert_env_length w_179 2;
        drop_n w_179 2 0;
        assert_env_length w_179 2;
        drop_n w_179 2 0;
        assert_env_length w_179 2;
        drop_n w_179 2 1;
        assert_env_length w_179 1;
        return_n w_179 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_179 3;
        push_env w_179 (Dynarray.get w_179.state.e 0);
        assert_env_length w_179 4;
        push_env w_179 (Dynarray.get w_179.state.e 2);
        assert_env_length w_179 5;
        ignore (env_call w_179 [] 2);
        w_179.state.c <- pc_to_exp (int_to_pc 84)))
    179;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 0;
  Words.set_constructor_degree 3 1;
  Words.set_constructor_degree 4 1;
  Words.set_constructor_degree 5 0;
  Words.set_constructor_degree 6 0;
  Words.set_constructor_degree 7 (-1);
  Words.set_constructor_degree 8 (-1);
  Words.set_constructor_degree 9 1;
  Words.set_constructor_degree 10 0;
  Words.set_constructor_degree 11 1;
  Words.set_constructor_degree 12 (-1);
  Words.set_constructor_degree 13 1;
  Words.set_constructor_degree 14 (-1);
  Words.set_constructor_degree 15 (-2);
  Words.set_constructor_degree 16 (-1);
  Words.set_constructor_degree 17 (-2);
  Words.set_constructor_degree 18 (-2);
  Words.set_constructor_degree 19 (-2);
  Words.set_constructor_degree 20 (-2);
  Words.set_constructor_degree 21 (-2);
  Words.set_constructor_degree 22 (-2);
  Words.set_constructor_degree 23 (-4);
  Words.set_constructor_degree 24 (-1);
  Words.set_constructor_degree 25 (-4);
  Words.set_constructor_degree 26 (-1);
  Words.set_constructor_degree 27 (-2);
  Words.set_constructor_degree 28 (-4);
  Words.set_constructor_degree 29 (-1);
  Words.set_constructor_degree 30 (-1);
  Words.set_constructor_degree 31 (-1);
  Words.set_constructor_degree 32 (-1);
  Words.set_constructor_degree 33 (-1);
  Words.set_constructor_degree 34 (-1);
  Words.set_constructor_degree 35 0;
  Words.set_constructor_degree 36 (-1);
  Words.set_constructor_degree 37 0;
  Words.set_constructor_degree 38 (-4);
  Words.set_constructor_degree 39 (-2);
  Words.set_constructor_degree 40 (-4);
  Words.set_constructor_degree 41 (-3);
  Words.set_constructor_degree 42 (-2);
  Words.set_constructor_degree 43 (-1);
  Words.set_constructor_degree 44 0;
  Words.set_constructor_degree 45 0;
  Words.set_constructor_degree 46 (-1);
  Words.set_constructor_degree 47 (-2);
  Words.set_constructor_degree 48 (-2);
  Words.set_constructor_degree 49 (-1);
  Words.set_constructor_degree 50 (-1);
  Words.set_constructor_degree 51 (-1);
  Words.set_constructor_degree 52 (-1);
  Words.set_constructor_degree 53 (-2);
  Words.set_constructor_degree 54 (-3);
  Words.set_constructor_degree 55 (-3);
  Words.set_constructor_degree 56 0;
  Words.set_constructor_degree 57 (-3);
  Words.set_constructor_degree 58 (-1);
  Words.set_constructor_degree 59 (-1);
  Words.set_constructor_degree 60 (-1);
  Words.set_constructor_degree 61 (-1);
  Words.set_constructor_degree 62 (-1);
  Words.set_constructor_degree 63 (-3);
  Words.set_constructor_degree 64 (-3);
  Words.set_constructor_degree 65 (-4);
  Words.set_constructor_degree 66 (-1);
  Words.set_constructor_degree 67 (-3);
  Words.set_constructor_degree 68 (-1);
  Words.set_constructor_degree 69 (-1);
  Words.set_constructor_degree 70 (-1);
  Words.set_constructor_degree 71 (-1);
  Words.set_constructor_degree 72 (-1);
  Words.set_constructor_degree 73 (-1);
  Words.set_constructor_degree 74 (-1);
  Words.set_constructor_degree 75 (-1);
  Words.set_constructor_degree 76 (-4);
  Words.set_constructor_degree 77 (-2);
  Words.set_constructor_degree 78 (-4);
  Words.set_constructor_degree 79 (-4);
  Words.set_constructor_degree 80 (-1);
  Words.set_constructor_degree 81 0;
  Words.set_constructor_degree 82 0;
  Words.set_constructor_degree 83 0;
  Words.set_constructor_degree 84 (-1);
  Words.set_constructor_degree 85 (-2);
  Words.set_constructor_degree 86 (-2);
  Words.set_constructor_degree 87 (-1);
  Words.set_constructor_degree 88 (-1);
  Words.set_constructor_degree 89 (-1);
  Words.set_constructor_degree 90 (-1);
  Words.set_constructor_degree 91 (-2);
  Words.set_constructor_degree 92 (-1);
  Words.set_constructor_degree 93 (-1);
  Words.set_constructor_degree 94 0;
  Words.set_constructor_degree 95 (-1);
  Words.set_constructor_degree 96 (-2);
  Words.set_constructor_degree 97 (-2);
  Words.set_constructor_degree 98 (-2);
  Words.set_constructor_degree 99 (-1);
  Words.set_constructor_degree 100 (-2);
  Words.set_constructor_degree 101 (-1);
  Words.set_constructor_degree 102 (-5);
  Words.set_constructor_degree 103 0;
  Words.set_constructor_degree 104 (-1);
  Words.set_constructor_degree 105 (-1);
  Words.set_constructor_degree 106 0;
  Words.set_constructor_degree 107 0;
  Words.set_constructor_degree 108 (-3);
  Words.set_constructor_degree 109 (-3);
  Words.set_constructor_degree 110 (-1);
  Words.set_constructor_degree 111 (-2);
  Words.set_constructor_degree 112 (-1);
  Words.set_constructor_degree 113 (-2);
  Words.set_constructor_degree 114 (-1);
  Words.set_constructor_degree 115 (-2);
  Words.set_constructor_degree 116 (-2);
  Words.set_constructor_degree 117 (-3);
  Words.set_constructor_degree 118 (-1);
  Words.set_constructor_degree 119 (-1);
  Words.set_constructor_degree 120 (-1);
  Words.set_constructor_degree 121 (-2);
  Words.set_constructor_degree 122 (-2);
  Words.set_constructor_degree 123 (-1);
  Words.set_constructor_degree 124 (-2);
  Words.set_constructor_degree 125 (-2);
  Words.set_constructor_degree 126 (-1);
  Words.set_constructor_degree 127 (-3);
  Words.set_constructor_degree 128 (-3);
  Words.set_constructor_degree 129 0;
  Words.set_constructor_degree 130 0;
  Words.set_constructor_degree 131 0;
  Words.set_constructor_degree 132 0
