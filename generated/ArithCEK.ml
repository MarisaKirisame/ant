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

let rec reverse_exprs_aux memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 38)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec reverse_exprs memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 40)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec flatten_add memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 41)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec flatten_mul memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 45)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_coeff memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 47)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_base memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 50)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_total_coeff memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 53)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_bases memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 55)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec build_mul memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 57)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize_mul_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 60)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec combine_like_terms_acc memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 61)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec combine_like_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 65)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec factor_adjacent memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 67)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec pick_factored memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 70)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 72)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec build_add memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 74)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_round memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 77)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize_add_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 78)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_opt memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 79)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 82)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec simplify_aux memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 84)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec diffx memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 85)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 88)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec main memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 91)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_91 ->
      assert_env_length w_91 1;
      let hd_0, tl_0 = resolve w_91 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_91
      | 15 (* tag_cont_1 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          let keep_37 = env_call w_91 [ 0; 1; 2 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_37; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 3)
      | 16 (* tag_cont_2 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_38 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_38; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 1)
      | 17 (* tag_cont_3 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_39 = env_call w_91 [ 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_39; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 18 (* tag_cont_4 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_40 = env_call w_91 [ 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_40; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 19 (* tag_cont_5 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 92)
      | 20 (* tag_cont_6 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 93)
      | 21 (* tag_cont_7 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          let keep_43 = env_call w_91 [ 0; 1; 2 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_43; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 13)
      | 22 (* tag_cont_8 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 95)
      | 23 (* tag_cont_9 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 4);
          w_91.state.c <- pc_to_exp (int_to_pc 96)
      | 24 (* tag_cont_10 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_32 = pop_env w_91 in
          let ctor_arg_33 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_33; ctor_arg_32 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 25 (* tag_cont_11 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          assert_env_length w_91 5;
          push_env w_91 (Memo.from_int 0);
          w_91.state.c <- pc_to_exp (int_to_pc 98)
      | 26 (* tag_cont_12 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 34)
      | 27 (* tag_cont_13 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_47 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_50; keep_47; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 41)
      | 28 (* tag_cont_14 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_48 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; keep_48; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 45)
      | 29 (* tag_cont_15 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_49 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_52; keep_49; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 53)
      | 30 (* tag_cont_16 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          w_91.state.c <- pc_to_exp (int_to_pc 101)
      | 31 (* tag_cont_17 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_36 = pop_env w_91 in
          let ctor_arg_37 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_37; ctor_arg_36 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 32 (* tag_cont_18 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_52 = env_call w_91 [ 0 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; keep_52; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 53)
      | 33 (* tag_cont_19 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_53 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_56; keep_53; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 45)
      | 34 (* tag_cont_20 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Memo.from_constructor tag_ENil);
          assert_env_length w_91 2;
          let ctor_arg_38 = pop_env w_91 in
          let ctor_arg_39 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_39; ctor_arg_38 ]);
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 35 (* tag_cont_21 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 6;
          let keep_54 = env_call w_91 [ 0; 1; 3; 4 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; keep_54; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 23)
      | 36 (* tag_cont_22 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 4;
          let keep_55 = env_call w_91 [ 1; 2 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_58; keep_55; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 23)
      | 37 (* tag_cont_23 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 4);
          assert_env_length w_91 6;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 7;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 8;
          let ctor_arg_40 = pop_env w_91 in
          let ctor_arg_41 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_41; ctor_arg_40 ]);
          assert_env_length w_91 7;
          let keep_56 = env_call w_91 [ 0; 1; 3; 4 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_59; keep_56; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 38 (* tag_cont_24 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 3);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 6;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 7;
          let ctor_arg_42 = pop_env w_91 in
          let ctor_arg_43 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_43; ctor_arg_42 ]);
          assert_env_length w_91 6;
          let keep_57 = env_call w_91 [ 0; 1; 2; 3 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_60; keep_57; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 39 (* tag_cont_25 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 102)
      | 40 (* tag_cont_26 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_44 = pop_env w_91 in
          let ctor_arg_45 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_45; ctor_arg_44 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 41 (* tag_cont_27 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_60 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_63; keep_60; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 72)
      | 42 (* tag_cont_28 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_61 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_64; keep_61; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 36)
      | 43 (* tag_cont_29 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_62 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_65; keep_62; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 41)
      | 44 (* tag_cont_30 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_63 = env_call w_91 [ 0; 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_66; keep_63; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 79)
      | 45 (* tag_cont_31 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_64 = env_call w_91 [ 0; 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_67; keep_64; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 79)
      | 46 (* tag_cont_32 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_65 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_68; keep_65; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 82)
      | 47 (* tag_cont_33 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_66 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_69; keep_66; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 82)
      | 48 (* tag_cont_34 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Memo.from_constructor tag_Z);
          assert_env_length w_91 3;
          let ctor_arg_46 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_46 ]);
          assert_env_length w_91 3;
          let ctor_arg_47 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_47 ]);
          assert_env_length w_91 3;
          let ctor_arg_48 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_48 ]);
          assert_env_length w_91 3;
          let ctor_arg_49 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_49 ]);
          assert_env_length w_91 3;
          let ctor_arg_50 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_50 ]);
          assert_env_length w_91 3;
          let ctor_arg_51 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_51 ]);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          let keep_67 = env_call w_91 [ 0 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_70; keep_67; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 79)
      | 49 (* tag_cont_35 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_68 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_71; keep_68; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 85)
      | 50 (* tag_cont_36 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          let ctor_arg_52 = pop_env w_91 in
          let ctor_arg_53 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_53; ctor_arg_52 ]);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_69 = env_call w_91 [ 2; 3 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_72; keep_69; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 85)
      | 51 (* tag_cont_37 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 6;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 7;
          let keep_70 = env_call w_91 [ 3 ] 3 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_73; keep_70; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 88)
      | 52 (* tag_cont_38 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 6;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 7;
          let keep_71 = env_call w_91 [ 3 ] 3 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_74; keep_71; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 88)
      | 53 (* tag_cont_39 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_72 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_75; keep_72; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 85)
      | 54 (* tag_cont_40 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 3);
          w_91.state.c <- pc_to_exp (int_to_pc 117)
      | 55 (* tag_cont_41 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 118)
      | 56 (* tag_cont_42 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 119)
      | 57 (* tag_cont_43 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 120)
      | 58 (* tag_cont_44 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 121)
      | 59 (* tag_cont_45 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 122)
      | 60 (* tag_cont_46 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 3);
          w_91.state.c <- pc_to_exp (int_to_pc 126)
      | 61 (* tag_cont_47 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 3);
          w_91.state.c <- pc_to_exp (int_to_pc 127)
      | 62 (* tag_cont_48 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 4);
          w_91.state.c <- pc_to_exp (int_to_pc 128)
      | 63 (* tag_cont_49 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_63 = pop_env w_91 in
          let ctor_arg_64 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_64; ctor_arg_63 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 64 (* tag_cont_50 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 32)
      | 65 (* tag_cont_51 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 32)
      | 66 (* tag_cont_52 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 129)
      | 67 (* tag_cont_53 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 34)
      | 68 (* tag_cont_54 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 34)
      | 69 (* tag_cont_55 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 3;
          push_env w_91 (Memo.from_int 0);
          w_91.state.c <- pc_to_exp (int_to_pc 131)
      | 70 (* tag_cont_56 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let keep_79 = env_call w_91 [] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_79; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 32)
      | 71 (* tag_cont_57 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 6;
          push_env w_91 (Dynarray.get w_91.state.e 3);
          assert_env_length w_91 7;
          let keep_80 = env_call w_91 [ 0; 1; 2; 3; 4 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_82; keep_80; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 72 (* tag_cont_58 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 6;
          ignore (env_call w_91 [] 3);
          w_91.state.c <- pc_to_exp (int_to_pc 61)
      | 73 (* tag_cont_59 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 132)
      | 74 (* tag_cont_60 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 4 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 133)
      | 75 (* tag_cont_61 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_68 = pop_env w_91 in
          let ctor_arg_69 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_69; ctor_arg_68 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 76 (* tag_cont_62 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          ignore (env_call w_91 [] 1);
          w_91.state.c <- pc_to_exp (int_to_pc 72)
      | 77 (* tag_cont_63 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_84 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_86; keep_84; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 67)
      | 78 (* tag_cont_64 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_85 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_87; keep_85; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 65)
      | 79 (* tag_cont_65 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let keep_86 = env_call w_91 [] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_86; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 32)
      | 80 (* tag_cont_66 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          let keep_87 = env_call w_91 [ 0; 1; 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_88; keep_87; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 78)
      | 81 (* tag_cont_67 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          let keep_88 = env_call w_91 [ 0; 1; 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_89; keep_88; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 60)
      | 82 (* tag_cont_68 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 78)
      | 83 (* tag_cont_69 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 60)
      | 84 (* tag_cont_70 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 3;
          let keep_89 = env_call w_91 [ 0 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_90; keep_89; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 82)
      | 85 (* tag_cont_71 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_70 = pop_env w_91 in
          let ctor_arg_71 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_71; ctor_arg_70 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 86 (* tag_cont_72 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          let ctor_arg_72 = pop_env w_91 in
          let ctor_arg_73 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_73; ctor_arg_72 ]);
          assert_env_length w_91 2;
          let ctor_arg_74 = pop_env w_91 in
          let ctor_arg_75 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_75; ctor_arg_74 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 87 (* tag_cont_73 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 134)
      | 88 (* tag_cont_74 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 135)
      | 89 (* tag_cont_75 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 1);
          w_91.state.c <- pc_to_exp (int_to_pc 84)
      | 90 (* tag_cont_76 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_90 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_91; keep_90; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 1)
      | 91 (* tag_cont_77 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          push_env w_91 (Memo.from_int 0);
          w_91.state.c <- pc_to_exp (int_to_pc 137)
      | 92 (* tag_cont_78 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          push_env w_91 (Memo.from_int 0);
          w_91.state.c <- pc_to_exp (int_to_pc 139)
      | 93 (* tag_cont_79 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Memo.from_int 0);
          w_91.state.c <- pc_to_exp (int_to_pc 141)
      | 94 (* tag_cont_80 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          w_91.state.c <- pc_to_exp (int_to_pc 142)
      | 95 (* tag_cont_81 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 3;
          let keep_91 = env_call w_91 [ 0 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_92; keep_91; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 57)
      | 96 (* tag_cont_82 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 5 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 144)
      | 97 (* tag_cont_83 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          ignore (env_call w_91 [] 1);
          w_91.state.c <- pc_to_exp (int_to_pc 67)
      | 98 (* tag_cont_84 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let ctor_arg_79 = pop_env w_91 in
          let ctor_arg_80 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_80; ctor_arg_79 ]);
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          drop_n w_91 1 0;
          assert_env_length w_91 1;
          return_n w_91 1 (pc_to_exp (int_to_pc 0))
      | 99 (* tag_cont_85 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 145)
      | 100 (* tag_cont_86 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_93 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_94; keep_93; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 72)
      | 101 (* tag_cont_87 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_94 = env_call w_91 [ 0 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_95; keep_94; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 77)
      | 102 (* tag_cont_88 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 6;
          let keep_95 = env_call w_91 [ 0; 3 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_96; keep_95; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 78)
      | 103 (* tag_cont_89 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 6;
          let keep_96 = env_call w_91 [ 0; 3 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_97; keep_96; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 60)
      | 104 (* tag_cont_90 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          let keep_97 = env_call w_91 [ 1 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_98; keep_97; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 105 (* tag_cont_91 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          w_91.state.c <- pc_to_exp (int_to_pc 150)
      | 106 (* tag_cont_92 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          push_env w_91 (Memo.from_int 1);
          w_91.state.c <- pc_to_exp (int_to_pc 152)
      | 107 (* tag_cont_93 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          push_env w_91 (Memo.from_int 0);
          w_91.state.c <- pc_to_exp (int_to_pc 154)
      | 108 (* tag_cont_94 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_99 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_100; keep_99; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 67)
      | 109 (* tag_cont_95 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_100 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_102; keep_100; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 40)
      | 110 (* tag_cont_96 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          let keep_101 = env_call w_91 [ 0; 1 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_103; keep_101; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 78)
      | 111 (* tag_cont_97 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 5;
          let keep_102 = env_call w_91 [ 0; 1 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_104; keep_102; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 60)
      | 112 (* tag_cont_98 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 155)
      | 113 (* tag_cont_99 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          ignore (env_call w_91 [] 2);
          w_91.state.c <- pc_to_exp (int_to_pc 34)
      | 114 (* tag_cont_100 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_103 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_105; keep_103; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 72)
      | 115 (* tag_cont_101 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_104 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_106; keep_104; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 74)
      | 116 (* tag_cont_102 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          let keep_105 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_101; keep_105; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 77)
      | 117 (* tag_cont_103 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_106 = env_call w_91 [ 0; 1 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_107; keep_106; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 78)
      | 118 (* tag_cont_104 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_107 = env_call w_91 [ 0; 1 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_108; keep_107; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 60)
      | 119 (* tag_cont_105 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_108 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_109; keep_108; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 67)
      | 120 (* tag_cont_106 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          let keep_109 = env_call w_91 [ 1 ] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_110; keep_109; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 74)
      | 121 (* tag_cont_107 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_110 = env_call w_91 [ 0; 1; 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_111; keep_110; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 122 (* tag_cont_108 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 2 tl_0;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 5;
          let keep_111 = env_call w_91 [ 0; 1; 2 ] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_112; keep_111; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 6)
      | 123 (* tag_cont_109 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 1);
          w_91.state.c <- pc_to_exp (int_to_pc 72)
      | 124 (* tag_cont_110 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 1 tl_0;
          assert_env_length w_91 2;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 1);
          assert_env_length w_91 4;
          let keep_112 = env_call w_91 [] 2 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_113; keep_112; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 15)
      | 125 (* tag_cont_111 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 156)
      | 126 (* tag_cont_112 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 3 tl_0;
          w_91.state.c <- pc_to_exp (int_to_pc 157)
      | 127 (* tag_cont_113 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          let keep_113 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_116; keep_113; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 41)
      | 128 (* tag_cont_114 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          push_env w_91 (Dynarray.get w_91.state.e 0);
          assert_env_length w_91 2;
          ignore (env_call w_91 [] 1);
          w_91.state.c <- pc_to_exp (int_to_pc 74)
      | 129 (* tag_cont_115 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          let keep_114 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_114; keep_114; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 65)
      | 130 (* tag_cont_116 *) ->
          w_91.state.k <- get_next_cont tl_0;
          restore_env w_91 0 tl_0;
          assert_env_length w_91 1;
          let keep_115 = env_call w_91 [] 1 in
          w_91.state.k <- Memo.appends [ Memo.from_constructor tag_cont_115; keep_115; w_91.state.k ];
          w_91.state.c <- pc_to_exp (int_to_pc 36)
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
      w_37.state.c <- pc_to_exp (int_to_pc 39))
    38;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 3;
      let last_17 = Source.E 2 in
      let x_17 = resolve w_38 last_17 in
      match Word.get_value (fst x_17) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_38);
          assert_env_length w_38 2;
          push_env w_38 (Dynarray.get w_38.state.e 1);
          assert_env_length w_38 3;
          return_n w_38 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_27 = Memo.splits (snd x_17) in
          let split0_27 = List.nth splits_27 0 in
          let split1_14 = List.nth splits_27 1 in
          ignore (pop_env w_38);
          push_env w_38 split0_27;
          push_env w_38 split1_14;
          assert_env_length w_38 4;
          push_env w_38 (Dynarray.get w_38.state.e 3);
          assert_env_length w_38 5;
          push_env w_38 (Dynarray.get w_38.state.e 2);
          assert_env_length w_38 6;
          push_env w_38 (Dynarray.get w_38.state.e 1);
          assert_env_length w_38 7;
          let ctor_arg_10 = pop_env w_38 in
          let ctor_arg_11 = pop_env w_38 in
          push_env w_38 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_11; ctor_arg_10 ]);
          assert_env_length w_38 6;
          ignore (env_call w_38 [] 2);
          w_38.state.c <- pc_to_exp (int_to_pc 38)
      | _ -> failwith "unreachable (39)")
    39;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 1;
      push_env w_39 (Dynarray.get w_39.state.e 0);
      assert_env_length w_39 2;
      push_env w_39 (Memo.from_constructor tag_ENil);
      assert_env_length w_39 3;
      ignore (env_call w_39 [] 2);
      w_39.state.c <- pc_to_exp (int_to_pc 38))
    40;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 1;
      push_env w_40 (Dynarray.get w_40.state.e 0);
      w_40.state.c <- pc_to_exp (int_to_pc 44))
    41;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 3;
      let cond_2 = resolve w_43 (Source.E 2) in
      ignore (pop_env w_43);
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_43 2;
        push_env w_43 (Memo.from_constructor tag_ENil);
        assert_env_length w_43 3;
        drop_n w_43 3 1;
        assert_env_length w_43 2;
        return_n w_43 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_43 2;
        push_env w_43 (Dynarray.get w_43.state.e 0);
        assert_env_length w_43 3;
        push_env w_43 (Memo.from_constructor tag_ENil);
        assert_env_length w_43 4;
        let ctor_arg_12 = pop_env w_43 in
        let ctor_arg_13 = pop_env w_43 in
        push_env w_43 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_13; ctor_arg_12 ]);
        assert_env_length w_43 3;
        drop_n w_43 3 1;
        assert_env_length w_43 2;
        return_n w_43 2 (pc_to_exp (int_to_pc 0))))
    42;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 4;
      let x0_4 = resolve w_42 (Source.E 2) in
      let x1_4 = resolve w_42 (Source.E 3) in
      ignore (pop_env w_42);
      ignore (pop_env w_42);
      push_env w_42 (Memo.from_int (if Word.get_value (fst x0_4) = Word.get_value (fst x1_4) then 1 else 0));
      w_42.state.c <- pc_to_exp (int_to_pc 42))
    43;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 2;
      let last_18 = Source.E 1 in
      let x_18 = resolve w_41 last_18 in
      match Word.get_value (fst x_18) with
      | 7 (* tag_Add *) ->
          let splits_28 = Memo.splits (snd x_18) in
          let split0_28 = List.nth splits_28 0 in
          let split1_15 = List.nth splits_28 1 in
          ignore (pop_env w_41);
          push_env w_41 split0_28;
          push_env w_41 split1_15;
          assert_env_length w_41 3;
          push_env w_41 (Dynarray.get w_41.state.e 1);
          assert_env_length w_41 4;
          let keep_12 = env_call w_41 [ 2 ] 1 in
          w_41.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_41.state.k ];
          w_41.state.c <- pc_to_exp (int_to_pc 41)
      | 5 (* tag_Const *) ->
          let splits_29 = Memo.splits (snd x_18) in
          let split0_29 = List.nth splits_29 0 in
          ignore (pop_env w_41);
          push_env w_41 split0_29;
          assert_env_length w_41 2;
          push_env w_41 (Dynarray.get w_41.state.e 1);
          assert_env_length w_41 3;
          push_env w_41 (Memo.from_int 0);
          w_41.state.c <- pc_to_exp (int_to_pc 43)
      | _ ->
          ignore (pop_env w_41);
          assert_env_length w_41 1;
          push_env w_41 (Dynarray.get w_41.state.e 0);
          assert_env_length w_41 2;
          push_env w_41 (Memo.from_constructor tag_ENil);
          assert_env_length w_41 3;
          let ctor_arg_14 = pop_env w_41 in
          let ctor_arg_15 = pop_env w_41 in
          push_env w_41 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_15; ctor_arg_14 ]);
          assert_env_length w_41 2;
          return_n w_41 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (44)")
    44;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 1;
      push_env w_44 (Dynarray.get w_44.state.e 0);
      w_44.state.c <- pc_to_exp (int_to_pc 46))
    45;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 2;
      let last_19 = Source.E 1 in
      let x_19 = resolve w_45 last_19 in
      match Word.get_value (fst x_19) with
      | 8 (* tag_Mul *) ->
          let splits_30 = Memo.splits (snd x_19) in
          let split0_30 = List.nth splits_30 0 in
          let split1_16 = List.nth splits_30 1 in
          ignore (pop_env w_45);
          push_env w_45 split0_30;
          push_env w_45 split1_16;
          assert_env_length w_45 3;
          push_env w_45 (Dynarray.get w_45.state.e 1);
          assert_env_length w_45 4;
          let keep_13 = env_call w_45 [ 2 ] 1 in
          w_45.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_45.state.k ];
          w_45.state.c <- pc_to_exp (int_to_pc 45)
      | _ ->
          ignore (pop_env w_45);
          assert_env_length w_45 1;
          push_env w_45 (Dynarray.get w_45.state.e 0);
          assert_env_length w_45 2;
          push_env w_45 (Memo.from_constructor tag_ENil);
          assert_env_length w_45 3;
          let ctor_arg_16 = pop_env w_45 in
          let ctor_arg_17 = pop_env w_45 in
          push_env w_45 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_17; ctor_arg_16 ]);
          assert_env_length w_45 2;
          return_n w_45 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (46)")
    46;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 1;
      push_env w_46 (Dynarray.get w_46.state.e 0);
      w_46.state.c <- pc_to_exp (int_to_pc 49))
    47;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 4;
      let last_21 = Source.E 3 in
      let x_21 = resolve w_48 last_21 in
      match Word.get_value (fst x_21) with
      | 5 (* tag_Const *) ->
          let splits_33 = Memo.splits (snd x_21) in
          let split0_33 = List.nth splits_33 0 in
          ignore (pop_env w_48);
          push_env w_48 split0_33;
          assert_env_length w_48 4;
          push_env w_48 (Dynarray.get w_48.state.e 3);
          assert_env_length w_48 5;
          drop_n w_48 5 1;
          assert_env_length w_48 4;
          drop_n w_48 4 2;
          assert_env_length w_48 2;
          return_n w_48 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_48);
          assert_env_length w_48 3;
          push_env w_48 (Memo.from_int 1);
          assert_env_length w_48 4;
          drop_n w_48 4 2;
          assert_env_length w_48 2;
          return_n w_48 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (48)")
    48;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 2;
      let last_20 = Source.E 1 in
      let x_20 = resolve w_47 last_20 in
      match Word.get_value (fst x_20) with
      | 5 (* tag_Const *) ->
          let splits_31 = Memo.splits (snd x_20) in
          let split0_31 = List.nth splits_31 0 in
          ignore (pop_env w_47);
          push_env w_47 split0_31;
          assert_env_length w_47 2;
          push_env w_47 (Dynarray.get w_47.state.e 1);
          assert_env_length w_47 3;
          drop_n w_47 3 1;
          assert_env_length w_47 2;
          return_n w_47 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_32 = Memo.splits (snd x_20) in
          let split0_32 = List.nth splits_32 0 in
          let split1_17 = List.nth splits_32 1 in
          ignore (pop_env w_47);
          push_env w_47 split0_32;
          push_env w_47 split1_17;
          assert_env_length w_47 3;
          push_env w_47 (Dynarray.get w_47.state.e 1);
          w_47.state.c <- pc_to_exp (int_to_pc 48)
      | _ ->
          ignore (pop_env w_47);
          assert_env_length w_47 1;
          push_env w_47 (Memo.from_int 1);
          assert_env_length w_47 2;
          return_n w_47 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (49)")
    49;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 1;
      push_env w_49 (Dynarray.get w_49.state.e 0);
      w_49.state.c <- pc_to_exp (int_to_pc 52))
    50;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 4;
      let last_23 = Source.E 3 in
      let x_23 = resolve w_51 last_23 in
      match Word.get_value (fst x_23) with
      | 5 (* tag_Const *) ->
          let splits_36 = Memo.splits (snd x_23) in
          let split0_36 = List.nth splits_36 0 in
          ignore (pop_env w_51);
          push_env w_51 split0_36;
          assert_env_length w_51 4;
          push_env w_51 (Dynarray.get w_51.state.e 2);
          assert_env_length w_51 5;
          drop_n w_51 5 1;
          assert_env_length w_51 4;
          drop_n w_51 4 2;
          assert_env_length w_51 2;
          return_n w_51 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_51);
          assert_env_length w_51 3;
          push_env w_51 (Dynarray.get w_51.state.e 0);
          assert_env_length w_51 4;
          drop_n w_51 4 2;
          assert_env_length w_51 2;
          return_n w_51 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (51)")
    51;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 2;
      let last_22 = Source.E 1 in
      let x_22 = resolve w_50 last_22 in
      match Word.get_value (fst x_22) with
      | 5 (* tag_Const *) ->
          let splits_34 = Memo.splits (snd x_22) in
          let split0_34 = List.nth splits_34 0 in
          ignore (pop_env w_50);
          push_env w_50 split0_34;
          assert_env_length w_50 2;
          push_env w_50 (Memo.from_int 1);
          assert_env_length w_50 3;
          let ctor_arg_18 = pop_env w_50 in
          push_env w_50 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_18 ]);
          assert_env_length w_50 3;
          drop_n w_50 3 1;
          assert_env_length w_50 2;
          return_n w_50 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_35 = Memo.splits (snd x_22) in
          let split0_35 = List.nth splits_35 0 in
          let split1_18 = List.nth splits_35 1 in
          ignore (pop_env w_50);
          push_env w_50 split0_35;
          push_env w_50 split1_18;
          assert_env_length w_50 3;
          push_env w_50 (Dynarray.get w_50.state.e 1);
          w_50.state.c <- pc_to_exp (int_to_pc 51)
      | _ ->
          ignore (pop_env w_50);
          assert_env_length w_50 1;
          push_env w_50 (Dynarray.get w_50.state.e 0);
          assert_env_length w_50 2;
          return_n w_50 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (52)")
    52;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 1;
      push_env w_52 (Dynarray.get w_52.state.e 0);
      w_52.state.c <- pc_to_exp (int_to_pc 54))
    53;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 2;
      let last_24 = Source.E 1 in
      let x_24 = resolve w_53 last_24 in
      match Word.get_value (fst x_24) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_53);
          assert_env_length w_53 1;
          push_env w_53 (Memo.from_int 1);
          assert_env_length w_53 2;
          return_n w_53 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_37 = Memo.splits (snd x_24) in
          let split0_37 = List.nth splits_37 0 in
          let split1_19 = List.nth splits_37 1 in
          ignore (pop_env w_53);
          push_env w_53 split0_37;
          push_env w_53 split1_19;
          assert_env_length w_53 3;
          push_env w_53 (Dynarray.get w_53.state.e 1);
          assert_env_length w_53 4;
          let keep_14 = env_call w_53 [ 2 ] 1 in
          w_53.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_53.state.k ];
          w_53.state.c <- pc_to_exp (int_to_pc 47)
      | _ -> failwith "unreachable (54)")
    54;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 1;
      push_env w_54 (Dynarray.get w_54.state.e 0);
      w_54.state.c <- pc_to_exp (int_to_pc 56))
    55;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 2;
      let last_25 = Source.E 1 in
      let x_25 = resolve w_55 last_25 in
      match Word.get_value (fst x_25) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_55);
          assert_env_length w_55 1;
          push_env w_55 (Memo.from_constructor tag_ENil);
          assert_env_length w_55 2;
          return_n w_55 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_38 = Memo.splits (snd x_25) in
          let split0_38 = List.nth splits_38 0 in
          let split1_20 = List.nth splits_38 1 in
          ignore (pop_env w_55);
          push_env w_55 split0_38;
          push_env w_55 split1_20;
          assert_env_length w_55 3;
          push_env w_55 (Dynarray.get w_55.state.e 1);
          assert_env_length w_55 4;
          let keep_15 = env_call w_55 [ 2 ] 1 in
          w_55.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_55.state.k ];
          w_55.state.c <- pc_to_exp (int_to_pc 50)
      | _ -> failwith "unreachable (56)")
    56;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 1;
      push_env w_56 (Dynarray.get w_56.state.e 0);
      w_56.state.c <- pc_to_exp (int_to_pc 59))
    57;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 4;
      let last_27 = Source.E 3 in
      let x_27 = resolve w_58 last_27 in
      match Word.get_value (fst x_27) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_58);
          assert_env_length w_58 3;
          push_env w_58 (Dynarray.get w_58.state.e 1);
          assert_env_length w_58 4;
          drop_n w_58 4 2;
          assert_env_length w_58 2;
          return_n w_58 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_58);
          assert_env_length w_58 3;
          push_env w_58 (Dynarray.get w_58.state.e 1);
          assert_env_length w_58 4;
          push_env w_58 (Dynarray.get w_58.state.e 2);
          assert_env_length w_58 5;
          let keep_16 = env_call w_58 [ 3 ] 1 in
          w_58.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_58.state.k ];
          w_58.state.c <- pc_to_exp (int_to_pc 57)
      | _ -> failwith "unreachable (58)")
    58;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 2;
      let last_26 = Source.E 1 in
      let x_26 = resolve w_57 last_26 in
      match Word.get_value (fst x_26) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_57);
          assert_env_length w_57 1;
          push_env w_57 (Memo.from_int 1);
          assert_env_length w_57 2;
          let ctor_arg_19 = pop_env w_57 in
          push_env w_57 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_19 ]);
          assert_env_length w_57 2;
          return_n w_57 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_39 = Memo.splits (snd x_26) in
          let split0_39 = List.nth splits_39 0 in
          let split1_21 = List.nth splits_39 1 in
          ignore (pop_env w_57);
          push_env w_57 split0_39;
          push_env w_57 split1_21;
          assert_env_length w_57 3;
          push_env w_57 (Dynarray.get w_57.state.e 2);
          w_57.state.c <- pc_to_exp (int_to_pc 58)
      | _ -> failwith "unreachable (59)")
    59;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 2;
      push_env w_59 (Dynarray.get w_59.state.e 0);
      assert_env_length w_59 3;
      let keep_17 = env_call w_59 [ 1 ] 1 in
      w_59.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_17; w_59.state.k ];
      w_59.state.c <- pc_to_exp (int_to_pc 45))
    60;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 3;
      push_env w_60 (Dynarray.get w_60.state.e 2);
      w_60.state.c <- pc_to_exp (int_to_pc 64))
    61;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 4;
      let cond_3 = resolve w_63 (Source.E 3) in
      ignore (pop_env w_63);
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_63 3;
        push_env w_63 (Memo.from_constructor tag_ENil);
        assert_env_length w_63 4;
        return_n w_63 4 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_63 3;
        push_env w_63 (Dynarray.get w_63.state.e 1);
        assert_env_length w_63 4;
        push_env w_63 (Dynarray.get w_63.state.e 0);
        assert_env_length w_63 5;
        let keep_18 = env_call w_63 [] 2 in
        w_63.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_18; w_63.state.k ];
        w_63.state.c <- pc_to_exp (int_to_pc 16)))
    62;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 5;
      let x0_5 = resolve w_62 (Source.E 3) in
      let x1_5 = resolve w_62 (Source.E 4) in
      ignore (pop_env w_62);
      ignore (pop_env w_62);
      push_env w_62 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      w_62.state.c <- pc_to_exp (int_to_pc 62))
    63;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 4;
      let last_28 = Source.E 3 in
      let x_28 = resolve w_61 last_28 in
      match Word.get_value (fst x_28) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_61);
          assert_env_length w_61 3;
          push_env w_61 (Dynarray.get w_61.state.e 1);
          assert_env_length w_61 4;
          push_env w_61 (Memo.from_int 0);
          w_61.state.c <- pc_to_exp (int_to_pc 63)
      | 12 (* tag_ECons *) ->
          let splits_40 = Memo.splits (snd x_28) in
          let split0_40 = List.nth splits_40 0 in
          let split1_22 = List.nth splits_40 1 in
          ignore (pop_env w_61);
          push_env w_61 split0_40;
          push_env w_61 split1_22;
          assert_env_length w_61 5;
          push_env w_61 (Dynarray.get w_61.state.e 3);
          assert_env_length w_61 6;
          let keep_19 = env_call w_61 [ 0; 1; 3; 4 ] 1 in
          w_61.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_19; w_61.state.k ];
          w_61.state.c <- pc_to_exp (int_to_pc 26)
      | _ -> failwith "unreachable (64)")
    64;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 1;
      push_env w_64 (Dynarray.get w_64.state.e 0);
      w_64.state.c <- pc_to_exp (int_to_pc 66))
    65;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 2;
      let last_29 = Source.E 1 in
      let x_29 = resolve w_65 last_29 in
      match Word.get_value (fst x_29) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_65);
          assert_env_length w_65 1;
          push_env w_65 (Memo.from_constructor tag_ENil);
          assert_env_length w_65 2;
          return_n w_65 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_41 = Memo.splits (snd x_29) in
          let split0_41 = List.nth splits_41 0 in
          let split1_23 = List.nth splits_41 1 in
          ignore (pop_env w_65);
          push_env w_65 split0_41;
          push_env w_65 split1_23;
          assert_env_length w_65 3;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          assert_env_length w_65 4;
          let keep_20 = env_call w_65 [ 1; 2 ] 1 in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_20; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 26)
      | _ -> failwith "unreachable (66)")
    66;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 1;
      push_env w_66 (Dynarray.get w_66.state.e 0);
      w_66.state.c <- pc_to_exp (int_to_pc 69))
    67;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 4;
      let last_31 = Source.E 3 in
      let x_31 = resolve w_68 last_31 in
      match Word.get_value (fst x_31) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_68);
          assert_env_length w_68 3;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          assert_env_length w_68 4;
          push_env w_68 (Memo.from_constructor tag_ENil);
          assert_env_length w_68 5;
          let ctor_arg_20 = pop_env w_68 in
          let ctor_arg_21 = pop_env w_68 in
          push_env w_68 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_21; ctor_arg_20 ]);
          assert_env_length w_68 4;
          drop_n w_68 4 2;
          assert_env_length w_68 2;
          return_n w_68 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_43 = Memo.splits (snd x_31) in
          let split0_43 = List.nth splits_43 0 in
          let split1_25 = List.nth splits_43 1 in
          ignore (pop_env w_68);
          push_env w_68 split0_43;
          push_env w_68 split1_25;
          assert_env_length w_68 5;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          assert_env_length w_68 6;
          push_env w_68 (Dynarray.get w_68.state.e 3);
          assert_env_length w_68 7;
          let keep_21 = env_call w_68 [ 1; 2; 3; 4 ] 2 in
          w_68.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_21; w_68.state.k ];
          w_68.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (68)")
    68;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 2;
      let last_30 = Source.E 1 in
      let x_30 = resolve w_67 last_30 in
      match Word.get_value (fst x_30) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_67);
          assert_env_length w_67 1;
          push_env w_67 (Memo.from_constructor tag_ENil);
          assert_env_length w_67 2;
          return_n w_67 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_42 = Memo.splits (snd x_30) in
          let split0_42 = List.nth splits_42 0 in
          let split1_24 = List.nth splits_42 1 in
          ignore (pop_env w_67);
          push_env w_67 split0_42;
          push_env w_67 split1_24;
          assert_env_length w_67 3;
          push_env w_67 (Dynarray.get w_67.state.e 2);
          w_67.state.c <- pc_to_exp (int_to_pc 68)
      | _ -> failwith "unreachable (69)")
    69;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 2;
      push_env w_69 (Dynarray.get w_69.state.e 1);
      w_69.state.c <- pc_to_exp (int_to_pc 71))
    70;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 3;
      let last_32 = Source.E 2 in
      let x_32 = resolve w_70 last_32 in
      match Word.get_value (fst x_32) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_70);
          assert_env_length w_70 2;
          push_env w_70 (Memo.from_constructor tag_NoPick);
          assert_env_length w_70 3;
          return_n w_70 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_44 = Memo.splits (snd x_32) in
          let split0_44 = List.nth splits_44 0 in
          let split1_26 = List.nth splits_44 1 in
          ignore (pop_env w_70);
          push_env w_70 split0_44;
          push_env w_70 split1_26;
          assert_env_length w_70 4;
          push_env w_70 (Dynarray.get w_70.state.e 0);
          assert_env_length w_70 5;
          push_env w_70 (Dynarray.get w_70.state.e 2);
          assert_env_length w_70 6;
          let keep_22 = env_call w_70 [ 0; 2; 3 ] 2 in
          w_70.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_22; w_70.state.k ];
          w_70.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (71)")
    71;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 1;
      push_env w_71 (Dynarray.get w_71.state.e 0);
      w_71.state.c <- pc_to_exp (int_to_pc 73))
    72;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 2;
      let last_33 = Source.E 1 in
      let x_33 = resolve w_72 last_33 in
      match Word.get_value (fst x_33) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_72);
          assert_env_length w_72 1;
          push_env w_72 (Memo.from_constructor tag_ENil);
          assert_env_length w_72 2;
          return_n w_72 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_45 = Memo.splits (snd x_33) in
          let split0_45 = List.nth splits_45 0 in
          let split1_27 = List.nth splits_45 1 in
          ignore (pop_env w_72);
          push_env w_72 split0_45;
          push_env w_72 split1_27;
          assert_env_length w_72 3;
          push_env w_72 (Dynarray.get w_72.state.e 1);
          assert_env_length w_72 4;
          push_env w_72 (Dynarray.get w_72.state.e 2);
          assert_env_length w_72 5;
          let keep_23 = env_call w_72 [ 1; 2 ] 2 in
          w_72.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_23; w_72.state.k ];
          w_72.state.c <- pc_to_exp (int_to_pc 70)
      | _ -> failwith "unreachable (73)")
    73;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 1;
      push_env w_73 (Dynarray.get w_73.state.e 0);
      w_73.state.c <- pc_to_exp (int_to_pc 76))
    74;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 4;
      let last_35 = Source.E 3 in
      let x_35 = resolve w_75 last_35 in
      match Word.get_value (fst x_35) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_75);
          assert_env_length w_75 3;
          push_env w_75 (Dynarray.get w_75.state.e 1);
          assert_env_length w_75 4;
          drop_n w_75 4 2;
          assert_env_length w_75 2;
          return_n w_75 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_75);
          assert_env_length w_75 3;
          push_env w_75 (Dynarray.get w_75.state.e 1);
          assert_env_length w_75 4;
          push_env w_75 (Dynarray.get w_75.state.e 2);
          assert_env_length w_75 5;
          let keep_24 = env_call w_75 [ 3 ] 1 in
          w_75.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_24; w_75.state.k ];
          w_75.state.c <- pc_to_exp (int_to_pc 74)
      | _ -> failwith "unreachable (75)")
    75;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 2;
      let last_34 = Source.E 1 in
      let x_34 = resolve w_74 last_34 in
      match Word.get_value (fst x_34) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_74);
          assert_env_length w_74 1;
          push_env w_74 (Memo.from_int 0);
          assert_env_length w_74 2;
          let ctor_arg_22 = pop_env w_74 in
          push_env w_74 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_22 ]);
          assert_env_length w_74 2;
          return_n w_74 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_46 = Memo.splits (snd x_34) in
          let split0_46 = List.nth splits_46 0 in
          let split1_28 = List.nth splits_46 1 in
          ignore (pop_env w_74);
          push_env w_74 split0_46;
          push_env w_74 split1_28;
          assert_env_length w_74 3;
          push_env w_74 (Dynarray.get w_74.state.e 2);
          w_74.state.c <- pc_to_exp (int_to_pc 75)
      | _ -> failwith "unreachable (76)")
    76;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 1;
      push_env w_76 (Dynarray.get w_76.state.e 0);
      assert_env_length w_76 2;
      let keep_25 = env_call w_76 [] 1 in
      w_76.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_25; w_76.state.k ];
      w_76.state.c <- pc_to_exp (int_to_pc 67))
    77;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 2;
      push_env w_77 (Dynarray.get w_77.state.e 0);
      assert_env_length w_77 3;
      let keep_26 = env_call w_77 [ 1 ] 1 in
      w_77.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_26; w_77.state.k ];
      w_77.state.c <- pc_to_exp (int_to_pc 41))
    78;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 2;
      push_env w_78 (Dynarray.get w_78.state.e 0);
      w_78.state.c <- pc_to_exp (int_to_pc 81))
    79;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 4;
      let last_37 = Source.E 3 in
      let x_37 = resolve w_80 last_37 in
      match Word.get_value (fst x_37) with
      | 7 (* tag_Add *) ->
          let splits_48 = Memo.splits (snd x_37) in
          let split0_48 = List.nth splits_48 0 in
          let split1_29 = List.nth splits_48 1 in
          ignore (pop_env w_80);
          push_env w_80 split0_48;
          push_env w_80 split1_29;
          assert_env_length w_80 5;
          push_env w_80 (Dynarray.get w_80.state.e 2);
          assert_env_length w_80 6;
          push_env w_80 (Dynarray.get w_80.state.e 3);
          assert_env_length w_80 7;
          let keep_27 = env_call w_80 [ 2; 4 ] 2 in
          w_80.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_27; w_80.state.k ];
          w_80.state.c <- pc_to_exp (int_to_pc 79)
      | 8 (* tag_Mul *) ->
          let splits_49 = Memo.splits (snd x_37) in
          let split0_49 = List.nth splits_49 0 in
          let split1_30 = List.nth splits_49 1 in
          ignore (pop_env w_80);
          push_env w_80 split0_49;
          push_env w_80 split1_30;
          assert_env_length w_80 5;
          push_env w_80 (Dynarray.get w_80.state.e 2);
          assert_env_length w_80 6;
          push_env w_80 (Dynarray.get w_80.state.e 3);
          assert_env_length w_80 7;
          let keep_28 = env_call w_80 [ 2; 4 ] 2 in
          w_80.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_28; w_80.state.k ];
          w_80.state.c <- pc_to_exp (int_to_pc 79)
      | _ ->
          ignore (pop_env w_80);
          assert_env_length w_80 3;
          push_env w_80 (Dynarray.get w_80.state.e 1);
          assert_env_length w_80 4;
          drop_n w_80 4 1;
          assert_env_length w_80 3;
          return_n w_80 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (80)")
    80;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 3;
      let last_36 = Source.E 2 in
      let x_36 = resolve w_79 last_36 in
      match Word.get_value (fst x_36) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_79);
          assert_env_length w_79 2;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          assert_env_length w_79 3;
          return_n w_79 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_47 = Memo.splits (snd x_36) in
          let split0_47 = List.nth splits_47 0 in
          ignore (pop_env w_79);
          push_env w_79 split0_47;
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          w_79.state.c <- pc_to_exp (int_to_pc 80)
      | _ -> failwith "unreachable (81)")
    81;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 1;
      push_env w_81 (Dynarray.get w_81.state.e 0);
      w_81.state.c <- pc_to_exp (int_to_pc 83))
    82;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 2;
      let last_38 = Source.E 1 in
      let x_38 = resolve w_82 last_38 in
      match Word.get_value (fst x_38) with
      | 5 (* tag_Const *) ->
          let splits_50 = Memo.splits (snd x_38) in
          let split0_50 = List.nth splits_50 0 in
          ignore (pop_env w_82);
          push_env w_82 split0_50;
          assert_env_length w_82 2;
          push_env w_82 (Dynarray.get w_82.state.e 0);
          assert_env_length w_82 3;
          drop_n w_82 3 1;
          assert_env_length w_82 2;
          return_n w_82 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_51 = Memo.splits (snd x_38) in
          let split0_51 = List.nth splits_51 0 in
          ignore (pop_env w_82);
          push_env w_82 split0_51;
          assert_env_length w_82 2;
          push_env w_82 (Dynarray.get w_82.state.e 0);
          assert_env_length w_82 3;
          drop_n w_82 3 1;
          assert_env_length w_82 2;
          return_n w_82 2 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_52 = Memo.splits (snd x_38) in
          let split0_52 = List.nth splits_52 0 in
          let split1_31 = List.nth splits_52 1 in
          ignore (pop_env w_82);
          push_env w_82 split0_52;
          push_env w_82 split1_31;
          assert_env_length w_82 3;
          push_env w_82 (Dynarray.get w_82.state.e 1);
          assert_env_length w_82 4;
          let keep_29 = env_call w_82 [ 2 ] 1 in
          w_82.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_29; w_82.state.k ];
          w_82.state.c <- pc_to_exp (int_to_pc 82)
      | 8 (* tag_Mul *) ->
          let splits_53 = Memo.splits (snd x_38) in
          let split0_53 = List.nth splits_53 0 in
          let split1_32 = List.nth splits_53 1 in
          ignore (pop_env w_82);
          push_env w_82 split0_53;
          push_env w_82 split1_32;
          assert_env_length w_82 3;
          push_env w_82 (Dynarray.get w_82.state.e 1);
          assert_env_length w_82 4;
          let keep_30 = env_call w_82 [ 2 ] 1 in
          w_82.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_30; w_82.state.k ];
          w_82.state.c <- pc_to_exp (int_to_pc 82)
      | _ -> failwith "unreachable (83)")
    83;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 1;
      push_env w_83 (Dynarray.get w_83.state.e 0);
      assert_env_length w_83 2;
      let keep_31 = env_call w_83 [ 0 ] 1 in
      w_83.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_31; w_83.state.k ];
      w_83.state.c <- pc_to_exp (int_to_pc 82))
    84;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 1;
      push_env w_84 (Dynarray.get w_84.state.e 0);
      w_84.state.c <- pc_to_exp (int_to_pc 87))
    85;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 3;
      let last_40 = Source.E 2 in
      let x_40 = resolve w_86 last_40 in
      match Word.get_value (fst x_40) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_86);
          assert_env_length w_86 2;
          push_env w_86 (Memo.from_int 1);
          assert_env_length w_86 3;
          let ctor_arg_24 = pop_env w_86 in
          push_env w_86 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_24 ]);
          assert_env_length w_86 3;
          drop_n w_86 3 1;
          assert_env_length w_86 2;
          return_n w_86 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_86);
          assert_env_length w_86 2;
          push_env w_86 (Memo.from_int 0);
          assert_env_length w_86 3;
          let ctor_arg_25 = pop_env w_86 in
          push_env w_86 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_25 ]);
          assert_env_length w_86 3;
          drop_n w_86 3 1;
          assert_env_length w_86 2;
          return_n w_86 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (86)")
    86;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 2;
      let last_39 = Source.E 1 in
      let x_39 = resolve w_85 last_39 in
      match Word.get_value (fst x_39) with
      | 5 (* tag_Const *) ->
          let splits_54 = Memo.splits (snd x_39) in
          let split0_54 = List.nth splits_54 0 in
          ignore (pop_env w_85);
          push_env w_85 split0_54;
          assert_env_length w_85 2;
          push_env w_85 (Memo.from_int 0);
          assert_env_length w_85 3;
          let ctor_arg_23 = pop_env w_85 in
          push_env w_85 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_23 ]);
          assert_env_length w_85 3;
          drop_n w_85 3 1;
          assert_env_length w_85 2;
          return_n w_85 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_55 = Memo.splits (snd x_39) in
          let split0_55 = List.nth splits_55 0 in
          ignore (pop_env w_85);
          push_env w_85 split0_55;
          assert_env_length w_85 2;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          w_85.state.c <- pc_to_exp (int_to_pc 86)
      | 7 (* tag_Add *) ->
          let splits_56 = Memo.splits (snd x_39) in
          let split0_56 = List.nth splits_56 0 in
          let split1_33 = List.nth splits_56 1 in
          ignore (pop_env w_85);
          push_env w_85 split0_56;
          push_env w_85 split1_33;
          assert_env_length w_85 3;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          assert_env_length w_85 4;
          let keep_32 = env_call w_85 [ 2 ] 1 in
          w_85.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_32; w_85.state.k ];
          w_85.state.c <- pc_to_exp (int_to_pc 85)
      | 8 (* tag_Mul *) ->
          let splits_57 = Memo.splits (snd x_39) in
          let split0_57 = List.nth splits_57 0 in
          let split1_34 = List.nth splits_57 1 in
          ignore (pop_env w_85);
          push_env w_85 split0_57;
          push_env w_85 split1_34;
          assert_env_length w_85 3;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          assert_env_length w_85 4;
          let keep_33 = env_call w_85 [ 1; 2 ] 1 in
          w_85.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_33; w_85.state.k ];
          w_85.state.c <- pc_to_exp (int_to_pc 85)
      | _ -> failwith "unreachable (87)")
    87;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 3;
      push_env w_87 (Dynarray.get w_87.state.e 0);
      w_87.state.c <- pc_to_exp (int_to_pc 90))
    88;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 5;
      let last_42 = Source.E 4 in
      let x_42 = resolve w_89 last_42 in
      match Word.get_value (fst x_42) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_89);
          assert_env_length w_89 4;
          push_env w_89 (Dynarray.get w_89.state.e 1);
          assert_env_length w_89 5;
          drop_n w_89 5 1;
          assert_env_length w_89 4;
          return_n w_89 4 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_89);
          assert_env_length w_89 4;
          push_env w_89 (Dynarray.get w_89.state.e 2);
          assert_env_length w_89 5;
          drop_n w_89 5 1;
          assert_env_length w_89 4;
          return_n w_89 4 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (89)")
    89;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 4;
      let last_41 = Source.E 3 in
      let x_41 = resolve w_88 last_41 in
      match Word.get_value (fst x_41) with
      | 5 (* tag_Const *) ->
          let splits_58 = Memo.splits (snd x_41) in
          let split0_58 = List.nth splits_58 0 in
          ignore (pop_env w_88);
          push_env w_88 split0_58;
          assert_env_length w_88 4;
          push_env w_88 (Dynarray.get w_88.state.e 3);
          assert_env_length w_88 5;
          drop_n w_88 5 1;
          assert_env_length w_88 4;
          return_n w_88 4 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_59 = Memo.splits (snd x_41) in
          let split0_59 = List.nth splits_59 0 in
          ignore (pop_env w_88);
          push_env w_88 split0_59;
          assert_env_length w_88 4;
          push_env w_88 (Dynarray.get w_88.state.e 3);
          w_88.state.c <- pc_to_exp (int_to_pc 89)
      | 7 (* tag_Add *) ->
          let splits_60 = Memo.splits (snd x_41) in
          let split0_60 = List.nth splits_60 0 in
          let split1_35 = List.nth splits_60 1 in
          ignore (pop_env w_88);
          push_env w_88 split0_60;
          push_env w_88 split1_35;
          assert_env_length w_88 5;
          push_env w_88 (Dynarray.get w_88.state.e 3);
          assert_env_length w_88 6;
          push_env w_88 (Dynarray.get w_88.state.e 1);
          assert_env_length w_88 7;
          push_env w_88 (Dynarray.get w_88.state.e 2);
          assert_env_length w_88 8;
          let keep_34 = env_call w_88 [ 1; 2; 4 ] 3 in
          w_88.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_34; w_88.state.k ];
          w_88.state.c <- pc_to_exp (int_to_pc 88)
      | 8 (* tag_Mul *) ->
          let splits_61 = Memo.splits (snd x_41) in
          let split0_61 = List.nth splits_61 0 in
          let split1_36 = List.nth splits_61 1 in
          ignore (pop_env w_88);
          push_env w_88 split0_61;
          push_env w_88 split1_36;
          assert_env_length w_88 5;
          push_env w_88 (Dynarray.get w_88.state.e 3);
          assert_env_length w_88 6;
          push_env w_88 (Dynarray.get w_88.state.e 1);
          assert_env_length w_88 7;
          push_env w_88 (Dynarray.get w_88.state.e 2);
          assert_env_length w_88 8;
          let keep_35 = env_call w_88 [ 1; 2; 4 ] 3 in
          w_88.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_35; w_88.state.k ];
          w_88.state.c <- pc_to_exp (int_to_pc 88)
      | _ -> failwith "unreachable (90)")
    90;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 1;
      push_env w_90 (Dynarray.get w_90.state.e 0);
      assert_env_length w_90 2;
      let keep_36 = env_call w_90 [] 1 in
      w_90.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_36; w_90.state.k ];
      w_90.state.c <- pc_to_exp (int_to_pc 85))
    91;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 3;
      let x0_6 = resolve w_92 (Source.E 1) in
      let x1_6 = resolve w_92 (Source.E 2) in
      ignore (pop_env w_92);
      ignore (pop_env w_92);
      push_env w_92 (Memo.from_int (Word.get_value (fst x0_6) + Word.get_value (fst x1_6)));
      assert_env_length w_92 2;
      push_env w_92 (Dynarray.get w_92.state.e 0);
      assert_env_length w_92 3;
      let keep_41 = env_call w_92 [ 1 ] 1 in
      w_92.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_41; w_92.state.k ];
      w_92.state.c <- pc_to_exp (int_to_pc 13))
    92;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 3;
      let x0_7 = resolve w_93 (Source.E 1) in
      let x1_7 = resolve w_93 (Source.E 2) in
      ignore (pop_env w_93);
      ignore (pop_env w_93);
      push_env w_93 (Memo.from_int (Word.get_value (fst x0_7) + Word.get_value (fst x1_7)));
      assert_env_length w_93 2;
      push_env w_93 (Dynarray.get w_93.state.e 0);
      assert_env_length w_93 3;
      let keep_42 = env_call w_93 [ 1 ] 1 in
      w_93.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_42; w_93.state.k ];
      w_93.state.c <- pc_to_exp (int_to_pc 13))
    93;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 3;
      let last_43 = Source.E 2 in
      let x_43 = resolve w_95 last_43 in
      match Word.get_value (fst x_43) with
      | 8 (* tag_Mul *) ->
          let splits_62 = Memo.splits (snd x_43) in
          let split0_62 = List.nth splits_62 0 in
          let split1_37 = List.nth splits_62 1 in
          ignore (pop_env w_95);
          push_env w_95 split0_62;
          push_env w_95 split1_37;
          assert_env_length w_95 4;
          push_env w_95 (Dynarray.get w_95.state.e 0);
          assert_env_length w_95 5;
          push_env w_95 (Dynarray.get w_95.state.e 2);
          assert_env_length w_95 6;
          let keep_44 = env_call w_95 [ 0; 2; 3 ] 2 in
          w_95.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_44; w_95.state.k ];
          w_95.state.c <- pc_to_exp (int_to_pc 29)
      | _ ->
          ignore (pop_env w_95);
          assert_env_length w_95 2;
          push_env w_95 (Memo.from_constructor tag_Missing);
          assert_env_length w_95 3;
          return_n w_95 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (94)")
    94;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 3;
      let cond_4 = resolve w_94 (Source.E 2) in
      ignore (pop_env w_94);
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_94 2;
        push_env w_94 (Memo.from_int 1);
        assert_env_length w_94 3;
        let ctor_arg_26 = pop_env w_94 in
        push_env w_94 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_26 ]);
        assert_env_length w_94 3;
        let ctor_arg_27 = pop_env w_94 in
        push_env w_94 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_27 ]);
        assert_env_length w_94 3;
        return_n w_94 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_94 2;
        push_env w_94 (Dynarray.get w_94.state.e 1);
        w_94.state.c <- pc_to_exp (int_to_pc 94)))
    95;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 6;
      let last_44 = Source.E 5 in
      let x_44 = resolve w_96 last_44 in
      match Word.get_value (fst x_44) with
      | 10 (* tag_Found *) ->
          let splits_63 = Memo.splits (snd x_44) in
          let split0_63 = List.nth splits_63 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_63;
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 7;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          assert_env_length w_96 8;
          push_env w_96 (Dynarray.get w_96.state.e 5);
          assert_env_length w_96 9;
          let ctor_arg_28 = pop_env w_96 in
          let ctor_arg_29 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_29; ctor_arg_28 ]);
          assert_env_length w_96 8;
          let ctor_arg_30 = pop_env w_96 in
          let ctor_arg_31 = pop_env w_96 in
          push_env w_96 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_31; ctor_arg_30 ]);
          assert_env_length w_96 7;
          drop_n w_96 7 1;
          assert_env_length w_96 6;
          drop_n w_96 6 1;
          assert_env_length w_96 5;
          drop_n w_96 5 2;
          assert_env_length w_96 3;
          return_n w_96 3 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_96);
          assert_env_length w_96 5;
          push_env w_96 (Dynarray.get w_96.state.e 3);
          assert_env_length w_96 6;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 7;
          let keep_45 = env_call w_96 [ 0; 1; 2; 3 ] 2 in
          w_96.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_45; w_96.state.k ];
          w_96.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (96)")
    96;
  add_exp
    (fun w_98 ->
      assert_env_length w_98 5;
      let cond_5 = resolve w_98 (Source.E 4) in
      ignore (pop_env w_98);
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_98 4;
        push_env w_98 (Dynarray.get w_98.state.e 0);
        assert_env_length w_98 5;
        push_env w_98 (Dynarray.get w_98.state.e 1);
        assert_env_length w_98 6;
        let ctor_arg_34 = pop_env w_98 in
        let ctor_arg_35 = pop_env w_98 in
        push_env w_98 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_35; ctor_arg_34 ]);
        assert_env_length w_98 5;
        drop_n w_98 5 2;
        assert_env_length w_98 3;
        return_n w_98 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_98 4;
        push_env w_98 (Dynarray.get w_98.state.e 2);
        assert_env_length w_98 5;
        push_env w_98 (Dynarray.get w_98.state.e 0);
        assert_env_length w_98 6;
        push_env w_98 (Dynarray.get w_98.state.e 3);
        assert_env_length w_98 7;
        let keep_46 = env_call w_98 [ 4 ] 2 in
        w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; keep_46; w_98.state.k ];
        w_98.state.c <- pc_to_exp (int_to_pc 34)))
    97;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 6;
      let x0_8 = resolve w_97 (Source.E 4) in
      let x1_8 = resolve w_97 (Source.E 5) in
      ignore (pop_env w_97);
      ignore (pop_env w_97);
      push_env w_97 (Memo.from_int (if Word.get_value (fst x0_8) <= Word.get_value (fst x1_8) then 1 else 0));
      w_97.state.c <- pc_to_exp (int_to_pc 97))
    98;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 4;
      let cond_6 = resolve w_101 (Source.E 3) in
      ignore (pop_env w_101);
      if Word.get_value (fst cond_6) <> 0 then (
        assert_env_length w_101 3;
        push_env w_101 (Dynarray.get w_101.state.e 0);
        assert_env_length w_101 4;
        ignore (env_call w_101 [] 1);
        w_101.state.c <- pc_to_exp (int_to_pc 55))
      else (
        assert_env_length w_101 3;
        push_env w_101 (Dynarray.get w_101.state.e 1);
        assert_env_length w_101 4;
        push_env w_101 (Dynarray.get w_101.state.e 0);
        assert_env_length w_101 5;
        let keep_50 = env_call w_101 [ 3 ] 1 in
        w_101.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; keep_50; w_101.state.k ];
        w_101.state.c <- pc_to_exp (int_to_pc 55)))
    99;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 5;
      let x0_9 = resolve w_100 (Source.E 3) in
      let x1_9 = resolve w_100 (Source.E 4) in
      ignore (pop_env w_100);
      ignore (pop_env w_100);
      push_env w_100 (Memo.from_int (if Word.get_value (fst x0_9) = Word.get_value (fst x1_9) then 1 else 0));
      w_100.state.c <- pc_to_exp (int_to_pc 99))
    100;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 3;
      let last_45 = Source.E 2 in
      let x_45 = resolve w_99 last_45 in
      match Word.get_value (fst x_45) with
      | 5 (* tag_Const *) ->
          let splits_64 = Memo.splits (snd x_45) in
          let split0_64 = List.nth splits_64 0 in
          ignore (pop_env w_99);
          push_env w_99 split0_64;
          assert_env_length w_99 3;
          push_env w_99 (Dynarray.get w_99.state.e 2);
          assert_env_length w_99 4;
          push_env w_99 (Memo.from_int 1);
          w_99.state.c <- pc_to_exp (int_to_pc 100)
      | _ ->
          ignore (pop_env w_99);
          assert_env_length w_99 2;
          push_env w_99 (Dynarray.get w_99.state.e 1);
          assert_env_length w_99 3;
          push_env w_99 (Dynarray.get w_99.state.e 0);
          assert_env_length w_99 4;
          let keep_51 = env_call w_99 [ 2 ] 1 in
          w_99.state.k <- Memo.appends [ Memo.from_constructor tag_cont_54; keep_51; w_99.state.k ];
          w_99.state.c <- pc_to_exp (int_to_pc 55)
      | _ -> failwith "unreachable (101)")
    101;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 3;
      let last_46 = Source.E 2 in
      let x_46 = resolve w_102 last_46 in
      match Word.get_value (fst x_46) with
      | 13 (* tag_NoPick *) ->
          ignore (pop_env w_102);
          assert_env_length w_102 2;
          push_env w_102 (Dynarray.get w_102.state.e 0);
          assert_env_length w_102 3;
          push_env w_102 (Dynarray.get w_102.state.e 1);
          assert_env_length w_102 4;
          let keep_58 = env_call w_102 [ 2 ] 1 in
          w_102.state.k <- Memo.appends [ Memo.from_constructor tag_cont_61; keep_58; w_102.state.k ];
          w_102.state.c <- pc_to_exp (int_to_pc 72)
      | 14 (* tag_Pick *) ->
          let splits_65 = Memo.splits (snd x_46) in
          let split0_65 = List.nth splits_65 0 in
          let split1_38 = List.nth splits_65 1 in
          ignore (pop_env w_102);
          push_env w_102 split0_65;
          push_env w_102 split1_38;
          assert_env_length w_102 4;
          push_env w_102 (Dynarray.get w_102.state.e 2);
          assert_env_length w_102 5;
          push_env w_102 (Dynarray.get w_102.state.e 3);
          assert_env_length w_102 6;
          let keep_59 = env_call w_102 [] 2 in
          w_102.state.k <- Memo.appends [ Memo.from_constructor tag_cont_62; keep_59; w_102.state.k ];
          w_102.state.c <- pc_to_exp (int_to_pc 34)
      | _ -> failwith "unreachable (102)")
    102;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 7;
      let cond_10 = resolve w_112 (Source.E 6) in
      ignore (pop_env w_112);
      if Word.get_value (fst cond_10) <> 0 then (
        assert_env_length w_112 6;
        push_env w_112 (Memo.from_int 1);
        assert_env_length w_112 7;
        drop_n w_112 7 1;
        assert_env_length w_112 6;
        drop_n w_112 6 1;
        assert_env_length w_112 5;
        drop_n w_112 5 1;
        assert_env_length w_112 4;
        drop_n w_112 4 1;
        assert_env_length w_112 3;
        return_n w_112 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_112 6;
        push_env w_112 (Memo.from_int 0);
        assert_env_length w_112 7;
        drop_n w_112 7 1;
        assert_env_length w_112 6;
        drop_n w_112 6 1;
        assert_env_length w_112 5;
        drop_n w_112 5 1;
        assert_env_length w_112 4;
        drop_n w_112 4 1;
        assert_env_length w_112 3;
        return_n w_112 3 (pc_to_exp (int_to_pc 0))))
    103;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 8;
      let x0_13 = resolve w_111 (Source.E 6) in
      let x1_13 = resolve w_111 (Source.E 7) in
      ignore (pop_env w_111);
      ignore (pop_env w_111);
      push_env w_111 (Memo.from_int (if Word.get_value (fst x0_13) > Word.get_value (fst x1_13) then 1 else 0));
      w_111.state.c <- pc_to_exp (int_to_pc 103))
    104;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 8;
      let x0_14 = resolve w_113 (Source.E 6) in
      let x1_14 = resolve w_113 (Source.E 7) in
      ignore (pop_env w_113);
      ignore (pop_env w_113);
      push_env w_113 (Memo.from_int (Word.get_value (fst x0_14) - Word.get_value (fst x1_14)));
      assert_env_length w_113 7;
      drop_n w_113 7 1;
      assert_env_length w_113 6;
      drop_n w_113 6 1;
      assert_env_length w_113 5;
      drop_n w_113 5 1;
      assert_env_length w_113 4;
      drop_n w_113 4 1;
      assert_env_length w_113 3;
      return_n w_113 3 (pc_to_exp (int_to_pc 0)))
    105;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 7;
      let cond_9 = resolve w_110 (Source.E 6) in
      ignore (pop_env w_110);
      if Word.get_value (fst cond_9) <> 0 then (
        assert_env_length w_110 6;
        push_env w_110 (Memo.from_int 0);
        assert_env_length w_110 7;
        push_env w_110 (Memo.from_int 1);
        w_110.state.c <- pc_to_exp (int_to_pc 105))
      else (
        assert_env_length w_110 6;
        push_env w_110 (Dynarray.get w_110.state.e 4);
        assert_env_length w_110 7;
        push_env w_110 (Dynarray.get w_110.state.e 5);
        w_110.state.c <- pc_to_exp (int_to_pc 104)))
    106;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 8;
      let x0_12 = resolve w_109 (Source.E 6) in
      let x1_12 = resolve w_109 (Source.E 7) in
      ignore (pop_env w_109);
      ignore (pop_env w_109);
      push_env w_109 (Memo.from_int (if Word.get_value (fst x0_12) < Word.get_value (fst x1_12) then 1 else 0));
      w_109.state.c <- pc_to_exp (int_to_pc 106))
    107;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 6;
      let last_48 = Source.E 5 in
      let x_48 = resolve w_108 last_48 in
      match Word.get_value (fst x_48) with
      | 5 (* tag_Const *) ->
          let splits_67 = Memo.splits (snd x_48) in
          let split0_67 = List.nth splits_67 0 in
          ignore (pop_env w_108);
          push_env w_108 split0_67;
          assert_env_length w_108 6;
          push_env w_108 (Dynarray.get w_108.state.e 4);
          assert_env_length w_108 7;
          push_env w_108 (Dynarray.get w_108.state.e 5);
          w_108.state.c <- pc_to_exp (int_to_pc 107)
      | _ -> failwith "unreachable (108)")
    108;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 6;
      let last_49 = Source.E 5 in
      let x_49 = resolve w_114 last_49 in
      match Word.get_value (fst x_49) with
      | 6 (* tag_Var *) ->
          let splits_69 = Memo.splits (snd x_49) in
          let split0_69 = List.nth splits_69 0 in
          ignore (pop_env w_114);
          push_env w_114 split0_69;
          assert_env_length w_114 6;
          push_env w_114 (Dynarray.get w_114.state.e 4);
          assert_env_length w_114 7;
          let keep_73 = env_call w_114 [ 5 ] 1 in
          w_114.state.k <- Memo.appends [ Memo.from_constructor tag_cont_76; keep_73; w_114.state.k ];
          w_114.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (109)")
    109;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 7;
      let last_50 = Source.E 6 in
      let x_50 = resolve w_115 last_50 in
      match Word.get_value (fst x_50) with
      | 7 (* tag_Add *) ->
          let splits_71 = Memo.splits (snd x_50) in
          let split0_71 = List.nth splits_71 0 in
          let split1_40 = List.nth splits_71 1 in
          ignore (pop_env w_115);
          push_env w_115 split0_71;
          push_env w_115 split1_40;
          assert_env_length w_115 8;
          push_env w_115 (Dynarray.get w_115.state.e 4);
          assert_env_length w_115 9;
          push_env w_115 (Dynarray.get w_115.state.e 6);
          assert_env_length w_115 10;
          let keep_74 = env_call w_115 [ 5; 7 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_77; keep_74; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (110)")
    110;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 7;
      let last_51 = Source.E 6 in
      let x_51 = resolve w_116 last_51 in
      match Word.get_value (fst x_51) with
      | 8 (* tag_Mul *) ->
          let splits_73 = Memo.splits (snd x_51) in
          let split0_73 = List.nth splits_73 0 in
          let split1_42 = List.nth splits_73 1 in
          ignore (pop_env w_116);
          push_env w_116 split0_73;
          push_env w_116 split1_42;
          assert_env_length w_116 8;
          push_env w_116 (Dynarray.get w_116.state.e 4);
          assert_env_length w_116 9;
          push_env w_116 (Dynarray.get w_116.state.e 6);
          assert_env_length w_116 10;
          let keep_75 = env_call w_116 [ 5; 7 ] 2 in
          w_116.state.k <- Memo.appends [ Memo.from_constructor tag_cont_78; keep_75; w_116.state.k ];
          w_116.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (111)")
    111;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 5;
      let last_47 = Source.E 4 in
      let x_47 = resolve w_107 last_47 in
      match Word.get_value (fst x_47) with
      | 5 (* tag_Const *) ->
          let splits_66 = Memo.splits (snd x_47) in
          let split0_66 = List.nth splits_66 0 in
          ignore (pop_env w_107);
          push_env w_107 split0_66;
          assert_env_length w_107 5;
          push_env w_107 (Dynarray.get w_107.state.e 1);
          w_107.state.c <- pc_to_exp (int_to_pc 108)
      | 6 (* tag_Var *) ->
          let splits_68 = Memo.splits (snd x_47) in
          let split0_68 = List.nth splits_68 0 in
          ignore (pop_env w_107);
          push_env w_107 split0_68;
          assert_env_length w_107 5;
          push_env w_107 (Dynarray.get w_107.state.e 1);
          w_107.state.c <- pc_to_exp (int_to_pc 109)
      | 7 (* tag_Add *) ->
          let splits_70 = Memo.splits (snd x_47) in
          let split0_70 = List.nth splits_70 0 in
          let split1_39 = List.nth splits_70 1 in
          ignore (pop_env w_107);
          push_env w_107 split0_70;
          push_env w_107 split1_39;
          assert_env_length w_107 6;
          push_env w_107 (Dynarray.get w_107.state.e 1);
          w_107.state.c <- pc_to_exp (int_to_pc 110)
      | 8 (* tag_Mul *) ->
          let splits_72 = Memo.splits (snd x_47) in
          let split0_72 = List.nth splits_72 0 in
          let split1_41 = List.nth splits_72 1 in
          ignore (pop_env w_107);
          push_env w_107 split0_72;
          push_env w_107 split1_41;
          assert_env_length w_107 6;
          push_env w_107 (Dynarray.get w_107.state.e 1);
          w_107.state.c <- pc_to_exp (int_to_pc 111)
      | _ -> failwith "unreachable (112)")
    112;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 5;
      let cond_8 = resolve w_106 (Source.E 4) in
      ignore (pop_env w_106);
      if Word.get_value (fst cond_8) <> 0 then (
        assert_env_length w_106 4;
        push_env w_106 (Memo.from_int 1);
        assert_env_length w_106 5;
        drop_n w_106 5 1;
        assert_env_length w_106 4;
        drop_n w_106 4 1;
        assert_env_length w_106 3;
        return_n w_106 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_106 4;
        push_env w_106 (Dynarray.get w_106.state.e 0);
        w_106.state.c <- pc_to_exp (int_to_pc 112)))
    113;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 6;
      let x0_11 = resolve w_105 (Source.E 4) in
      let x1_11 = resolve w_105 (Source.E 5) in
      ignore (pop_env w_105);
      ignore (pop_env w_105);
      push_env w_105 (Memo.from_int (if Word.get_value (fst x0_11) > Word.get_value (fst x1_11) then 1 else 0));
      w_105.state.c <- pc_to_exp (int_to_pc 113))
    114;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 6;
      let x0_15 = resolve w_117 (Source.E 4) in
      let x1_15 = resolve w_117 (Source.E 5) in
      ignore (pop_env w_117);
      ignore (pop_env w_117);
      push_env w_117 (Memo.from_int (Word.get_value (fst x0_15) - Word.get_value (fst x1_15)));
      assert_env_length w_117 5;
      drop_n w_117 5 1;
      assert_env_length w_117 4;
      drop_n w_117 4 1;
      assert_env_length w_117 3;
      return_n w_117 3 (pc_to_exp (int_to_pc 0)))
    115;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 5;
      let cond_7 = resolve w_104 (Source.E 4) in
      ignore (pop_env w_104);
      if Word.get_value (fst cond_7) <> 0 then (
        assert_env_length w_104 4;
        push_env w_104 (Memo.from_int 0);
        assert_env_length w_104 5;
        push_env w_104 (Memo.from_int 1);
        w_104.state.c <- pc_to_exp (int_to_pc 115))
      else (
        assert_env_length w_104 4;
        push_env w_104 (Dynarray.get w_104.state.e 2);
        assert_env_length w_104 5;
        push_env w_104 (Dynarray.get w_104.state.e 3);
        w_104.state.c <- pc_to_exp (int_to_pc 114)))
    116;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 6;
      let x0_10 = resolve w_103 (Source.E 4) in
      let x1_10 = resolve w_103 (Source.E 5) in
      ignore (pop_env w_103);
      ignore (pop_env w_103);
      push_env w_103 (Memo.from_int (if Word.get_value (fst x0_10) < Word.get_value (fst x1_10) then 1 else 0));
      w_103.state.c <- pc_to_exp (int_to_pc 116))
    117;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 2;
      let x0_16 = resolve w_118 (Source.E 0) in
      let x1_16 = resolve w_118 (Source.E 1) in
      ignore (pop_env w_118);
      ignore (pop_env w_118);
      push_env w_118 (Memo.from_int (if Word.get_value (fst x0_16) = Word.get_value (fst x1_16) then 1 else 0));
      assert_env_length w_118 1;
      drop_n w_118 1 0;
      assert_env_length w_118 1;
      drop_n w_118 1 0;
      assert_env_length w_118 1;
      return_n w_118 1 (pc_to_exp (int_to_pc 0)))
    118;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 2;
      let x0_17 = resolve w_119 (Source.E 0) in
      let x1_17 = resolve w_119 (Source.E 1) in
      ignore (pop_env w_119);
      ignore (pop_env w_119);
      push_env w_119
        (Memo.from_int (if Word.get_value (fst x0_17) <> 0 && Word.get_value (fst x1_17) <> 0 then 1 else 0));
      assert_env_length w_119 1;
      drop_n w_119 1 0;
      assert_env_length w_119 1;
      drop_n w_119 1 0;
      assert_env_length w_119 1;
      return_n w_119 1 (pc_to_exp (int_to_pc 0)))
    119;
  add_exp
    (fun w_120 ->
      assert_env_length w_120 2;
      let x0_18 = resolve w_120 (Source.E 0) in
      let x1_18 = resolve w_120 (Source.E 1) in
      ignore (pop_env w_120);
      ignore (pop_env w_120);
      push_env w_120
        (Memo.from_int (if Word.get_value (fst x0_18) <> 0 && Word.get_value (fst x1_18) <> 0 then 1 else 0));
      assert_env_length w_120 1;
      drop_n w_120 1 0;
      assert_env_length w_120 1;
      drop_n w_120 1 0;
      assert_env_length w_120 1;
      return_n w_120 1 (pc_to_exp (int_to_pc 0)))
    120;
  add_exp
    (fun w_121 ->
      assert_env_length w_121 2;
      let x0_19 = resolve w_121 (Source.E 0) in
      let x1_19 = resolve w_121 (Source.E 1) in
      ignore (pop_env w_121);
      ignore (pop_env w_121);
      push_env w_121 (Memo.from_int (Word.get_value (fst x0_19) + Word.get_value (fst x1_19)));
      assert_env_length w_121 1;
      drop_n w_121 1 0;
      assert_env_length w_121 1;
      return_n w_121 1 (pc_to_exp (int_to_pc 0)))
    121;
  add_exp
    (fun w_122 ->
      assert_env_length w_122 2;
      let x0_20 = resolve w_122 (Source.E 0) in
      let x1_20 = resolve w_122 (Source.E 1) in
      ignore (pop_env w_122);
      ignore (pop_env w_122);
      push_env w_122 (Memo.from_int (Word.get_value (fst x0_20) + Word.get_value (fst x1_20)));
      assert_env_length w_122 1;
      drop_n w_122 1 0;
      assert_env_length w_122 1;
      return_n w_122 1 (pc_to_exp (int_to_pc 0)))
    122;
  add_exp
    (fun w_126 ->
      assert_env_length w_126 5;
      let cond_12 = resolve w_126 (Source.E 4) in
      ignore (pop_env w_126);
      if Word.get_value (fst cond_12) <> 0 then (
        assert_env_length w_126 4;
        push_env w_126 (Dynarray.get w_126.state.e 1);
        assert_env_length w_126 5;
        drop_n w_126 5 1;
        assert_env_length w_126 4;
        drop_n w_126 4 1;
        assert_env_length w_126 3;
        return_n w_126 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_126 4;
        push_env w_126 (Dynarray.get w_126.state.e 0);
        assert_env_length w_126 5;
        push_env w_126 (Dynarray.get w_126.state.e 1);
        assert_env_length w_126 6;
        let keep_76 = env_call w_126 [ 0; 1 ] 2 in
        w_126.state.k <- Memo.appends [ Memo.from_constructor tag_cont_79; keep_76; w_126.state.k ];
        w_126.state.c <- pc_to_exp (int_to_pc 5)))
    123;
  add_exp
    (fun w_125 ->
      assert_env_length w_125 6;
      let x0_22 = resolve w_125 (Source.E 4) in
      let x1_22 = resolve w_125 (Source.E 5) in
      ignore (pop_env w_125);
      ignore (pop_env w_125);
      push_env w_125 (Memo.from_int (if Word.get_value (fst x0_22) < Word.get_value (fst x1_22) then 1 else 0));
      w_125.state.c <- pc_to_exp (int_to_pc 123))
    124;
  add_exp
    (fun w_124 ->
      assert_env_length w_124 5;
      let cond_11 = resolve w_124 (Source.E 4) in
      ignore (pop_env w_124);
      if Word.get_value (fst cond_11) <> 0 then (
        assert_env_length w_124 4;
        push_env w_124 (Dynarray.get w_124.state.e 0);
        assert_env_length w_124 5;
        drop_n w_124 5 1;
        assert_env_length w_124 4;
        drop_n w_124 4 1;
        assert_env_length w_124 3;
        return_n w_124 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_124 4;
        push_env w_124 (Dynarray.get w_124.state.e 3);
        assert_env_length w_124 5;
        push_env w_124 (Dynarray.get w_124.state.e 2);
        w_124.state.c <- pc_to_exp (int_to_pc 124)))
    125;
  add_exp
    (fun w_123 ->
      assert_env_length w_123 6;
      let x0_21 = resolve w_123 (Source.E 4) in
      let x1_21 = resolve w_123 (Source.E 5) in
      ignore (pop_env w_123);
      ignore (pop_env w_123);
      push_env w_123 (Memo.from_int (if Word.get_value (fst x0_21) < Word.get_value (fst x1_21) then 1 else 0));
      w_123.state.c <- pc_to_exp (int_to_pc 125))
    126;
  add_exp
    (fun w_127 ->
      assert_env_length w_127 5;
      let last_52 = Source.E 4 in
      let x_52 = resolve w_127 last_52 in
      match Word.get_value (fst x_52) with
      | 10 (* tag_Found *) ->
          let splits_74 = Memo.splits (snd x_52) in
          let split0_74 = List.nth splits_74 0 in
          ignore (pop_env w_127);
          push_env w_127 split0_74;
          assert_env_length w_127 5;
          push_env w_127 (Dynarray.get w_127.state.e 4);
          assert_env_length w_127 6;
          push_env w_127 (Dynarray.get w_127.state.e 2);
          assert_env_length w_127 7;
          let ctor_arg_54 = pop_env w_127 in
          let ctor_arg_55 = pop_env w_127 in
          push_env w_127 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_55; ctor_arg_54 ]);
          assert_env_length w_127 6;
          let ctor_arg_56 = pop_env w_127 in
          push_env w_127 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_56 ]);
          assert_env_length w_127 6;
          drop_n w_127 6 1;
          assert_env_length w_127 5;
          drop_n w_127 5 1;
          assert_env_length w_127 4;
          drop_n w_127 4 2;
          assert_env_length w_127 2;
          return_n w_127 2 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_127);
          assert_env_length w_127 4;
          push_env w_127 (Dynarray.get w_127.state.e 0);
          assert_env_length w_127 5;
          push_env w_127 (Dynarray.get w_127.state.e 2);
          assert_env_length w_127 6;
          let keep_77 = env_call w_127 [ 1 ] 2 in
          w_127.state.k <- Memo.appends [ Memo.from_constructor tag_cont_80; keep_77; w_127.state.k ];
          w_127.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (127)")
    127;
  add_exp
    (fun w_128 ->
      assert_env_length w_128 6;
      let last_53 = Source.E 5 in
      let x_53 = resolve w_128 last_53 in
      match Word.get_value (fst x_53) with
      | 10 (* tag_Found *) ->
          let splits_75 = Memo.splits (snd x_53) in
          let split0_75 = List.nth splits_75 0 in
          ignore (pop_env w_128);
          push_env w_128 split0_75;
          assert_env_length w_128 6;
          push_env w_128 (Dynarray.get w_128.state.e 3);
          assert_env_length w_128 7;
          push_env w_128 (Dynarray.get w_128.state.e 2);
          assert_env_length w_128 8;
          push_env w_128 (Dynarray.get w_128.state.e 5);
          assert_env_length w_128 9;
          let ctor_arg_57 = pop_env w_128 in
          let ctor_arg_58 = pop_env w_128 in
          push_env w_128 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_58; ctor_arg_57 ]);
          assert_env_length w_128 8;
          let ctor_arg_59 = pop_env w_128 in
          let ctor_arg_60 = pop_env w_128 in
          push_env w_128 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_60; ctor_arg_59 ]);
          assert_env_length w_128 7;
          drop_n w_128 7 1;
          assert_env_length w_128 6;
          drop_n w_128 6 1;
          assert_env_length w_128 5;
          drop_n w_128 5 0;
          assert_env_length w_128 5;
          drop_n w_128 5 2;
          assert_env_length w_128 3;
          return_n w_128 3 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_128);
          assert_env_length w_128 5;
          push_env w_128 (Dynarray.get w_128.state.e 0);
          assert_env_length w_128 6;
          push_env w_128 (Dynarray.get w_128.state.e 1);
          assert_env_length w_128 7;
          let ctor_arg_61 = pop_env w_128 in
          let ctor_arg_62 = pop_env w_128 in
          push_env w_128 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_62; ctor_arg_61 ]);
          assert_env_length w_128 6;
          drop_n w_128 6 1;
          assert_env_length w_128 5;
          drop_n w_128 5 0;
          assert_env_length w_128 5;
          drop_n w_128 5 2;
          assert_env_length w_128 3;
          return_n w_128 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (128)")
    128;
  add_exp
    (fun w_129 ->
      assert_env_length w_129 2;
      let x0_23 = resolve w_129 (Source.E 0) in
      let x1_23 = resolve w_129 (Source.E 1) in
      ignore (pop_env w_129);
      ignore (pop_env w_129);
      push_env w_129 (Memo.from_int (Word.get_value (fst x0_23) * Word.get_value (fst x1_23)));
      assert_env_length w_129 1;
      drop_n w_129 1 0;
      assert_env_length w_129 1;
      return_n w_129 1 (pc_to_exp (int_to_pc 0)))
    129;
  add_exp
    (fun w_131 ->
      assert_env_length w_131 3;
      let cond_13 = resolve w_131 (Source.E 2) in
      ignore (pop_env w_131);
      if Word.get_value (fst cond_13) <> 0 then (
        assert_env_length w_131 2;
        push_env w_131 (Memo.from_int 0);
        assert_env_length w_131 3;
        let ctor_arg_65 = pop_env w_131 in
        push_env w_131 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_65 ]);
        assert_env_length w_131 3;
        drop_n w_131 3 1;
        assert_env_length w_131 2;
        drop_n w_131 2 1;
        assert_env_length w_131 1;
        return_n w_131 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_131 2;
        push_env w_131 (Dynarray.get w_131.state.e 0);
        assert_env_length w_131 3;
        let keep_78 = env_call w_131 [ 1 ] 1 in
        w_131.state.k <- Memo.appends [ Memo.from_constructor tag_cont_81; keep_78; w_131.state.k ];
        w_131.state.c <- pc_to_exp (int_to_pc 55)))
    130;
  add_exp
    (fun w_130 ->
      assert_env_length w_130 4;
      let x0_24 = resolve w_130 (Source.E 2) in
      let x1_24 = resolve w_130 (Source.E 3) in
      ignore (pop_env w_130);
      ignore (pop_env w_130);
      push_env w_130 (Memo.from_int (if Word.get_value (fst x0_24) = Word.get_value (fst x1_24) then 1 else 0));
      w_130.state.c <- pc_to_exp (int_to_pc 130))
    131;
  add_exp
    (fun w_132 ->
      assert_env_length w_132 5;
      let cond_14 = resolve w_132 (Source.E 4) in
      ignore (pop_env w_132);
      if Word.get_value (fst cond_14) <> 0 then (
        assert_env_length w_132 4;
        push_env w_132 (Dynarray.get w_132.state.e 0);
        assert_env_length w_132 5;
        push_env w_132 (Dynarray.get w_132.state.e 1);
        assert_env_length w_132 6;
        let keep_82 = env_call w_132 [ 4 ] 1 in
        w_132.state.k <- Memo.appends [ Memo.from_constructor tag_cont_84; keep_82; w_132.state.k ];
        w_132.state.c <- pc_to_exp (int_to_pc 67))
      else (
        assert_env_length w_132 4;
        push_env w_132 (Dynarray.get w_132.state.e 3);
        assert_env_length w_132 5;
        push_env w_132 (Dynarray.get w_132.state.e 2);
        assert_env_length w_132 6;
        let keep_81 = env_call w_132 [] 2 in
        w_132.state.k <- Memo.appends [ Memo.from_constructor tag_cont_83; keep_81; w_132.state.k ];
        w_132.state.c <- pc_to_exp (int_to_pc 34)))
    132;
  add_exp
    (fun w_133 ->
      assert_env_length w_133 5;
      let cond_15 = resolve w_133 (Source.E 4) in
      ignore (pop_env w_133);
      if Word.get_value (fst cond_15) <> 0 then (
        assert_env_length w_133 4;
        push_env w_133 (Dynarray.get w_133.state.e 0);
        assert_env_length w_133 5;
        push_env w_133 (Dynarray.get w_133.state.e 2);
        assert_env_length w_133 6;
        let keep_83 = env_call w_133 [ 1 ] 2 in
        w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_85; keep_83; w_133.state.k ];
        w_133.state.c <- pc_to_exp (int_to_pc 70))
      else (
        assert_env_length w_133 4;
        push_env w_133 (Dynarray.get w_133.state.e 3);
        assert_env_length w_133 5;
        push_env w_133 (Dynarray.get w_133.state.e 2);
        assert_env_length w_133 6;
        let ctor_arg_66 = pop_env w_133 in
        let ctor_arg_67 = pop_env w_133 in
        push_env w_133 (Memo.appends [ Memo.from_constructor tag_Pick; ctor_arg_67; ctor_arg_66 ]);
        assert_env_length w_133 5;
        drop_n w_133 5 1;
        assert_env_length w_133 4;
        drop_n w_133 4 2;
        assert_env_length w_133 2;
        return_n w_133 2 (pc_to_exp (int_to_pc 0))))
    133;
  add_exp
    (fun w_134 ->
      assert_env_length w_134 2;
      let x0_25 = resolve w_134 (Source.E 0) in
      let x1_25 = resolve w_134 (Source.E 1) in
      ignore (pop_env w_134);
      ignore (pop_env w_134);
      push_env w_134 (Memo.from_int (Word.get_value (fst x0_25) + Word.get_value (fst x1_25)));
      assert_env_length w_134 1;
      drop_n w_134 1 0;
      assert_env_length w_134 1;
      return_n w_134 1 (pc_to_exp (int_to_pc 0)))
    134;
  add_exp
    (fun w_135 ->
      assert_env_length w_135 2;
      let x0_26 = resolve w_135 (Source.E 0) in
      let x1_26 = resolve w_135 (Source.E 1) in
      ignore (pop_env w_135);
      ignore (pop_env w_135);
      push_env w_135 (Memo.from_int (Word.get_value (fst x0_26) * Word.get_value (fst x1_26)));
      assert_env_length w_135 1;
      drop_n w_135 1 0;
      assert_env_length w_135 1;
      return_n w_135 1 (pc_to_exp (int_to_pc 0)))
    135;
  add_exp
    (fun w_137 ->
      assert_env_length w_137 4;
      let cond_16 = resolve w_137 (Source.E 3) in
      ignore (pop_env w_137);
      if Word.get_value (fst cond_16) <> 0 then (
        assert_env_length w_137 3;
        push_env w_137 (Dynarray.get w_137.state.e 0);
        assert_env_length w_137 4;
        push_env w_137 (Dynarray.get w_137.state.e 1);
        assert_env_length w_137 5;
        ignore (env_call w_137 [] 2);
        w_137.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_137 3;
        push_env w_137 (Dynarray.get w_137.state.e 2);
        assert_env_length w_137 4;
        drop_n w_137 4 1;
        assert_env_length w_137 3;
        drop_n w_137 3 1;
        assert_env_length w_137 2;
        drop_n w_137 2 1;
        assert_env_length w_137 1;
        drop_n w_137 1 0;
        assert_env_length w_137 1;
        drop_n w_137 1 0;
        assert_env_length w_137 1;
        return_n w_137 1 (pc_to_exp (int_to_pc 0))))
    136;
  add_exp
    (fun w_136 ->
      assert_env_length w_136 5;
      let x0_27 = resolve w_136 (Source.E 3) in
      let x1_27 = resolve w_136 (Source.E 4) in
      ignore (pop_env w_136);
      ignore (pop_env w_136);
      push_env w_136 (Memo.from_int (if Word.get_value (fst x0_27) = Word.get_value (fst x1_27) then 1 else 0));
      w_136.state.c <- pc_to_exp (int_to_pc 136))
    137;
  add_exp
    (fun w_139 ->
      assert_env_length w_139 4;
      let cond_17 = resolve w_139 (Source.E 3) in
      ignore (pop_env w_139);
      if Word.get_value (fst cond_17) <> 0 then (
        assert_env_length w_139 3;
        push_env w_139 (Dynarray.get w_139.state.e 0);
        assert_env_length w_139 4;
        push_env w_139 (Dynarray.get w_139.state.e 1);
        assert_env_length w_139 5;
        ignore (env_call w_139 [] 2);
        w_139.state.c <- pc_to_exp (int_to_pc 5))
      else (
        assert_env_length w_139 3;
        push_env w_139 (Dynarray.get w_139.state.e 2);
        assert_env_length w_139 4;
        drop_n w_139 4 1;
        assert_env_length w_139 3;
        drop_n w_139 3 1;
        assert_env_length w_139 2;
        drop_n w_139 2 1;
        assert_env_length w_139 1;
        drop_n w_139 1 0;
        assert_env_length w_139 1;
        drop_n w_139 1 0;
        assert_env_length w_139 1;
        return_n w_139 1 (pc_to_exp (int_to_pc 0))))
    138;
  add_exp
    (fun w_138 ->
      assert_env_length w_138 5;
      let x0_28 = resolve w_138 (Source.E 3) in
      let x1_28 = resolve w_138 (Source.E 4) in
      ignore (pop_env w_138);
      ignore (pop_env w_138);
      push_env w_138 (Memo.from_int (if Word.get_value (fst x0_28) = Word.get_value (fst x1_28) then 1 else 0));
      w_138.state.c <- pc_to_exp (int_to_pc 138))
    139;
  add_exp
    (fun w_141 ->
      assert_env_length w_141 3;
      let cond_18 = resolve w_141 (Source.E 2) in
      ignore (pop_env w_141);
      if Word.get_value (fst cond_18) <> 0 then (
        assert_env_length w_141 2;
        push_env w_141 (Dynarray.get w_141.state.e 0);
        assert_env_length w_141 3;
        drop_n w_141 3 0;
        assert_env_length w_141 3;
        drop_n w_141 3 0;
        assert_env_length w_141 3;
        return_n w_141 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_141 2;
        push_env w_141 (Dynarray.get w_141.state.e 1);
        assert_env_length w_141 3;
        drop_n w_141 3 0;
        assert_env_length w_141 3;
        drop_n w_141 3 0;
        assert_env_length w_141 3;
        return_n w_141 3 (pc_to_exp (int_to_pc 0))))
    140;
  add_exp
    (fun w_140 ->
      assert_env_length w_140 4;
      let x0_29 = resolve w_140 (Source.E 2) in
      let x1_29 = resolve w_140 (Source.E 3) in
      ignore (pop_env w_140);
      ignore (pop_env w_140);
      push_env w_140 (Memo.from_int (if Word.get_value (fst x0_29) <= Word.get_value (fst x1_29) then 1 else 0));
      w_140.state.c <- pc_to_exp (int_to_pc 140))
    141;
  add_exp
    (fun w_142 ->
      assert_env_length w_142 3;
      let last_54 = Source.E 2 in
      let x_54 = resolve w_142 last_54 in
      match Word.get_value (fst x_54) with
      | 10 (* tag_Found *) ->
          let splits_76 = Memo.splits (snd x_54) in
          let split0_76 = List.nth splits_76 0 in
          ignore (pop_env w_142);
          push_env w_142 split0_76;
          assert_env_length w_142 3;
          push_env w_142 (Dynarray.get w_142.state.e 0);
          assert_env_length w_142 4;
          push_env w_142 (Dynarray.get w_142.state.e 2);
          assert_env_length w_142 5;
          let ctor_arg_76 = pop_env w_142 in
          let ctor_arg_77 = pop_env w_142 in
          push_env w_142 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_77; ctor_arg_76 ]);
          assert_env_length w_142 4;
          let ctor_arg_78 = pop_env w_142 in
          push_env w_142 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_78 ]);
          assert_env_length w_142 4;
          drop_n w_142 4 1;
          assert_env_length w_142 3;
          drop_n w_142 3 1;
          assert_env_length w_142 2;
          drop_n w_142 2 0;
          assert_env_length w_142 2;
          drop_n w_142 2 1;
          assert_env_length w_142 1;
          return_n w_142 1 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_142);
          assert_env_length w_142 2;
          push_env w_142 (Memo.from_constructor tag_Missing);
          assert_env_length w_142 3;
          drop_n w_142 3 1;
          assert_env_length w_142 2;
          drop_n w_142 2 0;
          assert_env_length w_142 2;
          drop_n w_142 2 1;
          assert_env_length w_142 1;
          return_n w_142 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (142)")
    142;
  add_exp
    (fun w_144 ->
      assert_env_length w_144 8;
      let x0_30 = resolve w_144 (Source.E 6) in
      let x1_30 = resolve w_144 (Source.E 7) in
      ignore (pop_env w_144);
      ignore (pop_env w_144);
      push_env w_144 (Memo.from_int (Word.get_value (fst x0_30) + Word.get_value (fst x1_30)));
      assert_env_length w_144 7;
      push_env w_144 (Dynarray.get w_144.state.e 2);
      assert_env_length w_144 8;
      ignore (env_call w_144 [] 3);
      w_144.state.c <- pc_to_exp (int_to_pc 61))
    143;
  add_exp
    (fun w_143 ->
      assert_env_length w_143 6;
      let cond_19 = resolve w_143 (Source.E 5) in
      ignore (pop_env w_143);
      if Word.get_value (fst cond_19) <> 0 then (
        assert_env_length w_143 5;
        push_env w_143 (Dynarray.get w_143.state.e 0);
        assert_env_length w_143 6;
        push_env w_143 (Dynarray.get w_143.state.e 1);
        assert_env_length w_143 7;
        push_env w_143 (Dynarray.get w_143.state.e 4);
        w_143.state.c <- pc_to_exp (int_to_pc 143))
      else (
        assert_env_length w_143 5;
        push_env w_143 (Dynarray.get w_143.state.e 3);
        assert_env_length w_143 6;
        push_env w_143 (Dynarray.get w_143.state.e 4);
        assert_env_length w_143 7;
        push_env w_143 (Dynarray.get w_143.state.e 2);
        assert_env_length w_143 8;
        let keep_92 = env_call w_143 [ 0; 1 ] 3 in
        w_143.state.k <- Memo.appends [ Memo.from_constructor tag_cont_93; keep_92; w_143.state.k ];
        w_143.state.c <- pc_to_exp (int_to_pc 61)))
    144;
  add_exp
    (fun w_145 ->
      assert_env_length w_145 2;
      let last_55 = Source.E 1 in
      let x_55 = resolve w_145 last_55 in
      match Word.get_value (fst x_55) with
      | 13 (* tag_NoPick *) ->
          ignore (pop_env w_145);
          assert_env_length w_145 1;
          push_env w_145 (Memo.from_constructor tag_NoPick);
          assert_env_length w_145 2;
          drop_n w_145 2 0;
          assert_env_length w_145 2;
          drop_n w_145 2 1;
          assert_env_length w_145 1;
          return_n w_145 1 (pc_to_exp (int_to_pc 0))
      | 14 (* tag_Pick *) ->
          let splits_77 = Memo.splits (snd x_55) in
          let split0_77 = List.nth splits_77 0 in
          let split1_43 = List.nth splits_77 1 in
          ignore (pop_env w_145);
          push_env w_145 split0_77;
          push_env w_145 split1_43;
          assert_env_length w_145 3;
          push_env w_145 (Dynarray.get w_145.state.e 1);
          assert_env_length w_145 4;
          push_env w_145 (Dynarray.get w_145.state.e 0);
          assert_env_length w_145 5;
          push_env w_145 (Dynarray.get w_145.state.e 2);
          assert_env_length w_145 6;
          let ctor_arg_81 = pop_env w_145 in
          let ctor_arg_82 = pop_env w_145 in
          push_env w_145 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_82; ctor_arg_81 ]);
          assert_env_length w_145 5;
          let ctor_arg_83 = pop_env w_145 in
          let ctor_arg_84 = pop_env w_145 in
          push_env w_145 (Memo.appends [ Memo.from_constructor tag_Pick; ctor_arg_84; ctor_arg_83 ]);
          assert_env_length w_145 4;
          drop_n w_145 4 2;
          assert_env_length w_145 2;
          drop_n w_145 2 0;
          assert_env_length w_145 2;
          drop_n w_145 2 1;
          assert_env_length w_145 1;
          return_n w_145 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (145)")
    145;
  add_exp
    (fun w_149 ->
      assert_env_length w_149 3;
      let cond_21 = resolve w_149 (Source.E 2) in
      ignore (pop_env w_149);
      if Word.get_value (fst cond_21) <> 0 then (
        assert_env_length w_149 2;
        push_env w_149 (Memo.from_int 1);
        assert_env_length w_149 3;
        drop_n w_149 3 1;
        assert_env_length w_149 2;
        drop_n w_149 2 1;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        return_n w_149 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_149 2;
        push_env w_149 (Memo.from_int 0);
        assert_env_length w_149 3;
        drop_n w_149 3 1;
        assert_env_length w_149 2;
        drop_n w_149 2 1;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        drop_n w_149 1 0;
        assert_env_length w_149 1;
        return_n w_149 1 (pc_to_exp (int_to_pc 0))))
    146;
  add_exp
    (fun w_148 ->
      assert_env_length w_148 4;
      let x0_32 = resolve w_148 (Source.E 2) in
      let x1_32 = resolve w_148 (Source.E 3) in
      ignore (pop_env w_148);
      ignore (pop_env w_148);
      push_env w_148 (Memo.from_int (if Word.get_value (fst x0_32) > Word.get_value (fst x1_32) then 1 else 0));
      w_148.state.c <- pc_to_exp (int_to_pc 146))
    147;
  add_exp
    (fun w_150 ->
      assert_env_length w_150 4;
      let x0_33 = resolve w_150 (Source.E 2) in
      let x1_33 = resolve w_150 (Source.E 3) in
      ignore (pop_env w_150);
      ignore (pop_env w_150);
      push_env w_150 (Memo.from_int (Word.get_value (fst x0_33) - Word.get_value (fst x1_33)));
      assert_env_length w_150 3;
      drop_n w_150 3 1;
      assert_env_length w_150 2;
      drop_n w_150 2 1;
      assert_env_length w_150 1;
      drop_n w_150 1 0;
      assert_env_length w_150 1;
      drop_n w_150 1 0;
      assert_env_length w_150 1;
      drop_n w_150 1 0;
      assert_env_length w_150 1;
      drop_n w_150 1 0;
      assert_env_length w_150 1;
      return_n w_150 1 (pc_to_exp (int_to_pc 0)))
    148;
  add_exp
    (fun w_147 ->
      assert_env_length w_147 3;
      let cond_20 = resolve w_147 (Source.E 2) in
      ignore (pop_env w_147);
      if Word.get_value (fst cond_20) <> 0 then (
        assert_env_length w_147 2;
        push_env w_147 (Memo.from_int 0);
        assert_env_length w_147 3;
        push_env w_147 (Memo.from_int 1);
        w_147.state.c <- pc_to_exp (int_to_pc 148))
      else (
        assert_env_length w_147 2;
        push_env w_147 (Dynarray.get w_147.state.e 0);
        assert_env_length w_147 3;
        push_env w_147 (Dynarray.get w_147.state.e 1);
        w_147.state.c <- pc_to_exp (int_to_pc 147)))
    149;
  add_exp
    (fun w_146 ->
      assert_env_length w_146 4;
      let x0_31 = resolve w_146 (Source.E 2) in
      let x1_31 = resolve w_146 (Source.E 3) in
      ignore (pop_env w_146);
      ignore (pop_env w_146);
      push_env w_146 (Memo.from_int (if Word.get_value (fst x0_31) < Word.get_value (fst x1_31) then 1 else 0));
      w_146.state.c <- pc_to_exp (int_to_pc 149))
    150;
  add_exp
    (fun w_152 ->
      assert_env_length w_152 3;
      let cond_22 = resolve w_152 (Source.E 2) in
      ignore (pop_env w_152);
      if Word.get_value (fst cond_22) <> 0 then (
        assert_env_length w_152 2;
        push_env w_152 (Dynarray.get w_152.state.e 1);
        assert_env_length w_152 3;
        drop_n w_152 3 1;
        assert_env_length w_152 2;
        drop_n w_152 2 0;
        assert_env_length w_152 2;
        drop_n w_152 2 1;
        assert_env_length w_152 1;
        drop_n w_152 1 0;
        assert_env_length w_152 1;
        return_n w_152 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_152 2;
        push_env w_152 (Dynarray.get w_152.state.e 0);
        assert_env_length w_152 3;
        push_env w_152 (Dynarray.get w_152.state.e 1);
        assert_env_length w_152 4;
        ignore (env_call w_152 [] 2);
        w_152.state.c <- pc_to_exp (int_to_pc 16)))
    151;
  add_exp
    (fun w_151 ->
      assert_env_length w_151 4;
      let x0_34 = resolve w_151 (Source.E 2) in
      let x1_34 = resolve w_151 (Source.E 3) in
      ignore (pop_env w_151);
      ignore (pop_env w_151);
      push_env w_151 (Memo.from_int (if Word.get_value (fst x0_34) = Word.get_value (fst x1_34) then 1 else 0));
      w_151.state.c <- pc_to_exp (int_to_pc 151))
    152;
  add_exp
    (fun w_154 ->
      assert_env_length w_154 4;
      let cond_23 = resolve w_154 (Source.E 3) in
      ignore (pop_env w_154);
      if Word.get_value (fst cond_23) <> 0 then (
        assert_env_length w_154 3;
        push_env w_154 (Dynarray.get w_154.state.e 2);
        assert_env_length w_154 4;
        drop_n w_154 4 1;
        assert_env_length w_154 3;
        drop_n w_154 3 0;
        assert_env_length w_154 3;
        drop_n w_154 3 0;
        assert_env_length w_154 3;
        drop_n w_154 3 0;
        assert_env_length w_154 3;
        return_n w_154 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_154 3;
        push_env w_154 (Dynarray.get w_154.state.e 1);
        assert_env_length w_154 4;
        push_env w_154 (Dynarray.get w_154.state.e 0);
        assert_env_length w_154 5;
        let keep_98 = env_call w_154 [ 2 ] 2 in
        w_154.state.k <- Memo.appends [ Memo.from_constructor tag_cont_99; keep_98; w_154.state.k ];
        w_154.state.c <- pc_to_exp (int_to_pc 16)))
    153;
  add_exp
    (fun w_153 ->
      assert_env_length w_153 5;
      let x0_35 = resolve w_153 (Source.E 3) in
      let x1_35 = resolve w_153 (Source.E 4) in
      ignore (pop_env w_153);
      ignore (pop_env w_153);
      push_env w_153 (Memo.from_int (if Word.get_value (fst x0_35) = Word.get_value (fst x1_35) then 1 else 0));
      w_153.state.c <- pc_to_exp (int_to_pc 153))
    154;
  add_exp
    (fun w_155 ->
      assert_env_length w_155 2;
      let cond_24 = resolve w_155 (Source.E 1) in
      ignore (pop_env w_155);
      if Word.get_value (fst cond_24) <> 0 then (
        assert_env_length w_155 1;
        push_env w_155 (Dynarray.get w_155.state.e 0);
        assert_env_length w_155 2;
        drop_n w_155 2 1;
        assert_env_length w_155 1;
        drop_n w_155 1 0;
        assert_env_length w_155 1;
        drop_n w_155 1 0;
        assert_env_length w_155 1;
        return_n w_155 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_155 1;
        push_env w_155 (Dynarray.get w_155.state.e 0);
        assert_env_length w_155 2;
        ignore (env_call w_155 [] 1);
        w_155.state.c <- pc_to_exp (int_to_pc 84)))
    155;
  add_exp
    (fun w_156 ->
      assert_env_length w_156 4;
      let cond_25 = resolve w_156 (Source.E 3) in
      ignore (pop_env w_156);
      if Word.get_value (fst cond_25) <> 0 then (
        assert_env_length w_156 3;
        push_env w_156 (Dynarray.get w_156.state.e 1);
        assert_env_length w_156 4;
        drop_n w_156 4 1;
        assert_env_length w_156 3;
        drop_n w_156 3 0;
        assert_env_length w_156 3;
        drop_n w_156 3 0;
        assert_env_length w_156 3;
        drop_n w_156 3 1;
        assert_env_length w_156 2;
        drop_n w_156 2 0;
        assert_env_length w_156 2;
        drop_n w_156 2 0;
        assert_env_length w_156 2;
        drop_n w_156 2 0;
        assert_env_length w_156 2;
        drop_n w_156 2 1;
        assert_env_length w_156 1;
        return_n w_156 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_156 3;
        push_env w_156 (Dynarray.get w_156.state.e 0);
        assert_env_length w_156 4;
        push_env w_156 (Dynarray.get w_156.state.e 2);
        assert_env_length w_156 5;
        ignore (env_call w_156 [] 2);
        w_156.state.c <- pc_to_exp (int_to_pc 79)))
    156;
  add_exp
    (fun w_157 ->
      assert_env_length w_157 4;
      let cond_26 = resolve w_157 (Source.E 3) in
      ignore (pop_env w_157);
      if Word.get_value (fst cond_26) <> 0 then (
        assert_env_length w_157 3;
        push_env w_157 (Dynarray.get w_157.state.e 1);
        assert_env_length w_157 4;
        drop_n w_157 4 1;
        assert_env_length w_157 3;
        drop_n w_157 3 0;
        assert_env_length w_157 3;
        drop_n w_157 3 0;
        assert_env_length w_157 3;
        drop_n w_157 3 1;
        assert_env_length w_157 2;
        drop_n w_157 2 0;
        assert_env_length w_157 2;
        drop_n w_157 2 0;
        assert_env_length w_157 2;
        drop_n w_157 2 0;
        assert_env_length w_157 2;
        drop_n w_157 2 1;
        assert_env_length w_157 1;
        return_n w_157 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_157 3;
        push_env w_157 (Dynarray.get w_157.state.e 0);
        assert_env_length w_157 4;
        push_env w_157 (Dynarray.get w_157.state.e 2);
        assert_env_length w_157 5;
        ignore (env_call w_157 [] 2);
        w_157.state.c <- pc_to_exp (int_to_pc 79)))
    157;
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
  Words.set_constructor_degree 27 (-1);
  Words.set_constructor_degree 28 (-1);
  Words.set_constructor_degree 29 (-1);
  Words.set_constructor_degree 30 (-1);
  Words.set_constructor_degree 31 (-1);
  Words.set_constructor_degree 32 0;
  Words.set_constructor_degree 33 (-1);
  Words.set_constructor_degree 34 0;
  Words.set_constructor_degree 35 (-4);
  Words.set_constructor_degree 36 (-2);
  Words.set_constructor_degree 37 (-4);
  Words.set_constructor_degree 38 (-3);
  Words.set_constructor_degree 39 (-2);
  Words.set_constructor_degree 40 (-1);
  Words.set_constructor_degree 41 0;
  Words.set_constructor_degree 42 0;
  Words.set_constructor_degree 43 (-1);
  Words.set_constructor_degree 44 (-2);
  Words.set_constructor_degree 45 (-2);
  Words.set_constructor_degree 46 (-1);
  Words.set_constructor_degree 47 (-1);
  Words.set_constructor_degree 48 (-1);
  Words.set_constructor_degree 49 (-1);
  Words.set_constructor_degree 50 (-2);
  Words.set_constructor_degree 51 (-3);
  Words.set_constructor_degree 52 (-3);
  Words.set_constructor_degree 53 0;
  Words.set_constructor_degree 54 (-3);
  Words.set_constructor_degree 55 (-1);
  Words.set_constructor_degree 56 (-1);
  Words.set_constructor_degree 57 (-1);
  Words.set_constructor_degree 58 (-1);
  Words.set_constructor_degree 59 (-1);
  Words.set_constructor_degree 60 (-3);
  Words.set_constructor_degree 61 (-3);
  Words.set_constructor_degree 62 (-4);
  Words.set_constructor_degree 63 (-1);
  Words.set_constructor_degree 64 (-1);
  Words.set_constructor_degree 65 (-1);
  Words.set_constructor_degree 66 (-1);
  Words.set_constructor_degree 67 (-1);
  Words.set_constructor_degree 68 (-1);
  Words.set_constructor_degree 69 (-1);
  Words.set_constructor_degree 70 (-1);
  Words.set_constructor_degree 71 (-4);
  Words.set_constructor_degree 72 (-2);
  Words.set_constructor_degree 73 (-4);
  Words.set_constructor_degree 74 (-4);
  Words.set_constructor_degree 75 (-1);
  Words.set_constructor_degree 76 0;
  Words.set_constructor_degree 77 0;
  Words.set_constructor_degree 78 0;
  Words.set_constructor_degree 79 (-1);
  Words.set_constructor_degree 80 (-2);
  Words.set_constructor_degree 81 (-2);
  Words.set_constructor_degree 82 (-1);
  Words.set_constructor_degree 83 (-1);
  Words.set_constructor_degree 84 (-1);
  Words.set_constructor_degree 85 (-1);
  Words.set_constructor_degree 86 (-2);
  Words.set_constructor_degree 87 (-1);
  Words.set_constructor_degree 88 (-1);
  Words.set_constructor_degree 89 0;
  Words.set_constructor_degree 90 (-1);
  Words.set_constructor_degree 91 (-2);
  Words.set_constructor_degree 92 (-2);
  Words.set_constructor_degree 93 (-2);
  Words.set_constructor_degree 94 (-1);
  Words.set_constructor_degree 95 (-1);
  Words.set_constructor_degree 96 (-5);
  Words.set_constructor_degree 97 0;
  Words.set_constructor_degree 98 (-1);
  Words.set_constructor_degree 99 (-1);
  Words.set_constructor_degree 100 0;
  Words.set_constructor_degree 101 0;
  Words.set_constructor_degree 102 (-3);
  Words.set_constructor_degree 103 (-3);
  Words.set_constructor_degree 104 (-1);
  Words.set_constructor_degree 105 (-1);
  Words.set_constructor_degree 106 (-1);
  Words.set_constructor_degree 107 (-2);
  Words.set_constructor_degree 108 0;
  Words.set_constructor_degree 109 (-1);
  Words.set_constructor_degree 110 (-2);
  Words.set_constructor_degree 111 (-2);
  Words.set_constructor_degree 112 (-1);
  Words.set_constructor_degree 113 (-1);
  Words.set_constructor_degree 114 0;
  Words.set_constructor_degree 115 (-1);
  Words.set_constructor_degree 116 (-1);
  Words.set_constructor_degree 117 (-2);
  Words.set_constructor_degree 118 (-2);
  Words.set_constructor_degree 119 0;
  Words.set_constructor_degree 120 (-1);
  Words.set_constructor_degree 121 (-2);
  Words.set_constructor_degree 122 (-2);
  Words.set_constructor_degree 123 0;
  Words.set_constructor_degree 124 (-1);
  Words.set_constructor_degree 125 (-3);
  Words.set_constructor_degree 126 (-3);
  Words.set_constructor_degree 127 0;
  Words.set_constructor_degree 128 0;
  Words.set_constructor_degree 129 0;
  Words.set_constructor_degree 130 0
