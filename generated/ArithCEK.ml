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
let tag_cont_119 = 133
let tag_cont_120 = 134
let tag_cont_121 = 135
let tag_cont_122 = 136
let tag_cont_123 = 137
let tag_cont_124 = 138
let tag_cont_125 = 139
let tag_cont_126 = 140
let tag_cont_127 = 141
let tag_cont_128 = 142
let tag_cont_129 = 143

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

type expr = Const of nat | Var of var | Add of expr * expr | Mul of expr * expr

let rec from_ocaml_expr x =
  match x with
  | Const x0 -> Memo.appends [ Memo.from_constructor tag_Const; from_ocaml_nat x0 ]
  | Var x0 -> Memo.appends [ Memo.from_constructor tag_Var; from_ocaml_var x0 ]
  | Add (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Add; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | Mul (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Mul; from_ocaml_expr x0; from_ocaml_expr x1 ]

let rec to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 5 (* tag_Const *) ->
      let x0 = Memo.splits_1 t in
      Const (to_ocaml_nat x0)
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

let rec nat_add memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec nat_mul memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec nat_compare memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec nat_equal memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 10)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec nat_is_zero memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 11)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec nat_is_one memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 13)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec var_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 16)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_rank memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 18)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec compare_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 20)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_equal memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 21)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_size memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 27)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec better_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 29)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec scale memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 30)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec coeff_value memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 31)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec coeff_base memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 34)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec extract_factor memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 37)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_factor memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 38)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec append_exprs memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 40)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec insert_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 42)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec sort_exprs memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 44)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec reverse_exprs_aux memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 46)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec reverse_exprs memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 48)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec flatten_add memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 49)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec flatten_mul memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 51)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_coeff memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 53)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_base memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 56)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_total_coeff memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 59)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec mul_bases memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 61)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec build_mul memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 63)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize_mul_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 66)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec combine_like_terms_acc memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 67)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec combine_like_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 69)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec factor_adjacent memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 71)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec pick_factored memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 74)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_terms memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 76)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec build_add memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 78)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_round memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 81)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize_add_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 82)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec search_opt memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 83)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec normalize memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 89)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec simplify_aux memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 91)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec diffx memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 92)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 95)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec main memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 98)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_98 ->
      assert_env_length w_98 1;
      let hd_0, tl_0 = resolve w_98 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_98
      | 15 (* tag_cont_1 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          let ctor_arg_28 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_28 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 16 (* tag_cont_2 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 1)
      | 17 (* tag_cont_3 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Memo.from_int 0);
          w_98.state.c <- pc_to_exp (int_to_pc 99)
      | 18 (* tag_cont_4 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let keep_42 = env_call w_98 [ 0; 1; 2 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_42; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 18)
      | 19 (* tag_cont_5 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_43 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_43; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 16)
      | 20 (* tag_cont_6 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_44 = env_call w_98 [ 2 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_44; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 21 (* tag_cont_7 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_45 = env_call w_98 [ 2 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_45; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 22 (* tag_cont_8 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 100)
      | 23 (* tag_cont_9 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 101)
      | 24 (* tag_cont_10 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let keep_48 = env_call w_98 [ 0; 1; 2 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; keep_48; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 27)
      | 25 (* tag_cont_11 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 103)
      | 26 (* tag_cont_12 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 105)
      | 27 (* tag_cont_13 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 4);
          w_98.state.c <- pc_to_exp (int_to_pc 106)
      | 28 (* tag_cont_14 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_37 = pop_env w_98 in
          let ctor_arg_38 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_38; ctor_arg_37 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 29 (* tag_cont_15 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          assert_env_length w_98 5;
          push_env w_98 (Memo.from_int 0);
          w_98.state.c <- pc_to_exp (int_to_pc 108)
      | 30 (* tag_cont_16 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 42)
      | 31 (* tag_cont_17 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_54 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; keep_54; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 49)
      | 32 (* tag_cont_18 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 109)
      | 33 (* tag_cont_19 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_55 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_58; keep_55; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 51)
      | 34 (* tag_cont_20 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_56 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_59; keep_56; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 59)
      | 35 (* tag_cont_21 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          w_98.state.c <- pc_to_exp (int_to_pc 110)
      | 36 (* tag_cont_22 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_43 = pop_env w_98 in
          let ctor_arg_44 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_44; ctor_arg_43 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 37 (* tag_cont_23 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_59 = env_call w_98 [ 0 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_62; keep_59; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 59)
      | 38 (* tag_cont_24 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_60 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_63; keep_60; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 51)
      | 39 (* tag_cont_25 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 111)
      | 40 (* tag_cont_26 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 6;
          let keep_62 = env_call w_98 [ 0; 1; 3; 4 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_65; keep_62; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 31)
      | 41 (* tag_cont_27 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          let keep_63 = env_call w_98 [ 1; 2 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_66; keep_63; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 31)
      | 42 (* tag_cont_28 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 4);
          assert_env_length w_98 6;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 7;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 8;
          let ctor_arg_45 = pop_env w_98 in
          let ctor_arg_46 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_46; ctor_arg_45 ]);
          assert_env_length w_98 7;
          let keep_64 = env_call w_98 [ 0; 1; 3; 4 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_67; keep_64; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 43 (* tag_cont_29 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 3);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 6;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 7;
          let ctor_arg_47 = pop_env w_98 in
          let ctor_arg_48 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_48; ctor_arg_47 ]);
          assert_env_length w_98 6;
          let keep_65 = env_call w_98 [ 0; 1; 2; 3 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_68; keep_65; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 44 (* tag_cont_30 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 112)
      | 45 (* tag_cont_31 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_49 = pop_env w_98 in
          let ctor_arg_50 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_50; ctor_arg_49 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 46 (* tag_cont_32 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_68 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_71; keep_68; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 76)
      | 47 (* tag_cont_33 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_69 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_72; keep_69; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 44)
      | 48 (* tag_cont_34 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_70 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_73; keep_70; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 49)
      | 49 (* tag_cont_35 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          push_env w_98 (Memo.from_int 1);
          w_98.state.c <- pc_to_exp (int_to_pc 113)
      | 50 (* tag_cont_36 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          push_env w_98 (Memo.from_int 1);
          w_98.state.c <- pc_to_exp (int_to_pc 114)
      | 51 (* tag_cont_37 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_73 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_76; keep_73; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 89)
      | 52 (* tag_cont_38 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_74 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_77; keep_74; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 89)
      | 53 (* tag_cont_39 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Memo.from_int 6);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let keep_75 = env_call w_98 [ 0 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_78; keep_75; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 83)
      | 54 (* tag_cont_40 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_76 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_79; keep_76; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 92)
      | 55 (* tag_cont_41 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let ctor_arg_51 = pop_env w_98 in
          let ctor_arg_52 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_52; ctor_arg_51 ]);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_77 = env_call w_98 [ 2; 3 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_80; keep_77; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 92)
      | 56 (* tag_cont_42 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 6;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 7;
          let keep_78 = env_call w_98 [ 3 ] 3 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_81; keep_78; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 95)
      | 57 (* tag_cont_43 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 6;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 7;
          let keep_79 = env_call w_98 [ 3 ] 3 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_82; keep_79; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 95)
      | 58 (* tag_cont_44 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_80 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_83; keep_80; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 92)
      | 59 (* tag_cont_45 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 3);
          w_98.state.c <- pc_to_exp (int_to_pc 124)
      | 60 (* tag_cont_46 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 125)
      | 61 (* tag_cont_47 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 126)
      | 62 (* tag_cont_48 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 127)
      | 63 (* tag_cont_49 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 128)
      | 64 (* tag_cont_50 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 129)
      | 65 (* tag_cont_51 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 3);
          w_98.state.c <- pc_to_exp (int_to_pc 133)
      | 66 (* tag_cont_52 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          let ctor_arg_53 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_53 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 67 (* tag_cont_53 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 134)
      | 68 (* tag_cont_54 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 3);
          w_98.state.c <- pc_to_exp (int_to_pc 135)
      | 69 (* tag_cont_55 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 4);
          w_98.state.c <- pc_to_exp (int_to_pc 136)
      | 70 (* tag_cont_56 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_66 = pop_env w_98 in
          let ctor_arg_67 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_67; ctor_arg_66 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 71 (* tag_cont_57 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 40)
      | 72 (* tag_cont_58 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 40)
      | 73 (* tag_cont_59 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 3)
      | 74 (* tag_cont_60 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 137)
      | 75 (* tag_cont_61 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 42)
      | 76 (* tag_cont_62 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 3;
          let keep_87 = env_call w_98 [ 0; 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_90; keep_87; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 11)
      | 77 (* tag_cont_63 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let keep_88 = env_call w_98 [] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_88; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 40)
      | 78 (* tag_cont_64 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Memo.from_constructor tag_ENil);
          assert_env_length w_98 2;
          let ctor_arg_68 = pop_env w_98 in
          let ctor_arg_69 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_69; ctor_arg_68 ]);
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 79 (* tag_cont_65 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 6;
          push_env w_98 (Dynarray.get w_98.state.e 3);
          assert_env_length w_98 7;
          let keep_89 = env_call w_98 [ 0; 1; 2; 3; 4 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_91; keep_89; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 80 (* tag_cont_66 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 6;
          ignore (env_call w_98 [] 3);
          w_98.state.c <- pc_to_exp (int_to_pc 67)
      | 81 (* tag_cont_67 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 138)
      | 82 (* tag_cont_68 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 4 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 139)
      | 83 (* tag_cont_69 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_72 = pop_env w_98 in
          let ctor_arg_73 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_73; ctor_arg_72 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 84 (* tag_cont_70 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          ignore (env_call w_98 [] 1);
          w_98.state.c <- pc_to_exp (int_to_pc 76)
      | 85 (* tag_cont_71 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_93 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_95; keep_93; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 71)
      | 86 (* tag_cont_72 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_94 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_96; keep_94; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 69)
      | 87 (* tag_cont_73 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let keep_95 = env_call w_98 [] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_95; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 40)
      | 88 (* tag_cont_74 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          let keep_96 = env_call w_98 [ 0; 1; 2 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_97; keep_96; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 82)
      | 89 (* tag_cont_75 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          let keep_97 = env_call w_98 [ 0; 1; 2 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_98; keep_97; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 66)
      | 90 (* tag_cont_76 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 82)
      | 91 (* tag_cont_77 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 66)
      | 92 (* tag_cont_78 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 3;
          let keep_98 = env_call w_98 [ 0 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_99; keep_98; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 89)
      | 93 (* tag_cont_79 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_74 = pop_env w_98 in
          let ctor_arg_75 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_75; ctor_arg_74 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 94 (* tag_cont_80 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          let ctor_arg_76 = pop_env w_98 in
          let ctor_arg_77 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_77; ctor_arg_76 ]);
          assert_env_length w_98 2;
          let ctor_arg_78 = pop_env w_98 in
          let ctor_arg_79 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_79; ctor_arg_78 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 95 (* tag_cont_81 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 1)
      | 96 (* tag_cont_82 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 3)
      | 97 (* tag_cont_83 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 1);
          w_98.state.c <- pc_to_exp (int_to_pc 91)
      | 98 (* tag_cont_84 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_99 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_100; keep_99; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 16)
      | 99 (* tag_cont_85 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 4;
          push_env w_98 (Memo.from_int 0);
          w_98.state.c <- pc_to_exp (int_to_pc 141)
      | 100 (* tag_cont_86 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 4;
          push_env w_98 (Memo.from_int 0);
          w_98.state.c <- pc_to_exp (int_to_pc 143)
      | 101 (* tag_cont_87 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Memo.from_int 0);
          w_98.state.c <- pc_to_exp (int_to_pc 145)
      | 102 (* tag_cont_88 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          w_98.state.c <- pc_to_exp (int_to_pc 146)
      | 103 (* tag_cont_89 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 42)
      | 104 (* tag_cont_90 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 147)
      | 105 (* tag_cont_91 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 5 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 148)
      | 106 (* tag_cont_92 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          ignore (env_call w_98 [] 1);
          w_98.state.c <- pc_to_exp (int_to_pc 71)
      | 107 (* tag_cont_93 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let ctor_arg_84 = pop_env w_98 in
          let ctor_arg_85 = pop_env w_98 in
          push_env w_98 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_85; ctor_arg_84 ]);
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          drop_n w_98 1 0;
          assert_env_length w_98 1;
          return_n w_98 1 (pc_to_exp (int_to_pc 0))
      | 108 (* tag_cont_94 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 149)
      | 109 (* tag_cont_95 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_103 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_104; keep_103; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 76)
      | 110 (* tag_cont_96 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_104 = env_call w_98 [ 0 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_105; keep_104; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 81)
      | 111 (* tag_cont_97 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 6;
          let keep_105 = env_call w_98 [ 0; 3 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_106; keep_105; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 82)
      | 112 (* tag_cont_98 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 6;
          let keep_106 = env_call w_98 [ 0; 3 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_107; keep_106; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 66)
      | 113 (* tag_cont_99 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let keep_107 = env_call w_98 [ 1 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_108; keep_107; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 114 (* tag_cont_100 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          w_98.state.c <- pc_to_exp (int_to_pc 154)
      | 115 (* tag_cont_101 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 3;
          let keep_108 = env_call w_98 [ 0 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_109; keep_108; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 63)
      | 116 (* tag_cont_102 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let keep_109 = env_call w_98 [ 0; 1; 2 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_110; keep_109; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 11)
      | 117 (* tag_cont_103 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 4;
          ignore (env_call w_98 [] 3);
          w_98.state.c <- pc_to_exp (int_to_pc 67)
      | 118 (* tag_cont_104 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_110 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_111; keep_110; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 71)
      | 119 (* tag_cont_105 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_111 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_113; keep_111; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 48)
      | 120 (* tag_cont_106 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          let keep_112 = env_call w_98 [ 0; 1 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_114; keep_112; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 82)
      | 121 (* tag_cont_107 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 5;
          let keep_113 = env_call w_98 [ 0; 1 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_115; keep_113; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 66)
      | 122 (* tag_cont_108 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 155)
      | 123 (* tag_cont_109 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_114 = env_call w_98 [ 0; 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_116; keep_114; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 13)
      | 124 (* tag_cont_110 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 156)
      | 125 (* tag_cont_111 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_116 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_118; keep_116; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 76)
      | 126 (* tag_cont_112 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_117 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_119; keep_117; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 78)
      | 127 (* tag_cont_113 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          let keep_118 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_112; keep_118; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 81)
      | 128 (* tag_cont_114 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_119 = env_call w_98 [ 0; 1 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_120; keep_119; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 82)
      | 129 (* tag_cont_115 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_120 = env_call w_98 [ 0; 1 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_121; keep_120; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 66)
      | 130 (* tag_cont_116 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 157)
      | 131 (* tag_cont_117 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          ignore (env_call w_98 [] 2);
          w_98.state.c <- pc_to_exp (int_to_pc 42)
      | 132 (* tag_cont_118 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_121 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_122; keep_121; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 71)
      | 133 (* tag_cont_119 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          let keep_122 = env_call w_98 [ 1 ] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_123; keep_122; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 78)
      | 134 (* tag_cont_120 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_123 = env_call w_98 [ 0; 1; 2 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_124; keep_123; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 135 (* tag_cont_121 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 2 tl_0;
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 2);
          assert_env_length w_98 4;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 5;
          let keep_124 = env_call w_98 [ 0; 1; 2 ] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_125; keep_124; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 21)
      | 136 (* tag_cont_122 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 1);
          w_98.state.c <- pc_to_exp (int_to_pc 76)
      | 137 (* tag_cont_123 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 1 tl_0;
          assert_env_length w_98 2;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 3;
          push_env w_98 (Dynarray.get w_98.state.e 1);
          assert_env_length w_98 4;
          let keep_125 = env_call w_98 [] 2 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_126; keep_125; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 29)
      | 138 (* tag_cont_124 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 159)
      | 139 (* tag_cont_125 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 3 tl_0;
          w_98.state.c <- pc_to_exp (int_to_pc 161)
      | 140 (* tag_cont_126 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          let keep_126 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_129; keep_126; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 49)
      | 141 (* tag_cont_127 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          push_env w_98 (Dynarray.get w_98.state.e 0);
          assert_env_length w_98 2;
          ignore (env_call w_98 [] 1);
          w_98.state.c <- pc_to_exp (int_to_pc 78)
      | 142 (* tag_cont_128 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          let keep_127 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_127; keep_127; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 69)
      | 143 (* tag_cont_129 *) ->
          w_98.state.k <- get_next_cont tl_0;
          restore_env w_98 0 tl_0;
          assert_env_length w_98 1;
          let keep_128 = env_call w_98 [] 1 in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_128; keep_128; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 44)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 2;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 2))
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      let last_0 = Source.E 2 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_1);
          assert_env_length w_1 2;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          assert_env_length w_1 3;
          return_n w_1 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          ignore (pop_env w_1);
          push_env w_1 split0_0;
          assert_env_length w_1 3;
          push_env w_1 (Dynarray.get w_1.state.e 2);
          assert_env_length w_1 4;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          assert_env_length w_1 5;
          let keep_0 = env_call w_1 [] 2 in
          w_1.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_1.state.k ];
          w_1.state.c <- pc_to_exp (int_to_pc 1)
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      push_env w_2 (Dynarray.get w_2.state.e 0);
      w_2.state.c <- pc_to_exp (int_to_pc 4))
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 3;
      let last_1 = Source.E 2 in
      let x_1 = resolve w_3 last_1 in
      match Word.get_value (fst x_1) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_3);
          assert_env_length w_3 2;
          push_env w_3 (Memo.from_constructor tag_Z);
          assert_env_length w_3 3;
          return_n w_3 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          ignore (pop_env w_3);
          push_env w_3 split0_1;
          assert_env_length w_3 3;
          push_env w_3 (Dynarray.get w_3.state.e 1);
          assert_env_length w_3 4;
          push_env w_3 (Dynarray.get w_3.state.e 2);
          assert_env_length w_3 5;
          push_env w_3 (Dynarray.get w_3.state.e 1);
          assert_env_length w_3 6;
          let keep_1 = env_call w_3 [ 3 ] 2 in
          w_3.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_3.state.k ];
          w_3.state.c <- pc_to_exp (int_to_pc 3)
      | _ -> failwith "unreachable (4)")
    4;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 2;
      push_env w_4 (Dynarray.get w_4.state.e 0);
      w_4.state.c <- pc_to_exp (int_to_pc 9))
    5;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 5;
      let x0_0 = resolve w_7 (Source.E 3) in
      let x1_0 = resolve w_7 (Source.E 4) in
      ignore (pop_env w_7);
      ignore (pop_env w_7);
      push_env w_7 (Memo.from_int (Word.get_value (fst x0_0) - Word.get_value (fst x1_0)));
      assert_env_length w_7 4;
      drop_n w_7 4 1;
      assert_env_length w_7 3;
      return_n w_7 3 (pc_to_exp (int_to_pc 0)))
    6;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 3;
      let last_3 = Source.E 2 in
      let x_3 = resolve w_6 last_3 in
      match Word.get_value (fst x_3) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_6);
          assert_env_length w_6 2;
          push_env w_6 (Memo.from_int 0);
          assert_env_length w_6 3;
          return_n w_6 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_2 = Memo.splits (snd x_3) in
          let split0_2 = List.nth splits_2 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_2;
          assert_env_length w_6 3;
          push_env w_6 (Memo.from_int 0);
          assert_env_length w_6 4;
          push_env w_6 (Memo.from_int 1);
          w_6.state.c <- pc_to_exp (int_to_pc 6)
      | _ -> failwith "unreachable (7)")
    7;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 4;
      let last_4 = Source.E 3 in
      let x_4 = resolve w_8 last_4 in
      match Word.get_value (fst x_4) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_8);
          assert_env_length w_8 3;
          push_env w_8 (Memo.from_int 1);
          assert_env_length w_8 4;
          drop_n w_8 4 1;
          assert_env_length w_8 3;
          return_n w_8 3 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_4 = Memo.splits (snd x_4) in
          let split0_4 = List.nth splits_4 0 in
          ignore (pop_env w_8);
          push_env w_8 split0_4;
          assert_env_length w_8 4;
          push_env w_8 (Dynarray.get w_8.state.e 2);
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 3);
          assert_env_length w_8 6;
          ignore (env_call w_8 [] 2);
          w_8.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (8)")
    8;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 3;
      let last_2 = Source.E 2 in
      let x_2 = resolve w_5 last_2 in
      match Word.get_value (fst x_2) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_5);
          assert_env_length w_5 2;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          w_5.state.c <- pc_to_exp (int_to_pc 7)
      | 2 (* tag_S *) ->
          let splits_3 = Memo.splits (snd x_2) in
          let split0_3 = List.nth splits_3 0 in
          ignore (pop_env w_5);
          push_env w_5 split0_3;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          w_5.state.c <- pc_to_exp (int_to_pc 8)
      | _ -> failwith "unreachable (9)")
    9;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 2;
      push_env w_9 (Dynarray.get w_9.state.e 0);
      assert_env_length w_9 3;
      push_env w_9 (Dynarray.get w_9.state.e 1);
      assert_env_length w_9 4;
      let keep_2 = env_call w_9 [] 2 in
      w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_9.state.k ];
      w_9.state.c <- pc_to_exp (int_to_pc 5))
    10;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 1;
      push_env w_10 (Dynarray.get w_10.state.e 0);
      w_10.state.c <- pc_to_exp (int_to_pc 12))
    11;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 2;
      let last_5 = Source.E 1 in
      let x_5 = resolve w_11 last_5 in
      match Word.get_value (fst x_5) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_11);
          assert_env_length w_11 1;
          push_env w_11 (Memo.from_int 1);
          assert_env_length w_11 2;
          return_n w_11 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_5 = Memo.splits (snd x_5) in
          let split0_5 = List.nth splits_5 0 in
          ignore (pop_env w_11);
          push_env w_11 split0_5;
          assert_env_length w_11 2;
          push_env w_11 (Memo.from_int 0);
          assert_env_length w_11 3;
          drop_n w_11 3 1;
          assert_env_length w_11 2;
          return_n w_11 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (12)")
    12;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 1;
      push_env w_12 (Dynarray.get w_12.state.e 0);
      w_12.state.c <- pc_to_exp (int_to_pc 15))
    13;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 3;
      let last_7 = Source.E 2 in
      let x_7 = resolve w_14 last_7 in
      match Word.get_value (fst x_7) with
      | 1 (* tag_Z *) ->
          ignore (pop_env w_14);
          assert_env_length w_14 2;
          push_env w_14 (Memo.from_int 1);
          assert_env_length w_14 3;
          drop_n w_14 3 1;
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_7 = Memo.splits (snd x_7) in
          let split0_7 = List.nth splits_7 0 in
          ignore (pop_env w_14);
          push_env w_14 split0_7;
          assert_env_length w_14 3;
          push_env w_14 (Memo.from_int 0);
          assert_env_length w_14 4;
          drop_n w_14 4 1;
          assert_env_length w_14 3;
          drop_n w_14 3 1;
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (14)")
    14;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 2;
      let last_6 = Source.E 1 in
      let x_6 = resolve w_13 last_6 in
      match Word.get_value (fst x_6) with
      | 2 (* tag_S *) ->
          let splits_6 = Memo.splits (snd x_6) in
          let split0_6 = List.nth splits_6 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_6;
          assert_env_length w_13 2;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          w_13.state.c <- pc_to_exp (int_to_pc 14)
      | 1 (* tag_Z *) ->
          ignore (pop_env w_13);
          assert_env_length w_13 1;
          push_env w_13 (Memo.from_int 0);
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (15)")
    15;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 1;
      push_env w_15 (Dynarray.get w_15.state.e 0);
      w_15.state.c <- pc_to_exp (int_to_pc 17))
    16;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 2;
      let last_8 = Source.E 1 in
      let x_8 = resolve w_16 last_8 in
      match Word.get_value (fst x_8) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_16);
          assert_env_length w_16 1;
          push_env w_16 (Memo.from_int 0);
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_16);
          assert_env_length w_16 1;
          push_env w_16 (Memo.from_int 1);
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (17)")
    17;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 1;
      push_env w_17 (Dynarray.get w_17.state.e 0);
      w_17.state.c <- pc_to_exp (int_to_pc 19))
    18;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 2;
      let last_9 = Source.E 1 in
      let x_9 = resolve w_18 last_9 in
      match Word.get_value (fst x_9) with
      | 5 (* tag_Const *) ->
          let splits_8 = Memo.splits (snd x_9) in
          let split0_8 = List.nth splits_8 0 in
          ignore (pop_env w_18);
          push_env w_18 split0_8;
          assert_env_length w_18 2;
          push_env w_18 (Memo.from_int 0);
          assert_env_length w_18 3;
          drop_n w_18 3 1;
          assert_env_length w_18 2;
          return_n w_18 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_9 = Memo.splits (snd x_9) in
          let split0_9 = List.nth splits_9 0 in
          ignore (pop_env w_18);
          push_env w_18 split0_9;
          assert_env_length w_18 2;
          push_env w_18 (Memo.from_int 1);
          assert_env_length w_18 3;
          drop_n w_18 3 1;
          assert_env_length w_18 2;
          return_n w_18 2 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_10 = Memo.splits (snd x_9) in
          let split0_10 = List.nth splits_10 0 in
          let split1_0 = List.nth splits_10 1 in
          ignore (pop_env w_18);
          push_env w_18 split0_10;
          push_env w_18 split1_0;
          assert_env_length w_18 3;
          push_env w_18 (Memo.from_int 2);
          assert_env_length w_18 4;
          drop_n w_18 4 2;
          assert_env_length w_18 2;
          return_n w_18 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_11 = Memo.splits (snd x_9) in
          let split0_11 = List.nth splits_11 0 in
          let split1_1 = List.nth splits_11 1 in
          ignore (pop_env w_18);
          push_env w_18 split0_11;
          push_env w_18 split1_1;
          assert_env_length w_18 3;
          push_env w_18 (Memo.from_int 3);
          assert_env_length w_18 4;
          drop_n w_18 4 2;
          assert_env_length w_18 2;
          return_n w_18 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (19)")
    19;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 2;
      push_env w_19 (Dynarray.get w_19.state.e 0);
      assert_env_length w_19 3;
      let keep_3 = env_call w_19 [ 0; 1 ] 1 in
      w_19.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_19.state.k ];
      w_19.state.c <- pc_to_exp (int_to_pc 18))
    20;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 2;
      push_env w_20 (Dynarray.get w_20.state.e 0);
      w_20.state.c <- pc_to_exp (int_to_pc 26))
    21;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 4;
      let last_11 = Source.E 3 in
      let x_11 = resolve w_22 last_11 in
      match Word.get_value (fst x_11) with
      | 5 (* tag_Const *) ->
          let splits_13 = Memo.splits (snd x_11) in
          let split0_13 = List.nth splits_13 0 in
          ignore (pop_env w_22);
          push_env w_22 split0_13;
          assert_env_length w_22 4;
          push_env w_22 (Dynarray.get w_22.state.e 2);
          assert_env_length w_22 5;
          push_env w_22 (Dynarray.get w_22.state.e 3);
          assert_env_length w_22 6;
          ignore (env_call w_22 [] 2);
          w_22.state.c <- pc_to_exp (int_to_pc 10)
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
      let last_12 = Source.E 3 in
      let x_12 = resolve w_23 last_12 in
      match Word.get_value (fst x_12) with
      | 6 (* tag_Var *) ->
          let splits_15 = Memo.splits (snd x_12) in
          let split0_15 = List.nth splits_15 0 in
          ignore (pop_env w_23);
          push_env w_23 split0_15;
          assert_env_length w_23 4;
          push_env w_23 (Dynarray.get w_23.state.e 2);
          assert_env_length w_23 5;
          let keep_4 = env_call w_23 [ 3 ] 1 in
          w_23.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_23.state.k ];
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
    (fun w_24 ->
      assert_env_length w_24 5;
      let last_13 = Source.E 4 in
      let x_13 = resolve w_24 last_13 in
      match Word.get_value (fst x_13) with
      | 7 (* tag_Add *) ->
          let splits_17 = Memo.splits (snd x_13) in
          let split0_17 = List.nth splits_17 0 in
          let split1_3 = List.nth splits_17 1 in
          ignore (pop_env w_24);
          push_env w_24 split0_17;
          push_env w_24 split1_3;
          assert_env_length w_24 6;
          push_env w_24 (Dynarray.get w_24.state.e 2);
          assert_env_length w_24 7;
          push_env w_24 (Dynarray.get w_24.state.e 4);
          assert_env_length w_24 8;
          let keep_5 = env_call w_24 [ 3; 5 ] 2 in
          w_24.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_24.state.k ];
          w_24.state.c <- pc_to_exp (int_to_pc 21)
      | _ ->
          ignore (pop_env w_24);
          assert_env_length w_24 4;
          push_env w_24 (Memo.from_int 0);
          assert_env_length w_24 5;
          drop_n w_24 5 2;
          assert_env_length w_24 3;
          return_n w_24 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (24)")
    24;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 5;
      let last_14 = Source.E 4 in
      let x_14 = resolve w_25 last_14 in
      match Word.get_value (fst x_14) with
      | 8 (* tag_Mul *) ->
          let splits_19 = Memo.splits (snd x_14) in
          let split0_19 = List.nth splits_19 0 in
          let split1_5 = List.nth splits_19 1 in
          ignore (pop_env w_25);
          push_env w_25 split0_19;
          push_env w_25 split1_5;
          assert_env_length w_25 6;
          push_env w_25 (Dynarray.get w_25.state.e 2);
          assert_env_length w_25 7;
          push_env w_25 (Dynarray.get w_25.state.e 4);
          assert_env_length w_25 8;
          let keep_6 = env_call w_25 [ 3; 5 ] 2 in
          w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_25.state.k ];
          w_25.state.c <- pc_to_exp (int_to_pc 21)
      | _ ->
          ignore (pop_env w_25);
          assert_env_length w_25 4;
          push_env w_25 (Memo.from_int 0);
          assert_env_length w_25 5;
          drop_n w_25 5 2;
          assert_env_length w_25 3;
          return_n w_25 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (25)")
    25;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 3;
      let last_10 = Source.E 2 in
      let x_10 = resolve w_21 last_10 in
      match Word.get_value (fst x_10) with
      | 5 (* tag_Const *) ->
          let splits_12 = Memo.splits (snd x_10) in
          let split0_12 = List.nth splits_12 0 in
          ignore (pop_env w_21);
          push_env w_21 split0_12;
          assert_env_length w_21 3;
          push_env w_21 (Dynarray.get w_21.state.e 1);
          w_21.state.c <- pc_to_exp (int_to_pc 22)
      | 6 (* tag_Var *) ->
          let splits_14 = Memo.splits (snd x_10) in
          let split0_14 = List.nth splits_14 0 in
          ignore (pop_env w_21);
          push_env w_21 split0_14;
          assert_env_length w_21 3;
          push_env w_21 (Dynarray.get w_21.state.e 1);
          w_21.state.c <- pc_to_exp (int_to_pc 23)
      | 7 (* tag_Add *) ->
          let splits_16 = Memo.splits (snd x_10) in
          let split0_16 = List.nth splits_16 0 in
          let split1_2 = List.nth splits_16 1 in
          ignore (pop_env w_21);
          push_env w_21 split0_16;
          push_env w_21 split1_2;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 1);
          w_21.state.c <- pc_to_exp (int_to_pc 24)
      | 8 (* tag_Mul *) ->
          let splits_18 = Memo.splits (snd x_10) in
          let split0_18 = List.nth splits_18 0 in
          let split1_4 = List.nth splits_18 1 in
          ignore (pop_env w_21);
          push_env w_21 split0_18;
          push_env w_21 split1_4;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 1);
          w_21.state.c <- pc_to_exp (int_to_pc 25)
      | _ -> failwith "unreachable (26)")
    26;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 1;
      push_env w_26 (Dynarray.get w_26.state.e 0);
      w_26.state.c <- pc_to_exp (int_to_pc 28))
    27;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 2;
      let last_15 = Source.E 1 in
      let x_15 = resolve w_27 last_15 in
      match Word.get_value (fst x_15) with
      | 5 (* tag_Const *) ->
          let splits_20 = Memo.splits (snd x_15) in
          let split0_20 = List.nth splits_20 0 in
          ignore (pop_env w_27);
          push_env w_27 split0_20;
          assert_env_length w_27 2;
          push_env w_27 (Memo.from_int 1);
          assert_env_length w_27 3;
          drop_n w_27 3 1;
          assert_env_length w_27 2;
          return_n w_27 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_21 = Memo.splits (snd x_15) in
          let split0_21 = List.nth splits_21 0 in
          ignore (pop_env w_27);
          push_env w_27 split0_21;
          assert_env_length w_27 2;
          push_env w_27 (Memo.from_int 1);
          assert_env_length w_27 3;
          drop_n w_27 3 1;
          assert_env_length w_27 2;
          return_n w_27 2 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_22 = Memo.splits (snd x_15) in
          let split0_22 = List.nth splits_22 0 in
          let split1_6 = List.nth splits_22 1 in
          ignore (pop_env w_27);
          push_env w_27 split0_22;
          push_env w_27 split1_6;
          assert_env_length w_27 3;
          push_env w_27 (Memo.from_int 1);
          assert_env_length w_27 4;
          push_env w_27 (Dynarray.get w_27.state.e 1);
          assert_env_length w_27 5;
          let keep_7 = env_call w_27 [ 2; 3 ] 1 in
          w_27.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_27.state.k ];
          w_27.state.c <- pc_to_exp (int_to_pc 27)
      | 8 (* tag_Mul *) ->
          let splits_23 = Memo.splits (snd x_15) in
          let split0_23 = List.nth splits_23 0 in
          let split1_7 = List.nth splits_23 1 in
          ignore (pop_env w_27);
          push_env w_27 split0_23;
          push_env w_27 split1_7;
          assert_env_length w_27 3;
          push_env w_27 (Memo.from_int 1);
          assert_env_length w_27 4;
          push_env w_27 (Dynarray.get w_27.state.e 1);
          assert_env_length w_27 5;
          let keep_8 = env_call w_27 [ 2; 3 ] 1 in
          w_27.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_27.state.k ];
          w_27.state.c <- pc_to_exp (int_to_pc 27)
      | _ -> failwith "unreachable (28)")
    28;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 2;
      push_env w_28 (Dynarray.get w_28.state.e 0);
      assert_env_length w_28 3;
      let keep_9 = env_call w_28 [ 0; 1 ] 1 in
      w_28.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_28.state.k ];
      w_28.state.c <- pc_to_exp (int_to_pc 27))
    29;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 2;
      push_env w_29 (Dynarray.get w_29.state.e 0);
      assert_env_length w_29 3;
      let keep_10 = env_call w_29 [ 0; 1 ] 1 in
      w_29.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_29.state.k ];
      w_29.state.c <- pc_to_exp (int_to_pc 11))
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
      let last_17 = Source.E 3 in
      let x_17 = resolve w_32 last_17 in
      match Word.get_value (fst x_17) with
      | 5 (* tag_Const *) ->
          let splits_26 = Memo.splits (snd x_17) in
          let split0_26 = List.nth splits_26 0 in
          ignore (pop_env w_32);
          push_env w_32 split0_26;
          assert_env_length w_32 4;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 5;
          drop_n w_32 5 1;
          assert_env_length w_32 4;
          drop_n w_32 4 2;
          assert_env_length w_32 2;
          return_n w_32 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_32);
          assert_env_length w_32 3;
          push_env w_32 (Memo.from_constructor tag_Z);
          assert_env_length w_32 4;
          let ctor_arg_0 = pop_env w_32 in
          push_env w_32 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_0 ]);
          assert_env_length w_32 4;
          drop_n w_32 4 2;
          assert_env_length w_32 2;
          return_n w_32 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (32)")
    32;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 2;
      let last_16 = Source.E 1 in
      let x_16 = resolve w_31 last_16 in
      match Word.get_value (fst x_16) with
      | 5 (* tag_Const *) ->
          let splits_24 = Memo.splits (snd x_16) in
          let split0_24 = List.nth splits_24 0 in
          ignore (pop_env w_31);
          push_env w_31 split0_24;
          assert_env_length w_31 2;
          push_env w_31 (Dynarray.get w_31.state.e 1);
          assert_env_length w_31 3;
          drop_n w_31 3 1;
          assert_env_length w_31 2;
          return_n w_31 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_25 = Memo.splits (snd x_16) in
          let split0_25 = List.nth splits_25 0 in
          let split1_8 = List.nth splits_25 1 in
          ignore (pop_env w_31);
          push_env w_31 split0_25;
          push_env w_31 split1_8;
          assert_env_length w_31 3;
          push_env w_31 (Dynarray.get w_31.state.e 1);
          w_31.state.c <- pc_to_exp (int_to_pc 32)
      | _ ->
          ignore (pop_env w_31);
          assert_env_length w_31 1;
          push_env w_31 (Memo.from_constructor tag_Z);
          assert_env_length w_31 2;
          let ctor_arg_1 = pop_env w_31 in
          push_env w_31 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_1 ]);
          assert_env_length w_31 2;
          return_n w_31 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (33)")
    33;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 1;
      push_env w_33 (Dynarray.get w_33.state.e 0);
      w_33.state.c <- pc_to_exp (int_to_pc 36))
    34;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 4;
      let last_19 = Source.E 3 in
      let x_19 = resolve w_35 last_19 in
      match Word.get_value (fst x_19) with
      | 5 (* tag_Const *) ->
          let splits_29 = Memo.splits (snd x_19) in
          let split0_29 = List.nth splits_29 0 in
          ignore (pop_env w_35);
          push_env w_35 split0_29;
          assert_env_length w_35 4;
          push_env w_35 (Dynarray.get w_35.state.e 2);
          assert_env_length w_35 5;
          drop_n w_35 5 1;
          assert_env_length w_35 4;
          drop_n w_35 4 2;
          assert_env_length w_35 2;
          return_n w_35 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_35);
          assert_env_length w_35 3;
          push_env w_35 (Dynarray.get w_35.state.e 0);
          assert_env_length w_35 4;
          drop_n w_35 4 2;
          assert_env_length w_35 2;
          return_n w_35 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (35)")
    35;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 2;
      let last_18 = Source.E 1 in
      let x_18 = resolve w_34 last_18 in
      match Word.get_value (fst x_18) with
      | 5 (* tag_Const *) ->
          let splits_27 = Memo.splits (snd x_18) in
          let split0_27 = List.nth splits_27 0 in
          ignore (pop_env w_34);
          push_env w_34 split0_27;
          assert_env_length w_34 2;
          push_env w_34 (Memo.from_constructor tag_Z);
          assert_env_length w_34 3;
          let ctor_arg_2 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_2 ]);
          assert_env_length w_34 3;
          let ctor_arg_3 = pop_env w_34 in
          push_env w_34 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_3 ]);
          assert_env_length w_34 3;
          drop_n w_34 3 1;
          assert_env_length w_34 2;
          return_n w_34 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_28 = Memo.splits (snd x_18) in
          let split0_28 = List.nth splits_28 0 in
          let split1_9 = List.nth splits_28 1 in
          ignore (pop_env w_34);
          push_env w_34 split0_28;
          push_env w_34 split1_9;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          w_34.state.c <- pc_to_exp (int_to_pc 35)
      | _ ->
          ignore (pop_env w_34);
          assert_env_length w_34 1;
          push_env w_34 (Dynarray.get w_34.state.e 0);
          assert_env_length w_34 2;
          return_n w_34 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (36)")
    36;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 2;
      push_env w_36 (Dynarray.get w_36.state.e 0);
      assert_env_length w_36 3;
      push_env w_36 (Dynarray.get w_36.state.e 1);
      assert_env_length w_36 4;
      let keep_11 = env_call w_36 [ 0; 1 ] 2 in
      w_36.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_36.state.k ];
      w_36.state.c <- pc_to_exp (int_to_pc 21))
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
      let last_20 = Source.E 2 in
      let x_20 = resolve w_38 last_20 in
      match Word.get_value (fst x_20) with
      | 8 (* tag_Mul *) ->
          let splits_30 = Memo.splits (snd x_20) in
          let split0_30 = List.nth splits_30 0 in
          let split1_10 = List.nth splits_30 1 in
          ignore (pop_env w_38);
          push_env w_38 split0_30;
          push_env w_38 split1_10;
          assert_env_length w_38 4;
          push_env w_38 (Dynarray.get w_38.state.e 2);
          assert_env_length w_38 5;
          push_env w_38 (Dynarray.get w_38.state.e 1);
          assert_env_length w_38 6;
          let keep_12 = env_call w_38 [ 0; 1; 2; 3 ] 2 in
          w_38.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_38.state.k ];
          w_38.state.c <- pc_to_exp (int_to_pc 37)
      | _ ->
          ignore (pop_env w_38);
          assert_env_length w_38 2;
          push_env w_38 (Dynarray.get w_38.state.e 0);
          assert_env_length w_38 3;
          push_env w_38 (Dynarray.get w_38.state.e 1);
          assert_env_length w_38 4;
          let ctor_arg_4 = pop_env w_38 in
          let ctor_arg_5 = pop_env w_38 in
          push_env w_38 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_5; ctor_arg_4 ]);
          assert_env_length w_38 3;
          return_n w_38 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (39)")
    39;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 2;
      push_env w_39 (Dynarray.get w_39.state.e 0);
      w_39.state.c <- pc_to_exp (int_to_pc 41))
    40;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 3;
      let last_21 = Source.E 2 in
      let x_21 = resolve w_40 last_21 in
      match Word.get_value (fst x_21) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_40);
          assert_env_length w_40 2;
          push_env w_40 (Dynarray.get w_40.state.e 1);
          assert_env_length w_40 3;
          return_n w_40 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_31 = Memo.splits (snd x_21) in
          let split0_31 = List.nth splits_31 0 in
          let split1_11 = List.nth splits_31 1 in
          ignore (pop_env w_40);
          push_env w_40 split0_31;
          push_env w_40 split1_11;
          assert_env_length w_40 4;
          push_env w_40 (Dynarray.get w_40.state.e 2);
          assert_env_length w_40 5;
          push_env w_40 (Dynarray.get w_40.state.e 3);
          assert_env_length w_40 6;
          push_env w_40 (Dynarray.get w_40.state.e 1);
          assert_env_length w_40 7;
          let keep_13 = env_call w_40 [ 4 ] 2 in
          w_40.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_40.state.k ];
          w_40.state.c <- pc_to_exp (int_to_pc 40)
      | _ -> failwith "unreachable (41)")
    41;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 2;
      push_env w_41 (Dynarray.get w_41.state.e 1);
      w_41.state.c <- pc_to_exp (int_to_pc 43))
    42;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 3;
      let last_22 = Source.E 2 in
      let x_22 = resolve w_42 last_22 in
      match Word.get_value (fst x_22) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_42);
          assert_env_length w_42 2;
          push_env w_42 (Dynarray.get w_42.state.e 0);
          assert_env_length w_42 3;
          push_env w_42 (Memo.from_constructor tag_ENil);
          assert_env_length w_42 4;
          let ctor_arg_6 = pop_env w_42 in
          let ctor_arg_7 = pop_env w_42 in
          push_env w_42 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_7; ctor_arg_6 ]);
          assert_env_length w_42 3;
          return_n w_42 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_32 = Memo.splits (snd x_22) in
          let split0_32 = List.nth splits_32 0 in
          let split1_12 = List.nth splits_32 1 in
          ignore (pop_env w_42);
          push_env w_42 split0_32;
          push_env w_42 split1_12;
          assert_env_length w_42 4;
          push_env w_42 (Dynarray.get w_42.state.e 0);
          assert_env_length w_42 5;
          push_env w_42 (Dynarray.get w_42.state.e 2);
          assert_env_length w_42 6;
          let keep_14 = env_call w_42 [ 0; 1; 2; 3 ] 2 in
          w_42.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_42.state.k ];
          w_42.state.c <- pc_to_exp (int_to_pc 20)
      | _ -> failwith "unreachable (43)")
    43;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 1;
      push_env w_43 (Dynarray.get w_43.state.e 0);
      w_43.state.c <- pc_to_exp (int_to_pc 45))
    44;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 2;
      let last_23 = Source.E 1 in
      let x_23 = resolve w_44 last_23 in
      match Word.get_value (fst x_23) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_44);
          assert_env_length w_44 1;
          push_env w_44 (Memo.from_constructor tag_ENil);
          assert_env_length w_44 2;
          return_n w_44 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_33 = Memo.splits (snd x_23) in
          let split0_33 = List.nth splits_33 0 in
          let split1_13 = List.nth splits_33 1 in
          ignore (pop_env w_44);
          push_env w_44 split0_33;
          push_env w_44 split1_13;
          assert_env_length w_44 3;
          push_env w_44 (Dynarray.get w_44.state.e 1);
          assert_env_length w_44 4;
          push_env w_44 (Dynarray.get w_44.state.e 2);
          assert_env_length w_44 5;
          let keep_15 = env_call w_44 [ 3 ] 1 in
          w_44.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_44.state.k ];
          w_44.state.c <- pc_to_exp (int_to_pc 44)
      | _ -> failwith "unreachable (45)")
    45;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 2;
      push_env w_45 (Dynarray.get w_45.state.e 0);
      w_45.state.c <- pc_to_exp (int_to_pc 47))
    46;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 3;
      let last_24 = Source.E 2 in
      let x_24 = resolve w_46 last_24 in
      match Word.get_value (fst x_24) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_46);
          assert_env_length w_46 2;
          push_env w_46 (Dynarray.get w_46.state.e 1);
          assert_env_length w_46 3;
          return_n w_46 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_34 = Memo.splits (snd x_24) in
          let split0_34 = List.nth splits_34 0 in
          let split1_14 = List.nth splits_34 1 in
          ignore (pop_env w_46);
          push_env w_46 split0_34;
          push_env w_46 split1_14;
          assert_env_length w_46 4;
          push_env w_46 (Dynarray.get w_46.state.e 3);
          assert_env_length w_46 5;
          push_env w_46 (Dynarray.get w_46.state.e 2);
          assert_env_length w_46 6;
          push_env w_46 (Dynarray.get w_46.state.e 1);
          assert_env_length w_46 7;
          let ctor_arg_8 = pop_env w_46 in
          let ctor_arg_9 = pop_env w_46 in
          push_env w_46 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_9; ctor_arg_8 ]);
          assert_env_length w_46 6;
          ignore (env_call w_46 [] 2);
          w_46.state.c <- pc_to_exp (int_to_pc 46)
      | _ -> failwith "unreachable (47)")
    47;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 1;
      push_env w_47 (Dynarray.get w_47.state.e 0);
      assert_env_length w_47 2;
      push_env w_47 (Memo.from_constructor tag_ENil);
      assert_env_length w_47 3;
      ignore (env_call w_47 [] 2);
      w_47.state.c <- pc_to_exp (int_to_pc 46))
    48;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 1;
      push_env w_48 (Dynarray.get w_48.state.e 0);
      w_48.state.c <- pc_to_exp (int_to_pc 50))
    49;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 2;
      let last_25 = Source.E 1 in
      let x_25 = resolve w_49 last_25 in
      match Word.get_value (fst x_25) with
      | 7 (* tag_Add *) ->
          let splits_35 = Memo.splits (snd x_25) in
          let split0_35 = List.nth splits_35 0 in
          let split1_15 = List.nth splits_35 1 in
          ignore (pop_env w_49);
          push_env w_49 split0_35;
          push_env w_49 split1_15;
          assert_env_length w_49 3;
          push_env w_49 (Dynarray.get w_49.state.e 1);
          assert_env_length w_49 4;
          let keep_16 = env_call w_49 [ 2 ] 1 in
          w_49.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_49.state.k ];
          w_49.state.c <- pc_to_exp (int_to_pc 49)
      | 5 (* tag_Const *) ->
          let splits_36 = Memo.splits (snd x_25) in
          let split0_36 = List.nth splits_36 0 in
          ignore (pop_env w_49);
          push_env w_49 split0_36;
          assert_env_length w_49 2;
          push_env w_49 (Dynarray.get w_49.state.e 1);
          assert_env_length w_49 3;
          let keep_17 = env_call w_49 [ 0 ] 1 in
          w_49.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_49.state.k ];
          w_49.state.c <- pc_to_exp (int_to_pc 11)
      | _ ->
          ignore (pop_env w_49);
          assert_env_length w_49 1;
          push_env w_49 (Dynarray.get w_49.state.e 0);
          assert_env_length w_49 2;
          push_env w_49 (Memo.from_constructor tag_ENil);
          assert_env_length w_49 3;
          let ctor_arg_10 = pop_env w_49 in
          let ctor_arg_11 = pop_env w_49 in
          push_env w_49 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_11; ctor_arg_10 ]);
          assert_env_length w_49 2;
          return_n w_49 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (50)")
    50;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 1;
      push_env w_50 (Dynarray.get w_50.state.e 0);
      w_50.state.c <- pc_to_exp (int_to_pc 52))
    51;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 2;
      let last_26 = Source.E 1 in
      let x_26 = resolve w_51 last_26 in
      match Word.get_value (fst x_26) with
      | 8 (* tag_Mul *) ->
          let splits_37 = Memo.splits (snd x_26) in
          let split0_37 = List.nth splits_37 0 in
          let split1_16 = List.nth splits_37 1 in
          ignore (pop_env w_51);
          push_env w_51 split0_37;
          push_env w_51 split1_16;
          assert_env_length w_51 3;
          push_env w_51 (Dynarray.get w_51.state.e 1);
          assert_env_length w_51 4;
          let keep_18 = env_call w_51 [ 2 ] 1 in
          w_51.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_51.state.k ];
          w_51.state.c <- pc_to_exp (int_to_pc 51)
      | _ ->
          ignore (pop_env w_51);
          assert_env_length w_51 1;
          push_env w_51 (Dynarray.get w_51.state.e 0);
          assert_env_length w_51 2;
          push_env w_51 (Memo.from_constructor tag_ENil);
          assert_env_length w_51 3;
          let ctor_arg_12 = pop_env w_51 in
          let ctor_arg_13 = pop_env w_51 in
          push_env w_51 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_13; ctor_arg_12 ]);
          assert_env_length w_51 2;
          return_n w_51 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (52)")
    52;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 1;
      push_env w_52 (Dynarray.get w_52.state.e 0);
      w_52.state.c <- pc_to_exp (int_to_pc 55))
    53;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 4;
      let last_28 = Source.E 3 in
      let x_28 = resolve w_54 last_28 in
      match Word.get_value (fst x_28) with
      | 5 (* tag_Const *) ->
          let splits_40 = Memo.splits (snd x_28) in
          let split0_40 = List.nth splits_40 0 in
          ignore (pop_env w_54);
          push_env w_54 split0_40;
          assert_env_length w_54 4;
          push_env w_54 (Dynarray.get w_54.state.e 3);
          assert_env_length w_54 5;
          drop_n w_54 5 1;
          assert_env_length w_54 4;
          drop_n w_54 4 2;
          assert_env_length w_54 2;
          return_n w_54 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_54);
          assert_env_length w_54 3;
          push_env w_54 (Memo.from_constructor tag_Z);
          assert_env_length w_54 4;
          let ctor_arg_14 = pop_env w_54 in
          push_env w_54 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_14 ]);
          assert_env_length w_54 4;
          drop_n w_54 4 2;
          assert_env_length w_54 2;
          return_n w_54 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (54)")
    54;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 2;
      let last_27 = Source.E 1 in
      let x_27 = resolve w_53 last_27 in
      match Word.get_value (fst x_27) with
      | 5 (* tag_Const *) ->
          let splits_38 = Memo.splits (snd x_27) in
          let split0_38 = List.nth splits_38 0 in
          ignore (pop_env w_53);
          push_env w_53 split0_38;
          assert_env_length w_53 2;
          push_env w_53 (Dynarray.get w_53.state.e 1);
          assert_env_length w_53 3;
          drop_n w_53 3 1;
          assert_env_length w_53 2;
          return_n w_53 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_39 = Memo.splits (snd x_27) in
          let split0_39 = List.nth splits_39 0 in
          let split1_17 = List.nth splits_39 1 in
          ignore (pop_env w_53);
          push_env w_53 split0_39;
          push_env w_53 split1_17;
          assert_env_length w_53 3;
          push_env w_53 (Dynarray.get w_53.state.e 1);
          w_53.state.c <- pc_to_exp (int_to_pc 54)
      | _ ->
          ignore (pop_env w_53);
          assert_env_length w_53 1;
          push_env w_53 (Memo.from_constructor tag_Z);
          assert_env_length w_53 2;
          let ctor_arg_15 = pop_env w_53 in
          push_env w_53 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_15 ]);
          assert_env_length w_53 2;
          return_n w_53 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (55)")
    55;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 1;
      push_env w_55 (Dynarray.get w_55.state.e 0);
      w_55.state.c <- pc_to_exp (int_to_pc 58))
    56;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 4;
      let last_30 = Source.E 3 in
      let x_30 = resolve w_57 last_30 in
      match Word.get_value (fst x_30) with
      | 5 (* tag_Const *) ->
          let splits_43 = Memo.splits (snd x_30) in
          let split0_43 = List.nth splits_43 0 in
          ignore (pop_env w_57);
          push_env w_57 split0_43;
          assert_env_length w_57 4;
          push_env w_57 (Dynarray.get w_57.state.e 2);
          assert_env_length w_57 5;
          drop_n w_57 5 1;
          assert_env_length w_57 4;
          drop_n w_57 4 2;
          assert_env_length w_57 2;
          return_n w_57 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_57);
          assert_env_length w_57 3;
          push_env w_57 (Dynarray.get w_57.state.e 0);
          assert_env_length w_57 4;
          drop_n w_57 4 2;
          assert_env_length w_57 2;
          return_n w_57 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (57)")
    57;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 2;
      let last_29 = Source.E 1 in
      let x_29 = resolve w_56 last_29 in
      match Word.get_value (fst x_29) with
      | 5 (* tag_Const *) ->
          let splits_41 = Memo.splits (snd x_29) in
          let split0_41 = List.nth splits_41 0 in
          ignore (pop_env w_56);
          push_env w_56 split0_41;
          assert_env_length w_56 2;
          push_env w_56 (Memo.from_constructor tag_Z);
          assert_env_length w_56 3;
          let ctor_arg_16 = pop_env w_56 in
          push_env w_56 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_16 ]);
          assert_env_length w_56 3;
          let ctor_arg_17 = pop_env w_56 in
          push_env w_56 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_17 ]);
          assert_env_length w_56 3;
          drop_n w_56 3 1;
          assert_env_length w_56 2;
          return_n w_56 2 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_42 = Memo.splits (snd x_29) in
          let split0_42 = List.nth splits_42 0 in
          let split1_18 = List.nth splits_42 1 in
          ignore (pop_env w_56);
          push_env w_56 split0_42;
          push_env w_56 split1_18;
          assert_env_length w_56 3;
          push_env w_56 (Dynarray.get w_56.state.e 1);
          w_56.state.c <- pc_to_exp (int_to_pc 57)
      | _ ->
          ignore (pop_env w_56);
          assert_env_length w_56 1;
          push_env w_56 (Dynarray.get w_56.state.e 0);
          assert_env_length w_56 2;
          return_n w_56 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (58)")
    58;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 1;
      push_env w_58 (Dynarray.get w_58.state.e 0);
      w_58.state.c <- pc_to_exp (int_to_pc 60))
    59;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 2;
      let last_31 = Source.E 1 in
      let x_31 = resolve w_59 last_31 in
      match Word.get_value (fst x_31) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_59);
          assert_env_length w_59 1;
          push_env w_59 (Memo.from_constructor tag_Z);
          assert_env_length w_59 2;
          let ctor_arg_18 = pop_env w_59 in
          push_env w_59 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_18 ]);
          assert_env_length w_59 2;
          return_n w_59 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_44 = Memo.splits (snd x_31) in
          let split0_44 = List.nth splits_44 0 in
          let split1_19 = List.nth splits_44 1 in
          ignore (pop_env w_59);
          push_env w_59 split0_44;
          push_env w_59 split1_19;
          assert_env_length w_59 3;
          push_env w_59 (Dynarray.get w_59.state.e 1);
          assert_env_length w_59 4;
          let keep_19 = env_call w_59 [ 2 ] 1 in
          w_59.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_19; w_59.state.k ];
          w_59.state.c <- pc_to_exp (int_to_pc 53)
      | _ -> failwith "unreachable (60)")
    60;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 1;
      push_env w_60 (Dynarray.get w_60.state.e 0);
      w_60.state.c <- pc_to_exp (int_to_pc 62))
    61;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 2;
      let last_32 = Source.E 1 in
      let x_32 = resolve w_61 last_32 in
      match Word.get_value (fst x_32) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_61);
          assert_env_length w_61 1;
          push_env w_61 (Memo.from_constructor tag_ENil);
          assert_env_length w_61 2;
          return_n w_61 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_45 = Memo.splits (snd x_32) in
          let split0_45 = List.nth splits_45 0 in
          let split1_20 = List.nth splits_45 1 in
          ignore (pop_env w_61);
          push_env w_61 split0_45;
          push_env w_61 split1_20;
          assert_env_length w_61 3;
          push_env w_61 (Dynarray.get w_61.state.e 1);
          assert_env_length w_61 4;
          let keep_20 = env_call w_61 [ 2 ] 1 in
          w_61.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_20; w_61.state.k ];
          w_61.state.c <- pc_to_exp (int_to_pc 56)
      | _ -> failwith "unreachable (62)")
    62;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 1;
      push_env w_62 (Dynarray.get w_62.state.e 0);
      w_62.state.c <- pc_to_exp (int_to_pc 65))
    63;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 4;
      let last_34 = Source.E 3 in
      let x_34 = resolve w_64 last_34 in
      match Word.get_value (fst x_34) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_64);
          assert_env_length w_64 3;
          push_env w_64 (Dynarray.get w_64.state.e 1);
          assert_env_length w_64 4;
          drop_n w_64 4 2;
          assert_env_length w_64 2;
          return_n w_64 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_64);
          assert_env_length w_64 3;
          push_env w_64 (Dynarray.get w_64.state.e 1);
          assert_env_length w_64 4;
          push_env w_64 (Dynarray.get w_64.state.e 2);
          assert_env_length w_64 5;
          let keep_21 = env_call w_64 [ 3 ] 1 in
          w_64.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_21; w_64.state.k ];
          w_64.state.c <- pc_to_exp (int_to_pc 63)
      | _ -> failwith "unreachable (64)")
    64;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 2;
      let last_33 = Source.E 1 in
      let x_33 = resolve w_63 last_33 in
      match Word.get_value (fst x_33) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_63);
          assert_env_length w_63 1;
          push_env w_63 (Memo.from_constructor tag_Z);
          assert_env_length w_63 2;
          let ctor_arg_19 = pop_env w_63 in
          push_env w_63 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_19 ]);
          assert_env_length w_63 2;
          let ctor_arg_20 = pop_env w_63 in
          push_env w_63 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_20 ]);
          assert_env_length w_63 2;
          return_n w_63 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_46 = Memo.splits (snd x_33) in
          let split0_46 = List.nth splits_46 0 in
          let split1_21 = List.nth splits_46 1 in
          ignore (pop_env w_63);
          push_env w_63 split0_46;
          push_env w_63 split1_21;
          assert_env_length w_63 3;
          push_env w_63 (Dynarray.get w_63.state.e 2);
          w_63.state.c <- pc_to_exp (int_to_pc 64)
      | _ -> failwith "unreachable (65)")
    65;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 2;
      push_env w_65 (Dynarray.get w_65.state.e 0);
      assert_env_length w_65 3;
      let keep_22 = env_call w_65 [ 1 ] 1 in
      w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_22; w_65.state.k ];
      w_65.state.c <- pc_to_exp (int_to_pc 51))
    66;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 3;
      push_env w_66 (Dynarray.get w_66.state.e 2);
      w_66.state.c <- pc_to_exp (int_to_pc 68))
    67;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 4;
      let last_35 = Source.E 3 in
      let x_35 = resolve w_67 last_35 in
      match Word.get_value (fst x_35) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_67);
          assert_env_length w_67 3;
          push_env w_67 (Dynarray.get w_67.state.e 1);
          assert_env_length w_67 4;
          let keep_23 = env_call w_67 [ 0; 1 ] 1 in
          w_67.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_23; w_67.state.k ];
          w_67.state.c <- pc_to_exp (int_to_pc 11)
      | 12 (* tag_ECons *) ->
          let splits_47 = Memo.splits (snd x_35) in
          let split0_47 = List.nth splits_47 0 in
          let split1_22 = List.nth splits_47 1 in
          ignore (pop_env w_67);
          push_env w_67 split0_47;
          push_env w_67 split1_22;
          assert_env_length w_67 5;
          push_env w_67 (Dynarray.get w_67.state.e 3);
          assert_env_length w_67 6;
          let keep_24 = env_call w_67 [ 0; 1; 3; 4 ] 1 in
          w_67.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_24; w_67.state.k ];
          w_67.state.c <- pc_to_exp (int_to_pc 34)
      | _ -> failwith "unreachable (68)")
    68;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 1;
      push_env w_68 (Dynarray.get w_68.state.e 0);
      w_68.state.c <- pc_to_exp (int_to_pc 70))
    69;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 2;
      let last_36 = Source.E 1 in
      let x_36 = resolve w_69 last_36 in
      match Word.get_value (fst x_36) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_69);
          assert_env_length w_69 1;
          push_env w_69 (Memo.from_constructor tag_ENil);
          assert_env_length w_69 2;
          return_n w_69 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_48 = Memo.splits (snd x_36) in
          let split0_48 = List.nth splits_48 0 in
          let split1_23 = List.nth splits_48 1 in
          ignore (pop_env w_69);
          push_env w_69 split0_48;
          push_env w_69 split1_23;
          assert_env_length w_69 3;
          push_env w_69 (Dynarray.get w_69.state.e 1);
          assert_env_length w_69 4;
          let keep_25 = env_call w_69 [ 1; 2 ] 1 in
          w_69.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_25; w_69.state.k ];
          w_69.state.c <- pc_to_exp (int_to_pc 34)
      | _ -> failwith "unreachable (70)")
    70;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 1;
      push_env w_70 (Dynarray.get w_70.state.e 0);
      w_70.state.c <- pc_to_exp (int_to_pc 73))
    71;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 4;
      let last_38 = Source.E 3 in
      let x_38 = resolve w_72 last_38 in
      match Word.get_value (fst x_38) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_72);
          assert_env_length w_72 3;
          push_env w_72 (Dynarray.get w_72.state.e 1);
          assert_env_length w_72 4;
          push_env w_72 (Memo.from_constructor tag_ENil);
          assert_env_length w_72 5;
          let ctor_arg_21 = pop_env w_72 in
          let ctor_arg_22 = pop_env w_72 in
          push_env w_72 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_22; ctor_arg_21 ]);
          assert_env_length w_72 4;
          drop_n w_72 4 2;
          assert_env_length w_72 2;
          return_n w_72 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_50 = Memo.splits (snd x_38) in
          let split0_50 = List.nth splits_50 0 in
          let split1_25 = List.nth splits_50 1 in
          ignore (pop_env w_72);
          push_env w_72 split0_50;
          push_env w_72 split1_25;
          assert_env_length w_72 5;
          push_env w_72 (Dynarray.get w_72.state.e 1);
          assert_env_length w_72 6;
          push_env w_72 (Dynarray.get w_72.state.e 3);
          assert_env_length w_72 7;
          let keep_26 = env_call w_72 [ 1; 2; 3; 4 ] 2 in
          w_72.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_26; w_72.state.k ];
          w_72.state.c <- pc_to_exp (int_to_pc 38)
      | _ -> failwith "unreachable (72)")
    72;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 2;
      let last_37 = Source.E 1 in
      let x_37 = resolve w_71 last_37 in
      match Word.get_value (fst x_37) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_71);
          assert_env_length w_71 1;
          push_env w_71 (Memo.from_constructor tag_ENil);
          assert_env_length w_71 2;
          return_n w_71 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_49 = Memo.splits (snd x_37) in
          let split0_49 = List.nth splits_49 0 in
          let split1_24 = List.nth splits_49 1 in
          ignore (pop_env w_71);
          push_env w_71 split0_49;
          push_env w_71 split1_24;
          assert_env_length w_71 3;
          push_env w_71 (Dynarray.get w_71.state.e 2);
          w_71.state.c <- pc_to_exp (int_to_pc 72)
      | _ -> failwith "unreachable (73)")
    73;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 2;
      push_env w_73 (Dynarray.get w_73.state.e 1);
      w_73.state.c <- pc_to_exp (int_to_pc 75))
    74;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 3;
      let last_39 = Source.E 2 in
      let x_39 = resolve w_74 last_39 in
      match Word.get_value (fst x_39) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_74);
          assert_env_length w_74 2;
          push_env w_74 (Memo.from_constructor tag_NoPick);
          assert_env_length w_74 3;
          return_n w_74 3 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_51 = Memo.splits (snd x_39) in
          let split0_51 = List.nth splits_51 0 in
          let split1_26 = List.nth splits_51 1 in
          ignore (pop_env w_74);
          push_env w_74 split0_51;
          push_env w_74 split1_26;
          assert_env_length w_74 4;
          push_env w_74 (Dynarray.get w_74.state.e 0);
          assert_env_length w_74 5;
          push_env w_74 (Dynarray.get w_74.state.e 2);
          assert_env_length w_74 6;
          let keep_27 = env_call w_74 [ 0; 2; 3 ] 2 in
          w_74.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_27; w_74.state.k ];
          w_74.state.c <- pc_to_exp (int_to_pc 38)
      | _ -> failwith "unreachable (75)")
    75;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 1;
      push_env w_75 (Dynarray.get w_75.state.e 0);
      w_75.state.c <- pc_to_exp (int_to_pc 77))
    76;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 2;
      let last_40 = Source.E 1 in
      let x_40 = resolve w_76 last_40 in
      match Word.get_value (fst x_40) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_76);
          assert_env_length w_76 1;
          push_env w_76 (Memo.from_constructor tag_ENil);
          assert_env_length w_76 2;
          return_n w_76 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_52 = Memo.splits (snd x_40) in
          let split0_52 = List.nth splits_52 0 in
          let split1_27 = List.nth splits_52 1 in
          ignore (pop_env w_76);
          push_env w_76 split0_52;
          push_env w_76 split1_27;
          assert_env_length w_76 3;
          push_env w_76 (Dynarray.get w_76.state.e 1);
          assert_env_length w_76 4;
          push_env w_76 (Dynarray.get w_76.state.e 2);
          assert_env_length w_76 5;
          let keep_28 = env_call w_76 [ 1; 2 ] 2 in
          w_76.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_28; w_76.state.k ];
          w_76.state.c <- pc_to_exp (int_to_pc 74)
      | _ -> failwith "unreachable (77)")
    77;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 1;
      push_env w_77 (Dynarray.get w_77.state.e 0);
      w_77.state.c <- pc_to_exp (int_to_pc 80))
    78;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 4;
      let last_42 = Source.E 3 in
      let x_42 = resolve w_79 last_42 in
      match Word.get_value (fst x_42) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_79);
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          assert_env_length w_79 4;
          drop_n w_79 4 2;
          assert_env_length w_79 2;
          return_n w_79 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_79);
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          assert_env_length w_79 4;
          push_env w_79 (Dynarray.get w_79.state.e 2);
          assert_env_length w_79 5;
          let keep_29 = env_call w_79 [ 3 ] 1 in
          w_79.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_29; w_79.state.k ];
          w_79.state.c <- pc_to_exp (int_to_pc 78)
      | _ -> failwith "unreachable (79)")
    79;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 2;
      let last_41 = Source.E 1 in
      let x_41 = resolve w_78 last_41 in
      match Word.get_value (fst x_41) with
      | 11 (* tag_ENil *) ->
          ignore (pop_env w_78);
          assert_env_length w_78 1;
          push_env w_78 (Memo.from_constructor tag_Z);
          assert_env_length w_78 2;
          let ctor_arg_23 = pop_env w_78 in
          push_env w_78 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_23 ]);
          assert_env_length w_78 2;
          return_n w_78 2 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_53 = Memo.splits (snd x_41) in
          let split0_53 = List.nth splits_53 0 in
          let split1_28 = List.nth splits_53 1 in
          ignore (pop_env w_78);
          push_env w_78 split0_53;
          push_env w_78 split1_28;
          assert_env_length w_78 3;
          push_env w_78 (Dynarray.get w_78.state.e 2);
          w_78.state.c <- pc_to_exp (int_to_pc 79)
      | _ -> failwith "unreachable (80)")
    80;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 1;
      push_env w_80 (Dynarray.get w_80.state.e 0);
      assert_env_length w_80 2;
      let keep_30 = env_call w_80 [] 1 in
      w_80.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_30; w_80.state.k ];
      w_80.state.c <- pc_to_exp (int_to_pc 71))
    81;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 2;
      push_env w_81 (Dynarray.get w_81.state.e 0);
      assert_env_length w_81 3;
      let keep_31 = env_call w_81 [ 1 ] 1 in
      w_81.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_31; w_81.state.k ];
      w_81.state.c <- pc_to_exp (int_to_pc 49))
    82;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 2;
      push_env w_82 (Dynarray.get w_82.state.e 0);
      assert_env_length w_82 3;
      push_env w_82 (Memo.from_int 0);
      w_82.state.c <- pc_to_exp (int_to_pc 88))
    83;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 6;
      let x0_2 = resolve w_86 (Source.E 4) in
      let x1_2 = resolve w_86 (Source.E 5) in
      ignore (pop_env w_86);
      ignore (pop_env w_86);
      push_env w_86 (Memo.from_int (Word.get_value (fst x0_2) - Word.get_value (fst x1_2)));
      assert_env_length w_86 5;
      push_env w_86 (Dynarray.get w_86.state.e 2);
      assert_env_length w_86 6;
      let keep_32 = env_call w_86 [ 0; 3 ] 2 in
      w_86.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_32; w_86.state.k ];
      w_86.state.c <- pc_to_exp (int_to_pc 83))
    84;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 6;
      let x0_3 = resolve w_87 (Source.E 4) in
      let x1_3 = resolve w_87 (Source.E 5) in
      ignore (pop_env w_87);
      ignore (pop_env w_87);
      push_env w_87 (Memo.from_int (Word.get_value (fst x0_3) - Word.get_value (fst x1_3)));
      assert_env_length w_87 5;
      push_env w_87 (Dynarray.get w_87.state.e 2);
      assert_env_length w_87 6;
      let keep_33 = env_call w_87 [ 0; 3 ] 2 in
      w_87.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_33; w_87.state.k ];
      w_87.state.c <- pc_to_exp (int_to_pc 83))
    85;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 3;
      let last_43 = Source.E 2 in
      let x_43 = resolve w_85 last_43 in
      match Word.get_value (fst x_43) with
      | 7 (* tag_Add *) ->
          let splits_54 = Memo.splits (snd x_43) in
          let split0_54 = List.nth splits_54 0 in
          let split1_29 = List.nth splits_54 1 in
          ignore (pop_env w_85);
          push_env w_85 split0_54;
          push_env w_85 split1_29;
          assert_env_length w_85 4;
          push_env w_85 (Dynarray.get w_85.state.e 0);
          assert_env_length w_85 5;
          push_env w_85 (Memo.from_int 1);
          w_85.state.c <- pc_to_exp (int_to_pc 84)
      | 8 (* tag_Mul *) ->
          let splits_55 = Memo.splits (snd x_43) in
          let split0_55 = List.nth splits_55 0 in
          let split1_30 = List.nth splits_55 1 in
          ignore (pop_env w_85);
          push_env w_85 split0_55;
          push_env w_85 split1_30;
          assert_env_length w_85 4;
          push_env w_85 (Dynarray.get w_85.state.e 0);
          assert_env_length w_85 5;
          push_env w_85 (Memo.from_int 1);
          w_85.state.c <- pc_to_exp (int_to_pc 85)
      | _ ->
          ignore (pop_env w_85);
          assert_env_length w_85 2;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          assert_env_length w_85 3;
          return_n w_85 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (86)")
    86;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 3;
      let cond_0 = resolve w_84 (Source.E 2) in
      ignore (pop_env w_84);
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_84 2;
        push_env w_84 (Dynarray.get w_84.state.e 1);
        assert_env_length w_84 3;
        return_n w_84 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_84 2;
        push_env w_84 (Dynarray.get w_84.state.e 1);
        w_84.state.c <- pc_to_exp (int_to_pc 86)))
    87;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 4;
      let x0_1 = resolve w_83 (Source.E 2) in
      let x1_1 = resolve w_83 (Source.E 3) in
      ignore (pop_env w_83);
      ignore (pop_env w_83);
      push_env w_83 (Memo.from_int (if Word.get_value (fst x0_1) = Word.get_value (fst x1_1) then 1 else 0));
      w_83.state.c <- pc_to_exp (int_to_pc 87))
    88;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 1;
      push_env w_88 (Dynarray.get w_88.state.e 0);
      w_88.state.c <- pc_to_exp (int_to_pc 90))
    89;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 2;
      let last_44 = Source.E 1 in
      let x_44 = resolve w_89 last_44 in
      match Word.get_value (fst x_44) with
      | 5 (* tag_Const *) ->
          let splits_56 = Memo.splits (snd x_44) in
          let split0_56 = List.nth splits_56 0 in
          ignore (pop_env w_89);
          push_env w_89 split0_56;
          assert_env_length w_89 2;
          push_env w_89 (Dynarray.get w_89.state.e 0);
          assert_env_length w_89 3;
          drop_n w_89 3 1;
          assert_env_length w_89 2;
          return_n w_89 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_57 = Memo.splits (snd x_44) in
          let split0_57 = List.nth splits_57 0 in
          ignore (pop_env w_89);
          push_env w_89 split0_57;
          assert_env_length w_89 2;
          push_env w_89 (Dynarray.get w_89.state.e 0);
          assert_env_length w_89 3;
          drop_n w_89 3 1;
          assert_env_length w_89 2;
          return_n w_89 2 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_58 = Memo.splits (snd x_44) in
          let split0_58 = List.nth splits_58 0 in
          let split1_31 = List.nth splits_58 1 in
          ignore (pop_env w_89);
          push_env w_89 split0_58;
          push_env w_89 split1_31;
          assert_env_length w_89 3;
          push_env w_89 (Dynarray.get w_89.state.e 1);
          assert_env_length w_89 4;
          let keep_34 = env_call w_89 [ 2 ] 1 in
          w_89.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_34; w_89.state.k ];
          w_89.state.c <- pc_to_exp (int_to_pc 89)
      | 8 (* tag_Mul *) ->
          let splits_59 = Memo.splits (snd x_44) in
          let split0_59 = List.nth splits_59 0 in
          let split1_32 = List.nth splits_59 1 in
          ignore (pop_env w_89);
          push_env w_89 split0_59;
          push_env w_89 split1_32;
          assert_env_length w_89 3;
          push_env w_89 (Dynarray.get w_89.state.e 1);
          assert_env_length w_89 4;
          let keep_35 = env_call w_89 [ 2 ] 1 in
          w_89.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_35; w_89.state.k ];
          w_89.state.c <- pc_to_exp (int_to_pc 89)
      | _ -> failwith "unreachable (90)")
    90;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 1;
      push_env w_90 (Dynarray.get w_90.state.e 0);
      assert_env_length w_90 2;
      let keep_36 = env_call w_90 [ 0 ] 1 in
      w_90.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_36; w_90.state.k ];
      w_90.state.c <- pc_to_exp (int_to_pc 89))
    91;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 1;
      push_env w_91 (Dynarray.get w_91.state.e 0);
      w_91.state.c <- pc_to_exp (int_to_pc 94))
    92;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 3;
      let last_46 = Source.E 2 in
      let x_46 = resolve w_93 last_46 in
      match Word.get_value (fst x_46) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_93);
          assert_env_length w_93 2;
          push_env w_93 (Memo.from_constructor tag_Z);
          assert_env_length w_93 3;
          let ctor_arg_25 = pop_env w_93 in
          push_env w_93 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_25 ]);
          assert_env_length w_93 3;
          let ctor_arg_26 = pop_env w_93 in
          push_env w_93 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_26 ]);
          assert_env_length w_93 3;
          drop_n w_93 3 1;
          assert_env_length w_93 2;
          return_n w_93 2 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_93);
          assert_env_length w_93 2;
          push_env w_93 (Memo.from_constructor tag_Z);
          assert_env_length w_93 3;
          let ctor_arg_27 = pop_env w_93 in
          push_env w_93 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_27 ]);
          assert_env_length w_93 3;
          drop_n w_93 3 1;
          assert_env_length w_93 2;
          return_n w_93 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (93)")
    93;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 2;
      let last_45 = Source.E 1 in
      let x_45 = resolve w_92 last_45 in
      match Word.get_value (fst x_45) with
      | 5 (* tag_Const *) ->
          let splits_60 = Memo.splits (snd x_45) in
          let split0_60 = List.nth splits_60 0 in
          ignore (pop_env w_92);
          push_env w_92 split0_60;
          assert_env_length w_92 2;
          push_env w_92 (Memo.from_constructor tag_Z);
          assert_env_length w_92 3;
          let ctor_arg_24 = pop_env w_92 in
          push_env w_92 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_24 ]);
          assert_env_length w_92 3;
          drop_n w_92 3 1;
          assert_env_length w_92 2;
          return_n w_92 2 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_61 = Memo.splits (snd x_45) in
          let split0_61 = List.nth splits_61 0 in
          ignore (pop_env w_92);
          push_env w_92 split0_61;
          assert_env_length w_92 2;
          push_env w_92 (Dynarray.get w_92.state.e 1);
          w_92.state.c <- pc_to_exp (int_to_pc 93)
      | 7 (* tag_Add *) ->
          let splits_62 = Memo.splits (snd x_45) in
          let split0_62 = List.nth splits_62 0 in
          let split1_33 = List.nth splits_62 1 in
          ignore (pop_env w_92);
          push_env w_92 split0_62;
          push_env w_92 split1_33;
          assert_env_length w_92 3;
          push_env w_92 (Dynarray.get w_92.state.e 1);
          assert_env_length w_92 4;
          let keep_37 = env_call w_92 [ 2 ] 1 in
          w_92.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_37; w_92.state.k ];
          w_92.state.c <- pc_to_exp (int_to_pc 92)
      | 8 (* tag_Mul *) ->
          let splits_63 = Memo.splits (snd x_45) in
          let split0_63 = List.nth splits_63 0 in
          let split1_34 = List.nth splits_63 1 in
          ignore (pop_env w_92);
          push_env w_92 split0_63;
          push_env w_92 split1_34;
          assert_env_length w_92 3;
          push_env w_92 (Dynarray.get w_92.state.e 1);
          assert_env_length w_92 4;
          let keep_38 = env_call w_92 [ 1; 2 ] 1 in
          w_92.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_38; w_92.state.k ];
          w_92.state.c <- pc_to_exp (int_to_pc 92)
      | _ -> failwith "unreachable (94)")
    94;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 3;
      push_env w_94 (Dynarray.get w_94.state.e 0);
      w_94.state.c <- pc_to_exp (int_to_pc 97))
    95;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 5;
      let last_48 = Source.E 4 in
      let x_48 = resolve w_96 last_48 in
      match Word.get_value (fst x_48) with
      | 3 (* tag_X *) ->
          ignore (pop_env w_96);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          assert_env_length w_96 5;
          drop_n w_96 5 1;
          assert_env_length w_96 4;
          return_n w_96 4 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          ignore (pop_env w_96);
          assert_env_length w_96 4;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 5;
          drop_n w_96 5 1;
          assert_env_length w_96 4;
          return_n w_96 4 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (96)")
    96;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 4;
      let last_47 = Source.E 3 in
      let x_47 = resolve w_95 last_47 in
      match Word.get_value (fst x_47) with
      | 5 (* tag_Const *) ->
          let splits_64 = Memo.splits (snd x_47) in
          let split0_64 = List.nth splits_64 0 in
          ignore (pop_env w_95);
          push_env w_95 split0_64;
          assert_env_length w_95 4;
          push_env w_95 (Dynarray.get w_95.state.e 3);
          assert_env_length w_95 5;
          drop_n w_95 5 1;
          assert_env_length w_95 4;
          return_n w_95 4 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_65 = Memo.splits (snd x_47) in
          let split0_65 = List.nth splits_65 0 in
          ignore (pop_env w_95);
          push_env w_95 split0_65;
          assert_env_length w_95 4;
          push_env w_95 (Dynarray.get w_95.state.e 3);
          w_95.state.c <- pc_to_exp (int_to_pc 96)
      | 7 (* tag_Add *) ->
          let splits_66 = Memo.splits (snd x_47) in
          let split0_66 = List.nth splits_66 0 in
          let split1_35 = List.nth splits_66 1 in
          ignore (pop_env w_95);
          push_env w_95 split0_66;
          push_env w_95 split1_35;
          assert_env_length w_95 5;
          push_env w_95 (Dynarray.get w_95.state.e 3);
          assert_env_length w_95 6;
          push_env w_95 (Dynarray.get w_95.state.e 1);
          assert_env_length w_95 7;
          push_env w_95 (Dynarray.get w_95.state.e 2);
          assert_env_length w_95 8;
          let keep_39 = env_call w_95 [ 1; 2; 4 ] 3 in
          w_95.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_39; w_95.state.k ];
          w_95.state.c <- pc_to_exp (int_to_pc 95)
      | 8 (* tag_Mul *) ->
          let splits_67 = Memo.splits (snd x_47) in
          let split0_67 = List.nth splits_67 0 in
          let split1_36 = List.nth splits_67 1 in
          ignore (pop_env w_95);
          push_env w_95 split0_67;
          push_env w_95 split1_36;
          assert_env_length w_95 5;
          push_env w_95 (Dynarray.get w_95.state.e 3);
          assert_env_length w_95 6;
          push_env w_95 (Dynarray.get w_95.state.e 1);
          assert_env_length w_95 7;
          push_env w_95 (Dynarray.get w_95.state.e 2);
          assert_env_length w_95 8;
          let keep_40 = env_call w_95 [ 1; 2; 4 ] 3 in
          w_95.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_40; w_95.state.k ];
          w_95.state.c <- pc_to_exp (int_to_pc 95)
      | _ -> failwith "unreachable (97)")
    97;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 1;
      push_env w_97 (Dynarray.get w_97.state.e 0);
      assert_env_length w_97 2;
      let keep_41 = env_call w_97 [] 1 in
      w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_41; w_97.state.k ];
      w_97.state.c <- pc_to_exp (int_to_pc 92))
    98;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 2;
      let x0_4 = resolve w_99 (Source.E 0) in
      let x1_4 = resolve w_99 (Source.E 1) in
      ignore (pop_env w_99);
      ignore (pop_env w_99);
      push_env w_99 (Memo.from_int (if Word.get_value (fst x0_4) = Word.get_value (fst x1_4) then 1 else 0));
      assert_env_length w_99 1;
      return_n w_99 1 (pc_to_exp (int_to_pc 0)))
    99;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 3;
      let x0_5 = resolve w_100 (Source.E 1) in
      let x1_5 = resolve w_100 (Source.E 2) in
      ignore (pop_env w_100);
      ignore (pop_env w_100);
      push_env w_100 (Memo.from_int (Word.get_value (fst x0_5) + Word.get_value (fst x1_5)));
      assert_env_length w_100 2;
      push_env w_100 (Dynarray.get w_100.state.e 0);
      assert_env_length w_100 3;
      let keep_46 = env_call w_100 [ 1 ] 1 in
      w_100.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; keep_46; w_100.state.k ];
      w_100.state.c <- pc_to_exp (int_to_pc 27))
    100;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 3;
      let x0_6 = resolve w_101 (Source.E 1) in
      let x1_6 = resolve w_101 (Source.E 2) in
      ignore (pop_env w_101);
      ignore (pop_env w_101);
      push_env w_101 (Memo.from_int (Word.get_value (fst x0_6) + Word.get_value (fst x1_6)));
      assert_env_length w_101 2;
      push_env w_101 (Dynarray.get w_101.state.e 0);
      assert_env_length w_101 3;
      let keep_47 = env_call w_101 [ 1 ] 1 in
      w_101.state.k <- Memo.appends [ Memo.from_constructor tag_cont_50; keep_47; w_101.state.k ];
      w_101.state.c <- pc_to_exp (int_to_pc 27))
    101;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 3;
      let last_49 = Source.E 2 in
      let x_49 = resolve w_103 last_49 in
      match Word.get_value (fst x_49) with
      | 5 (* tag_Const *) ->
          let splits_68 = Memo.splits (snd x_49) in
          let split0_68 = List.nth splits_68 0 in
          ignore (pop_env w_103);
          push_env w_103 split0_68;
          assert_env_length w_103 3;
          push_env w_103 (Dynarray.get w_103.state.e 0);
          assert_env_length w_103 4;
          push_env w_103 (Dynarray.get w_103.state.e 2);
          assert_env_length w_103 5;
          let keep_49 = env_call w_103 [] 2 in
          w_103.state.k <- Memo.appends [ Memo.from_constructor tag_cont_52; keep_49; w_103.state.k ];
          w_103.state.c <- pc_to_exp (int_to_pc 3)
      | _ ->
          ignore (pop_env w_103);
          assert_env_length w_103 2;
          push_env w_103 (Dynarray.get w_103.state.e 0);
          assert_env_length w_103 3;
          let keep_50 = env_call w_103 [ 0; 1 ] 1 in
          w_103.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; keep_50; w_103.state.k ];
          w_103.state.c <- pc_to_exp (int_to_pc 13)
      | _ -> failwith "unreachable (102)")
    102;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 3;
      let cond_1 = resolve w_102 (Source.E 2) in
      ignore (pop_env w_102);
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_102 2;
        push_env w_102 (Memo.from_constructor tag_Z);
        assert_env_length w_102 3;
        let ctor_arg_29 = pop_env w_102 in
        push_env w_102 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_29 ]);
        assert_env_length w_102 3;
        return_n w_102 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_102 2;
        push_env w_102 (Dynarray.get w_102.state.e 1);
        w_102.state.c <- pc_to_exp (int_to_pc 102)))
    103;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 3;
      let last_50 = Source.E 2 in
      let x_50 = resolve w_105 last_50 in
      match Word.get_value (fst x_50) with
      | 8 (* tag_Mul *) ->
          let splits_69 = Memo.splits (snd x_50) in
          let split0_69 = List.nth splits_69 0 in
          let split1_37 = List.nth splits_69 1 in
          ignore (pop_env w_105);
          push_env w_105 split0_69;
          push_env w_105 split1_37;
          assert_env_length w_105 4;
          push_env w_105 (Dynarray.get w_105.state.e 0);
          assert_env_length w_105 5;
          push_env w_105 (Dynarray.get w_105.state.e 2);
          assert_env_length w_105 6;
          let keep_51 = env_call w_105 [ 0; 2; 3 ] 2 in
          w_105.state.k <- Memo.appends [ Memo.from_constructor tag_cont_54; keep_51; w_105.state.k ];
          w_105.state.c <- pc_to_exp (int_to_pc 37)
      | _ ->
          ignore (pop_env w_105);
          assert_env_length w_105 2;
          push_env w_105 (Memo.from_constructor tag_Missing);
          assert_env_length w_105 3;
          return_n w_105 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (104)")
    104;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 3;
      let cond_2 = resolve w_104 (Source.E 2) in
      ignore (pop_env w_104);
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_104 2;
        push_env w_104 (Memo.from_constructor tag_Z);
        assert_env_length w_104 3;
        let ctor_arg_30 = pop_env w_104 in
        push_env w_104 (Memo.appends [ Memo.from_constructor tag_S; ctor_arg_30 ]);
        assert_env_length w_104 3;
        let ctor_arg_31 = pop_env w_104 in
        push_env w_104 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_31 ]);
        assert_env_length w_104 3;
        let ctor_arg_32 = pop_env w_104 in
        push_env w_104 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_32 ]);
        assert_env_length w_104 3;
        return_n w_104 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_104 2;
        push_env w_104 (Dynarray.get w_104.state.e 1);
        w_104.state.c <- pc_to_exp (int_to_pc 104)))
    105;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 6;
      let last_51 = Source.E 5 in
      let x_51 = resolve w_106 last_51 in
      match Word.get_value (fst x_51) with
      | 10 (* tag_Found *) ->
          let splits_70 = Memo.splits (snd x_51) in
          let split0_70 = List.nth splits_70 0 in
          ignore (pop_env w_106);
          push_env w_106 split0_70;
          assert_env_length w_106 6;
          push_env w_106 (Dynarray.get w_106.state.e 2);
          assert_env_length w_106 7;
          push_env w_106 (Dynarray.get w_106.state.e 3);
          assert_env_length w_106 8;
          push_env w_106 (Dynarray.get w_106.state.e 5);
          assert_env_length w_106 9;
          let ctor_arg_33 = pop_env w_106 in
          let ctor_arg_34 = pop_env w_106 in
          push_env w_106 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_34; ctor_arg_33 ]);
          assert_env_length w_106 8;
          let ctor_arg_35 = pop_env w_106 in
          let ctor_arg_36 = pop_env w_106 in
          push_env w_106 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_36; ctor_arg_35 ]);
          assert_env_length w_106 7;
          drop_n w_106 7 1;
          assert_env_length w_106 6;
          drop_n w_106 6 1;
          assert_env_length w_106 5;
          drop_n w_106 5 2;
          assert_env_length w_106 3;
          return_n w_106 3 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_106);
          assert_env_length w_106 5;
          push_env w_106 (Dynarray.get w_106.state.e 3);
          assert_env_length w_106 6;
          push_env w_106 (Dynarray.get w_106.state.e 1);
          assert_env_length w_106 7;
          let keep_52 = env_call w_106 [ 0; 1; 2; 3 ] 2 in
          w_106.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; keep_52; w_106.state.k ];
          w_106.state.c <- pc_to_exp (int_to_pc 37)
      | _ -> failwith "unreachable (106)")
    106;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 5;
      let cond_3 = resolve w_108 (Source.E 4) in
      ignore (pop_env w_108);
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_108 4;
        push_env w_108 (Dynarray.get w_108.state.e 0);
        assert_env_length w_108 5;
        push_env w_108 (Dynarray.get w_108.state.e 1);
        assert_env_length w_108 6;
        let ctor_arg_39 = pop_env w_108 in
        let ctor_arg_40 = pop_env w_108 in
        push_env w_108 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_40; ctor_arg_39 ]);
        assert_env_length w_108 5;
        drop_n w_108 5 2;
        assert_env_length w_108 3;
        return_n w_108 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_108 4;
        push_env w_108 (Dynarray.get w_108.state.e 2);
        assert_env_length w_108 5;
        push_env w_108 (Dynarray.get w_108.state.e 0);
        assert_env_length w_108 6;
        push_env w_108 (Dynarray.get w_108.state.e 3);
        assert_env_length w_108 7;
        let keep_53 = env_call w_108 [ 4 ] 2 in
        w_108.state.k <- Memo.appends [ Memo.from_constructor tag_cont_56; keep_53; w_108.state.k ];
        w_108.state.c <- pc_to_exp (int_to_pc 42)))
    107;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 6;
      let x0_7 = resolve w_107 (Source.E 4) in
      let x1_7 = resolve w_107 (Source.E 5) in
      ignore (pop_env w_107);
      ignore (pop_env w_107);
      push_env w_107 (Memo.from_int (if Word.get_value (fst x0_7) <= Word.get_value (fst x1_7) then 1 else 0));
      w_107.state.c <- pc_to_exp (int_to_pc 107))
    108;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 2;
      let cond_4 = resolve w_109 (Source.E 1) in
      ignore (pop_env w_109);
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_109 1;
        push_env w_109 (Memo.from_constructor tag_ENil);
        assert_env_length w_109 2;
        drop_n w_109 2 0;
        assert_env_length w_109 2;
        return_n w_109 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_109 1;
        push_env w_109 (Dynarray.get w_109.state.e 0);
        assert_env_length w_109 2;
        push_env w_109 (Memo.from_constructor tag_ENil);
        assert_env_length w_109 3;
        let ctor_arg_41 = pop_env w_109 in
        let ctor_arg_42 = pop_env w_109 in
        push_env w_109 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_42; ctor_arg_41 ]);
        assert_env_length w_109 2;
        drop_n w_109 2 0;
        assert_env_length w_109 2;
        return_n w_109 2 (pc_to_exp (int_to_pc 0))))
    109;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 3;
      let last_52 = Source.E 2 in
      let x_52 = resolve w_110 last_52 in
      match Word.get_value (fst x_52) with
      | 5 (* tag_Const *) ->
          let splits_71 = Memo.splits (snd x_52) in
          let split0_71 = List.nth splits_71 0 in
          ignore (pop_env w_110);
          push_env w_110 split0_71;
          assert_env_length w_110 3;
          push_env w_110 (Dynarray.get w_110.state.e 2);
          assert_env_length w_110 4;
          let keep_57 = env_call w_110 [ 0; 1 ] 1 in
          w_110.state.k <- Memo.appends [ Memo.from_constructor tag_cont_60; keep_57; w_110.state.k ];
          w_110.state.c <- pc_to_exp (int_to_pc 13)
      | _ ->
          ignore (pop_env w_110);
          assert_env_length w_110 2;
          push_env w_110 (Dynarray.get w_110.state.e 1);
          assert_env_length w_110 3;
          push_env w_110 (Dynarray.get w_110.state.e 0);
          assert_env_length w_110 4;
          let keep_58 = env_call w_110 [ 2 ] 1 in
          w_110.state.k <- Memo.appends [ Memo.from_constructor tag_cont_61; keep_58; w_110.state.k ];
          w_110.state.c <- pc_to_exp (int_to_pc 61)
      | _ -> failwith "unreachable (110)")
    110;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 3;
      let cond_5 = resolve w_111 (Source.E 2) in
      ignore (pop_env w_111);
      if Word.get_value (fst cond_5) <> 0 then (
        assert_env_length w_111 2;
        push_env w_111 (Memo.from_constructor tag_ENil);
        assert_env_length w_111 3;
        return_n w_111 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_111 2;
        push_env w_111 (Dynarray.get w_111.state.e 1);
        assert_env_length w_111 3;
        push_env w_111 (Dynarray.get w_111.state.e 0);
        assert_env_length w_111 4;
        let keep_61 = env_call w_111 [] 2 in
        w_111.state.k <- Memo.appends [ Memo.from_constructor tag_cont_64; keep_61; w_111.state.k ];
        w_111.state.c <- pc_to_exp (int_to_pc 30)))
    111;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 3;
      let last_53 = Source.E 2 in
      let x_53 = resolve w_112 last_53 in
      match Word.get_value (fst x_53) with
      | 13 (* tag_NoPick *) ->
          ignore (pop_env w_112);
          assert_env_length w_112 2;
          push_env w_112 (Dynarray.get w_112.state.e 0);
          assert_env_length w_112 3;
          push_env w_112 (Dynarray.get w_112.state.e 1);
          assert_env_length w_112 4;
          let keep_66 = env_call w_112 [ 2 ] 1 in
          w_112.state.k <- Memo.appends [ Memo.from_constructor tag_cont_69; keep_66; w_112.state.k ];
          w_112.state.c <- pc_to_exp (int_to_pc 76)
      | 14 (* tag_Pick *) ->
          let splits_72 = Memo.splits (snd x_53) in
          let split0_72 = List.nth splits_72 0 in
          let split1_38 = List.nth splits_72 1 in
          ignore (pop_env w_112);
          push_env w_112 split0_72;
          push_env w_112 split1_38;
          assert_env_length w_112 4;
          push_env w_112 (Dynarray.get w_112.state.e 2);
          assert_env_length w_112 5;
          push_env w_112 (Dynarray.get w_112.state.e 3);
          assert_env_length w_112 6;
          let keep_67 = env_call w_112 [] 2 in
          w_112.state.k <- Memo.appends [ Memo.from_constructor tag_cont_70; keep_67; w_112.state.k ];
          w_112.state.c <- pc_to_exp (int_to_pc 42)
      | _ -> failwith "unreachable (112)")
    112;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 5;
      let x0_8 = resolve w_113 (Source.E 3) in
      let x1_8 = resolve w_113 (Source.E 4) in
      ignore (pop_env w_113);
      ignore (pop_env w_113);
      push_env w_113 (Memo.from_int (Word.get_value (fst x0_8) - Word.get_value (fst x1_8)));
      assert_env_length w_113 4;
      push_env w_113 (Dynarray.get w_113.state.e 1);
      assert_env_length w_113 5;
      let keep_71 = env_call w_113 [ 0; 2 ] 2 in
      w_113.state.k <- Memo.appends [ Memo.from_constructor tag_cont_74; keep_71; w_113.state.k ];
      w_113.state.c <- pc_to_exp (int_to_pc 83))
    113;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 5;
      let x0_9 = resolve w_114 (Source.E 3) in
      let x1_9 = resolve w_114 (Source.E 4) in
      ignore (pop_env w_114);
      ignore (pop_env w_114);
      push_env w_114 (Memo.from_int (Word.get_value (fst x0_9) - Word.get_value (fst x1_9)));
      assert_env_length w_114 4;
      push_env w_114 (Dynarray.get w_114.state.e 1);
      assert_env_length w_114 5;
      let keep_72 = env_call w_114 [ 0; 2 ] 2 in
      w_114.state.k <- Memo.appends [ Memo.from_constructor tag_cont_75; keep_72; w_114.state.k ];
      w_114.state.c <- pc_to_exp (int_to_pc 83))
    114;
  add_exp
    (fun w_120 ->
      assert_env_length w_120 6;
      let last_55 = Source.E 5 in
      let x_55 = resolve w_120 last_55 in
      match Word.get_value (fst x_55) with
      | 5 (* tag_Const *) ->
          let splits_74 = Memo.splits (snd x_55) in
          let split0_74 = List.nth splits_74 0 in
          ignore (pop_env w_120);
          push_env w_120 split0_74;
          assert_env_length w_120 6;
          push_env w_120 (Dynarray.get w_120.state.e 4);
          assert_env_length w_120 7;
          push_env w_120 (Dynarray.get w_120.state.e 5);
          assert_env_length w_120 8;
          ignore (env_call w_120 [] 2);
          w_120.state.c <- pc_to_exp (int_to_pc 5)
      | _ -> failwith "unreachable (115)")
    115;
  add_exp
    (fun w_121 ->
      assert_env_length w_121 6;
      let last_56 = Source.E 5 in
      let x_56 = resolve w_121 last_56 in
      match Word.get_value (fst x_56) with
      | 6 (* tag_Var *) ->
          let splits_76 = Memo.splits (snd x_56) in
          let split0_76 = List.nth splits_76 0 in
          ignore (pop_env w_121);
          push_env w_121 split0_76;
          assert_env_length w_121 6;
          push_env w_121 (Dynarray.get w_121.state.e 4);
          assert_env_length w_121 7;
          let keep_81 = env_call w_121 [ 5 ] 1 in
          w_121.state.k <- Memo.appends [ Memo.from_constructor tag_cont_84; keep_81; w_121.state.k ];
          w_121.state.c <- pc_to_exp (int_to_pc 16)
      | _ -> failwith "unreachable (116)")
    116;
  add_exp
    (fun w_122 ->
      assert_env_length w_122 7;
      let last_57 = Source.E 6 in
      let x_57 = resolve w_122 last_57 in
      match Word.get_value (fst x_57) with
      | 7 (* tag_Add *) ->
          let splits_78 = Memo.splits (snd x_57) in
          let split0_78 = List.nth splits_78 0 in
          let split1_40 = List.nth splits_78 1 in
          ignore (pop_env w_122);
          push_env w_122 split0_78;
          push_env w_122 split1_40;
          assert_env_length w_122 8;
          push_env w_122 (Dynarray.get w_122.state.e 4);
          assert_env_length w_122 9;
          push_env w_122 (Dynarray.get w_122.state.e 6);
          assert_env_length w_122 10;
          let keep_82 = env_call w_122 [ 5; 7 ] 2 in
          w_122.state.k <- Memo.appends [ Memo.from_constructor tag_cont_85; keep_82; w_122.state.k ];
          w_122.state.c <- pc_to_exp (int_to_pc 20)
      | _ -> failwith "unreachable (117)")
    117;
  add_exp
    (fun w_123 ->
      assert_env_length w_123 7;
      let last_58 = Source.E 6 in
      let x_58 = resolve w_123 last_58 in
      match Word.get_value (fst x_58) with
      | 8 (* tag_Mul *) ->
          let splits_80 = Memo.splits (snd x_58) in
          let split0_80 = List.nth splits_80 0 in
          let split1_42 = List.nth splits_80 1 in
          ignore (pop_env w_123);
          push_env w_123 split0_80;
          push_env w_123 split1_42;
          assert_env_length w_123 8;
          push_env w_123 (Dynarray.get w_123.state.e 4);
          assert_env_length w_123 9;
          push_env w_123 (Dynarray.get w_123.state.e 6);
          assert_env_length w_123 10;
          let keep_83 = env_call w_123 [ 5; 7 ] 2 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_86; keep_83; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 20)
      | _ -> failwith "unreachable (118)")
    118;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 5;
      let last_54 = Source.E 4 in
      let x_54 = resolve w_119 last_54 in
      match Word.get_value (fst x_54) with
      | 5 (* tag_Const *) ->
          let splits_73 = Memo.splits (snd x_54) in
          let split0_73 = List.nth splits_73 0 in
          ignore (pop_env w_119);
          push_env w_119 split0_73;
          assert_env_length w_119 5;
          push_env w_119 (Dynarray.get w_119.state.e 1);
          w_119.state.c <- pc_to_exp (int_to_pc 115)
      | 6 (* tag_Var *) ->
          let splits_75 = Memo.splits (snd x_54) in
          let split0_75 = List.nth splits_75 0 in
          ignore (pop_env w_119);
          push_env w_119 split0_75;
          assert_env_length w_119 5;
          push_env w_119 (Dynarray.get w_119.state.e 1);
          w_119.state.c <- pc_to_exp (int_to_pc 116)
      | 7 (* tag_Add *) ->
          let splits_77 = Memo.splits (snd x_54) in
          let split0_77 = List.nth splits_77 0 in
          let split1_39 = List.nth splits_77 1 in
          ignore (pop_env w_119);
          push_env w_119 split0_77;
          push_env w_119 split1_39;
          assert_env_length w_119 6;
          push_env w_119 (Dynarray.get w_119.state.e 1);
          w_119.state.c <- pc_to_exp (int_to_pc 117)
      | 8 (* tag_Mul *) ->
          let splits_79 = Memo.splits (snd x_54) in
          let split0_79 = List.nth splits_79 0 in
          let split1_41 = List.nth splits_79 1 in
          ignore (pop_env w_119);
          push_env w_119 split0_79;
          push_env w_119 split1_41;
          assert_env_length w_119 6;
          push_env w_119 (Dynarray.get w_119.state.e 1);
          w_119.state.c <- pc_to_exp (int_to_pc 118)
      | _ -> failwith "unreachable (119)")
    119;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 5;
      let cond_7 = resolve w_118 (Source.E 4) in
      ignore (pop_env w_118);
      if Word.get_value (fst cond_7) <> 0 then (
        assert_env_length w_118 4;
        push_env w_118 (Memo.from_int 1);
        assert_env_length w_118 5;
        drop_n w_118 5 1;
        assert_env_length w_118 4;
        drop_n w_118 4 1;
        assert_env_length w_118 3;
        return_n w_118 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_118 4;
        push_env w_118 (Dynarray.get w_118.state.e 0);
        w_118.state.c <- pc_to_exp (int_to_pc 119)))
    120;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 6;
      let x0_11 = resolve w_117 (Source.E 4) in
      let x1_11 = resolve w_117 (Source.E 5) in
      ignore (pop_env w_117);
      ignore (pop_env w_117);
      push_env w_117 (Memo.from_int (if Word.get_value (fst x0_11) > Word.get_value (fst x1_11) then 1 else 0));
      w_117.state.c <- pc_to_exp (int_to_pc 120))
    121;
  add_exp
    (fun w_124 ->
      assert_env_length w_124 6;
      let x0_12 = resolve w_124 (Source.E 4) in
      let x1_12 = resolve w_124 (Source.E 5) in
      ignore (pop_env w_124);
      ignore (pop_env w_124);
      push_env w_124 (Memo.from_int (Word.get_value (fst x0_12) - Word.get_value (fst x1_12)));
      assert_env_length w_124 5;
      drop_n w_124 5 1;
      assert_env_length w_124 4;
      drop_n w_124 4 1;
      assert_env_length w_124 3;
      return_n w_124 3 (pc_to_exp (int_to_pc 0)))
    122;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 5;
      let cond_6 = resolve w_116 (Source.E 4) in
      ignore (pop_env w_116);
      if Word.get_value (fst cond_6) <> 0 then (
        assert_env_length w_116 4;
        push_env w_116 (Memo.from_int 0);
        assert_env_length w_116 5;
        push_env w_116 (Memo.from_int 1);
        w_116.state.c <- pc_to_exp (int_to_pc 122))
      else (
        assert_env_length w_116 4;
        push_env w_116 (Dynarray.get w_116.state.e 2);
        assert_env_length w_116 5;
        push_env w_116 (Dynarray.get w_116.state.e 3);
        w_116.state.c <- pc_to_exp (int_to_pc 121)))
    123;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 6;
      let x0_10 = resolve w_115 (Source.E 4) in
      let x1_10 = resolve w_115 (Source.E 5) in
      ignore (pop_env w_115);
      ignore (pop_env w_115);
      push_env w_115 (Memo.from_int (if Word.get_value (fst x0_10) < Word.get_value (fst x1_10) then 1 else 0));
      w_115.state.c <- pc_to_exp (int_to_pc 123))
    124;
  add_exp
    (fun w_125 ->
      assert_env_length w_125 2;
      let x0_13 = resolve w_125 (Source.E 0) in
      let x1_13 = resolve w_125 (Source.E 1) in
      ignore (pop_env w_125);
      ignore (pop_env w_125);
      push_env w_125 (Memo.from_int (if Word.get_value (fst x0_13) = Word.get_value (fst x1_13) then 1 else 0));
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
      let x0_14 = resolve w_126 (Source.E 0) in
      let x1_14 = resolve w_126 (Source.E 1) in
      ignore (pop_env w_126);
      ignore (pop_env w_126);
      push_env w_126
        (Memo.from_int (if Word.get_value (fst x0_14) <> 0 && Word.get_value (fst x1_14) <> 0 then 1 else 0));
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
      let x0_15 = resolve w_127 (Source.E 0) in
      let x1_15 = resolve w_127 (Source.E 1) in
      ignore (pop_env w_127);
      ignore (pop_env w_127);
      push_env w_127
        (Memo.from_int (if Word.get_value (fst x0_15) <> 0 && Word.get_value (fst x1_15) <> 0 then 1 else 0));
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
      let x0_16 = resolve w_128 (Source.E 0) in
      let x1_16 = resolve w_128 (Source.E 1) in
      ignore (pop_env w_128);
      ignore (pop_env w_128);
      push_env w_128 (Memo.from_int (Word.get_value (fst x0_16) + Word.get_value (fst x1_16)));
      assert_env_length w_128 1;
      drop_n w_128 1 0;
      assert_env_length w_128 1;
      return_n w_128 1 (pc_to_exp (int_to_pc 0)))
    128;
  add_exp
    (fun w_129 ->
      assert_env_length w_129 2;
      let x0_17 = resolve w_129 (Source.E 0) in
      let x1_17 = resolve w_129 (Source.E 1) in
      ignore (pop_env w_129);
      ignore (pop_env w_129);
      push_env w_129 (Memo.from_int (Word.get_value (fst x0_17) + Word.get_value (fst x1_17)));
      assert_env_length w_129 1;
      drop_n w_129 1 0;
      assert_env_length w_129 1;
      return_n w_129 1 (pc_to_exp (int_to_pc 0)))
    129;
  add_exp
    (fun w_133 ->
      assert_env_length w_133 5;
      let cond_9 = resolve w_133 (Source.E 4) in
      ignore (pop_env w_133);
      if Word.get_value (fst cond_9) <> 0 then (
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
        let keep_84 = env_call w_133 [ 0; 1 ] 2 in
        w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_87; keep_84; w_133.state.k ];
        w_133.state.c <- pc_to_exp (int_to_pc 20)))
    130;
  add_exp
    (fun w_132 ->
      assert_env_length w_132 6;
      let x0_19 = resolve w_132 (Source.E 4) in
      let x1_19 = resolve w_132 (Source.E 5) in
      ignore (pop_env w_132);
      ignore (pop_env w_132);
      push_env w_132 (Memo.from_int (if Word.get_value (fst x0_19) < Word.get_value (fst x1_19) then 1 else 0));
      w_132.state.c <- pc_to_exp (int_to_pc 130))
    131;
  add_exp
    (fun w_131 ->
      assert_env_length w_131 5;
      let cond_8 = resolve w_131 (Source.E 4) in
      ignore (pop_env w_131);
      if Word.get_value (fst cond_8) <> 0 then (
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
      let x0_18 = resolve w_130 (Source.E 4) in
      let x1_18 = resolve w_130 (Source.E 5) in
      ignore (pop_env w_130);
      ignore (pop_env w_130);
      push_env w_130 (Memo.from_int (if Word.get_value (fst x0_18) < Word.get_value (fst x1_18) then 1 else 0));
      w_130.state.c <- pc_to_exp (int_to_pc 132))
    133;
  add_exp
    (fun w_134 ->
      assert_env_length w_134 3;
      let cond_10 = resolve w_134 (Source.E 2) in
      ignore (pop_env w_134);
      if Word.get_value (fst cond_10) <> 0 then (
        assert_env_length w_134 2;
        push_env w_134 (Dynarray.get w_134.state.e 1);
        assert_env_length w_134 3;
        return_n w_134 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_134 2;
        push_env w_134 (Dynarray.get w_134.state.e 0);
        assert_env_length w_134 3;
        let ctor_arg_54 = pop_env w_134 in
        push_env w_134 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_54 ]);
        assert_env_length w_134 3;
        push_env w_134 (Dynarray.get w_134.state.e 1);
        assert_env_length w_134 4;
        let ctor_arg_55 = pop_env w_134 in
        let ctor_arg_56 = pop_env w_134 in
        push_env w_134 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_56; ctor_arg_55 ]);
        assert_env_length w_134 3;
        return_n w_134 3 (pc_to_exp (int_to_pc 0))))
    134;
  add_exp
    (fun w_135 ->
      assert_env_length w_135 5;
      let last_59 = Source.E 4 in
      let x_59 = resolve w_135 last_59 in
      match Word.get_value (fst x_59) with
      | 10 (* tag_Found *) ->
          let splits_81 = Memo.splits (snd x_59) in
          let split0_81 = List.nth splits_81 0 in
          ignore (pop_env w_135);
          push_env w_135 split0_81;
          assert_env_length w_135 5;
          push_env w_135 (Dynarray.get w_135.state.e 4);
          assert_env_length w_135 6;
          push_env w_135 (Dynarray.get w_135.state.e 2);
          assert_env_length w_135 7;
          let ctor_arg_57 = pop_env w_135 in
          let ctor_arg_58 = pop_env w_135 in
          push_env w_135 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_58; ctor_arg_57 ]);
          assert_env_length w_135 6;
          let ctor_arg_59 = pop_env w_135 in
          push_env w_135 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_59 ]);
          assert_env_length w_135 6;
          drop_n w_135 6 1;
          assert_env_length w_135 5;
          drop_n w_135 5 1;
          assert_env_length w_135 4;
          drop_n w_135 4 2;
          assert_env_length w_135 2;
          return_n w_135 2 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_135);
          assert_env_length w_135 4;
          push_env w_135 (Dynarray.get w_135.state.e 0);
          assert_env_length w_135 5;
          push_env w_135 (Dynarray.get w_135.state.e 2);
          assert_env_length w_135 6;
          let keep_85 = env_call w_135 [ 1 ] 2 in
          w_135.state.k <- Memo.appends [ Memo.from_constructor tag_cont_88; keep_85; w_135.state.k ];
          w_135.state.c <- pc_to_exp (int_to_pc 37)
      | _ -> failwith "unreachable (135)")
    135;
  add_exp
    (fun w_136 ->
      assert_env_length w_136 6;
      let last_60 = Source.E 5 in
      let x_60 = resolve w_136 last_60 in
      match Word.get_value (fst x_60) with
      | 10 (* tag_Found *) ->
          let splits_82 = Memo.splits (snd x_60) in
          let split0_82 = List.nth splits_82 0 in
          ignore (pop_env w_136);
          push_env w_136 split0_82;
          assert_env_length w_136 6;
          push_env w_136 (Dynarray.get w_136.state.e 3);
          assert_env_length w_136 7;
          push_env w_136 (Dynarray.get w_136.state.e 2);
          assert_env_length w_136 8;
          push_env w_136 (Dynarray.get w_136.state.e 5);
          assert_env_length w_136 9;
          let ctor_arg_60 = pop_env w_136 in
          let ctor_arg_61 = pop_env w_136 in
          push_env w_136 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_61; ctor_arg_60 ]);
          assert_env_length w_136 8;
          let ctor_arg_62 = pop_env w_136 in
          let ctor_arg_63 = pop_env w_136 in
          push_env w_136 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_63; ctor_arg_62 ]);
          assert_env_length w_136 7;
          drop_n w_136 7 1;
          assert_env_length w_136 6;
          drop_n w_136 6 1;
          assert_env_length w_136 5;
          drop_n w_136 5 0;
          assert_env_length w_136 5;
          drop_n w_136 5 2;
          assert_env_length w_136 3;
          return_n w_136 3 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_136);
          assert_env_length w_136 5;
          push_env w_136 (Dynarray.get w_136.state.e 0);
          assert_env_length w_136 6;
          push_env w_136 (Dynarray.get w_136.state.e 1);
          assert_env_length w_136 7;
          let ctor_arg_64 = pop_env w_136 in
          let ctor_arg_65 = pop_env w_136 in
          push_env w_136 (Memo.appends [ Memo.from_constructor tag_Add; ctor_arg_65; ctor_arg_64 ]);
          assert_env_length w_136 6;
          drop_n w_136 6 1;
          assert_env_length w_136 5;
          drop_n w_136 5 0;
          assert_env_length w_136 5;
          drop_n w_136 5 2;
          assert_env_length w_136 3;
          return_n w_136 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (136)")
    136;
  add_exp
    (fun w_137 ->
      assert_env_length w_137 3;
      let cond_11 = resolve w_137 (Source.E 2) in
      ignore (pop_env w_137);
      if Word.get_value (fst cond_11) <> 0 then (
        assert_env_length w_137 2;
        push_env w_137 (Dynarray.get w_137.state.e 0);
        assert_env_length w_137 3;
        ignore (env_call w_137 [] 1);
        w_137.state.c <- pc_to_exp (int_to_pc 61))
      else (
        assert_env_length w_137 2;
        push_env w_137 (Dynarray.get w_137.state.e 1);
        assert_env_length w_137 3;
        push_env w_137 (Dynarray.get w_137.state.e 0);
        assert_env_length w_137 4;
        let keep_86 = env_call w_137 [ 2 ] 1 in
        w_137.state.k <- Memo.appends [ Memo.from_constructor tag_cont_89; keep_86; w_137.state.k ];
        w_137.state.c <- pc_to_exp (int_to_pc 61)))
    137;
  add_exp
    (fun w_138 ->
      assert_env_length w_138 5;
      let cond_12 = resolve w_138 (Source.E 4) in
      ignore (pop_env w_138);
      if Word.get_value (fst cond_12) <> 0 then (
        assert_env_length w_138 4;
        push_env w_138 (Dynarray.get w_138.state.e 0);
        assert_env_length w_138 5;
        push_env w_138 (Dynarray.get w_138.state.e 1);
        assert_env_length w_138 6;
        let keep_91 = env_call w_138 [ 4 ] 1 in
        w_138.state.k <- Memo.appends [ Memo.from_constructor tag_cont_93; keep_91; w_138.state.k ];
        w_138.state.c <- pc_to_exp (int_to_pc 71))
      else (
        assert_env_length w_138 4;
        push_env w_138 (Dynarray.get w_138.state.e 3);
        assert_env_length w_138 5;
        push_env w_138 (Dynarray.get w_138.state.e 2);
        assert_env_length w_138 6;
        let keep_90 = env_call w_138 [] 2 in
        w_138.state.k <- Memo.appends [ Memo.from_constructor tag_cont_92; keep_90; w_138.state.k ];
        w_138.state.c <- pc_to_exp (int_to_pc 42)))
    138;
  add_exp
    (fun w_139 ->
      assert_env_length w_139 5;
      let cond_13 = resolve w_139 (Source.E 4) in
      ignore (pop_env w_139);
      if Word.get_value (fst cond_13) <> 0 then (
        assert_env_length w_139 4;
        push_env w_139 (Dynarray.get w_139.state.e 0);
        assert_env_length w_139 5;
        push_env w_139 (Dynarray.get w_139.state.e 2);
        assert_env_length w_139 6;
        let keep_92 = env_call w_139 [ 1 ] 2 in
        w_139.state.k <- Memo.appends [ Memo.from_constructor tag_cont_94; keep_92; w_139.state.k ];
        w_139.state.c <- pc_to_exp (int_to_pc 74))
      else (
        assert_env_length w_139 4;
        push_env w_139 (Dynarray.get w_139.state.e 3);
        assert_env_length w_139 5;
        push_env w_139 (Dynarray.get w_139.state.e 2);
        assert_env_length w_139 6;
        let ctor_arg_70 = pop_env w_139 in
        let ctor_arg_71 = pop_env w_139 in
        push_env w_139 (Memo.appends [ Memo.from_constructor tag_Pick; ctor_arg_71; ctor_arg_70 ]);
        assert_env_length w_139 5;
        drop_n w_139 5 1;
        assert_env_length w_139 4;
        drop_n w_139 4 2;
        assert_env_length w_139 2;
        return_n w_139 2 (pc_to_exp (int_to_pc 0))))
    139;
  add_exp
    (fun w_141 ->
      assert_env_length w_141 4;
      let cond_14 = resolve w_141 (Source.E 3) in
      ignore (pop_env w_141);
      if Word.get_value (fst cond_14) <> 0 then (
        assert_env_length w_141 3;
        push_env w_141 (Dynarray.get w_141.state.e 0);
        assert_env_length w_141 4;
        push_env w_141 (Dynarray.get w_141.state.e 1);
        assert_env_length w_141 5;
        ignore (env_call w_141 [] 2);
        w_141.state.c <- pc_to_exp (int_to_pc 20))
      else (
        assert_env_length w_141 3;
        push_env w_141 (Dynarray.get w_141.state.e 2);
        assert_env_length w_141 4;
        drop_n w_141 4 1;
        assert_env_length w_141 3;
        drop_n w_141 3 1;
        assert_env_length w_141 2;
        drop_n w_141 2 1;
        assert_env_length w_141 1;
        drop_n w_141 1 0;
        assert_env_length w_141 1;
        drop_n w_141 1 0;
        assert_env_length w_141 1;
        return_n w_141 1 (pc_to_exp (int_to_pc 0))))
    140;
  add_exp
    (fun w_140 ->
      assert_env_length w_140 5;
      let x0_20 = resolve w_140 (Source.E 3) in
      let x1_20 = resolve w_140 (Source.E 4) in
      ignore (pop_env w_140);
      ignore (pop_env w_140);
      push_env w_140 (Memo.from_int (if Word.get_value (fst x0_20) = Word.get_value (fst x1_20) then 1 else 0));
      w_140.state.c <- pc_to_exp (int_to_pc 140))
    141;
  add_exp
    (fun w_143 ->
      assert_env_length w_143 4;
      let cond_15 = resolve w_143 (Source.E 3) in
      ignore (pop_env w_143);
      if Word.get_value (fst cond_15) <> 0 then (
        assert_env_length w_143 3;
        push_env w_143 (Dynarray.get w_143.state.e 0);
        assert_env_length w_143 4;
        push_env w_143 (Dynarray.get w_143.state.e 1);
        assert_env_length w_143 5;
        ignore (env_call w_143 [] 2);
        w_143.state.c <- pc_to_exp (int_to_pc 20))
      else (
        assert_env_length w_143 3;
        push_env w_143 (Dynarray.get w_143.state.e 2);
        assert_env_length w_143 4;
        drop_n w_143 4 1;
        assert_env_length w_143 3;
        drop_n w_143 3 1;
        assert_env_length w_143 2;
        drop_n w_143 2 1;
        assert_env_length w_143 1;
        drop_n w_143 1 0;
        assert_env_length w_143 1;
        drop_n w_143 1 0;
        assert_env_length w_143 1;
        return_n w_143 1 (pc_to_exp (int_to_pc 0))))
    142;
  add_exp
    (fun w_142 ->
      assert_env_length w_142 5;
      let x0_21 = resolve w_142 (Source.E 3) in
      let x1_21 = resolve w_142 (Source.E 4) in
      ignore (pop_env w_142);
      ignore (pop_env w_142);
      push_env w_142 (Memo.from_int (if Word.get_value (fst x0_21) = Word.get_value (fst x1_21) then 1 else 0));
      w_142.state.c <- pc_to_exp (int_to_pc 142))
    143;
  add_exp
    (fun w_145 ->
      assert_env_length w_145 3;
      let cond_16 = resolve w_145 (Source.E 2) in
      ignore (pop_env w_145);
      if Word.get_value (fst cond_16) <> 0 then (
        assert_env_length w_145 2;
        push_env w_145 (Dynarray.get w_145.state.e 0);
        assert_env_length w_145 3;
        drop_n w_145 3 0;
        assert_env_length w_145 3;
        drop_n w_145 3 0;
        assert_env_length w_145 3;
        return_n w_145 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_145 2;
        push_env w_145 (Dynarray.get w_145.state.e 1);
        assert_env_length w_145 3;
        drop_n w_145 3 0;
        assert_env_length w_145 3;
        drop_n w_145 3 0;
        assert_env_length w_145 3;
        return_n w_145 3 (pc_to_exp (int_to_pc 0))))
    144;
  add_exp
    (fun w_144 ->
      assert_env_length w_144 4;
      let x0_22 = resolve w_144 (Source.E 2) in
      let x1_22 = resolve w_144 (Source.E 3) in
      ignore (pop_env w_144);
      ignore (pop_env w_144);
      push_env w_144 (Memo.from_int (if Word.get_value (fst x0_22) <= Word.get_value (fst x1_22) then 1 else 0));
      w_144.state.c <- pc_to_exp (int_to_pc 144))
    145;
  add_exp
    (fun w_146 ->
      assert_env_length w_146 3;
      let last_61 = Source.E 2 in
      let x_61 = resolve w_146 last_61 in
      match Word.get_value (fst x_61) with
      | 10 (* tag_Found *) ->
          let splits_83 = Memo.splits (snd x_61) in
          let split0_83 = List.nth splits_83 0 in
          ignore (pop_env w_146);
          push_env w_146 split0_83;
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          push_env w_146 (Dynarray.get w_146.state.e 2);
          assert_env_length w_146 5;
          let ctor_arg_80 = pop_env w_146 in
          let ctor_arg_81 = pop_env w_146 in
          push_env w_146 (Memo.appends [ Memo.from_constructor tag_Mul; ctor_arg_81; ctor_arg_80 ]);
          assert_env_length w_146 4;
          let ctor_arg_82 = pop_env w_146 in
          push_env w_146 (Memo.appends [ Memo.from_constructor tag_Found; ctor_arg_82 ]);
          assert_env_length w_146 4;
          drop_n w_146 4 1;
          assert_env_length w_146 3;
          drop_n w_146 3 1;
          assert_env_length w_146 2;
          drop_n w_146 2 0;
          assert_env_length w_146 2;
          drop_n w_146 2 1;
          assert_env_length w_146 1;
          return_n w_146 1 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          ignore (pop_env w_146);
          assert_env_length w_146 2;
          push_env w_146 (Memo.from_constructor tag_Missing);
          assert_env_length w_146 3;
          drop_n w_146 3 1;
          assert_env_length w_146 2;
          drop_n w_146 2 0;
          assert_env_length w_146 2;
          drop_n w_146 2 1;
          assert_env_length w_146 1;
          return_n w_146 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (146)")
    146;
  add_exp
    (fun w_147 ->
      assert_env_length w_147 3;
      let cond_17 = resolve w_147 (Source.E 2) in
      ignore (pop_env w_147);
      if Word.get_value (fst cond_17) <> 0 then (
        assert_env_length w_147 2;
        push_env w_147 (Memo.from_constructor tag_Z);
        assert_env_length w_147 3;
        let ctor_arg_83 = pop_env w_147 in
        push_env w_147 (Memo.appends [ Memo.from_constructor tag_Const; ctor_arg_83 ]);
        assert_env_length w_147 3;
        drop_n w_147 3 1;
        assert_env_length w_147 2;
        drop_n w_147 2 1;
        assert_env_length w_147 1;
        return_n w_147 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_147 2;
        push_env w_147 (Dynarray.get w_147.state.e 0);
        assert_env_length w_147 3;
        let keep_100 = env_call w_147 [ 1 ] 1 in
        w_147.state.k <- Memo.appends [ Memo.from_constructor tag_cont_101; keep_100; w_147.state.k ];
        w_147.state.c <- pc_to_exp (int_to_pc 61)))
    147;
  add_exp
    (fun w_148 ->
      assert_env_length w_148 6;
      let cond_18 = resolve w_148 (Source.E 5) in
      ignore (pop_env w_148);
      if Word.get_value (fst cond_18) <> 0 then (
        assert_env_length w_148 5;
        push_env w_148 (Dynarray.get w_148.state.e 0);
        assert_env_length w_148 6;
        push_env w_148 (Dynarray.get w_148.state.e 1);
        assert_env_length w_148 7;
        push_env w_148 (Dynarray.get w_148.state.e 4);
        assert_env_length w_148 8;
        let keep_102 = env_call w_148 [ 2; 5 ] 2 in
        w_148.state.k <- Memo.appends [ Memo.from_constructor tag_cont_103; keep_102; w_148.state.k ];
        w_148.state.c <- pc_to_exp (int_to_pc 1))
      else (
        assert_env_length w_148 5;
        push_env w_148 (Dynarray.get w_148.state.e 3);
        assert_env_length w_148 6;
        push_env w_148 (Dynarray.get w_148.state.e 4);
        assert_env_length w_148 7;
        push_env w_148 (Dynarray.get w_148.state.e 2);
        assert_env_length w_148 8;
        let keep_101 = env_call w_148 [ 0; 1 ] 3 in
        w_148.state.k <- Memo.appends [ Memo.from_constructor tag_cont_102; keep_101; w_148.state.k ];
        w_148.state.c <- pc_to_exp (int_to_pc 67)))
    148;
  add_exp
    (fun w_149 ->
      assert_env_length w_149 2;
      let last_62 = Source.E 1 in
      let x_62 = resolve w_149 last_62 in
      match Word.get_value (fst x_62) with
      | 13 (* tag_NoPick *) ->
          ignore (pop_env w_149);
          assert_env_length w_149 1;
          push_env w_149 (Memo.from_constructor tag_NoPick);
          assert_env_length w_149 2;
          drop_n w_149 2 0;
          assert_env_length w_149 2;
          drop_n w_149 2 1;
          assert_env_length w_149 1;
          return_n w_149 1 (pc_to_exp (int_to_pc 0))
      | 14 (* tag_Pick *) ->
          let splits_84 = Memo.splits (snd x_62) in
          let split0_84 = List.nth splits_84 0 in
          let split1_43 = List.nth splits_84 1 in
          ignore (pop_env w_149);
          push_env w_149 split0_84;
          push_env w_149 split1_43;
          assert_env_length w_149 3;
          push_env w_149 (Dynarray.get w_149.state.e 1);
          assert_env_length w_149 4;
          push_env w_149 (Dynarray.get w_149.state.e 0);
          assert_env_length w_149 5;
          push_env w_149 (Dynarray.get w_149.state.e 2);
          assert_env_length w_149 6;
          let ctor_arg_86 = pop_env w_149 in
          let ctor_arg_87 = pop_env w_149 in
          push_env w_149 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_87; ctor_arg_86 ]);
          assert_env_length w_149 5;
          let ctor_arg_88 = pop_env w_149 in
          let ctor_arg_89 = pop_env w_149 in
          push_env w_149 (Memo.appends [ Memo.from_constructor tag_Pick; ctor_arg_89; ctor_arg_88 ]);
          assert_env_length w_149 4;
          drop_n w_149 4 2;
          assert_env_length w_149 2;
          drop_n w_149 2 0;
          assert_env_length w_149 2;
          drop_n w_149 2 1;
          assert_env_length w_149 1;
          return_n w_149 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (149)")
    149;
  add_exp
    (fun w_153 ->
      assert_env_length w_153 3;
      let cond_20 = resolve w_153 (Source.E 2) in
      ignore (pop_env w_153);
      if Word.get_value (fst cond_20) <> 0 then (
        assert_env_length w_153 2;
        push_env w_153 (Memo.from_int 1);
        assert_env_length w_153 3;
        drop_n w_153 3 1;
        assert_env_length w_153 2;
        drop_n w_153 2 1;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        return_n w_153 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_153 2;
        push_env w_153 (Memo.from_int 0);
        assert_env_length w_153 3;
        drop_n w_153 3 1;
        assert_env_length w_153 2;
        drop_n w_153 2 1;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        drop_n w_153 1 0;
        assert_env_length w_153 1;
        return_n w_153 1 (pc_to_exp (int_to_pc 0))))
    150;
  add_exp
    (fun w_152 ->
      assert_env_length w_152 4;
      let x0_24 = resolve w_152 (Source.E 2) in
      let x1_24 = resolve w_152 (Source.E 3) in
      ignore (pop_env w_152);
      ignore (pop_env w_152);
      push_env w_152 (Memo.from_int (if Word.get_value (fst x0_24) > Word.get_value (fst x1_24) then 1 else 0));
      w_152.state.c <- pc_to_exp (int_to_pc 150))
    151;
  add_exp
    (fun w_154 ->
      assert_env_length w_154 4;
      let x0_25 = resolve w_154 (Source.E 2) in
      let x1_25 = resolve w_154 (Source.E 3) in
      ignore (pop_env w_154);
      ignore (pop_env w_154);
      push_env w_154 (Memo.from_int (Word.get_value (fst x0_25) - Word.get_value (fst x1_25)));
      assert_env_length w_154 3;
      drop_n w_154 3 1;
      assert_env_length w_154 2;
      drop_n w_154 2 1;
      assert_env_length w_154 1;
      drop_n w_154 1 0;
      assert_env_length w_154 1;
      drop_n w_154 1 0;
      assert_env_length w_154 1;
      drop_n w_154 1 0;
      assert_env_length w_154 1;
      drop_n w_154 1 0;
      assert_env_length w_154 1;
      return_n w_154 1 (pc_to_exp (int_to_pc 0)))
    152;
  add_exp
    (fun w_151 ->
      assert_env_length w_151 3;
      let cond_19 = resolve w_151 (Source.E 2) in
      ignore (pop_env w_151);
      if Word.get_value (fst cond_19) <> 0 then (
        assert_env_length w_151 2;
        push_env w_151 (Memo.from_int 0);
        assert_env_length w_151 3;
        push_env w_151 (Memo.from_int 1);
        w_151.state.c <- pc_to_exp (int_to_pc 152))
      else (
        assert_env_length w_151 2;
        push_env w_151 (Dynarray.get w_151.state.e 0);
        assert_env_length w_151 3;
        push_env w_151 (Dynarray.get w_151.state.e 1);
        w_151.state.c <- pc_to_exp (int_to_pc 151)))
    153;
  add_exp
    (fun w_150 ->
      assert_env_length w_150 4;
      let x0_23 = resolve w_150 (Source.E 2) in
      let x1_23 = resolve w_150 (Source.E 3) in
      ignore (pop_env w_150);
      ignore (pop_env w_150);
      push_env w_150 (Memo.from_int (if Word.get_value (fst x0_23) < Word.get_value (fst x1_23) then 1 else 0));
      w_150.state.c <- pc_to_exp (int_to_pc 153))
    154;
  add_exp
    (fun w_155 ->
      assert_env_length w_155 2;
      let cond_21 = resolve w_155 (Source.E 1) in
      ignore (pop_env w_155);
      if Word.get_value (fst cond_21) <> 0 then (
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
        w_155.state.c <- pc_to_exp (int_to_pc 91)))
    155;
  add_exp
    (fun w_156 ->
      assert_env_length w_156 4;
      let cond_22 = resolve w_156 (Source.E 3) in
      ignore (pop_env w_156);
      if Word.get_value (fst cond_22) <> 0 then (
        assert_env_length w_156 3;
        push_env w_156 (Dynarray.get w_156.state.e 2);
        assert_env_length w_156 4;
        drop_n w_156 4 1;
        assert_env_length w_156 3;
        drop_n w_156 3 0;
        assert_env_length w_156 3;
        drop_n w_156 3 0;
        assert_env_length w_156 3;
        drop_n w_156 3 0;
        assert_env_length w_156 3;
        return_n w_156 3 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_156 3;
        push_env w_156 (Dynarray.get w_156.state.e 1);
        assert_env_length w_156 4;
        push_env w_156 (Dynarray.get w_156.state.e 0);
        assert_env_length w_156 5;
        let keep_115 = env_call w_156 [ 2 ] 2 in
        w_156.state.k <- Memo.appends [ Memo.from_constructor tag_cont_117; keep_115; w_156.state.k ];
        w_156.state.c <- pc_to_exp (int_to_pc 30)))
    156;
  add_exp
    (fun w_157 ->
      assert_env_length w_157 3;
      let cond_23 = resolve w_157 (Source.E 2) in
      ignore (pop_env w_157);
      if Word.get_value (fst cond_23) <> 0 then (
        assert_env_length w_157 2;
        push_env w_157 (Dynarray.get w_157.state.e 1);
        assert_env_length w_157 3;
        drop_n w_157 3 1;
        assert_env_length w_157 2;
        drop_n w_157 2 0;
        assert_env_length w_157 2;
        drop_n w_157 2 1;
        assert_env_length w_157 1;
        drop_n w_157 1 0;
        assert_env_length w_157 1;
        return_n w_157 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_157 2;
        push_env w_157 (Dynarray.get w_157.state.e 0);
        assert_env_length w_157 3;
        push_env w_157 (Dynarray.get w_157.state.e 1);
        assert_env_length w_157 4;
        ignore (env_call w_157 [] 2);
        w_157.state.c <- pc_to_exp (int_to_pc 30)))
    157;
  add_exp
    (fun w_159 ->
      assert_env_length w_159 5;
      let x0_26 = resolve w_159 (Source.E 3) in
      let x1_26 = resolve w_159 (Source.E 4) in
      ignore (pop_env w_159);
      ignore (pop_env w_159);
      push_env w_159 (Memo.from_int (Word.get_value (fst x0_26) - Word.get_value (fst x1_26)));
      assert_env_length w_159 4;
      push_env w_159 (Dynarray.get w_159.state.e 2);
      assert_env_length w_159 5;
      ignore (env_call w_159 [] 2);
      w_159.state.c <- pc_to_exp (int_to_pc 83))
    158;
  add_exp
    (fun w_158 ->
      assert_env_length w_158 4;
      let cond_24 = resolve w_158 (Source.E 3) in
      ignore (pop_env w_158);
      if Word.get_value (fst cond_24) <> 0 then (
        assert_env_length w_158 3;
        push_env w_158 (Dynarray.get w_158.state.e 1);
        assert_env_length w_158 4;
        drop_n w_158 4 1;
        assert_env_length w_158 3;
        drop_n w_158 3 0;
        assert_env_length w_158 3;
        drop_n w_158 3 0;
        assert_env_length w_158 3;
        drop_n w_158 3 1;
        assert_env_length w_158 2;
        drop_n w_158 2 0;
        assert_env_length w_158 2;
        drop_n w_158 2 0;
        assert_env_length w_158 2;
        drop_n w_158 2 0;
        assert_env_length w_158 2;
        return_n w_158 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_158 3;
        push_env w_158 (Dynarray.get w_158.state.e 0);
        assert_env_length w_158 4;
        push_env w_158 (Memo.from_int 1);
        w_158.state.c <- pc_to_exp (int_to_pc 158)))
    159;
  add_exp
    (fun w_161 ->
      assert_env_length w_161 5;
      let x0_27 = resolve w_161 (Source.E 3) in
      let x1_27 = resolve w_161 (Source.E 4) in
      ignore (pop_env w_161);
      ignore (pop_env w_161);
      push_env w_161 (Memo.from_int (Word.get_value (fst x0_27) - Word.get_value (fst x1_27)));
      assert_env_length w_161 4;
      push_env w_161 (Dynarray.get w_161.state.e 2);
      assert_env_length w_161 5;
      ignore (env_call w_161 [] 2);
      w_161.state.c <- pc_to_exp (int_to_pc 83))
    160;
  add_exp
    (fun w_160 ->
      assert_env_length w_160 4;
      let cond_25 = resolve w_160 (Source.E 3) in
      ignore (pop_env w_160);
      if Word.get_value (fst cond_25) <> 0 then (
        assert_env_length w_160 3;
        push_env w_160 (Dynarray.get w_160.state.e 1);
        assert_env_length w_160 4;
        drop_n w_160 4 1;
        assert_env_length w_160 3;
        drop_n w_160 3 0;
        assert_env_length w_160 3;
        drop_n w_160 3 0;
        assert_env_length w_160 3;
        drop_n w_160 3 1;
        assert_env_length w_160 2;
        drop_n w_160 2 0;
        assert_env_length w_160 2;
        drop_n w_160 2 0;
        assert_env_length w_160 2;
        drop_n w_160 2 0;
        assert_env_length w_160 2;
        return_n w_160 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_160 3;
        push_env w_160 (Dynarray.get w_160.state.e 0);
        assert_env_length w_160 4;
        push_env w_160 (Memo.from_int 1);
        w_160.state.c <- pc_to_exp (int_to_pc 160)))
    161;
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
  Words.set_constructor_degree 15 0;
  Words.set_constructor_degree 16 (-1);
  Words.set_constructor_degree 17 0;
  Words.set_constructor_degree 18 (-2);
  Words.set_constructor_degree 19 (-1);
  Words.set_constructor_degree 20 (-2);
  Words.set_constructor_degree 21 (-2);
  Words.set_constructor_degree 22 (-2);
  Words.set_constructor_degree 23 (-2);
  Words.set_constructor_degree 24 (-2);
  Words.set_constructor_degree 25 (-2);
  Words.set_constructor_degree 26 (-2);
  Words.set_constructor_degree 27 (-4);
  Words.set_constructor_degree 28 (-1);
  Words.set_constructor_degree 29 (-4);
  Words.set_constructor_degree 30 (-1);
  Words.set_constructor_degree 31 (-1);
  Words.set_constructor_degree 32 (-1);
  Words.set_constructor_degree 33 (-1);
  Words.set_constructor_degree 34 (-1);
  Words.set_constructor_degree 35 (-1);
  Words.set_constructor_degree 36 (-1);
  Words.set_constructor_degree 37 0;
  Words.set_constructor_degree 38 (-1);
  Words.set_constructor_degree 39 (-2);
  Words.set_constructor_degree 40 (-4);
  Words.set_constructor_degree 41 (-2);
  Words.set_constructor_degree 42 (-4);
  Words.set_constructor_degree 43 (-3);
  Words.set_constructor_degree 44 (-2);
  Words.set_constructor_degree 45 (-1);
  Words.set_constructor_degree 46 0;
  Words.set_constructor_degree 47 0;
  Words.set_constructor_degree 48 (-1);
  Words.set_constructor_degree 49 (-2);
  Words.set_constructor_degree 50 (-2);
  Words.set_constructor_degree 51 (-1);
  Words.set_constructor_degree 52 (-1);
  Words.set_constructor_degree 53 (-1);
  Words.set_constructor_degree 54 (-1);
  Words.set_constructor_degree 55 (-2);
  Words.set_constructor_degree 56 (-3);
  Words.set_constructor_degree 57 (-3);
  Words.set_constructor_degree 58 0;
  Words.set_constructor_degree 59 (-3);
  Words.set_constructor_degree 60 (-1);
  Words.set_constructor_degree 61 (-1);
  Words.set_constructor_degree 62 (-1);
  Words.set_constructor_degree 63 (-1);
  Words.set_constructor_degree 64 (-1);
  Words.set_constructor_degree 65 (-3);
  Words.set_constructor_degree 66 0;
  Words.set_constructor_degree 67 (-2);
  Words.set_constructor_degree 68 (-3);
  Words.set_constructor_degree 69 (-4);
  Words.set_constructor_degree 70 (-1);
  Words.set_constructor_degree 71 (-1);
  Words.set_constructor_degree 72 (-1);
  Words.set_constructor_degree 73 (-1);
  Words.set_constructor_degree 74 (-2);
  Words.set_constructor_degree 75 (-1);
  Words.set_constructor_degree 76 (-1);
  Words.set_constructor_degree 77 (-1);
  Words.set_constructor_degree 78 0;
  Words.set_constructor_degree 79 (-4);
  Words.set_constructor_degree 80 (-2);
  Words.set_constructor_degree 81 (-4);
  Words.set_constructor_degree 82 (-4);
  Words.set_constructor_degree 83 (-1);
  Words.set_constructor_degree 84 0;
  Words.set_constructor_degree 85 0;
  Words.set_constructor_degree 86 0;
  Words.set_constructor_degree 87 (-1);
  Words.set_constructor_degree 88 (-2);
  Words.set_constructor_degree 89 (-2);
  Words.set_constructor_degree 90 (-1);
  Words.set_constructor_degree 91 (-1);
  Words.set_constructor_degree 92 (-1);
  Words.set_constructor_degree 93 (-1);
  Words.set_constructor_degree 94 (-2);
  Words.set_constructor_degree 95 (-1);
  Words.set_constructor_degree 96 (-1);
  Words.set_constructor_degree 97 0;
  Words.set_constructor_degree 98 (-1);
  Words.set_constructor_degree 99 (-2);
  Words.set_constructor_degree 100 (-2);
  Words.set_constructor_degree 101 (-2);
  Words.set_constructor_degree 102 (-1);
  Words.set_constructor_degree 103 (-1);
  Words.set_constructor_degree 104 (-2);
  Words.set_constructor_degree 105 (-5);
  Words.set_constructor_degree 106 0;
  Words.set_constructor_degree 107 (-1);
  Words.set_constructor_degree 108 (-1);
  Words.set_constructor_degree 109 0;
  Words.set_constructor_degree 110 0;
  Words.set_constructor_degree 111 (-3);
  Words.set_constructor_degree 112 (-3);
  Words.set_constructor_degree 113 (-1);
  Words.set_constructor_degree 114 (-1);
  Words.set_constructor_degree 115 (-1);
  Words.set_constructor_degree 116 (-2);
  Words.set_constructor_degree 117 (-2);
  Words.set_constructor_degree 118 0;
  Words.set_constructor_degree 119 (-1);
  Words.set_constructor_degree 120 (-2);
  Words.set_constructor_degree 121 (-2);
  Words.set_constructor_degree 122 (-1);
  Words.set_constructor_degree 123 (-1);
  Words.set_constructor_degree 124 (-3);
  Words.set_constructor_degree 125 0;
  Words.set_constructor_degree 126 (-1);
  Words.set_constructor_degree 127 (-1);
  Words.set_constructor_degree 128 (-2);
  Words.set_constructor_degree 129 (-2);
  Words.set_constructor_degree 130 (-2);
  Words.set_constructor_degree 131 (-1);
  Words.set_constructor_degree 132 0;
  Words.set_constructor_degree 133 (-1);
  Words.set_constructor_degree 134 (-2);
  Words.set_constructor_degree 135 (-2);
  Words.set_constructor_degree 136 0;
  Words.set_constructor_degree 137 (-1);
  Words.set_constructor_degree 138 (-3);
  Words.set_constructor_degree 139 (-3);
  Words.set_constructor_degree 140 0;
  Words.set_constructor_degree 141 0;
  Words.set_constructor_degree 142 0;
  Words.set_constructor_degree 143 0
