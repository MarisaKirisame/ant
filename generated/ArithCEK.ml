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

let rec var_rank ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec expr_rank ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 2)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec compare_expr ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec expr_equal ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec expr_size ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 10)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec better_expr ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 11)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec scale ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 12)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec coeff_value ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 18)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec coeff_base ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 20)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec extract_factor ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 22)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec search_factor ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 23)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec append_exprs ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 24)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec insert_expr ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 25)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec sort_exprs ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 26)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec compare_add_term ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 27)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec insert_add_term ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 28)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec sort_add_terms ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 29)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec reverse_exprs_aux ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 30)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec reverse_exprs ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 31)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec flatten_add ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 32)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec flatten_mul ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 35)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec mul_coeff ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 36)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec mul_base ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 38)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec mul_total_coeff ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 40)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec mul_bases ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 41)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec build_mul ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 42)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec normalize_mul_flat ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 44)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec combine_like_terms_acc ?config (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 45)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done)

let rec combine_like_terms ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 48)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec factor_adjacent ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 49)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec pick_factored ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 51)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec search_terms ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 52)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec build_add ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 53)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec search_round ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 55)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec normalize_add_flat ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 56)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec search_opt ?config (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 57)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done)

let rec normalize ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 59)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec simplify_aux ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 60)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec diffx ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 61)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let rec eval ?config (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 63)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done)

let rec main ?config (x0 : Value.seq) : exec_result =
  exec_cek ?config (pc_to_exp (int_to_pc 65)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done)

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_65 ->
      assert_env_length w_65 1;
      let hd_0, tl_0 = resolve w_65 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_65
      | 15 (* tag_cont_1 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_40 = env_call w_65 [ 0; 1; 2 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_40; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 2)
      | 16 (* tag_cont_2 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_41 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_41; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 1)
      | 17 (* tag_cont_3 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_42 = env_call w_65 [ 2 ] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_42; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 18 (* tag_cont_4 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_43 = env_call w_65 [ 2 ] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_43; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 19 (* tag_cont_5 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 66)
      | 20 (* tag_cont_6 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 67)
      | 21 (* tag_cont_7 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_46 = env_call w_65 [ 0; 1; 2 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_46; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 10)
      | 22 (* tag_cont_8 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 68)
      | 23 (* tag_cont_9 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          assert_env_length w_65 5;
          w_65.state.c <- pc_to_exp (int_to_pc 69)
      | 24 (* tag_cont_10 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 25 (* tag_cont_11 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 71)
      | 26 (* tag_cont_12 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 0; 1 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 25)
      | 27 (* tag_cont_13 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_50 = env_call w_65 [ 0; 1; 2 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; keep_50; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 20)
      | 28 (* tag_cont_14 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 73)
      | 29 (* tag_cont_15 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 0; 1 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 28)
      | 30 (* tag_cont_16 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_52 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; keep_52; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 32)
      | 31 (* tag_cont_17 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_53 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_54; keep_53; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 35)
      | 32 (* tag_cont_18 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_54 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; keep_54; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 40)
      | 33 (* tag_cont_19 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          w_65.state.c <- pc_to_exp (int_to_pc 76)
      | 34 (* tag_cont_20 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 35 (* tag_cont_21 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_57 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_58; keep_57; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 35)
      | 36 (* tag_cont_22 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          push_env w_65 (Memo.from_constructor tag_ENil);
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 37 (* tag_cont_23 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          assert_env_length w_65 5;
          let keep_58 = env_call w_65 [ 0; 1; 3; 4 ] [ 2 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_59; keep_58; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 18)
      | 38 (* tag_cont_24 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_59 = env_call w_65 [ 1; 2 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_60; keep_59; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 18)
      | 39 (* tag_cont_25 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          Dynarray.set w_65.state.e 2
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 2 ]);
          assert_env_length w_65 5;
          let keep_60 = env_call w_65 [ 0; 1; 3; 4 ] [ 4; 2 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_61; keep_60; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 40 (* tag_cont_26 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          push_env w_65
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 5;
          let keep_61 = env_call w_65 [ 0; 1; 2; 3 ] [ 3; 4 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_62; keep_61; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 41 (* tag_cont_27 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          w_65.state.c <- pc_to_exp (int_to_pc 77)
      | 42 (* tag_cont_28 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 43 (* tag_cont_29 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_64 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_65; keep_64; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 52)
      | 44 (* tag_cont_30 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_65 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_66; keep_65; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 32)
      | 45 (* tag_cont_31 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_66 = env_call w_65 [ 0; 2 ] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_67; keep_66; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 57)
      | 46 (* tag_cont_32 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_67 = env_call w_65 [ 0; 2 ] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_68; keep_67; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 57)
      | 47 (* tag_cont_33 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_68 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_69; keep_68; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 59)
      | 48 (* tag_cont_34 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_69 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_70; keep_69; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 59)
      | 49 (* tag_cont_35 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_70 = env_call w_65 [ 1 ] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_71; keep_70; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 50 (* tag_cont_36 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_71 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_72; keep_71; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 61)
      | 51 (* tag_cont_37 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          Dynarray.set w_65.state.e 2
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_65.state.e 2; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 3;
          let keep_72 = env_call w_65 [ 0; 2 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_73; keep_72; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 61)
      | 52 (* tag_cont_38 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          assert_env_length w_65 4;
          let keep_73 = env_call w_65 [ 3 ] [ 2; 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_74; keep_73; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 63)
      | 53 (* tag_cont_39 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          assert_env_length w_65 4;
          let keep_74 = env_call w_65 [ 3 ] [ 2; 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_75; keep_74; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 63)
      | 54 (* tag_cont_40 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_75 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_76; keep_75; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 61)
      | 55 (* tag_cont_41 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 89)
      | 56 (* tag_cont_42 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 90)
      | 57 (* tag_cont_43 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 91)
      | 58 (* tag_cont_44 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 92)
      | 59 (* tag_cont_45 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 93)
      | 60 (* tag_cont_46 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 94)
      | 61 (* tag_cont_47 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 97)
      | 62 (* tag_cont_48 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          assert_env_length w_65 4;
          w_65.state.c <- pc_to_exp (int_to_pc 98)
      | 63 (* tag_cont_49 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          assert_env_length w_65 5;
          w_65.state.c <- pc_to_exp (int_to_pc 99)
      | 64 (* tag_cont_50 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 65 (* tag_cont_51 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          assert_env_length w_65 4;
          let keep_81 = env_call w_65 [ 0; 1 ] [ 2; 3 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_82; keep_81; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 3)
      | 66 (* tag_cont_52 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 67 (* tag_cont_53 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 0; 1 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 24)
      | 68 (* tag_cont_54 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 0; 1 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 24)
      | 69 (* tag_cont_55 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 100)
      | 70 (* tag_cont_56 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 0; 1 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 25)
      | 71 (* tag_cont_57 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 0; 1 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 25)
      | 72 (* tag_cont_58 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_82 = env_call w_65 [] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_83; keep_82; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 24)
      | 73 (* tag_cont_59 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          assert_env_length w_65 5;
          let keep_83 = env_call w_65 [ 0; 1; 2; 3; 4 ] [ 0; 3 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_84; keep_83; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 74 (* tag_cont_60 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          ignore (env_call w_65 [] [ 1; 2; 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 45)
      | 75 (* tag_cont_61 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 101)
      | 76 (* tag_cont_62 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 4 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 102)
      | 77 (* tag_cont_63 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 78 (* tag_cont_64 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          ignore (env_call w_65 [] [ 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 52)
      | 79 (* tag_cont_65 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_87 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_88; keep_87; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 49)
      | 80 (* tag_cont_66 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_88 = env_call w_65 [] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_89; keep_88; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 24)
      | 81 (* tag_cont_67 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_89 = env_call w_65 [ 0; 1; 2 ] [ 1; 2 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_90; keep_89; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 56)
      | 82 (* tag_cont_68 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_90 = env_call w_65 [ 0; 1; 2 ] [ 1; 2 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_91; keep_90; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 44)
      | 83 (* tag_cont_69 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          w_65.state.c <- pc_to_exp (int_to_pc 105)
      | 84 (* tag_cont_70 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          w_65.state.c <- pc_to_exp (int_to_pc 110)
      | 85 (* tag_cont_71 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 111)
      | 86 (* tag_cont_72 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 87 (* tag_cont_73 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 2 ]);
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_65.state.e 1; Dynarray.get w_65.state.e 0 ]);
          assert_env_length w_65 3;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 88 (* tag_cont_74 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 112)
      | 89 (* tag_cont_75 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 113)
      | 90 (* tag_cont_76 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          ignore (env_call w_65 [] [ 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 60)
      | 91 (* tag_cont_77 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_91 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_92; keep_91; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 1)
      | 92 (* tag_cont_78 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 115)
      | 93 (* tag_cont_79 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 117)
      | 94 (* tag_cont_80 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 119)
      | 95 (* tag_cont_81 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          w_65.state.c <- pc_to_exp (int_to_pc 120)
      | 96 (* tag_cont_82 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 122)
      | 97 (* tag_cont_83 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_93 = env_call w_65 [ 0 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_94; keep_93; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 40)
      | 98 (* tag_cont_84 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 5 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 123)
      | 99 (* tag_cont_85 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          Dynarray.set w_65.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_65.state.e 0; Dynarray.get w_65.state.e 1 ]);
          assert_env_length w_65 2;
          return_n w_65 0 (pc_to_exp (int_to_pc 0))
      | 100 (* tag_cont_86 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          ignore (env_call w_65 [] [ 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 49)
      | 101 (* tag_cont_87 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          w_65.state.c <- pc_to_exp (int_to_pc 124)
      | 102 (* tag_cont_88 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          ignore (env_call w_65 [] [ 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 52)
      | 103 (* tag_cont_89 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_95 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_96; keep_95; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 29)
      | 104 (* tag_cont_90 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          assert_env_length w_65 4;
          let keep_96 = env_call w_65 [ 0; 3 ] [ 2; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_97; keep_96; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 56)
      | 105 (* tag_cont_91 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          assert_env_length w_65 4;
          let keep_97 = env_call w_65 [ 0; 3 ] [ 2; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_98; keep_97; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 44)
      | 106 (* tag_cont_92 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 128)
      | 107 (* tag_cont_93 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_98 = env_call w_65 [ 0; 1; 2 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_99; keep_98; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 18)
      | 108 (* tag_cont_94 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 130)
      | 109 (* tag_cont_95 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          push_env w_65 (Memo.from_int 0);
          w_65.state.c <- pc_to_exp (int_to_pc 132)
      | 110 (* tag_cont_96 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_101 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_102; keep_101; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 48)
      | 111 (* tag_cont_97 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_102 = env_call w_65 [ 0; 1 ] [ 1; 2 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_103; keep_102; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 56)
      | 112 (* tag_cont_98 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_103 = env_call w_65 [ 0; 1 ] [ 1; 2 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_104; keep_103; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 44)
      | 113 (* tag_cont_99 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 136)
      | 114 (* tag_cont_100 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_104 = env_call w_65 [ 0 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_105; keep_104; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 42)
      | 115 (* tag_cont_101 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          ignore (env_call w_65 [] [ 1; 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 28)
      | 116 (* tag_cont_102 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_105 = env_call w_65 [ 0 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_106; keep_105; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 55)
      | 117 (* tag_cont_103 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_106 = env_call w_65 [ 0; 1 ] [ 2; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_107; keep_106; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 56)
      | 118 (* tag_cont_104 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_107 = env_call w_65 [ 0; 1 ] [ 2; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_108; keep_107; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 44)
      | 119 (* tag_cont_105 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          push_env w_65 (Memo.from_int 1);
          w_65.state.c <- pc_to_exp (int_to_pc 138)
      | 120 (* tag_cont_106 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_108 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_109; keep_108; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 31)
      | 121 (* tag_cont_107 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_109 = env_call w_65 [ 0; 1; 2 ] [ 2; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_110; keep_109; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 122 (* tag_cont_108 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 2 tl_0;
          assert_env_length w_65 3;
          let keep_110 = env_call w_65 [ 0; 1; 2 ] [ 2; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_111; keep_110; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 4)
      | 123 (* tag_cont_109 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_111 = env_call w_65 [ 0 ] [ 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_112; keep_111; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 55)
      | 124 (* tag_cont_110 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 139)
      | 125 (* tag_cont_111 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 3 tl_0;
          w_65.state.c <- pc_to_exp (int_to_pc 140)
      | 126 (* tag_cont_112 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_112 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_113; keep_112; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 53)
      | 127 (* tag_cont_113 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_113 = env_call w_65 [ 1 ] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_114; keep_113; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 53)
      | 128 (* tag_cont_114 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 1 tl_0;
          assert_env_length w_65 2;
          let keep_114 = env_call w_65 [] [ 0; 1 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_115; keep_114; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 11)
      | 129 (* tag_cont_115 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_115 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_116; keep_115; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 32)
      | 130 (* tag_cont_116 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_116 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_117; keep_116; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 29)
      | 131 (* tag_cont_117 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          let keep_117 = env_call w_65 [] [ 0 ] in
          w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_118; keep_117; w_65.state.k ];
          w_65.state.c <- pc_to_exp (int_to_pc 48)
      | 132 (* tag_cont_118 *) ->
          w_65.state.k <- get_next_cont tl_0;
          restore_env w_65 0 tl_0;
          assert_env_length w_65 1;
          ignore (env_call w_65 [] [ 0 ]);
          w_65.state.c <- pc_to_exp (int_to_pc 53)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      let x_0 = resolve w_0 (Source.E 0) in
      match Word.get_value (fst x_0) with
      | 3 (* tag_X *) ->
          Dynarray.set w_0.state.e 0 (Memo.from_int 0);
          assert_env_length w_0 1;
          return_n w_0 0 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          Dynarray.set w_0.state.e 0 (Memo.from_int 1);
          assert_env_length w_0 1;
          return_n w_0 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (2)")
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 1;
      let x_1 = resolve w_1 (Source.E 0) in
      match Word.get_value (fst x_1) with
      | 5 (* tag_Const *) ->
          let splits_0 = Memo.splits (snd x_1) in
          let split0_0 = List.nth splits_0 0 in
          Dynarray.set w_1.state.e 0 split0_0;
          Dynarray.set w_1.state.e 0 (Memo.from_int 0);
          assert_env_length w_1 1;
          return_n w_1 0 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          Dynarray.set w_1.state.e 0 split0_1;
          Dynarray.set w_1.state.e 0 (Memo.from_int 1);
          assert_env_length w_1 1;
          return_n w_1 0 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_2 = Memo.splits (snd x_1) in
          let split0_2 = List.nth splits_2 0 in
          let split1_0 = List.nth splits_2 1 in
          Dynarray.set w_1.state.e 0 split0_2;
          push_env w_1 split1_0;
          Dynarray.set w_1.state.e 0 (Memo.from_int 2);
          assert_env_length w_1 2;
          return_n w_1 0 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_3 = Memo.splits (snd x_1) in
          let split0_3 = List.nth splits_3 0 in
          let split1_1 = List.nth splits_3 1 in
          Dynarray.set w_1.state.e 0 split0_3;
          push_env w_1 split1_1;
          Dynarray.set w_1.state.e 0 (Memo.from_int 3);
          assert_env_length w_1 2;
          return_n w_1 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (3)")
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      let keep_0 = env_call w_2 [ 0; 1 ] [ 0 ] in
      w_2.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_2.state.k ];
      w_2.state.c <- pc_to_exp (int_to_pc 2))
    3;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      let x_2 = resolve w_3 (Source.E 0) in
      match Word.get_value (fst x_2) with
      | 5 (* tag_Const *) ->
          let splits_4 = Memo.splits (snd x_2) in
          let split0_4 = List.nth splits_4 0 in
          Dynarray.set w_3.state.e 0 split0_4;
          assert_env_length w_3 2;
          w_3.state.c <- pc_to_exp (int_to_pc 6)
      | 6 (* tag_Var *) ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          Dynarray.set w_3.state.e 0 split0_6;
          assert_env_length w_3 2;
          w_3.state.c <- pc_to_exp (int_to_pc 7)
      | 7 (* tag_Add *) ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          let split1_2 = List.nth splits_8 1 in
          Dynarray.set w_3.state.e 0 split0_8;
          push_env w_3 split1_2;
          assert_env_length w_3 3;
          w_3.state.c <- pc_to_exp (int_to_pc 8)
      | 8 (* tag_Mul *) ->
          let splits_10 = Memo.splits (snd x_2) in
          let split0_10 = List.nth splits_10 0 in
          let split1_4 = List.nth splits_10 1 in
          Dynarray.set w_3.state.e 0 split0_10;
          push_env w_3 split1_4;
          assert_env_length w_3 3;
          w_3.state.c <- pc_to_exp (int_to_pc 9)
      | _ -> failwith "unreachable (10)")
    4;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 2;
      let x0_0 = resolve w_5 (Source.E 0) in
      let x1_0 = resolve w_5 (Source.E 1) in
      Dynarray.set w_5.state.e 0
        (Memo.from_int (if Word.get_value (fst x0_0) = Word.get_value (fst x1_0) then 1 else 0));
      assert_env_length w_5 2;
      return_n w_5 0 (pc_to_exp (int_to_pc 0)))
    5;
  add_exp
    (fun w_4 ->
      let x_3 = resolve w_4 (Source.E 1) in
      match Word.get_value (fst x_3) with
      | 5 (* tag_Const *) ->
          let splits_5 = Memo.splits (snd x_3) in
          let split0_5 = List.nth splits_5 0 in
          Dynarray.set w_4.state.e 1 split0_5;
          w_4.state.c <- pc_to_exp (int_to_pc 5)
      | _ ->
          Dynarray.set w_4.state.e 0 (Memo.from_int 0);
          assert_env_length w_4 2;
          return_n w_4 0 (pc_to_exp (int_to_pc 0)))
    6;
  add_exp
    (fun w_6 ->
      let x_4 = resolve w_6 (Source.E 1) in
      match Word.get_value (fst x_4) with
      | 6 (* tag_Var *) ->
          let splits_7 = Memo.splits (snd x_4) in
          let split0_7 = List.nth splits_7 0 in
          Dynarray.set w_6.state.e 1 split0_7;
          assert_env_length w_6 2;
          let keep_1 = env_call w_6 [ 1 ] [ 0 ] in
          w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_6.state.k ];
          w_6.state.c <- pc_to_exp (int_to_pc 1)
      | _ ->
          Dynarray.set w_6.state.e 0 (Memo.from_int 0);
          assert_env_length w_6 2;
          return_n w_6 0 (pc_to_exp (int_to_pc 0)))
    7;
  add_exp
    (fun w_7 ->
      let x_5 = resolve w_7 (Source.E 1) in
      match Word.get_value (fst x_5) with
      | 7 (* tag_Add *) ->
          let splits_9 = Memo.splits (snd x_5) in
          let split0_9 = List.nth splits_9 0 in
          let split1_3 = List.nth splits_9 1 in
          Dynarray.set w_7.state.e 1 split0_9;
          push_env w_7 split1_3;
          assert_env_length w_7 4;
          let keep_2 = env_call w_7 [ 2; 3 ] [ 0; 1 ] in
          w_7.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_7.state.k ];
          w_7.state.c <- pc_to_exp (int_to_pc 4)
      | _ ->
          Dynarray.set w_7.state.e 0 (Memo.from_int 0);
          assert_env_length w_7 3;
          return_n w_7 0 (pc_to_exp (int_to_pc 0)))
    8;
  add_exp
    (fun w_8 ->
      let x_6 = resolve w_8 (Source.E 1) in
      match Word.get_value (fst x_6) with
      | 8 (* tag_Mul *) ->
          let splits_11 = Memo.splits (snd x_6) in
          let split0_11 = List.nth splits_11 0 in
          let split1_5 = List.nth splits_11 1 in
          Dynarray.set w_8.state.e 1 split0_11;
          push_env w_8 split1_5;
          assert_env_length w_8 4;
          let keep_3 = env_call w_8 [ 2; 3 ] [ 0; 1 ] in
          w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_8.state.k ];
          w_8.state.c <- pc_to_exp (int_to_pc 4)
      | _ ->
          Dynarray.set w_8.state.e 0 (Memo.from_int 0);
          assert_env_length w_8 3;
          return_n w_8 0 (pc_to_exp (int_to_pc 0)))
    9;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 1;
      let x_7 = resolve w_9 (Source.E 0) in
      match Word.get_value (fst x_7) with
      | 5 (* tag_Const *) ->
          let splits_12 = Memo.splits (snd x_7) in
          let split0_12 = List.nth splits_12 0 in
          Dynarray.set w_9.state.e 0 split0_12;
          Dynarray.set w_9.state.e 0 (Memo.from_int 1);
          assert_env_length w_9 1;
          return_n w_9 0 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_13 = Memo.splits (snd x_7) in
          let split0_13 = List.nth splits_13 0 in
          Dynarray.set w_9.state.e 0 split0_13;
          Dynarray.set w_9.state.e 0 (Memo.from_int 1);
          assert_env_length w_9 1;
          return_n w_9 0 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_14 = Memo.splits (snd x_7) in
          let split0_14 = List.nth splits_14 0 in
          let split1_6 = List.nth splits_14 1 in
          Dynarray.set w_9.state.e 0 split0_14;
          push_env w_9 split1_6;
          push_env w_9 (Memo.from_int 1);
          assert_env_length w_9 3;
          let keep_4 = env_call w_9 [ 1; 2 ] [ 0 ] in
          w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_9.state.k ];
          w_9.state.c <- pc_to_exp (int_to_pc 10)
      | 8 (* tag_Mul *) ->
          let splits_15 = Memo.splits (snd x_7) in
          let split0_15 = List.nth splits_15 0 in
          let split1_7 = List.nth splits_15 1 in
          Dynarray.set w_9.state.e 0 split0_15;
          push_env w_9 split1_7;
          push_env w_9 (Memo.from_int 1);
          assert_env_length w_9 3;
          let keep_5 = env_call w_9 [ 1; 2 ] [ 0 ] in
          w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_9.state.k ];
          w_9.state.c <- pc_to_exp (int_to_pc 10)
      | _ -> failwith "unreachable (11)")
    10;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 2;
      let keep_6 = env_call w_10 [ 0; 1 ] [ 0 ] in
      w_10.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_10.state.k ];
      w_10.state.c <- pc_to_exp (int_to_pc 10))
    11;
  add_exp
    (fun w_11 ->
      push_env w_11 (Memo.from_int 0);
      w_11.state.c <- pc_to_exp (int_to_pc 17))
    12;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 3;
      let x0_2 = resolve w_14 (Source.E 0) in
      let x1_2 = resolve w_14 (Source.E 1) in
      Dynarray.set w_14.state.e 0 (Memo.from_int (Word.get_value (fst x0_2) * Word.get_value (fst x1_2)));
      Dynarray.set w_14.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_14.state.e 0 ]);
      assert_env_length w_14 3;
      return_n w_14 0 (pc_to_exp (int_to_pc 0)))
    13;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 3;
      let cond_1 = resolve w_16 (Source.E 2) in
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_16 3;
        return_n w_16 1 (pc_to_exp (int_to_pc 0)))
      else (
        Dynarray.set w_16.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_16.state.e 0 ]);
        Dynarray.set w_16.state.e 0
          (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_16.state.e 0; Dynarray.get w_16.state.e 1 ]);
        assert_env_length w_16 3;
        return_n w_16 0 (pc_to_exp (int_to_pc 0))))
    14;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 3;
      let x0_3 = resolve w_15 (Source.E 0) in
      let x1_3 = resolve w_15 (Source.E 2) in
      Dynarray.set w_15.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_3) = Word.get_value (fst x1_3) then 1 else 0));
      w_15.state.c <- pc_to_exp (int_to_pc 14))
    15;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 3;
      let cond_0 = resolve w_13 (Source.E 2) in
      if Word.get_value (fst cond_0) <> 0 then (
        Dynarray.set w_13.state.e 0 (Memo.from_int 0);
        Dynarray.set w_13.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_13.state.e 0 ]);
        assert_env_length w_13 3;
        return_n w_13 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_13 3;
        let x_8 = resolve w_13 (Source.E 1) in
        match Word.get_value (fst x_8) with
        | 5 (* tag_Const *) ->
            let splits_16 = Memo.splits (snd x_8) in
            let split0_16 = List.nth splits_16 0 in
            Dynarray.set w_13.state.e 1 split0_16;
            w_13.state.c <- pc_to_exp (int_to_pc 13)
        | _ ->
            Dynarray.set w_13.state.e 2 (Memo.from_int 1);
            w_13.state.c <- pc_to_exp (int_to_pc 15)))
    16;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 3;
      let x0_1 = resolve w_12 (Source.E 0) in
      let x1_1 = resolve w_12 (Source.E 2) in
      Dynarray.set w_12.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_1) = Word.get_value (fst x1_1) then 1 else 0));
      w_12.state.c <- pc_to_exp (int_to_pc 16))
    17;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 1;
      let x_9 = resolve w_17 (Source.E 0) in
      match Word.get_value (fst x_9) with
      | 5 (* tag_Const *) ->
          let splits_17 = Memo.splits (snd x_9) in
          let split0_17 = List.nth splits_17 0 in
          Dynarray.set w_17.state.e 0 split0_17;
          assert_env_length w_17 1;
          return_n w_17 0 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_18 = Memo.splits (snd x_9) in
          let split0_18 = List.nth splits_18 0 in
          let split1_8 = List.nth splits_18 1 in
          Dynarray.set w_17.state.e 0 split0_18;
          push_env w_17 split1_8;
          assert_env_length w_17 2;
          w_17.state.c <- pc_to_exp (int_to_pc 19)
      | _ ->
          Dynarray.set w_17.state.e 0 (Memo.from_int 1);
          assert_env_length w_17 1;
          return_n w_17 0 (pc_to_exp (int_to_pc 0)))
    18;
  add_exp
    (fun w_18 ->
      let x_10 = resolve w_18 (Source.E 0) in
      match Word.get_value (fst x_10) with
      | 5 (* tag_Const *) ->
          let splits_19 = Memo.splits (snd x_10) in
          let split0_19 = List.nth splits_19 0 in
          Dynarray.set w_18.state.e 0 split0_19;
          assert_env_length w_18 2;
          return_n w_18 0 (pc_to_exp (int_to_pc 0))
      | _ ->
          Dynarray.set w_18.state.e 0 (Memo.from_int 1);
          assert_env_length w_18 2;
          return_n w_18 0 (pc_to_exp (int_to_pc 0)))
    19;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 1;
      let x_11 = resolve w_19 (Source.E 0) in
      match Word.get_value (fst x_11) with
      | 5 (* tag_Const *) ->
          let splits_20 = Memo.splits (snd x_11) in
          let split0_20 = List.nth splits_20 0 in
          Dynarray.set w_19.state.e 0 split0_20;
          Dynarray.set w_19.state.e 0 (Memo.from_int 1);
          Dynarray.set w_19.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_19.state.e 0 ]);
          assert_env_length w_19 1;
          return_n w_19 0 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_21 = Memo.splits (snd x_11) in
          let split0_21 = List.nth splits_21 0 in
          let split1_9 = List.nth splits_21 1 in
          push_env w_19 split0_21;
          push_env w_19 split1_9;
          assert_env_length w_19 3;
          w_19.state.c <- pc_to_exp (int_to_pc 21)
      | _ ->
          assert_env_length w_19 1;
          return_n w_19 0 (pc_to_exp (int_to_pc 0)))
    20;
  add_exp
    (fun w_20 ->
      let x_12 = resolve w_20 (Source.E 1) in
      match Word.get_value (fst x_12) with
      | 5 (* tag_Const *) ->
          let splits_22 = Memo.splits (snd x_12) in
          let split0_22 = List.nth splits_22 0 in
          Dynarray.set w_20.state.e 0 split0_22;
          assert_env_length w_20 3;
          return_n w_20 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_20 3;
          return_n w_20 0 (pc_to_exp (int_to_pc 0)))
    21;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 2;
      let keep_7 = env_call w_21 [ 0; 1 ] [ 0; 1 ] in
      w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_21.state.k ];
      w_21.state.c <- pc_to_exp (int_to_pc 4))
    22;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 2;
      let x_13 = resolve w_22 (Source.E 0) in
      match Word.get_value (fst x_13) with
      | 8 (* tag_Mul *) ->
          let splits_23 = Memo.splits (snd x_13) in
          let split0_23 = List.nth splits_23 0 in
          let split1_10 = List.nth splits_23 1 in
          push_env w_22 split0_23;
          push_env w_22 split1_10;
          assert_env_length w_22 4;
          let keep_8 = env_call w_22 [ 0; 1; 2; 3 ] [ 2; 1 ] in
          w_22.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_22.state.k ];
          w_22.state.c <- pc_to_exp (int_to_pc 22)
      | _ ->
          Dynarray.set w_22.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_22.state.e 0; Dynarray.get w_22.state.e 1 ]);
          assert_env_length w_22 2;
          return_n w_22 0 (pc_to_exp (int_to_pc 0)))
    23;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 2;
      let x_14 = resolve w_23 (Source.E 0) in
      match Word.get_value (fst x_14) with
      | 11 (* tag_ENil *) ->
          assert_env_length w_23 2;
          return_n w_23 1 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_24 = Memo.splits (snd x_14) in
          let split0_24 = List.nth splits_24 0 in
          let split1_11 = List.nth splits_24 1 in
          Dynarray.set w_23.state.e 0 split0_24;
          push_env w_23 split1_11;
          assert_env_length w_23 3;
          let keep_9 = env_call w_23 [ 0 ] [ 2; 1 ] in
          w_23.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_23.state.k ];
          w_23.state.c <- pc_to_exp (int_to_pc 24)
      | _ -> failwith "unreachable (25)")
    24;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 2;
      let x_15 = resolve w_24 (Source.E 1) in
      match Word.get_value (fst x_15) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_24.state.e 1 (Memo.from_constructor tag_ENil);
          Dynarray.set w_24.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_24.state.e 0; Dynarray.get w_24.state.e 1 ]);
          assert_env_length w_24 2;
          return_n w_24 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_25 = Memo.splits (snd x_15) in
          let split0_25 = List.nth splits_25 0 in
          let split1_12 = List.nth splits_25 1 in
          push_env w_24 split0_25;
          push_env w_24 split1_12;
          assert_env_length w_24 4;
          let keep_10 = env_call w_24 [ 0; 1; 2; 3 ] [ 0; 2 ] in
          w_24.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_24.state.k ];
          w_24.state.c <- pc_to_exp (int_to_pc 3)
      | _ -> failwith "unreachable (26)")
    25;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 1;
      let x_16 = resolve w_25 (Source.E 0) in
      match Word.get_value (fst x_16) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_25.state.e 0 (Memo.from_constructor tag_ENil);
          assert_env_length w_25 1;
          return_n w_25 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_26 = Memo.splits (snd x_16) in
          let split0_26 = List.nth splits_26 0 in
          let split1_13 = List.nth splits_26 1 in
          Dynarray.set w_25.state.e 0 split0_26;
          push_env w_25 split1_13;
          assert_env_length w_25 2;
          let keep_11 = env_call w_25 [ 0 ] [ 1 ] in
          w_25.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_25.state.k ];
          w_25.state.c <- pc_to_exp (int_to_pc 26)
      | _ -> failwith "unreachable (27)")
    26;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 2;
      let keep_12 = env_call w_26 [ 0; 1 ] [ 0 ] in
      w_26.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_26.state.k ];
      w_26.state.c <- pc_to_exp (int_to_pc 20))
    27;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 2;
      let x_17 = resolve w_27 (Source.E 1) in
      match Word.get_value (fst x_17) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_27.state.e 1 (Memo.from_constructor tag_ENil);
          Dynarray.set w_27.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_27.state.e 0; Dynarray.get w_27.state.e 1 ]);
          assert_env_length w_27 2;
          return_n w_27 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_27 = Memo.splits (snd x_17) in
          let split0_27 = List.nth splits_27 0 in
          let split1_14 = List.nth splits_27 1 in
          push_env w_27 split0_27;
          push_env w_27 split1_14;
          assert_env_length w_27 4;
          let keep_13 = env_call w_27 [ 0; 1; 2; 3 ] [ 0; 2 ] in
          w_27.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_27.state.k ];
          w_27.state.c <- pc_to_exp (int_to_pc 27)
      | _ -> failwith "unreachable (29)")
    28;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 1;
      let x_18 = resolve w_28 (Source.E 0) in
      match Word.get_value (fst x_18) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_28.state.e 0 (Memo.from_constructor tag_ENil);
          assert_env_length w_28 1;
          return_n w_28 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_28 = Memo.splits (snd x_18) in
          let split0_28 = List.nth splits_28 0 in
          let split1_15 = List.nth splits_28 1 in
          Dynarray.set w_28.state.e 0 split0_28;
          push_env w_28 split1_15;
          assert_env_length w_28 2;
          let keep_14 = env_call w_28 [ 0 ] [ 1 ] in
          w_28.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_28.state.k ];
          w_28.state.c <- pc_to_exp (int_to_pc 29)
      | _ -> failwith "unreachable (30)")
    29;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 2;
      let x_19 = resolve w_29 (Source.E 0) in
      match Word.get_value (fst x_19) with
      | 11 (* tag_ENil *) ->
          assert_env_length w_29 2;
          return_n w_29 1 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_29 = Memo.splits (snd x_19) in
          let split0_29 = List.nth splits_29 0 in
          let split1_16 = List.nth splits_29 1 in
          Dynarray.set w_29.state.e 0 split0_29;
          push_env w_29 split1_16;
          Dynarray.set w_29.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_29.state.e 0; Dynarray.get w_29.state.e 1 ]);
          assert_env_length w_29 3;
          ignore (env_call w_29 [] [ 2; 0 ]);
          w_29.state.c <- pc_to_exp (int_to_pc 30)
      | _ -> failwith "unreachable (31)")
    30;
  add_exp
    (fun w_30 ->
      push_env w_30 (Memo.from_constructor tag_ENil);
      assert_env_length w_30 2;
      ignore (env_call w_30 [] [ 0; 1 ]);
      w_30.state.c <- pc_to_exp (int_to_pc 30))
    31;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 1;
      let x_20 = resolve w_31 (Source.E 0) in
      match Word.get_value (fst x_20) with
      | 7 (* tag_Add *) ->
          let splits_30 = Memo.splits (snd x_20) in
          let split0_30 = List.nth splits_30 0 in
          let split1_17 = List.nth splits_30 1 in
          Dynarray.set w_31.state.e 0 split0_30;
          push_env w_31 split1_17;
          assert_env_length w_31 2;
          let keep_15 = env_call w_31 [ 1 ] [ 0 ] in
          w_31.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_31.state.k ];
          w_31.state.c <- pc_to_exp (int_to_pc 32)
      | 5 (* tag_Const *) ->
          let splits_31 = Memo.splits (snd x_20) in
          let split0_31 = List.nth splits_31 0 in
          push_env w_31 split0_31;
          push_env w_31 (Memo.from_int 0);
          w_31.state.c <- pc_to_exp (int_to_pc 34)
      | _ ->
          push_env w_31 (Memo.from_constructor tag_ENil);
          Dynarray.set w_31.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_31.state.e 0; Dynarray.get w_31.state.e 1 ]);
          assert_env_length w_31 2;
          return_n w_31 0 (pc_to_exp (int_to_pc 0)))
    32;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 3;
      let cond_2 = resolve w_33 (Source.E 1) in
      if Word.get_value (fst cond_2) <> 0 then (
        Dynarray.set w_33.state.e 0 (Memo.from_constructor tag_ENil);
        assert_env_length w_33 3;
        return_n w_33 0 (pc_to_exp (int_to_pc 0)))
      else (
        Dynarray.set w_33.state.e 1 (Memo.from_constructor tag_ENil);
        Dynarray.set w_33.state.e 0
          (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_33.state.e 0; Dynarray.get w_33.state.e 1 ]);
        assert_env_length w_33 3;
        return_n w_33 0 (pc_to_exp (int_to_pc 0))))
    33;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 3;
      let x0_4 = resolve w_32 (Source.E 1) in
      let x1_4 = resolve w_32 (Source.E 2) in
      Dynarray.set w_32.state.e 1
        (Memo.from_int (if Word.get_value (fst x0_4) = Word.get_value (fst x1_4) then 1 else 0));
      w_32.state.c <- pc_to_exp (int_to_pc 33))
    34;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 1;
      let x_21 = resolve w_34 (Source.E 0) in
      match Word.get_value (fst x_21) with
      | 8 (* tag_Mul *) ->
          let splits_32 = Memo.splits (snd x_21) in
          let split0_32 = List.nth splits_32 0 in
          let split1_18 = List.nth splits_32 1 in
          Dynarray.set w_34.state.e 0 split0_32;
          push_env w_34 split1_18;
          assert_env_length w_34 2;
          let keep_16 = env_call w_34 [ 1 ] [ 0 ] in
          w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_34.state.k ];
          w_34.state.c <- pc_to_exp (int_to_pc 35)
      | _ ->
          push_env w_34 (Memo.from_constructor tag_ENil);
          Dynarray.set w_34.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_34.state.e 0; Dynarray.get w_34.state.e 1 ]);
          assert_env_length w_34 2;
          return_n w_34 0 (pc_to_exp (int_to_pc 0)))
    35;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 1;
      let x_22 = resolve w_35 (Source.E 0) in
      match Word.get_value (fst x_22) with
      | 5 (* tag_Const *) ->
          let splits_33 = Memo.splits (snd x_22) in
          let split0_33 = List.nth splits_33 0 in
          Dynarray.set w_35.state.e 0 split0_33;
          assert_env_length w_35 1;
          return_n w_35 0 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_34 = Memo.splits (snd x_22) in
          let split0_34 = List.nth splits_34 0 in
          let split1_19 = List.nth splits_34 1 in
          Dynarray.set w_35.state.e 0 split0_34;
          push_env w_35 split1_19;
          assert_env_length w_35 2;
          w_35.state.c <- pc_to_exp (int_to_pc 37)
      | _ ->
          Dynarray.set w_35.state.e 0 (Memo.from_int 1);
          assert_env_length w_35 1;
          return_n w_35 0 (pc_to_exp (int_to_pc 0)))
    36;
  add_exp
    (fun w_36 ->
      let x_23 = resolve w_36 (Source.E 0) in
      match Word.get_value (fst x_23) with
      | 5 (* tag_Const *) ->
          let splits_35 = Memo.splits (snd x_23) in
          let split0_35 = List.nth splits_35 0 in
          Dynarray.set w_36.state.e 0 split0_35;
          assert_env_length w_36 2;
          return_n w_36 0 (pc_to_exp (int_to_pc 0))
      | _ ->
          Dynarray.set w_36.state.e 0 (Memo.from_int 1);
          assert_env_length w_36 2;
          return_n w_36 0 (pc_to_exp (int_to_pc 0)))
    37;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 1;
      let x_24 = resolve w_37 (Source.E 0) in
      match Word.get_value (fst x_24) with
      | 5 (* tag_Const *) ->
          let splits_36 = Memo.splits (snd x_24) in
          let split0_36 = List.nth splits_36 0 in
          Dynarray.set w_37.state.e 0 split0_36;
          Dynarray.set w_37.state.e 0 (Memo.from_int 1);
          Dynarray.set w_37.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_37.state.e 0 ]);
          assert_env_length w_37 1;
          return_n w_37 0 (pc_to_exp (int_to_pc 0))
      | 8 (* tag_Mul *) ->
          let splits_37 = Memo.splits (snd x_24) in
          let split0_37 = List.nth splits_37 0 in
          let split1_20 = List.nth splits_37 1 in
          push_env w_37 split0_37;
          push_env w_37 split1_20;
          assert_env_length w_37 3;
          w_37.state.c <- pc_to_exp (int_to_pc 39)
      | _ ->
          assert_env_length w_37 1;
          return_n w_37 0 (pc_to_exp (int_to_pc 0)))
    38;
  add_exp
    (fun w_38 ->
      let x_25 = resolve w_38 (Source.E 1) in
      match Word.get_value (fst x_25) with
      | 5 (* tag_Const *) ->
          let splits_38 = Memo.splits (snd x_25) in
          let split0_38 = List.nth splits_38 0 in
          Dynarray.set w_38.state.e 0 split0_38;
          assert_env_length w_38 3;
          return_n w_38 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_38 3;
          return_n w_38 0 (pc_to_exp (int_to_pc 0)))
    39;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 1;
      let x_26 = resolve w_39 (Source.E 0) in
      match Word.get_value (fst x_26) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_39.state.e 0 (Memo.from_int 1);
          assert_env_length w_39 1;
          return_n w_39 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_39 = Memo.splits (snd x_26) in
          let split0_39 = List.nth splits_39 0 in
          let split1_21 = List.nth splits_39 1 in
          Dynarray.set w_39.state.e 0 split0_39;
          push_env w_39 split1_21;
          assert_env_length w_39 2;
          let keep_17 = env_call w_39 [ 1 ] [ 0 ] in
          w_39.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_39.state.k ];
          w_39.state.c <- pc_to_exp (int_to_pc 36)
      | _ -> failwith "unreachable (41)")
    40;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 1;
      let x_27 = resolve w_40 (Source.E 0) in
      match Word.get_value (fst x_27) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_40.state.e 0 (Memo.from_constructor tag_ENil);
          assert_env_length w_40 1;
          return_n w_40 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_40 = Memo.splits (snd x_27) in
          let split0_40 = List.nth splits_40 0 in
          let split1_22 = List.nth splits_40 1 in
          Dynarray.set w_40.state.e 0 split0_40;
          push_env w_40 split1_22;
          assert_env_length w_40 2;
          let keep_18 = env_call w_40 [ 1 ] [ 0 ] in
          w_40.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_40.state.k ];
          w_40.state.c <- pc_to_exp (int_to_pc 38)
      | _ -> failwith "unreachable (42)")
    41;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 1;
      let x_28 = resolve w_41 (Source.E 0) in
      match Word.get_value (fst x_28) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_41.state.e 0 (Memo.from_int 1);
          Dynarray.set w_41.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_41.state.e 0 ]);
          assert_env_length w_41 1;
          return_n w_41 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_41 = Memo.splits (snd x_28) in
          let split0_41 = List.nth splits_41 0 in
          let split1_23 = List.nth splits_41 1 in
          Dynarray.set w_41.state.e 0 split0_41;
          push_env w_41 split1_23;
          assert_env_length w_41 2;
          w_41.state.c <- pc_to_exp (int_to_pc 43)
      | _ -> failwith "unreachable (44)")
    42;
  add_exp
    (fun w_42 ->
      let x_29 = resolve w_42 (Source.E 1) in
      match Word.get_value (fst x_29) with
      | 11 (* tag_ENil *) ->
          assert_env_length w_42 2;
          return_n w_42 0 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_42 2;
          let keep_19 = env_call w_42 [ 0 ] [ 1 ] in
          w_42.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_19; w_42.state.k ];
          w_42.state.c <- pc_to_exp (int_to_pc 42))
    43;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 2;
      let keep_20 = env_call w_43 [ 1 ] [ 0 ] in
      w_43.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_20; w_43.state.k ];
      w_43.state.c <- pc_to_exp (int_to_pc 35))
    44;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 3;
      let x_30 = resolve w_44 (Source.E 2) in
      match Word.get_value (fst x_30) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_44.state.e 2 (Memo.from_int 0);
          w_44.state.c <- pc_to_exp (int_to_pc 47)
      | 12 (* tag_ECons *) ->
          let splits_42 = Memo.splits (snd x_30) in
          let split0_42 = List.nth splits_42 0 in
          let split1_24 = List.nth splits_42 1 in
          Dynarray.set w_44.state.e 2 split0_42;
          push_env w_44 split1_24;
          assert_env_length w_44 4;
          let keep_22 = env_call w_44 [ 0; 1; 2; 3 ] [ 2 ] in
          w_44.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_22; w_44.state.k ];
          w_44.state.c <- pc_to_exp (int_to_pc 20)
      | _ -> failwith "unreachable (48)")
    45;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 3;
      let cond_3 = resolve w_46 (Source.E 2) in
      if Word.get_value (fst cond_3) <> 0 then (
        Dynarray.set w_46.state.e 0 (Memo.from_constructor tag_ENil);
        assert_env_length w_46 3;
        return_n w_46 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_46 3;
        let keep_21 = env_call w_46 [] [ 1; 0 ] in
        w_46.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_21; w_46.state.k ];
        w_46.state.c <- pc_to_exp (int_to_pc 12)))
    46;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 3;
      let x0_5 = resolve w_45 (Source.E 1) in
      let x1_5 = resolve w_45 (Source.E 2) in
      Dynarray.set w_45.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      w_45.state.c <- pc_to_exp (int_to_pc 46))
    47;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 1;
      let x_31 = resolve w_47 (Source.E 0) in
      match Word.get_value (fst x_31) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_47.state.e 0 (Memo.from_constructor tag_ENil);
          assert_env_length w_47 1;
          return_n w_47 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_43 = Memo.splits (snd x_31) in
          let split0_43 = List.nth splits_43 0 in
          let split1_25 = List.nth splits_43 1 in
          Dynarray.set w_47.state.e 0 split0_43;
          push_env w_47 split1_25;
          assert_env_length w_47 2;
          let keep_23 = env_call w_47 [ 0; 1 ] [ 0 ] in
          w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_23; w_47.state.k ];
          w_47.state.c <- pc_to_exp (int_to_pc 20)
      | _ -> failwith "unreachable (49)")
    48;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 1;
      let x_32 = resolve w_48 (Source.E 0) in
      match Word.get_value (fst x_32) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_48.state.e 0 (Memo.from_constructor tag_ENil);
          assert_env_length w_48 1;
          return_n w_48 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_44 = Memo.splits (snd x_32) in
          let split0_44 = List.nth splits_44 0 in
          let split1_26 = List.nth splits_44 1 in
          Dynarray.set w_48.state.e 0 split0_44;
          push_env w_48 split1_26;
          assert_env_length w_48 2;
          w_48.state.c <- pc_to_exp (int_to_pc 50)
      | _ -> failwith "unreachable (51)")
    49;
  add_exp
    (fun w_49 ->
      let x_33 = resolve w_49 (Source.E 1) in
      match Word.get_value (fst x_33) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_49.state.e 1 (Memo.from_constructor tag_ENil);
          Dynarray.set w_49.state.e 0
            (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_49.state.e 0; Dynarray.get w_49.state.e 1 ]);
          assert_env_length w_49 2;
          return_n w_49 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_45 = Memo.splits (snd x_33) in
          let split0_45 = List.nth splits_45 0 in
          let split1_27 = List.nth splits_45 1 in
          push_env w_49 split0_45;
          push_env w_49 split1_27;
          assert_env_length w_49 4;
          let keep_24 = env_call w_49 [ 0; 1; 2; 3 ] [ 0; 2 ] in
          w_49.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_24; w_49.state.k ];
          w_49.state.c <- pc_to_exp (int_to_pc 23)
      | _ -> failwith "unreachable (50)")
    50;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 2;
      let x_34 = resolve w_50 (Source.E 1) in
      match Word.get_value (fst x_34) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_50.state.e 0 (Memo.from_constructor tag_NoPick);
          assert_env_length w_50 2;
          return_n w_50 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_46 = Memo.splits (snd x_34) in
          let split0_46 = List.nth splits_46 0 in
          let split1_28 = List.nth splits_46 1 in
          Dynarray.set w_50.state.e 1 split0_46;
          push_env w_50 split1_28;
          assert_env_length w_50 3;
          let keep_25 = env_call w_50 [ 0; 1; 2 ] [ 0; 1 ] in
          w_50.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_25; w_50.state.k ];
          w_50.state.c <- pc_to_exp (int_to_pc 23)
      | _ -> failwith "unreachable (52)")
    51;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 1;
      let x_35 = resolve w_51 (Source.E 0) in
      match Word.get_value (fst x_35) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_51.state.e 0 (Memo.from_constructor tag_ENil);
          assert_env_length w_51 1;
          return_n w_51 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_47 = Memo.splits (snd x_35) in
          let split0_47 = List.nth splits_47 0 in
          let split1_29 = List.nth splits_47 1 in
          Dynarray.set w_51.state.e 0 split0_47;
          push_env w_51 split1_29;
          assert_env_length w_51 2;
          let keep_26 = env_call w_51 [ 0; 1 ] [ 0; 1 ] in
          w_51.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_26; w_51.state.k ];
          w_51.state.c <- pc_to_exp (int_to_pc 51)
      | _ -> failwith "unreachable (53)")
    52;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 1;
      let x_36 = resolve w_52 (Source.E 0) in
      match Word.get_value (fst x_36) with
      | 11 (* tag_ENil *) ->
          Dynarray.set w_52.state.e 0 (Memo.from_int 0);
          Dynarray.set w_52.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_52.state.e 0 ]);
          assert_env_length w_52 1;
          return_n w_52 0 (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let splits_48 = Memo.splits (snd x_36) in
          let split0_48 = List.nth splits_48 0 in
          let split1_30 = List.nth splits_48 1 in
          Dynarray.set w_52.state.e 0 split0_48;
          push_env w_52 split1_30;
          assert_env_length w_52 2;
          w_52.state.c <- pc_to_exp (int_to_pc 54)
      | _ -> failwith "unreachable (55)")
    53;
  add_exp
    (fun w_53 ->
      let x_37 = resolve w_53 (Source.E 1) in
      match Word.get_value (fst x_37) with
      | 11 (* tag_ENil *) ->
          assert_env_length w_53 2;
          return_n w_53 0 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_53 2;
          let keep_27 = env_call w_53 [ 0 ] [ 1 ] in
          w_53.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_27; w_53.state.k ];
          w_53.state.c <- pc_to_exp (int_to_pc 53))
    54;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 1;
      let keep_28 = env_call w_54 [] [ 0 ] in
      w_54.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_28; w_54.state.k ];
      w_54.state.c <- pc_to_exp (int_to_pc 49))
    55;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 2;
      let keep_29 = env_call w_55 [ 1 ] [ 0 ] in
      w_55.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_29; w_55.state.k ];
      w_55.state.c <- pc_to_exp (int_to_pc 32))
    56;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 2;
      let x_38 = resolve w_56 (Source.E 0) in
      match Word.get_value (fst x_38) with
      | 1 (* tag_Z *) ->
          assert_env_length w_56 2;
          return_n w_56 1 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let splits_49 = Memo.splits (snd x_38) in
          let split0_49 = List.nth splits_49 0 in
          Dynarray.set w_56.state.e 0 split0_49;
          assert_env_length w_56 2;
          w_56.state.c <- pc_to_exp (int_to_pc 58)
      | _ -> failwith "unreachable (59)")
    57;
  add_exp
    (fun w_57 ->
      let x_39 = resolve w_57 (Source.E 1) in
      match Word.get_value (fst x_39) with
      | 7 (* tag_Add *) ->
          let splits_50 = Memo.splits (snd x_39) in
          let split0_50 = List.nth splits_50 0 in
          let split1_31 = List.nth splits_50 1 in
          Dynarray.set w_57.state.e 1 split0_50;
          push_env w_57 split1_31;
          assert_env_length w_57 3;
          let keep_30 = env_call w_57 [ 0; 2 ] [ 0; 1 ] in
          w_57.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_30; w_57.state.k ];
          w_57.state.c <- pc_to_exp (int_to_pc 57)
      | 8 (* tag_Mul *) ->
          let splits_51 = Memo.splits (snd x_39) in
          let split0_51 = List.nth splits_51 0 in
          let split1_32 = List.nth splits_51 1 in
          Dynarray.set w_57.state.e 1 split0_51;
          push_env w_57 split1_32;
          assert_env_length w_57 3;
          let keep_31 = env_call w_57 [ 0; 2 ] [ 0; 1 ] in
          w_57.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_31; w_57.state.k ];
          w_57.state.c <- pc_to_exp (int_to_pc 57)
      | _ ->
          assert_env_length w_57 2;
          return_n w_57 1 (pc_to_exp (int_to_pc 0)))
    58;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 1;
      let x_40 = resolve w_58 (Source.E 0) in
      match Word.get_value (fst x_40) with
      | 5 (* tag_Const *) ->
          let splits_52 = Memo.splits (snd x_40) in
          let split0_52 = List.nth splits_52 0 in
          push_env w_58 split0_52;
          assert_env_length w_58 2;
          return_n w_58 0 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_53 = Memo.splits (snd x_40) in
          let split0_53 = List.nth splits_53 0 in
          push_env w_58 split0_53;
          assert_env_length w_58 2;
          return_n w_58 0 (pc_to_exp (int_to_pc 0))
      | 7 (* tag_Add *) ->
          let splits_54 = Memo.splits (snd x_40) in
          let split0_54 = List.nth splits_54 0 in
          let split1_33 = List.nth splits_54 1 in
          Dynarray.set w_58.state.e 0 split0_54;
          push_env w_58 split1_33;
          assert_env_length w_58 2;
          let keep_32 = env_call w_58 [ 1 ] [ 0 ] in
          w_58.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_32; w_58.state.k ];
          w_58.state.c <- pc_to_exp (int_to_pc 59)
      | 8 (* tag_Mul *) ->
          let splits_55 = Memo.splits (snd x_40) in
          let split0_55 = List.nth splits_55 0 in
          let split1_34 = List.nth splits_55 1 in
          Dynarray.set w_58.state.e 0 split0_55;
          push_env w_58 split1_34;
          assert_env_length w_58 2;
          let keep_33 = env_call w_58 [ 1 ] [ 0 ] in
          w_58.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_33; w_58.state.k ];
          w_58.state.c <- pc_to_exp (int_to_pc 59)
      | _ -> failwith "unreachable (60)")
    59;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 1;
      let keep_34 = env_call w_59 [ 0 ] [ 0 ] in
      w_59.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_34; w_59.state.k ];
      w_59.state.c <- pc_to_exp (int_to_pc 59))
    60;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 1;
      let x_41 = resolve w_60 (Source.E 0) in
      match Word.get_value (fst x_41) with
      | 5 (* tag_Const *) ->
          let splits_56 = Memo.splits (snd x_41) in
          let split0_56 = List.nth splits_56 0 in
          Dynarray.set w_60.state.e 0 split0_56;
          Dynarray.set w_60.state.e 0 (Memo.from_int 0);
          Dynarray.set w_60.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_60.state.e 0 ]);
          assert_env_length w_60 1;
          return_n w_60 0 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_57 = Memo.splits (snd x_41) in
          let split0_57 = List.nth splits_57 0 in
          Dynarray.set w_60.state.e 0 split0_57;
          assert_env_length w_60 1;
          w_60.state.c <- pc_to_exp (int_to_pc 62)
      | 7 (* tag_Add *) ->
          let splits_58 = Memo.splits (snd x_41) in
          let split0_58 = List.nth splits_58 0 in
          let split1_35 = List.nth splits_58 1 in
          Dynarray.set w_60.state.e 0 split0_58;
          push_env w_60 split1_35;
          assert_env_length w_60 2;
          let keep_35 = env_call w_60 [ 1 ] [ 0 ] in
          w_60.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_35; w_60.state.k ];
          w_60.state.c <- pc_to_exp (int_to_pc 61)
      | 8 (* tag_Mul *) ->
          let splits_59 = Memo.splits (snd x_41) in
          let split0_59 = List.nth splits_59 0 in
          let split1_36 = List.nth splits_59 1 in
          Dynarray.set w_60.state.e 0 split0_59;
          push_env w_60 split1_36;
          assert_env_length w_60 2;
          let keep_36 = env_call w_60 [ 0; 1 ] [ 0 ] in
          w_60.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_36; w_60.state.k ];
          w_60.state.c <- pc_to_exp (int_to_pc 61)
      | _ -> failwith "unreachable (63)")
    61;
  add_exp
    (fun w_61 ->
      let x_42 = resolve w_61 (Source.E 0) in
      match Word.get_value (fst x_42) with
      | 3 (* tag_X *) ->
          Dynarray.set w_61.state.e 0 (Memo.from_int 1);
          Dynarray.set w_61.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_61.state.e 0 ]);
          assert_env_length w_61 1;
          return_n w_61 0 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          Dynarray.set w_61.state.e 0 (Memo.from_int 0);
          Dynarray.set w_61.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_61.state.e 0 ]);
          assert_env_length w_61 1;
          return_n w_61 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (62)")
    62;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 3;
      let x_43 = resolve w_62 (Source.E 0) in
      match Word.get_value (fst x_43) with
      | 5 (* tag_Const *) ->
          let splits_60 = Memo.splits (snd x_43) in
          let split0_60 = List.nth splits_60 0 in
          Dynarray.set w_62.state.e 0 split0_60;
          assert_env_length w_62 3;
          return_n w_62 0 (pc_to_exp (int_to_pc 0))
      | 6 (* tag_Var *) ->
          let splits_61 = Memo.splits (snd x_43) in
          let split0_61 = List.nth splits_61 0 in
          Dynarray.set w_62.state.e 0 split0_61;
          assert_env_length w_62 3;
          w_62.state.c <- pc_to_exp (int_to_pc 64)
      | 7 (* tag_Add *) ->
          let splits_62 = Memo.splits (snd x_43) in
          let split0_62 = List.nth splits_62 0 in
          let split1_37 = List.nth splits_62 1 in
          Dynarray.set w_62.state.e 0 split0_62;
          push_env w_62 split1_37;
          assert_env_length w_62 4;
          let keep_37 = env_call w_62 [ 1; 2; 3 ] [ 0; 1; 2 ] in
          w_62.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_37; w_62.state.k ];
          w_62.state.c <- pc_to_exp (int_to_pc 63)
      | 8 (* tag_Mul *) ->
          let splits_63 = Memo.splits (snd x_43) in
          let split0_63 = List.nth splits_63 0 in
          let split1_38 = List.nth splits_63 1 in
          Dynarray.set w_62.state.e 0 split0_63;
          push_env w_62 split1_38;
          assert_env_length w_62 4;
          let keep_38 = env_call w_62 [ 1; 2; 3 ] [ 0; 1; 2 ] in
          w_62.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_38; w_62.state.k ];
          w_62.state.c <- pc_to_exp (int_to_pc 63)
      | _ -> failwith "unreachable (65)")
    63;
  add_exp
    (fun w_63 ->
      let x_44 = resolve w_63 (Source.E 0) in
      match Word.get_value (fst x_44) with
      | 3 (* tag_X *) ->
          assert_env_length w_63 3;
          return_n w_63 1 (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          assert_env_length w_63 3;
          return_n w_63 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (64)")
    64;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 1;
      let keep_39 = env_call w_64 [] [ 0 ] in
      w_64.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_39; w_64.state.k ];
      w_64.state.c <- pc_to_exp (int_to_pc 61))
    65;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 3;
      let x0_6 = resolve w_66 (Source.E 1) in
      let x1_6 = resolve w_66 (Source.E 2) in
      Dynarray.set w_66.state.e 1 (Memo.from_int (Word.get_value (fst x0_6) + Word.get_value (fst x1_6)));
      assert_env_length w_66 3;
      let keep_44 = env_call w_66 [ 1 ] [ 0 ] in
      w_66.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_44; w_66.state.k ];
      w_66.state.c <- pc_to_exp (int_to_pc 10))
    66;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 3;
      let x0_7 = resolve w_67 (Source.E 1) in
      let x1_7 = resolve w_67 (Source.E 2) in
      Dynarray.set w_67.state.e 1 (Memo.from_int (Word.get_value (fst x0_7) + Word.get_value (fst x1_7)));
      assert_env_length w_67 3;
      let keep_45 = env_call w_67 [ 1 ] [ 0 ] in
      w_67.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_45; w_67.state.k ];
      w_67.state.c <- pc_to_exp (int_to_pc 10))
    67;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 3;
      let cond_4 = resolve w_68 (Source.E 2) in
      if Word.get_value (fst cond_4) <> 0 then (
        Dynarray.set w_68.state.e 0 (Memo.from_int 1);
        Dynarray.set w_68.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_68.state.e 0 ]);
        Dynarray.set w_68.state.e 0 (Memo.appends [ Memo.from_constructor tag_Found; Dynarray.get w_68.state.e 0 ]);
        assert_env_length w_68 3;
        return_n w_68 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_68 3;
        let x_45 = resolve w_68 (Source.E 1) in
        match Word.get_value (fst x_45) with
        | 8 (* tag_Mul *) ->
            let splits_64 = Memo.splits (snd x_45) in
            let split0_64 = List.nth splits_64 0 in
            let split1_39 = List.nth splits_64 1 in
            Dynarray.set w_68.state.e 1 split0_64;
            Dynarray.set w_68.state.e 2 split1_39;
            assert_env_length w_68 3;
            let keep_47 = env_call w_68 [ 0; 1; 2 ] [ 0; 1 ] in
            w_68.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_47; w_68.state.k ];
            w_68.state.c <- pc_to_exp (int_to_pc 22)
        | _ ->
            Dynarray.set w_68.state.e 0 (Memo.from_constructor tag_Missing);
            assert_env_length w_68 3;
            return_n w_68 0 (pc_to_exp (int_to_pc 0))))
    68;
  add_exp
    (fun w_69 ->
      let x_46 = resolve w_69 (Source.E 4) in
      match Word.get_value (fst x_46) with
      | 10 (* tag_Found *) ->
          let splits_65 = Memo.splits (snd x_46) in
          let split0_65 = List.nth splits_65 0 in
          Dynarray.set w_69.state.e 0 split0_65;
          Dynarray.set w_69.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_69.state.e 3; Dynarray.get w_69.state.e 0 ]);
          Dynarray.set w_69.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_69.state.e 2; Dynarray.get w_69.state.e 0 ]);
          assert_env_length w_69 5;
          return_n w_69 0 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          assert_env_length w_69 5;
          let keep_48 = env_call w_69 [ 0; 1; 2; 3 ] [ 3; 1 ] in
          w_69.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; keep_48; w_69.state.k ];
          w_69.state.c <- pc_to_exp (int_to_pc 22)
      | _ -> failwith "unreachable (69)")
    69;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 6;
      let cond_5 = resolve w_71 (Source.E 4) in
      if Word.get_value (fst cond_5) <> 0 then (
        Dynarray.set w_71.state.e 0
          (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_71.state.e 0; Dynarray.get w_71.state.e 1 ]);
        assert_env_length w_71 6;
        return_n w_71 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_71 6;
        let keep_49 = env_call w_71 [ 2 ] [ 0; 3 ] in
        w_71.state.k <- Memo.appends [ Memo.from_constructor tag_cont_50; keep_49; w_71.state.k ];
        w_71.state.c <- pc_to_exp (int_to_pc 25)))
    70;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 6;
      let x0_8 = resolve w_70 (Source.E 4) in
      let x1_8 = resolve w_70 (Source.E 5) in
      Dynarray.set w_70.state.e 4
        (Memo.from_int (if Word.get_value (fst x0_8) <= Word.get_value (fst x1_8) then 1 else 0));
      w_70.state.c <- pc_to_exp (int_to_pc 70))
    71;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 6;
      let cond_6 = resolve w_73 (Source.E 4) in
      if Word.get_value (fst cond_6) <> 0 then (
        Dynarray.set w_73.state.e 0
          (Memo.appends [ Memo.from_constructor tag_ECons; Dynarray.get w_73.state.e 0; Dynarray.get w_73.state.e 1 ]);
        assert_env_length w_73 6;
        return_n w_73 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_73 6;
        let keep_51 = env_call w_73 [ 2 ] [ 0; 3 ] in
        w_73.state.k <- Memo.appends [ Memo.from_constructor tag_cont_52; keep_51; w_73.state.k ];
        w_73.state.c <- pc_to_exp (int_to_pc 28)))
    72;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 6;
      let x0_9 = resolve w_72 (Source.E 4) in
      let x1_9 = resolve w_72 (Source.E 5) in
      Dynarray.set w_72.state.e 4
        (Memo.from_int (if Word.get_value (fst x0_9) <= Word.get_value (fst x1_9) then 1 else 0));
      w_72.state.c <- pc_to_exp (int_to_pc 72))
    73;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 4;
      let cond_7 = resolve w_76 (Source.E 2) in
      if Word.get_value (fst cond_7) <> 0 then (
        assert_env_length w_76 4;
        ignore (env_call w_76 [] [ 0 ]);
        w_76.state.c <- pc_to_exp (int_to_pc 41))
      else (
        assert_env_length w_76 4;
        let keep_55 = env_call w_76 [ 1 ] [ 0 ] in
        w_76.state.k <- Memo.appends [ Memo.from_constructor tag_cont_56; keep_55; w_76.state.k ];
        w_76.state.c <- pc_to_exp (int_to_pc 41)))
    74;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 4;
      let x0_10 = resolve w_75 (Source.E 2) in
      let x1_10 = resolve w_75 (Source.E 3) in
      Dynarray.set w_75.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_10) = Word.get_value (fst x1_10) then 1 else 0));
      w_75.state.c <- pc_to_exp (int_to_pc 74))
    75;
  add_exp
    (fun w_74 ->
      let x_47 = resolve w_74 (Source.E 1) in
      match Word.get_value (fst x_47) with
      | 5 (* tag_Const *) ->
          let splits_66 = Memo.splits (snd x_47) in
          let split0_66 = List.nth splits_66 0 in
          push_env w_74 split0_66;
          push_env w_74 (Memo.from_int 1);
          w_74.state.c <- pc_to_exp (int_to_pc 75)
      | _ ->
          assert_env_length w_74 2;
          let keep_56 = env_call w_74 [ 1 ] [ 0 ] in
          w_74.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; keep_56; w_74.state.k ];
          w_74.state.c <- pc_to_exp (int_to_pc 41))
    76;
  add_exp
    (fun w_77 ->
      let x_48 = resolve w_77 (Source.E 2) in
      match Word.get_value (fst x_48) with
      | 13 (* tag_NoPick *) ->
          assert_env_length w_77 3;
          let keep_62 = env_call w_77 [ 0 ] [ 1 ] in
          w_77.state.k <- Memo.appends [ Memo.from_constructor tag_cont_63; keep_62; w_77.state.k ];
          w_77.state.c <- pc_to_exp (int_to_pc 52)
      | 14 (* tag_Pick *) ->
          let splits_67 = Memo.splits (snd x_48) in
          let split0_67 = List.nth splits_67 0 in
          let split1_40 = List.nth splits_67 1 in
          Dynarray.set w_77.state.e 0 split0_67;
          Dynarray.set w_77.state.e 1 split1_40;
          assert_env_length w_77 3;
          let keep_63 = env_call w_77 [] [ 0; 1 ] in
          w_77.state.k <- Memo.appends [ Memo.from_constructor tag_cont_64; keep_63; w_77.state.k ];
          w_77.state.c <- pc_to_exp (int_to_pc 28)
      | _ -> failwith "unreachable (77)")
    77;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 5;
      let x0_12 = resolve w_80 (Source.E 0) in
      let x1_12 = resolve w_80 (Source.E 1) in
      Dynarray.set w_80.state.e 0 (Memo.from_int (Word.get_value (fst x0_12) - Word.get_value (fst x1_12)));
      assert_env_length w_80 5;
      return_n w_80 0 (pc_to_exp (int_to_pc 0)))
    78;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 5;
      let x0_15 = resolve w_85 (Source.E 0) in
      let x1_15 = resolve w_85 (Source.E 1) in
      Dynarray.set w_85.state.e 0 (Memo.from_int (Word.get_value (fst x0_15) - Word.get_value (fst x1_15)));
      assert_env_length w_85 5;
      return_n w_85 0 (pc_to_exp (int_to_pc 0)))
    79;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 5;
      let cond_11 = resolve w_86 (Source.E 0) in
      if Word.get_value (fst cond_11) <> 0 then (
        Dynarray.set w_86.state.e 0 (Memo.from_int 1);
        assert_env_length w_86 5;
        return_n w_86 0 (pc_to_exp (int_to_pc 0)))
      else (
        Dynarray.set w_86.state.e 0 (Memo.from_int 0);
        assert_env_length w_86 5;
        return_n w_86 0 (pc_to_exp (int_to_pc 0))))
    80;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 5;
      let cond_10 = resolve w_84 (Source.E 2) in
      if Word.get_value (fst cond_10) <> 0 then (
        Dynarray.set w_84.state.e 0 (Memo.from_int 0);
        Dynarray.set w_84.state.e 1 (Memo.from_int 1);
        w_84.state.c <- pc_to_exp (int_to_pc 79))
      else (
        assert_env_length w_84 5;
        let x0_16 = resolve w_84 (Source.E 0) in
        let x1_16 = resolve w_84 (Source.E 1) in
        Dynarray.set w_84.state.e 0
          (Memo.from_int (if Word.get_value (fst x0_16) > Word.get_value (fst x1_16) then 1 else 0));
        w_84.state.c <- pc_to_exp (int_to_pc 80)))
    81;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 5;
      let x0_14 = resolve w_83 (Source.E 0) in
      let x1_14 = resolve w_83 (Source.E 1) in
      Dynarray.set w_83.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_14) < Word.get_value (fst x1_14) then 1 else 0));
      w_83.state.c <- pc_to_exp (int_to_pc 81))
    82;
  add_exp
    (fun w_82 ->
      let x_50 = resolve w_82 (Source.E 1) in
      match Word.get_value (fst x_50) with
      | 5 (* tag_Const *) ->
          let splits_69 = Memo.splits (snd x_50) in
          let split0_69 = List.nth splits_69 0 in
          Dynarray.set w_82.state.e 1 split0_69;
          w_82.state.c <- pc_to_exp (int_to_pc 82)
      | _ ->
          Dynarray.set w_82.state.e 0 (Memo.from_int 0);
          assert_env_length w_82 5;
          return_n w_82 0 (pc_to_exp (int_to_pc 0)))
    83;
  add_exp
    (fun w_87 ->
      let x_51 = resolve w_87 (Source.E 1) in
      match Word.get_value (fst x_51) with
      | 6 (* tag_Var *) ->
          let splits_71 = Memo.splits (snd x_51) in
          let split0_71 = List.nth splits_71 0 in
          Dynarray.set w_87.state.e 1 split0_71;
          assert_env_length w_87 5;
          let keep_76 = env_call w_87 [ 1 ] [ 0 ] in
          w_87.state.k <- Memo.appends [ Memo.from_constructor tag_cont_77; keep_76; w_87.state.k ];
          w_87.state.c <- pc_to_exp (int_to_pc 1)
      | _ ->
          Dynarray.set w_87.state.e 0 (Memo.from_int 0);
          assert_env_length w_87 5;
          return_n w_87 0 (pc_to_exp (int_to_pc 0)))
    84;
  add_exp
    (fun w_88 ->
      let x_52 = resolve w_88 (Source.E 1) in
      match Word.get_value (fst x_52) with
      | 7 (* tag_Add *) ->
          let splits_73 = Memo.splits (snd x_52) in
          let split0_73 = List.nth splits_73 0 in
          let split1_42 = List.nth splits_73 1 in
          Dynarray.set w_88.state.e 1 split0_73;
          Dynarray.set w_88.state.e 3 split1_42;
          assert_env_length w_88 5;
          let keep_77 = env_call w_88 [ 2; 3 ] [ 0; 1 ] in
          w_88.state.k <- Memo.appends [ Memo.from_constructor tag_cont_78; keep_77; w_88.state.k ];
          w_88.state.c <- pc_to_exp (int_to_pc 3)
      | _ ->
          Dynarray.set w_88.state.e 0 (Memo.from_int 0);
          assert_env_length w_88 5;
          return_n w_88 0 (pc_to_exp (int_to_pc 0)))
    85;
  add_exp
    (fun w_89 ->
      let x_53 = resolve w_89 (Source.E 1) in
      match Word.get_value (fst x_53) with
      | 8 (* tag_Mul *) ->
          let splits_75 = Memo.splits (snd x_53) in
          let split0_75 = List.nth splits_75 0 in
          let split1_44 = List.nth splits_75 1 in
          Dynarray.set w_89.state.e 1 split0_75;
          Dynarray.set w_89.state.e 3 split1_44;
          assert_env_length w_89 5;
          let keep_78 = env_call w_89 [ 2; 3 ] [ 0; 1 ] in
          w_89.state.k <- Memo.appends [ Memo.from_constructor tag_cont_79; keep_78; w_89.state.k ];
          w_89.state.c <- pc_to_exp (int_to_pc 3)
      | _ ->
          Dynarray.set w_89.state.e 0 (Memo.from_int 0);
          assert_env_length w_89 5;
          return_n w_89 0 (pc_to_exp (int_to_pc 0)))
    86;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 5;
      let cond_9 = resolve w_81 (Source.E 2) in
      if Word.get_value (fst cond_9) <> 0 then (
        Dynarray.set w_81.state.e 0 (Memo.from_int 1);
        assert_env_length w_81 5;
        return_n w_81 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_81 5;
        let x_49 = resolve w_81 (Source.E 0) in
        match Word.get_value (fst x_49) with
        | 5 (* tag_Const *) ->
            let splits_68 = Memo.splits (snd x_49) in
            let split0_68 = List.nth splits_68 0 in
            Dynarray.set w_81.state.e 0 split0_68;
            assert_env_length w_81 5;
            w_81.state.c <- pc_to_exp (int_to_pc 83)
        | 6 (* tag_Var *) ->
            let splits_70 = Memo.splits (snd x_49) in
            let split0_70 = List.nth splits_70 0 in
            Dynarray.set w_81.state.e 0 split0_70;
            assert_env_length w_81 5;
            w_81.state.c <- pc_to_exp (int_to_pc 84)
        | 7 (* tag_Add *) ->
            let splits_72 = Memo.splits (snd x_49) in
            let split0_72 = List.nth splits_72 0 in
            let split1_41 = List.nth splits_72 1 in
            Dynarray.set w_81.state.e 0 split0_72;
            Dynarray.set w_81.state.e 2 split1_41;
            assert_env_length w_81 5;
            w_81.state.c <- pc_to_exp (int_to_pc 85)
        | 8 (* tag_Mul *) ->
            let splits_74 = Memo.splits (snd x_49) in
            let split0_74 = List.nth splits_74 0 in
            let split1_43 = List.nth splits_74 1 in
            Dynarray.set w_81.state.e 0 split0_74;
            Dynarray.set w_81.state.e 2 split1_43;
            assert_env_length w_81 5;
            w_81.state.c <- pc_to_exp (int_to_pc 86)
        | _ -> failwith "unreachable (87)"))
    87;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 5;
      let cond_8 = resolve w_79 (Source.E 4) in
      if Word.get_value (fst cond_8) <> 0 then (
        Dynarray.set w_79.state.e 0 (Memo.from_int 0);
        Dynarray.set w_79.state.e 1 (Memo.from_int 1);
        w_79.state.c <- pc_to_exp (int_to_pc 78))
      else (
        assert_env_length w_79 5;
        let x0_13 = resolve w_79 (Source.E 2) in
        let x1_13 = resolve w_79 (Source.E 3) in
        Dynarray.set w_79.state.e 2
          (Memo.from_int (if Word.get_value (fst x0_13) > Word.get_value (fst x1_13) then 1 else 0));
        w_79.state.c <- pc_to_exp (int_to_pc 87)))
    88;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 4;
      let x0_11 = resolve w_78 (Source.E 2) in
      let x1_11 = resolve w_78 (Source.E 3) in
      push_env w_78 (Memo.from_int (if Word.get_value (fst x0_11) < Word.get_value (fst x1_11) then 1 else 0));
      w_78.state.c <- pc_to_exp (int_to_pc 88))
    89;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 2;
      let x0_17 = resolve w_90 (Source.E 0) in
      let x1_17 = resolve w_90 (Source.E 1) in
      Dynarray.set w_90.state.e 0
        (Memo.from_int (if Word.get_value (fst x0_17) = Word.get_value (fst x1_17) then 1 else 0));
      assert_env_length w_90 2;
      return_n w_90 0 (pc_to_exp (int_to_pc 0)))
    90;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 2;
      let x0_18 = resolve w_91 (Source.E 0) in
      let x1_18 = resolve w_91 (Source.E 1) in
      Dynarray.set w_91.state.e 0
        (Memo.from_int (if Word.get_value (fst x0_18) <> 0 && Word.get_value (fst x1_18) <> 0 then 1 else 0));
      assert_env_length w_91 2;
      return_n w_91 0 (pc_to_exp (int_to_pc 0)))
    91;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 2;
      let x0_19 = resolve w_92 (Source.E 0) in
      let x1_19 = resolve w_92 (Source.E 1) in
      Dynarray.set w_92.state.e 0
        (Memo.from_int (if Word.get_value (fst x0_19) <> 0 && Word.get_value (fst x1_19) <> 0 then 1 else 0));
      assert_env_length w_92 2;
      return_n w_92 0 (pc_to_exp (int_to_pc 0)))
    92;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 2;
      let x0_20 = resolve w_93 (Source.E 0) in
      let x1_20 = resolve w_93 (Source.E 1) in
      Dynarray.set w_93.state.e 0 (Memo.from_int (Word.get_value (fst x0_20) + Word.get_value (fst x1_20)));
      assert_env_length w_93 2;
      return_n w_93 0 (pc_to_exp (int_to_pc 0)))
    93;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 2;
      let x0_21 = resolve w_94 (Source.E 0) in
      let x1_21 = resolve w_94 (Source.E 1) in
      Dynarray.set w_94.state.e 0 (Memo.from_int (Word.get_value (fst x0_21) + Word.get_value (fst x1_21)));
      assert_env_length w_94 2;
      return_n w_94 0 (pc_to_exp (int_to_pc 0)))
    94;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 5;
      let cond_13 = resolve w_97 (Source.E 2) in
      if Word.get_value (fst cond_13) <> 0 then (
        assert_env_length w_97 5;
        return_n w_97 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_97 5;
        let keep_79 = env_call w_97 [ 0; 1 ] [ 0; 1 ] in
        w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_80; keep_79; w_97.state.k ];
        w_97.state.c <- pc_to_exp (int_to_pc 3)))
    95;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 5;
      let cond_12 = resolve w_96 (Source.E 4) in
      if Word.get_value (fst cond_12) <> 0 then (
        assert_env_length w_96 5;
        return_n w_96 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_96 5;
        let x0_23 = resolve w_96 (Source.E 3) in
        let x1_23 = resolve w_96 (Source.E 2) in
        Dynarray.set w_96.state.e 2
          (Memo.from_int (if Word.get_value (fst x0_23) < Word.get_value (fst x1_23) then 1 else 0));
        w_96.state.c <- pc_to_exp (int_to_pc 95)))
    96;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 4;
      let x0_22 = resolve w_95 (Source.E 2) in
      let x1_22 = resolve w_95 (Source.E 3) in
      push_env w_95 (Memo.from_int (if Word.get_value (fst x0_22) < Word.get_value (fst x1_22) then 1 else 0));
      w_95.state.c <- pc_to_exp (int_to_pc 96))
    97;
  add_exp
    (fun w_98 ->
      let x_54 = resolve w_98 (Source.E 3) in
      match Word.get_value (fst x_54) with
      | 10 (* tag_Found *) ->
          let splits_76 = Memo.splits (snd x_54) in
          let split0_76 = List.nth splits_76 0 in
          Dynarray.set w_98.state.e 0 split0_76;
          Dynarray.set w_98.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_98.state.e 0; Dynarray.get w_98.state.e 2 ]);
          Dynarray.set w_98.state.e 0 (Memo.appends [ Memo.from_constructor tag_Found; Dynarray.get w_98.state.e 0 ]);
          assert_env_length w_98 4;
          return_n w_98 0 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          assert_env_length w_98 4;
          let keep_80 = env_call w_98 [ 1 ] [ 0; 2 ] in
          w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_81; keep_80; w_98.state.k ];
          w_98.state.c <- pc_to_exp (int_to_pc 22)
      | _ -> failwith "unreachable (98)")
    98;
  add_exp
    (fun w_99 ->
      let x_55 = resolve w_99 (Source.E 4) in
      match Word.get_value (fst x_55) with
      | 10 (* tag_Found *) ->
          let splits_77 = Memo.splits (snd x_55) in
          let split0_77 = List.nth splits_77 0 in
          Dynarray.set w_99.state.e 0 split0_77;
          Dynarray.set w_99.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_99.state.e 2; Dynarray.get w_99.state.e 0 ]);
          Dynarray.set w_99.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_99.state.e 3; Dynarray.get w_99.state.e 0 ]);
          assert_env_length w_99 5;
          return_n w_99 0 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          Dynarray.set w_99.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Add; Dynarray.get w_99.state.e 0; Dynarray.get w_99.state.e 1 ]);
          assert_env_length w_99 5;
          return_n w_99 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (99)")
    99;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 2;
      let x0_24 = resolve w_100 (Source.E 0) in
      let x1_24 = resolve w_100 (Source.E 1) in
      Dynarray.set w_100.state.e 0 (Memo.from_int (Word.get_value (fst x0_24) * Word.get_value (fst x1_24)));
      assert_env_length w_100 2;
      return_n w_100 0 (pc_to_exp (int_to_pc 0)))
    100;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 5;
      let cond_14 = resolve w_101 (Source.E 4) in
      if Word.get_value (fst cond_14) <> 0 then (
        assert_env_length w_101 5;
        let keep_84 = env_call w_101 [ 0 ] [ 1 ] in
        w_101.state.k <- Memo.appends [ Memo.from_constructor tag_cont_85; keep_84; w_101.state.k ];
        w_101.state.c <- pc_to_exp (int_to_pc 49))
      else (
        assert_env_length w_101 5;
        let keep_85 = env_call w_101 [] [ 3; 2 ] in
        w_101.state.k <- Memo.appends [ Memo.from_constructor tag_cont_86; keep_85; w_101.state.k ];
        w_101.state.c <- pc_to_exp (int_to_pc 28)))
    101;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 5;
      let cond_15 = resolve w_102 (Source.E 4) in
      if Word.get_value (fst cond_15) <> 0 then (
        assert_env_length w_102 5;
        let keep_86 = env_call w_102 [ 1 ] [ 0; 2 ] in
        w_102.state.k <- Memo.appends [ Memo.from_constructor tag_cont_87; keep_86; w_102.state.k ];
        w_102.state.c <- pc_to_exp (int_to_pc 51))
      else (
        Dynarray.set w_102.state.e 0
          (Memo.appends [ Memo.from_constructor tag_Pick; Dynarray.get w_102.state.e 3; Dynarray.get w_102.state.e 2 ]);
        assert_env_length w_102 5;
        return_n w_102 0 (pc_to_exp (int_to_pc 0))))
    102;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 4;
      let cond_16 = resolve w_105 (Source.E 2) in
      if Word.get_value (fst cond_16) <> 0 then (
        assert_env_length w_105 4;
        return_n w_105 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_105 4;
        ignore (env_call w_105 [] [ 0; 1 ]);
        w_105.state.c <- pc_to_exp (int_to_pc 56)))
    103;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 4;
      let x0_25 = resolve w_104 (Source.E 2) in
      let x1_25 = resolve w_104 (Source.E 3) in
      Dynarray.set w_104.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_25) = Word.get_value (fst x1_25) then 1 else 0));
      w_104.state.c <- pc_to_exp (int_to_pc 103))
    104;
  add_exp
    (fun w_103 ->
      let x_56 = resolve w_103 (Source.E 0) in
      match Word.get_value (fst x_56) with
      | 5 (* tag_Const *) ->
          let splits_78 = Memo.splits (snd x_56) in
          let split0_78 = List.nth splits_78 0 in
          push_env w_103 split0_78;
          push_env w_103 (Memo.from_int 0);
          w_103.state.c <- pc_to_exp (int_to_pc 104)
      | _ ->
          assert_env_length w_103 2;
          ignore (env_call w_103 [] [ 0; 1 ]);
          w_103.state.c <- pc_to_exp (int_to_pc 56))
    105;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 4;
      let cond_18 = resolve w_110 (Source.E 2) in
      if Word.get_value (fst cond_18) <> 0 then (
        assert_env_length w_110 4;
        return_n w_110 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_110 4;
        ignore (env_call w_110 [] [ 0; 1 ]);
        w_110.state.c <- pc_to_exp (int_to_pc 44)))
    106;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 4;
      let x0_27 = resolve w_109 (Source.E 2) in
      let x1_27 = resolve w_109 (Source.E 3) in
      Dynarray.set w_109.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_27) = Word.get_value (fst x1_27) then 1 else 0));
      w_109.state.c <- pc_to_exp (int_to_pc 106))
    107;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 4;
      let cond_17 = resolve w_108 (Source.E 3) in
      if Word.get_value (fst cond_17) <> 0 then (
        assert_env_length w_108 4;
        return_n w_108 0 (pc_to_exp (int_to_pc 0)))
      else (
        Dynarray.set w_108.state.e 3 (Memo.from_int 1);
        w_108.state.c <- pc_to_exp (int_to_pc 107)))
    108;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 4;
      let x0_26 = resolve w_107 (Source.E 2) in
      let x1_26 = resolve w_107 (Source.E 3) in
      Dynarray.set w_107.state.e 3
        (Memo.from_int (if Word.get_value (fst x0_26) = Word.get_value (fst x1_26) then 1 else 0));
      w_107.state.c <- pc_to_exp (int_to_pc 108))
    109;
  add_exp
    (fun w_106 ->
      let x_57 = resolve w_106 (Source.E 0) in
      match Word.get_value (fst x_57) with
      | 5 (* tag_Const *) ->
          let splits_79 = Memo.splits (snd x_57) in
          let split0_79 = List.nth splits_79 0 in
          push_env w_106 split0_79;
          push_env w_106 (Memo.from_int 0);
          w_106.state.c <- pc_to_exp (int_to_pc 109)
      | _ ->
          assert_env_length w_106 2;
          ignore (env_call w_106 [] [ 0; 1 ]);
          w_106.state.c <- pc_to_exp (int_to_pc 44))
    110;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 2;
      let cond_19 = resolve w_111 (Source.E 1) in
      if Word.get_value (fst cond_19) <> 0 then (
        assert_env_length w_111 2;
        return_n w_111 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_111 2;
        ignore (env_call w_111 [] [ 0 ]);
        w_111.state.c <- pc_to_exp (int_to_pc 60)))
    111;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 2;
      let x0_28 = resolve w_112 (Source.E 0) in
      let x1_28 = resolve w_112 (Source.E 1) in
      Dynarray.set w_112.state.e 0 (Memo.from_int (Word.get_value (fst x0_28) + Word.get_value (fst x1_28)));
      assert_env_length w_112 2;
      return_n w_112 0 (pc_to_exp (int_to_pc 0)))
    112;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 2;
      let x0_29 = resolve w_113 (Source.E 0) in
      let x1_29 = resolve w_113 (Source.E 1) in
      Dynarray.set w_113.state.e 0 (Memo.from_int (Word.get_value (fst x0_29) * Word.get_value (fst x1_29)));
      assert_env_length w_113 2;
      return_n w_113 0 (pc_to_exp (int_to_pc 0)))
    113;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 4;
      let cond_20 = resolve w_115 (Source.E 3) in
      if Word.get_value (fst cond_20) <> 0 then (
        assert_env_length w_115 4;
        ignore (env_call w_115 [] [ 0; 1 ]);
        w_115.state.c <- pc_to_exp (int_to_pc 3))
      else (
        assert_env_length w_115 4;
        return_n w_115 2 (pc_to_exp (int_to_pc 0))))
    114;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 4;
      let x0_30 = resolve w_114 (Source.E 2) in
      let x1_30 = resolve w_114 (Source.E 3) in
      Dynarray.set w_114.state.e 3
        (Memo.from_int (if Word.get_value (fst x0_30) = Word.get_value (fst x1_30) then 1 else 0));
      w_114.state.c <- pc_to_exp (int_to_pc 114))
    115;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 4;
      let cond_21 = resolve w_117 (Source.E 3) in
      if Word.get_value (fst cond_21) <> 0 then (
        assert_env_length w_117 4;
        ignore (env_call w_117 [] [ 0; 1 ]);
        w_117.state.c <- pc_to_exp (int_to_pc 3))
      else (
        assert_env_length w_117 4;
        return_n w_117 2 (pc_to_exp (int_to_pc 0))))
    116;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 4;
      let x0_31 = resolve w_116 (Source.E 2) in
      let x1_31 = resolve w_116 (Source.E 3) in
      Dynarray.set w_116.state.e 3
        (Memo.from_int (if Word.get_value (fst x0_31) = Word.get_value (fst x1_31) then 1 else 0));
      w_116.state.c <- pc_to_exp (int_to_pc 116))
    117;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 4;
      let cond_22 = resolve w_119 (Source.E 2) in
      if Word.get_value (fst cond_22) <> 0 then (
        assert_env_length w_119 4;
        return_n w_119 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_119 4;
        return_n w_119 1 (pc_to_exp (int_to_pc 0))))
    118;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 4;
      let x0_32 = resolve w_118 (Source.E 2) in
      let x1_32 = resolve w_118 (Source.E 3) in
      Dynarray.set w_118.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_32) <= Word.get_value (fst x1_32) then 1 else 0));
      w_118.state.c <- pc_to_exp (int_to_pc 118))
    119;
  add_exp
    (fun w_120 ->
      let x_58 = resolve w_120 (Source.E 1) in
      match Word.get_value (fst x_58) with
      | 10 (* tag_Found *) ->
          let splits_80 = Memo.splits (snd x_58) in
          let split0_80 = List.nth splits_80 0 in
          Dynarray.set w_120.state.e 1 split0_80;
          Dynarray.set w_120.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Mul; Dynarray.get w_120.state.e 0; Dynarray.get w_120.state.e 1 ]);
          Dynarray.set w_120.state.e 0 (Memo.appends [ Memo.from_constructor tag_Found; Dynarray.get w_120.state.e 0 ]);
          assert_env_length w_120 2;
          return_n w_120 0 (pc_to_exp (int_to_pc 0))
      | 9 (* tag_Missing *) ->
          Dynarray.set w_120.state.e 0 (Memo.from_constructor tag_Missing);
          assert_env_length w_120 2;
          return_n w_120 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (120)")
    120;
  add_exp
    (fun w_122 ->
      assert_env_length w_122 4;
      let cond_23 = resolve w_122 (Source.E 3) in
      if Word.get_value (fst cond_23) <> 0 then (
        assert_env_length w_122 4;
        let keep_92 = env_call w_122 [ 0; 1 ] [ 0 ] in
        w_122.state.k <- Memo.appends [ Memo.from_constructor tag_cont_93; keep_92; w_122.state.k ];
        w_122.state.c <- pc_to_exp (int_to_pc 18))
      else (
        assert_env_length w_122 4;
        return_n w_122 2 (pc_to_exp (int_to_pc 0))))
    121;
  add_exp
    (fun w_121 ->
      assert_env_length w_121 4;
      let x0_33 = resolve w_121 (Source.E 2) in
      let x1_33 = resolve w_121 (Source.E 3) in
      Dynarray.set w_121.state.e 3
        (Memo.from_int (if Word.get_value (fst x0_33) = Word.get_value (fst x1_33) then 1 else 0));
      w_121.state.c <- pc_to_exp (int_to_pc 121))
    122;
  add_exp
    (fun w_123 ->
      assert_env_length w_123 6;
      let cond_24 = resolve w_123 (Source.E 5) in
      if Word.get_value (fst cond_24) <> 0 then (
        assert_env_length w_123 6;
        let x0_34 = resolve w_123 (Source.E 1) in
        let x1_34 = resolve w_123 (Source.E 4) in
        Dynarray.set w_123.state.e 1 (Memo.from_int (Word.get_value (fst x0_34) + Word.get_value (fst x1_34)));
        assert_env_length w_123 6;
        ignore (env_call w_123 [] [ 0; 1; 2 ]);
        w_123.state.c <- pc_to_exp (int_to_pc 45))
      else (
        assert_env_length w_123 6;
        let keep_94 = env_call w_123 [ 0; 1 ] [ 3; 4; 2 ] in
        w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_95; keep_94; w_123.state.k ];
        w_123.state.c <- pc_to_exp (int_to_pc 45)))
    123;
  add_exp
    (fun w_124 ->
      let x_59 = resolve w_124 (Source.E 1) in
      match Word.get_value (fst x_59) with
      | 13 (* tag_NoPick *) ->
          Dynarray.set w_124.state.e 0 (Memo.from_constructor tag_NoPick);
          assert_env_length w_124 2;
          return_n w_124 0 (pc_to_exp (int_to_pc 0))
      | 14 (* tag_Pick *) ->
          let splits_81 = Memo.splits (snd x_59) in
          let split0_81 = List.nth splits_81 0 in
          let split1_45 = List.nth splits_81 1 in
          Dynarray.set w_124.state.e 1 split0_81;
          push_env w_124 split1_45;
          Dynarray.set w_124.state.e 0
            (Memo.appends
               [ Memo.from_constructor tag_ECons; Dynarray.get w_124.state.e 0; Dynarray.get w_124.state.e 2 ]);
          Dynarray.set w_124.state.e 0
            (Memo.appends
               [ Memo.from_constructor tag_Pick; Dynarray.get w_124.state.e 1; Dynarray.get w_124.state.e 0 ]);
          assert_env_length w_124 3;
          return_n w_124 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (124)")
    124;
  add_exp
    (fun w_127 ->
      assert_env_length w_127 3;
      let x0_36 = resolve w_127 (Source.E 0) in
      let x1_36 = resolve w_127 (Source.E 1) in
      Dynarray.set w_127.state.e 0 (Memo.from_int (Word.get_value (fst x0_36) - Word.get_value (fst x1_36)));
      assert_env_length w_127 3;
      return_n w_127 0 (pc_to_exp (int_to_pc 0)))
    125;
  add_exp
    (fun w_128 ->
      assert_env_length w_128 3;
      let cond_26 = resolve w_128 (Source.E 0) in
      if Word.get_value (fst cond_26) <> 0 then (
        Dynarray.set w_128.state.e 0 (Memo.from_int 1);
        assert_env_length w_128 3;
        return_n w_128 0 (pc_to_exp (int_to_pc 0)))
      else (
        Dynarray.set w_128.state.e 0 (Memo.from_int 0);
        assert_env_length w_128 3;
        return_n w_128 0 (pc_to_exp (int_to_pc 0))))
    126;
  add_exp
    (fun w_126 ->
      assert_env_length w_126 3;
      let cond_25 = resolve w_126 (Source.E 2) in
      if Word.get_value (fst cond_25) <> 0 then (
        Dynarray.set w_126.state.e 0 (Memo.from_int 0);
        Dynarray.set w_126.state.e 1 (Memo.from_int 1);
        w_126.state.c <- pc_to_exp (int_to_pc 125))
      else (
        assert_env_length w_126 3;
        let x0_37 = resolve w_126 (Source.E 0) in
        let x1_37 = resolve w_126 (Source.E 1) in
        Dynarray.set w_126.state.e 0
          (Memo.from_int (if Word.get_value (fst x0_37) > Word.get_value (fst x1_37) then 1 else 0));
        w_126.state.c <- pc_to_exp (int_to_pc 126)))
    127;
  add_exp
    (fun w_125 ->
      assert_env_length w_125 2;
      let x0_35 = resolve w_125 (Source.E 0) in
      let x1_35 = resolve w_125 (Source.E 1) in
      push_env w_125 (Memo.from_int (if Word.get_value (fst x0_35) < Word.get_value (fst x1_35) then 1 else 0));
      w_125.state.c <- pc_to_exp (int_to_pc 127))
    128;
  add_exp
    (fun w_130 ->
      assert_env_length w_130 3;
      let cond_27 = resolve w_130 (Source.E 2) in
      if Word.get_value (fst cond_27) <> 0 then (
        Dynarray.set w_130.state.e 0 (Memo.from_int 0);
        Dynarray.set w_130.state.e 0 (Memo.appends [ Memo.from_constructor tag_Const; Dynarray.get w_130.state.e 0 ]);
        assert_env_length w_130 3;
        return_n w_130 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_130 3;
        let keep_99 = env_call w_130 [ 1 ] [ 0 ] in
        w_130.state.k <- Memo.appends [ Memo.from_constructor tag_cont_100; keep_99; w_130.state.k ];
        w_130.state.c <- pc_to_exp (int_to_pc 41)))
    129;
  add_exp
    (fun w_129 ->
      assert_env_length w_129 3;
      let x0_38 = resolve w_129 (Source.E 1) in
      let x1_38 = resolve w_129 (Source.E 2) in
      Dynarray.set w_129.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_38) = Word.get_value (fst x1_38) then 1 else 0));
      w_129.state.c <- pc_to_exp (int_to_pc 129))
    130;
  add_exp
    (fun w_132 ->
      assert_env_length w_132 4;
      let cond_28 = resolve w_132 (Source.E 3) in
      if Word.get_value (fst cond_28) <> 0 then (
        assert_env_length w_132 4;
        return_n w_132 2 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_132 4;
        let keep_100 = env_call w_132 [ 2 ] [ 1; 0 ] in
        w_132.state.k <- Memo.appends [ Memo.from_constructor tag_cont_101; keep_100; w_132.state.k ];
        w_132.state.c <- pc_to_exp (int_to_pc 12)))
    131;
  add_exp
    (fun w_131 ->
      assert_env_length w_131 4;
      let x0_39 = resolve w_131 (Source.E 1) in
      let x1_39 = resolve w_131 (Source.E 3) in
      Dynarray.set w_131.state.e 3
        (Memo.from_int (if Word.get_value (fst x0_39) = Word.get_value (fst x1_39) then 1 else 0));
      w_131.state.c <- pc_to_exp (int_to_pc 131))
    132;
  add_exp
    (fun w_135 ->
      assert_env_length w_135 5;
      let x0_41 = resolve w_135 (Source.E 0) in
      let x1_41 = resolve w_135 (Source.E 1) in
      Dynarray.set w_135.state.e 0 (Memo.from_int (Word.get_value (fst x0_41) - Word.get_value (fst x1_41)));
      assert_env_length w_135 5;
      return_n w_135 0 (pc_to_exp (int_to_pc 0)))
    133;
  add_exp
    (fun w_136 ->
      assert_env_length w_136 5;
      let cond_30 = resolve w_136 (Source.E 2) in
      if Word.get_value (fst cond_30) <> 0 then (
        Dynarray.set w_136.state.e 0 (Memo.from_int 1);
        assert_env_length w_136 5;
        return_n w_136 0 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_136 5;
        ignore (env_call w_136 [] [ 0; 1 ]);
        w_136.state.c <- pc_to_exp (int_to_pc 3)))
    134;
  add_exp
    (fun w_134 ->
      assert_env_length w_134 5;
      let cond_29 = resolve w_134 (Source.E 4) in
      if Word.get_value (fst cond_29) <> 0 then (
        Dynarray.set w_134.state.e 0 (Memo.from_int 0);
        Dynarray.set w_134.state.e 1 (Memo.from_int 1);
        w_134.state.c <- pc_to_exp (int_to_pc 133))
      else (
        assert_env_length w_134 5;
        let x0_42 = resolve w_134 (Source.E 2) in
        let x1_42 = resolve w_134 (Source.E 3) in
        Dynarray.set w_134.state.e 2
          (Memo.from_int (if Word.get_value (fst x0_42) > Word.get_value (fst x1_42) then 1 else 0));
        w_134.state.c <- pc_to_exp (int_to_pc 134)))
    135;
  add_exp
    (fun w_133 ->
      assert_env_length w_133 4;
      let x0_40 = resolve w_133 (Source.E 2) in
      let x1_40 = resolve w_133 (Source.E 3) in
      push_env w_133 (Memo.from_int (if Word.get_value (fst x0_40) < Word.get_value (fst x1_40) then 1 else 0));
      w_133.state.c <- pc_to_exp (int_to_pc 135))
    136;
  add_exp
    (fun w_138 ->
      assert_env_length w_138 3;
      let cond_31 = resolve w_138 (Source.E 2) in
      if Word.get_value (fst cond_31) <> 0 then (
        assert_env_length w_138 3;
        return_n w_138 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_138 3;
        ignore (env_call w_138 [] [ 0; 1 ]);
        w_138.state.c <- pc_to_exp (int_to_pc 12)))
    137;
  add_exp
    (fun w_137 ->
      assert_env_length w_137 3;
      let x0_43 = resolve w_137 (Source.E 0) in
      let x1_43 = resolve w_137 (Source.E 2) in
      Dynarray.set w_137.state.e 2
        (Memo.from_int (if Word.get_value (fst x0_43) = Word.get_value (fst x1_43) then 1 else 0));
      w_137.state.c <- pc_to_exp (int_to_pc 137))
    138;
  add_exp
    (fun w_139 ->
      assert_env_length w_139 4;
      let cond_32 = resolve w_139 (Source.E 3) in
      if Word.get_value (fst cond_32) <> 0 then (
        assert_env_length w_139 4;
        return_n w_139 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_139 4;
        ignore (env_call w_139 [] [ 0; 2 ]);
        w_139.state.c <- pc_to_exp (int_to_pc 57)))
    139;
  add_exp
    (fun w_140 ->
      assert_env_length w_140 4;
      let cond_33 = resolve w_140 (Source.E 3) in
      if Word.get_value (fst cond_33) <> 0 then (
        assert_env_length w_140 4;
        return_n w_140 1 (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_140 4;
        ignore (env_call w_140 [] [ 0; 2 ]);
        w_140.state.c <- pc_to_exp (int_to_pc 57)))
    140;
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
  Words.set_constructor_degree 35 (-1);
  Words.set_constructor_degree 36 0;
  Words.set_constructor_degree 37 (-4);
  Words.set_constructor_degree 38 (-2);
  Words.set_constructor_degree 39 (-4);
  Words.set_constructor_degree 40 (-3);
  Words.set_constructor_degree 41 (-2);
  Words.set_constructor_degree 42 (-1);
  Words.set_constructor_degree 43 0;
  Words.set_constructor_degree 44 (-1);
  Words.set_constructor_degree 45 (-2);
  Words.set_constructor_degree 46 (-2);
  Words.set_constructor_degree 47 (-1);
  Words.set_constructor_degree 48 (-1);
  Words.set_constructor_degree 49 (-1);
  Words.set_constructor_degree 50 (-1);
  Words.set_constructor_degree 51 (-2);
  Words.set_constructor_degree 52 (-3);
  Words.set_constructor_degree 53 (-3);
  Words.set_constructor_degree 54 0;
  Words.set_constructor_degree 55 (-3);
  Words.set_constructor_degree 56 (-1);
  Words.set_constructor_degree 57 (-1);
  Words.set_constructor_degree 58 (-1);
  Words.set_constructor_degree 59 (-1);
  Words.set_constructor_degree 60 (-1);
  Words.set_constructor_degree 61 (-3);
  Words.set_constructor_degree 62 (-3);
  Words.set_constructor_degree 63 (-4);
  Words.set_constructor_degree 64 (-1);
  Words.set_constructor_degree 65 (-3);
  Words.set_constructor_degree 66 (-1);
  Words.set_constructor_degree 67 (-1);
  Words.set_constructor_degree 68 (-1);
  Words.set_constructor_degree 69 (-1);
  Words.set_constructor_degree 70 (-1);
  Words.set_constructor_degree 71 (-1);
  Words.set_constructor_degree 72 (-1);
  Words.set_constructor_degree 73 (-4);
  Words.set_constructor_degree 74 (-2);
  Words.set_constructor_degree 75 (-4);
  Words.set_constructor_degree 76 (-4);
  Words.set_constructor_degree 77 (-1);
  Words.set_constructor_degree 78 0;
  Words.set_constructor_degree 79 0;
  Words.set_constructor_degree 80 (-1);
  Words.set_constructor_degree 81 (-2);
  Words.set_constructor_degree 82 (-2);
  Words.set_constructor_degree 83 (-1);
  Words.set_constructor_degree 84 (-1);
  Words.set_constructor_degree 85 (-1);
  Words.set_constructor_degree 86 (-1);
  Words.set_constructor_degree 87 (-2);
  Words.set_constructor_degree 88 (-1);
  Words.set_constructor_degree 89 (-1);
  Words.set_constructor_degree 90 0;
  Words.set_constructor_degree 91 (-1);
  Words.set_constructor_degree 92 (-2);
  Words.set_constructor_degree 93 (-2);
  Words.set_constructor_degree 94 (-2);
  Words.set_constructor_degree 95 (-1);
  Words.set_constructor_degree 96 (-2);
  Words.set_constructor_degree 97 0;
  Words.set_constructor_degree 98 (-5);
  Words.set_constructor_degree 99 (-1);
  Words.set_constructor_degree 100 0;
  Words.set_constructor_degree 101 (-1);
  Words.set_constructor_degree 102 0;
  Words.set_constructor_degree 103 0;
  Words.set_constructor_degree 104 (-3);
  Words.set_constructor_degree 105 (-3);
  Words.set_constructor_degree 106 (-1);
  Words.set_constructor_degree 107 (-2);
  Words.set_constructor_degree 108 (-1);
  Words.set_constructor_degree 109 (-2);
  Words.set_constructor_degree 110 0;
  Words.set_constructor_degree 111 (-2);
  Words.set_constructor_degree 112 (-2);
  Words.set_constructor_degree 113 (-3);
  Words.set_constructor_degree 114 (-1);
  Words.set_constructor_degree 115 (-1);
  Words.set_constructor_degree 116 0;
  Words.set_constructor_degree 117 (-2);
  Words.set_constructor_degree 118 (-2);
  Words.set_constructor_degree 119 (-1);
  Words.set_constructor_degree 120 (-1);
  Words.set_constructor_degree 121 (-2);
  Words.set_constructor_degree 122 (-2);
  Words.set_constructor_degree 123 (-1);
  Words.set_constructor_degree 124 (-3);
  Words.set_constructor_degree 125 (-3);
  Words.set_constructor_degree 126 (-1);
  Words.set_constructor_degree 127 (-1);
  Words.set_constructor_degree 128 (-1);
  Words.set_constructor_degree 129 0;
  Words.set_constructor_degree 130 0;
  Words.set_constructor_degree 131 0;
  Words.set_constructor_degree 132 0
