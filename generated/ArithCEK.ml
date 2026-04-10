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
let tag_cont_0 = 15
let tag_cont_1 = 16
let tag_cont_2 = 17
let tag_cont_3 = 18
let tag_cont_4 = 19
let tag_cont_5 = 20
let tag_cont_6 = 21
let tag_cont_7 = 22
let tag_cont_8 = 23
let tag_cont_9 = 24
let tag_cont_10 = 25
let tag_cont_11 = 26
let tag_cont_12 = 27
let tag_cont_13 = 28
let tag_cont_14 = 29
let tag_cont_15 = 30
let tag_cont_16 = 31
let tag_cont_17 = 32
let tag_cont_18 = 33
let tag_cont_19 = 34
let tag_cont_20 = 35
let tag_cont_21 = 36
let tag_cont_22 = 37
let tag_cont_23 = 38
let tag_cont_24 = 39
let tag_cont_25 = 40
let tag_cont_26 = 41
let tag_cont_27 = 42
let tag_cont_28 = 43
let tag_cont_29 = 44
let tag_cont_30 = 45
let tag_cont_31 = 46
let tag_cont_32 = 47
let tag_cont_33 = 48
let tag_cont_34 = 49
let tag_cont_35 = 50
let tag_cont_36 = 51
let tag_cont_37 = 52
let tag_cont_38 = 53
let tag_cont_39 = 54
let tag_cont_40 = 55
let tag_cont_41 = 56
let tag_cont_42 = 57
let tag_cont_43 = 58
let tag_cont_44 = 59
let tag_cont_45 = 60
let tag_cont_46 = 61
let tag_cont_47 = 62
let tag_cont_48 = 63
let tag_cont_49 = 64
let tag_cont_50 = 65
let tag_cont_51 = 66
let tag_cont_52 = 67
let tag_cont_53 = 68
let tag_cont_54 = 69
let tag_cont_55 = 70
let tag_cont_56 = 71
let tag_cont_57 = 72
let tag_cont_58 = 73
let tag_cont_59 = 74
let tag_cont_60 = 75
let tag_cont_61 = 76
let tag_cont_62 = 77
let tag_cont_63 = 78
let tag_cont_64 = 79
let tag_cont_65 = 80
let tag_cont_66 = 81
let tag_cont_67 = 82
let tag_cont_68 = 83
let tag_cont_69 = 84
let tag_cont_70 = 85
let tag_cont_71 = 86
let tag_cont_72 = 87
let tag_cont_73 = 88
let tag_cont_74 = 89
let tag_cont_75 = 90
let tag_cont_76 = 91
let tag_cont_77 = 92
let tag_cont_78 = 93
let tag_cont_79 = 94
let tag_cont_80 = 95
let tag_cont_81 = 96
let tag_cont_82 = 97
let tag_cont_83 = 98
let tag_cont_84 = 99
let tag_cont_85 = 100
let tag_cont_86 = 101
let tag_cont_87 = 102
let tag_cont_88 = 103
let tag_cont_89 = 104
let tag_cont_90 = 105
let tag_cont_91 = 106
let tag_cont_92 = 107
let tag_cont_93 = 108
let tag_cont_94 = 109
let tag_cont_95 = 110
let tag_cont_96 = 111
let tag_cont_97 = 112
let tag_cont_98 = 113
let tag_cont_99 = 114
let tag_cont_100 = 115
let tag_cont_101 = 116
let tag_cont_102 = 117
let tag_cont_103 = 118
let tag_cont_104 = 119
let tag_cont_105 = 120
let tag_cont_106 = 121
let tag_cont_107 = 122
let tag_cont_108 = 123
let tag_cont_109 = 124
let tag_cont_110 = 125
let tag_cont_111 = 126
let tag_cont_112 = 127
let tag_cont_113 = 128
let tag_cont_114 = 129
let tag_cont_115 = 130
let tag_cont_116 = 131
let tag_cont_117 = 132

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

let var_rank memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 1)) initial_env (Memo.from_constructor tag_cont_done) memo

let expr_rank memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 2)) initial_env (Memo.from_constructor tag_cont_done) memo

let compare_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 3)) initial_env (Memo.from_constructor tag_cont_done) memo

let expr_equal memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 6)) initial_env (Memo.from_constructor tag_cont_done) memo

let expr_size memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 7)) initial_env (Memo.from_constructor tag_cont_done) memo

let better_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 8)) initial_env (Memo.from_constructor tag_cont_done) memo

let scale memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 9)) initial_env (Memo.from_constructor tag_cont_done) memo

let coeff_value memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 12)) initial_env (Memo.from_constructor tag_cont_done) memo

let coeff_base memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 14)) initial_env (Memo.from_constructor tag_cont_done) memo

let extract_factor memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 15)) initial_env (Memo.from_constructor tag_cont_done) memo

let search_factor memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 16)) initial_env (Memo.from_constructor tag_cont_done) memo

let append_exprs memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 17)) initial_env (Memo.from_constructor tag_cont_done) memo

let insert_expr memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 18)) initial_env (Memo.from_constructor tag_cont_done) memo

let sort_exprs memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 19)) initial_env (Memo.from_constructor tag_cont_done) memo

let compare_add_term memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 20)) initial_env (Memo.from_constructor tag_cont_done) memo

let insert_add_term memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 21)) initial_env (Memo.from_constructor tag_cont_done) memo

let sort_add_terms memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 22)) initial_env (Memo.from_constructor tag_cont_done) memo

let reverse_exprs_aux memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 23)) initial_env (Memo.from_constructor tag_cont_done) memo

let reverse_exprs memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 24)) initial_env (Memo.from_constructor tag_cont_done) memo

let flatten_add memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 26)) initial_env (Memo.from_constructor tag_cont_done) memo

let flatten_mul memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 27)) initial_env (Memo.from_constructor tag_cont_done) memo

let mul_coeff memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 29)) initial_env (Memo.from_constructor tag_cont_done) memo

let mul_base memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 31)) initial_env (Memo.from_constructor tag_cont_done) memo

let mul_total_coeff memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 32)) initial_env (Memo.from_constructor tag_cont_done) memo

let mul_bases memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 34)) initial_env (Memo.from_constructor tag_cont_done) memo

let build_mul memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 36)) initial_env (Memo.from_constructor tag_cont_done) memo

let normalize_mul_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 1 x0;
  Dynarray.set initial_env 0 x1;
  exec_cek (pc_to_exp (int_to_pc 37)) initial_env (Memo.from_constructor tag_cont_done) memo

let combine_like_terms_acc memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 3 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  Dynarray.set initial_env 2 x2;
  exec_cek (pc_to_exp (int_to_pc 38)) initial_env (Memo.from_constructor tag_cont_done) memo

let combine_like_terms memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 39)) initial_env (Memo.from_constructor tag_cont_done) memo

let factor_adjacent memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 41)) initial_env (Memo.from_constructor tag_cont_done) memo

let pick_factored memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 42)) initial_env (Memo.from_constructor tag_cont_done) memo

let search_terms memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 43)) initial_env (Memo.from_constructor tag_cont_done) memo

let build_add memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 45)) initial_env (Memo.from_constructor tag_cont_done) memo

let search_round memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 46)) initial_env (Memo.from_constructor tag_cont_done) memo

let normalize_add_flat memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 1 x0;
  Dynarray.set initial_env 0 x1;
  exec_cek (pc_to_exp (int_to_pc 47)) initial_env (Memo.from_constructor tag_cont_done) memo

let search_opt memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 48)) initial_env (Memo.from_constructor tag_cont_done) memo

let normalize memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 51)) initial_env (Memo.from_constructor tag_cont_done) memo

let simplify_aux memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 52)) initial_env (Memo.from_constructor tag_cont_done) memo

let diffx memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 54)) initial_env (Memo.from_constructor tag_cont_done) memo

let eval memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 3 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  Dynarray.set initial_env 2 x2;
  exec_cek (pc_to_exp (int_to_pc 56)) initial_env (Memo.from_constructor tag_cont_done) memo

let main memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 57)) initial_env (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_175 ->
      assert_env_length w_175 1;
      let hd_0, tl_0 = resolve w_175 K in
      match Word.get_value hd_0 with
      | 15 (* tag_cont_0 *) ->
          let ret_0 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 3 ] tl_0;
          set_env_slot w_175 4 ret_0;
          w_175.state.c <- pc_to_exp (int_to_pc 59)
      | 16 (* tag_cont_1 *) ->
          let ret_1 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 1; 2 ] tl_0;
          set_env_slot w_175 3 ret_1;
          w_175.state.c <- pc_to_exp (int_to_pc 58)
      | 17 (* tag_cont_2 *) ->
          let ret_2 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 4 ] tl_0;
          set_env_slot w_175 5 ret_2;
          w_175.state.c <- pc_to_exp (int_to_pc 64)
      | 18 (* tag_cont_3 *) ->
          let ret_3 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 4 ret_3;
          w_175.state.c <- pc_to_exp (int_to_pc 63)
      | 19 (* tag_cont_4 *) ->
          let ret_4 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 3 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 2 ret_4;
          w_175.state.c <- pc_to_exp (int_to_pc 62)
      | 20 (* tag_cont_5 *) ->
          let ret_5 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2 ] tl_0;
          set_env_slot w_175 3 ret_5;
          w_175.state.c <- pc_to_exp (int_to_pc 61)
      | 21 (* tag_cont_6 *) ->
          let ret_6 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 3 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 2 ret_6;
          w_175.state.c <- pc_to_exp (int_to_pc 60)
      | 22 (* tag_cont_7 *) ->
          let ret_7 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 3 ] tl_0;
          set_env_slot w_175 4 ret_7;
          w_175.state.c <- pc_to_exp (int_to_pc 66)
      | 23 (* tag_cont_8 *) ->
          let ret_8 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 3 ret_8;
          w_175.state.c <- pc_to_exp (int_to_pc 65)
      | 24 (* tag_cont_9 *) ->
          let ret_9 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 3 ] tl_0;
          set_env_slot w_175 4 ret_9;
          w_175.state.c <- pc_to_exp (int_to_pc 68)
      | 25 (* tag_cont_10 *) ->
          let ret_10 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 3 ret_10;
          w_175.state.c <- pc_to_exp (int_to_pc 67)
      | 26 (* tag_cont_11 *) ->
          let ret_11 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 5 ] tl_0;
          set_env_slot w_175 6 ret_11;
          w_175.state.c <- pc_to_exp (int_to_pc 70)
      | 27 (* tag_cont_12 *) ->
          let ret_12 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 5 ret_12;
          w_175.state.c <- pc_to_exp (int_to_pc 69)
      | 28 (* tag_cont_13 *) ->
          let ret_13 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 8 ] tl_0;
          set_env_slot w_175 9 ret_13;
          w_175.state.c <- pc_to_exp (int_to_pc 72)
      | 29 (* tag_cont_14 *) ->
          let ret_14 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 4 ] tl_0;
          set_env_slot w_175 8 ret_14;
          w_175.state.c <- pc_to_exp (int_to_pc 71)
      | 30 (* tag_cont_15 *) ->
          let ret_15 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_15;
          w_175.state.c <- pc_to_exp (int_to_pc 86)
      | 31 (* tag_cont_16 *) ->
          let ret_16 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_16;
          w_175.state.c <- pc_to_exp (int_to_pc 85)
      | 32 (* tag_cont_17 *) ->
          let ret_17 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_17;
          w_175.state.c <- pc_to_exp (int_to_pc 84)
      | 33 (* tag_cont_18 *) ->
          let ret_18 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_18;
          w_175.state.c <- pc_to_exp (int_to_pc 83)
      | 34 (* tag_cont_19 *) ->
          let ret_19 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_19;
          w_175.state.c <- pc_to_exp (int_to_pc 82)
      | 35 (* tag_cont_20 *) ->
          let ret_20 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 0 ret_20;
          w_175.state.c <- pc_to_exp (int_to_pc 81)
      | 36 (* tag_cont_21 *) ->
          let ret_21 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_21;
          w_175.state.c <- pc_to_exp (int_to_pc 80)
      | 37 (* tag_cont_22 *) ->
          let ret_22 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_22;
          w_175.state.c <- pc_to_exp (int_to_pc 79)
      | 38 (* tag_cont_23 *) ->
          let ret_23 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 0 ret_23;
          w_175.state.c <- pc_to_exp (int_to_pc 78)
      | 39 (* tag_cont_24 *) ->
          let ret_24 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 1 ret_24;
          w_175.state.c <- pc_to_exp (int_to_pc 77)
      | 40 (* tag_cont_25 *) ->
          let ret_25 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_25;
          w_175.state.c <- pc_to_exp (int_to_pc 76)
      | 41 (* tag_cont_26 *) ->
          let ret_26 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_26;
          w_175.state.c <- pc_to_exp (int_to_pc 75)
      | 42 (* tag_cont_27 *) ->
          let ret_27 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 0 ret_27;
          w_175.state.c <- pc_to_exp (int_to_pc 74)
      | 43 (* tag_cont_28 *) ->
          let ret_28 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_28;
          w_175.state.c <- pc_to_exp (int_to_pc 73)
      | 44 (* tag_cont_29 *) ->
          let ret_29 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 3 ret_29;
          w_175.state.c <- pc_to_exp (int_to_pc 87)
      | 45 (* tag_cont_30 *) ->
          let ret_30 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 20 (Memo.from_int 0);
          restore_env_slots w_175 [ 18 ] tl_0;
          set_env_slot w_175 19 ret_30;
          w_175.state.c <- pc_to_exp (int_to_pc 91)
      | 46 (* tag_cont_31 *) ->
          let ret_31 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 19 (Memo.from_int 0);
          restore_env_slots w_175 [ 4 ] tl_0;
          set_env_slot w_175 18 ret_31;
          w_175.state.c <- pc_to_exp (int_to_pc 90)
      | 47 (* tag_cont_32 *) ->
          let ret_32 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 24 (Memo.from_int 0);
          restore_env_slots w_175 [ 6; 8 ] tl_0;
          set_env_slot w_175 23 ret_32;
          w_175.state.c <- pc_to_exp (int_to_pc 92)
      | 48 (* tag_cont_33 *) ->
          let ret_33 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 27 (Memo.from_int 0);
          restore_env_slots w_175 [ 10; 12 ] tl_0;
          set_env_slot w_175 26 ret_33;
          w_175.state.c <- pc_to_exp (int_to_pc 93)
      | 49 (* tag_cont_34 *) ->
          let ret_34 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 15 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 13 ] tl_0;
          set_env_slot w_175 14 ret_34;
          w_175.state.c <- pc_to_exp (int_to_pc 89)
      | 50 (* tag_cont_35 *) ->
          let ret_35 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 14 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 13 ret_35;
          w_175.state.c <- pc_to_exp (int_to_pc 88)
      | 51 (* tag_cont_36 *) ->
          let ret_36 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 6 ret_36;
          w_175.state.c <- pc_to_exp (int_to_pc 95)
      | 52 (* tag_cont_37 *) ->
          let ret_37 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 8 ret_37;
          w_175.state.c <- pc_to_exp (int_to_pc 96)
      | 53 (* tag_cont_38 *) ->
          let ret_38 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 1; 2; 3; 4 ] tl_0;
          set_env_slot w_175 5 ret_38;
          w_175.state.c <- pc_to_exp (int_to_pc 94)
      | 54 (* tag_cont_39 *) ->
          let ret_39 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 8 ret_39;
          w_175.state.c <- pc_to_exp (int_to_pc 98)
      | 55 (* tag_cont_40 *) ->
          let ret_40 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2; 3 ] tl_0;
          set_env_slot w_175 5 ret_40;
          w_175.state.c <- pc_to_exp (int_to_pc 97)
      | 56 (* tag_cont_41 *) ->
          let ret_41 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_41;
          w_175.state.c <- pc_to_exp (int_to_pc 100)
      | 57 (* tag_cont_42 *) ->
          let ret_42 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_42;
          w_175.state.c <- pc_to_exp (int_to_pc 99)
      | 58 (* tag_cont_43 *) ->
          let ret_43 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 0 ret_43;
          w_175.state.c <- pc_to_exp (int_to_pc 102)
      | 59 (* tag_cont_44 *) ->
          let ret_44 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_44;
          w_175.state.c <- pc_to_exp (int_to_pc 101)
      | 60 (* tag_cont_45 *) ->
          let ret_45 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 4; 5 ] tl_0;
          set_env_slot w_175 9 ret_45;
          w_175.state.c <- pc_to_exp (int_to_pc 105)
      | 61 (* tag_cont_46 *) ->
          let ret_46 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 2; 3; 4; 5; 6 ] tl_0;
          set_env_slot w_175 8 ret_46;
          w_175.state.c <- pc_to_exp (int_to_pc 104)
      | 62 (* tag_cont_47 *) ->
          let ret_47 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 2; 3; 4; 5 ] tl_0;
          set_env_slot w_175 6 ret_47;
          w_175.state.c <- pc_to_exp (int_to_pc 103)
      | 63 (* tag_cont_48 *) ->
          let ret_48 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 3 ] tl_0;
          set_env_slot w_175 4 ret_48;
          w_175.state.c <- pc_to_exp (int_to_pc 106)
      | 64 (* tag_cont_49 *) ->
          let ret_49 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 3 ] tl_0;
          set_env_slot w_175 4 ret_49;
          w_175.state.c <- pc_to_exp (int_to_pc 108)
      | 65 (* tag_cont_50 *) ->
          let ret_50 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 3 ret_50;
          w_175.state.c <- pc_to_exp (int_to_pc 107)
      | 66 (* tag_cont_51 *) ->
          let ret_51 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 15 (Memo.from_int 0);
          restore_env_slots w_175 [ 13 ] tl_0;
          set_env_slot w_175 14 ret_51;
          w_175.state.c <- pc_to_exp (int_to_pc 110)
      | 67 (* tag_cont_52 *) ->
          let ret_52 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 14 (Memo.from_int 0);
          restore_env_slots w_175 [ 4 ] tl_0;
          set_env_slot w_175 13 ret_52;
          w_175.state.c <- pc_to_exp (int_to_pc 109)
      | 68 (* tag_cont_53 *) ->
          let ret_53 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 18 (Memo.from_int 0);
          restore_env_slots w_175 [ 16 ] tl_0;
          set_env_slot w_175 17 ret_53;
          w_175.state.c <- pc_to_exp (int_to_pc 112)
      | 69 (* tag_cont_54 *) ->
          let ret_54 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 17 (Memo.from_int 0);
          restore_env_slots w_175 [ 6; 8 ] tl_0;
          set_env_slot w_175 16 ret_54;
          w_175.state.c <- pc_to_exp (int_to_pc 111)
      | 70 (* tag_cont_55 *) ->
          let ret_55 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 21 (Memo.from_int 0);
          restore_env_slots w_175 [ 19 ] tl_0;
          set_env_slot w_175 20 ret_55;
          w_175.state.c <- pc_to_exp (int_to_pc 114)
      | 71 (* tag_cont_56 *) ->
          let ret_56 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 20 (Memo.from_int 0);
          restore_env_slots w_175 [ 10; 12 ] tl_0;
          set_env_slot w_175 19 ret_56;
          w_175.state.c <- pc_to_exp (int_to_pc 113)
      | 72 (* tag_cont_57 *) ->
          let ret_57 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 8 ] tl_0;
          set_env_slot w_175 9 ret_57;
          w_175.state.c <- pc_to_exp (int_to_pc 116)
      | 73 (* tag_cont_58 *) ->
          let ret_58 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 1; 2; 5 ] tl_0;
          set_env_slot w_175 8 ret_58;
          w_175.state.c <- pc_to_exp (int_to_pc 115)
      | 74 (* tag_cont_59 *) ->
          let ret_59 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 13 (Memo.from_int 0);
          restore_env_slots w_175 [ 11 ] tl_0;
          set_env_slot w_175 12 ret_59;
          w_175.state.c <- pc_to_exp (int_to_pc 118)
      | 75 (* tag_cont_60 *) ->
          let ret_60 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 12 (Memo.from_int 0);
          restore_env_slots w_175 [ 1; 2; 7 ] tl_0;
          set_env_slot w_175 11 ret_60;
          w_175.state.c <- pc_to_exp (int_to_pc 117)
      | 76 (* tag_cont_61 *) ->
          let ret_61 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2; 3; 5 ] tl_0;
          set_env_slot w_175 9 ret_61;
          w_175.state.c <- pc_to_exp (int_to_pc 120)
      | 77 (* tag_cont_62 *) ->
          let ret_62 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2; 3; 4; 5 ] tl_0;
          set_env_slot w_175 6 ret_62;
          w_175.state.c <- pc_to_exp (int_to_pc 119)
      | 78 (* tag_cont_63 *) ->
          let ret_63 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 3 ret_63;
          w_175.state.c <- pc_to_exp (int_to_pc 121)
      | 79 (* tag_cont_64 *) ->
          let ret_64 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_64;
          w_175.state.c <- pc_to_exp (int_to_pc 124)
      | 80 (* tag_cont_65 *) ->
          let ret_65 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_65;
          w_175.state.c <- pc_to_exp (int_to_pc 123)
      | 81 (* tag_cont_66 *) ->
          let ret_66 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_66;
          w_175.state.c <- pc_to_exp (int_to_pc 122)
      | 82 (* tag_cont_67 *) ->
          let ret_67 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 5 ret_67;
          w_175.state.c <- pc_to_exp (int_to_pc 130)
      | 83 (* tag_cont_68 *) ->
          let ret_68 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 4 ret_68;
          w_175.state.c <- pc_to_exp (int_to_pc 129)
      | 84 (* tag_cont_69 *) ->
          let ret_69 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_69;
          w_175.state.c <- pc_to_exp (int_to_pc 128)
      | 85 (* tag_cont_70 *) ->
          let ret_70 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 1 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 0 ret_70;
          w_175.state.c <- pc_to_exp (int_to_pc 127)
      | 86 (* tag_cont_71 *) ->
          let ret_71 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 1 ] tl_0;
          set_env_slot w_175 0 ret_71;
          w_175.state.c <- pc_to_exp (int_to_pc 126)
      | 87 (* tag_cont_72 *) ->
          let ret_72 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 2 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 1 ret_72;
          w_175.state.c <- pc_to_exp (int_to_pc 125)
      | 88 (* tag_cont_73 *) ->
          let ret_73 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 3 ] tl_0;
          set_env_slot w_175 4 ret_73;
          w_175.state.c <- pc_to_exp (int_to_pc 132)
      | 89 (* tag_cont_74 *) ->
          let ret_74 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 3 ret_74;
          w_175.state.c <- pc_to_exp (int_to_pc 131)
      | 90 (* tag_cont_75 *) ->
          let ret_75 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 3 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 2 ret_75;
          w_175.state.c <- pc_to_exp (int_to_pc 133)
      | 91 (* tag_cont_76 *) ->
          let ret_76 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 11 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 10 ret_76;
          w_175.state.c <- pc_to_exp (int_to_pc 136)
      | 92 (* tag_cont_77 *) ->
          let ret_77 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 8 ret_77;
          w_175.state.c <- pc_to_exp (int_to_pc 137)
      | 93 (* tag_cont_78 *) ->
          let ret_78 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 8 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 3; 5 ] tl_0;
          set_env_slot w_175 7 ret_78;
          w_175.state.c <- pc_to_exp (int_to_pc 135)
      | 94 (* tag_cont_79 *) ->
          let ret_79 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2; 3 ] tl_0;
          set_env_slot w_175 5 ret_79;
          w_175.state.c <- pc_to_exp (int_to_pc 134)
      | 95 (* tag_cont_80 *) ->
          let ret_80 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [] tl_0;
          set_env_slot w_175 6 ret_80;
          w_175.state.c <- pc_to_exp (int_to_pc 138)
      | 96 (* tag_cont_81 *) ->
          let ret_81 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 16 (Memo.from_int 0);
          restore_env_slots w_175 [ 13 ] tl_0;
          set_env_slot w_175 15 ret_81;
          w_175.state.c <- pc_to_exp (int_to_pc 143)
      | 97 (* tag_cont_82 *) ->
          let ret_82 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 14 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 13 ret_82;
          w_175.state.c <- pc_to_exp (int_to_pc 142)
      | 98 (* tag_cont_83 *) ->
          let ret_83 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 11 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 4; 8; 9 ] tl_0;
          set_env_slot w_175 10 ret_83;
          w_175.state.c <- pc_to_exp (int_to_pc 141)
      | 99 (* tag_cont_84 *) ->
          let ret_84 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 4; 8 ] tl_0;
          set_env_slot w_175 9 ret_84;
          w_175.state.c <- pc_to_exp (int_to_pc 140)
      | 100 (* tag_cont_85 *) ->
          let ret_85 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 3; 4 ] tl_0;
          set_env_slot w_175 8 ret_85;
          w_175.state.c <- pc_to_exp (int_to_pc 139)
      | 101 (* tag_cont_86 *) ->
          let ret_86 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 5 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 4 ret_86;
          w_175.state.c <- pc_to_exp (int_to_pc 144)
      | 102 (* tag_cont_87 *) ->
          let ret_87 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 13 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 5 ] tl_0;
          set_env_slot w_175 12 ret_87;
          w_175.state.c <- pc_to_exp (int_to_pc 147)
      | 103 (* tag_cont_88 *) ->
          let ret_88 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 2; 3; 4; 5 ] tl_0;
          set_env_slot w_175 9 ret_88;
          w_175.state.c <- pc_to_exp (int_to_pc 146)
      | 104 (* tag_cont_89 *) ->
          let ret_89 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 6 ret_89;
          w_175.state.c <- pc_to_exp (int_to_pc 145)
      | 105 (* tag_cont_90 *) ->
          let ret_90 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 8 (Memo.from_int 0);
          restore_env_slots w_175 [ 6 ] tl_0;
          set_env_slot w_175 7 ret_90;
          w_175.state.c <- pc_to_exp (int_to_pc 149)
      | 106 (* tag_cont_91 *) ->
          let ret_91 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 6 ret_91;
          w_175.state.c <- pc_to_exp (int_to_pc 148)
      | 107 (* tag_cont_92 *) ->
          let ret_92 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 12 (Memo.from_int 0);
          restore_env_slots w_175 [ 3; 10 ] tl_0;
          set_env_slot w_175 11 ret_92;
          w_175.state.c <- pc_to_exp (int_to_pc 151)
      | 108 (* tag_cont_93 *) ->
          let ret_93 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 3; 4 ] tl_0;
          set_env_slot w_175 9 ret_93;
          w_175.state.c <- pc_to_exp (int_to_pc 150)
      | 109 (* tag_cont_94 *) ->
          let ret_94 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 8 (Memo.from_int 0);
          restore_env_slots w_175 [ 6 ] tl_0;
          set_env_slot w_175 7 ret_94;
          w_175.state.c <- pc_to_exp (int_to_pc 153)
      | 110 (* tag_cont_95 *) ->
          let ret_95 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 5 ret_95;
          w_175.state.c <- pc_to_exp (int_to_pc 152)
      | 111 (* tag_cont_96 *) ->
          let ret_96 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 12 (Memo.from_int 0);
          restore_env_slots w_175 [ 10 ] tl_0;
          set_env_slot w_175 11 ret_96;
          w_175.state.c <- pc_to_exp (int_to_pc 155)
      | 112 (* tag_cont_97 *) ->
          let ret_97 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 4 ] tl_0;
          set_env_slot w_175 9 ret_97;
          w_175.state.c <- pc_to_exp (int_to_pc 154)
      | 113 (* tag_cont_98 *) ->
          let ret_98 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 7 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 6 ret_98;
          w_175.state.c <- pc_to_exp (int_to_pc 158)
      | 114 (* tag_cont_99 *) ->
          let ret_99 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 4 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2 ] tl_0;
          set_env_slot w_175 3 ret_99;
          w_175.state.c <- pc_to_exp (int_to_pc 157)
      | 115 (* tag_cont_100 *) ->
          let ret_100 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 3 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1 ] tl_0;
          set_env_slot w_175 2 ret_100;
          w_175.state.c <- pc_to_exp (int_to_pc 156)
      | 116 (* tag_cont_101 *) ->
          let ret_101 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 14 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 9; 12 ] tl_0;
          set_env_slot w_175 13 ret_101;
          w_175.state.c <- pc_to_exp (int_to_pc 165)
      | 117 (* tag_cont_102 *) ->
          let ret_102 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 13 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 9 ] tl_0;
          set_env_slot w_175 12 ret_102;
          w_175.state.c <- pc_to_exp (int_to_pc 164)
      | 118 (* tag_cont_103 *) ->
          let ret_103 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 12 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 9 ] tl_0;
          set_env_slot w_175 11 ret_103;
          w_175.state.c <- pc_to_exp (int_to_pc 163)
      | 119 (* tag_cont_104 *) ->
          let ret_104 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 11 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 9 ] tl_0;
          set_env_slot w_175 10 ret_104;
          w_175.state.c <- pc_to_exp (int_to_pc 162)
      | 120 (* tag_cont_105 *) ->
          let ret_105 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 10 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 7; 8 ] tl_0;
          set_env_slot w_175 9 ret_105;
          w_175.state.c <- pc_to_exp (int_to_pc 161)
      | 121 (* tag_cont_106 *) ->
          let ret_106 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 7 ] tl_0;
          set_env_slot w_175 8 ret_106;
          w_175.state.c <- pc_to_exp (int_to_pc 160)
      | 122 (* tag_cont_107 *) ->
          let ret_107 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 8 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 4 ] tl_0;
          set_env_slot w_175 7 ret_107;
          w_175.state.c <- pc_to_exp (int_to_pc 159)
      | 123 (* tag_cont_108 *) ->
          let ret_108 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 22 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 17; 20 ] tl_0;
          set_env_slot w_175 21 ret_108;
          w_175.state.c <- pc_to_exp (int_to_pc 172)
      | 124 (* tag_cont_109 *) ->
          let ret_109 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 21 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 17 ] tl_0;
          set_env_slot w_175 20 ret_109;
          w_175.state.c <- pc_to_exp (int_to_pc 171)
      | 125 (* tag_cont_110 *) ->
          let ret_110 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 20 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 17 ] tl_0;
          set_env_slot w_175 19 ret_110;
          w_175.state.c <- pc_to_exp (int_to_pc 170)
      | 126 (* tag_cont_111 *) ->
          let ret_111 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 19 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 17 ] tl_0;
          set_env_slot w_175 18 ret_111;
          w_175.state.c <- pc_to_exp (int_to_pc 169)
      | 127 (* tag_cont_112 *) ->
          let ret_112 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 18 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 15; 16 ] tl_0;
          set_env_slot w_175 17 ret_112;
          w_175.state.c <- pc_to_exp (int_to_pc 168)
      | 128 (* tag_cont_113 *) ->
          let ret_113 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 17 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 15 ] tl_0;
          set_env_slot w_175 16 ret_113;
          w_175.state.c <- pc_to_exp (int_to_pc 167)
      | 129 (* tag_cont_114 *) ->
          let ret_114 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 16 (Memo.from_int 0);
          restore_env_slots w_175 [ 2; 6 ] tl_0;
          set_env_slot w_175 15 ret_114;
          w_175.state.c <- pc_to_exp (int_to_pc 166)
      | 130 (* tag_cont_115 *) ->
          let ret_115 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 3 (Memo.from_int 0);
          restore_env_slots w_175 [ 0 ] tl_0;
          set_env_slot w_175 2 ret_115;
          w_175.state.c <- pc_to_exp (int_to_pc 173)
      | 131 (* tag_cont_116 *) ->
          let ret_116 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 9 (Memo.from_int 0);
          restore_env_slots w_175 [ 2 ] tl_0;
          set_env_slot w_175 8 ret_116;
          w_175.state.c <- pc_to_exp (int_to_pc 175)
      | 132 (* tag_cont_117 *) ->
          let ret_117 = get_env_slot w_175 0 in
          assert_env_length w_175 1;
          w_175.state.k <- get_next_cont tl_0;
          init_frame w_175 6 (Memo.from_int 0);
          restore_env_slots w_175 [ 0; 1; 2; 3 ] tl_0;
          set_env_slot w_175 5 ret_117;
          w_175.state.c <- pc_to_exp (int_to_pc 174)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_144 ->
      assert_env_length w_144 1;
      assert_env_length w_144 1;
      let resolved_130 = resolve w_144 (Source.E 0) in
      let tag_53 = Word.get_value (fst resolved_130) in
      match tag_53 with
      | 3 (* tag_X *) -> return_value w_144 (Memo.from_int 0) (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) -> return_value w_144 (Memo.from_int 1) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (1)")
    1;
  add_exp
    (fun w_13 ->
      assert_env_length w_13 1;
      assert_env_length w_13 1;
      let resolved_17 = resolve w_13 (Source.E 0) in
      let tag_3 = Word.get_value (fst resolved_17) in
      match tag_3 with
      | 5 (* tag_Const *) ->
          let parts_3 = Memo.splits (snd resolved_17) in
          if List.length parts_3 = 1 then
            let part0_3 = List.nth parts_3 0 in
            return_value w_13 (Memo.from_int 0) (pc_to_exp (int_to_pc 0))
          else failwith "unreachable (2)"
      | 6 (* tag_Var *) ->
          let parts_4 = Memo.splits (snd resolved_17) in
          if List.length parts_4 = 1 then
            let part0_4 = List.nth parts_4 0 in
            return_value w_13 (Memo.from_int 1) (pc_to_exp (int_to_pc 0))
          else failwith "unreachable (2)"
      | 7 (* tag_Add *) ->
          let parts_5 = Memo.splits (snd resolved_17) in
          if List.length parts_5 = 2 then
            let part0_5 = List.nth parts_5 0 in
            let part1_2 = List.nth parts_5 1 in
            return_value w_13 (Memo.from_int 2) (pc_to_exp (int_to_pc 0))
          else failwith "unreachable (2)"
      | 8 (* tag_Mul *) ->
          let parts_6 = Memo.splits (snd resolved_17) in
          if List.length parts_6 = 2 then
            let part0_6 = List.nth parts_6 0 in
            let part1_3 = List.nth parts_6 1 in
            return_value w_13 (Memo.from_int 3) (pc_to_exp (int_to_pc 0))
          else failwith "unreachable (2)"
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_47 ->
      assert_env_length w_47 2;
      resize_frame w_47 29 (Memo.from_int 0);
      let arg0_42 = get_env_slot w_47 0 in
      assert_env_length w_47 29;
      w_47.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; collect_env_slots w_47 [ 0; 1 ]; w_47.state.k ];
      trim_resolved w_47 2;
      init_frame w_47 1 (Memo.from_int 0);
      set_env_slot w_47 0 arg0_42;
      w_47.state.c <- pc_to_exp (int_to_pc 2))
    3;
  add_exp
    (fun w_54 ->
      assert_env_length w_54 3;
      resize_frame w_54 5 (Memo.from_int 0);
      assert_env_length w_54 5;
      let resolved_55 = resolve w_54 (Source.E 2) in
      let resolved_56 = resolve w_54 (Source.E 0) in
      set_env_slot w_54 1
        (Memo.from_int (if Word.get_value (fst resolved_55) < Word.get_value (fst resolved_56) then 1 else 0));
      let resolved_57 = resolve w_54 (Source.E 1) in
      if Word.get_value (fst resolved_57) <> 0 then (
        assert_env_length w_54 5;
        set_env_slot w_54 3
          (Memo.from_int
             (Word.get_value (Memo.to_word (Memo.from_int 0)) - Word.get_value (Memo.to_word (Memo.from_int 1))));
        trim_resolved w_54 3;
        return_value w_54 (get_env_slot w_54 3) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_54 5;
        let resolved_58 = resolve w_54 (Source.E 2) in
        let resolved_59 = resolve w_54 (Source.E 0) in
        set_env_slot w_54 4
          (Memo.from_int (if Word.get_value (fst resolved_58) > Word.get_value (fst resolved_59) then 1 else 0));
        let resolved_60 = resolve w_54 (Source.E 4) in
        if Word.get_value (fst resolved_60) <> 0 then (
          trim_resolved w_54 3;
          return_value w_54 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
        else (
          trim_resolved w_54 3;
          return_value w_54 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))))
    4;
  add_exp
    (fun w_77 ->
      assert_env_length w_77 3;
      assert_env_length w_77 3;
      let resolved_74 = resolve w_77 (Source.E 2) in
      let resolved_75 = resolve w_77 (Source.E 0) in
      set_env_slot w_77 0
        (Memo.from_int (if Word.get_value (fst resolved_74) = Word.get_value (fst resolved_75) then 1 else 0));
      return_value w_77 (get_env_slot w_77 0) (pc_to_exp (int_to_pc 0)))
    5;
  add_exp
    (fun w_78 ->
      assert_env_length w_78 2;
      resize_frame w_78 22 (Memo.from_int 0);
      assert_env_length w_78 22;
      let resolved_76 = resolve w_78 (Source.E 0) in
      let tag_24 = Word.get_value (fst resolved_76) in
      match tag_24 with
      | 5 (* tag_Const *) ->
          let parts_35 = Memo.splits (snd resolved_76) in
          if List.length parts_35 = 1 then (
            let part0_35 = List.nth parts_35 0 in
            set_env_slot w_78 2 part0_35;
            assert_env_length w_78 22;
            let resolved_77 = resolve w_78 (Source.E 1) in
            let tag_25 = Word.get_value (fst resolved_77) in
            match tag_25 with
            | 5 (* tag_Const *) ->
                let parts_36 = Memo.splits (snd resolved_77) in
                if List.length parts_36 = 1 then (
                  let part0_36 = List.nth parts_36 0 in
                  trim_resolved w_78 2;
                  shuffle_frame w_78 [| NewValue part0_36; Blank; OldSlot 2 |] (Memo.from_int 0);
                  w_78.state.c <- pc_to_exp (int_to_pc 5))
                else (
                  trim_resolved w_78 2;
                  return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
            | _ ->
                trim_resolved w_78 2;
                return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (6)"
      | 6 (* tag_Var *) ->
          let parts_37 = Memo.splits (snd resolved_76) in
          if List.length parts_37 = 1 then (
            let part0_37 = List.nth parts_37 0 in
            set_env_slot w_78 3 part0_37;
            assert_env_length w_78 22;
            let resolved_78 = resolve w_78 (Source.E 1) in
            let tag_26 = Word.get_value (fst resolved_78) in
            match tag_26 with
            | 6 (* tag_Var *) ->
                let parts_38 = Memo.splits (snd resolved_78) in
                if List.length parts_38 = 1 then (
                  let part0_38 = List.nth parts_38 0 in
                  set_env_slot w_78 4 part0_38;
                  let arg0_71 = get_env_slot w_78 3 in
                  assert_env_length w_78 22;
                  w_78.state.k <-
                    Memo.appends [ Memo.from_constructor tag_cont_52; collect_env_slots w_78 [ 4 ]; w_78.state.k ];
                  trim_resolved w_78 2;
                  init_frame w_78 1 (Memo.from_int 0);
                  set_env_slot w_78 0 arg0_71;
                  w_78.state.c <- pc_to_exp (int_to_pc 1))
                else (
                  trim_resolved w_78 2;
                  return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
            | _ ->
                trim_resolved w_78 2;
                return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (6)"
      | 7 (* tag_Add *) ->
          let parts_39 = Memo.splits (snd resolved_76) in
          if List.length parts_39 = 2 then (
            let part0_39 = List.nth parts_39 0 in
            let part1_20 = List.nth parts_39 1 in
            set_env_slot w_78 5 part0_39;
            set_env_slot w_78 6 part1_20;
            assert_env_length w_78 22;
            let resolved_81 = resolve w_78 (Source.E 1) in
            let tag_27 = Word.get_value (fst resolved_81) in
            match tag_27 with
            | 7 (* tag_Add *) ->
                let parts_40 = Memo.splits (snd resolved_81) in
                if List.length parts_40 = 2 then (
                  let part0_40 = List.nth parts_40 0 in
                  let part1_21 = List.nth parts_40 1 in
                  set_env_slot w_78 7 part0_40;
                  set_env_slot w_78 8 part1_21;
                  let arg0_73 = get_env_slot w_78 5 in
                  let arg1_28 = get_env_slot w_78 7 in
                  assert_env_length w_78 22;
                  w_78.state.k <-
                    Memo.appends [ Memo.from_constructor tag_cont_54; collect_env_slots w_78 [ 6; 8 ]; w_78.state.k ];
                  trim_resolved w_78 2;
                  init_frame w_78 2 (Memo.from_int 0);
                  set_env_slot w_78 0 arg0_73;
                  set_env_slot w_78 1 arg1_28;
                  w_78.state.c <- pc_to_exp (int_to_pc 6))
                else (
                  trim_resolved w_78 2;
                  return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
            | _ ->
                trim_resolved w_78 2;
                return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (6)"
      | 8 (* tag_Mul *) ->
          let parts_41 = Memo.splits (snd resolved_76) in
          if List.length parts_41 = 2 then (
            let part0_41 = List.nth parts_41 0 in
            let part1_22 = List.nth parts_41 1 in
            set_env_slot w_78 9 part0_41;
            set_env_slot w_78 10 part1_22;
            assert_env_length w_78 22;
            let resolved_84 = resolve w_78 (Source.E 1) in
            let tag_28 = Word.get_value (fst resolved_84) in
            match tag_28 with
            | 8 (* tag_Mul *) ->
                let parts_42 = Memo.splits (snd resolved_84) in
                if List.length parts_42 = 2 then (
                  let part0_42 = List.nth parts_42 0 in
                  let part1_23 = List.nth parts_42 1 in
                  set_env_slot w_78 11 part0_42;
                  set_env_slot w_78 12 part1_23;
                  let arg0_75 = get_env_slot w_78 9 in
                  let arg1_30 = get_env_slot w_78 11 in
                  assert_env_length w_78 22;
                  w_78.state.k <-
                    Memo.appends [ Memo.from_constructor tag_cont_56; collect_env_slots w_78 [ 10; 12 ]; w_78.state.k ];
                  trim_resolved w_78 2;
                  init_frame w_78 2 (Memo.from_int 0);
                  set_env_slot w_78 0 arg0_75;
                  set_env_slot w_78 1 arg1_30;
                  w_78.state.c <- pc_to_exp (int_to_pc 6))
                else (
                  trim_resolved w_78 2;
                  return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
            | _ ->
                trim_resolved w_78 2;
                return_value w_78 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (6)"
      | _ -> failwith "unreachable (6)")
    6;
  add_exp
    (fun w_145 ->
      assert_env_length w_145 1;
      resize_frame w_145 13 (Memo.from_int 0);
      assert_env_length w_145 13;
      let resolved_131 = resolve w_145 (Source.E 0) in
      let tag_54 = Word.get_value (fst resolved_131) in
      match tag_54 with
      | 5 (* tag_Const *) ->
          let parts_73 = Memo.splits (snd resolved_131) in
          if List.length parts_73 = 1 then (
            let part0_73 = List.nth parts_73 0 in
            trim_resolved w_145 1;
            return_value w_145 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (7)"
      | 6 (* tag_Var *) ->
          let parts_74 = Memo.splits (snd resolved_131) in
          if List.length parts_74 = 1 then (
            let part0_74 = List.nth parts_74 0 in
            trim_resolved w_145 1;
            return_value w_145 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (7)"
      | 7 (* tag_Add *) ->
          let parts_75 = Memo.splits (snd resolved_131) in
          if List.length parts_75 = 2 then (
            let part0_75 = List.nth parts_75 0 in
            let part1_40 = List.nth parts_75 1 in
            set_env_slot w_145 1 part0_75;
            set_env_slot w_145 2 part1_40;
            let arg0_120 = get_env_slot w_145 1 in
            assert_env_length w_145 13;
            w_145.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_95; collect_env_slots w_145 [ 2 ]; w_145.state.k ];
            trim_resolved w_145 1;
            init_frame w_145 1 (Memo.from_int 0);
            set_env_slot w_145 0 arg0_120;
            w_145.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | 8 (* tag_Mul *) ->
          let parts_76 = Memo.splits (snd resolved_131) in
          if List.length parts_76 = 2 then (
            let part0_76 = List.nth parts_76 0 in
            let part1_41 = List.nth parts_76 1 in
            set_env_slot w_145 3 part0_76;
            set_env_slot w_145 4 part1_41;
            let arg0_122 = get_env_slot w_145 3 in
            assert_env_length w_145 13;
            w_145.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_97; collect_env_slots w_145 [ 4 ]; w_145.state.k ];
            trim_resolved w_145 1;
            init_frame w_145 1 (Memo.from_int 0);
            set_env_slot w_145 0 arg0_122;
            w_145.state.c <- pc_to_exp (int_to_pc 7))
          else failwith "unreachable (7)"
      | _ -> failwith "unreachable (7)")
    7;
  add_exp
    (fun w_150 ->
      assert_env_length w_150 2;
      resize_frame w_150 8 (Memo.from_int 0);
      let arg0_124 = get_env_slot w_150 0 in
      assert_env_length w_150 8;
      w_150.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_100; collect_env_slots w_150 [ 0; 1 ]; w_150.state.k ];
      trim_resolved w_150 2;
      init_frame w_150 1 (Memo.from_int 0);
      set_env_slot w_150 0 arg0_124;
      w_150.state.c <- pc_to_exp (int_to_pc 7))
    8;
  add_exp
    (fun w_11 ->
      assert_env_length w_11 2;
      resize_frame w_11 7 (Memo.from_int 0);
      assert_env_length w_11 7;
      let resolved_10 = resolve w_11 (Source.E 0) in
      set_env_slot w_11 2
        (Memo.from_int
           (if Word.get_value (fst resolved_10) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_11 = resolve w_11 (Source.E 2) in
      if Word.get_value (fst resolved_11) <> 0 then (
        assert_env_length w_11 7;
        set_env_slot w_11 3 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
        trim_resolved w_11 2;
        return_value w_11 (get_env_slot w_11 3) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_11 7;
        let resolved_12 = resolve w_11 (Source.E 1) in
        let tag_2 = Word.get_value (fst resolved_12) in
        match tag_2 with
        | 5 (* tag_Const *) ->
            let parts_2 = Memo.splits (snd resolved_12) in
            if List.length parts_2 = 1 then (
              let part0_2 = List.nth parts_2 0 in
              trim_resolved w_11 2;
              shuffle_frame w_11 [| OldSlot 0; NewValue part0_2 |] (Memo.from_int 0);
              w_11.state.c <- pc_to_exp (int_to_pc 10))
            else (
              assert_env_length w_11 7;
              let resolved_13 = resolve w_11 (Source.E 0) in
              set_env_slot w_11 4
                (Memo.from_int
                   (if Word.get_value (fst resolved_13) = Word.get_value (Memo.to_word (Memo.from_int 1)) then 1 else 0));
              let resolved_14 = resolve w_11 (Source.E 4) in
              if Word.get_value (fst resolved_14) <> 0 then (
                trim_resolved w_11 2;
                return_value w_11 (get_env_slot w_11 1) (pc_to_exp (int_to_pc 0)))
              else (
                assert_env_length w_11 7;
                set_env_slot w_11 5 (Memo.appends [ Memo.from_constructor tag_Const; get_env_slot w_11 0 ]);
                assert_env_length w_11 7;
                set_env_slot w_11 6
                  (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_11 5; get_env_slot w_11 1 ]);
                trim_resolved w_11 2;
                return_value w_11 (get_env_slot w_11 6) (pc_to_exp (int_to_pc 0))))
        | _ ->
            assert_env_length w_11 7;
            let resolved_13 = resolve w_11 (Source.E 0) in
            set_env_slot w_11 4
              (Memo.from_int
                 (if Word.get_value (fst resolved_13) = Word.get_value (Memo.to_word (Memo.from_int 1)) then 1 else 0));
            let resolved_14 = resolve w_11 (Source.E 4) in
            if Word.get_value (fst resolved_14) <> 0 then (
              trim_resolved w_11 2;
              return_value w_11 (get_env_slot w_11 1) (pc_to_exp (int_to_pc 0)))
            else (
              assert_env_length w_11 7;
              set_env_slot w_11 5 (Memo.appends [ Memo.from_constructor tag_Const; get_env_slot w_11 0 ]);
              assert_env_length w_11 7;
              set_env_slot w_11 6
                (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_11 5; get_env_slot w_11 1 ]);
              trim_resolved w_11 2;
              return_value w_11 (get_env_slot w_11 6) (pc_to_exp (int_to_pc 0)))))
    9;
  add_exp
    (fun w_12 ->
      assert_env_length w_12 2;
      assert_env_length w_12 2;
      let resolved_15 = resolve w_12 (Source.E 0) in
      let resolved_16 = resolve w_12 (Source.E 1) in
      set_env_slot w_12 0 (Memo.from_int (Word.get_value (fst resolved_15) * Word.get_value (fst resolved_16)));
      assert_env_length w_12 2;
      set_env_slot w_12 0 (Memo.appends [ Memo.from_constructor tag_Const; get_env_slot w_12 0 ]);
      return_value w_12 (get_env_slot w_12 0) (pc_to_exp (int_to_pc 0)))
    10;
  add_exp
    (fun w_17 ->
      assert_env_length w_17 1;
      resize_frame w_17 2 (Memo.from_int 0);
      assert_env_length w_17 2;
      let resolved_19 = resolve w_17 (Source.E 0) in
      let tag_5 = Word.get_value (fst resolved_19) in
      match tag_5 with
      | 5 (* tag_Const *) ->
          let parts_8 = Memo.splits (snd resolved_19) in
          if List.length parts_8 = 1 then (
            let part0_8 = List.nth parts_8 0 in
            set_env_slot w_17 1 part0_8;
            trim_resolved w_17 1;
            return_value w_17 (get_env_slot w_17 1) (pc_to_exp (int_to_pc 0)))
          else (
            trim_resolved w_17 1;
            return_value w_17 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_17 1;
          return_value w_17 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    11;
  add_exp
    (fun w_18 ->
      assert_env_length w_18 1;
      resize_frame w_18 2 (Memo.from_int 0);
      assert_env_length w_18 2;
      let resolved_20 = resolve w_18 (Source.E 0) in
      let tag_6 = Word.get_value (fst resolved_20) in
      match tag_6 with
      | 5 (* tag_Const *) ->
          let parts_9 = Memo.splits (snd resolved_20) in
          if List.length parts_9 = 1 then (
            let part0_9 = List.nth parts_9 0 in
            set_env_slot w_18 1 part0_9;
            trim_resolved w_18 1;
            return_value w_18 (get_env_slot w_18 1) (pc_to_exp (int_to_pc 0)))
          else (
            trim_resolved w_18 1;
            return_value w_18 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
      | 8 (* tag_Mul *) ->
          let parts_10 = Memo.splits (snd resolved_20) in
          if List.length parts_10 = 2 then (
            let part0_10 = List.nth parts_10 0 in
            let part1_5 = List.nth parts_10 1 in
            trim_resolved w_18 1;
            shuffle_frame w_18 [| NewValue part0_10 |] (Memo.from_int 0);
            w_18.state.c <- pc_to_exp (int_to_pc 11))
          else (
            trim_resolved w_18 1;
            return_value w_18 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_18 1;
          return_value w_18 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    12;
  add_exp
    (fun w_102 ->
      assert_env_length w_102 3;
      assert_env_length w_102 3;
      let resolved_99 = resolve w_102 (Source.E 1) in
      let tag_37 = Word.get_value (fst resolved_99) in
      match tag_37 with
      | 5 (* tag_Const *) ->
          let parts_54 = Memo.splits (snd resolved_99) in
          if List.length parts_54 = 1 then
            let part0_54 = List.nth parts_54 0 in
            return_value w_102 (get_env_slot w_102 2) (pc_to_exp (int_to_pc 0))
          else return_value w_102 (get_env_slot w_102 0) (pc_to_exp (int_to_pc 0))
      | _ -> return_value w_102 (get_env_slot w_102 0) (pc_to_exp (int_to_pc 0)))
    13;
  add_exp
    (fun w_103 ->
      assert_env_length w_103 1;
      resize_frame w_103 2 (Memo.from_int 0);
      assert_env_length w_103 2;
      let resolved_100 = resolve w_103 (Source.E 0) in
      let tag_38 = Word.get_value (fst resolved_100) in
      match tag_38 with
      | 5 (* tag_Const *) ->
          let parts_55 = Memo.splits (snd resolved_100) in
          if List.length parts_55 = 1 then (
            let part0_55 = List.nth parts_55 0 in
            assert_env_length w_103 2;
            set_env_slot w_103 1 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 1 ]);
            trim_resolved w_103 1;
            return_value w_103 (get_env_slot w_103 1) (pc_to_exp (int_to_pc 0)))
          else (
            trim_resolved w_103 1;
            return_value w_103 (get_env_slot w_103 0) (pc_to_exp (int_to_pc 0)))
      | 8 (* tag_Mul *) ->
          let parts_56 = Memo.splits (snd resolved_100) in
          if List.length parts_56 = 2 then (
            let part0_56 = List.nth parts_56 0 in
            let part1_29 = List.nth parts_56 1 in
            trim_resolved w_103 1;
            shuffle_frame w_103 [| OldSlot 0; NewValue part0_56; NewValue part1_29 |] (Memo.from_int 0);
            w_103.state.c <- pc_to_exp (int_to_pc 13))
          else (
            trim_resolved w_103 1;
            return_value w_103 (get_env_slot w_103 0) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_103 1;
          return_value w_103 (get_env_slot w_103 0) (pc_to_exp (int_to_pc 0)))
    14;
  add_exp
    (fun w_132 ->
      assert_env_length w_132 2;
      resize_frame w_132 15 (Memo.from_int 0);
      let arg0_113 = get_env_slot w_132 0 in
      let arg1_51 = get_env_slot w_132 1 in
      assert_env_length w_132 15;
      w_132.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_89; collect_env_slots w_132 [ 0; 1 ]; w_132.state.k ];
      trim_resolved w_132 2;
      init_frame w_132 2 (Memo.from_int 0);
      set_env_slot w_132 0 arg0_113;
      set_env_slot w_132 1 arg1_51;
      w_132.state.c <- pc_to_exp (int_to_pc 6))
    15;
  add_exp
    (fun w_93 ->
      assert_env_length w_93 2;
      resize_frame w_93 14 (Memo.from_int 0);
      assert_env_length w_93 14;
      let resolved_95 = resolve w_93 (Source.E 0) in
      let tag_33 = Word.get_value (fst resolved_95) in
      match tag_33 with
      | 8 (* tag_Mul *) ->
          let parts_50 = Memo.splits (snd resolved_95) in
          if List.length parts_50 = 2 then (
            let part0_50 = List.nth parts_50 0 in
            let part1_27 = List.nth parts_50 1 in
            set_env_slot w_93 2 part0_50;
            set_env_slot w_93 3 part1_27;
            let arg0_81 = get_env_slot w_93 2 in
            let arg1_36 = get_env_slot w_93 1 in
            assert_env_length w_93 14;
            w_93.state.k <-
              Memo.appends
                [ Memo.from_constructor tag_cont_62; collect_env_slots w_93 [ 0; 1; 2; 3; 4; 5 ]; w_93.state.k ];
            trim_resolved w_93 2;
            init_frame w_93 2 (Memo.from_int 0);
            set_env_slot w_93 0 arg0_81;
            set_env_slot w_93 1 arg1_36;
            w_93.state.c <- pc_to_exp (int_to_pc 15))
          else (
            assert_env_length w_93 14;
            set_env_slot w_93 13
              (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_93 0; get_env_slot w_93 1 ]);
            trim_resolved w_93 2;
            return_value w_93 (get_env_slot w_93 13) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_93 14;
          set_env_slot w_93 13
            (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_93 0; get_env_slot w_93 1 ]);
          trim_resolved w_93 2;
          return_value w_93 (get_env_slot w_93 13) (pc_to_exp (int_to_pc 0)))
    16;
  add_exp
    (fun w_130 ->
      assert_env_length w_130 2;
      resize_frame w_130 6 (Memo.from_int 0);
      assert_env_length w_130 6;
      let resolved_121 = resolve w_130 (Source.E 0) in
      let tag_45 = Word.get_value (fst resolved_121) in
      match tag_45 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_130 2;
          return_value w_130 (get_env_slot w_130 1) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_62 = Memo.splits (snd resolved_121) in
          if List.length parts_62 = 2 then (
            let part0_62 = List.nth parts_62 0 in
            let part1_35 = List.nth parts_62 1 in
            set_env_slot w_130 2 part0_62;
            set_env_slot w_130 3 part1_35;
            let arg0_112 = get_env_slot w_130 3 in
            let arg1_50 = get_env_slot w_130 1 in
            assert_env_length w_130 6;
            w_130.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_86; collect_env_slots w_130 [ 2 ]; w_130.state.k ];
            trim_resolved w_130 2;
            init_frame w_130 2 (Memo.from_int 0);
            set_env_slot w_130 0 arg0_112;
            set_env_slot w_130 1 arg1_50;
            w_130.state.c <- pc_to_exp (int_to_pc 17))
          else failwith "unreachable (17)"
      | _ -> failwith "unreachable (17)")
    17;
  add_exp
    (fun w_172 ->
      assert_env_length w_172 2;
      resize_frame w_172 10 (Memo.from_int 0);
      assert_env_length w_172 10;
      let resolved_152 = resolve w_172 (Source.E 1) in
      let tag_59 = Word.get_value (fst resolved_152) in
      match tag_59 with
      | 11 (* tag_ENil *) ->
          assert_env_length w_172 10;
          set_env_slot w_172 4
            (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_172 0; Memo.from_constructor tag_ENil ]);
          trim_resolved w_172 2;
          return_value w_172 (get_env_slot w_172 4) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_81 = Memo.splits (snd resolved_152) in
          if List.length parts_81 = 2 then (
            let part0_81 = List.nth parts_81 0 in
            let part1_45 = List.nth parts_81 1 in
            set_env_slot w_172 2 part0_81;
            set_env_slot w_172 3 part1_45;
            let arg0_144 = get_env_slot w_172 0 in
            let arg1_71 = get_env_slot w_172 2 in
            assert_env_length w_172 10;
            w_172.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_117; collect_env_slots w_172 [ 0; 1; 2; 3 ]; w_172.state.k ];
            trim_resolved w_172 2;
            init_frame w_172 2 (Memo.from_int 0);
            set_env_slot w_172 0 arg0_144;
            set_env_slot w_172 1 arg1_71;
            w_172.state.c <- pc_to_exp (int_to_pc 3))
          else failwith "unreachable (18)"
      | _ -> failwith "unreachable (18)")
    18;
  add_exp
    (fun w_96 ->
      assert_env_length w_96 1;
      resize_frame w_96 5 (Memo.from_int 0);
      assert_env_length w_96 5;
      let resolved_98 = resolve w_96 (Source.E 0) in
      let tag_36 = Word.get_value (fst resolved_98) in
      match tag_36 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_96 1;
          return_value w_96 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_53 = Memo.splits (snd resolved_98) in
          if List.length parts_53 = 2 then (
            let part0_53 = List.nth parts_53 0 in
            let part1_28 = List.nth parts_53 1 in
            set_env_slot w_96 1 part0_53;
            set_env_slot w_96 2 part1_28;
            let arg0_83 = get_env_slot w_96 2 in
            assert_env_length w_96 5;
            w_96.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_63; collect_env_slots w_96 [ 1 ]; w_96.state.k ];
            trim_resolved w_96 1;
            init_frame w_96 1 (Memo.from_int 0);
            set_env_slot w_96 0 arg0_83;
            w_96.state.c <- pc_to_exp (int_to_pc 19))
          else failwith "unreachable (19)"
      | _ -> failwith "unreachable (19)")
    19;
  add_exp
    (fun w_5 ->
      assert_env_length w_5 2;
      resize_frame w_5 10 (Memo.from_int 0);
      let arg0_5 = get_env_slot w_5 0 in
      assert_env_length w_5 10;
      w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; collect_env_slots w_5 [ 0; 1 ]; w_5.state.k ];
      trim_resolved w_5 2;
      init_frame w_5 1 (Memo.from_int 0);
      set_env_slot w_5 0 arg0_5;
      w_5.state.c <- pc_to_exp (int_to_pc 14))
    20;
  add_exp
    (fun w_59 ->
      assert_env_length w_59 2;
      resize_frame w_59 10 (Memo.from_int 0);
      assert_env_length w_59 10;
      let resolved_63 = resolve w_59 (Source.E 1) in
      let tag_19 = Word.get_value (fst resolved_63) in
      match tag_19 with
      | 11 (* tag_ENil *) ->
          assert_env_length w_59 10;
          set_env_slot w_59 4
            (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_59 0; Memo.from_constructor tag_ENil ]);
          trim_resolved w_59 2;
          return_value w_59 (get_env_slot w_59 4) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_30 = Memo.splits (snd resolved_63) in
          if List.length parts_30 = 2 then (
            let part0_30 = List.nth parts_30 0 in
            let part1_16 = List.nth parts_30 1 in
            set_env_slot w_59 2 part0_30;
            set_env_slot w_59 3 part1_16;
            let arg0_54 = get_env_slot w_59 0 in
            let arg1_20 = get_env_slot w_59 2 in
            assert_env_length w_59 10;
            w_59.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_40; collect_env_slots w_59 [ 0; 1; 2; 3 ]; w_59.state.k ];
            trim_resolved w_59 2;
            init_frame w_59 2 (Memo.from_int 0);
            set_env_slot w_59 0 arg0_54;
            set_env_slot w_59 1 arg1_20;
            w_59.state.c <- pc_to_exp (int_to_pc 20))
          else failwith "unreachable (21)"
      | _ -> failwith "unreachable (21)")
    21;
  add_exp
    (fun w_45 ->
      assert_env_length w_45 1;
      resize_frame w_45 5 (Memo.from_int 0);
      assert_env_length w_45 5;
      let resolved_33 = resolve w_45 (Source.E 0) in
      let tag_11 = Word.get_value (fst resolved_33) in
      match tag_11 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_45 1;
          return_value w_45 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_19 = Memo.splits (snd resolved_33) in
          if List.length parts_19 = 2 then (
            let part0_19 = List.nth parts_19 0 in
            let part1_9 = List.nth parts_19 1 in
            set_env_slot w_45 1 part0_19;
            set_env_slot w_45 2 part1_9;
            let arg0_40 = get_env_slot w_45 2 in
            assert_env_length w_45 5;
            w_45.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_29; collect_env_slots w_45 [ 1 ]; w_45.state.k ];
            trim_resolved w_45 1;
            init_frame w_45 1 (Memo.from_int 0);
            set_env_slot w_45 0 arg0_40;
            w_45.state.c <- pc_to_exp (int_to_pc 22))
          else failwith "unreachable (22)"
      | _ -> failwith "unreachable (22)")
    22;
  add_exp
    (fun w_4 ->
      assert_env_length w_4 2;
      resize_frame w_4 6 (Memo.from_int 0);
      assert_env_length w_4 6;
      let resolved_1 = resolve w_4 (Source.E 0) in
      let tag_1 = Word.get_value (fst resolved_1) in
      match tag_1 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_4 2;
          return_value w_4 (get_env_slot w_4 1) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_1 = Memo.splits (snd resolved_1) in
          if List.length parts_1 = 2 then (
            let part0_1 = List.nth parts_1 0 in
            let part1_1 = List.nth parts_1 1 in
            set_env_slot w_4 2 part0_1;
            set_env_slot w_4 3 part1_1;
            assert_env_length w_4 6;
            set_env_slot w_4 4
              (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_4 2; get_env_slot w_4 1 ]);
            let arg0_4 = get_env_slot w_4 3 in
            let arg1_2 = get_env_slot w_4 4 in
            assert_env_length w_4 6;
            trim_resolved w_4 2;
            init_frame w_4 2 (Memo.from_int 0);
            set_env_slot w_4 0 arg0_4;
            set_env_slot w_4 1 arg1_2;
            w_4.state.c <- pc_to_exp (int_to_pc 23))
          else failwith "unreachable (23)"
      | _ -> failwith "unreachable (23)")
    23;
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      let arg0_3 = get_env_slot w_3 0 in
      let arg1_1 = Memo.from_constructor tag_ENil in
      assert_env_length w_3 1;
      init_frame w_3 2 (Memo.from_int 0);
      set_env_slot w_3 0 arg0_3;
      set_env_slot w_3 1 arg1_1;
      w_3.state.c <- pc_to_exp (int_to_pc 23))
    24;
  add_exp
    (fun w_19 ->
      assert_env_length w_19 2;
      resize_frame w_19 3 (Memo.from_int 0);
      assert_env_length w_19 3;
      let resolved_21 = resolve w_19 (Source.E 1) in
      set_env_slot w_19 1
        (Memo.from_int
           (if Word.get_value (fst resolved_21) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_22 = resolve w_19 (Source.E 1) in
      if Word.get_value (fst resolved_22) <> 0 then (
        trim_resolved w_19 2;
        return_value w_19 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_19 3;
        set_env_slot w_19 2
          (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_19 0; Memo.from_constructor tag_ENil ]);
        trim_resolved w_19 2;
        return_value w_19 (get_env_slot w_19 2) (pc_to_exp (int_to_pc 0))))
    25;
  add_exp
    (fun w_20 ->
      assert_env_length w_20 1;
      resize_frame w_20 7 (Memo.from_int 0);
      assert_env_length w_20 7;
      let resolved_23 = resolve w_20 (Source.E 0) in
      let tag_7 = Word.get_value (fst resolved_23) in
      match tag_7 with
      | 7 (* tag_Add *) ->
          let parts_11 = Memo.splits (snd resolved_23) in
          if List.length parts_11 = 2 then (
            let part0_11 = List.nth parts_11 0 in
            let part1_6 = List.nth parts_11 1 in
            set_env_slot w_20 1 part0_11;
            set_env_slot w_20 2 part1_6;
            let arg0_14 = get_env_slot w_20 1 in
            assert_env_length w_20 7;
            w_20.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_10; collect_env_slots w_20 [ 2 ]; w_20.state.k ];
            trim_resolved w_20 1;
            init_frame w_20 1 (Memo.from_int 0);
            set_env_slot w_20 0 arg0_14;
            w_20.state.c <- pc_to_exp (int_to_pc 26))
          else (
            assert_env_length w_20 7;
            set_env_slot w_20 6
              (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_20 0; Memo.from_constructor tag_ENil ]);
            trim_resolved w_20 1;
            return_value w_20 (get_env_slot w_20 6) (pc_to_exp (int_to_pc 0)))
      | 5 (* tag_Const *) ->
          let parts_12 = Memo.splits (snd resolved_23) in
          if List.length parts_12 = 1 then (
            let part0_12 = List.nth parts_12 0 in
            trim_resolved w_20 1;
            shuffle_frame w_20 [| OldSlot 0; NewValue part0_12 |] (Memo.from_int 0);
            w_20.state.c <- pc_to_exp (int_to_pc 25))
          else (
            assert_env_length w_20 7;
            set_env_slot w_20 6
              (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_20 0; Memo.from_constructor tag_ENil ]);
            trim_resolved w_20 1;
            return_value w_20 (get_env_slot w_20 6) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_20 7;
          set_env_slot w_20 6
            (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_20 0; Memo.from_constructor tag_ENil ]);
          trim_resolved w_20 1;
          return_value w_20 (get_env_slot w_20 6) (pc_to_exp (int_to_pc 0)))
    26;
  add_exp
    (fun w_14 ->
      assert_env_length w_14 1;
      resize_frame w_14 7 (Memo.from_int 0);
      assert_env_length w_14 7;
      let resolved_18 = resolve w_14 (Source.E 0) in
      let tag_4 = Word.get_value (fst resolved_18) in
      match tag_4 with
      | 8 (* tag_Mul *) ->
          let parts_7 = Memo.splits (snd resolved_18) in
          if List.length parts_7 = 2 then (
            let part0_7 = List.nth parts_7 0 in
            let part1_4 = List.nth parts_7 1 in
            set_env_slot w_14 1 part0_7;
            set_env_slot w_14 2 part1_4;
            let arg0_11 = get_env_slot w_14 1 in
            assert_env_length w_14 7;
            w_14.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_8; collect_env_slots w_14 [ 2 ]; w_14.state.k ];
            trim_resolved w_14 1;
            init_frame w_14 1 (Memo.from_int 0);
            set_env_slot w_14 0 arg0_11;
            w_14.state.c <- pc_to_exp (int_to_pc 27))
          else (
            assert_env_length w_14 7;
            set_env_slot w_14 6
              (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_14 0; Memo.from_constructor tag_ENil ]);
            trim_resolved w_14 1;
            return_value w_14 (get_env_slot w_14 6) (pc_to_exp (int_to_pc 0)))
      | _ ->
          assert_env_length w_14 7;
          set_env_slot w_14 6
            (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_14 0; Memo.from_constructor tag_ENil ]);
          trim_resolved w_14 1;
          return_value w_14 (get_env_slot w_14 6) (pc_to_exp (int_to_pc 0)))
    27;
  add_exp
    (fun w_142 ->
      assert_env_length w_142 1;
      resize_frame w_142 2 (Memo.from_int 0);
      assert_env_length w_142 2;
      let resolved_128 = resolve w_142 (Source.E 0) in
      let tag_51 = Word.get_value (fst resolved_128) in
      match tag_51 with
      | 5 (* tag_Const *) ->
          let parts_70 = Memo.splits (snd resolved_128) in
          if List.length parts_70 = 1 then (
            let part0_70 = List.nth parts_70 0 in
            set_env_slot w_142 1 part0_70;
            trim_resolved w_142 1;
            return_value w_142 (get_env_slot w_142 1) (pc_to_exp (int_to_pc 0)))
          else (
            trim_resolved w_142 1;
            return_value w_142 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_142 1;
          return_value w_142 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    28;
  add_exp
    (fun w_143 ->
      assert_env_length w_143 1;
      resize_frame w_143 2 (Memo.from_int 0);
      assert_env_length w_143 2;
      let resolved_129 = resolve w_143 (Source.E 0) in
      let tag_52 = Word.get_value (fst resolved_129) in
      match tag_52 with
      | 5 (* tag_Const *) ->
          let parts_71 = Memo.splits (snd resolved_129) in
          if List.length parts_71 = 1 then (
            let part0_71 = List.nth parts_71 0 in
            set_env_slot w_143 1 part0_71;
            trim_resolved w_143 1;
            return_value w_143 (get_env_slot w_143 1) (pc_to_exp (int_to_pc 0)))
          else (
            trim_resolved w_143 1;
            return_value w_143 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
      | 8 (* tag_Mul *) ->
          let parts_72 = Memo.splits (snd resolved_129) in
          if List.length parts_72 = 2 then (
            let part0_72 = List.nth parts_72 0 in
            let part1_39 = List.nth parts_72 1 in
            trim_resolved w_143 1;
            shuffle_frame w_143 [| NewValue part0_72 |] (Memo.from_int 0);
            w_143.state.c <- pc_to_exp (int_to_pc 28))
          else (
            trim_resolved w_143 1;
            return_value w_143 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_143 1;
          return_value w_143 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
    29;
  add_exp
    (fun w_91 ->
      assert_env_length w_91 3;
      assert_env_length w_91 3;
      let resolved_93 = resolve w_91 (Source.E 1) in
      let tag_31 = Word.get_value (fst resolved_93) in
      match tag_31 with
      | 5 (* tag_Const *) ->
          let parts_47 = Memo.splits (snd resolved_93) in
          if List.length parts_47 = 1 then
            let part0_47 = List.nth parts_47 0 in
            return_value w_91 (get_env_slot w_91 2) (pc_to_exp (int_to_pc 0))
          else return_value w_91 (get_env_slot w_91 0) (pc_to_exp (int_to_pc 0))
      | _ -> return_value w_91 (get_env_slot w_91 0) (pc_to_exp (int_to_pc 0)))
    30;
  add_exp
    (fun w_92 ->
      assert_env_length w_92 1;
      resize_frame w_92 2 (Memo.from_int 0);
      assert_env_length w_92 2;
      let resolved_94 = resolve w_92 (Source.E 0) in
      let tag_32 = Word.get_value (fst resolved_94) in
      match tag_32 with
      | 5 (* tag_Const *) ->
          let parts_48 = Memo.splits (snd resolved_94) in
          if List.length parts_48 = 1 then (
            let part0_48 = List.nth parts_48 0 in
            assert_env_length w_92 2;
            set_env_slot w_92 1 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 1 ]);
            trim_resolved w_92 1;
            return_value w_92 (get_env_slot w_92 1) (pc_to_exp (int_to_pc 0)))
          else (
            trim_resolved w_92 1;
            return_value w_92 (get_env_slot w_92 0) (pc_to_exp (int_to_pc 0)))
      | 8 (* tag_Mul *) ->
          let parts_49 = Memo.splits (snd resolved_94) in
          if List.length parts_49 = 2 then (
            let part0_49 = List.nth parts_49 0 in
            let part1_26 = List.nth parts_49 1 in
            trim_resolved w_92 1;
            shuffle_frame w_92 [| OldSlot 0; NewValue part0_49; NewValue part1_26 |] (Memo.from_int 0);
            w_92.state.c <- pc_to_exp (int_to_pc 30))
          else (
            trim_resolved w_92 1;
            return_value w_92 (get_env_slot w_92 0) (pc_to_exp (int_to_pc 0)))
      | _ ->
          trim_resolved w_92 1;
          return_value w_92 (get_env_slot w_92 0) (pc_to_exp (int_to_pc 0)))
    31;
  add_exp
    (fun w_111 ->
      assert_env_length w_111 1;
      resize_frame w_111 6 (Memo.from_int 0);
      assert_env_length w_111 6;
      let resolved_105 = resolve w_111 (Source.E 0) in
      let tag_39 = Word.get_value (fst resolved_105) in
      match tag_39 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_111 1;
          return_value w_111 (Memo.from_int 1) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_57 = Memo.splits (snd resolved_105) in
          if List.length parts_57 = 2 then (
            let part0_57 = List.nth parts_57 0 in
            let part1_30 = List.nth parts_57 1 in
            set_env_slot w_111 1 part0_57;
            set_env_slot w_111 2 part1_30;
            let arg0_96 = get_env_slot w_111 1 in
            assert_env_length w_111 6;
            w_111.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_74; collect_env_slots w_111 [ 2 ]; w_111.state.k ];
            trim_resolved w_111 1;
            init_frame w_111 1 (Memo.from_int 0);
            set_env_slot w_111 0 arg0_96;
            w_111.state.c <- pc_to_exp (int_to_pc 29))
          else failwith "unreachable (32)"
      | _ -> failwith "unreachable (32)")
    32;
  add_exp
    (fun w_72 ->
      assert_env_length w_72 4;
      resize_frame w_72 6 (Memo.from_int 0);
      assert_env_length w_72 6;
      let resolved_70 = resolve w_72 (Source.E 0) in
      set_env_slot w_72 0
        (Memo.from_int
           (if Word.get_value (fst resolved_70) = Word.get_value (Memo.to_word (Memo.from_int 1)) then 1 else 0));
      let resolved_71 = resolve w_72 (Source.E 0) in
      if Word.get_value (fst resolved_71) <> 0 then (
        let arg0_67 = get_env_slot w_72 2 in
        assert_env_length w_72 6;
        trim_resolved w_72 4;
        init_frame w_72 1 (Memo.from_int 0);
        set_env_slot w_72 0 arg0_67;
        w_72.state.c <- pc_to_exp (int_to_pc 34))
      else
        let arg0_65 = get_env_slot w_72 2 in
        assert_env_length w_72 6;
        w_72.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; collect_env_slots w_72 [ 3 ]; w_72.state.k ];
        trim_resolved w_72 4;
        init_frame w_72 1 (Memo.from_int 0);
        set_env_slot w_72 0 arg0_65;
        w_72.state.c <- pc_to_exp (int_to_pc 34))
    33;
  add_exp
    (fun w_74 ->
      assert_env_length w_74 1;
      resize_frame w_74 6 (Memo.from_int 0);
      assert_env_length w_74 6;
      let resolved_72 = resolve w_74 (Source.E 0) in
      let tag_22 = Word.get_value (fst resolved_72) in
      match tag_22 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_74 1;
          return_value w_74 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_33 = Memo.splits (snd resolved_72) in
          if List.length parts_33 = 2 then (
            let part0_33 = List.nth parts_33 0 in
            let part1_19 = List.nth parts_33 1 in
            set_env_slot w_74 1 part0_33;
            set_env_slot w_74 2 part1_19;
            let arg0_68 = get_env_slot w_74 1 in
            assert_env_length w_74 6;
            w_74.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_50; collect_env_slots w_74 [ 2 ]; w_74.state.k ];
            trim_resolved w_74 1;
            init_frame w_74 1 (Memo.from_int 0);
            set_env_slot w_74 0 arg0_68;
            w_74.state.c <- pc_to_exp (int_to_pc 31))
          else failwith "unreachable (34)"
      | _ -> failwith "unreachable (34)")
    34;
  add_exp
    (fun w_169 ->
      assert_env_length w_169 2;
      resize_frame w_169 4 (Memo.from_int 0);
      assert_env_length w_169 4;
      let resolved_150 = resolve w_169 (Source.E 1) in
      let tag_57 = Word.get_value (fst resolved_150) in
      match tag_57 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_169 2;
          return_value w_169 (get_env_slot w_169 0) (pc_to_exp (int_to_pc 0))
      | _ ->
          let arg0_143 = get_env_slot w_169 1 in
          assert_env_length w_169 4;
          w_169.state.k <-
            Memo.appends [ Memo.from_constructor tag_cont_115; collect_env_slots w_169 [ 0 ]; w_169.state.k ];
          trim_resolved w_169 2;
          init_frame w_169 1 (Memo.from_int 0);
          set_env_slot w_169 0 arg0_143;
          w_169.state.c <- pc_to_exp (int_to_pc 36))
    35;
  add_exp
    (fun w_171 ->
      assert_env_length w_171 1;
      resize_frame w_171 2 (Memo.from_int 0);
      assert_env_length w_171 2;
      let resolved_151 = resolve w_171 (Source.E 0) in
      let tag_58 = Word.get_value (fst resolved_151) in
      match tag_58 with
      | 11 (* tag_ENil *) ->
          assert_env_length w_171 2;
          set_env_slot w_171 1 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 1 ]);
          trim_resolved w_171 1;
          return_value w_171 (get_env_slot w_171 1) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_80 = Memo.splits (snd resolved_151) in
          if List.length parts_80 = 2 then (
            let part0_80 = List.nth parts_80 0 in
            let part1_44 = List.nth parts_80 1 in
            trim_resolved w_171 1;
            shuffle_frame w_171 [| NewValue part0_80; NewValue part1_44 |] (Memo.from_int 0);
            w_171.state.c <- pc_to_exp (int_to_pc 35))
          else failwith "unreachable (36)"
      | _ -> failwith "unreachable (36)")
    36;
  add_exp
    (fun w_104 ->
      assert_env_length w_104 2;
      resize_frame w_104 8 (Memo.from_int 0);
      let arg0_89 = get_env_slot w_104 1 in
      assert_env_length w_104 8;
      w_104.state.k <- Memo.appends [ Memo.from_constructor tag_cont_72; collect_env_slots w_104 [ 0 ]; w_104.state.k ];
      trim_resolved w_104 2;
      init_frame w_104 1 (Memo.from_int 0);
      set_env_slot w_104 0 arg0_89;
      w_104.state.c <- pc_to_exp (int_to_pc 27))
    37;
  add_exp
    (fun w_123 ->
      assert_env_length w_123 3;
      resize_frame w_123 17 (Memo.from_int 0);
      assert_env_length w_123 17;
      let resolved_113 = resolve w_123 (Source.E 2) in
      let tag_44 = Word.get_value (fst resolved_113) in
      match tag_44 with
      | 11 (* tag_ENil *) ->
          assert_env_length w_123 17;
          let resolved_114 = resolve w_123 (Source.E 1) in
          set_env_slot w_123 5
            (Memo.from_int
               (if Word.get_value (fst resolved_114) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
          let resolved_115 = resolve w_123 (Source.E 5) in
          if Word.get_value (fst resolved_115) <> 0 then (
            trim_resolved w_123 3;
            return_value w_123 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0)))
          else
            let arg0_104 = get_env_slot w_123 1 in
            let arg1_44 = get_env_slot w_123 0 in
            assert_env_length w_123 17;
            w_123.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_80; collect_env_slots w_123 []; w_123.state.k ];
            trim_resolved w_123 3;
            init_frame w_123 2 (Memo.from_int 0);
            set_env_slot w_123 0 arg0_104;
            set_env_slot w_123 1 arg1_44;
            w_123.state.c <- pc_to_exp (int_to_pc 9)
      | 12 (* tag_ECons *) ->
          let parts_61 = Memo.splits (snd resolved_113) in
          if List.length parts_61 = 2 then (
            let part0_61 = List.nth parts_61 0 in
            let part1_34 = List.nth parts_61 1 in
            set_env_slot w_123 3 part0_61;
            set_env_slot w_123 4 part1_34;
            let arg0_105 = get_env_slot w_123 3 in
            assert_env_length w_123 17;
            w_123.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_85; collect_env_slots w_123 [ 0; 1; 3; 4 ]; w_123.state.k ];
            trim_resolved w_123 3;
            init_frame w_123 1 (Memo.from_int 0);
            set_env_slot w_123 0 arg0_105;
            w_123.state.c <- pc_to_exp (int_to_pc 14))
          else failwith "unreachable (38)"
      | _ -> failwith "unreachable (38)")
    38;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      resize_frame w_0 6 (Memo.from_int 0);
      assert_env_length w_0 6;
      let resolved_0 = resolve w_0 (Source.E 0) in
      let tag_0 = Word.get_value (fst resolved_0) in
      match tag_0 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_0 1;
          return_value w_0 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_0 = Memo.splits (snd resolved_0) in
          if List.length parts_0 = 2 then (
            let part0_0 = List.nth parts_0 0 in
            let part1_0 = List.nth parts_0 1 in
            set_env_slot w_0 1 part0_0;
            set_env_slot w_0 2 part1_0;
            let arg0_0 = get_env_slot w_0 1 in
            assert_env_length w_0 6;
            w_0.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_1; collect_env_slots w_0 [ 1; 2 ]; w_0.state.k ];
            trim_resolved w_0 1;
            init_frame w_0 1 (Memo.from_int 0);
            set_env_slot w_0 0 arg0_0;
            w_0.state.c <- pc_to_exp (int_to_pc 14))
          else failwith "unreachable (39)"
      | _ -> failwith "unreachable (39)")
    39;
  add_exp
    (fun w_117 ->
      assert_env_length w_117 2;
      resize_frame w_117 12 (Memo.from_int 0);
      assert_env_length w_117 12;
      let resolved_110 = resolve w_117 (Source.E 1) in
      let tag_42 = Word.get_value (fst resolved_110) in
      match tag_42 with
      | 11 (* tag_ENil *) ->
          assert_env_length w_117 12;
          set_env_slot w_117 4
            (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_117 0; Memo.from_constructor tag_ENil ]);
          trim_resolved w_117 2;
          return_value w_117 (get_env_slot w_117 4) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_59 = Memo.splits (snd resolved_110) in
          if List.length parts_59 = 2 then (
            let part0_59 = List.nth parts_59 0 in
            let part1_32 = List.nth parts_59 1 in
            set_env_slot w_117 2 part0_59;
            set_env_slot w_117 3 part1_32;
            let arg0_99 = get_env_slot w_117 0 in
            let arg1_41 = get_env_slot w_117 2 in
            assert_env_length w_117 12;
            w_117.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_79; collect_env_slots w_117 [ 0; 1; 2; 3 ]; w_117.state.k ];
            trim_resolved w_117 2;
            init_frame w_117 2 (Memo.from_int 0);
            set_env_slot w_117 0 arg0_99;
            set_env_slot w_117 1 arg1_41;
            w_117.state.c <- pc_to_exp (int_to_pc 16))
          else failwith "unreachable (40)"
      | _ -> failwith "unreachable (40)")
    40;
  add_exp
    (fun w_122 ->
      assert_env_length w_122 1;
      assert_env_length w_122 1;
      let resolved_112 = resolve w_122 (Source.E 0) in
      let tag_43 = Word.get_value (fst resolved_112) in
      match tag_43 with
      | 11 (* tag_ENil *) -> return_value w_122 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_60 = Memo.splits (snd resolved_112) in
          if List.length parts_60 = 2 then (
            let part0_60 = List.nth parts_60 0 in
            let part1_33 = List.nth parts_60 1 in
            shuffle_frame w_122 [| NewValue part0_60; NewValue part1_33 |] (Memo.from_int 0);
            w_122.state.c <- pc_to_exp (int_to_pc 40))
          else failwith "unreachable (41)"
      | _ -> failwith "unreachable (41)")
    41;
  add_exp
    (fun w_68 ->
      assert_env_length w_68 2;
      resize_frame w_68 13 (Memo.from_int 0);
      assert_env_length w_68 13;
      let resolved_67 = resolve w_68 (Source.E 1) in
      let tag_20 = Word.get_value (fst resolved_67) in
      match tag_20 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_68 2;
          return_value w_68 (Memo.from_constructor tag_NoPick) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_31 = Memo.splits (snd resolved_67) in
          if List.length parts_31 = 2 then (
            let part0_31 = List.nth parts_31 0 in
            let part1_17 = List.nth parts_31 1 in
            set_env_slot w_68 2 part0_31;
            set_env_slot w_68 3 part1_17;
            let arg0_62 = get_env_slot w_68 0 in
            let arg1_23 = get_env_slot w_68 2 in
            assert_env_length w_68 13;
            w_68.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_47; collect_env_slots w_68 [ 0; 2; 3; 4; 5 ]; w_68.state.k ];
            trim_resolved w_68 2;
            init_frame w_68 2 (Memo.from_int 0);
            set_env_slot w_68 0 arg0_62;
            set_env_slot w_68 1 arg1_23;
            w_68.state.c <- pc_to_exp (int_to_pc 16))
          else failwith "unreachable (42)"
      | _ -> failwith "unreachable (42)")
    42;
  add_exp
    (fun w_55 ->
      assert_env_length w_55 1;
      resize_frame w_55 10 (Memo.from_int 0);
      assert_env_length w_55 10;
      let resolved_61 = resolve w_55 (Source.E 0) in
      let tag_17 = Word.get_value (fst resolved_61) in
      match tag_17 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_55 1;
          return_value w_55 (Memo.from_constructor tag_ENil) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_28 = Memo.splits (snd resolved_61) in
          if List.length parts_28 = 2 then (
            let part0_28 = List.nth parts_28 0 in
            let part1_14 = List.nth parts_28 1 in
            set_env_slot w_55 1 part0_28;
            set_env_slot w_55 2 part1_14;
            let arg0_50 = get_env_slot w_55 1 in
            let arg1_18 = get_env_slot w_55 2 in
            assert_env_length w_55 10;
            w_55.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_38; collect_env_slots w_55 [ 1; 2; 3; 4 ]; w_55.state.k ];
            trim_resolved w_55 1;
            init_frame w_55 2 (Memo.from_int 0);
            set_env_slot w_55 0 arg0_50;
            set_env_slot w_55 1 arg1_18;
            w_55.state.c <- pc_to_exp (int_to_pc 42))
          else failwith "unreachable (43)"
      | _ -> failwith "unreachable (43)")
    43;
  add_exp
    (fun w_114 ->
      assert_env_length w_114 2;
      resize_frame w_114 4 (Memo.from_int 0);
      assert_env_length w_114 4;
      let resolved_108 = resolve w_114 (Source.E 1) in
      let tag_40 = Word.get_value (fst resolved_108) in
      match tag_40 with
      | 11 (* tag_ENil *) ->
          trim_resolved w_114 2;
          return_value w_114 (get_env_slot w_114 0) (pc_to_exp (int_to_pc 0))
      | _ ->
          let arg0_98 = get_env_slot w_114 1 in
          assert_env_length w_114 4;
          w_114.state.k <-
            Memo.appends [ Memo.from_constructor tag_cont_75; collect_env_slots w_114 [ 0 ]; w_114.state.k ];
          trim_resolved w_114 2;
          init_frame w_114 1 (Memo.from_int 0);
          set_env_slot w_114 0 arg0_98;
          w_114.state.c <- pc_to_exp (int_to_pc 45))
    44;
  add_exp
    (fun w_116 ->
      assert_env_length w_116 1;
      resize_frame w_116 2 (Memo.from_int 0);
      assert_env_length w_116 2;
      let resolved_109 = resolve w_116 (Source.E 0) in
      let tag_41 = Word.get_value (fst resolved_109) in
      match tag_41 with
      | 11 (* tag_ENil *) ->
          assert_env_length w_116 2;
          set_env_slot w_116 1 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
          trim_resolved w_116 1;
          return_value w_116 (get_env_slot w_116 1) (pc_to_exp (int_to_pc 0))
      | 12 (* tag_ECons *) ->
          let parts_58 = Memo.splits (snd resolved_109) in
          if List.length parts_58 = 2 then (
            let part0_58 = List.nth parts_58 0 in
            let part1_31 = List.nth parts_58 1 in
            trim_resolved w_116 1;
            shuffle_frame w_116 [| NewValue part0_58; NewValue part1_31 |] (Memo.from_int 0);
            w_116.state.c <- pc_to_exp (int_to_pc 44))
          else failwith "unreachable (45)"
      | _ -> failwith "unreachable (45)")
    45;
  add_exp
    (fun w_98 ->
      assert_env_length w_98 1;
      let arg0_85 = get_env_slot w_98 0 in
      assert_env_length w_98 1;
      w_98.state.k <- Memo.appends [ Memo.from_constructor tag_cont_66; collect_env_slots w_98 []; w_98.state.k ];
      init_frame w_98 1 (Memo.from_int 0);
      set_env_slot w_98 0 arg0_85;
      w_98.state.c <- pc_to_exp (int_to_pc 41))
    46;
  add_exp
    (fun w_30 ->
      assert_env_length w_30 2;
      let arg0_25 = get_env_slot w_30 1 in
      assert_env_length w_30 2;
      w_30.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; collect_env_slots w_30 [ 0 ]; w_30.state.k ];
      init_frame w_30 1 (Memo.from_int 0);
      set_env_slot w_30 0 arg0_25;
      w_30.state.c <- pc_to_exp (int_to_pc 26))
    47;
  add_exp
    (fun w_154 ->
      assert_env_length w_154 2;
      resize_frame w_154 23 (Memo.from_int 0);
      assert_env_length w_154 23;
      let resolved_146 = resolve w_154 (Source.E 0) in
      let tag_55 = Word.get_value (fst resolved_146) in
      match tag_55 with
      | 1 (* tag_Z *) ->
          trim_resolved w_154 2;
          return_value w_154 (get_env_slot w_154 1) (pc_to_exp (int_to_pc 0))
      | 2 (* tag_S *) ->
          let parts_77 = Memo.splits (snd resolved_146) in
          if List.length parts_77 = 1 then (
            let part0_77 = List.nth parts_77 0 in
            set_env_slot w_154 2 part0_77;
            assert_env_length w_154 23;
            let resolved_147 = resolve w_154 (Source.E 1) in
            let tag_56 = Word.get_value (fst resolved_147) in
            match tag_56 with
            | 7 (* tag_Add *) ->
                let parts_78 = Memo.splits (snd resolved_147) in
                if List.length parts_78 = 2 then (
                  let part0_78 = List.nth parts_78 0 in
                  let part1_42 = List.nth parts_78 1 in
                  set_env_slot w_154 3 part0_78;
                  set_env_slot w_154 4 part1_42;
                  let arg0_127 = get_env_slot w_154 2 in
                  let arg1_55 = get_env_slot w_154 3 in
                  assert_env_length w_154 23;
                  w_154.state.k <-
                    Memo.appends [ Memo.from_constructor tag_cont_107; collect_env_slots w_154 [ 2; 4 ]; w_154.state.k ];
                  trim_resolved w_154 2;
                  init_frame w_154 2 (Memo.from_int 0);
                  set_env_slot w_154 0 arg0_127;
                  set_env_slot w_154 1 arg1_55;
                  w_154.state.c <- pc_to_exp (int_to_pc 48))
                else (
                  trim_resolved w_154 2;
                  return_value w_154 (get_env_slot w_154 1) (pc_to_exp (int_to_pc 0)))
            | 8 (* tag_Mul *) ->
                let parts_79 = Memo.splits (snd resolved_147) in
                if List.length parts_79 = 2 then (
                  let part0_79 = List.nth parts_79 0 in
                  let part1_43 = List.nth parts_79 1 in
                  set_env_slot w_154 5 part0_79;
                  set_env_slot w_154 6 part1_43;
                  let arg0_135 = get_env_slot w_154 2 in
                  let arg1_63 = get_env_slot w_154 5 in
                  assert_env_length w_154 23;
                  w_154.state.k <-
                    Memo.appends [ Memo.from_constructor tag_cont_114; collect_env_slots w_154 [ 2; 6 ]; w_154.state.k ];
                  trim_resolved w_154 2;
                  init_frame w_154 2 (Memo.from_int 0);
                  set_env_slot w_154 0 arg0_135;
                  set_env_slot w_154 1 arg1_63;
                  w_154.state.c <- pc_to_exp (int_to_pc 48))
                else (
                  trim_resolved w_154 2;
                  return_value w_154 (get_env_slot w_154 1) (pc_to_exp (int_to_pc 0)))
            | _ ->
                trim_resolved w_154 2;
                return_value w_154 (get_env_slot w_154 1) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (48)"
      | _ -> failwith "unreachable (48)")
    48;
  add_exp
    (fun w_23 ->
      assert_env_length w_23 7;
      assert_env_length w_23 7;
      let resolved_24 = resolve w_23 (Source.E 0) in
      set_env_slot w_23 0
        (Memo.from_int
           (if Word.get_value (fst resolved_24) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_25 = resolve w_23 (Source.E 0) in
      if Word.get_value (fst resolved_25) <> 0 then return_value w_23 (get_env_slot w_23 6) (pc_to_exp (int_to_pc 0))
      else
        let arg0_17 = get_env_slot w_23 5 in
        let arg1_7 = get_env_slot w_23 6 in
        assert_env_length w_23 7;
        init_frame w_23 2 (Memo.from_int 0);
        set_env_slot w_23 1 arg0_17;
        set_env_slot w_23 0 arg1_7;
        w_23.state.c <- pc_to_exp (int_to_pc 47))
    49;
  add_exp
    (fun w_24 ->
      assert_env_length w_24 10;
      assert_env_length w_24 10;
      let resolved_26 = resolve w_24 (Source.E 0) in
      set_env_slot w_24 1
        (Memo.from_int
           (if Word.get_value (fst resolved_26) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_27 = resolve w_24 (Source.E 1) in
      if Word.get_value (fst resolved_27) <> 0 then return_value w_24 (get_env_slot w_24 8) (pc_to_exp (int_to_pc 0))
      else (
        assert_env_length w_24 10;
        let resolved_28 = resolve w_24 (Source.E 0) in
        set_env_slot w_24 2
          (Memo.from_int
             (if Word.get_value (fst resolved_28) = Word.get_value (Memo.to_word (Memo.from_int 1)) then 1 else 0));
        let resolved_29 = resolve w_24 (Source.E 2) in
        if Word.get_value (fst resolved_29) <> 0 then return_value w_24 (get_env_slot w_24 9) (pc_to_exp (int_to_pc 0))
        else
          let arg0_18 = get_env_slot w_24 8 in
          let arg1_8 = get_env_slot w_24 9 in
          assert_env_length w_24 10;
          init_frame w_24 2 (Memo.from_int 0);
          set_env_slot w_24 1 arg0_18;
          set_env_slot w_24 0 arg1_8;
          w_24.state.c <- pc_to_exp (int_to_pc 37)))
    50;
  add_exp
    (fun w_25 ->
      assert_env_length w_25 1;
      resize_frame w_25 11 (Memo.from_int 0);
      assert_env_length w_25 11;
      let resolved_30 = resolve w_25 (Source.E 0) in
      let tag_8 = Word.get_value (fst resolved_30) in
      match tag_8 with
      | 5 (* tag_Const *) ->
          let parts_13 = Memo.splits (snd resolved_30) in
          if List.length parts_13 = 1 then (
            let part0_13 = List.nth parts_13 0 in
            trim_resolved w_25 1;
            return_value w_25 (get_env_slot w_25 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (51)"
      | 6 (* tag_Var *) ->
          let parts_14 = Memo.splits (snd resolved_30) in
          if List.length parts_14 = 1 then (
            let part0_14 = List.nth parts_14 0 in
            trim_resolved w_25 1;
            return_value w_25 (get_env_slot w_25 0) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (51)"
      | 7 (* tag_Add *) ->
          let parts_15 = Memo.splits (snd resolved_30) in
          if List.length parts_15 = 2 then (
            let part0_15 = List.nth parts_15 0 in
            let part1_7 = List.nth parts_15 1 in
            set_env_slot w_25 1 part0_15;
            set_env_slot w_25 2 part1_7;
            let arg0_19 = get_env_slot w_25 1 in
            assert_env_length w_25 11;
            w_25.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_12; collect_env_slots w_25 [ 2 ]; w_25.state.k ];
            trim_resolved w_25 1;
            init_frame w_25 1 (Memo.from_int 0);
            set_env_slot w_25 0 arg0_19;
            w_25.state.c <- pc_to_exp (int_to_pc 51))
          else failwith "unreachable (51)"
      | 8 (* tag_Mul *) ->
          let parts_17 = Memo.splits (snd resolved_30) in
          if List.length parts_17 = 2 then (
            let part0_17 = List.nth parts_17 0 in
            let part1_8 = List.nth parts_17 1 in
            set_env_slot w_25 3 part0_17;
            set_env_slot w_25 4 part1_8;
            let arg0_22 = get_env_slot w_25 3 in
            assert_env_length w_25 11;
            w_25.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_14; collect_env_slots w_25 [ 4 ]; w_25.state.k ];
            trim_resolved w_25 1;
            init_frame w_25 1 (Memo.from_int 0);
            set_env_slot w_25 0 arg0_22;
            w_25.state.c <- pc_to_exp (int_to_pc 51))
          else failwith "unreachable (51)"
      | _ -> failwith "unreachable (51)")
    51;
  add_exp
    (fun w_65 ->
      assert_env_length w_65 1;
      resize_frame w_65 3 (Memo.from_int 0);
      let arg0_59 = get_env_slot w_65 0 in
      assert_env_length w_65 3;
      w_65.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; collect_env_slots w_65 [ 0 ]; w_65.state.k ];
      trim_resolved w_65 1;
      init_frame w_65 1 (Memo.from_int 0);
      set_env_slot w_65 0 arg0_59;
      w_65.state.c <- pc_to_exp (int_to_pc 51))
    52;
  add_exp
    (fun w_136 ->
      assert_env_length w_136 1;
      resize_frame w_136 3 (Memo.from_int 0);
      assert_env_length w_136 3;
      let resolved_126 = resolve w_136 (Source.E 0) in
      let tag_49 = Word.get_value (fst resolved_126) in
      match tag_49 with
      | 3 (* tag_X *) ->
          assert_env_length w_136 3;
          set_env_slot w_136 1 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 1 ]);
          trim_resolved w_136 1;
          return_value w_136 (get_env_slot w_136 1) (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) ->
          assert_env_length w_136 3;
          set_env_slot w_136 2 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
          trim_resolved w_136 1;
          return_value w_136 (get_env_slot w_136 2) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (53)")
    53;
  add_exp
    (fun w_137 ->
      assert_env_length w_137 1;
      resize_frame w_137 14 (Memo.from_int 0);
      assert_env_length w_137 14;
      let resolved_127 = resolve w_137 (Source.E 0) in
      let tag_50 = Word.get_value (fst resolved_127) in
      match tag_50 with
      | 5 (* tag_Const *) ->
          let parts_66 = Memo.splits (snd resolved_127) in
          if List.length parts_66 = 1 then (
            let part0_66 = List.nth parts_66 0 in
            assert_env_length w_137 14;
            set_env_slot w_137 5 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
            trim_resolved w_137 1;
            return_value w_137 (get_env_slot w_137 5) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (54)"
      | 6 (* tag_Var *) ->
          let parts_67 = Memo.splits (snd resolved_127) in
          if List.length parts_67 = 1 then (
            let part0_67 = List.nth parts_67 0 in
            trim_resolved w_137 1;
            shuffle_frame w_137 [| NewValue part0_67 |] (Memo.from_int 0);
            w_137.state.c <- pc_to_exp (int_to_pc 53))
          else failwith "unreachable (54)"
      | 7 (* tag_Add *) ->
          let parts_68 = Memo.splits (snd resolved_127) in
          if List.length parts_68 = 2 then (
            let part0_68 = List.nth parts_68 0 in
            let part1_37 = List.nth parts_68 1 in
            set_env_slot w_137 1 part0_68;
            set_env_slot w_137 2 part1_37;
            let arg0_116 = get_env_slot w_137 1 in
            assert_env_length w_137 14;
            w_137.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_91; collect_env_slots w_137 [ 2 ]; w_137.state.k ];
            trim_resolved w_137 1;
            init_frame w_137 1 (Memo.from_int 0);
            set_env_slot w_137 0 arg0_116;
            w_137.state.c <- pc_to_exp (int_to_pc 54))
          else failwith "unreachable (54)"
      | 8 (* tag_Mul *) ->
          let parts_69 = Memo.splits (snd resolved_127) in
          if List.length parts_69 = 2 then (
            let part0_69 = List.nth parts_69 0 in
            let part1_38 = List.nth parts_69 1 in
            set_env_slot w_137 3 part0_69;
            set_env_slot w_137 4 part1_38;
            let arg0_118 = get_env_slot w_137 3 in
            assert_env_length w_137 14;
            w_137.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_93; collect_env_slots w_137 [ 3; 4 ]; w_137.state.k ];
            trim_resolved w_137 1;
            init_frame w_137 1 (Memo.from_int 0);
            set_env_slot w_137 0 arg0_118;
            w_137.state.c <- pc_to_exp (int_to_pc 54))
          else failwith "unreachable (54)"
      | _ -> failwith "unreachable (54)")
    54;
  add_exp
    (fun w_85 ->
      assert_env_length w_85 3;
      assert_env_length w_85 3;
      let resolved_87 = resolve w_85 (Source.E 0) in
      let tag_29 = Word.get_value (fst resolved_87) in
      match tag_29 with
      | 3 (* tag_X *) -> return_value w_85 (get_env_slot w_85 1) (pc_to_exp (int_to_pc 0))
      | 4 (* tag_Y *) -> return_value w_85 (get_env_slot w_85 2) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (55)")
    55;
  add_exp
    (fun w_86 ->
      assert_env_length w_86 3;
      resize_frame w_86 14 (Memo.from_int 0);
      assert_env_length w_86 14;
      let resolved_88 = resolve w_86 (Source.E 0) in
      let tag_30 = Word.get_value (fst resolved_88) in
      match tag_30 with
      | 5 (* tag_Const *) ->
          let parts_43 = Memo.splits (snd resolved_88) in
          if List.length parts_43 = 1 then (
            let part0_43 = List.nth parts_43 0 in
            set_env_slot w_86 3 part0_43;
            trim_resolved w_86 3;
            return_value w_86 (get_env_slot w_86 3) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (56)"
      | 6 (* tag_Var *) ->
          let parts_44 = Memo.splits (snd resolved_88) in
          if List.length parts_44 = 1 then (
            let part0_44 = List.nth parts_44 0 in
            trim_resolved w_86 3;
            shuffle_frame w_86 [| NewValue part0_44; OldSlot 1; OldSlot 2 |] (Memo.from_int 0);
            w_86.state.c <- pc_to_exp (int_to_pc 55))
          else failwith "unreachable (56)"
      | 7 (* tag_Add *) ->
          let parts_45 = Memo.splits (snd resolved_88) in
          if List.length parts_45 = 2 then (
            let part0_45 = List.nth parts_45 0 in
            let part1_24 = List.nth parts_45 1 in
            set_env_slot w_86 4 part0_45;
            set_env_slot w_86 5 part1_24;
            let arg0_77 = get_env_slot w_86 4 in
            let arg1_32 = get_env_slot w_86 1 in
            let arg2_1 = get_env_slot w_86 2 in
            assert_env_length w_86 14;
            w_86.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_58; collect_env_slots w_86 [ 1; 2; 5 ]; w_86.state.k ];
            trim_resolved w_86 3;
            init_frame w_86 3 (Memo.from_int 0);
            set_env_slot w_86 0 arg0_77;
            set_env_slot w_86 1 arg1_32;
            set_env_slot w_86 2 arg2_1;
            w_86.state.c <- pc_to_exp (int_to_pc 56))
          else failwith "unreachable (56)"
      | 8 (* tag_Mul *) ->
          let parts_46 = Memo.splits (snd resolved_88) in
          if List.length parts_46 = 2 then (
            let part0_46 = List.nth parts_46 0 in
            let part1_25 = List.nth parts_46 1 in
            set_env_slot w_86 6 part0_46;
            set_env_slot w_86 7 part1_25;
            let arg0_79 = get_env_slot w_86 6 in
            let arg1_34 = get_env_slot w_86 1 in
            let arg2_3 = get_env_slot w_86 2 in
            assert_env_length w_86 14;
            w_86.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_60; collect_env_slots w_86 [ 1; 2; 7 ]; w_86.state.k ];
            trim_resolved w_86 3;
            init_frame w_86 3 (Memo.from_int 0);
            set_env_slot w_86 0 arg0_79;
            set_env_slot w_86 1 arg1_34;
            set_env_slot w_86 2 arg2_3;
            w_86.state.c <- pc_to_exp (int_to_pc 56))
          else failwith "unreachable (56)"
      | _ -> failwith "unreachable (56)")
    56;
  add_exp
    (fun w_62 ->
      assert_env_length w_62 1;
      let arg0_56 = get_env_slot w_62 0 in
      assert_env_length w_62 1;
      w_62.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; collect_env_slots w_62 []; w_62.state.k ];
      init_frame w_62 1 (Memo.from_int 0);
      set_env_slot w_62 0 arg0_56;
      w_62.state.c <- pc_to_exp (int_to_pc 54))
    57;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 4;
      resize_frame w_1 6 (Memo.from_int 0);
      let arg0_1 = get_env_slot w_1 1 in
      assert_env_length w_1 6;
      w_1.state.k <- Memo.appends [ Memo.from_constructor tag_cont_0; collect_env_slots w_1 [ 2; 3 ]; w_1.state.k ];
      trim_resolved w_1 4;
      init_frame w_1 1 (Memo.from_int 0);
      set_env_slot w_1 0 arg0_1;
      w_1.state.c <- pc_to_exp (int_to_pc 12))
    58;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 5;
      resize_frame w_2 6 (Memo.from_int 0);
      let arg0_2 = get_env_slot w_2 3 in
      let arg1_0 = get_env_slot w_2 4 in
      let arg2_0 = get_env_slot w_2 2 in
      assert_env_length w_2 6;
      trim_resolved w_2 5;
      init_frame w_2 3 (Memo.from_int 0);
      set_env_slot w_2 0 arg0_2;
      set_env_slot w_2 1 arg1_0;
      set_env_slot w_2 2 arg2_0;
      w_2.state.c <- pc_to_exp (int_to_pc 38))
    59;
  add_exp
    (fun w_6 ->
      assert_env_length w_6 3;
      resize_frame w_6 10 (Memo.from_int 0);
      let arg0_6 = get_env_slot w_6 1 in
      assert_env_length w_6 10;
      w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; collect_env_slots w_6 [ 0; 1; 2 ]; w_6.state.k ];
      trim_resolved w_6 3;
      init_frame w_6 1 (Memo.from_int 0);
      set_env_slot w_6 0 arg0_6;
      w_6.state.c <- pc_to_exp (int_to_pc 14))
    60;
  add_exp
    (fun w_7 ->
      assert_env_length w_7 4;
      resize_frame w_7 10 (Memo.from_int 0);
      let arg0_7 = get_env_slot w_7 2 in
      let arg1_3 = get_env_slot w_7 3 in
      assert_env_length w_7 10;
      w_7.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; collect_env_slots w_7 [ 0; 1 ]; w_7.state.k ];
      trim_resolved w_7 4;
      init_frame w_7 2 (Memo.from_int 0);
      set_env_slot w_7 0 arg0_7;
      set_env_slot w_7 1 arg1_3;
      w_7.state.c <- pc_to_exp (int_to_pc 3))
    61;
  add_exp
    (fun w_8 ->
      assert_env_length w_8 3;
      resize_frame w_8 10 (Memo.from_int 0);
      assert_env_length w_8 10;
      let resolved_2 = resolve w_8 (Source.E 2) in
      set_env_slot w_8 3
        (Memo.from_int
           (if Word.get_value (fst resolved_2) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_3 = resolve w_8 (Source.E 3) in
      if Word.get_value (fst resolved_3) <> 0 then (
        let arg0_8 = get_env_slot w_8 0 in
        assert_env_length w_8 10;
        w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; collect_env_slots w_8 [ 0; 1 ]; w_8.state.k ];
        trim_resolved w_8 3;
        init_frame w_8 1 (Memo.from_int 0);
        set_env_slot w_8 0 arg0_8;
        w_8.state.c <- pc_to_exp (int_to_pc 12))
      else (
        trim_resolved w_8 3;
        return_value w_8 (get_env_slot w_8 2) (pc_to_exp (int_to_pc 0))))
    62;
  add_exp
    (fun w_9 ->
      assert_env_length w_9 5;
      resize_frame w_9 10 (Memo.from_int 0);
      let arg0_9 = get_env_slot w_9 1 in
      assert_env_length w_9 10;
      w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; collect_env_slots w_9 [ 0; 1; 4 ]; w_9.state.k ];
      trim_resolved w_9 5;
      init_frame w_9 1 (Memo.from_int 0);
      set_env_slot w_9 0 arg0_9;
      w_9.state.c <- pc_to_exp (int_to_pc 12))
    63;
  add_exp
    (fun w_10 ->
      assert_env_length w_10 6;
      resize_frame w_10 10 (Memo.from_int 0);
      assert_env_length w_10 10;
      let resolved_4 = resolve w_10 (Source.E 4) in
      let resolved_5 = resolve w_10 (Source.E 5) in
      set_env_slot w_10 6
        (Memo.from_int (if Word.get_value (fst resolved_4) < Word.get_value (fst resolved_5) then 1 else 0));
      let resolved_6 = resolve w_10 (Source.E 6) in
      if Word.get_value (fst resolved_6) <> 0 then (
        assert_env_length w_10 10;
        set_env_slot w_10 7
          (Memo.from_int
             (Word.get_value (Memo.to_word (Memo.from_int 0)) - Word.get_value (Memo.to_word (Memo.from_int 1))));
        trim_resolved w_10 6;
        return_value w_10 (get_env_slot w_10 7) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_10 10;
        let resolved_7 = resolve w_10 (Source.E 4) in
        let resolved_8 = resolve w_10 (Source.E 5) in
        set_env_slot w_10 8
          (Memo.from_int (if Word.get_value (fst resolved_7) > Word.get_value (fst resolved_8) then 1 else 0));
        let resolved_9 = resolve w_10 (Source.E 8) in
        if Word.get_value (fst resolved_9) <> 0 then (
          trim_resolved w_10 6;
          return_value w_10 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
        else
          let arg0_10 = get_env_slot w_10 0 in
          let arg1_4 = get_env_slot w_10 1 in
          assert_env_length w_10 10;
          trim_resolved w_10 6;
          init_frame w_10 2 (Memo.from_int 0);
          set_env_slot w_10 0 arg0_10;
          set_env_slot w_10 1 arg1_4;
          w_10.state.c <- pc_to_exp (int_to_pc 3)))
    64;
  add_exp
    (fun w_15 ->
      assert_env_length w_15 4;
      resize_frame w_15 7 (Memo.from_int 0);
      let arg0_12 = get_env_slot w_15 2 in
      assert_env_length w_15 7;
      w_15.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; collect_env_slots w_15 [ 3 ]; w_15.state.k ];
      trim_resolved w_15 4;
      init_frame w_15 1 (Memo.from_int 0);
      set_env_slot w_15 0 arg0_12;
      w_15.state.c <- pc_to_exp (int_to_pc 27))
    65;
  add_exp
    (fun w_16 ->
      assert_env_length w_16 5;
      resize_frame w_16 7 (Memo.from_int 0);
      let arg0_13 = get_env_slot w_16 3 in
      let arg1_5 = get_env_slot w_16 4 in
      assert_env_length w_16 7;
      trim_resolved w_16 5;
      init_frame w_16 2 (Memo.from_int 0);
      set_env_slot w_16 0 arg0_13;
      set_env_slot w_16 1 arg1_5;
      w_16.state.c <- pc_to_exp (int_to_pc 17))
    66;
  add_exp
    (fun w_21 ->
      assert_env_length w_21 4;
      resize_frame w_21 7 (Memo.from_int 0);
      let arg0_15 = get_env_slot w_21 2 in
      assert_env_length w_21 7;
      w_21.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; collect_env_slots w_21 [ 3 ]; w_21.state.k ];
      trim_resolved w_21 4;
      init_frame w_21 1 (Memo.from_int 0);
      set_env_slot w_21 0 arg0_15;
      w_21.state.c <- pc_to_exp (int_to_pc 26))
    67;
  add_exp
    (fun w_22 ->
      assert_env_length w_22 5;
      resize_frame w_22 7 (Memo.from_int 0);
      let arg0_16 = get_env_slot w_22 3 in
      let arg1_6 = get_env_slot w_22 4 in
      assert_env_length w_22 7;
      trim_resolved w_22 5;
      init_frame w_22 2 (Memo.from_int 0);
      set_env_slot w_22 0 arg0_16;
      set_env_slot w_22 1 arg1_6;
      w_22.state.c <- pc_to_exp (int_to_pc 17))
    68;
  add_exp
    (fun w_26 ->
      assert_env_length w_26 6;
      resize_frame w_26 11 (Memo.from_int 0);
      let arg0_20 = get_env_slot w_26 2 in
      assert_env_length w_26 11;
      w_26.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; collect_env_slots w_26 [ 5 ]; w_26.state.k ];
      trim_resolved w_26 6;
      init_frame w_26 1 (Memo.from_int 0);
      set_env_slot w_26 0 arg0_20;
      w_26.state.c <- pc_to_exp (int_to_pc 51))
    69;
  add_exp
    (fun w_27 ->
      assert_env_length w_27 7;
      resize_frame w_27 11 (Memo.from_int 0);
      assert_env_length w_27 11;
      let resolved_31 = resolve w_27 (Source.E 5) in
      let tag_9 = Word.get_value (fst resolved_31) in
      match tag_9 with
      | 5 (* tag_Const *) ->
          let parts_16 = Memo.splits (snd resolved_31) in
          if List.length parts_16 = 1 then (
            let part0_16 = List.nth parts_16 0 in
            trim_resolved w_27 7;
            shuffle_frame w_27
              [| NewValue part0_16; Blank; Blank; Blank; Blank; OldSlot 5; OldSlot 6 |]
              (Memo.from_int 0);
            w_27.state.c <- pc_to_exp (int_to_pc 49))
          else
            let arg0_21 = get_env_slot w_27 5 in
            let arg1_9 = get_env_slot w_27 6 in
            assert_env_length w_27 11;
            trim_resolved w_27 7;
            init_frame w_27 2 (Memo.from_int 0);
            set_env_slot w_27 1 arg0_21;
            set_env_slot w_27 0 arg1_9;
            w_27.state.c <- pc_to_exp (int_to_pc 47)
      | _ ->
          let arg0_21 = get_env_slot w_27 5 in
          let arg1_9 = get_env_slot w_27 6 in
          assert_env_length w_27 11;
          trim_resolved w_27 7;
          init_frame w_27 2 (Memo.from_int 0);
          set_env_slot w_27 1 arg0_21;
          set_env_slot w_27 0 arg1_9;
          w_27.state.c <- pc_to_exp (int_to_pc 47))
    70;
  add_exp
    (fun w_28 ->
      assert_env_length w_28 9;
      resize_frame w_28 11 (Memo.from_int 0);
      let arg0_23 = get_env_slot w_28 4 in
      assert_env_length w_28 11;
      w_28.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; collect_env_slots w_28 [ 8 ]; w_28.state.k ];
      trim_resolved w_28 9;
      init_frame w_28 1 (Memo.from_int 0);
      set_env_slot w_28 0 arg0_23;
      w_28.state.c <- pc_to_exp (int_to_pc 51))
    71;
  add_exp
    (fun w_29 ->
      assert_env_length w_29 10;
      resize_frame w_29 11 (Memo.from_int 0);
      assert_env_length w_29 11;
      let resolved_32 = resolve w_29 (Source.E 8) in
      let tag_10 = Word.get_value (fst resolved_32) in
      match tag_10 with
      | 5 (* tag_Const *) ->
          let parts_18 = Memo.splits (snd resolved_32) in
          if List.length parts_18 = 1 then (
            let part0_18 = List.nth parts_18 0 in
            trim_resolved w_29 10;
            shuffle_frame w_29
              [| NewValue part0_18; Blank; Blank; Blank; Blank; Blank; Blank; Blank; OldSlot 8; OldSlot 9 |]
              (Memo.from_int 0);
            w_29.state.c <- pc_to_exp (int_to_pc 50))
          else
            let arg0_24 = get_env_slot w_29 8 in
            let arg1_10 = get_env_slot w_29 9 in
            assert_env_length w_29 11;
            trim_resolved w_29 10;
            init_frame w_29 2 (Memo.from_int 0);
            set_env_slot w_29 1 arg0_24;
            set_env_slot w_29 0 arg1_10;
            w_29.state.c <- pc_to_exp (int_to_pc 37)
      | _ ->
          let arg0_24 = get_env_slot w_29 8 in
          let arg1_10 = get_env_slot w_29 9 in
          assert_env_length w_29 11;
          trim_resolved w_29 10;
          init_frame w_29 2 (Memo.from_int 0);
          set_env_slot w_29 1 arg0_24;
          set_env_slot w_29 0 arg1_10;
          w_29.state.c <- pc_to_exp (int_to_pc 37))
    72;
  add_exp
    (fun w_31 ->
      assert_env_length w_31 2;
      let arg0_26 = get_env_slot w_31 0 in
      assert_env_length w_31 2;
      w_31.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; collect_env_slots w_31 [ 1 ]; w_31.state.k ];
      init_frame w_31 1 (Memo.from_int 0);
      set_env_slot w_31 0 arg0_26;
      w_31.state.c <- pc_to_exp (int_to_pc 26))
    73;
  add_exp
    (fun w_32 ->
      assert_env_length w_32 2;
      let arg0_27 = get_env_slot w_32 1 in
      let arg1_11 = get_env_slot w_32 0 in
      assert_env_length w_32 2;
      w_32.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; collect_env_slots w_32 []; w_32.state.k ];
      init_frame w_32 2 (Memo.from_int 0);
      set_env_slot w_32 0 arg0_27;
      set_env_slot w_32 1 arg1_11;
      w_32.state.c <- pc_to_exp (int_to_pc 17))
    74;
  add_exp
    (fun w_33 ->
      assert_env_length w_33 1;
      resize_frame w_33 2 (Memo.from_int 0);
      let arg0_28 = get_env_slot w_33 0 in
      assert_env_length w_33 2;
      w_33.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; collect_env_slots w_33 []; w_33.state.k ];
      trim_resolved w_33 1;
      init_frame w_33 1 (Memo.from_int 0);
      set_env_slot w_33 0 arg0_28;
      w_33.state.c <- pc_to_exp (int_to_pc 22))
    75;
  add_exp
    (fun w_34 ->
      assert_env_length w_34 1;
      resize_frame w_34 2 (Memo.from_int 0);
      let arg0_29 = get_env_slot w_34 0 in
      assert_env_length w_34 2;
      w_34.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; collect_env_slots w_34 []; w_34.state.k ];
      trim_resolved w_34 1;
      init_frame w_34 1 (Memo.from_int 0);
      set_env_slot w_34 0 arg0_29;
      w_34.state.c <- pc_to_exp (int_to_pc 39))
    76;
  add_exp
    (fun w_35 ->
      assert_env_length w_35 2;
      let arg0_30 = get_env_slot w_35 1 in
      assert_env_length w_35 2;
      w_35.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; collect_env_slots w_35 [ 1 ]; w_35.state.k ];
      init_frame w_35 1 (Memo.from_int 0);
      set_env_slot w_35 0 arg0_30;
      w_35.state.c <- pc_to_exp (int_to_pc 46))
    77;
  add_exp
    (fun w_36 ->
      assert_env_length w_36 2;
      let arg0_31 = get_env_slot w_36 1 in
      assert_env_length w_36 2;
      w_36.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; collect_env_slots w_36 [ 0 ]; w_36.state.k ];
      init_frame w_36 1 (Memo.from_int 0);
      set_env_slot w_36 0 arg0_31;
      w_36.state.c <- pc_to_exp (int_to_pc 24))
    78;
  add_exp
    (fun w_37 ->
      assert_env_length w_37 2;
      let arg0_32 = get_env_slot w_37 1 in
      assert_env_length w_37 2;
      w_37.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; collect_env_slots w_37 [ 0 ]; w_37.state.k ];
      init_frame w_37 1 (Memo.from_int 0);
      set_env_slot w_37 0 arg0_32;
      w_37.state.c <- pc_to_exp (int_to_pc 46))
    79;
  add_exp
    (fun w_38 ->
      assert_env_length w_38 2;
      let arg0_33 = get_env_slot w_38 0 in
      assert_env_length w_38 2;
      w_38.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; collect_env_slots w_38 [ 1 ]; w_38.state.k ];
      init_frame w_38 1 (Memo.from_int 0);
      set_env_slot w_38 0 arg0_33;
      w_38.state.c <- pc_to_exp (int_to_pc 45))
    80;
  add_exp
    (fun w_39 ->
      assert_env_length w_39 2;
      let arg0_34 = get_env_slot w_39 1 in
      assert_env_length w_39 2;
      w_39.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; collect_env_slots w_39 [ 0 ]; w_39.state.k ];
      init_frame w_39 1 (Memo.from_int 0);
      set_env_slot w_39 0 arg0_34;
      w_39.state.c <- pc_to_exp (int_to_pc 45))
    81;
  add_exp
    (fun w_40 ->
      assert_env_length w_40 2;
      let arg0_35 = get_env_slot w_40 0 in
      let arg1_12 = get_env_slot w_40 1 in
      assert_env_length w_40 2;
      w_40.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; collect_env_slots w_40 []; w_40.state.k ];
      init_frame w_40 2 (Memo.from_int 0);
      set_env_slot w_40 0 arg0_35;
      set_env_slot w_40 1 arg1_12;
      w_40.state.c <- pc_to_exp (int_to_pc 8))
    82;
  add_exp
    (fun w_41 ->
      assert_env_length w_41 1;
      resize_frame w_41 2 (Memo.from_int 0);
      let arg0_36 = get_env_slot w_41 0 in
      assert_env_length w_41 2;
      w_41.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; collect_env_slots w_41 []; w_41.state.k ];
      trim_resolved w_41 1;
      init_frame w_41 1 (Memo.from_int 0);
      set_env_slot w_41 0 arg0_36;
      w_41.state.c <- pc_to_exp (int_to_pc 26))
    83;
  add_exp
    (fun w_42 ->
      assert_env_length w_42 1;
      resize_frame w_42 2 (Memo.from_int 0);
      let arg0_37 = get_env_slot w_42 0 in
      assert_env_length w_42 2;
      w_42.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; collect_env_slots w_42 []; w_42.state.k ];
      trim_resolved w_42 1;
      init_frame w_42 1 (Memo.from_int 0);
      set_env_slot w_42 0 arg0_37;
      w_42.state.c <- pc_to_exp (int_to_pc 22))
    84;
  add_exp
    (fun w_43 ->
      assert_env_length w_43 1;
      resize_frame w_43 2 (Memo.from_int 0);
      let arg0_38 = get_env_slot w_43 0 in
      assert_env_length w_43 2;
      w_43.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; collect_env_slots w_43 []; w_43.state.k ];
      trim_resolved w_43 1;
      init_frame w_43 1 (Memo.from_int 0);
      set_env_slot w_43 0 arg0_38;
      w_43.state.c <- pc_to_exp (int_to_pc 39))
    85;
  add_exp
    (fun w_44 ->
      assert_env_length w_44 1;
      resize_frame w_44 2 (Memo.from_int 0);
      let arg0_39 = get_env_slot w_44 0 in
      assert_env_length w_44 2;
      trim_resolved w_44 1;
      init_frame w_44 1 (Memo.from_int 0);
      set_env_slot w_44 0 arg0_39;
      w_44.state.c <- pc_to_exp (int_to_pc 45))
    86;
  add_exp
    (fun w_46 ->
      assert_env_length w_46 4;
      resize_frame w_46 5 (Memo.from_int 0);
      let arg0_41 = get_env_slot w_46 1 in
      let arg1_13 = get_env_slot w_46 3 in
      assert_env_length w_46 5;
      trim_resolved w_46 4;
      init_frame w_46 2 (Memo.from_int 0);
      set_env_slot w_46 0 arg0_41;
      set_env_slot w_46 1 arg1_13;
      w_46.state.c <- pc_to_exp (int_to_pc 21))
    87;
  add_exp
    (fun w_48 ->
      assert_env_length w_48 14;
      resize_frame w_48 29 (Memo.from_int 0);
      let arg0_43 = get_env_slot w_48 1 in
      assert_env_length w_48 29;
      w_48.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_34; collect_env_slots w_48 [ 0; 1; 13 ]; w_48.state.k ];
      trim_resolved w_48 14;
      init_frame w_48 1 (Memo.from_int 0);
      set_env_slot w_48 0 arg0_43;
      w_48.state.c <- pc_to_exp (int_to_pc 2))
    88;
  add_exp
    (fun w_49 ->
      assert_env_length w_49 15;
      resize_frame w_49 29 (Memo.from_int 0);
      assert_env_length w_49 29;
      let resolved_34 = resolve w_49 (Source.E 13) in
      let resolved_35 = resolve w_49 (Source.E 14) in
      set_env_slot w_49 15
        (Memo.from_int (if Word.get_value (fst resolved_34) < Word.get_value (fst resolved_35) then 1 else 0));
      let resolved_36 = resolve w_49 (Source.E 15) in
      if Word.get_value (fst resolved_36) <> 0 then (
        assert_env_length w_49 29;
        set_env_slot w_49 16
          (Memo.from_int
             (Word.get_value (Memo.to_word (Memo.from_int 0)) - Word.get_value (Memo.to_word (Memo.from_int 1))));
        trim_resolved w_49 15;
        return_value w_49 (get_env_slot w_49 16) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_49 29;
        let resolved_37 = resolve w_49 (Source.E 13) in
        let resolved_38 = resolve w_49 (Source.E 14) in
        set_env_slot w_49 17
          (Memo.from_int (if Word.get_value (fst resolved_37) > Word.get_value (fst resolved_38) then 1 else 0));
        let resolved_39 = resolve w_49 (Source.E 17) in
        if Word.get_value (fst resolved_39) <> 0 then (
          trim_resolved w_49 15;
          return_value w_49 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
        else (
          assert_env_length w_49 29;
          let resolved_40 = resolve w_49 (Source.E 0) in
          let tag_12 = Word.get_value (fst resolved_40) in
          match tag_12 with
          | 5 (* tag_Const *) ->
              let parts_20 = Memo.splits (snd resolved_40) in
              if List.length parts_20 = 1 then (
                let part0_20 = List.nth parts_20 0 in
                set_env_slot w_49 2 part0_20;
                assert_env_length w_49 29;
                let resolved_41 = resolve w_49 (Source.E 1) in
                let tag_13 = Word.get_value (fst resolved_41) in
                match tag_13 with
                | 5 (* tag_Const *) ->
                    let parts_21 = Memo.splits (snd resolved_41) in
                    if List.length parts_21 = 1 then (
                      let part0_21 = List.nth parts_21 0 in
                      trim_resolved w_49 15;
                      shuffle_frame w_49 [| NewValue part0_21; Blank; OldSlot 2 |] (Memo.from_int 0);
                      w_49.state.c <- pc_to_exp (int_to_pc 4))
                    else failwith "unreachable (89)"
                | _ -> failwith "unreachable (89)")
              else failwith "unreachable (89)"
          | 6 (* tag_Var *) ->
              let parts_22 = Memo.splits (snd resolved_40) in
              if List.length parts_22 = 1 then (
                let part0_22 = List.nth parts_22 0 in
                set_env_slot w_49 3 part0_22;
                assert_env_length w_49 29;
                let resolved_42 = resolve w_49 (Source.E 1) in
                let tag_14 = Word.get_value (fst resolved_42) in
                match tag_14 with
                | 6 (* tag_Var *) ->
                    let parts_23 = Memo.splits (snd resolved_42) in
                    if List.length parts_23 = 1 then (
                      let part0_23 = List.nth parts_23 0 in
                      set_env_slot w_49 4 part0_23;
                      let arg0_44 = get_env_slot w_49 3 in
                      assert_env_length w_49 29;
                      w_49.state.k <-
                        Memo.appends [ Memo.from_constructor tag_cont_31; collect_env_slots w_49 [ 4 ]; w_49.state.k ];
                      trim_resolved w_49 15;
                      init_frame w_49 1 (Memo.from_int 0);
                      set_env_slot w_49 0 arg0_44;
                      w_49.state.c <- pc_to_exp (int_to_pc 1))
                    else failwith "unreachable (89)"
                | _ -> failwith "unreachable (89)")
              else failwith "unreachable (89)"
          | 7 (* tag_Add *) ->
              let parts_24 = Memo.splits (snd resolved_40) in
              if List.length parts_24 = 2 then (
                let part0_24 = List.nth parts_24 0 in
                let part1_10 = List.nth parts_24 1 in
                set_env_slot w_49 5 part0_24;
                set_env_slot w_49 6 part1_10;
                assert_env_length w_49 29;
                let resolved_49 = resolve w_49 (Source.E 1) in
                let tag_15 = Word.get_value (fst resolved_49) in
                match tag_15 with
                | 7 (* tag_Add *) ->
                    let parts_25 = Memo.splits (snd resolved_49) in
                    if List.length parts_25 = 2 then (
                      let part0_25 = List.nth parts_25 0 in
                      let part1_11 = List.nth parts_25 1 in
                      set_env_slot w_49 7 part0_25;
                      set_env_slot w_49 8 part1_11;
                      let arg0_46 = get_env_slot w_49 5 in
                      let arg1_14 = get_env_slot w_49 7 in
                      assert_env_length w_49 29;
                      w_49.state.k <-
                        Memo.appends
                          [ Memo.from_constructor tag_cont_32; collect_env_slots w_49 [ 6; 8 ]; w_49.state.k ];
                      trim_resolved w_49 15;
                      init_frame w_49 2 (Memo.from_int 0);
                      set_env_slot w_49 0 arg0_46;
                      set_env_slot w_49 1 arg1_14;
                      w_49.state.c <- pc_to_exp (int_to_pc 3))
                    else failwith "unreachable (89)"
                | _ -> failwith "unreachable (89)")
              else failwith "unreachable (89)"
          | 8 (* tag_Mul *) ->
              let parts_26 = Memo.splits (snd resolved_40) in
              if List.length parts_26 = 2 then (
                let part0_26 = List.nth parts_26 0 in
                let part1_12 = List.nth parts_26 1 in
                set_env_slot w_49 9 part0_26;
                set_env_slot w_49 10 part1_12;
                assert_env_length w_49 29;
                let resolved_52 = resolve w_49 (Source.E 1) in
                let tag_16 = Word.get_value (fst resolved_52) in
                match tag_16 with
                | 8 (* tag_Mul *) ->
                    let parts_27 = Memo.splits (snd resolved_52) in
                    if List.length parts_27 = 2 then (
                      let part0_27 = List.nth parts_27 0 in
                      let part1_13 = List.nth parts_27 1 in
                      set_env_slot w_49 11 part0_27;
                      set_env_slot w_49 12 part1_13;
                      let arg0_48 = get_env_slot w_49 9 in
                      let arg1_16 = get_env_slot w_49 11 in
                      assert_env_length w_49 29;
                      w_49.state.k <-
                        Memo.appends
                          [ Memo.from_constructor tag_cont_33; collect_env_slots w_49 [ 10; 12 ]; w_49.state.k ];
                      trim_resolved w_49 15;
                      init_frame w_49 2 (Memo.from_int 0);
                      set_env_slot w_49 0 arg0_48;
                      set_env_slot w_49 1 arg1_16;
                      w_49.state.c <- pc_to_exp (int_to_pc 3))
                    else failwith "unreachable (89)"
                | _ -> failwith "unreachable (89)")
              else failwith "unreachable (89)"
          | _ -> failwith "unreachable (89)")))
    89;
  add_exp
    (fun w_50 ->
      assert_env_length w_50 19;
      resize_frame w_50 29 (Memo.from_int 0);
      let arg0_45 = get_env_slot w_50 4 in
      assert_env_length w_50 29;
      w_50.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; collect_env_slots w_50 [ 18 ]; w_50.state.k ];
      trim_resolved w_50 19;
      init_frame w_50 1 (Memo.from_int 0);
      set_env_slot w_50 0 arg0_45;
      w_50.state.c <- pc_to_exp (int_to_pc 1))
    90;
  add_exp
    (fun w_51 ->
      assert_env_length w_51 20;
      resize_frame w_51 29 (Memo.from_int 0);
      assert_env_length w_51 29;
      let resolved_43 = resolve w_51 (Source.E 18) in
      let resolved_44 = resolve w_51 (Source.E 19) in
      set_env_slot w_51 20
        (Memo.from_int (if Word.get_value (fst resolved_43) < Word.get_value (fst resolved_44) then 1 else 0));
      let resolved_45 = resolve w_51 (Source.E 20) in
      if Word.get_value (fst resolved_45) <> 0 then (
        assert_env_length w_51 29;
        set_env_slot w_51 21
          (Memo.from_int
             (Word.get_value (Memo.to_word (Memo.from_int 0)) - Word.get_value (Memo.to_word (Memo.from_int 1))));
        trim_resolved w_51 20;
        return_value w_51 (get_env_slot w_51 21) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_51 29;
        let resolved_46 = resolve w_51 (Source.E 18) in
        let resolved_47 = resolve w_51 (Source.E 19) in
        set_env_slot w_51 22
          (Memo.from_int (if Word.get_value (fst resolved_46) > Word.get_value (fst resolved_47) then 1 else 0));
        let resolved_48 = resolve w_51 (Source.E 22) in
        if Word.get_value (fst resolved_48) <> 0 then (
          trim_resolved w_51 20;
          return_value w_51 (Memo.from_int 1) (pc_to_exp (int_to_pc 0)))
        else (
          trim_resolved w_51 20;
          return_value w_51 (Memo.from_int 0) (pc_to_exp (int_to_pc 0)))))
    91;
  add_exp
    (fun w_52 ->
      assert_env_length w_52 24;
      resize_frame w_52 29 (Memo.from_int 0);
      assert_env_length w_52 29;
      let resolved_50 = resolve w_52 (Source.E 23) in
      set_env_slot w_52 24
        (Memo.from_int
           (if Word.get_value (fst resolved_50) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_51 = resolve w_52 (Source.E 24) in
      if Word.get_value (fst resolved_51) <> 0 then (
        let arg0_47 = get_env_slot w_52 6 in
        let arg1_15 = get_env_slot w_52 8 in
        assert_env_length w_52 29;
        trim_resolved w_52 24;
        init_frame w_52 2 (Memo.from_int 0);
        set_env_slot w_52 0 arg0_47;
        set_env_slot w_52 1 arg1_15;
        w_52.state.c <- pc_to_exp (int_to_pc 3))
      else (
        trim_resolved w_52 24;
        return_value w_52 (get_env_slot w_52 23) (pc_to_exp (int_to_pc 0))))
    92;
  add_exp
    (fun w_53 ->
      assert_env_length w_53 27;
      resize_frame w_53 29 (Memo.from_int 0);
      assert_env_length w_53 29;
      let resolved_53 = resolve w_53 (Source.E 26) in
      set_env_slot w_53 27
        (Memo.from_int
           (if Word.get_value (fst resolved_53) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_54 = resolve w_53 (Source.E 27) in
      if Word.get_value (fst resolved_54) <> 0 then (
        let arg0_49 = get_env_slot w_53 10 in
        let arg1_17 = get_env_slot w_53 12 in
        assert_env_length w_53 29;
        trim_resolved w_53 27;
        init_frame w_53 2 (Memo.from_int 0);
        set_env_slot w_53 0 arg0_49;
        set_env_slot w_53 1 arg1_17;
        w_53.state.c <- pc_to_exp (int_to_pc 3))
      else (
        trim_resolved w_53 27;
        return_value w_53 (get_env_slot w_53 26) (pc_to_exp (int_to_pc 0))))
    93;
  add_exp
    (fun w_56 ->
      assert_env_length w_56 6;
      resize_frame w_56 10 (Memo.from_int 0);
      assert_env_length w_56 10;
      let resolved_62 = resolve w_56 (Source.E 5) in
      let tag_18 = Word.get_value (fst resolved_62) in
      match tag_18 with
      | 13 (* tag_NoPick *) ->
          let arg0_51 = get_env_slot w_56 2 in
          assert_env_length w_56 10;
          w_56.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; collect_env_slots w_56 [ 1 ]; w_56.state.k ];
          trim_resolved w_56 6;
          init_frame w_56 1 (Memo.from_int 0);
          set_env_slot w_56 0 arg0_51;
          w_56.state.c <- pc_to_exp (int_to_pc 43)
      | 14 (* tag_Pick *) ->
          let parts_29 = Memo.splits (snd resolved_62) in
          if List.length parts_29 = 2 then (
            let part0_29 = List.nth parts_29 0 in
            let part1_15 = List.nth parts_29 1 in
            set_env_slot w_56 3 part0_29;
            set_env_slot w_56 4 part1_15;
            let arg0_52 = get_env_slot w_56 3 in
            let arg1_19 = get_env_slot w_56 4 in
            assert_env_length w_56 10;
            w_56.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; collect_env_slots w_56 []; w_56.state.k ];
            trim_resolved w_56 6;
            init_frame w_56 2 (Memo.from_int 0);
            set_env_slot w_56 0 arg0_52;
            set_env_slot w_56 1 arg1_19;
            w_56.state.c <- pc_to_exp (int_to_pc 21))
          else failwith "unreachable (94)"
      | _ -> failwith "unreachable (94)")
    94;
  add_exp
    (fun w_57 ->
      assert_env_length w_57 7;
      resize_frame w_57 10 (Memo.from_int 0);
      assert_env_length w_57 10;
      set_env_slot w_57 7 (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_57 1; get_env_slot w_57 6 ]);
      trim_resolved w_57 7;
      return_value w_57 (get_env_slot w_57 7) (pc_to_exp (int_to_pc 0)))
    95;
  add_exp
    (fun w_58 ->
      assert_env_length w_58 9;
      resize_frame w_58 10 (Memo.from_int 0);
      let arg0_53 = get_env_slot w_58 8 in
      assert_env_length w_58 10;
      trim_resolved w_58 9;
      init_frame w_58 1 (Memo.from_int 0);
      set_env_slot w_58 0 arg0_53;
      w_58.state.c <- pc_to_exp (int_to_pc 43))
    96;
  add_exp
    (fun w_60 ->
      assert_env_length w_60 6;
      resize_frame w_60 10 (Memo.from_int 0);
      assert_env_length w_60 10;
      let resolved_64 = resolve w_60 (Source.E 5) in
      set_env_slot w_60 6
        (Memo.from_int
           (if Word.get_value (fst resolved_64) <= Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_65 = resolve w_60 (Source.E 6) in
      if Word.get_value (fst resolved_65) <> 0 then (
        assert_env_length w_60 10;
        set_env_slot w_60 7 (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_60 0; get_env_slot w_60 1 ]);
        trim_resolved w_60 6;
        return_value w_60 (get_env_slot w_60 7) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_55 = get_env_slot w_60 0 in
        let arg1_21 = get_env_slot w_60 3 in
        assert_env_length w_60 10;
        w_60.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; collect_env_slots w_60 [ 2 ]; w_60.state.k ];
        trim_resolved w_60 6;
        init_frame w_60 2 (Memo.from_int 0);
        set_env_slot w_60 0 arg0_55;
        set_env_slot w_60 1 arg1_21;
        w_60.state.c <- pc_to_exp (int_to_pc 21))
    97;
  add_exp
    (fun w_61 ->
      assert_env_length w_61 9;
      resize_frame w_61 10 (Memo.from_int 0);
      assert_env_length w_61 10;
      set_env_slot w_61 9 (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_61 2; get_env_slot w_61 8 ]);
      trim_resolved w_61 9;
      return_value w_61 (get_env_slot w_61 9) (pc_to_exp (int_to_pc 0)))
    98;
  add_exp
    (fun w_63 ->
      assert_env_length w_63 1;
      let arg0_57 = get_env_slot w_63 0 in
      assert_env_length w_63 1;
      w_63.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; collect_env_slots w_63 []; w_63.state.k ];
      init_frame w_63 1 (Memo.from_int 0);
      set_env_slot w_63 0 arg0_57;
      w_63.state.c <- pc_to_exp (int_to_pc 54))
    99;
  add_exp
    (fun w_64 ->
      assert_env_length w_64 1;
      let arg0_58 = get_env_slot w_64 0 in
      assert_env_length w_64 1;
      init_frame w_64 1 (Memo.from_int 0);
      set_env_slot w_64 0 arg0_58;
      w_64.state.c <- pc_to_exp (int_to_pc 52))
    100;
  add_exp
    (fun w_66 ->
      assert_env_length w_66 2;
      resize_frame w_66 3 (Memo.from_int 0);
      let arg0_60 = get_env_slot w_66 0 in
      let arg1_22 = get_env_slot w_66 1 in
      assert_env_length w_66 3;
      w_66.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; collect_env_slots w_66 [ 1 ]; w_66.state.k ];
      trim_resolved w_66 2;
      init_frame w_66 2 (Memo.from_int 0);
      set_env_slot w_66 0 arg0_60;
      set_env_slot w_66 1 arg1_22;
      w_66.state.c <- pc_to_exp (int_to_pc 6))
    101;
  add_exp
    (fun w_67 ->
      assert_env_length w_67 2;
      resize_frame w_67 3 (Memo.from_int 0);
      let resolved_66 = resolve w_67 (Source.E 0) in
      if Word.get_value (fst resolved_66) <> 0 then (
        trim_resolved w_67 2;
        return_value w_67 (get_env_slot w_67 1) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_61 = get_env_slot w_67 1 in
        assert_env_length w_67 3;
        trim_resolved w_67 2;
        init_frame w_67 1 (Memo.from_int 0);
        set_env_slot w_67 0 arg0_61;
        w_67.state.c <- pc_to_exp (int_to_pc 52))
    102;
  add_exp
    (fun w_69 ->
      assert_env_length w_69 7;
      resize_frame w_69 13 (Memo.from_int 0);
      assert_env_length w_69 13;
      set_env_slot w_69 7 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_69 0; get_env_slot w_69 2 ]);
      let arg0_63 = get_env_slot w_69 6 in
      let arg1_24 = get_env_slot w_69 7 in
      assert_env_length w_69 13;
      w_69.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_46; collect_env_slots w_69 [ 0; 2; 3; 4; 5; 6 ]; w_69.state.k ];
      trim_resolved w_69 7;
      init_frame w_69 2 (Memo.from_int 0);
      set_env_slot w_69 0 arg0_63;
      set_env_slot w_69 1 arg1_24;
      w_69.state.c <- pc_to_exp (int_to_pc 6))
    103;
  add_exp
    (fun w_70 ->
      assert_env_length w_70 9;
      resize_frame w_70 13 (Memo.from_int 0);
      let resolved_68 = resolve w_70 (Source.E 8) in
      if Word.get_value (fst resolved_68) <> 0 then (
        let arg0_64 = get_env_slot w_70 0 in
        let arg1_25 = get_env_slot w_70 3 in
        assert_env_length w_70 13;
        w_70.state.k <-
          Memo.appends [ Memo.from_constructor tag_cont_45; collect_env_slots w_70 [ 2; 4; 5 ]; w_70.state.k ];
        trim_resolved w_70 9;
        init_frame w_70 2 (Memo.from_int 0);
        set_env_slot w_70 0 arg0_64;
        set_env_slot w_70 1 arg1_25;
        w_70.state.c <- pc_to_exp (int_to_pc 42))
      else (
        assert_env_length w_70 13;
        set_env_slot w_70 12 (Memo.appends [ Memo.from_constructor tag_Pick; get_env_slot w_70 6; get_env_slot w_70 3 ]);
        trim_resolved w_70 9;
        return_value w_70 (get_env_slot w_70 12) (pc_to_exp (int_to_pc 0))))
    104;
  add_exp
    (fun w_71 ->
      assert_env_length w_71 10;
      resize_frame w_71 13 (Memo.from_int 0);
      assert_env_length w_71 13;
      let resolved_69 = resolve w_71 (Source.E 9) in
      let tag_21 = Word.get_value (fst resolved_69) in
      match tag_21 with
      | 13 (* tag_NoPick *) ->
          trim_resolved w_71 10;
          return_value w_71 (Memo.from_constructor tag_NoPick) (pc_to_exp (int_to_pc 0))
      | 14 (* tag_Pick *) ->
          let parts_32 = Memo.splits (snd resolved_69) in
          if List.length parts_32 = 2 then (
            let part0_32 = List.nth parts_32 0 in
            let part1_18 = List.nth parts_32 1 in
            set_env_slot w_71 4 part0_32;
            set_env_slot w_71 5 part1_18;
            assert_env_length w_71 13;
            set_env_slot w_71 10
              (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_71 2; get_env_slot w_71 5 ]);
            assert_env_length w_71 13;
            set_env_slot w_71 11
              (Memo.appends [ Memo.from_constructor tag_Pick; get_env_slot w_71 4; get_env_slot w_71 10 ]);
            trim_resolved w_71 10;
            return_value w_71 (get_env_slot w_71 11) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (105)"
      | _ -> failwith "unreachable (105)")
    105;
  add_exp
    (fun w_73 ->
      assert_env_length w_73 5;
      resize_frame w_73 6 (Memo.from_int 0);
      let arg0_66 = get_env_slot w_73 3 in
      let arg1_26 = get_env_slot w_73 4 in
      assert_env_length w_73 6;
      trim_resolved w_73 5;
      init_frame w_73 2 (Memo.from_int 0);
      set_env_slot w_73 0 arg0_66;
      set_env_slot w_73 1 arg1_26;
      w_73.state.c <- pc_to_exp (int_to_pc 18))
    106;
  add_exp
    (fun w_75 ->
      assert_env_length w_75 4;
      resize_frame w_75 6 (Memo.from_int 0);
      assert_env_length w_75 6;
      let resolved_73 = resolve w_75 (Source.E 3) in
      let tag_23 = Word.get_value (fst resolved_73) in
      match tag_23 with
      | 5 (* tag_Const *) ->
          let parts_34 = Memo.splits (snd resolved_73) in
          if List.length parts_34 = 1 then (
            let part0_34 = List.nth parts_34 0 in
            trim_resolved w_75 4;
            shuffle_frame w_75 [| NewValue part0_34; Blank; OldSlot 2; OldSlot 3 |] (Memo.from_int 0);
            w_75.state.c <- pc_to_exp (int_to_pc 33))
          else
            let arg0_69 = get_env_slot w_75 2 in
            assert_env_length w_75 6;
            w_75.state.k <-
              Memo.appends [ Memo.from_constructor tag_cont_49; collect_env_slots w_75 [ 3 ]; w_75.state.k ];
            trim_resolved w_75 4;
            init_frame w_75 1 (Memo.from_int 0);
            set_env_slot w_75 0 arg0_69;
            w_75.state.c <- pc_to_exp (int_to_pc 34)
      | _ ->
          let arg0_69 = get_env_slot w_75 2 in
          assert_env_length w_75 6;
          w_75.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; collect_env_slots w_75 [ 3 ]; w_75.state.k ];
          trim_resolved w_75 4;
          init_frame w_75 1 (Memo.from_int 0);
          set_env_slot w_75 0 arg0_69;
          w_75.state.c <- pc_to_exp (int_to_pc 34))
    107;
  add_exp
    (fun w_76 ->
      assert_env_length w_76 5;
      resize_frame w_76 6 (Memo.from_int 0);
      let arg0_70 = get_env_slot w_76 3 in
      let arg1_27 = get_env_slot w_76 4 in
      assert_env_length w_76 6;
      trim_resolved w_76 5;
      init_frame w_76 2 (Memo.from_int 0);
      set_env_slot w_76 0 arg0_70;
      set_env_slot w_76 1 arg1_27;
      w_76.state.c <- pc_to_exp (int_to_pc 18))
    108;
  add_exp
    (fun w_79 ->
      assert_env_length w_79 14;
      resize_frame w_79 22 (Memo.from_int 0);
      let arg0_72 = get_env_slot w_79 4 in
      assert_env_length w_79 22;
      w_79.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; collect_env_slots w_79 [ 13 ]; w_79.state.k ];
      trim_resolved w_79 14;
      init_frame w_79 1 (Memo.from_int 0);
      set_env_slot w_79 0 arg0_72;
      w_79.state.c <- pc_to_exp (int_to_pc 1))
    109;
  add_exp
    (fun w_80 ->
      assert_env_length w_80 15;
      resize_frame w_80 22 (Memo.from_int 0);
      assert_env_length w_80 22;
      let resolved_79 = resolve w_80 (Source.E 13) in
      let resolved_80 = resolve w_80 (Source.E 14) in
      set_env_slot w_80 15
        (Memo.from_int (if Word.get_value (fst resolved_79) = Word.get_value (fst resolved_80) then 1 else 0));
      trim_resolved w_80 15;
      return_value w_80 (get_env_slot w_80 15) (pc_to_exp (int_to_pc 0)))
    110;
  add_exp
    (fun w_81 ->
      assert_env_length w_81 17;
      resize_frame w_81 22 (Memo.from_int 0);
      let arg0_74 = get_env_slot w_81 6 in
      let arg1_29 = get_env_slot w_81 8 in
      assert_env_length w_81 22;
      w_81.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; collect_env_slots w_81 [ 16 ]; w_81.state.k ];
      trim_resolved w_81 17;
      init_frame w_81 2 (Memo.from_int 0);
      set_env_slot w_81 0 arg0_74;
      set_env_slot w_81 1 arg1_29;
      w_81.state.c <- pc_to_exp (int_to_pc 6))
    111;
  add_exp
    (fun w_82 ->
      assert_env_length w_82 18;
      resize_frame w_82 22 (Memo.from_int 0);
      assert_env_length w_82 22;
      let resolved_82 = resolve w_82 (Source.E 16) in
      let resolved_83 = resolve w_82 (Source.E 17) in
      set_env_slot w_82 18
        (Memo.from_int
           (if Word.get_value (fst resolved_82) <> 0 && Word.get_value (fst resolved_83) <> 0 then 1 else 0));
      trim_resolved w_82 18;
      return_value w_82 (get_env_slot w_82 18) (pc_to_exp (int_to_pc 0)))
    112;
  add_exp
    (fun w_83 ->
      assert_env_length w_83 20;
      resize_frame w_83 22 (Memo.from_int 0);
      let arg0_76 = get_env_slot w_83 10 in
      let arg1_31 = get_env_slot w_83 12 in
      assert_env_length w_83 22;
      w_83.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; collect_env_slots w_83 [ 19 ]; w_83.state.k ];
      trim_resolved w_83 20;
      init_frame w_83 2 (Memo.from_int 0);
      set_env_slot w_83 0 arg0_76;
      set_env_slot w_83 1 arg1_31;
      w_83.state.c <- pc_to_exp (int_to_pc 6))
    113;
  add_exp
    (fun w_84 ->
      assert_env_length w_84 21;
      resize_frame w_84 22 (Memo.from_int 0);
      assert_env_length w_84 22;
      let resolved_85 = resolve w_84 (Source.E 19) in
      let resolved_86 = resolve w_84 (Source.E 20) in
      set_env_slot w_84 21
        (Memo.from_int
           (if Word.get_value (fst resolved_85) <> 0 && Word.get_value (fst resolved_86) <> 0 then 1 else 0));
      trim_resolved w_84 21;
      return_value w_84 (get_env_slot w_84 21) (pc_to_exp (int_to_pc 0)))
    114;
  add_exp
    (fun w_87 ->
      assert_env_length w_87 9;
      resize_frame w_87 14 (Memo.from_int 0);
      let arg0_78 = get_env_slot w_87 5 in
      let arg1_33 = get_env_slot w_87 1 in
      let arg2_2 = get_env_slot w_87 2 in
      assert_env_length w_87 14;
      w_87.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; collect_env_slots w_87 [ 8 ]; w_87.state.k ];
      trim_resolved w_87 9;
      init_frame w_87 3 (Memo.from_int 0);
      set_env_slot w_87 0 arg0_78;
      set_env_slot w_87 1 arg1_33;
      set_env_slot w_87 2 arg2_2;
      w_87.state.c <- pc_to_exp (int_to_pc 56))
    115;
  add_exp
    (fun w_88 ->
      assert_env_length w_88 10;
      resize_frame w_88 14 (Memo.from_int 0);
      assert_env_length w_88 14;
      let resolved_89 = resolve w_88 (Source.E 8) in
      let resolved_90 = resolve w_88 (Source.E 9) in
      set_env_slot w_88 10 (Memo.from_int (Word.get_value (fst resolved_89) + Word.get_value (fst resolved_90)));
      trim_resolved w_88 10;
      return_value w_88 (get_env_slot w_88 10) (pc_to_exp (int_to_pc 0)))
    116;
  add_exp
    (fun w_89 ->
      assert_env_length w_89 12;
      resize_frame w_89 14 (Memo.from_int 0);
      let arg0_80 = get_env_slot w_89 7 in
      let arg1_35 = get_env_slot w_89 1 in
      let arg2_4 = get_env_slot w_89 2 in
      assert_env_length w_89 14;
      w_89.state.k <- Memo.appends [ Memo.from_constructor tag_cont_59; collect_env_slots w_89 [ 11 ]; w_89.state.k ];
      trim_resolved w_89 12;
      init_frame w_89 3 (Memo.from_int 0);
      set_env_slot w_89 0 arg0_80;
      set_env_slot w_89 1 arg1_35;
      set_env_slot w_89 2 arg2_4;
      w_89.state.c <- pc_to_exp (int_to_pc 56))
    117;
  add_exp
    (fun w_90 ->
      assert_env_length w_90 13;
      resize_frame w_90 14 (Memo.from_int 0);
      assert_env_length w_90 14;
      let resolved_91 = resolve w_90 (Source.E 11) in
      let resolved_92 = resolve w_90 (Source.E 12) in
      set_env_slot w_90 13 (Memo.from_int (Word.get_value (fst resolved_91) * Word.get_value (fst resolved_92)));
      trim_resolved w_90 13;
      return_value w_90 (get_env_slot w_90 13) (pc_to_exp (int_to_pc 0)))
    118;
  add_exp
    (fun w_94 ->
      assert_env_length w_94 7;
      resize_frame w_94 14 (Memo.from_int 0);
      assert_env_length w_94 14;
      let resolved_96 = resolve w_94 (Source.E 6) in
      let tag_34 = Word.get_value (fst resolved_96) in
      match tag_34 with
      | 10 (* tag_Found *) ->
          let parts_51 = Memo.splits (snd resolved_96) in
          if List.length parts_51 = 1 then (
            let part0_51 = List.nth parts_51 0 in
            set_env_slot w_94 4 part0_51;
            assert_env_length w_94 14;
            set_env_slot w_94 7
              (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_94 3; get_env_slot w_94 4 ]);
            assert_env_length w_94 14;
            set_env_slot w_94 8
              (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_94 2; get_env_slot w_94 7 ]);
            trim_resolved w_94 7;
            return_value w_94 (get_env_slot w_94 8) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (119)"
      | 9 (* tag_Missing *) ->
          let arg0_82 = get_env_slot w_94 3 in
          let arg1_37 = get_env_slot w_94 1 in
          assert_env_length w_94 14;
          w_94.state.k <-
            Memo.appends [ Memo.from_constructor tag_cont_61; collect_env_slots w_94 [ 0; 1; 2; 3; 5 ]; w_94.state.k ];
          trim_resolved w_94 7;
          init_frame w_94 2 (Memo.from_int 0);
          set_env_slot w_94 0 arg0_82;
          set_env_slot w_94 1 arg1_37;
          w_94.state.c <- pc_to_exp (int_to_pc 15)
      | _ -> failwith "unreachable (119)")
    119;
  add_exp
    (fun w_95 ->
      assert_env_length w_95 10;
      resize_frame w_95 14 (Memo.from_int 0);
      assert_env_length w_95 14;
      let resolved_97 = resolve w_95 (Source.E 9) in
      let tag_35 = Word.get_value (fst resolved_97) in
      match tag_35 with
      | 10 (* tag_Found *) ->
          let parts_52 = Memo.splits (snd resolved_97) in
          if List.length parts_52 = 1 then (
            let part0_52 = List.nth parts_52 0 in
            set_env_slot w_95 5 part0_52;
            assert_env_length w_95 14;
            set_env_slot w_95 10
              (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_95 2; get_env_slot w_95 5 ]);
            assert_env_length w_95 14;
            set_env_slot w_95 11
              (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_95 3; get_env_slot w_95 10 ]);
            trim_resolved w_95 10;
            return_value w_95 (get_env_slot w_95 11) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (120)"
      | 9 (* tag_Missing *) ->
          assert_env_length w_95 14;
          set_env_slot w_95 12
            (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_95 0; get_env_slot w_95 1 ]);
          trim_resolved w_95 10;
          return_value w_95 (get_env_slot w_95 12) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (120)")
    120;
  add_exp
    (fun w_97 ->
      assert_env_length w_97 4;
      resize_frame w_97 5 (Memo.from_int 0);
      let arg0_84 = get_env_slot w_97 1 in
      let arg1_38 = get_env_slot w_97 3 in
      assert_env_length w_97 5;
      trim_resolved w_97 4;
      init_frame w_97 2 (Memo.from_int 0);
      set_env_slot w_97 0 arg0_84;
      set_env_slot w_97 1 arg1_38;
      w_97.state.c <- pc_to_exp (int_to_pc 18))
    121;
  add_exp
    (fun w_99 ->
      assert_env_length w_99 1;
      let arg0_86 = get_env_slot w_99 0 in
      assert_env_length w_99 1;
      w_99.state.k <- Memo.appends [ Memo.from_constructor tag_cont_65; collect_env_slots w_99 []; w_99.state.k ];
      init_frame w_99 1 (Memo.from_int 0);
      set_env_slot w_99 0 arg0_86;
      w_99.state.c <- pc_to_exp (int_to_pc 43))
    122;
  add_exp
    (fun w_100 ->
      assert_env_length w_100 1;
      let arg0_87 = get_env_slot w_100 0 in
      assert_env_length w_100 1;
      w_100.state.k <- Memo.appends [ Memo.from_constructor tag_cont_64; collect_env_slots w_100 []; w_100.state.k ];
      init_frame w_100 1 (Memo.from_int 0);
      set_env_slot w_100 0 arg0_87;
      w_100.state.c <- pc_to_exp (int_to_pc 41))
    123;
  add_exp
    (fun w_101 ->
      assert_env_length w_101 1;
      let arg0_88 = get_env_slot w_101 0 in
      assert_env_length w_101 1;
      init_frame w_101 1 (Memo.from_int 0);
      set_env_slot w_101 0 arg0_88;
      w_101.state.c <- pc_to_exp (int_to_pc 43))
    124;
  add_exp
    (fun w_105 ->
      assert_env_length w_105 2;
      resize_frame w_105 8 (Memo.from_int 0);
      let arg0_90 = get_env_slot w_105 0 in
      assert_env_length w_105 8;
      w_105.state.k <- Memo.appends [ Memo.from_constructor tag_cont_71; collect_env_slots w_105 [ 1 ]; w_105.state.k ];
      trim_resolved w_105 2;
      init_frame w_105 1 (Memo.from_int 0);
      set_env_slot w_105 0 arg0_90;
      w_105.state.c <- pc_to_exp (int_to_pc 27))
    125;
  add_exp
    (fun w_106 ->
      assert_env_length w_106 2;
      resize_frame w_106 8 (Memo.from_int 0);
      let arg0_91 = get_env_slot w_106 1 in
      let arg1_39 = get_env_slot w_106 0 in
      assert_env_length w_106 8;
      w_106.state.k <- Memo.appends [ Memo.from_constructor tag_cont_70; collect_env_slots w_106 []; w_106.state.k ];
      trim_resolved w_106 2;
      init_frame w_106 2 (Memo.from_int 0);
      set_env_slot w_106 0 arg0_91;
      set_env_slot w_106 1 arg1_39;
      w_106.state.c <- pc_to_exp (int_to_pc 17))
    126;
  add_exp
    (fun w_107 ->
      assert_env_length w_107 1;
      resize_frame w_107 8 (Memo.from_int 0);
      let arg0_92 = get_env_slot w_107 0 in
      assert_env_length w_107 8;
      w_107.state.k <- Memo.appends [ Memo.from_constructor tag_cont_69; collect_env_slots w_107 [ 0 ]; w_107.state.k ];
      trim_resolved w_107 1;
      init_frame w_107 1 (Memo.from_int 0);
      set_env_slot w_107 0 arg0_92;
      w_107.state.c <- pc_to_exp (int_to_pc 32))
    127;
  add_exp
    (fun w_108 ->
      assert_env_length w_108 2;
      resize_frame w_108 8 (Memo.from_int 0);
      assert_env_length w_108 8;
      let resolved_101 = resolve w_108 (Source.E 1) in
      set_env_slot w_108 2
        (Memo.from_int
           (if Word.get_value (fst resolved_101) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_102 = resolve w_108 (Source.E 2) in
      if Word.get_value (fst resolved_102) <> 0 then (
        assert_env_length w_108 8;
        set_env_slot w_108 3 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 0 ]);
        trim_resolved w_108 2;
        return_value w_108 (get_env_slot w_108 3) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_93 = get_env_slot w_108 0 in
        assert_env_length w_108 8;
        w_108.state.k <-
          Memo.appends [ Memo.from_constructor tag_cont_68; collect_env_slots w_108 [ 1 ]; w_108.state.k ];
        trim_resolved w_108 2;
        init_frame w_108 1 (Memo.from_int 0);
        set_env_slot w_108 0 arg0_93;
        w_108.state.c <- pc_to_exp (int_to_pc 34))
    128;
  add_exp
    (fun w_109 ->
      assert_env_length w_109 5;
      resize_frame w_109 8 (Memo.from_int 0);
      let arg0_94 = get_env_slot w_109 4 in
      assert_env_length w_109 8;
      w_109.state.k <- Memo.appends [ Memo.from_constructor tag_cont_67; collect_env_slots w_109 [ 1 ]; w_109.state.k ];
      trim_resolved w_109 5;
      init_frame w_109 1 (Memo.from_int 0);
      set_env_slot w_109 0 arg0_94;
      w_109.state.c <- pc_to_exp (int_to_pc 36))
    129;
  add_exp
    (fun w_110 ->
      assert_env_length w_110 6;
      resize_frame w_110 8 (Memo.from_int 0);
      assert_env_length w_110 8;
      let resolved_103 = resolve w_110 (Source.E 1) in
      set_env_slot w_110 6
        (Memo.from_int
           (if Word.get_value (fst resolved_103) = Word.get_value (Memo.to_word (Memo.from_int 1)) then 1 else 0));
      let resolved_104 = resolve w_110 (Source.E 6) in
      if Word.get_value (fst resolved_104) <> 0 then (
        trim_resolved w_110 6;
        return_value w_110 (get_env_slot w_110 5) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_95 = get_env_slot w_110 1 in
        let arg1_40 = get_env_slot w_110 5 in
        assert_env_length w_110 8;
        trim_resolved w_110 6;
        init_frame w_110 2 (Memo.from_int 0);
        set_env_slot w_110 0 arg0_95;
        set_env_slot w_110 1 arg1_40;
        w_110.state.c <- pc_to_exp (int_to_pc 9))
    130;
  add_exp
    (fun w_112 ->
      assert_env_length w_112 4;
      resize_frame w_112 6 (Memo.from_int 0);
      let arg0_97 = get_env_slot w_112 2 in
      assert_env_length w_112 6;
      w_112.state.k <- Memo.appends [ Memo.from_constructor tag_cont_73; collect_env_slots w_112 [ 3 ]; w_112.state.k ];
      trim_resolved w_112 4;
      init_frame w_112 1 (Memo.from_int 0);
      set_env_slot w_112 0 arg0_97;
      w_112.state.c <- pc_to_exp (int_to_pc 32))
    131;
  add_exp
    (fun w_113 ->
      assert_env_length w_113 5;
      resize_frame w_113 6 (Memo.from_int 0);
      assert_env_length w_113 6;
      let resolved_106 = resolve w_113 (Source.E 3) in
      let resolved_107 = resolve w_113 (Source.E 4) in
      set_env_slot w_113 5 (Memo.from_int (Word.get_value (fst resolved_106) * Word.get_value (fst resolved_107)));
      trim_resolved w_113 5;
      return_value w_113 (get_env_slot w_113 5) (pc_to_exp (int_to_pc 0)))
    132;
  add_exp
    (fun w_115 ->
      assert_env_length w_115 3;
      resize_frame w_115 4 (Memo.from_int 0);
      assert_env_length w_115 4;
      set_env_slot w_115 3 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_115 0; get_env_slot w_115 2 ]);
      trim_resolved w_115 3;
      return_value w_115 (get_env_slot w_115 3) (pc_to_exp (int_to_pc 0)))
    133;
  add_exp
    (fun w_118 ->
      assert_env_length w_118 6;
      resize_frame w_118 12 (Memo.from_int 0);
      assert_env_length w_118 12;
      set_env_slot w_118 6 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_118 0; get_env_slot w_118 2 ]);
      let arg0_100 = get_env_slot w_118 5 in
      let arg1_42 = get_env_slot w_118 6 in
      assert_env_length w_118 12;
      w_118.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_78; collect_env_slots w_118 [ 0; 1; 3; 5 ]; w_118.state.k ];
      trim_resolved w_118 6;
      init_frame w_118 2 (Memo.from_int 0);
      set_env_slot w_118 0 arg0_100;
      set_env_slot w_118 1 arg1_42;
      w_118.state.c <- pc_to_exp (int_to_pc 6))
    134;
  add_exp
    (fun w_119 ->
      assert_env_length w_119 8;
      resize_frame w_119 12 (Memo.from_int 0);
      let resolved_111 = resolve w_119 (Source.E 7) in
      if Word.get_value (fst resolved_111) <> 0 then (
        let arg0_103 = get_env_slot w_119 1 in
        assert_env_length w_119 12;
        w_119.state.k <-
          Memo.appends [ Memo.from_constructor tag_cont_77; collect_env_slots w_119 [ 0 ]; w_119.state.k ];
        trim_resolved w_119 8;
        init_frame w_119 1 (Memo.from_int 0);
        set_env_slot w_119 0 arg0_103;
        w_119.state.c <- pc_to_exp (int_to_pc 41))
      else
        let arg0_101 = get_env_slot w_119 5 in
        let arg1_43 = get_env_slot w_119 3 in
        assert_env_length w_119 12;
        w_119.state.k <- Memo.appends [ Memo.from_constructor tag_cont_76; collect_env_slots w_119 []; w_119.state.k ];
        trim_resolved w_119 8;
        init_frame w_119 2 (Memo.from_int 0);
        set_env_slot w_119 0 arg0_101;
        set_env_slot w_119 1 arg1_43;
        w_119.state.c <- pc_to_exp (int_to_pc 21))
    135;
  add_exp
    (fun w_120 ->
      assert_env_length w_120 11;
      resize_frame w_120 12 (Memo.from_int 0);
      let arg0_102 = get_env_slot w_120 10 in
      assert_env_length w_120 12;
      trim_resolved w_120 11;
      init_frame w_120 1 (Memo.from_int 0);
      set_env_slot w_120 0 arg0_102;
      w_120.state.c <- pc_to_exp (int_to_pc 41))
    136;
  add_exp
    (fun w_121 ->
      assert_env_length w_121 9;
      resize_frame w_121 12 (Memo.from_int 0);
      assert_env_length w_121 12;
      set_env_slot w_121 9
        (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_121 0; get_env_slot w_121 8 ]);
      trim_resolved w_121 9;
      return_value w_121 (get_env_slot w_121 9) (pc_to_exp (int_to_pc 0)))
    137;
  add_exp
    (fun w_124 ->
      assert_env_length w_124 7;
      resize_frame w_124 17 (Memo.from_int 0);
      assert_env_length w_124 17;
      set_env_slot w_124 7
        (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_124 6; Memo.from_constructor tag_ENil ]);
      trim_resolved w_124 7;
      return_value w_124 (get_env_slot w_124 7) (pc_to_exp (int_to_pc 0)))
    138;
  add_exp
    (fun w_125 ->
      assert_env_length w_125 9;
      resize_frame w_125 17 (Memo.from_int 0);
      let arg0_106 = get_env_slot w_125 3 in
      assert_env_length w_125 17;
      w_125.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_84; collect_env_slots w_125 [ 0; 1; 4; 8 ]; w_125.state.k ];
      trim_resolved w_125 9;
      init_frame w_125 1 (Memo.from_int 0);
      set_env_slot w_125 0 arg0_106;
      w_125.state.c <- pc_to_exp (int_to_pc 12))
    139;
  add_exp
    (fun w_126 ->
      assert_env_length w_126 10;
      resize_frame w_126 17 (Memo.from_int 0);
      let arg0_107 = get_env_slot w_126 0 in
      let arg1_45 = get_env_slot w_126 8 in
      assert_env_length w_126 17;
      w_126.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_83; collect_env_slots w_126 [ 0; 1; 4; 8; 9 ]; w_126.state.k ];
      trim_resolved w_126 10;
      init_frame w_126 2 (Memo.from_int 0);
      set_env_slot w_126 0 arg0_107;
      set_env_slot w_126 1 arg1_45;
      w_126.state.c <- pc_to_exp (int_to_pc 6))
    140;
  add_exp
    (fun w_127 ->
      assert_env_length w_127 11;
      resize_frame w_127 17 (Memo.from_int 0);
      let resolved_116 = resolve w_127 (Source.E 10) in
      if Word.get_value (fst resolved_116) <> 0 then (
        assert_env_length w_127 17;
        let resolved_119 = resolve w_127 (Source.E 1) in
        let resolved_120 = resolve w_127 (Source.E 9) in
        set_env_slot w_127 11 (Memo.from_int (Word.get_value (fst resolved_119) + Word.get_value (fst resolved_120)));
        let arg0_111 = get_env_slot w_127 0 in
        let arg1_49 = get_env_slot w_127 11 in
        let arg2_6 = get_env_slot w_127 4 in
        assert_env_length w_127 17;
        trim_resolved w_127 11;
        init_frame w_127 3 (Memo.from_int 0);
        set_env_slot w_127 0 arg0_111;
        set_env_slot w_127 1 arg1_49;
        set_env_slot w_127 2 arg2_6;
        w_127.state.c <- pc_to_exp (int_to_pc 38))
      else
        let arg0_108 = get_env_slot w_127 8 in
        let arg1_46 = get_env_slot w_127 9 in
        let arg2_5 = get_env_slot w_127 4 in
        assert_env_length w_127 17;
        w_127.state.k <-
          Memo.appends [ Memo.from_constructor tag_cont_82; collect_env_slots w_127 [ 0; 1 ]; w_127.state.k ];
        trim_resolved w_127 11;
        init_frame w_127 3 (Memo.from_int 0);
        set_env_slot w_127 0 arg0_108;
        set_env_slot w_127 1 arg1_46;
        set_env_slot w_127 2 arg2_5;
        w_127.state.c <- pc_to_exp (int_to_pc 38))
    141;
  add_exp
    (fun w_128 ->
      assert_env_length w_128 14;
      resize_frame w_128 17 (Memo.from_int 0);
      assert_env_length w_128 17;
      let resolved_117 = resolve w_128 (Source.E 1) in
      set_env_slot w_128 14
        (Memo.from_int
           (if Word.get_value (fst resolved_117) = Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_118 = resolve w_128 (Source.E 14) in
      if Word.get_value (fst resolved_118) <> 0 then (
        trim_resolved w_128 14;
        return_value w_128 (get_env_slot w_128 13) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_109 = get_env_slot w_128 1 in
        let arg1_47 = get_env_slot w_128 0 in
        assert_env_length w_128 17;
        w_128.state.k <-
          Memo.appends [ Memo.from_constructor tag_cont_81; collect_env_slots w_128 [ 13 ]; w_128.state.k ];
        trim_resolved w_128 14;
        init_frame w_128 2 (Memo.from_int 0);
        set_env_slot w_128 0 arg0_109;
        set_env_slot w_128 1 arg1_47;
        w_128.state.c <- pc_to_exp (int_to_pc 9))
    142;
  add_exp
    (fun w_129 ->
      assert_env_length w_129 16;
      resize_frame w_129 17 (Memo.from_int 0);
      let arg0_110 = get_env_slot w_129 15 in
      let arg1_48 = get_env_slot w_129 13 in
      assert_env_length w_129 17;
      trim_resolved w_129 16;
      init_frame w_129 2 (Memo.from_int 0);
      set_env_slot w_129 0 arg0_110;
      set_env_slot w_129 1 arg1_48;
      w_129.state.c <- pc_to_exp (int_to_pc 21))
    143;
  add_exp
    (fun w_131 ->
      assert_env_length w_131 5;
      resize_frame w_131 6 (Memo.from_int 0);
      assert_env_length w_131 6;
      set_env_slot w_131 5
        (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_131 2; get_env_slot w_131 4 ]);
      trim_resolved w_131 5;
      return_value w_131 (get_env_slot w_131 5) (pc_to_exp (int_to_pc 0)))
    144;
  add_exp
    (fun w_133 ->
      assert_env_length w_133 7;
      resize_frame w_133 15 (Memo.from_int 0);
      let resolved_122 = resolve w_133 (Source.E 6) in
      if Word.get_value (fst resolved_122) <> 0 then (
        assert_env_length w_133 15;
        set_env_slot w_133 7 (Memo.appends [ Memo.from_constructor tag_Const; Memo.from_int 1 ]);
        assert_env_length w_133 15;
        set_env_slot w_133 8 (Memo.appends [ Memo.from_constructor tag_Found; get_env_slot w_133 7 ]);
        trim_resolved w_133 7;
        return_value w_133 (get_env_slot w_133 8) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_133 15;
        let resolved_123 = resolve w_133 (Source.E 1) in
        let tag_46 = Word.get_value (fst resolved_123) in
        match tag_46 with
        | 8 (* tag_Mul *) ->
            let parts_63 = Memo.splits (snd resolved_123) in
            if List.length parts_63 = 2 then (
              let part0_63 = List.nth parts_63 0 in
              let part1_36 = List.nth parts_63 1 in
              set_env_slot w_133 2 part0_63;
              set_env_slot w_133 3 part1_36;
              let arg0_114 = get_env_slot w_133 0 in
              let arg1_52 = get_env_slot w_133 2 in
              assert_env_length w_133 15;
              w_133.state.k <-
                Memo.appends
                  [ Memo.from_constructor tag_cont_88; collect_env_slots w_133 [ 0; 2; 3; 4; 5 ]; w_133.state.k ];
              trim_resolved w_133 7;
              init_frame w_133 2 (Memo.from_int 0);
              set_env_slot w_133 0 arg0_114;
              set_env_slot w_133 1 arg1_52;
              w_133.state.c <- pc_to_exp (int_to_pc 15))
            else (
              trim_resolved w_133 7;
              return_value w_133 (Memo.from_constructor tag_Missing) (pc_to_exp (int_to_pc 0)))
        | _ ->
            trim_resolved w_133 7;
            return_value w_133 (Memo.from_constructor tag_Missing) (pc_to_exp (int_to_pc 0))))
    145;
  add_exp
    (fun w_134 ->
      assert_env_length w_134 10;
      resize_frame w_134 15 (Memo.from_int 0);
      assert_env_length w_134 15;
      let resolved_124 = resolve w_134 (Source.E 9) in
      let tag_47 = Word.get_value (fst resolved_124) in
      match tag_47 with
      | 10 (* tag_Found *) ->
          let parts_64 = Memo.splits (snd resolved_124) in
          if List.length parts_64 = 1 then (
            let part0_64 = List.nth parts_64 0 in
            set_env_slot w_134 4 part0_64;
            assert_env_length w_134 15;
            set_env_slot w_134 10
              (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_134 4; get_env_slot w_134 3 ]);
            assert_env_length w_134 15;
            set_env_slot w_134 11 (Memo.appends [ Memo.from_constructor tag_Found; get_env_slot w_134 10 ]);
            trim_resolved w_134 10;
            return_value w_134 (get_env_slot w_134 11) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (146)"
      | 9 (* tag_Missing *) ->
          let arg0_115 = get_env_slot w_134 0 in
          let arg1_53 = get_env_slot w_134 3 in
          assert_env_length w_134 15;
          w_134.state.k <-
            Memo.appends [ Memo.from_constructor tag_cont_87; collect_env_slots w_134 [ 2; 5 ]; w_134.state.k ];
          trim_resolved w_134 10;
          init_frame w_134 2 (Memo.from_int 0);
          set_env_slot w_134 0 arg0_115;
          set_env_slot w_134 1 arg1_53;
          w_134.state.c <- pc_to_exp (int_to_pc 15)
      | _ -> failwith "unreachable (146)")
    146;
  add_exp
    (fun w_135 ->
      assert_env_length w_135 13;
      resize_frame w_135 15 (Memo.from_int 0);
      assert_env_length w_135 15;
      let resolved_125 = resolve w_135 (Source.E 12) in
      let tag_48 = Word.get_value (fst resolved_125) in
      match tag_48 with
      | 10 (* tag_Found *) ->
          let parts_65 = Memo.splits (snd resolved_125) in
          if List.length parts_65 = 1 then (
            let part0_65 = List.nth parts_65 0 in
            set_env_slot w_135 5 part0_65;
            assert_env_length w_135 15;
            set_env_slot w_135 13
              (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_135 2; get_env_slot w_135 5 ]);
            assert_env_length w_135 15;
            set_env_slot w_135 14 (Memo.appends [ Memo.from_constructor tag_Found; get_env_slot w_135 13 ]);
            trim_resolved w_135 13;
            return_value w_135 (get_env_slot w_135 14) (pc_to_exp (int_to_pc 0)))
          else failwith "unreachable (147)"
      | 9 (* tag_Missing *) ->
          trim_resolved w_135 13;
          return_value w_135 (Memo.from_constructor tag_Missing) (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (147)")
    147;
  add_exp
    (fun w_138 ->
      assert_env_length w_138 7;
      resize_frame w_138 14 (Memo.from_int 0);
      let arg0_117 = get_env_slot w_138 2 in
      assert_env_length w_138 14;
      w_138.state.k <- Memo.appends [ Memo.from_constructor tag_cont_90; collect_env_slots w_138 [ 6 ]; w_138.state.k ];
      trim_resolved w_138 7;
      init_frame w_138 1 (Memo.from_int 0);
      set_env_slot w_138 0 arg0_117;
      w_138.state.c <- pc_to_exp (int_to_pc 54))
    148;
  add_exp
    (fun w_139 ->
      assert_env_length w_139 8;
      resize_frame w_139 14 (Memo.from_int 0);
      assert_env_length w_139 14;
      set_env_slot w_139 8 (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_139 6; get_env_slot w_139 7 ]);
      trim_resolved w_139 8;
      return_value w_139 (get_env_slot w_139 8) (pc_to_exp (int_to_pc 0)))
    149;
  add_exp
    (fun w_140 ->
      assert_env_length w_140 10;
      resize_frame w_140 14 (Memo.from_int 0);
      assert_env_length w_140 14;
      set_env_slot w_140 10 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_140 9; get_env_slot w_140 4 ]);
      let arg0_119 = get_env_slot w_140 4 in
      assert_env_length w_140 14;
      w_140.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_92; collect_env_slots w_140 [ 3; 10 ]; w_140.state.k ];
      trim_resolved w_140 10;
      init_frame w_140 1 (Memo.from_int 0);
      set_env_slot w_140 0 arg0_119;
      w_140.state.c <- pc_to_exp (int_to_pc 54))
    150;
  add_exp
    (fun w_141 ->
      assert_env_length w_141 12;
      resize_frame w_141 14 (Memo.from_int 0);
      assert_env_length w_141 14;
      set_env_slot w_141 12
        (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_141 3; get_env_slot w_141 11 ]);
      assert_env_length w_141 14;
      set_env_slot w_141 13
        (Memo.appends [ Memo.from_constructor tag_Add; get_env_slot w_141 10; get_env_slot w_141 12 ]);
      trim_resolved w_141 12;
      return_value w_141 (get_env_slot w_141 13) (pc_to_exp (int_to_pc 0)))
    151;
  add_exp
    (fun w_146 ->
      assert_env_length w_146 6;
      resize_frame w_146 13 (Memo.from_int 0);
      assert_env_length w_146 13;
      let resolved_132 = resolve w_146 (Source.E 5) in
      set_env_slot w_146 6
        (Memo.from_int (Word.get_value (Memo.to_word (Memo.from_int 1)) + Word.get_value (fst resolved_132)));
      let arg0_121 = get_env_slot w_146 2 in
      assert_env_length w_146 13;
      w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_94; collect_env_slots w_146 [ 6 ]; w_146.state.k ];
      trim_resolved w_146 6;
      init_frame w_146 1 (Memo.from_int 0);
      set_env_slot w_146 0 arg0_121;
      w_146.state.c <- pc_to_exp (int_to_pc 7))
    152;
  add_exp
    (fun w_147 ->
      assert_env_length w_147 8;
      resize_frame w_147 13 (Memo.from_int 0);
      assert_env_length w_147 13;
      let resolved_133 = resolve w_147 (Source.E 6) in
      let resolved_134 = resolve w_147 (Source.E 7) in
      set_env_slot w_147 8 (Memo.from_int (Word.get_value (fst resolved_133) + Word.get_value (fst resolved_134)));
      trim_resolved w_147 8;
      return_value w_147 (get_env_slot w_147 8) (pc_to_exp (int_to_pc 0)))
    153;
  add_exp
    (fun w_148 ->
      assert_env_length w_148 10;
      resize_frame w_148 13 (Memo.from_int 0);
      assert_env_length w_148 13;
      let resolved_135 = resolve w_148 (Source.E 9) in
      set_env_slot w_148 10
        (Memo.from_int (Word.get_value (Memo.to_word (Memo.from_int 1)) + Word.get_value (fst resolved_135)));
      let arg0_123 = get_env_slot w_148 4 in
      assert_env_length w_148 13;
      w_148.state.k <- Memo.appends [ Memo.from_constructor tag_cont_96; collect_env_slots w_148 [ 10 ]; w_148.state.k ];
      trim_resolved w_148 10;
      init_frame w_148 1 (Memo.from_int 0);
      set_env_slot w_148 0 arg0_123;
      w_148.state.c <- pc_to_exp (int_to_pc 7))
    154;
  add_exp
    (fun w_149 ->
      assert_env_length w_149 12;
      resize_frame w_149 13 (Memo.from_int 0);
      assert_env_length w_149 13;
      let resolved_136 = resolve w_149 (Source.E 10) in
      let resolved_137 = resolve w_149 (Source.E 11) in
      set_env_slot w_149 12 (Memo.from_int (Word.get_value (fst resolved_136) + Word.get_value (fst resolved_137)));
      trim_resolved w_149 12;
      return_value w_149 (get_env_slot w_149 12) (pc_to_exp (int_to_pc 0)))
    155;
  add_exp
    (fun w_151 ->
      assert_env_length w_151 3;
      resize_frame w_151 8 (Memo.from_int 0);
      let arg0_125 = get_env_slot w_151 1 in
      assert_env_length w_151 8;
      w_151.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_99; collect_env_slots w_151 [ 0; 1; 2 ]; w_151.state.k ];
      trim_resolved w_151 3;
      init_frame w_151 1 (Memo.from_int 0);
      set_env_slot w_151 0 arg0_125;
      w_151.state.c <- pc_to_exp (int_to_pc 7))
    156;
  add_exp
    (fun w_152 ->
      assert_env_length w_152 4;
      resize_frame w_152 8 (Memo.from_int 0);
      assert_env_length w_152 8;
      let resolved_138 = resolve w_152 (Source.E 2) in
      let resolved_139 = resolve w_152 (Source.E 3) in
      set_env_slot w_152 4
        (Memo.from_int (if Word.get_value (fst resolved_138) < Word.get_value (fst resolved_139) then 1 else 0));
      let resolved_140 = resolve w_152 (Source.E 4) in
      if Word.get_value (fst resolved_140) <> 0 then (
        trim_resolved w_152 4;
        return_value w_152 (get_env_slot w_152 0) (pc_to_exp (int_to_pc 0)))
      else (
        assert_env_length w_152 8;
        let resolved_141 = resolve w_152 (Source.E 3) in
        let resolved_142 = resolve w_152 (Source.E 2) in
        set_env_slot w_152 5
          (Memo.from_int (if Word.get_value (fst resolved_141) < Word.get_value (fst resolved_142) then 1 else 0));
        let resolved_143 = resolve w_152 (Source.E 5) in
        if Word.get_value (fst resolved_143) <> 0 then (
          trim_resolved w_152 4;
          return_value w_152 (get_env_slot w_152 1) (pc_to_exp (int_to_pc 0)))
        else
          let arg0_126 = get_env_slot w_152 0 in
          let arg1_54 = get_env_slot w_152 1 in
          assert_env_length w_152 8;
          w_152.state.k <-
            Memo.appends [ Memo.from_constructor tag_cont_98; collect_env_slots w_152 [ 0; 1 ]; w_152.state.k ];
          trim_resolved w_152 4;
          init_frame w_152 2 (Memo.from_int 0);
          set_env_slot w_152 0 arg0_126;
          set_env_slot w_152 1 arg1_54;
          w_152.state.c <- pc_to_exp (int_to_pc 3)))
    157;
  add_exp
    (fun w_153 ->
      assert_env_length w_153 7;
      resize_frame w_153 8 (Memo.from_int 0);
      assert_env_length w_153 8;
      let resolved_144 = resolve w_153 (Source.E 6) in
      set_env_slot w_153 7
        (Memo.from_int
           (if Word.get_value (fst resolved_144) <= Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_145 = resolve w_153 (Source.E 7) in
      if Word.get_value (fst resolved_145) <> 0 then (
        trim_resolved w_153 7;
        return_value w_153 (get_env_slot w_153 0) (pc_to_exp (int_to_pc 0)))
      else (
        trim_resolved w_153 7;
        return_value w_153 (get_env_slot w_153 1) (pc_to_exp (int_to_pc 0))))
    158;
  add_exp
    (fun w_155 ->
      assert_env_length w_155 8;
      resize_frame w_155 23 (Memo.from_int 0);
      let arg0_128 = get_env_slot w_155 2 in
      let arg1_56 = get_env_slot w_155 4 in
      assert_env_length w_155 23;
      w_155.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_106; collect_env_slots w_155 [ 2; 7 ]; w_155.state.k ];
      trim_resolved w_155 8;
      init_frame w_155 2 (Memo.from_int 0);
      set_env_slot w_155 0 arg0_128;
      set_env_slot w_155 1 arg1_56;
      w_155.state.c <- pc_to_exp (int_to_pc 48))
    159;
  add_exp
    (fun w_156 ->
      assert_env_length w_156 9;
      resize_frame w_156 23 (Memo.from_int 0);
      let arg0_129 = get_env_slot w_156 7 in
      let arg1_57 = get_env_slot w_156 8 in
      assert_env_length w_156 23;
      w_156.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_105; collect_env_slots w_156 [ 2; 7; 8 ]; w_156.state.k ];
      trim_resolved w_156 9;
      init_frame w_156 2 (Memo.from_int 0);
      set_env_slot w_156 1 arg0_129;
      set_env_slot w_156 0 arg1_57;
      w_156.state.c <- pc_to_exp (int_to_pc 47))
    160;
  add_exp
    (fun w_157 ->
      assert_env_length w_157 10;
      resize_frame w_157 23 (Memo.from_int 0);
      let arg0_130 = get_env_slot w_157 8 in
      let arg1_58 = get_env_slot w_157 7 in
      assert_env_length w_157 23;
      w_157.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_104; collect_env_slots w_157 [ 2; 9 ]; w_157.state.k ];
      trim_resolved w_157 10;
      init_frame w_157 2 (Memo.from_int 0);
      set_env_slot w_157 1 arg0_130;
      set_env_slot w_157 0 arg1_58;
      w_157.state.c <- pc_to_exp (int_to_pc 47))
    161;
  add_exp
    (fun w_158 ->
      assert_env_length w_158 11;
      resize_frame w_158 23 (Memo.from_int 0);
      let arg0_131 = get_env_slot w_158 9 in
      let arg1_59 = get_env_slot w_158 10 in
      assert_env_length w_158 23;
      w_158.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_103; collect_env_slots w_158 [ 2; 9 ]; w_158.state.k ];
      trim_resolved w_158 11;
      init_frame w_158 2 (Memo.from_int 0);
      set_env_slot w_158 1 arg0_131;
      set_env_slot w_158 0 arg1_59;
      w_158.state.c <- pc_to_exp (int_to_pc 47))
    162;
  add_exp
    (fun w_159 ->
      assert_env_length w_159 12;
      resize_frame w_159 23 (Memo.from_int 0);
      let arg0_132 = get_env_slot w_159 11 in
      let arg1_60 = get_env_slot w_159 9 in
      assert_env_length w_159 23;
      w_159.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_102; collect_env_slots w_159 [ 2; 9 ]; w_159.state.k ];
      trim_resolved w_159 12;
      init_frame w_159 2 (Memo.from_int 0);
      set_env_slot w_159 1 arg0_132;
      set_env_slot w_159 0 arg1_60;
      w_159.state.c <- pc_to_exp (int_to_pc 47))
    163;
  add_exp
    (fun w_160 ->
      assert_env_length w_160 13;
      resize_frame w_160 23 (Memo.from_int 0);
      let arg0_133 = get_env_slot w_160 12 in
      let arg1_61 = get_env_slot w_160 9 in
      assert_env_length w_160 23;
      w_160.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_101; collect_env_slots w_160 [ 2; 9; 12 ]; w_160.state.k ];
      trim_resolved w_160 13;
      init_frame w_160 2 (Memo.from_int 0);
      set_env_slot w_160 0 arg0_133;
      set_env_slot w_160 1 arg1_61;
      w_160.state.c <- pc_to_exp (int_to_pc 6))
    164;
  add_exp
    (fun w_161 ->
      assert_env_length w_161 14;
      resize_frame w_161 23 (Memo.from_int 0);
      let resolved_148 = resolve w_161 (Source.E 13) in
      if Word.get_value (fst resolved_148) <> 0 then (
        trim_resolved w_161 14;
        return_value w_161 (get_env_slot w_161 9) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_134 = get_env_slot w_161 2 in
        let arg1_62 = get_env_slot w_161 12 in
        assert_env_length w_161 23;
        trim_resolved w_161 14;
        init_frame w_161 2 (Memo.from_int 0);
        set_env_slot w_161 0 arg0_134;
        set_env_slot w_161 1 arg1_62;
        w_161.state.c <- pc_to_exp (int_to_pc 48))
    165;
  add_exp
    (fun w_162 ->
      assert_env_length w_162 16;
      resize_frame w_162 23 (Memo.from_int 0);
      let arg0_136 = get_env_slot w_162 2 in
      let arg1_64 = get_env_slot w_162 6 in
      assert_env_length w_162 23;
      w_162.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_113; collect_env_slots w_162 [ 2; 15 ]; w_162.state.k ];
      trim_resolved w_162 16;
      init_frame w_162 2 (Memo.from_int 0);
      set_env_slot w_162 0 arg0_136;
      set_env_slot w_162 1 arg1_64;
      w_162.state.c <- pc_to_exp (int_to_pc 48))
    166;
  add_exp
    (fun w_163 ->
      assert_env_length w_163 17;
      resize_frame w_163 23 (Memo.from_int 0);
      let arg0_137 = get_env_slot w_163 15 in
      let arg1_65 = get_env_slot w_163 16 in
      assert_env_length w_163 23;
      w_163.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_112; collect_env_slots w_163 [ 2; 15; 16 ]; w_163.state.k ];
      trim_resolved w_163 17;
      init_frame w_163 2 (Memo.from_int 0);
      set_env_slot w_163 1 arg0_137;
      set_env_slot w_163 0 arg1_65;
      w_163.state.c <- pc_to_exp (int_to_pc 37))
    167;
  add_exp
    (fun w_164 ->
      assert_env_length w_164 18;
      resize_frame w_164 23 (Memo.from_int 0);
      let arg0_138 = get_env_slot w_164 16 in
      let arg1_66 = get_env_slot w_164 15 in
      assert_env_length w_164 23;
      w_164.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_111; collect_env_slots w_164 [ 2; 17 ]; w_164.state.k ];
      trim_resolved w_164 18;
      init_frame w_164 2 (Memo.from_int 0);
      set_env_slot w_164 1 arg0_138;
      set_env_slot w_164 0 arg1_66;
      w_164.state.c <- pc_to_exp (int_to_pc 37))
    168;
  add_exp
    (fun w_165 ->
      assert_env_length w_165 19;
      resize_frame w_165 23 (Memo.from_int 0);
      let arg0_139 = get_env_slot w_165 17 in
      let arg1_67 = get_env_slot w_165 18 in
      assert_env_length w_165 23;
      w_165.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_110; collect_env_slots w_165 [ 2; 17 ]; w_165.state.k ];
      trim_resolved w_165 19;
      init_frame w_165 2 (Memo.from_int 0);
      set_env_slot w_165 1 arg0_139;
      set_env_slot w_165 0 arg1_67;
      w_165.state.c <- pc_to_exp (int_to_pc 37))
    169;
  add_exp
    (fun w_166 ->
      assert_env_length w_166 20;
      resize_frame w_166 23 (Memo.from_int 0);
      let arg0_140 = get_env_slot w_166 19 in
      let arg1_68 = get_env_slot w_166 17 in
      assert_env_length w_166 23;
      w_166.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_109; collect_env_slots w_166 [ 2; 17 ]; w_166.state.k ];
      trim_resolved w_166 20;
      init_frame w_166 2 (Memo.from_int 0);
      set_env_slot w_166 1 arg0_140;
      set_env_slot w_166 0 arg1_68;
      w_166.state.c <- pc_to_exp (int_to_pc 37))
    170;
  add_exp
    (fun w_167 ->
      assert_env_length w_167 21;
      resize_frame w_167 23 (Memo.from_int 0);
      let arg0_141 = get_env_slot w_167 20 in
      let arg1_69 = get_env_slot w_167 17 in
      assert_env_length w_167 23;
      w_167.state.k <-
        Memo.appends [ Memo.from_constructor tag_cont_108; collect_env_slots w_167 [ 2; 17; 20 ]; w_167.state.k ];
      trim_resolved w_167 21;
      init_frame w_167 2 (Memo.from_int 0);
      set_env_slot w_167 0 arg0_141;
      set_env_slot w_167 1 arg1_69;
      w_167.state.c <- pc_to_exp (int_to_pc 6))
    171;
  add_exp
    (fun w_168 ->
      assert_env_length w_168 22;
      resize_frame w_168 23 (Memo.from_int 0);
      let resolved_149 = resolve w_168 (Source.E 21) in
      if Word.get_value (fst resolved_149) <> 0 then (
        trim_resolved w_168 22;
        return_value w_168 (get_env_slot w_168 17) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_142 = get_env_slot w_168 2 in
        let arg1_70 = get_env_slot w_168 20 in
        assert_env_length w_168 23;
        trim_resolved w_168 22;
        init_frame w_168 2 (Memo.from_int 0);
        set_env_slot w_168 0 arg0_142;
        set_env_slot w_168 1 arg1_70;
        w_168.state.c <- pc_to_exp (int_to_pc 48))
    172;
  add_exp
    (fun w_170 ->
      assert_env_length w_170 3;
      resize_frame w_170 4 (Memo.from_int 0);
      assert_env_length w_170 4;
      set_env_slot w_170 3 (Memo.appends [ Memo.from_constructor tag_Mul; get_env_slot w_170 0; get_env_slot w_170 2 ]);
      trim_resolved w_170 3;
      return_value w_170 (get_env_slot w_170 3) (pc_to_exp (int_to_pc 0)))
    173;
  add_exp
    (fun w_173 ->
      assert_env_length w_173 6;
      resize_frame w_173 10 (Memo.from_int 0);
      assert_env_length w_173 10;
      let resolved_153 = resolve w_173 (Source.E 5) in
      set_env_slot w_173 6
        (Memo.from_int
           (if Word.get_value (fst resolved_153) <= Word.get_value (Memo.to_word (Memo.from_int 0)) then 1 else 0));
      let resolved_154 = resolve w_173 (Source.E 6) in
      if Word.get_value (fst resolved_154) <> 0 then (
        assert_env_length w_173 10;
        set_env_slot w_173 7
          (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_173 0; get_env_slot w_173 1 ]);
        trim_resolved w_173 6;
        return_value w_173 (get_env_slot w_173 7) (pc_to_exp (int_to_pc 0)))
      else
        let arg0_145 = get_env_slot w_173 0 in
        let arg1_72 = get_env_slot w_173 3 in
        assert_env_length w_173 10;
        w_173.state.k <-
          Memo.appends [ Memo.from_constructor tag_cont_116; collect_env_slots w_173 [ 2 ]; w_173.state.k ];
        trim_resolved w_173 6;
        init_frame w_173 2 (Memo.from_int 0);
        set_env_slot w_173 0 arg0_145;
        set_env_slot w_173 1 arg1_72;
        w_173.state.c <- pc_to_exp (int_to_pc 18))
    174;
  add_exp
    (fun w_174 ->
      assert_env_length w_174 9;
      resize_frame w_174 10 (Memo.from_int 0);
      assert_env_length w_174 10;
      set_env_slot w_174 9
        (Memo.appends [ Memo.from_constructor tag_ECons; get_env_slot w_174 2; get_env_slot w_174 8 ]);
      trim_resolved w_174 9;
      return_value w_174 (get_env_slot w_174 9) (pc_to_exp (int_to_pc 0)))
    175;
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
  Words.set_constructor_degree 16 (-2);
  Words.set_constructor_degree 17 (-3);
  Words.set_constructor_degree 18 (-2);
  Words.set_constructor_degree 19 (-2);
  Words.set_constructor_degree 20 (-3);
  Words.set_constructor_degree 21 (-2);
  Words.set_constructor_degree 22 (-1);
  Words.set_constructor_degree 23 (-1);
  Words.set_constructor_degree 24 (-1);
  Words.set_constructor_degree 25 (-1);
  Words.set_constructor_degree 26 (-1);
  Words.set_constructor_degree 27 (-1);
  Words.set_constructor_degree 28 (-1);
  Words.set_constructor_degree 29 (-1);
  Words.set_constructor_degree 30 0;
  Words.set_constructor_degree 31 0;
  Words.set_constructor_degree 32 0;
  Words.set_constructor_degree 33 0;
  Words.set_constructor_degree 34 (-1);
  Words.set_constructor_degree 35 (-1);
  Words.set_constructor_degree 36 (-1);
  Words.set_constructor_degree 37 (-1);
  Words.set_constructor_degree 38 (-1);
  Words.set_constructor_degree 39 0;
  Words.set_constructor_degree 40 0;
  Words.set_constructor_degree 41 0;
  Words.set_constructor_degree 42 (-1);
  Words.set_constructor_degree 43 (-1);
  Words.set_constructor_degree 44 (-1);
  Words.set_constructor_degree 45 (-1);
  Words.set_constructor_degree 46 (-1);
  Words.set_constructor_degree 47 (-2);
  Words.set_constructor_degree 48 (-2);
  Words.set_constructor_degree 49 (-3);
  Words.set_constructor_degree 50 (-2);
  Words.set_constructor_degree 51 (-1);
  Words.set_constructor_degree 52 0;
  Words.set_constructor_degree 53 (-4);
  Words.set_constructor_degree 54 (-1);
  Words.set_constructor_degree 55 (-4);
  Words.set_constructor_degree 56 0;
  Words.set_constructor_degree 57 0;
  Words.set_constructor_degree 58 (-1);
  Words.set_constructor_degree 59 (-1);
  Words.set_constructor_degree 60 (-3);
  Words.set_constructor_degree 61 (-6);
  Words.set_constructor_degree 62 (-5);
  Words.set_constructor_degree 63 (-1);
  Words.set_constructor_degree 64 (-1);
  Words.set_constructor_degree 65 (-1);
  Words.set_constructor_degree 66 (-1);
  Words.set_constructor_degree 67 (-1);
  Words.set_constructor_degree 68 (-1);
  Words.set_constructor_degree 69 (-2);
  Words.set_constructor_degree 70 (-1);
  Words.set_constructor_degree 71 (-2);
  Words.set_constructor_degree 72 (-1);
  Words.set_constructor_degree 73 (-3);
  Words.set_constructor_degree 74 (-1);
  Words.set_constructor_degree 75 (-3);
  Words.set_constructor_degree 76 (-5);
  Words.set_constructor_degree 77 (-6);
  Words.set_constructor_degree 78 (-1);
  Words.set_constructor_degree 79 0;
  Words.set_constructor_degree 80 0;
  Words.set_constructor_degree 81 0;
  Words.set_constructor_degree 82 (-1);
  Words.set_constructor_degree 83 (-1);
  Words.set_constructor_degree 84 (-1);
  Words.set_constructor_degree 85 0;
  Words.set_constructor_degree 86 (-1);
  Words.set_constructor_degree 87 (-1);
  Words.set_constructor_degree 88 (-1);
  Words.set_constructor_degree 89 (-1);
  Words.set_constructor_degree 90 (-1);
  Words.set_constructor_degree 91 0;
  Words.set_constructor_degree 92 (-1);
  Words.set_constructor_degree 93 (-4);
  Words.set_constructor_degree 94 (-4);
  Words.set_constructor_degree 95 0;
  Words.set_constructor_degree 96 (-1);
  Words.set_constructor_degree 97 (-2);
  Words.set_constructor_degree 98 (-5);
  Words.set_constructor_degree 99 (-4);
  Words.set_constructor_degree 100 (-4);
  Words.set_constructor_degree 101 (-1);
  Words.set_constructor_degree 102 (-2);
  Words.set_constructor_degree 103 (-5);
  Words.set_constructor_degree 104 (-2);
  Words.set_constructor_degree 105 (-1);
  Words.set_constructor_degree 106 (-1);
  Words.set_constructor_degree 107 (-2);
  Words.set_constructor_degree 108 (-2);
  Words.set_constructor_degree 109 (-1);
  Words.set_constructor_degree 110 (-1);
  Words.set_constructor_degree 111 (-1);
  Words.set_constructor_degree 112 (-1);
  Words.set_constructor_degree 113 (-2);
  Words.set_constructor_degree 114 (-3);
  Words.set_constructor_degree 115 (-2);
  Words.set_constructor_degree 116 (-3);
  Words.set_constructor_degree 117 (-2);
  Words.set_constructor_degree 118 (-2);
  Words.set_constructor_degree 119 (-2);
  Words.set_constructor_degree 120 (-3);
  Words.set_constructor_degree 121 (-2);
  Words.set_constructor_degree 122 (-2);
  Words.set_constructor_degree 123 (-3);
  Words.set_constructor_degree 124 (-2);
  Words.set_constructor_degree 125 (-2);
  Words.set_constructor_degree 126 (-2);
  Words.set_constructor_degree 127 (-3);
  Words.set_constructor_degree 128 (-2);
  Words.set_constructor_degree 129 (-2);
  Words.set_constructor_degree 130 (-1);
  Words.set_constructor_degree 131 (-1);
  Words.set_constructor_degree 132 (-4)
