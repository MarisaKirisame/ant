open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_Unit = 3
let tag_None = 4
let tag_Some = 5
let tag_SLambda = 6
let tag_SDefine = 7
let tag_SQuote = 8
let tag_SEq = 9
let tag_SCons = 10
let tag_SCond = 11
let tag_SAtom = 12
let tag_SCar = 13
let tag_SCdr = 14
let tag_AVar = 15
let tag_ANumber = 16
let tag_ASymbol = 17
let tag_ANIL = 18
let tag_EAtom = 19
let tag_ECons = 20
let tag_VNumber = 21
let tag_VSymbol = 22
let tag_VQuote = 23
let tag_VNIL = 24
let tag_VCons = 25
let tag_VClosure = 26
let tag_EnvEntry = 27
let tag_MkEnv = 28
let tag_cont_1 = 29
let tag_cont_2 = 30
let tag_cont_3 = 31
let tag_cont_4 = 32
let tag_cont_5 = 33
let tag_cont_6 = 34
let tag_cont_7 = 35
let tag_cont_8 = 36
let tag_cont_9 = 37
let tag_cont_10 = 38
let tag_cont_11 = 39
let tag_cont_12 = 40
let tag_cont_13 = 41
let tag_cont_14 = 42
let tag_cont_15 = 43
let tag_cont_16 = 44
let tag_cont_17 = 45
let tag_cont_18 = 46
let tag_cont_19 = 47
let tag_cont_20 = 48
let tag_cont_21 = 49
let tag_cont_22 = 50
let tag_cont_23 = 51
let tag_cont_24 = 52
let tag_cont_25 = 53
let tag_cont_26 = 54
let tag_cont_27 = 55
let tag_cont_28 = 56
let tag_cont_29 = 57
let tag_cont_30 = 58
let tag_cont_31 = 59
let tag_cont_32 = 60
let tag_cont_33 = 61
let tag_cont_34 = 62
let tag_cont_35 = 63
let tag_cont_36 = 64
let tag_cont_37 = 65
let tag_cont_38 = 66
let tag_cont_39 = 67
let tag_cont_40 = 68
let tag_cont_41 = 69
let tag_cont_42 = 70
let tag_cont_43 = 71
let tag_cont_44 = 72
let tag_cont_45 = 73
let tag_cont_46 = 74
let tag_cont_47 = 75
let tag_cont_48 = 76
let tag_cont_49 = 77
let tag_cont_50 = 78
let tag_cont_51 = 79
let tag_cont_52 = 80
let tag_cont_53 = 81
let tag_cont_54 = 82
let tag_cont_55 = 83
let tag_cont_56 = 84
let tag_cont_57 = 85

type 'a list = Nil | Cons of 'a * 'a list

let rec from_ocaml_list from_generic_a x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) ->
      Memo.appends [ Memo.from_constructor tag_Cons; from_generic_a x0; from_ocaml_list (fun x -> from_generic_a x) x1 ]

let rec to_ocaml_list to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_Nil -> Nil
  | c when c = tag_Cons ->
      let x0, x1 = Memo.splits_2 t in
      Cons (to_generic_a x0, to_ocaml_list (fun x -> to_generic_a x) x1)
  | _ -> failwith "unreachable"

let rec list_length_tc memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec list_length memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

type unit = Unit

let rec from_ocaml_unit x = match x with Unit -> Memo.appends [ Memo.from_constructor tag_Unit ]

let rec to_ocaml_unit x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with c when c = tag_Unit -> Unit | _ -> failwith "unreachable"

type 'a option = None | Some of 'a

let rec from_ocaml_option from_generic_a x =
  match x with
  | None -> Memo.appends [ Memo.from_constructor tag_None ]
  | Some x0 -> Memo.appends [ Memo.from_constructor tag_Some; from_generic_a x0 ]

let rec to_ocaml_option to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_None -> None
  | c when c = tag_Some ->
      let x0 = Memo.splits_1 t in
      Some (to_generic_a x0)
  | _ -> failwith "unreachable"

type symbol = SLambda | SDefine | SQuote | SEq | SCons | SCond | SAtom | SCar | SCdr

let rec from_ocaml_symbol x =
  match x with
  | SLambda -> Memo.appends [ Memo.from_constructor tag_SLambda ]
  | SDefine -> Memo.appends [ Memo.from_constructor tag_SDefine ]
  | SQuote -> Memo.appends [ Memo.from_constructor tag_SQuote ]
  | SEq -> Memo.appends [ Memo.from_constructor tag_SEq ]
  | SCons -> Memo.appends [ Memo.from_constructor tag_SCons ]
  | SCond -> Memo.appends [ Memo.from_constructor tag_SCond ]
  | SAtom -> Memo.appends [ Memo.from_constructor tag_SAtom ]
  | SCar -> Memo.appends [ Memo.from_constructor tag_SCar ]
  | SCdr -> Memo.appends [ Memo.from_constructor tag_SCdr ]

let rec to_ocaml_symbol x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_SLambda -> SLambda
  | c when c = tag_SDefine -> SDefine
  | c when c = tag_SQuote -> SQuote
  | c when c = tag_SEq -> SEq
  | c when c = tag_SCons -> SCons
  | c when c = tag_SCond -> SCond
  | c when c = tag_SAtom -> SAtom
  | c when c = tag_SCar -> SCar
  | c when c = tag_SCdr -> SCdr
  | _ -> failwith "unreachable"

type atom = AVar of int | ANumber of int | ASymbol of symbol | ANIL

let rec from_ocaml_atom x =
  match x with
  | AVar x0 -> Memo.appends [ Memo.from_constructor tag_AVar; Memo.from_int x0 ]
  | ANumber x0 -> Memo.appends [ Memo.from_constructor tag_ANumber; Memo.from_int x0 ]
  | ASymbol x0 -> Memo.appends [ Memo.from_constructor tag_ASymbol; from_ocaml_symbol x0 ]
  | ANIL -> Memo.appends [ Memo.from_constructor tag_ANIL ]

let rec to_ocaml_atom x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_AVar ->
      let x0 = Memo.splits_1 t in
      AVar (Word.get_value (Memo.to_word x0))
  | c when c = tag_ANumber ->
      let x0 = Memo.splits_1 t in
      ANumber (Word.get_value (Memo.to_word x0))
  | c when c = tag_ASymbol ->
      let x0 = Memo.splits_1 t in
      ASymbol (to_ocaml_symbol x0)
  | c when c = tag_ANIL -> ANIL
  | _ -> failwith "unreachable"

type expr = EAtom of atom | ECons of expr * expr

let rec from_ocaml_expr x =
  match x with
  | EAtom x0 -> Memo.appends [ Memo.from_constructor tag_EAtom; from_ocaml_atom x0 ]
  | ECons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ECons; from_ocaml_expr x0; from_ocaml_expr x1 ]

let rec to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_EAtom ->
      let x0 = Memo.splits_1 t in
      EAtom (to_ocaml_atom x0)
  | c when c = tag_ECons ->
      let x0, x1 = Memo.splits_2 t in
      ECons (to_ocaml_expr x0, to_ocaml_expr x1)
  | _ -> failwith "unreachable"

type value =
  | VNumber of int
  | VSymbol of symbol
  | VQuote of expr
  | VNIL
  | VCons of value * value
  | VClosure of int * expr * env

and env_entry = EnvEntry of int * value
and env = MkEnv of env_entry list

let rec from_ocaml_value x =
  match x with
  | VNumber x0 -> Memo.appends [ Memo.from_constructor tag_VNumber; Memo.from_int x0 ]
  | VSymbol x0 -> Memo.appends [ Memo.from_constructor tag_VSymbol; from_ocaml_symbol x0 ]
  | VQuote x0 -> Memo.appends [ Memo.from_constructor tag_VQuote; from_ocaml_expr x0 ]
  | VNIL -> Memo.appends [ Memo.from_constructor tag_VNIL ]
  | VCons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_VCons; from_ocaml_value x0; from_ocaml_value x1 ]
  | VClosure (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_VClosure; Memo.from_int x0; from_ocaml_expr x1; from_ocaml_env x2 ]

and from_ocaml_env_entry x =
  match x with
  | EnvEntry (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EnvEntry; Memo.from_int x0; from_ocaml_value x1 ]

and from_ocaml_env x =
  match x with
  | MkEnv x0 -> Memo.appends [ Memo.from_constructor tag_MkEnv; from_ocaml_list (fun x -> from_ocaml_env_entry x) x0 ]

let rec to_ocaml_value x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_VNumber ->
      let x0 = Memo.splits_1 t in
      VNumber (Word.get_value (Memo.to_word x0))
  | c when c = tag_VSymbol ->
      let x0 = Memo.splits_1 t in
      VSymbol (to_ocaml_symbol x0)
  | c when c = tag_VQuote ->
      let x0 = Memo.splits_1 t in
      VQuote (to_ocaml_expr x0)
  | c when c = tag_VNIL -> VNIL
  | c when c = tag_VCons ->
      let x0, x1 = Memo.splits_2 t in
      VCons (to_ocaml_value x0, to_ocaml_value x1)
  | c when c = tag_VClosure ->
      let x0, x1, x2 = Memo.splits_3 t in
      VClosure (Word.get_value (Memo.to_word x0), to_ocaml_expr x1, to_ocaml_env x2)
  | _ -> failwith "unreachable"

and to_ocaml_env_entry x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_EnvEntry ->
      let x0, x1 = Memo.splits_2 t in
      EnvEntry (Word.get_value (Memo.to_word x0), to_ocaml_value x1)
  | _ -> failwith "unreachable"

and to_ocaml_env x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_MkEnv ->
      let x0 = Memo.splits_1 t in
      MkEnv (to_ocaml_list (fun x -> to_ocaml_env_entry x) x0)
  | _ -> failwith "unreachable"

let rec quote_expr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 5)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec env_entry_name memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 6)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec env_entry_value memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 8)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_to_expr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 10)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_symbol memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 12)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_var memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 15)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec const_Nil memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 18)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_false memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 19)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_true memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 20)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_false memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 21)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_true memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 22)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec unwrap_env memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 23)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cons_env memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 25)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec caddr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 27)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cadr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 31)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec caar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 34)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cdar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 37)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cddar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 40)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cadar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 44)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec caddar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 48)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec car_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 53)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cdr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 55)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec symbol_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 57)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec atom_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 68)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 76)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 79)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec env_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 80)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_is_num memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 90)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec car memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 93)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cdr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 96)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_atom_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 99)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_eq_ memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 101)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec lookup memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 102)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec pairlis memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 103)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec evlis memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 104)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec evcon memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 105)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec destruct_names memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 106)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 107)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec evalquote memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 115)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_115 ->
      assert_env_length w_115 1;
      let hd_0, tl_0 = resolve w_115 K in
      match Word.get_value hd_0 with
      | c_82 when c_82 = tag_cont_done -> exec_done w_115
      | c_82 when c_82 = tag_cont_1 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_11 = env_call w_115 [ 1; 2 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_vals_11; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_115
      | c_82 when c_82 = tag_cont_2 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 5;
          let keep_vals_12 = env_call w_115 [ 2 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_vals_12; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 80);
          stepped w_115
      | c_82 when c_82 = tag_cont_3 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          assert_env_length w_115 1;
          let ctor_arg_29 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_VQuote; ctor_arg_29 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_4 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          assert_env_length w_115 1;
          let ctor_arg_30 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_VQuote; ctor_arg_30 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_5 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 116);
          stepped w_115
      | c_82 when c_82 = tag_cont_6 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 117);
          stepped w_115
      | c_82 when c_82 = tag_cont_7 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          ignore (env_call w_115 [] 3);
          w_115.state.c <- pc_to_exp (int_to_pc 103);
          stepped w_115
      | c_82 when c_82 = tag_cont_8 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 5;
          let keep_vals_14 = env_call w_115 [ 2 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_vals_14; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 104);
          stepped w_115
      | c_82 when c_82 = tag_cont_9 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 5;
          let keep_vals_15 = env_call w_115 [ 0; 1; 2 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_vals_15; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_10 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 118);
          stepped w_115
      | c_82 when c_82 = tag_cont_11 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          let keep_vals_17 = env_call w_115 [ 0; 1 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_vals_17; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 12);
          stepped w_115
      | c_82 when c_82 = tag_cont_12 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Memo.from_constructor tag_ANIL);
          assert_env_length w_115 4;
          let ctor_arg_31 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_31 ]);
          assert_env_length w_115 4;
          let ctor_arg_32 = pop_env w_115 in
          let ctor_arg_33 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_33; ctor_arg_32 ]);
          assert_env_length w_115 3;
          let ctor_arg_34 = pop_env w_115 in
          let ctor_arg_35 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_35; ctor_arg_34 ]);
          assert_env_length w_115 2;
          let ctor_arg_36 = pop_env w_115 in
          let ctor_arg_37 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_37; ctor_arg_36 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_13 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 119);
          stepped w_115
      | c_82 when c_82 = tag_cont_14 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          w_115.state.c <- pc_to_exp (int_to_pc 121);
          stepped w_115
      | c_82 when c_82 = tag_cont_15 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          let ctor_arg_39 = pop_env w_115 in
          let ctor_arg_40 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_40; ctor_arg_39 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_16 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          push_env w_115 (Memo.from_constructor tag_Unit);
          assert_env_length w_115 5;
          let keep_vals_18 = env_call w_115 [ 0; 1; 2; 3 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_vals_18; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 21);
          stepped w_115
      | c_82 when c_82 = tag_cont_17 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          let ctor_arg_41 = pop_env w_115 in
          let ctor_arg_42 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_42; ctor_arg_41 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_18 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 123);
          stepped w_115
      | c_82 when c_82 = tag_cont_19 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 4 tl_0;
          assert_env_length w_115 5;
          let keep_vals_27 = env_call w_115 [ 0; 1; 2 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_vals_27; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 79);
          stepped w_115
      | c_82 when c_82 = tag_cont_20 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          assert_env_length w_115 1;
          let ctor_arg_43 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_VQuote; ctor_arg_43 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_21 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          let keep_vals_28 = env_call w_115 [] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_vals_28; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_22 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 4;
          let keep_vals_29 = env_call w_115 [ 0; 1 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_vals_29; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_23 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          let keep_vals_30 = env_call w_115 [] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_vals_30; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_24 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          let keep_vals_31 = env_call w_115 [] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_vals_31; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_25 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 4;
          let keep_vals_32 = env_call w_115 [ 0; 1 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_vals_32; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_26 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          ignore (env_call w_115 [] 2);
          w_115.state.c <- pc_to_exp (int_to_pc 105);
          stepped w_115
      | c_82 when c_82 = tag_cont_27 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 124);
          stepped w_115
      | c_82 when c_82 = tag_cont_28 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 125);
          stepped w_115
      | c_82 when c_82 = tag_cont_29 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          assert_env_length w_115 1;
          ignore (env_call w_115 [] 1);
          w_115.state.c <- pc_to_exp (int_to_pc 99);
          stepped w_115
      | c_82 when c_82 = tag_cont_30 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_36 = env_call w_115 [ 1; 2 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_vals_36; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 27);
          stepped w_115
      | c_82 when c_82 = tag_cont_31 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          assert_env_length w_115 1;
          ignore (env_call w_115 [] 1);
          w_115.state.c <- pc_to_exp (int_to_pc 93);
          stepped w_115
      | c_82 when c_82 = tag_cont_32 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          assert_env_length w_115 1;
          ignore (env_call w_115 [] 1);
          w_115.state.c <- pc_to_exp (int_to_pc 96);
          stepped w_115
      | c_82 when c_82 = tag_cont_33 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_37 = env_call w_115 [ 1; 2 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_vals_37; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 27);
          stepped w_115
      | c_82 when c_82 = tag_cont_34 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          let keep_vals_38 = env_call w_115 [ 0; 1 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_vals_38; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 12);
          stepped w_115
      | c_82 when c_82 = tag_cont_35 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          let keep_vals_39 = env_call w_115 [ 0; 1 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_vals_39; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 15);
          stepped w_115
      | c_82 when c_82 = tag_cont_36 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          ignore (env_call w_115 [] 2);
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_37 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_40 = env_call w_115 [ 1 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_vals_40; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_38 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_41 = env_call w_115 [ 1 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_vals_41; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_39 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 127);
          stepped w_115
      | c_82 when c_82 = tag_cont_40 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_115
      | c_82 when c_82 = tag_cont_41 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          ignore (env_call w_115 [] 2);
          w_115.state.c <- pc_to_exp (int_to_pc 101);
          stepped w_115
      | c_82 when c_82 = tag_cont_42 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          let ctor_arg_46 = pop_env w_115 in
          let ctor_arg_47 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_VCons; ctor_arg_47; ctor_arg_46 ]);
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          drop_n w_115 1 0;
          assert_env_length w_115 1;
          return_n w_115 1 (pc_to_exp (int_to_pc 0))
      | c_82 when c_82 = tag_cont_43 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          let keep_vals_46 = env_call w_115 [ 0; 1 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_vals_46; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 106);
          stepped w_115
      | c_82 when c_82 = tag_cont_44 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          let ctor_arg_48 = pop_env w_115 in
          let ctor_arg_49 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_49; ctor_arg_48 ]);
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_47 = env_call w_115 [ 0; 1; 2 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_vals_47; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 44);
          stepped w_115
      | c_82 when c_82 = tag_cont_45 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 0 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 130);
          stepped w_115
      | c_82 when c_82 = tag_cont_46 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 131);
          stepped w_115
      | c_82 when c_82 = tag_cont_47 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          let keep_vals_49 = env_call w_115 [ 0; 1; 2 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_50; keep_vals_49; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 48);
          stepped w_115
      | c_82 when c_82 = tag_cont_48 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          let keep_vals_50 = env_call w_115 [ 0; 1; 2 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; keep_vals_50; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 90);
          stepped w_115
      | c_82 when c_82 = tag_cont_49 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 5;
          let keep_vals_51 = env_call w_115 [ 2; 3 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_52; keep_vals_51; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 55);
          stepped w_115
      | c_82 when c_82 = tag_cont_50 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 5;
          let keep_vals_52 = env_call w_115 [ 1; 2; 3 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; keep_vals_52; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 55);
          stepped w_115
      | c_82 when c_82 = tag_cont_51 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          w_115.state.c <- pc_to_exp (int_to_pc 132);
          stepped w_115
      | c_82 when c_82 = tag_cont_52 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 2 tl_0;
          assert_env_length w_115 3;
          let ctor_arg_60 = pop_env w_115 in
          let ctor_arg_61 = pop_env w_115 in
          push_env w_115 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_61; ctor_arg_60 ]);
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          ignore (env_call w_115 [] 2);
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_53 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 5;
          let keep_vals_54 = env_call w_115 [ 0; 1; 2 ] 2 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; keep_vals_54; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 104);
          stepped w_115
      | c_82 when c_82 = tag_cont_54 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          let keep_vals_55 = env_call w_115 [ 1 ] 1 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_56; keep_vals_55; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_115
      | c_82 when c_82 = tag_cont_55 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 3 tl_0;
          assert_env_length w_115 4;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 5;
          push_env w_115 (Dynarray.get w_115.state.e 3);
          assert_env_length w_115 6;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 7;
          let keep_vals_56 = env_call w_115 [ 2 ] 3 in
          w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; keep_vals_56; w_115.state.k ];
          w_115.state.c <- pc_to_exp (int_to_pc 103);
          stepped w_115
      | c_82 when c_82 = tag_cont_56 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 4;
          ignore (env_call w_115 [] 2);
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | c_82 when c_82 = tag_cont_57 ->
          w_115.state.k <- get_next_cont tl_0;
          restore_env w_115 1 tl_0;
          assert_env_length w_115 2;
          push_env w_115 (Dynarray.get w_115.state.e 0);
          assert_env_length w_115 3;
          push_env w_115 (Dynarray.get w_115.state.e 1);
          assert_env_length w_115 4;
          ignore (env_call w_115 [] 2);
          w_115.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_115
      | _ -> failwith "unreachable (0)")
    0

let () =
  add_exp
    (fun w_0 ->
      assert_env_length w_0 2;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 3);
      stepped w_0)
    1

let () =
  add_exp
    (fun w_2 ->
      assert_env_length w_2 7;
      let x0_0 = resolve w_2 (Source.E 5) in
      let x1_0 = resolve w_2 (Source.E 6) in
      ignore (pop_env w_2);
      ignore (pop_env w_2);
      push_env w_2 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_2 6;
      ignore (env_call w_2 [] 2);
      w_2.state.c <- pc_to_exp (int_to_pc 1);
      stepped w_2)
    2

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      let last_0 = Source.E 2 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | c_0 when c_0 = tag_Nil ->
          ignore (pop_env w_1);
          assert_env_length w_1 2;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          assert_env_length w_1 3;
          return_n w_1 3 (pc_to_exp (int_to_pc 0))
      | c_0 when c_0 = tag_Cons ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          ignore (pop_env w_1);
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 4;
          push_env w_1 (Dynarray.get w_1.state.e 3);
          assert_env_length w_1 5;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          assert_env_length w_1 6;
          push_env w_1 (Memo.from_int 1);
          w_1.state.c <- pc_to_exp (int_to_pc 2);
          stepped w_1
      | c_0 -> failwith ("unreachable:" ^ string_of_int c_0 ^ "(3)"))
    3

let () =
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      push_env w_3 (Dynarray.get w_3.state.e 0);
      assert_env_length w_3 2;
      push_env w_3 (Memo.from_int 0);
      assert_env_length w_3 3;
      ignore (env_call w_3 [] 2);
      w_3.state.c <- pc_to_exp (int_to_pc 1);
      stepped w_3)
    4

let () =
  add_exp
    (fun w_4 ->
      assert_env_length w_4 1;
      push_env w_4 (Memo.from_constructor tag_SQuote);
      assert_env_length w_4 2;
      let ctor_arg_0 = pop_env w_4 in
      push_env w_4 (Memo.appends [ Memo.from_constructor tag_ASymbol; ctor_arg_0 ]);
      assert_env_length w_4 2;
      let ctor_arg_1 = pop_env w_4 in
      push_env w_4 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_1 ]);
      assert_env_length w_4 2;
      push_env w_4 (Dynarray.get w_4.state.e 0);
      assert_env_length w_4 3;
      let ctor_arg_2 = pop_env w_4 in
      let ctor_arg_3 = pop_env w_4 in
      push_env w_4 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_3; ctor_arg_2 ]);
      assert_env_length w_4 2;
      return_n w_4 2 (pc_to_exp (int_to_pc 0)))
    5

let () =
  add_exp
    (fun w_5 ->
      assert_env_length w_5 1;
      push_env w_5 (Dynarray.get w_5.state.e 0);
      w_5.state.c <- pc_to_exp (int_to_pc 7);
      stepped w_5)
    6

let () =
  add_exp
    (fun w_6 ->
      assert_env_length w_6 2;
      let last_1 = Source.E 1 in
      let x_1 = resolve w_6 last_1 in
      match Word.get_value (fst x_1) with
      | c_1 when c_1 = tag_EnvEntry ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          let split1_1 = List.nth splits_1 1 in
          ignore (pop_env w_6);
          push_env w_6 split0_1;
          push_env w_6 split1_1;
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 4;
          drop_n w_6 4 2;
          assert_env_length w_6 2;
          return_n w_6 2 (pc_to_exp (int_to_pc 0))
      | c_1 -> failwith ("unreachable:" ^ string_of_int c_1 ^ "(7)"))
    7

let () =
  add_exp
    (fun w_7 ->
      assert_env_length w_7 1;
      push_env w_7 (Dynarray.get w_7.state.e 0);
      w_7.state.c <- pc_to_exp (int_to_pc 9);
      stepped w_7)
    8

let () =
  add_exp
    (fun w_8 ->
      assert_env_length w_8 2;
      let last_2 = Source.E 1 in
      let x_2 = resolve w_8 last_2 in
      match Word.get_value (fst x_2) with
      | c_2 when c_2 = tag_EnvEntry ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          let split1_2 = List.nth splits_2 1 in
          ignore (pop_env w_8);
          push_env w_8 split0_2;
          push_env w_8 split1_2;
          assert_env_length w_8 3;
          push_env w_8 (Dynarray.get w_8.state.e 2);
          assert_env_length w_8 4;
          drop_n w_8 4 2;
          assert_env_length w_8 2;
          return_n w_8 2 (pc_to_exp (int_to_pc 0))
      | c_2 -> failwith ("unreachable:" ^ string_of_int c_2 ^ "(9)"))
    9

let () =
  add_exp
    (fun w_9 ->
      assert_env_length w_9 1;
      push_env w_9 (Dynarray.get w_9.state.e 0);
      w_9.state.c <- pc_to_exp (int_to_pc 11);
      stepped w_9)
    10

let () =
  add_exp
    (fun w_10 ->
      assert_env_length w_10 2;
      let last_3 = Source.E 1 in
      let x_3 = resolve w_10 last_3 in
      match Word.get_value (fst x_3) with
      | c_3 when c_3 = tag_VNumber ->
          let splits_3 = Memo.splits (snd x_3) in
          let split0_3 = List.nth splits_3 0 in
          ignore (pop_env w_10);
          push_env w_10 split0_3;
          assert_env_length w_10 2;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 3;
          let ctor_arg_4 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_ANumber; ctor_arg_4 ]);
          assert_env_length w_10 3;
          let ctor_arg_5 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_5 ]);
          assert_env_length w_10 3;
          drop_n w_10 3 1;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | c_3 when c_3 = tag_VSymbol ->
          let splits_4 = Memo.splits (snd x_3) in
          let split0_4 = List.nth splits_4 0 in
          ignore (pop_env w_10);
          push_env w_10 split0_4;
          assert_env_length w_10 2;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 3;
          let ctor_arg_6 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_ASymbol; ctor_arg_6 ]);
          assert_env_length w_10 3;
          let ctor_arg_7 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_7 ]);
          assert_env_length w_10 3;
          drop_n w_10 3 1;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | c_3 when c_3 = tag_VQuote ->
          let splits_5 = Memo.splits (snd x_3) in
          let split0_5 = List.nth splits_5 0 in
          ignore (pop_env w_10);
          push_env w_10 split0_5;
          assert_env_length w_10 2;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 3;
          ignore (env_call w_10 [] 1);
          w_10.state.c <- pc_to_exp (int_to_pc 5);
          stepped w_10
      | c_3 when c_3 = tag_VNIL ->
          ignore (pop_env w_10);
          assert_env_length w_10 1;
          push_env w_10 (Memo.from_constructor tag_ANIL);
          assert_env_length w_10 2;
          let ctor_arg_8 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_8 ]);
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | c_3 when c_3 = tag_VCons ->
          let splits_6 = Memo.splits (snd x_3) in
          let split0_6 = List.nth splits_6 0 in
          let split1_3 = List.nth splits_6 1 in
          ignore (pop_env w_10);
          push_env w_10 split0_6;
          push_env w_10 split1_3;
          assert_env_length w_10 3;
          push_env w_10 (Memo.from_constructor tag_SCons);
          assert_env_length w_10 4;
          let ctor_arg_9 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_ASymbol; ctor_arg_9 ]);
          assert_env_length w_10 4;
          let ctor_arg_10 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_10 ]);
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 5;
          let keep_vals_0 = env_call w_10 [ 2; 3 ] 1 in
          w_10.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_vals_0; w_10.state.k ];
          w_10.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_10
      | c_3 when c_3 = tag_VClosure ->
          let splits_7 = Memo.splits (snd x_3) in
          let split0_7 = List.nth splits_7 0 in
          let split1_4 = List.nth splits_7 1 in
          let split2_0 = List.nth splits_7 2 in
          ignore (pop_env w_10);
          push_env w_10 split0_7;
          push_env w_10 split1_4;
          push_env w_10 split2_0;
          failwith "value_to_expr: unexpected CLOSURE"
      | c_3 -> failwith ("unreachable:" ^ string_of_int c_3 ^ "(11)"))
    11

let () =
  add_exp
    (fun w_11 ->
      assert_env_length w_11 1;
      push_env w_11 (Dynarray.get w_11.state.e 0);
      w_11.state.c <- pc_to_exp (int_to_pc 14);
      stepped w_11)
    12

let () =
  add_exp
    (fun w_13 ->
      assert_env_length w_13 3;
      let last_5 = Source.E 2 in
      let x_5 = resolve w_13 last_5 in
      match Word.get_value (fst x_5) with
      | c_5 when c_5 = tag_ASymbol ->
          let splits_9 = Memo.splits (snd x_5) in
          let split0_9 = List.nth splits_9 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_9;
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 2);
          assert_env_length w_13 4;
          let ctor_arg_11 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_11 ]);
          assert_env_length w_13 4;
          drop_n w_13 4 1;
          assert_env_length w_13 3;
          drop_n w_13 3 1;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_13);
          assert_env_length w_13 2;
          push_env w_13 (Memo.from_constructor tag_None);
          assert_env_length w_13 3;
          drop_n w_13 3 1;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | c_5 -> failwith ("unreachable:" ^ string_of_int c_5 ^ "(13)"))
    13

let () =
  add_exp
    (fun w_12 ->
      assert_env_length w_12 2;
      let last_4 = Source.E 1 in
      let x_4 = resolve w_12 last_4 in
      match Word.get_value (fst x_4) with
      | c_4 when c_4 = tag_EAtom ->
          let splits_8 = Memo.splits (snd x_4) in
          let split0_8 = List.nth splits_8 0 in
          ignore (pop_env w_12);
          push_env w_12 split0_8;
          assert_env_length w_12 2;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          w_12.state.c <- pc_to_exp (int_to_pc 13);
          stepped w_12
      | _ ->
          ignore (pop_env w_12);
          assert_env_length w_12 1;
          push_env w_12 (Memo.from_constructor tag_None);
          assert_env_length w_12 2;
          return_n w_12 2 (pc_to_exp (int_to_pc 0))
      | c_4 -> failwith ("unreachable:" ^ string_of_int c_4 ^ "(14)"))
    14

let () =
  add_exp
    (fun w_14 ->
      assert_env_length w_14 1;
      push_env w_14 (Dynarray.get w_14.state.e 0);
      w_14.state.c <- pc_to_exp (int_to_pc 17);
      stepped w_14)
    15

let () =
  add_exp
    (fun w_16 ->
      assert_env_length w_16 3;
      let last_7 = Source.E 2 in
      let x_7 = resolve w_16 last_7 in
      match Word.get_value (fst x_7) with
      | c_7 when c_7 = tag_AVar ->
          let splits_11 = Memo.splits (snd x_7) in
          let split0_11 = List.nth splits_11 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_11;
          assert_env_length w_16 3;
          push_env w_16 (Dynarray.get w_16.state.e 2);
          assert_env_length w_16 4;
          let ctor_arg_12 = pop_env w_16 in
          push_env w_16 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_12 ]);
          assert_env_length w_16 4;
          drop_n w_16 4 1;
          assert_env_length w_16 3;
          drop_n w_16 3 1;
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_16);
          assert_env_length w_16 2;
          push_env w_16 (Memo.from_constructor tag_None);
          assert_env_length w_16 3;
          drop_n w_16 3 1;
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | c_7 -> failwith ("unreachable:" ^ string_of_int c_7 ^ "(16)"))
    16

let () =
  add_exp
    (fun w_15 ->
      assert_env_length w_15 2;
      let last_6 = Source.E 1 in
      let x_6 = resolve w_15 last_6 in
      match Word.get_value (fst x_6) with
      | c_6 when c_6 = tag_EAtom ->
          let splits_10 = Memo.splits (snd x_6) in
          let split0_10 = List.nth splits_10 0 in
          ignore (pop_env w_15);
          push_env w_15 split0_10;
          assert_env_length w_15 2;
          push_env w_15 (Dynarray.get w_15.state.e 1);
          w_15.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_15
      | _ ->
          ignore (pop_env w_15);
          assert_env_length w_15 1;
          push_env w_15 (Memo.from_constructor tag_None);
          assert_env_length w_15 2;
          return_n w_15 2 (pc_to_exp (int_to_pc 0))
      | c_6 -> failwith ("unreachable:" ^ string_of_int c_6 ^ "(17)"))
    17

let () =
  add_exp
    (fun w_17 ->
      assert_env_length w_17 1;
      push_env w_17 (Memo.from_constructor tag_ANIL);
      assert_env_length w_17 2;
      let ctor_arg_13 = pop_env w_17 in
      push_env w_17 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_13 ]);
      assert_env_length w_17 2;
      return_n w_17 2 (pc_to_exp (int_to_pc 0)))
    18

let () =
  add_exp
    (fun w_18 ->
      assert_env_length w_18 1;
      push_env w_18 (Memo.from_constructor tag_ANIL);
      assert_env_length w_18 2;
      let ctor_arg_14 = pop_env w_18 in
      push_env w_18 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_14 ]);
      assert_env_length w_18 2;
      return_n w_18 2 (pc_to_exp (int_to_pc 0)))
    19

let () =
  add_exp
    (fun w_19 ->
      assert_env_length w_19 1;
      push_env w_19 (Memo.from_int 0);
      assert_env_length w_19 2;
      let ctor_arg_15 = pop_env w_19 in
      push_env w_19 (Memo.appends [ Memo.from_constructor tag_ANumber; ctor_arg_15 ]);
      assert_env_length w_19 2;
      let ctor_arg_16 = pop_env w_19 in
      push_env w_19 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_16 ]);
      assert_env_length w_19 2;
      return_n w_19 2 (pc_to_exp (int_to_pc 0)))
    20

let () =
  add_exp
    (fun w_20 ->
      assert_env_length w_20 1;
      push_env w_20 (Memo.from_constructor tag_VNIL);
      assert_env_length w_20 2;
      return_n w_20 2 (pc_to_exp (int_to_pc 0)))
    21

let () =
  add_exp
    (fun w_21 ->
      assert_env_length w_21 1;
      push_env w_21 (Memo.from_int 0);
      assert_env_length w_21 2;
      let ctor_arg_17 = pop_env w_21 in
      push_env w_21 (Memo.appends [ Memo.from_constructor tag_VNumber; ctor_arg_17 ]);
      assert_env_length w_21 2;
      return_n w_21 2 (pc_to_exp (int_to_pc 0)))
    22

let () =
  add_exp
    (fun w_22 ->
      assert_env_length w_22 1;
      push_env w_22 (Dynarray.get w_22.state.e 0);
      w_22.state.c <- pc_to_exp (int_to_pc 24);
      stepped w_22)
    23

let () =
  add_exp
    (fun w_23 ->
      assert_env_length w_23 2;
      let last_8 = Source.E 1 in
      let x_8 = resolve w_23 last_8 in
      match Word.get_value (fst x_8) with
      | c_8 when c_8 = tag_MkEnv ->
          let splits_12 = Memo.splits (snd x_8) in
          let split0_12 = List.nth splits_12 0 in
          ignore (pop_env w_23);
          push_env w_23 split0_12;
          assert_env_length w_23 2;
          push_env w_23 (Dynarray.get w_23.state.e 1);
          assert_env_length w_23 3;
          drop_n w_23 3 1;
          assert_env_length w_23 2;
          return_n w_23 2 (pc_to_exp (int_to_pc 0))
      | c_8 -> failwith ("unreachable:" ^ string_of_int c_8 ^ "(24)"))
    24

let () =
  add_exp
    (fun w_24 ->
      assert_env_length w_24 2;
      push_env w_24 (Dynarray.get w_24.state.e 1);
      w_24.state.c <- pc_to_exp (int_to_pc 26);
      stepped w_24)
    25

let () =
  add_exp
    (fun w_25 ->
      assert_env_length w_25 3;
      let last_9 = Source.E 2 in
      let x_9 = resolve w_25 last_9 in
      match Word.get_value (fst x_9) with
      | c_9 when c_9 = tag_MkEnv ->
          let splits_13 = Memo.splits (snd x_9) in
          let split0_13 = List.nth splits_13 0 in
          ignore (pop_env w_25);
          push_env w_25 split0_13;
          assert_env_length w_25 3;
          push_env w_25 (Dynarray.get w_25.state.e 0);
          assert_env_length w_25 4;
          push_env w_25 (Dynarray.get w_25.state.e 2);
          assert_env_length w_25 5;
          let ctor_arg_18 = pop_env w_25 in
          let ctor_arg_19 = pop_env w_25 in
          push_env w_25 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_19; ctor_arg_18 ]);
          assert_env_length w_25 4;
          let ctor_arg_20 = pop_env w_25 in
          push_env w_25 (Memo.appends [ Memo.from_constructor tag_MkEnv; ctor_arg_20 ]);
          assert_env_length w_25 4;
          drop_n w_25 4 1;
          assert_env_length w_25 3;
          return_n w_25 3 (pc_to_exp (int_to_pc 0))
      | c_9 -> failwith ("unreachable:" ^ string_of_int c_9 ^ "(26)"))
    26

let () =
  add_exp
    (fun w_26 ->
      assert_env_length w_26 1;
      push_env w_26 (Dynarray.get w_26.state.e 0);
      w_26.state.c <- pc_to_exp (int_to_pc 30);
      stepped w_26)
    27

let () =
  add_exp
    (fun w_29 ->
      assert_env_length w_29 6;
      let last_12 = Source.E 5 in
      let x_12 = resolve w_29 last_12 in
      match Word.get_value (fst x_12) with
      | c_12 when c_12 = tag_ECons ->
          let splits_16 = Memo.splits (snd x_12) in
          let split0_16 = List.nth splits_16 0 in
          let split1_7 = List.nth splits_16 1 in
          ignore (pop_env w_29);
          push_env w_29 split0_16;
          push_env w_29 split1_7;
          assert_env_length w_29 7;
          push_env w_29 (Dynarray.get w_29.state.e 5);
          assert_env_length w_29 8;
          drop_n w_29 8 2;
          assert_env_length w_29 6;
          drop_n w_29 6 2;
          assert_env_length w_29 4;
          drop_n w_29 4 2;
          assert_env_length w_29 2;
          return_n w_29 2 (pc_to_exp (int_to_pc 0))
      | c_12 -> failwith ("unreachable:" ^ string_of_int c_12 ^ "(28)"))
    28

let () =
  add_exp
    (fun w_28 ->
      assert_env_length w_28 4;
      let last_11 = Source.E 3 in
      let x_11 = resolve w_28 last_11 in
      match Word.get_value (fst x_11) with
      | c_11 when c_11 = tag_ECons ->
          let splits_15 = Memo.splits (snd x_11) in
          let split0_15 = List.nth splits_15 0 in
          let split1_6 = List.nth splits_15 1 in
          ignore (pop_env w_28);
          push_env w_28 split0_15;
          push_env w_28 split1_6;
          assert_env_length w_28 5;
          push_env w_28 (Dynarray.get w_28.state.e 4);
          w_28.state.c <- pc_to_exp (int_to_pc 28);
          stepped w_28
      | c_11 -> failwith ("unreachable:" ^ string_of_int c_11 ^ "(29)"))
    29

let () =
  add_exp
    (fun w_27 ->
      assert_env_length w_27 2;
      let last_10 = Source.E 1 in
      let x_10 = resolve w_27 last_10 in
      match Word.get_value (fst x_10) with
      | c_10 when c_10 = tag_ECons ->
          let splits_14 = Memo.splits (snd x_10) in
          let split0_14 = List.nth splits_14 0 in
          let split1_5 = List.nth splits_14 1 in
          ignore (pop_env w_27);
          push_env w_27 split0_14;
          push_env w_27 split1_5;
          assert_env_length w_27 3;
          push_env w_27 (Dynarray.get w_27.state.e 2);
          w_27.state.c <- pc_to_exp (int_to_pc 29);
          stepped w_27
      | c_10 -> failwith ("unreachable:" ^ string_of_int c_10 ^ "(30)"))
    30

let () =
  add_exp
    (fun w_30 ->
      assert_env_length w_30 1;
      push_env w_30 (Dynarray.get w_30.state.e 0);
      w_30.state.c <- pc_to_exp (int_to_pc 33);
      stepped w_30)
    31

let () =
  add_exp
    (fun w_32 ->
      assert_env_length w_32 4;
      let last_14 = Source.E 3 in
      let x_14 = resolve w_32 last_14 in
      match Word.get_value (fst x_14) with
      | c_14 when c_14 = tag_ECons ->
          let splits_18 = Memo.splits (snd x_14) in
          let split0_18 = List.nth splits_18 0 in
          let split1_9 = List.nth splits_18 1 in
          ignore (pop_env w_32);
          push_env w_32 split0_18;
          push_env w_32 split1_9;
          assert_env_length w_32 5;
          push_env w_32 (Dynarray.get w_32.state.e 3);
          assert_env_length w_32 6;
          drop_n w_32 6 2;
          assert_env_length w_32 4;
          drop_n w_32 4 2;
          assert_env_length w_32 2;
          return_n w_32 2 (pc_to_exp (int_to_pc 0))
      | c_14 -> failwith ("unreachable:" ^ string_of_int c_14 ^ "(32)"))
    32

let () =
  add_exp
    (fun w_31 ->
      assert_env_length w_31 2;
      let last_13 = Source.E 1 in
      let x_13 = resolve w_31 last_13 in
      match Word.get_value (fst x_13) with
      | c_13 when c_13 = tag_ECons ->
          let splits_17 = Memo.splits (snd x_13) in
          let split0_17 = List.nth splits_17 0 in
          let split1_8 = List.nth splits_17 1 in
          ignore (pop_env w_31);
          push_env w_31 split0_17;
          push_env w_31 split1_8;
          assert_env_length w_31 3;
          push_env w_31 (Dynarray.get w_31.state.e 2);
          w_31.state.c <- pc_to_exp (int_to_pc 32);
          stepped w_31
      | c_13 -> failwith ("unreachable:" ^ string_of_int c_13 ^ "(33)"))
    33

let () =
  add_exp
    (fun w_33 ->
      assert_env_length w_33 1;
      push_env w_33 (Dynarray.get w_33.state.e 0);
      w_33.state.c <- pc_to_exp (int_to_pc 36);
      stepped w_33)
    34

let () =
  add_exp
    (fun w_35 ->
      assert_env_length w_35 4;
      let last_16 = Source.E 3 in
      let x_16 = resolve w_35 last_16 in
      match Word.get_value (fst x_16) with
      | c_16 when c_16 = tag_ECons ->
          let splits_20 = Memo.splits (snd x_16) in
          let split0_20 = List.nth splits_20 0 in
          let split1_11 = List.nth splits_20 1 in
          ignore (pop_env w_35);
          push_env w_35 split0_20;
          push_env w_35 split1_11;
          assert_env_length w_35 5;
          push_env w_35 (Dynarray.get w_35.state.e 3);
          assert_env_length w_35 6;
          drop_n w_35 6 2;
          assert_env_length w_35 4;
          drop_n w_35 4 2;
          assert_env_length w_35 2;
          return_n w_35 2 (pc_to_exp (int_to_pc 0))
      | c_16 -> failwith ("unreachable:" ^ string_of_int c_16 ^ "(35)"))
    35

let () =
  add_exp
    (fun w_34 ->
      assert_env_length w_34 2;
      let last_15 = Source.E 1 in
      let x_15 = resolve w_34 last_15 in
      match Word.get_value (fst x_15) with
      | c_15 when c_15 = tag_ECons ->
          let splits_19 = Memo.splits (snd x_15) in
          let split0_19 = List.nth splits_19 0 in
          let split1_10 = List.nth splits_19 1 in
          ignore (pop_env w_34);
          push_env w_34 split0_19;
          push_env w_34 split1_10;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 1);
          w_34.state.c <- pc_to_exp (int_to_pc 35);
          stepped w_34
      | c_15 -> failwith ("unreachable:" ^ string_of_int c_15 ^ "(36)"))
    36

let () =
  add_exp
    (fun w_36 ->
      assert_env_length w_36 1;
      push_env w_36 (Dynarray.get w_36.state.e 0);
      w_36.state.c <- pc_to_exp (int_to_pc 39);
      stepped w_36)
    37

let () =
  add_exp
    (fun w_38 ->
      assert_env_length w_38 4;
      let last_18 = Source.E 3 in
      let x_18 = resolve w_38 last_18 in
      match Word.get_value (fst x_18) with
      | c_18 when c_18 = tag_ECons ->
          let splits_22 = Memo.splits (snd x_18) in
          let split0_22 = List.nth splits_22 0 in
          let split1_13 = List.nth splits_22 1 in
          ignore (pop_env w_38);
          push_env w_38 split0_22;
          push_env w_38 split1_13;
          assert_env_length w_38 5;
          push_env w_38 (Dynarray.get w_38.state.e 4);
          assert_env_length w_38 6;
          drop_n w_38 6 2;
          assert_env_length w_38 4;
          drop_n w_38 4 2;
          assert_env_length w_38 2;
          return_n w_38 2 (pc_to_exp (int_to_pc 0))
      | c_18 -> failwith ("unreachable:" ^ string_of_int c_18 ^ "(38)"))
    38

let () =
  add_exp
    (fun w_37 ->
      assert_env_length w_37 2;
      let last_17 = Source.E 1 in
      let x_17 = resolve w_37 last_17 in
      match Word.get_value (fst x_17) with
      | c_17 when c_17 = tag_ECons ->
          let splits_21 = Memo.splits (snd x_17) in
          let split0_21 = List.nth splits_21 0 in
          let split1_12 = List.nth splits_21 1 in
          ignore (pop_env w_37);
          push_env w_37 split0_21;
          push_env w_37 split1_12;
          assert_env_length w_37 3;
          push_env w_37 (Dynarray.get w_37.state.e 1);
          w_37.state.c <- pc_to_exp (int_to_pc 38);
          stepped w_37
      | c_17 -> failwith ("unreachable:" ^ string_of_int c_17 ^ "(39)"))
    39

let () =
  add_exp
    (fun w_39 ->
      assert_env_length w_39 1;
      push_env w_39 (Dynarray.get w_39.state.e 0);
      w_39.state.c <- pc_to_exp (int_to_pc 43);
      stepped w_39)
    40

let () =
  add_exp
    (fun w_42 ->
      assert_env_length w_42 6;
      let last_21 = Source.E 5 in
      let x_21 = resolve w_42 last_21 in
      match Word.get_value (fst x_21) with
      | c_21 when c_21 = tag_ECons ->
          let splits_25 = Memo.splits (snd x_21) in
          let split0_25 = List.nth splits_25 0 in
          let split1_16 = List.nth splits_25 1 in
          ignore (pop_env w_42);
          push_env w_42 split0_25;
          push_env w_42 split1_16;
          assert_env_length w_42 7;
          push_env w_42 (Dynarray.get w_42.state.e 6);
          assert_env_length w_42 8;
          drop_n w_42 8 2;
          assert_env_length w_42 6;
          drop_n w_42 6 2;
          assert_env_length w_42 4;
          drop_n w_42 4 2;
          assert_env_length w_42 2;
          return_n w_42 2 (pc_to_exp (int_to_pc 0))
      | c_21 -> failwith ("unreachable:" ^ string_of_int c_21 ^ "(41)"))
    41

let () =
  add_exp
    (fun w_41 ->
      assert_env_length w_41 4;
      let last_20 = Source.E 3 in
      let x_20 = resolve w_41 last_20 in
      match Word.get_value (fst x_20) with
      | c_20 when c_20 = tag_ECons ->
          let splits_24 = Memo.splits (snd x_20) in
          let split0_24 = List.nth splits_24 0 in
          let split1_15 = List.nth splits_24 1 in
          ignore (pop_env w_41);
          push_env w_41 split0_24;
          push_env w_41 split1_15;
          assert_env_length w_41 5;
          push_env w_41 (Dynarray.get w_41.state.e 4);
          w_41.state.c <- pc_to_exp (int_to_pc 41);
          stepped w_41
      | c_20 -> failwith ("unreachable:" ^ string_of_int c_20 ^ "(42)"))
    42

let () =
  add_exp
    (fun w_40 ->
      assert_env_length w_40 2;
      let last_19 = Source.E 1 in
      let x_19 = resolve w_40 last_19 in
      match Word.get_value (fst x_19) with
      | c_19 when c_19 = tag_ECons ->
          let splits_23 = Memo.splits (snd x_19) in
          let split0_23 = List.nth splits_23 0 in
          let split1_14 = List.nth splits_23 1 in
          ignore (pop_env w_40);
          push_env w_40 split0_23;
          push_env w_40 split1_14;
          assert_env_length w_40 3;
          push_env w_40 (Dynarray.get w_40.state.e 1);
          w_40.state.c <- pc_to_exp (int_to_pc 42);
          stepped w_40
      | c_19 -> failwith ("unreachable:" ^ string_of_int c_19 ^ "(43)"))
    43

let () =
  add_exp
    (fun w_43 ->
      assert_env_length w_43 1;
      push_env w_43 (Dynarray.get w_43.state.e 0);
      w_43.state.c <- pc_to_exp (int_to_pc 47);
      stepped w_43)
    44

let () =
  add_exp
    (fun w_46 ->
      assert_env_length w_46 6;
      let last_24 = Source.E 5 in
      let x_24 = resolve w_46 last_24 in
      match Word.get_value (fst x_24) with
      | c_24 when c_24 = tag_ECons ->
          let splits_28 = Memo.splits (snd x_24) in
          let split0_28 = List.nth splits_28 0 in
          let split1_19 = List.nth splits_28 1 in
          ignore (pop_env w_46);
          push_env w_46 split0_28;
          push_env w_46 split1_19;
          assert_env_length w_46 7;
          push_env w_46 (Dynarray.get w_46.state.e 5);
          assert_env_length w_46 8;
          drop_n w_46 8 2;
          assert_env_length w_46 6;
          drop_n w_46 6 2;
          assert_env_length w_46 4;
          drop_n w_46 4 2;
          assert_env_length w_46 2;
          return_n w_46 2 (pc_to_exp (int_to_pc 0))
      | c_24 -> failwith ("unreachable:" ^ string_of_int c_24 ^ "(45)"))
    45

let () =
  add_exp
    (fun w_45 ->
      assert_env_length w_45 4;
      let last_23 = Source.E 3 in
      let x_23 = resolve w_45 last_23 in
      match Word.get_value (fst x_23) with
      | c_23 when c_23 = tag_ECons ->
          let splits_27 = Memo.splits (snd x_23) in
          let split0_27 = List.nth splits_27 0 in
          let split1_18 = List.nth splits_27 1 in
          ignore (pop_env w_45);
          push_env w_45 split0_27;
          push_env w_45 split1_18;
          assert_env_length w_45 5;
          push_env w_45 (Dynarray.get w_45.state.e 4);
          w_45.state.c <- pc_to_exp (int_to_pc 45);
          stepped w_45
      | c_23 -> failwith ("unreachable:" ^ string_of_int c_23 ^ "(46)"))
    46

let () =
  add_exp
    (fun w_44 ->
      assert_env_length w_44 2;
      let last_22 = Source.E 1 in
      let x_22 = resolve w_44 last_22 in
      match Word.get_value (fst x_22) with
      | c_22 when c_22 = tag_ECons ->
          let splits_26 = Memo.splits (snd x_22) in
          let split0_26 = List.nth splits_26 0 in
          let split1_17 = List.nth splits_26 1 in
          ignore (pop_env w_44);
          push_env w_44 split0_26;
          push_env w_44 split1_17;
          assert_env_length w_44 3;
          push_env w_44 (Dynarray.get w_44.state.e 1);
          w_44.state.c <- pc_to_exp (int_to_pc 46);
          stepped w_44
      | c_22 -> failwith ("unreachable:" ^ string_of_int c_22 ^ "(47)"))
    47

let () =
  add_exp
    (fun w_47 ->
      assert_env_length w_47 1;
      push_env w_47 (Dynarray.get w_47.state.e 0);
      w_47.state.c <- pc_to_exp (int_to_pc 52);
      stepped w_47)
    48

let () =
  add_exp
    (fun w_51 ->
      assert_env_length w_51 8;
      let last_28 = Source.E 7 in
      let x_28 = resolve w_51 last_28 in
      match Word.get_value (fst x_28) with
      | c_28 when c_28 = tag_ECons ->
          let splits_32 = Memo.splits (snd x_28) in
          let split0_32 = List.nth splits_32 0 in
          let split1_23 = List.nth splits_32 1 in
          ignore (pop_env w_51);
          push_env w_51 split0_32;
          push_env w_51 split1_23;
          assert_env_length w_51 9;
          push_env w_51 (Dynarray.get w_51.state.e 7);
          assert_env_length w_51 10;
          drop_n w_51 10 2;
          assert_env_length w_51 8;
          drop_n w_51 8 2;
          assert_env_length w_51 6;
          drop_n w_51 6 2;
          assert_env_length w_51 4;
          drop_n w_51 4 2;
          assert_env_length w_51 2;
          return_n w_51 2 (pc_to_exp (int_to_pc 0))
      | c_28 -> failwith ("unreachable:" ^ string_of_int c_28 ^ "(49)"))
    49

let () =
  add_exp
    (fun w_50 ->
      assert_env_length w_50 6;
      let last_27 = Source.E 5 in
      let x_27 = resolve w_50 last_27 in
      match Word.get_value (fst x_27) with
      | c_27 when c_27 = tag_ECons ->
          let splits_31 = Memo.splits (snd x_27) in
          let split0_31 = List.nth splits_31 0 in
          let split1_22 = List.nth splits_31 1 in
          ignore (pop_env w_50);
          push_env w_50 split0_31;
          push_env w_50 split1_22;
          assert_env_length w_50 7;
          push_env w_50 (Dynarray.get w_50.state.e 6);
          w_50.state.c <- pc_to_exp (int_to_pc 49);
          stepped w_50
      | c_27 -> failwith ("unreachable:" ^ string_of_int c_27 ^ "(50)"))
    50

let () =
  add_exp
    (fun w_49 ->
      assert_env_length w_49 4;
      let last_26 = Source.E 3 in
      let x_26 = resolve w_49 last_26 in
      match Word.get_value (fst x_26) with
      | c_26 when c_26 = tag_ECons ->
          let splits_30 = Memo.splits (snd x_26) in
          let split0_30 = List.nth splits_30 0 in
          let split1_21 = List.nth splits_30 1 in
          ignore (pop_env w_49);
          push_env w_49 split0_30;
          push_env w_49 split1_21;
          assert_env_length w_49 5;
          push_env w_49 (Dynarray.get w_49.state.e 4);
          w_49.state.c <- pc_to_exp (int_to_pc 50);
          stepped w_49
      | c_26 -> failwith ("unreachable:" ^ string_of_int c_26 ^ "(51)"))
    51

let () =
  add_exp
    (fun w_48 ->
      assert_env_length w_48 2;
      let last_25 = Source.E 1 in
      let x_25 = resolve w_48 last_25 in
      match Word.get_value (fst x_25) with
      | c_25 when c_25 = tag_ECons ->
          let splits_29 = Memo.splits (snd x_25) in
          let split0_29 = List.nth splits_29 0 in
          let split1_20 = List.nth splits_29 1 in
          ignore (pop_env w_48);
          push_env w_48 split0_29;
          push_env w_48 split1_20;
          assert_env_length w_48 3;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 51);
          stepped w_48
      | c_25 -> failwith ("unreachable:" ^ string_of_int c_25 ^ "(52)"))
    52

let () =
  add_exp
    (fun w_52 ->
      assert_env_length w_52 1;
      push_env w_52 (Dynarray.get w_52.state.e 0);
      w_52.state.c <- pc_to_exp (int_to_pc 54);
      stepped w_52)
    53

let () =
  add_exp
    (fun w_53 ->
      assert_env_length w_53 2;
      let last_29 = Source.E 1 in
      let x_29 = resolve w_53 last_29 in
      match Word.get_value (fst x_29) with
      | c_29 when c_29 = tag_ECons ->
          let splits_33 = Memo.splits (snd x_29) in
          let split0_33 = List.nth splits_33 0 in
          let split1_24 = List.nth splits_33 1 in
          ignore (pop_env w_53);
          push_env w_53 split0_33;
          push_env w_53 split1_24;
          assert_env_length w_53 3;
          push_env w_53 (Dynarray.get w_53.state.e 1);
          assert_env_length w_53 4;
          drop_n w_53 4 2;
          assert_env_length w_53 2;
          return_n w_53 2 (pc_to_exp (int_to_pc 0))
      | c_29 -> failwith ("unreachable:" ^ string_of_int c_29 ^ "(54)"))
    54

let () =
  add_exp
    (fun w_54 ->
      assert_env_length w_54 1;
      push_env w_54 (Dynarray.get w_54.state.e 0);
      w_54.state.c <- pc_to_exp (int_to_pc 56);
      stepped w_54)
    55

let () =
  add_exp
    (fun w_55 ->
      assert_env_length w_55 2;
      let last_30 = Source.E 1 in
      let x_30 = resolve w_55 last_30 in
      match Word.get_value (fst x_30) with
      | c_30 when c_30 = tag_ECons ->
          let splits_34 = Memo.splits (snd x_30) in
          let split0_34 = List.nth splits_34 0 in
          let split1_25 = List.nth splits_34 1 in
          ignore (pop_env w_55);
          push_env w_55 split0_34;
          push_env w_55 split1_25;
          assert_env_length w_55 3;
          push_env w_55 (Dynarray.get w_55.state.e 2);
          assert_env_length w_55 4;
          drop_n w_55 4 2;
          assert_env_length w_55 2;
          return_n w_55 2 (pc_to_exp (int_to_pc 0))
      | c_30 -> failwith ("unreachable:" ^ string_of_int c_30 ^ "(56)"))
    56

let () =
  add_exp
    (fun w_56 ->
      assert_env_length w_56 2;
      push_env w_56 (Dynarray.get w_56.state.e 0);
      w_56.state.c <- pc_to_exp (int_to_pc 67);
      stepped w_56)
    57

let () =
  add_exp
    (fun w_58 ->
      assert_env_length w_58 3;
      let last_32 = Source.E 2 in
      let x_32 = resolve w_58 last_32 in
      match Word.get_value (fst x_32) with
      | c_32 when c_32 = tag_SLambda ->
          ignore (pop_env w_58);
          assert_env_length w_58 2;
          push_env w_58 (Memo.from_int 1);
          assert_env_length w_58 3;
          return_n w_58 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_58);
          assert_env_length w_58 2;
          push_env w_58 (Memo.from_int 0);
          assert_env_length w_58 3;
          return_n w_58 3 (pc_to_exp (int_to_pc 0))
      | c_32 -> failwith ("unreachable:" ^ string_of_int c_32 ^ "(58)"))
    58

let () =
  add_exp
    (fun w_59 ->
      assert_env_length w_59 3;
      let last_33 = Source.E 2 in
      let x_33 = resolve w_59 last_33 in
      match Word.get_value (fst x_33) with
      | c_33 when c_33 = tag_SDefine ->
          ignore (pop_env w_59);
          assert_env_length w_59 2;
          push_env w_59 (Memo.from_int 1);
          assert_env_length w_59 3;
          return_n w_59 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_59);
          assert_env_length w_59 2;
          push_env w_59 (Memo.from_int 0);
          assert_env_length w_59 3;
          return_n w_59 3 (pc_to_exp (int_to_pc 0))
      | c_33 -> failwith ("unreachable:" ^ string_of_int c_33 ^ "(59)"))
    59

let () =
  add_exp
    (fun w_60 ->
      assert_env_length w_60 3;
      let last_34 = Source.E 2 in
      let x_34 = resolve w_60 last_34 in
      match Word.get_value (fst x_34) with
      | c_34 when c_34 = tag_SQuote ->
          ignore (pop_env w_60);
          assert_env_length w_60 2;
          push_env w_60 (Memo.from_int 1);
          assert_env_length w_60 3;
          return_n w_60 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_60);
          assert_env_length w_60 2;
          push_env w_60 (Memo.from_int 0);
          assert_env_length w_60 3;
          return_n w_60 3 (pc_to_exp (int_to_pc 0))
      | c_34 -> failwith ("unreachable:" ^ string_of_int c_34 ^ "(60)"))
    60

let () =
  add_exp
    (fun w_61 ->
      assert_env_length w_61 3;
      let last_35 = Source.E 2 in
      let x_35 = resolve w_61 last_35 in
      match Word.get_value (fst x_35) with
      | c_35 when c_35 = tag_SEq ->
          ignore (pop_env w_61);
          assert_env_length w_61 2;
          push_env w_61 (Memo.from_int 1);
          assert_env_length w_61 3;
          return_n w_61 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_61);
          assert_env_length w_61 2;
          push_env w_61 (Memo.from_int 0);
          assert_env_length w_61 3;
          return_n w_61 3 (pc_to_exp (int_to_pc 0))
      | c_35 -> failwith ("unreachable:" ^ string_of_int c_35 ^ "(61)"))
    61

let () =
  add_exp
    (fun w_62 ->
      assert_env_length w_62 3;
      let last_36 = Source.E 2 in
      let x_36 = resolve w_62 last_36 in
      match Word.get_value (fst x_36) with
      | c_36 when c_36 = tag_SCons ->
          ignore (pop_env w_62);
          assert_env_length w_62 2;
          push_env w_62 (Memo.from_int 1);
          assert_env_length w_62 3;
          return_n w_62 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_62);
          assert_env_length w_62 2;
          push_env w_62 (Memo.from_int 0);
          assert_env_length w_62 3;
          return_n w_62 3 (pc_to_exp (int_to_pc 0))
      | c_36 -> failwith ("unreachable:" ^ string_of_int c_36 ^ "(62)"))
    62

let () =
  add_exp
    (fun w_63 ->
      assert_env_length w_63 3;
      let last_37 = Source.E 2 in
      let x_37 = resolve w_63 last_37 in
      match Word.get_value (fst x_37) with
      | c_37 when c_37 = tag_SCond ->
          ignore (pop_env w_63);
          assert_env_length w_63 2;
          push_env w_63 (Memo.from_int 1);
          assert_env_length w_63 3;
          return_n w_63 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_63);
          assert_env_length w_63 2;
          push_env w_63 (Memo.from_int 0);
          assert_env_length w_63 3;
          return_n w_63 3 (pc_to_exp (int_to_pc 0))
      | c_37 -> failwith ("unreachable:" ^ string_of_int c_37 ^ "(63)"))
    63

let () =
  add_exp
    (fun w_64 ->
      assert_env_length w_64 3;
      let last_38 = Source.E 2 in
      let x_38 = resolve w_64 last_38 in
      match Word.get_value (fst x_38) with
      | c_38 when c_38 = tag_SAtom ->
          ignore (pop_env w_64);
          assert_env_length w_64 2;
          push_env w_64 (Memo.from_int 1);
          assert_env_length w_64 3;
          return_n w_64 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_64);
          assert_env_length w_64 2;
          push_env w_64 (Memo.from_int 0);
          assert_env_length w_64 3;
          return_n w_64 3 (pc_to_exp (int_to_pc 0))
      | c_38 -> failwith ("unreachable:" ^ string_of_int c_38 ^ "(64)"))
    64

let () =
  add_exp
    (fun w_65 ->
      assert_env_length w_65 3;
      let last_39 = Source.E 2 in
      let x_39 = resolve w_65 last_39 in
      match Word.get_value (fst x_39) with
      | c_39 when c_39 = tag_SCar ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Memo.from_int 1);
          assert_env_length w_65 3;
          return_n w_65 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Memo.from_int 0);
          assert_env_length w_65 3;
          return_n w_65 3 (pc_to_exp (int_to_pc 0))
      | c_39 -> failwith ("unreachable:" ^ string_of_int c_39 ^ "(65)"))
    65

let () =
  add_exp
    (fun w_66 ->
      assert_env_length w_66 3;
      let last_40 = Source.E 2 in
      let x_40 = resolve w_66 last_40 in
      match Word.get_value (fst x_40) with
      | c_40 when c_40 = tag_SCdr ->
          ignore (pop_env w_66);
          assert_env_length w_66 2;
          push_env w_66 (Memo.from_int 1);
          assert_env_length w_66 3;
          return_n w_66 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_66);
          assert_env_length w_66 2;
          push_env w_66 (Memo.from_int 0);
          assert_env_length w_66 3;
          return_n w_66 3 (pc_to_exp (int_to_pc 0))
      | c_40 -> failwith ("unreachable:" ^ string_of_int c_40 ^ "(66)"))
    66

let () =
  add_exp
    (fun w_57 ->
      assert_env_length w_57 3;
      let last_31 = Source.E 2 in
      let x_31 = resolve w_57 last_31 in
      match Word.get_value (fst x_31) with
      | c_31 when c_31 = tag_SLambda ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 58);
          stepped w_57
      | c_31 when c_31 = tag_SDefine ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 59);
          stepped w_57
      | c_31 when c_31 = tag_SQuote ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 60);
          stepped w_57
      | c_31 when c_31 = tag_SEq ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_57
      | c_31 when c_31 = tag_SCons ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 62);
          stepped w_57
      | c_31 when c_31 = tag_SCond ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 63);
          stepped w_57
      | c_31 when c_31 = tag_SAtom ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 64);
          stepped w_57
      | c_31 when c_31 = tag_SCar ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 65);
          stepped w_57
      | c_31 when c_31 = tag_SCdr ->
          ignore (pop_env w_57);
          assert_env_length w_57 2;
          push_env w_57 (Dynarray.get w_57.state.e 1);
          w_57.state.c <- pc_to_exp (int_to_pc 66);
          stepped w_57
      | c_31 -> failwith ("unreachable:" ^ string_of_int c_31 ^ "(67)"))
    67

let () =
  add_exp
    (fun w_67 ->
      assert_env_length w_67 2;
      push_env w_67 (Dynarray.get w_67.state.e 0);
      w_67.state.c <- pc_to_exp (int_to_pc 75);
      stepped w_67)
    68

let () =
  add_exp
    (fun w_70 ->
      assert_env_length w_70 6;
      let x0_1 = resolve w_70 (Source.E 4) in
      let x1_1 = resolve w_70 (Source.E 5) in
      ignore (pop_env w_70);
      ignore (pop_env w_70);
      push_env w_70 (Memo.from_int (if Word.get_value (fst x0_1) = Word.get_value (fst x1_1) then 1 else 0));
      assert_env_length w_70 5;
      drop_n w_70 5 1;
      assert_env_length w_70 4;
      drop_n w_70 4 1;
      assert_env_length w_70 3;
      return_n w_70 3 (pc_to_exp (int_to_pc 0)))
    69

let () =
  add_exp
    (fun w_69 ->
      assert_env_length w_69 4;
      let last_42 = Source.E 3 in
      let x_42 = resolve w_69 last_42 in
      match Word.get_value (fst x_42) with
      | c_42 when c_42 = tag_AVar ->
          let splits_36 = Memo.splits (snd x_42) in
          let split0_36 = List.nth splits_36 0 in
          ignore (pop_env w_69);
          push_env w_69 split0_36;
          assert_env_length w_69 4;
          push_env w_69 (Dynarray.get w_69.state.e 2);
          assert_env_length w_69 5;
          push_env w_69 (Dynarray.get w_69.state.e 3);
          w_69.state.c <- pc_to_exp (int_to_pc 69);
          stepped w_69
      | _ ->
          ignore (pop_env w_69);
          assert_env_length w_69 3;
          push_env w_69 (Memo.from_int 0);
          assert_env_length w_69 4;
          drop_n w_69 4 1;
          assert_env_length w_69 3;
          return_n w_69 3 (pc_to_exp (int_to_pc 0))
      | c_42 -> failwith ("unreachable:" ^ string_of_int c_42 ^ "(70)"))
    70

let () =
  add_exp
    (fun w_72 ->
      assert_env_length w_72 6;
      let x0_2 = resolve w_72 (Source.E 4) in
      let x1_2 = resolve w_72 (Source.E 5) in
      ignore (pop_env w_72);
      ignore (pop_env w_72);
      push_env w_72 (Memo.from_int (if Word.get_value (fst x0_2) = Word.get_value (fst x1_2) then 1 else 0));
      assert_env_length w_72 5;
      drop_n w_72 5 1;
      assert_env_length w_72 4;
      drop_n w_72 4 1;
      assert_env_length w_72 3;
      return_n w_72 3 (pc_to_exp (int_to_pc 0)))
    71

let () =
  add_exp
    (fun w_71 ->
      assert_env_length w_71 4;
      let last_43 = Source.E 3 in
      let x_43 = resolve w_71 last_43 in
      match Word.get_value (fst x_43) with
      | c_43 when c_43 = tag_ANumber ->
          let splits_38 = Memo.splits (snd x_43) in
          let split0_38 = List.nth splits_38 0 in
          ignore (pop_env w_71);
          push_env w_71 split0_38;
          assert_env_length w_71 4;
          push_env w_71 (Dynarray.get w_71.state.e 2);
          assert_env_length w_71 5;
          push_env w_71 (Dynarray.get w_71.state.e 3);
          w_71.state.c <- pc_to_exp (int_to_pc 71);
          stepped w_71
      | _ ->
          ignore (pop_env w_71);
          assert_env_length w_71 3;
          push_env w_71 (Memo.from_int 0);
          assert_env_length w_71 4;
          drop_n w_71 4 1;
          assert_env_length w_71 3;
          return_n w_71 3 (pc_to_exp (int_to_pc 0))
      | c_43 -> failwith ("unreachable:" ^ string_of_int c_43 ^ "(72)"))
    72

let () =
  add_exp
    (fun w_73 ->
      assert_env_length w_73 4;
      let last_44 = Source.E 3 in
      let x_44 = resolve w_73 last_44 in
      match Word.get_value (fst x_44) with
      | c_44 when c_44 = tag_ASymbol ->
          let splits_40 = Memo.splits (snd x_44) in
          let split0_40 = List.nth splits_40 0 in
          ignore (pop_env w_73);
          push_env w_73 split0_40;
          assert_env_length w_73 4;
          push_env w_73 (Dynarray.get w_73.state.e 2);
          assert_env_length w_73 5;
          push_env w_73 (Dynarray.get w_73.state.e 3);
          assert_env_length w_73 6;
          ignore (env_call w_73 [] 2);
          w_73.state.c <- pc_to_exp (int_to_pc 57);
          stepped w_73
      | _ ->
          ignore (pop_env w_73);
          assert_env_length w_73 3;
          push_env w_73 (Memo.from_int 0);
          assert_env_length w_73 4;
          drop_n w_73 4 1;
          assert_env_length w_73 3;
          return_n w_73 3 (pc_to_exp (int_to_pc 0))
      | c_44 -> failwith ("unreachable:" ^ string_of_int c_44 ^ "(73)"))
    73

let () =
  add_exp
    (fun w_74 ->
      assert_env_length w_74 3;
      let last_45 = Source.E 2 in
      let x_45 = resolve w_74 last_45 in
      match Word.get_value (fst x_45) with
      | c_45 when c_45 = tag_ANIL ->
          ignore (pop_env w_74);
          assert_env_length w_74 2;
          push_env w_74 (Memo.from_int 1);
          assert_env_length w_74 3;
          return_n w_74 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_74);
          assert_env_length w_74 2;
          push_env w_74 (Memo.from_int 0);
          assert_env_length w_74 3;
          return_n w_74 3 (pc_to_exp (int_to_pc 0))
      | c_45 -> failwith ("unreachable:" ^ string_of_int c_45 ^ "(74)"))
    74

let () =
  add_exp
    (fun w_68 ->
      assert_env_length w_68 3;
      let last_41 = Source.E 2 in
      let x_41 = resolve w_68 last_41 in
      match Word.get_value (fst x_41) with
      | c_41 when c_41 = tag_AVar ->
          let splits_35 = Memo.splits (snd x_41) in
          let split0_35 = List.nth splits_35 0 in
          ignore (pop_env w_68);
          push_env w_68 split0_35;
          assert_env_length w_68 3;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          w_68.state.c <- pc_to_exp (int_to_pc 70);
          stepped w_68
      | c_41 when c_41 = tag_ANumber ->
          let splits_37 = Memo.splits (snd x_41) in
          let split0_37 = List.nth splits_37 0 in
          ignore (pop_env w_68);
          push_env w_68 split0_37;
          assert_env_length w_68 3;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          w_68.state.c <- pc_to_exp (int_to_pc 72);
          stepped w_68
      | c_41 when c_41 = tag_ASymbol ->
          let splits_39 = Memo.splits (snd x_41) in
          let split0_39 = List.nth splits_39 0 in
          ignore (pop_env w_68);
          push_env w_68 split0_39;
          assert_env_length w_68 3;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          w_68.state.c <- pc_to_exp (int_to_pc 73);
          stepped w_68
      | c_41 when c_41 = tag_ANIL ->
          ignore (pop_env w_68);
          assert_env_length w_68 2;
          push_env w_68 (Dynarray.get w_68.state.e 1);
          w_68.state.c <- pc_to_exp (int_to_pc 74);
          stepped w_68
      | c_41 -> failwith ("unreachable:" ^ string_of_int c_41 ^ "(75)"))
    75

let () =
  add_exp
    (fun w_75 ->
      assert_env_length w_75 2;
      push_env w_75 (Dynarray.get w_75.state.e 0);
      w_75.state.c <- pc_to_exp (int_to_pc 78);
      stepped w_75)
    76

let () =
  add_exp
    (fun w_77 ->
      assert_env_length w_77 4;
      let last_47 = Source.E 3 in
      let x_47 = resolve w_77 last_47 in
      match Word.get_value (fst x_47) with
      | c_47 when c_47 = tag_EAtom ->
          let splits_42 = Memo.splits (snd x_47) in
          let split0_42 = List.nth splits_42 0 in
          ignore (pop_env w_77);
          push_env w_77 split0_42;
          assert_env_length w_77 4;
          push_env w_77 (Dynarray.get w_77.state.e 2);
          assert_env_length w_77 5;
          push_env w_77 (Dynarray.get w_77.state.e 3);
          assert_env_length w_77 6;
          ignore (env_call w_77 [] 2);
          w_77.state.c <- pc_to_exp (int_to_pc 68);
          stepped w_77
      | _ ->
          ignore (pop_env w_77);
          assert_env_length w_77 3;
          push_env w_77 (Memo.from_int 0);
          assert_env_length w_77 4;
          drop_n w_77 4 1;
          assert_env_length w_77 3;
          return_n w_77 3 (pc_to_exp (int_to_pc 0))
      | c_47 -> failwith ("unreachable:" ^ string_of_int c_47 ^ "(77)"))
    77

let () =
  add_exp
    (fun w_76 ->
      assert_env_length w_76 3;
      let last_46 = Source.E 2 in
      let x_46 = resolve w_76 last_46 in
      match Word.get_value (fst x_46) with
      | c_46 when c_46 = tag_EAtom ->
          let splits_41 = Memo.splits (snd x_46) in
          let split0_41 = List.nth splits_41 0 in
          ignore (pop_env w_76);
          push_env w_76 split0_41;
          assert_env_length w_76 3;
          push_env w_76 (Dynarray.get w_76.state.e 1);
          w_76.state.c <- pc_to_exp (int_to_pc 77);
          stepped w_76
      | c_46 when c_46 = tag_ECons ->
          let splits_43 = Memo.splits (snd x_46) in
          let split0_43 = List.nth splits_43 0 in
          let split1_26 = List.nth splits_43 1 in
          ignore (pop_env w_76);
          push_env w_76 split0_43;
          push_env w_76 split1_26;
          assert_env_length w_76 4;
          push_env w_76 (Memo.from_int 0);
          assert_env_length w_76 5;
          drop_n w_76 5 2;
          assert_env_length w_76 3;
          return_n w_76 3 (pc_to_exp (int_to_pc 0))
      | c_46 -> failwith ("unreachable:" ^ string_of_int c_46 ^ "(78)"))
    78

let () =
  add_exp
    (fun w_78 ->
      assert_env_length w_78 2;
      push_env w_78 (Dynarray.get w_78.state.e 0);
      w_78.state.c <- pc_to_exp (int_to_pc 86);
      stepped w_78)
    79

let () =
  add_exp
    (fun w_85 ->
      assert_env_length w_85 2;
      push_env w_85 (Dynarray.get w_85.state.e 0);
      w_85.state.c <- pc_to_exp (int_to_pc 89);
      stepped w_85)
    80

let () =
  add_exp
    (fun w_81 ->
      assert_env_length w_81 6;
      let x0_3 = resolve w_81 (Source.E 4) in
      let x1_3 = resolve w_81 (Source.E 5) in
      ignore (pop_env w_81);
      ignore (pop_env w_81);
      push_env w_81 (Memo.from_int (if Word.get_value (fst x0_3) = Word.get_value (fst x1_3) then 1 else 0));
      assert_env_length w_81 5;
      drop_n w_81 5 1;
      assert_env_length w_81 4;
      drop_n w_81 4 1;
      assert_env_length w_81 3;
      return_n w_81 3 (pc_to_exp (int_to_pc 0)))
    81

let () =
  add_exp
    (fun w_80 ->
      assert_env_length w_80 4;
      let last_49 = Source.E 3 in
      let x_49 = resolve w_80 last_49 in
      match Word.get_value (fst x_49) with
      | c_49 when c_49 = tag_VNumber ->
          let splits_45 = Memo.splits (snd x_49) in
          let split0_45 = List.nth splits_45 0 in
          ignore (pop_env w_80);
          push_env w_80 split0_45;
          assert_env_length w_80 4;
          push_env w_80 (Dynarray.get w_80.state.e 2);
          assert_env_length w_80 5;
          push_env w_80 (Dynarray.get w_80.state.e 3);
          w_80.state.c <- pc_to_exp (int_to_pc 81);
          stepped w_80
      | _ ->
          ignore (pop_env w_80);
          assert_env_length w_80 3;
          push_env w_80 (Memo.from_int 0);
          assert_env_length w_80 4;
          drop_n w_80 4 1;
          assert_env_length w_80 3;
          return_n w_80 3 (pc_to_exp (int_to_pc 0))
      | c_49 -> failwith ("unreachable:" ^ string_of_int c_49 ^ "(82)"))
    82

let () =
  add_exp
    (fun w_82 ->
      assert_env_length w_82 4;
      let last_50 = Source.E 3 in
      let x_50 = resolve w_82 last_50 in
      match Word.get_value (fst x_50) with
      | c_50 when c_50 = tag_VSymbol ->
          let splits_47 = Memo.splits (snd x_50) in
          let split0_47 = List.nth splits_47 0 in
          ignore (pop_env w_82);
          push_env w_82 split0_47;
          assert_env_length w_82 4;
          push_env w_82 (Dynarray.get w_82.state.e 2);
          assert_env_length w_82 5;
          push_env w_82 (Dynarray.get w_82.state.e 3);
          assert_env_length w_82 6;
          ignore (env_call w_82 [] 2);
          w_82.state.c <- pc_to_exp (int_to_pc 57);
          stepped w_82
      | _ ->
          ignore (pop_env w_82);
          assert_env_length w_82 3;
          push_env w_82 (Memo.from_int 0);
          assert_env_length w_82 4;
          drop_n w_82 4 1;
          assert_env_length w_82 3;
          return_n w_82 3 (pc_to_exp (int_to_pc 0))
      | c_50 -> failwith ("unreachable:" ^ string_of_int c_50 ^ "(83)"))
    83

let () =
  add_exp
    (fun w_83 ->
      assert_env_length w_83 4;
      let last_51 = Source.E 3 in
      let x_51 = resolve w_83 last_51 in
      match Word.get_value (fst x_51) with
      | c_51 when c_51 = tag_VQuote ->
          let splits_49 = Memo.splits (snd x_51) in
          let split0_49 = List.nth splits_49 0 in
          ignore (pop_env w_83);
          push_env w_83 split0_49;
          assert_env_length w_83 4;
          push_env w_83 (Dynarray.get w_83.state.e 2);
          assert_env_length w_83 5;
          push_env w_83 (Dynarray.get w_83.state.e 3);
          assert_env_length w_83 6;
          ignore (env_call w_83 [] 2);
          w_83.state.c <- pc_to_exp (int_to_pc 76);
          stepped w_83
      | _ ->
          ignore (pop_env w_83);
          assert_env_length w_83 3;
          push_env w_83 (Memo.from_int 0);
          assert_env_length w_83 4;
          drop_n w_83 4 1;
          assert_env_length w_83 3;
          return_n w_83 3 (pc_to_exp (int_to_pc 0))
      | c_51 -> failwith ("unreachable:" ^ string_of_int c_51 ^ "(84)"))
    84

let () =
  add_exp
    (fun w_84 ->
      assert_env_length w_84 3;
      let last_52 = Source.E 2 in
      let x_52 = resolve w_84 last_52 in
      match Word.get_value (fst x_52) with
      | c_52 when c_52 = tag_VNIL ->
          ignore (pop_env w_84);
          assert_env_length w_84 2;
          push_env w_84 (Memo.from_int 1);
          assert_env_length w_84 3;
          return_n w_84 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_84);
          assert_env_length w_84 2;
          push_env w_84 (Memo.from_int 0);
          assert_env_length w_84 3;
          return_n w_84 3 (pc_to_exp (int_to_pc 0))
      | c_52 -> failwith ("unreachable:" ^ string_of_int c_52 ^ "(85)"))
    85

let () =
  add_exp
    (fun w_79 ->
      assert_env_length w_79 3;
      let last_48 = Source.E 2 in
      let x_48 = resolve w_79 last_48 in
      match Word.get_value (fst x_48) with
      | c_48 when c_48 = tag_VNumber ->
          let splits_44 = Memo.splits (snd x_48) in
          let split0_44 = List.nth splits_44 0 in
          ignore (pop_env w_79);
          push_env w_79 split0_44;
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          w_79.state.c <- pc_to_exp (int_to_pc 82);
          stepped w_79
      | c_48 when c_48 = tag_VSymbol ->
          let splits_46 = Memo.splits (snd x_48) in
          let split0_46 = List.nth splits_46 0 in
          ignore (pop_env w_79);
          push_env w_79 split0_46;
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          w_79.state.c <- pc_to_exp (int_to_pc 83);
          stepped w_79
      | c_48 when c_48 = tag_VQuote ->
          let splits_48 = Memo.splits (snd x_48) in
          let split0_48 = List.nth splits_48 0 in
          ignore (pop_env w_79);
          push_env w_79 split0_48;
          assert_env_length w_79 3;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          w_79.state.c <- pc_to_exp (int_to_pc 84);
          stepped w_79
      | c_48 when c_48 = tag_VNIL ->
          ignore (pop_env w_79);
          assert_env_length w_79 2;
          push_env w_79 (Dynarray.get w_79.state.e 1);
          w_79.state.c <- pc_to_exp (int_to_pc 85);
          stepped w_79
      | c_48 when c_48 = tag_VCons ->
          let splits_50 = Memo.splits (snd x_48) in
          let split0_50 = List.nth splits_50 0 in
          let split1_27 = List.nth splits_50 1 in
          ignore (pop_env w_79);
          push_env w_79 split0_50;
          push_env w_79 split1_27;
          assert_env_length w_79 4;
          push_env w_79 (Memo.from_int 0);
          assert_env_length w_79 5;
          drop_n w_79 5 2;
          assert_env_length w_79 3;
          return_n w_79 3 (pc_to_exp (int_to_pc 0))
      | c_48 when c_48 = tag_VClosure ->
          let splits_51 = Memo.splits (snd x_48) in
          let split0_51 = List.nth splits_51 0 in
          let split1_28 = List.nth splits_51 1 in
          let split2_1 = List.nth splits_51 2 in
          ignore (pop_env w_79);
          push_env w_79 split0_51;
          push_env w_79 split1_28;
          push_env w_79 split2_1;
          assert_env_length w_79 5;
          push_env w_79 (Memo.from_int 0);
          assert_env_length w_79 6;
          drop_n w_79 6 3;
          assert_env_length w_79 3;
          return_n w_79 3 (pc_to_exp (int_to_pc 0))
      | c_48 -> failwith ("unreachable:" ^ string_of_int c_48 ^ "(86)"))
    86

let () =
  add_exp
    (fun w_87 ->
      assert_env_length w_87 3;
      let last_54 = Source.E 2 in
      let x_54 = resolve w_87 last_54 in
      match Word.get_value (fst x_54) with
      | c_54 when c_54 = tag_Nil ->
          ignore (pop_env w_87);
          assert_env_length w_87 2;
          push_env w_87 (Memo.from_int 1);
          assert_env_length w_87 3;
          return_n w_87 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_87);
          assert_env_length w_87 2;
          push_env w_87 (Memo.from_int 0);
          assert_env_length w_87 3;
          return_n w_87 3 (pc_to_exp (int_to_pc 0))
      | c_54 -> failwith ("unreachable:" ^ string_of_int c_54 ^ "(87)"))
    87

let () =
  add_exp
    (fun w_88 ->
      assert_env_length w_88 5;
      let last_55 = Source.E 4 in
      let x_55 = resolve w_88 last_55 in
      match Word.get_value (fst x_55) with
      | c_55 when c_55 = tag_Cons ->
          let splits_53 = Memo.splits (snd x_55) in
          let split0_53 = List.nth splits_53 0 in
          let split1_30 = List.nth splits_53 1 in
          ignore (pop_env w_88);
          push_env w_88 split0_53;
          push_env w_88 split1_30;
          assert_env_length w_88 6;
          push_env w_88 (Dynarray.get w_88.state.e 2);
          assert_env_length w_88 7;
          push_env w_88 (Dynarray.get w_88.state.e 4);
          assert_env_length w_88 8;
          let keep_vals_1 = env_call w_88 [ 3; 5 ] 2 in
          w_88.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_vals_1; w_88.state.k ];
          w_88.state.c <- pc_to_exp (int_to_pc 79);
          stepped w_88
      | _ ->
          ignore (pop_env w_88);
          assert_env_length w_88 4;
          push_env w_88 (Memo.from_int 0);
          assert_env_length w_88 5;
          drop_n w_88 5 2;
          assert_env_length w_88 3;
          return_n w_88 3 (pc_to_exp (int_to_pc 0))
      | c_55 -> failwith ("unreachable:" ^ string_of_int c_55 ^ "(88)"))
    88

let () =
  add_exp
    (fun w_86 ->
      assert_env_length w_86 3;
      let last_53 = Source.E 2 in
      let x_53 = resolve w_86 last_53 in
      match Word.get_value (fst x_53) with
      | c_53 when c_53 = tag_Nil ->
          ignore (pop_env w_86);
          assert_env_length w_86 2;
          push_env w_86 (Dynarray.get w_86.state.e 1);
          w_86.state.c <- pc_to_exp (int_to_pc 87);
          stepped w_86
      | c_53 when c_53 = tag_Cons ->
          let splits_52 = Memo.splits (snd x_53) in
          let split0_52 = List.nth splits_52 0 in
          let split1_29 = List.nth splits_52 1 in
          ignore (pop_env w_86);
          push_env w_86 split0_52;
          push_env w_86 split1_29;
          assert_env_length w_86 4;
          push_env w_86 (Dynarray.get w_86.state.e 1);
          w_86.state.c <- pc_to_exp (int_to_pc 88);
          stepped w_86
      | c_53 -> failwith ("unreachable:" ^ string_of_int c_53 ^ "(89)"))
    89

let () =
  add_exp
    (fun w_89 ->
      assert_env_length w_89 1;
      push_env w_89 (Dynarray.get w_89.state.e 0);
      w_89.state.c <- pc_to_exp (int_to_pc 92);
      stepped w_89)
    90

let () =
  add_exp
    (fun w_91 ->
      assert_env_length w_91 3;
      let last_57 = Source.E 2 in
      let x_57 = resolve w_91 last_57 in
      match Word.get_value (fst x_57) with
      | c_57 when c_57 = tag_ANumber ->
          let splits_56 = Memo.splits (snd x_57) in
          let split0_56 = List.nth splits_56 0 in
          ignore (pop_env w_91);
          push_env w_91 split0_56;
          assert_env_length w_91 3;
          push_env w_91 (Dynarray.get w_91.state.e 2);
          assert_env_length w_91 4;
          let ctor_arg_21 = pop_env w_91 in
          push_env w_91 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_21 ]);
          assert_env_length w_91 4;
          drop_n w_91 4 1;
          assert_env_length w_91 3;
          drop_n w_91 3 1;
          assert_env_length w_91 2;
          return_n w_91 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_91);
          assert_env_length w_91 2;
          push_env w_91 (Memo.from_constructor tag_None);
          assert_env_length w_91 3;
          drop_n w_91 3 1;
          assert_env_length w_91 2;
          return_n w_91 2 (pc_to_exp (int_to_pc 0))
      | c_57 -> failwith ("unreachable:" ^ string_of_int c_57 ^ "(91)"))
    91

let () =
  add_exp
    (fun w_90 ->
      assert_env_length w_90 2;
      let last_56 = Source.E 1 in
      let x_56 = resolve w_90 last_56 in
      match Word.get_value (fst x_56) with
      | c_56 when c_56 = tag_ECons ->
          let splits_54 = Memo.splits (snd x_56) in
          let split0_54 = List.nth splits_54 0 in
          let split1_31 = List.nth splits_54 1 in
          ignore (pop_env w_90);
          push_env w_90 split0_54;
          push_env w_90 split1_31;
          assert_env_length w_90 3;
          push_env w_90 (Memo.from_constructor tag_None);
          assert_env_length w_90 4;
          drop_n w_90 4 2;
          assert_env_length w_90 2;
          return_n w_90 2 (pc_to_exp (int_to_pc 0))
      | c_56 when c_56 = tag_EAtom ->
          let splits_55 = Memo.splits (snd x_56) in
          let split0_55 = List.nth splits_55 0 in
          ignore (pop_env w_90);
          push_env w_90 split0_55;
          assert_env_length w_90 2;
          push_env w_90 (Dynarray.get w_90.state.e 1);
          w_90.state.c <- pc_to_exp (int_to_pc 91);
          stepped w_90
      | c_56 -> failwith ("unreachable:" ^ string_of_int c_56 ^ "(92)"))
    92

let () =
  add_exp
    (fun w_92 ->
      assert_env_length w_92 1;
      push_env w_92 (Dynarray.get w_92.state.e 0);
      w_92.state.c <- pc_to_exp (int_to_pc 95);
      stepped w_92)
    93

let () =
  add_exp
    (fun w_94 ->
      assert_env_length w_94 3;
      let last_59 = Source.E 2 in
      let x_59 = resolve w_94 last_59 in
      match Word.get_value (fst x_59) with
      | c_59 when c_59 = tag_ECons ->
          let splits_60 = Memo.splits (snd x_59) in
          let split0_60 = List.nth splits_60 0 in
          let split1_32 = List.nth splits_60 1 in
          ignore (pop_env w_94);
          push_env w_94 split0_60;
          push_env w_94 split1_32;
          assert_env_length w_94 4;
          push_env w_94 (Dynarray.get w_94.state.e 1);
          assert_env_length w_94 5;
          let keep_vals_2 = env_call w_94 [] 1 in
          w_94.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_vals_2; w_94.state.k ];
          w_94.state.c <- pc_to_exp (int_to_pc 53);
          stepped w_94
      | _ ->
          ignore (pop_env w_94);
          failwith "car: PAIR expected"
      | c_59 -> failwith ("unreachable:" ^ string_of_int c_59 ^ "(94)"))
    94

let () =
  add_exp
    (fun w_93 ->
      assert_env_length w_93 2;
      let last_58 = Source.E 1 in
      let x_58 = resolve w_93 last_58 in
      match Word.get_value (fst x_58) with
      | c_58 when c_58 = tag_VNumber ->
          let splits_57 = Memo.splits (snd x_58) in
          let split0_57 = List.nth splits_57 0 in
          ignore (pop_env w_93);
          push_env w_93 split0_57;
          failwith "car: cannot apply on NUMBER"
      | c_58 when c_58 = tag_VSymbol ->
          let splits_58 = Memo.splits (snd x_58) in
          let split0_58 = List.nth splits_58 0 in
          ignore (pop_env w_93);
          push_env w_93 split0_58;
          failwith "car: cannot apply on SYMBOL"
      | c_58 when c_58 = tag_VQuote ->
          let splits_59 = Memo.splits (snd x_58) in
          let split0_59 = List.nth splits_59 0 in
          ignore (pop_env w_93);
          push_env w_93 split0_59;
          assert_env_length w_93 2;
          push_env w_93 (Dynarray.get w_93.state.e 1);
          w_93.state.c <- pc_to_exp (int_to_pc 94);
          stepped w_93
      | c_58 when c_58 = tag_VNIL ->
          ignore (pop_env w_93);
          failwith "car: cannot apply on NIL"
      | c_58 when c_58 = tag_VCons ->
          let splits_61 = Memo.splits (snd x_58) in
          let split0_61 = List.nth splits_61 0 in
          let split1_33 = List.nth splits_61 1 in
          ignore (pop_env w_93);
          push_env w_93 split0_61;
          push_env w_93 split1_33;
          assert_env_length w_93 3;
          push_env w_93 (Dynarray.get w_93.state.e 1);
          assert_env_length w_93 4;
          drop_n w_93 4 2;
          assert_env_length w_93 2;
          return_n w_93 2 (pc_to_exp (int_to_pc 0))
      | c_58 when c_58 = tag_VClosure ->
          let splits_62 = Memo.splits (snd x_58) in
          let split0_62 = List.nth splits_62 0 in
          let split1_34 = List.nth splits_62 1 in
          let split2_2 = List.nth splits_62 2 in
          ignore (pop_env w_93);
          push_env w_93 split0_62;
          push_env w_93 split1_34;
          push_env w_93 split2_2;
          failwith "car: cannot apply on CLOSURE"
      | c_58 -> failwith ("unreachable:" ^ string_of_int c_58 ^ "(95)"))
    95

let () =
  add_exp
    (fun w_95 ->
      assert_env_length w_95 1;
      push_env w_95 (Dynarray.get w_95.state.e 0);
      w_95.state.c <- pc_to_exp (int_to_pc 98);
      stepped w_95)
    96

let () =
  add_exp
    (fun w_97 ->
      assert_env_length w_97 3;
      let last_61 = Source.E 2 in
      let x_61 = resolve w_97 last_61 in
      match Word.get_value (fst x_61) with
      | c_61 when c_61 = tag_ECons ->
          let splits_66 = Memo.splits (snd x_61) in
          let split0_66 = List.nth splits_66 0 in
          let split1_35 = List.nth splits_66 1 in
          ignore (pop_env w_97);
          push_env w_97 split0_66;
          push_env w_97 split1_35;
          assert_env_length w_97 4;
          push_env w_97 (Dynarray.get w_97.state.e 1);
          assert_env_length w_97 5;
          let keep_vals_3 = env_call w_97 [] 1 in
          w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_vals_3; w_97.state.k ];
          w_97.state.c <- pc_to_exp (int_to_pc 55);
          stepped w_97
      | _ ->
          ignore (pop_env w_97);
          failwith "cdr: PAIR expected"
      | c_61 -> failwith ("unreachable:" ^ string_of_int c_61 ^ "(97)"))
    97

let () =
  add_exp
    (fun w_96 ->
      assert_env_length w_96 2;
      let last_60 = Source.E 1 in
      let x_60 = resolve w_96 last_60 in
      match Word.get_value (fst x_60) with
      | c_60 when c_60 = tag_VNumber ->
          let splits_63 = Memo.splits (snd x_60) in
          let split0_63 = List.nth splits_63 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_63;
          failwith "cdr: cannot apply on NUMBER"
      | c_60 when c_60 = tag_VSymbol ->
          let splits_64 = Memo.splits (snd x_60) in
          let split0_64 = List.nth splits_64 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_64;
          failwith "cdr: cannot apply on SYMBOL"
      | c_60 when c_60 = tag_VQuote ->
          let splits_65 = Memo.splits (snd x_60) in
          let split0_65 = List.nth splits_65 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_65;
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 97);
          stepped w_96
      | c_60 when c_60 = tag_VNIL ->
          ignore (pop_env w_96);
          failwith "cdr: cannot apply on NIL"
      | c_60 when c_60 = tag_VCons ->
          let splits_67 = Memo.splits (snd x_60) in
          let split0_67 = List.nth splits_67 0 in
          let split1_36 = List.nth splits_67 1 in
          ignore (pop_env w_96);
          push_env w_96 split0_67;
          push_env w_96 split1_36;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 2);
          assert_env_length w_96 4;
          drop_n w_96 4 2;
          assert_env_length w_96 2;
          return_n w_96 2 (pc_to_exp (int_to_pc 0))
      | c_60 when c_60 = tag_VClosure ->
          let splits_68 = Memo.splits (snd x_60) in
          let split0_68 = List.nth splits_68 0 in
          let split1_37 = List.nth splits_68 1 in
          let split2_3 = List.nth splits_68 2 in
          ignore (pop_env w_96);
          push_env w_96 split0_68;
          push_env w_96 split1_37;
          push_env w_96 split2_3;
          failwith "cdr: cannot apply on CLOSURE"
      | c_60 -> failwith ("unreachable:" ^ string_of_int c_60 ^ "(98)"))
    98

let () =
  add_exp
    (fun w_98 ->
      assert_env_length w_98 1;
      push_env w_98 (Dynarray.get w_98.state.e 0);
      w_98.state.c <- pc_to_exp (int_to_pc 100);
      stepped w_98)
    99

let () =
  add_exp
    (fun w_99 ->
      assert_env_length w_99 2;
      let last_62 = Source.E 1 in
      let x_62 = resolve w_99 last_62 in
      match Word.get_value (fst x_62) with
      | c_62 when c_62 = tag_VNumber ->
          let splits_69 = Memo.splits (snd x_62) in
          let split0_69 = List.nth splits_69 0 in
          ignore (pop_env w_99);
          push_env w_99 split0_69;
          assert_env_length w_99 2;
          push_env w_99 (Memo.from_constructor tag_Unit);
          assert_env_length w_99 3;
          ignore (env_call w_99 [] 1);
          w_99.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_99
      | c_62 when c_62 = tag_VSymbol ->
          let splits_70 = Memo.splits (snd x_62) in
          let split0_70 = List.nth splits_70 0 in
          ignore (pop_env w_99);
          push_env w_99 split0_70;
          assert_env_length w_99 2;
          push_env w_99 (Memo.from_constructor tag_Unit);
          assert_env_length w_99 3;
          ignore (env_call w_99 [] 1);
          w_99.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_99
      | c_62 when c_62 = tag_VQuote ->
          let splits_71 = Memo.splits (snd x_62) in
          let split0_71 = List.nth splits_71 0 in
          ignore (pop_env w_99);
          push_env w_99 split0_71;
          assert_env_length w_99 2;
          push_env w_99 (Memo.from_constructor tag_Unit);
          assert_env_length w_99 3;
          ignore (env_call w_99 [] 1);
          w_99.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_99
      | c_62 when c_62 = tag_VNIL ->
          ignore (pop_env w_99);
          assert_env_length w_99 1;
          push_env w_99 (Memo.from_constructor tag_Unit);
          assert_env_length w_99 2;
          ignore (env_call w_99 [] 1);
          w_99.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_99
      | c_62 when c_62 = tag_VCons ->
          let splits_72 = Memo.splits (snd x_62) in
          let split0_72 = List.nth splits_72 0 in
          let split1_38 = List.nth splits_72 1 in
          ignore (pop_env w_99);
          push_env w_99 split0_72;
          push_env w_99 split1_38;
          assert_env_length w_99 3;
          push_env w_99 (Memo.from_constructor tag_Unit);
          assert_env_length w_99 4;
          ignore (env_call w_99 [] 1);
          w_99.state.c <- pc_to_exp (int_to_pc 21);
          stepped w_99
      | c_62 when c_62 = tag_VClosure ->
          let splits_73 = Memo.splits (snd x_62) in
          let split0_73 = List.nth splits_73 0 in
          let split1_39 = List.nth splits_73 1 in
          let split2_4 = List.nth splits_73 2 in
          ignore (pop_env w_99);
          push_env w_99 split0_73;
          push_env w_99 split1_39;
          push_env w_99 split2_4;
          assert_env_length w_99 4;
          push_env w_99 (Memo.from_constructor tag_Unit);
          assert_env_length w_99 5;
          ignore (env_call w_99 [] 1);
          w_99.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_99
      | c_62 -> failwith ("unreachable:" ^ string_of_int c_62 ^ "(100)"))
    100

let () =
  add_exp
    (fun w_100 ->
      assert_env_length w_100 2;
      push_env w_100 (Dynarray.get w_100.state.e 0);
      assert_env_length w_100 3;
      push_env w_100 (Dynarray.get w_100.state.e 1);
      assert_env_length w_100 4;
      let keep_vals_4 = env_call w_100 [] 2 in
      w_100.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_vals_4; w_100.state.k ];
      w_100.state.c <- pc_to_exp (int_to_pc 79);
      stepped w_100)
    101

let () =
  add_exp
    (fun w_101 ->
      assert_env_length w_101 2;
      push_env w_101 (Dynarray.get w_101.state.e 1);
      assert_env_length w_101 3;
      let keep_vals_5 = env_call w_101 [ 0 ] 1 in
      w_101.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_vals_5; w_101.state.k ];
      w_101.state.c <- pc_to_exp (int_to_pc 23);
      stepped w_101)
    102

let () =
  add_exp
    (fun w_102 ->
      assert_env_length w_102 3;
      push_env w_102 (Dynarray.get w_102.state.e 0);
      w_102.state.c <- pc_to_exp (int_to_pc 109);
      stepped w_102)
    103

let () =
  add_exp
    (fun w_105 ->
      assert_env_length w_105 2;
      push_env w_105 (Dynarray.get w_105.state.e 0);
      w_105.state.c <- pc_to_exp (int_to_pc 110);
      stepped w_105)
    104

let () =
  add_exp
    (fun w_107 ->
      assert_env_length w_107 2;
      push_env w_107 (Dynarray.get w_107.state.e 0);
      w_107.state.c <- pc_to_exp (int_to_pc 111);
      stepped w_107)
    105

let () =
  add_exp
    (fun w_109 ->
      assert_env_length w_109 1;
      push_env w_109 (Dynarray.get w_109.state.e 0);
      w_109.state.c <- pc_to_exp (int_to_pc 112);
      stepped w_109)
    106

let () =
  add_exp
    (fun w_111 ->
      assert_env_length w_111 2;
      push_env w_111 (Dynarray.get w_111.state.e 0);
      w_111.state.c <- pc_to_exp (int_to_pc 114);
      stepped w_111)
    107

let () =
  add_exp
    (fun w_104 ->
      assert_env_length w_104 6;
      let last_64 = Source.E 5 in
      let x_64 = resolve w_104 last_64 in
      match Word.get_value (fst x_64) with
      | c_64 when c_64 = tag_Nil ->
          ignore (pop_env w_104);
          failwith "pairlis: arguments too few"
      | c_64 when c_64 = tag_Cons ->
          let splits_75 = Memo.splits (snd x_64) in
          let split0_75 = List.nth splits_75 0 in
          let split1_41 = List.nth splits_75 1 in
          ignore (pop_env w_104);
          push_env w_104 split0_75;
          push_env w_104 split1_41;
          assert_env_length w_104 7;
          push_env w_104 (Dynarray.get w_104.state.e 4);
          assert_env_length w_104 8;
          push_env w_104 (Dynarray.get w_104.state.e 6);
          assert_env_length w_104 9;
          push_env w_104 (Dynarray.get w_104.state.e 3);
          assert_env_length w_104 10;
          push_env w_104 (Dynarray.get w_104.state.e 5);
          assert_env_length w_104 11;
          let ctor_arg_22 = pop_env w_104 in
          let ctor_arg_23 = pop_env w_104 in
          push_env w_104 (Memo.appends [ Memo.from_constructor tag_EnvEntry; ctor_arg_23; ctor_arg_22 ]);
          assert_env_length w_104 10;
          push_env w_104 (Dynarray.get w_104.state.e 2);
          assert_env_length w_104 11;
          let keep_vals_6 = env_call w_104 [ 7; 8 ] 2 in
          w_104.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_vals_6; w_104.state.k ];
          w_104.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_104
      | c_64 -> failwith ("unreachable:" ^ string_of_int c_64 ^ "(108)"))
    108

let () =
  add_exp
    (fun w_103 ->
      assert_env_length w_103 4;
      let last_63 = Source.E 3 in
      let x_63 = resolve w_103 last_63 in
      match Word.get_value (fst x_63) with
      | c_63 when c_63 = tag_Nil ->
          ignore (pop_env w_103);
          assert_env_length w_103 3;
          push_env w_103 (Dynarray.get w_103.state.e 2);
          assert_env_length w_103 4;
          return_n w_103 4 (pc_to_exp (int_to_pc 0))
      | c_63 when c_63 = tag_Cons ->
          let splits_74 = Memo.splits (snd x_63) in
          let split0_74 = List.nth splits_74 0 in
          let split1_40 = List.nth splits_74 1 in
          ignore (pop_env w_103);
          push_env w_103 split0_74;
          push_env w_103 split1_40;
          assert_env_length w_103 5;
          push_env w_103 (Dynarray.get w_103.state.e 1);
          w_103.state.c <- pc_to_exp (int_to_pc 108);
          stepped w_103
      | c_63 -> failwith ("unreachable:" ^ string_of_int c_63 ^ "(109)"))
    109

let () =
  add_exp
    (fun w_106 ->
      assert_env_length w_106 3;
      let last_65 = Source.E 2 in
      let x_65 = resolve w_106 last_65 in
      match Word.get_value (fst x_65) with
      | c_65 when c_65 = tag_ECons ->
          let splits_76 = Memo.splits (snd x_65) in
          let split0_76 = List.nth splits_76 0 in
          let split1_42 = List.nth splits_76 1 in
          ignore (pop_env w_106);
          push_env w_106 split0_76;
          push_env w_106 split1_42;
          assert_env_length w_106 4;
          push_env w_106 (Dynarray.get w_106.state.e 2);
          assert_env_length w_106 5;
          push_env w_106 (Dynarray.get w_106.state.e 1);
          assert_env_length w_106 6;
          let keep_vals_7 = env_call w_106 [ 1; 3 ] 2 in
          w_106.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_vals_7; w_106.state.k ];
          w_106.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_106
      | _ ->
          ignore (pop_env w_106);
          assert_env_length w_106 2;
          push_env w_106 (Memo.from_constructor tag_Nil);
          assert_env_length w_106 3;
          return_n w_106 3 (pc_to_exp (int_to_pc 0))
      | c_65 -> failwith ("unreachable:" ^ string_of_int c_65 ^ "(110)"))
    110

let () =
  add_exp
    (fun w_108 ->
      assert_env_length w_108 3;
      let last_66 = Source.E 2 in
      let x_66 = resolve w_108 last_66 in
      match Word.get_value (fst x_66) with
      | c_66 when c_66 = tag_ECons ->
          let splits_77 = Memo.splits (snd x_66) in
          let split0_77 = List.nth splits_77 0 in
          let split1_43 = List.nth splits_77 1 in
          ignore (pop_env w_108);
          push_env w_108 split0_77;
          push_env w_108 split1_43;
          assert_env_length w_108 4;
          push_env w_108 (Dynarray.get w_108.state.e 2);
          assert_env_length w_108 5;
          let keep_vals_8 = env_call w_108 [ 1; 2; 3 ] 1 in
          w_108.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_vals_8; w_108.state.k ];
          w_108.state.c <- pc_to_exp (int_to_pc 53);
          stepped w_108
      | _ ->
          ignore (pop_env w_108);
          failwith "no cond clause matched"
      | c_66 -> failwith ("unreachable:" ^ string_of_int c_66 ^ "(111)"))
    111

let () =
  add_exp
    (fun w_110 ->
      assert_env_length w_110 2;
      let last_67 = Source.E 1 in
      let x_67 = resolve w_110 last_67 in
      match Word.get_value (fst x_67) with
      | c_67 when c_67 = tag_ECons ->
          let splits_78 = Memo.splits (snd x_67) in
          let split0_78 = List.nth splits_78 0 in
          let split1_44 = List.nth splits_78 1 in
          ignore (pop_env w_110);
          push_env w_110 split0_78;
          push_env w_110 split1_44;
          assert_env_length w_110 3;
          push_env w_110 (Dynarray.get w_110.state.e 1);
          assert_env_length w_110 4;
          let keep_vals_9 = env_call w_110 [ 2 ] 1 in
          w_110.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_vals_9; w_110.state.k ];
          w_110.state.c <- pc_to_exp (int_to_pc 90);
          stepped w_110
      | c_67 -> failwith ("unreachable:" ^ string_of_int c_67 ^ "(112)"))
    112

let () =
  add_exp
    (fun w_113 ->
      assert_env_length w_113 4;
      let last_69 = Source.E 3 in
      let x_69 = resolve w_113 last_69 in
      match Word.get_value (fst x_69) with
      | c_69 when c_69 = tag_AVar ->
          let splits_80 = Memo.splits (snd x_69) in
          let split0_80 = List.nth splits_80 0 in
          ignore (pop_env w_113);
          push_env w_113 split0_80;
          assert_env_length w_113 4;
          push_env w_113 (Dynarray.get w_113.state.e 3);
          assert_env_length w_113 5;
          push_env w_113 (Dynarray.get w_113.state.e 1);
          assert_env_length w_113 6;
          ignore (env_call w_113 [] 2);
          w_113.state.c <- pc_to_exp (int_to_pc 102);
          stepped w_113
      | c_69 when c_69 = tag_ANumber ->
          let splits_81 = Memo.splits (snd x_69) in
          let split0_81 = List.nth splits_81 0 in
          ignore (pop_env w_113);
          push_env w_113 split0_81;
          assert_env_length w_113 4;
          push_env w_113 (Dynarray.get w_113.state.e 3);
          assert_env_length w_113 5;
          let ctor_arg_24 = pop_env w_113 in
          push_env w_113 (Memo.appends [ Memo.from_constructor tag_VNumber; ctor_arg_24 ]);
          assert_env_length w_113 5;
          drop_n w_113 5 1;
          assert_env_length w_113 4;
          drop_n w_113 4 1;
          assert_env_length w_113 3;
          return_n w_113 3 (pc_to_exp (int_to_pc 0))
      | c_69 when c_69 = tag_ASymbol ->
          let splits_82 = Memo.splits (snd x_69) in
          let split0_82 = List.nth splits_82 0 in
          ignore (pop_env w_113);
          push_env w_113 split0_82;
          assert_env_length w_113 4;
          push_env w_113 (Dynarray.get w_113.state.e 3);
          assert_env_length w_113 5;
          let ctor_arg_25 = pop_env w_113 in
          push_env w_113 (Memo.appends [ Memo.from_constructor tag_VSymbol; ctor_arg_25 ]);
          assert_env_length w_113 5;
          drop_n w_113 5 1;
          assert_env_length w_113 4;
          drop_n w_113 4 1;
          assert_env_length w_113 3;
          return_n w_113 3 (pc_to_exp (int_to_pc 0))
      | c_69 when c_69 = tag_ANIL ->
          ignore (pop_env w_113);
          assert_env_length w_113 3;
          push_env w_113 (Memo.from_constructor tag_VNIL);
          assert_env_length w_113 4;
          drop_n w_113 4 1;
          assert_env_length w_113 3;
          return_n w_113 3 (pc_to_exp (int_to_pc 0))
      | c_69 -> failwith ("unreachable:" ^ string_of_int c_69 ^ "(113)"))
    113

let () =
  add_exp
    (fun w_112 ->
      assert_env_length w_112 3;
      let last_68 = Source.E 2 in
      let x_68 = resolve w_112 last_68 in
      match Word.get_value (fst x_68) with
      | c_68 when c_68 = tag_EAtom ->
          let splits_79 = Memo.splits (snd x_68) in
          let split0_79 = List.nth splits_79 0 in
          ignore (pop_env w_112);
          push_env w_112 split0_79;
          assert_env_length w_112 3;
          push_env w_112 (Dynarray.get w_112.state.e 2);
          w_112.state.c <- pc_to_exp (int_to_pc 113);
          stepped w_112
      | c_68 when c_68 = tag_ECons ->
          let splits_83 = Memo.splits (snd x_68) in
          let split0_83 = List.nth splits_83 0 in
          let split1_45 = List.nth splits_83 1 in
          ignore (pop_env w_112);
          push_env w_112 split0_83;
          push_env w_112 split1_45;
          assert_env_length w_112 4;
          push_env w_112 (Dynarray.get w_112.state.e 0);
          assert_env_length w_112 5;
          let keep_vals_10 = env_call w_112 [ 0; 1 ] 1 in
          w_112.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_vals_10; w_112.state.k ];
          w_112.state.c <- pc_to_exp (int_to_pc 53);
          stepped w_112
      | c_68 -> failwith ("unreachable:" ^ string_of_int c_68 ^ "(114)"))
    114

let () =
  add_exp
    (fun w_114 ->
      assert_env_length w_114 2;
      push_env w_114 (Dynarray.get w_114.state.e 0);
      assert_env_length w_114 3;
      push_env w_114 (Dynarray.get w_114.state.e 1);
      assert_env_length w_114 4;
      let ctor_arg_26 = pop_env w_114 in
      let ctor_arg_27 = pop_env w_114 in
      push_env w_114 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_27; ctor_arg_26 ]);
      assert_env_length w_114 3;
      push_env w_114 (Memo.from_constructor tag_Nil);
      assert_env_length w_114 4;
      let ctor_arg_28 = pop_env w_114 in
      push_env w_114 (Memo.appends [ Memo.from_constructor tag_MkEnv; ctor_arg_28 ]);
      assert_env_length w_114 4;
      ignore (env_call w_114 [] 2);
      w_114.state.c <- pc_to_exp (int_to_pc 107);
      stepped w_114)
    115

let () =
  add_exp
    (fun w_116 ->
      assert_env_length w_116 1;
      let cond_0 = resolve w_116 (Source.E 0) in
      ignore (pop_env w_116);
      let if_kont_0 =
       fun _ ->
        assert_env_length w_116 1;
        return_n w_116 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_116 0;
        push_env w_116 (Memo.from_constructor tag_Unit);
        assert_env_length w_116 1;
        ignore (env_call w_116 [] 1);
        w_116.state.c <- pc_to_exp (int_to_pc 22);
        stepped w_116)
      else (
        assert_env_length w_116 0;
        push_env w_116 (Memo.from_constructor tag_Unit);
        assert_env_length w_116 1;
        ignore (env_call w_116 [] 1);
        w_116.state.c <- pc_to_exp (int_to_pc 21);
        stepped w_116))
    116

let () =
  add_exp
    (fun w_117 ->
      assert_env_length w_117 2;
      let last_70 = Source.E 1 in
      let x_70 = resolve w_117 last_70 in
      match Word.get_value (fst x_70) with
      | c_70 when c_70 = tag_Nil ->
          ignore (pop_env w_117);
          failwith "empty environment"
      | c_70 when c_70 = tag_Cons ->
          let splits_84 = Memo.splits (snd x_70) in
          let split0_84 = List.nth splits_84 0 in
          let split1_46 = List.nth splits_84 1 in
          ignore (pop_env w_117);
          push_env w_117 split0_84;
          push_env w_117 split1_46;
          assert_env_length w_117 3;
          push_env w_117 (Dynarray.get w_117.state.e 1);
          assert_env_length w_117 4;
          let keep_vals_13 = env_call w_117 [ 0; 1; 2 ] 1 in
          w_117.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_vals_13; w_117.state.k ];
          w_117.state.c <- pc_to_exp (int_to_pc 6);
          stepped w_117
      | c_70 -> failwith ("unreachable:" ^ string_of_int c_70 ^ "(117)"))
    117

let () =
  add_exp
    (fun w_118 ->
      assert_env_length w_118 2;
      let last_71 = Source.E 1 in
      let x_71 = resolve w_118 last_71 in
      match Word.get_value (fst x_71) with
      | c_71 when c_71 = tag_Some ->
          let splits_85 = Memo.splits (snd x_71) in
          let split0_85 = List.nth splits_85 0 in
          ignore (pop_env w_118);
          push_env w_118 split0_85;
          assert_env_length w_118 2;
          push_env w_118 (Dynarray.get w_118.state.e 1);
          assert_env_length w_118 3;
          push_env w_118 (Dynarray.get w_118.state.e 0);
          assert_env_length w_118 4;
          let keep_vals_16 = env_call w_118 [ 2 ] 1 in
          w_118.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_vals_16; w_118.state.k ];
          w_118.state.c <- pc_to_exp (int_to_pc 106);
          stepped w_118
      | _ ->
          ignore (pop_env w_118);
          failwith "destruct_names: impossible, names must be int literals"
      | _ ->
          ignore (pop_env w_118);
          assert_env_length w_118 1;
          push_env w_118 (Memo.from_constructor tag_Nil);
          assert_env_length w_118 2;
          drop_n w_118 2 1;
          assert_env_length w_118 1;
          return_n w_118 1 (pc_to_exp (int_to_pc 0))
      | c_71 -> failwith ("unreachable:" ^ string_of_int c_71 ^ "(118)"))
    118

let () =
  add_exp
    (fun w_119 ->
      assert_env_length w_119 2;
      let x0_4 = resolve w_119 (Source.E 0) in
      let x1_4 = resolve w_119 (Source.E 1) in
      ignore (pop_env w_119);
      ignore (pop_env w_119);
      push_env w_119 (Memo.from_int (if Word.get_value (fst x0_4) <> 0 && Word.get_value (fst x1_4) <> 0 then 1 else 0));
      assert_env_length w_119 1;
      drop_n w_119 1 0;
      assert_env_length w_119 1;
      drop_n w_119 1 0;
      assert_env_length w_119 1;
      return_n w_119 1 (pc_to_exp (int_to_pc 0)))
    119

let () =
  add_exp
    (fun w_121 ->
      assert_env_length w_121 4;
      let cond_1 = resolve w_121 (Source.E 3) in
      ignore (pop_env w_121);
      let if_kont_1 =
       fun _ ->
        assert_env_length w_121 4;
        drop_n w_121 4 2;
        assert_env_length w_121 2;
        return_n w_121 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_121 3;
        push_env w_121 (Dynarray.get w_121.state.e 1);
        assert_env_length w_121 4;
        ignore (env_call w_121 [] 1);
        w_121.state.c <- pc_to_exp (int_to_pc 8);
        stepped w_121)
      else (
        assert_env_length w_121 3;
        push_env w_121 (Dynarray.get w_121.state.e 0);
        assert_env_length w_121 4;
        push_env w_121 (Dynarray.get w_121.state.e 2);
        assert_env_length w_121 5;
        let ctor_arg_38 = pop_env w_121 in
        push_env w_121 (Memo.appends [ Memo.from_constructor tag_MkEnv; ctor_arg_38 ]);
        assert_env_length w_121 5;
        ignore (env_call w_121 [] 2);
        w_121.state.c <- pc_to_exp (int_to_pc 102);
        stepped w_121))
    120

let () =
  add_exp
    (fun w_120 ->
      assert_env_length w_120 5;
      let x0_5 = resolve w_120 (Source.E 3) in
      let x1_5 = resolve w_120 (Source.E 4) in
      ignore (pop_env w_120);
      ignore (pop_env w_120);
      push_env w_120 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      w_120.state.c <- pc_to_exp (int_to_pc 120);
      stepped w_120)
    121

let () =
  add_exp
    (fun w_123 ->
      assert_env_length w_123 4;
      let last_73 = Source.E 3 in
      let x_73 = resolve w_123 last_73 in
      match Word.get_value (fst x_73) with
      | c_73 when c_73 = tag_SQuote ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_19 = env_call w_123 [] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_vals_19; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_123
      | c_73 when c_73 = tag_SAtom ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_20 = env_call w_123 [ 1 ] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_vals_20; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_123
      | c_73 when c_73 = tag_SEq ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_21 = env_call w_123 [ 0; 1 ] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_vals_21; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_123
      | c_73 when c_73 = tag_SCar ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_22 = env_call w_123 [ 1 ] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_vals_22; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_123
      | c_73 when c_73 = tag_SCdr ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_23 = env_call w_123 [ 1 ] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_vals_23; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_123
      | c_73 when c_73 = tag_SCons ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_24 = env_call w_123 [ 0; 1 ] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_vals_24; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_123
      | c_73 when c_73 = tag_SCond ->
          ignore (pop_env w_123);
          assert_env_length w_123 3;
          push_env w_123 (Dynarray.get w_123.state.e 0);
          assert_env_length w_123 4;
          let keep_vals_25 = env_call w_123 [ 1 ] 1 in
          w_123.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_vals_25; w_123.state.k ];
          w_123.state.c <- pc_to_exp (int_to_pc 55);
          stepped w_123
      | _ ->
          ignore (pop_env w_123);
          failwith "invalid symbol here"
      | c_73 -> failwith ("unreachable:" ^ string_of_int c_73 ^ "(122)"))
    122

let () =
  add_exp
    (fun w_122 ->
      assert_env_length w_122 3;
      let last_72 = Source.E 2 in
      let x_72 = resolve w_122 last_72 in
      match Word.get_value (fst x_72) with
      | c_72 when c_72 = tag_Some ->
          let splits_86 = Memo.splits (snd x_72) in
          let split0_86 = List.nth splits_86 0 in
          ignore (pop_env w_122);
          push_env w_122 split0_86;
          assert_env_length w_122 3;
          push_env w_122 (Dynarray.get w_122.state.e 2);
          w_122.state.c <- pc_to_exp (int_to_pc 122);
          stepped w_122
      | c_72 when c_72 = tag_None ->
          ignore (pop_env w_122);
          assert_env_length w_122 2;
          push_env w_122 (Dynarray.get w_122.state.e 0);
          assert_env_length w_122 3;
          let keep_vals_26 = env_call w_122 [ 0; 1 ] 1 in
          w_122.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_vals_26; w_122.state.k ];
          w_122.state.c <- pc_to_exp (int_to_pc 53);
          stepped w_122
      | c_72 -> failwith ("unreachable:" ^ string_of_int c_72 ^ "(123)"))
    123

let () =
  add_exp
    (fun w_124 ->
      assert_env_length w_124 3;
      let last_74 = Source.E 2 in
      let x_74 = resolve w_124 last_74 in
      match Word.get_value (fst x_74) with
      | c_74 when c_74 = tag_ECons ->
          let splits_87 = Memo.splits (snd x_74) in
          let split0_87 = List.nth splits_87 0 in
          let split1_47 = List.nth splits_87 1 in
          ignore (pop_env w_124);
          push_env w_124 split0_87;
          push_env w_124 split1_47;
          assert_env_length w_124 4;
          push_env w_124 (Dynarray.get w_124.state.e 0);
          assert_env_length w_124 5;
          let keep_vals_33 = env_call w_124 [ 0; 1 ] 1 in
          w_124.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_vals_33; w_124.state.k ];
          w_124.state.c <- pc_to_exp (int_to_pc 34);
          stepped w_124
      | _ ->
          ignore (pop_env w_124);
          assert_env_length w_124 2;
          push_env w_124 (Dynarray.get w_124.state.e 0);
          assert_env_length w_124 3;
          let keep_vals_34 = env_call w_124 [ 0; 1 ] 1 in
          w_124.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_vals_34; w_124.state.k ];
          w_124.state.c <- pc_to_exp (int_to_pc 53);
          stepped w_124
      | c_74 -> failwith ("unreachable:" ^ string_of_int c_74 ^ "(124)"))
    124

let () =
  add_exp
    (fun w_125 ->
      assert_env_length w_125 4;
      let cond_2 = resolve w_125 (Source.E 3) in
      ignore (pop_env w_125);
      let if_kont_2 =
       fun _ ->
        assert_env_length w_125 4;
        drop_n w_125 4 2;
        assert_env_length w_125 2;
        return_n w_125 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_125 3;
        push_env w_125 (Dynarray.get w_125.state.e 2);
        assert_env_length w_125 4;
        push_env w_125 (Dynarray.get w_125.state.e 0);
        assert_env_length w_125 5;
        ignore (env_call w_125 [] 2);
        w_125.state.c <- pc_to_exp (int_to_pc 105);
        stepped w_125)
      else (
        assert_env_length w_125 3;
        push_env w_125 (Dynarray.get w_125.state.e 1);
        assert_env_length w_125 4;
        let keep_vals_35 = env_call w_125 [ 0 ] 1 in
        w_125.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_vals_35; w_125.state.k ];
        w_125.state.c <- pc_to_exp (int_to_pc 31);
        stepped w_125))
    125

let () =
  add_exp
    (fun w_127 ->
      assert_env_length w_127 4;
      let last_76 = Source.E 3 in
      let x_76 = resolve w_127 last_76 in
      match Word.get_value (fst x_76) with
      | c_76 when c_76 = tag_SLambda ->
          ignore (pop_env w_127);
          assert_env_length w_127 3;
          push_env w_127 (Dynarray.get w_127.state.e 0);
          assert_env_length w_127 4;
          let keep_vals_42 = env_call w_127 [ 0; 1 ] 1 in
          w_127.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_vals_42; w_127.state.k ];
          w_127.state.c <- pc_to_exp (int_to_pc 44);
          stepped w_127
      | c_76 when c_76 = tag_SDefine ->
          ignore (pop_env w_127);
          assert_env_length w_127 3;
          push_env w_127 (Memo.from_constructor tag_SLambda);
          assert_env_length w_127 4;
          let ctor_arg_44 = pop_env w_127 in
          push_env w_127 (Memo.appends [ Memo.from_constructor tag_ASymbol; ctor_arg_44 ]);
          assert_env_length w_127 4;
          let ctor_arg_45 = pop_env w_127 in
          push_env w_127 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_45 ]);
          assert_env_length w_127 4;
          push_env w_127 (Dynarray.get w_127.state.e 0);
          assert_env_length w_127 5;
          let keep_vals_43 = env_call w_127 [ 0; 1; 3 ] 1 in
          w_127.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_vals_43; w_127.state.k ];
          w_127.state.c <- pc_to_exp (int_to_pc 40);
          stepped w_127
      | c_76 when c_76 = tag_SQuote ->
          ignore (pop_env w_127);
          failwith "unexpected function quote"
      | c_76 when c_76 = tag_SEq ->
          ignore (pop_env w_127);
          failwith "unexpected function eq"
      | c_76 when c_76 = tag_SCons ->
          ignore (pop_env w_127);
          failwith "unexpected function cons"
      | c_76 when c_76 = tag_SCond ->
          ignore (pop_env w_127);
          failwith "unexpected function cond"
      | c_76 when c_76 = tag_SAtom ->
          ignore (pop_env w_127);
          failwith "unexpected function atom"
      | c_76 when c_76 = tag_SCar ->
          ignore (pop_env w_127);
          failwith "unexpected function car"
      | c_76 when c_76 = tag_SCdr ->
          ignore (pop_env w_127);
          failwith "unexpected function cdr"
      | _ ->
          ignore (pop_env w_127);
          failwith "1"
      | c_76 -> failwith ("unreachable:" ^ string_of_int c_76 ^ "(126)"))
    126

let () =
  add_exp
    (fun w_126 ->
      assert_env_length w_126 3;
      let last_75 = Source.E 2 in
      let x_75 = resolve w_126 last_75 in
      match Word.get_value (fst x_75) with
      | c_75 when c_75 = tag_Some ->
          let splits_88 = Memo.splits (snd x_75) in
          let split0_88 = List.nth splits_88 0 in
          ignore (pop_env w_126);
          push_env w_126 split0_88;
          assert_env_length w_126 3;
          push_env w_126 (Dynarray.get w_126.state.e 2);
          w_126.state.c <- pc_to_exp (int_to_pc 126);
          stepped w_126
      | c_75 when c_75 = tag_None ->
          ignore (pop_env w_126);
          assert_env_length w_126 2;
          push_env w_126 (Dynarray.get w_126.state.e 0);
          assert_env_length w_126 3;
          let keep_vals_44 = env_call w_126 [] 1 in
          w_126.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_vals_44; w_126.state.k ];
          w_126.state.c <- pc_to_exp (int_to_pc 34);
          stepped w_126
      | c_75 -> failwith ("unreachable:" ^ string_of_int c_75 ^ "(127)"))
    127

let () =
  add_exp
    (fun w_128 ->
      assert_env_length w_128 3;
      let last_77 = Source.E 2 in
      let x_77 = resolve w_128 last_77 in
      match Word.get_value (fst x_77) with
      | c_77 when c_77 = tag_Some ->
          let splits_89 = Memo.splits (snd x_77) in
          let split0_89 = List.nth splits_89 0 in
          ignore (pop_env w_128);
          push_env w_128 split0_89;
          assert_env_length w_128 3;
          push_env w_128 (Dynarray.get w_128.state.e 2);
          assert_env_length w_128 4;
          push_env w_128 (Dynarray.get w_128.state.e 1);
          assert_env_length w_128 5;
          let keep_vals_45 = env_call w_128 [ 0 ] 2 in
          w_128.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_vals_45; w_128.state.k ];
          w_128.state.c <- pc_to_exp (int_to_pc 102);
          stepped w_128
      | c_77 when c_77 = tag_None ->
          ignore (pop_env w_128);
          failwith "impossible"
      | c_77 -> failwith ("unreachable:" ^ string_of_int c_77 ^ "(128)"))
    128

let () =
  add_exp
    (fun w_130 ->
      assert_env_length w_130 2;
      let last_79 = Source.E 1 in
      let x_79 = resolve w_130 last_79 in
      match Word.get_value (fst x_79) with
      | c_79 when c_79 = tag_AVar ->
          let splits_91 = Memo.splits (snd x_79) in
          let split0_91 = List.nth splits_91 0 in
          ignore (pop_env w_130);
          push_env w_130 split0_91;
          failwith "unexpected function var"
      | c_79 when c_79 = tag_ANumber ->
          let splits_92 = Memo.splits (snd x_79) in
          let split0_92 = List.nth splits_92 0 in
          ignore (pop_env w_130);
          push_env w_130 split0_92;
          failwith "unexpected function number"
      | c_79 when c_79 = tag_ASymbol ->
          let splits_93 = Memo.splits (snd x_79) in
          let split0_93 = List.nth splits_93 0 in
          ignore (pop_env w_130);
          push_env w_130 split0_93;
          failwith "2"
      | c_79 when c_79 = tag_ANIL ->
          ignore (pop_env w_130);
          failwith "unexpected function nil"
      | c_79 -> failwith ("unreachable:" ^ string_of_int c_79 ^ "(129)"))
    129

let () =
  add_exp
    (fun w_129 ->
      assert_env_length w_129 1;
      let last_78 = Source.E 0 in
      let x_78 = resolve w_129 last_78 in
      match Word.get_value (fst x_78) with
      | c_78 when c_78 = tag_EAtom ->
          let splits_90 = Memo.splits (snd x_78) in
          let split0_90 = List.nth splits_90 0 in
          ignore (pop_env w_129);
          push_env w_129 split0_90;
          assert_env_length w_129 1;
          push_env w_129 (Dynarray.get w_129.state.e 0);
          w_129.state.c <- pc_to_exp (int_to_pc 129);
          stepped w_129
      | _ ->
          ignore (pop_env w_129);
          failwith "2"
      | c_78 -> failwith ("unreachable:" ^ string_of_int c_78 ^ "(130)"))
    130

let () =
  add_exp
    (fun w_131 ->
      assert_env_length w_131 2;
      let last_80 = Source.E 1 in
      let x_80 = resolve w_131 last_80 in
      match Word.get_value (fst x_80) with
      | c_80 when c_80 = tag_VNumber ->
          let splits_94 = Memo.splits (snd x_80) in
          let split0_94 = List.nth splits_94 0 in
          ignore (pop_env w_131);
          push_env w_131 split0_94;
          failwith "NUMBER is not PROCEDURE"
      | c_80 when c_80 = tag_VSymbol ->
          let splits_95 = Memo.splits (snd x_80) in
          let split0_95 = List.nth splits_95 0 in
          ignore (pop_env w_131);
          push_env w_131 split0_95;
          failwith "SYMBOL is not PROCEDURE"
      | c_80 when c_80 = tag_VQuote ->
          let splits_96 = Memo.splits (snd x_80) in
          let split0_96 = List.nth splits_96 0 in
          ignore (pop_env w_131);
          push_env w_131 split0_96;
          failwith "QUOTE is not PROCEDURE"
      | c_80 when c_80 = tag_VNIL ->
          ignore (pop_env w_131);
          failwith "NIL is not PROCEDURE"
      | c_80 when c_80 = tag_VCons ->
          let splits_97 = Memo.splits (snd x_80) in
          let split0_97 = List.nth splits_97 0 in
          let split1_48 = List.nth splits_97 1 in
          ignore (pop_env w_131);
          push_env w_131 split0_97;
          push_env w_131 split1_48;
          failwith "PAIR is not PROCEDURE"
      | c_80 when c_80 = tag_VClosure ->
          let splits_98 = Memo.splits (snd x_80) in
          let split0_98 = List.nth splits_98 0 in
          let split1_49 = List.nth splits_98 1 in
          let split2_5 = List.nth splits_98 2 in
          ignore (pop_env w_131);
          push_env w_131 split0_98;
          push_env w_131 split1_49;
          push_env w_131 split2_5;
          assert_env_length w_131 4;
          push_env w_131 (Dynarray.get w_131.state.e 1);
          assert_env_length w_131 5;
          push_env w_131 (Dynarray.get w_131.state.e 1);
          assert_env_length w_131 6;
          push_env w_131 (Dynarray.get w_131.state.e 2);
          assert_env_length w_131 7;
          push_env w_131 (Dynarray.get w_131.state.e 3);
          assert_env_length w_131 8;
          let ctor_arg_50 = pop_env w_131 in
          let ctor_arg_51 = pop_env w_131 in
          let ctor_arg_52 = pop_env w_131 in
          push_env w_131 (Memo.appends [ Memo.from_constructor tag_VClosure; ctor_arg_52; ctor_arg_51; ctor_arg_50 ]);
          assert_env_length w_131 6;
          let ctor_arg_53 = pop_env w_131 in
          let ctor_arg_54 = pop_env w_131 in
          push_env w_131 (Memo.appends [ Memo.from_constructor tag_EnvEntry; ctor_arg_54; ctor_arg_53 ]);
          assert_env_length w_131 5;
          push_env w_131 (Dynarray.get w_131.state.e 3);
          assert_env_length w_131 6;
          let keep_vals_48 = env_call w_131 [ 0; 2 ] 2 in
          w_131.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; keep_vals_48; w_131.state.k ];
          w_131.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_131
      | c_80 -> failwith ("unreachable:" ^ string_of_int c_80 ^ "(131)"))
    131

let () =
  add_exp
    (fun w_132 ->
      assert_env_length w_132 4;
      let last_81 = Source.E 3 in
      let x_81 = resolve w_132 last_81 in
      match Word.get_value (fst x_81) with
      | c_81 when c_81 = tag_None ->
          ignore (pop_env w_132);
          failwith "eval: function name must be int literal"
      | c_81 when c_81 = tag_Some ->
          let splits_99 = Memo.splits (snd x_81) in
          let split0_99 = List.nth splits_99 0 in
          ignore (pop_env w_132);
          push_env w_132 split0_99;
          assert_env_length w_132 4;
          push_env w_132 (Dynarray.get w_132.state.e 3);
          assert_env_length w_132 5;
          drop_n w_132 5 1;
          assert_env_length w_132 4;
          push_env w_132 (Dynarray.get w_132.state.e 3);
          assert_env_length w_132 5;
          push_env w_132 (Dynarray.get w_132.state.e 2);
          assert_env_length w_132 6;
          push_env w_132 (Dynarray.get w_132.state.e 1);
          assert_env_length w_132 7;
          let ctor_arg_55 = pop_env w_132 in
          let ctor_arg_56 = pop_env w_132 in
          let ctor_arg_57 = pop_env w_132 in
          push_env w_132 (Memo.appends [ Memo.from_constructor tag_VClosure; ctor_arg_57; ctor_arg_56; ctor_arg_55 ]);
          assert_env_length w_132 5;
          push_env w_132 (Dynarray.get w_132.state.e 3);
          assert_env_length w_132 6;
          push_env w_132 (Dynarray.get w_132.state.e 4);
          assert_env_length w_132 7;
          let ctor_arg_58 = pop_env w_132 in
          let ctor_arg_59 = pop_env w_132 in
          push_env w_132 (Memo.appends [ Memo.from_constructor tag_EnvEntry; ctor_arg_59; ctor_arg_58 ]);
          assert_env_length w_132 6;
          push_env w_132 (Dynarray.get w_132.state.e 1);
          assert_env_length w_132 7;
          let keep_vals_53 = env_call w_132 [ 0 ] 2 in
          w_132.state.k <- Memo.appends [ Memo.from_constructor tag_cont_54; keep_vals_53; w_132.state.k ];
          w_132.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_132
      | c_81 -> failwith ("unreachable:" ^ string_of_int c_81 ^ "(132)"))
    132

let () = Words.set_constructor_degree 0 1
let () = Words.set_constructor_degree 1 1
let () = Words.set_constructor_degree 2 (-1)
let () = Words.set_constructor_degree 3 1
let () = Words.set_constructor_degree 4 1
let () = Words.set_constructor_degree 5 0
let () = Words.set_constructor_degree 6 1
let () = Words.set_constructor_degree 7 1
let () = Words.set_constructor_degree 8 1
let () = Words.set_constructor_degree 9 1
let () = Words.set_constructor_degree 10 1
let () = Words.set_constructor_degree 11 1
let () = Words.set_constructor_degree 12 1
let () = Words.set_constructor_degree 13 1
let () = Words.set_constructor_degree 14 1
let () = Words.set_constructor_degree 15 0
let () = Words.set_constructor_degree 16 0
let () = Words.set_constructor_degree 17 0
let () = Words.set_constructor_degree 18 1
let () = Words.set_constructor_degree 19 0
let () = Words.set_constructor_degree 20 (-1)
let () = Words.set_constructor_degree 21 0
let () = Words.set_constructor_degree 22 0
let () = Words.set_constructor_degree 23 0
let () = Words.set_constructor_degree 24 1
let () = Words.set_constructor_degree 25 (-1)
let () = Words.set_constructor_degree 26 (-2)
let () = Words.set_constructor_degree 27 (-1)
let () = Words.set_constructor_degree 28 0
let () = Words.set_constructor_degree 29 (-2)
let () = Words.set_constructor_degree 30 (-2)
let () = Words.set_constructor_degree 31 0
let () = Words.set_constructor_degree 32 0
let () = Words.set_constructor_degree 33 0
let () = Words.set_constructor_degree 34 (-1)
let () = Words.set_constructor_degree 35 (-2)
let () = Words.set_constructor_degree 36 (-2)
let () = Words.set_constructor_degree 37 (-3)
let () = Words.set_constructor_degree 38 (-1)
let () = Words.set_constructor_degree 39 (-2)
let () = Words.set_constructor_degree 40 (-2)
let () = Words.set_constructor_degree 41 (-1)
let () = Words.set_constructor_degree 42 (-3)
let () = Words.set_constructor_degree 43 (-1)
let () = Words.set_constructor_degree 44 (-3)
let () = Words.set_constructor_degree 45 (-1)
let () = Words.set_constructor_degree 46 (-2)
let () = Words.set_constructor_degree 47 (-4)
let () = Words.set_constructor_degree 48 0
let () = Words.set_constructor_degree 49 (-1)
let () = Words.set_constructor_degree 50 (-2)
let () = Words.set_constructor_degree 51 (-1)
let () = Words.set_constructor_degree 52 (-1)
let () = Words.set_constructor_degree 53 (-2)
let () = Words.set_constructor_degree 54 (-1)
let () = Words.set_constructor_degree 55 (-2)
let () = Words.set_constructor_degree 56 (-3)
let () = Words.set_constructor_degree 57 0
let () = Words.set_constructor_degree 58 (-2)
let () = Words.set_constructor_degree 59 0
let () = Words.set_constructor_degree 60 0
let () = Words.set_constructor_degree 61 (-2)
let () = Words.set_constructor_degree 62 (-2)
let () = Words.set_constructor_degree 63 (-2)
let () = Words.set_constructor_degree 64 (-1)
let () = Words.set_constructor_degree 65 (-2)
let () = Words.set_constructor_degree 66 (-2)
let () = Words.set_constructor_degree 67 (-2)
let () = Words.set_constructor_degree 68 (-2)
let () = Words.set_constructor_degree 69 (-1)
let () = Words.set_constructor_degree 70 (-1)
let () = Words.set_constructor_degree 71 (-2)
let () = Words.set_constructor_degree 72 (-3)
let () = Words.set_constructor_degree 73 0
let () = Words.set_constructor_degree 74 (-1)
let () = Words.set_constructor_degree 75 (-2)
let () = Words.set_constructor_degree 76 (-3)
let () = Words.set_constructor_degree 77 (-2)
let () = Words.set_constructor_degree 78 (-3)
let () = Words.set_constructor_degree 79 (-3)
let () = Words.set_constructor_degree 80 (-2)
let () = Words.set_constructor_degree 81 (-3)
let () = Words.set_constructor_degree 82 (-1)
let () = Words.set_constructor_degree 83 (-3)
let () = Words.set_constructor_degree 84 (-1)
let () = Words.set_constructor_degree 85 (-1)
