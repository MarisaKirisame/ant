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
let tag_SIf = 10
let tag_SDefvar = 11
let tag_SCons = 12
let tag_SCond = 13
let tag_SAtom = 14
let tag_SPair = 15
let tag_SSymbol = 16
let tag_SCar = 17
let tag_SCdr = 18
let tag_SNull = 19
let tag_STrue = 20
let tag_SFalse = 21
let tag_SError = 22
let tag_SVar = 23
let tag_AVar = 24
let tag_ANumber = 25
let tag_ASymbol = 26
let tag_ANIL = 27
let tag_EAtom = 28
let tag_ECons = 29
let tag_VNumber = 30
let tag_VSymbol = 31
let tag_VQuote = 32
let tag_VNIL = 33
let tag_VCons = 34
let tag_VClosure = 35
let tag_EnvEntry = 36
let tag_MkEnv = 37
let tag_cont_1 = 38
let tag_cont_2 = 39
let tag_cont_3 = 40
let tag_cont_4 = 41
let tag_cont_5 = 42
let tag_cont_6 = 43
let tag_cont_7 = 44
let tag_cont_8 = 45
let tag_cont_9 = 46
let tag_cont_10 = 47
let tag_cont_11 = 48
let tag_cont_12 = 49
let tag_cont_13 = 50
let tag_cont_14 = 51
let tag_cont_15 = 52
let tag_cont_16 = 53
let tag_cont_17 = 54
let tag_cont_18 = 55
let tag_cont_19 = 56
let tag_cont_20 = 57
let tag_cont_21 = 58
let tag_cont_22 = 59
let tag_cont_23 = 60
let tag_cont_24 = 61
let tag_cont_25 = 62
let tag_cont_26 = 63
let tag_cont_27 = 64
let tag_cont_28 = 65
let tag_cont_29 = 66
let tag_cont_30 = 67
let tag_cont_31 = 68
let tag_cont_32 = 69
let tag_cont_33 = 70
let tag_cont_34 = 71
let tag_cont_35 = 72
let tag_cont_36 = 73
let tag_cont_37 = 74
let tag_cont_38 = 75
let tag_cont_39 = 76
let tag_cont_40 = 77
let tag_cont_41 = 78
let tag_cont_42 = 79
let tag_cont_43 = 80
let tag_cont_44 = 81
let tag_cont_45 = 82
let tag_cont_46 = 83
let tag_cont_47 = 84
let tag_cont_48 = 85
let tag_cont_49 = 86
let tag_cont_50 = 87
let tag_cont_51 = 88
let tag_cont_52 = 89
let tag_cont_53 = 90
let tag_cont_54 = 91
let tag_cont_55 = 92
let tag_cont_56 = 93
let tag_cont_57 = 94
let tag_cont_58 = 95
let tag_cont_59 = 96
let tag_cont_60 = 97
let tag_cont_61 = 98
let tag_cont_62 = 99
let tag_cont_63 = 100
let tag_cont_64 = 101
let tag_cont_65 = 102
let tag_cont_66 = 103
let tag_cont_67 = 104
let tag_cont_68 = 105
let tag_cont_69 = 106
let tag_cont_70 = 107
let tag_cont_71 = 108
let tag_cont_72 = 109
let tag_cont_73 = 110
let tag_cont_74 = 111
let tag_cont_75 = 112
let tag_cont_76 = 113
let tag_cont_77 = 114
let tag_cont_78 = 115

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

type symbol =
  | SLambda
  | SDefine
  | SQuote
  | SEq
  | SIf
  | SDefvar
  | SCons
  | SCond
  | SAtom
  | SPair
  | SSymbol
  | SCar
  | SCdr
  | SNull
  | STrue
  | SFalse
  | SError
  | SVar

let rec from_ocaml_symbol x =
  match x with
  | SLambda -> Memo.appends [ Memo.from_constructor tag_SLambda ]
  | SDefine -> Memo.appends [ Memo.from_constructor tag_SDefine ]
  | SQuote -> Memo.appends [ Memo.from_constructor tag_SQuote ]
  | SEq -> Memo.appends [ Memo.from_constructor tag_SEq ]
  | SIf -> Memo.appends [ Memo.from_constructor tag_SIf ]
  | SDefvar -> Memo.appends [ Memo.from_constructor tag_SDefvar ]
  | SCons -> Memo.appends [ Memo.from_constructor tag_SCons ]
  | SCond -> Memo.appends [ Memo.from_constructor tag_SCond ]
  | SAtom -> Memo.appends [ Memo.from_constructor tag_SAtom ]
  | SPair -> Memo.appends [ Memo.from_constructor tag_SPair ]
  | SSymbol -> Memo.appends [ Memo.from_constructor tag_SSymbol ]
  | SCar -> Memo.appends [ Memo.from_constructor tag_SCar ]
  | SCdr -> Memo.appends [ Memo.from_constructor tag_SCdr ]
  | SNull -> Memo.appends [ Memo.from_constructor tag_SNull ]
  | STrue -> Memo.appends [ Memo.from_constructor tag_STrue ]
  | SFalse -> Memo.appends [ Memo.from_constructor tag_SFalse ]
  | SError -> Memo.appends [ Memo.from_constructor tag_SError ]
  | SVar -> Memo.appends [ Memo.from_constructor tag_SVar ]

let rec to_ocaml_symbol x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_SLambda -> SLambda
  | c when c = tag_SDefine -> SDefine
  | c when c = tag_SQuote -> SQuote
  | c when c = tag_SEq -> SEq
  | c when c = tag_SIf -> SIf
  | c when c = tag_SDefvar -> SDefvar
  | c when c = tag_SCons -> SCons
  | c when c = tag_SCond -> SCond
  | c when c = tag_SAtom -> SAtom
  | c when c = tag_SPair -> SPair
  | c when c = tag_SSymbol -> SSymbol
  | c when c = tag_SCar -> SCar
  | c when c = tag_SCdr -> SCdr
  | c when c = tag_SNull -> SNull
  | c when c = tag_STrue -> STrue
  | c when c = tag_SFalse -> SFalse
  | c when c = tag_SError -> SError
  | c when c = tag_SVar -> SVar
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
  | VClosure of int * expr

and env_entry = EnvEntry of int * value
and env = MkEnv of env_entry list

let rec from_ocaml_value x =
  match x with
  | VNumber x0 -> Memo.appends [ Memo.from_constructor tag_VNumber; Memo.from_int x0 ]
  | VSymbol x0 -> Memo.appends [ Memo.from_constructor tag_VSymbol; from_ocaml_symbol x0 ]
  | VQuote x0 -> Memo.appends [ Memo.from_constructor tag_VQuote; from_ocaml_expr x0 ]
  | VNIL -> Memo.appends [ Memo.from_constructor tag_VNIL ]
  | VCons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_VCons; from_ocaml_value x0; from_ocaml_value x1 ]
  | VClosure (x0, x1) -> Memo.appends [ Memo.from_constructor tag_VClosure; Memo.from_int x0; from_ocaml_expr x1 ]

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
      let x0, x1 = Memo.splits_2 t in
      VClosure (Word.get_value (Memo.to_word x0), to_ocaml_expr x1)
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

let rec expr_is_symbol memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 10)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_is_var memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 13)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_is_num memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 16)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_is_nil memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 19)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_is_number_or_quote_number memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 22)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_false memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 24)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_true memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 25)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec unwrap_env memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 26)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cons_env memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 28)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec caddr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 30)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cadddr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 34)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cadr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 39)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec caar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 42)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cdar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 45)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cddar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 48)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cadar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 52)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec caddar_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 56)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec car_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 61)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cdr_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 63)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec symbol_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 65)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec atom_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 85)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec expr_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 93)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec value_eq memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 96)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec car memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 103)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec cdr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 106)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_atom_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 109)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_pair_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 111)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_symbol_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 114)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_eq_ memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 116)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec lookup memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 117)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec pairlis memo (x0 : Value.seq) (x1 : Value.seq) (x2 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 118)) (Dynarray.of_list [ x0; x1; x2 ]) (Memo.from_constructor tag_cont_done) memo

let rec destruct_names memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 121)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec is_null_ memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 124)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec evlis memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 126)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec evcon memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 127)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 128)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_133 ->
      assert_env_length w_133 1;
      let hd_0, tl_0 = resolve w_133 K in
      match Word.get_value hd_0 with
      | c_107 when c_107 = tag_cont_done -> exec_done w_133
      | c_107 when c_107 = tag_cont_1 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 135);
          stepped w_133
      | c_107 when c_107 = tag_cont_2 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 137);
          stepped w_133
      | c_107 when c_107 = tag_cont_3 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          let ctor_arg_15 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_VQuote; ctor_arg_15 ]);
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          return_n w_133 1 (pc_to_exp (int_to_pc 0))
      | c_107 when c_107 = tag_cont_4 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          let ctor_arg_16 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_VQuote; ctor_arg_16 ]);
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          return_n w_133 1 (pc_to_exp (int_to_pc 0))
      | c_107 when c_107 = tag_cont_5 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 138);
          stepped w_133
      | c_107 when c_107 = tag_cont_6 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 139);
          stepped w_133
      | c_107 when c_107 = tag_cont_7 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 140);
          stepped w_133
      | c_107 when c_107 = tag_cont_8 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          ignore (env_call w_133 [] 3);
          w_133.state.c <- pc_to_exp (int_to_pc 118);
          stepped w_133
      | c_107 when c_107 = tag_cont_9 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 141);
          stepped w_133
      | c_107 when c_107 = tag_cont_10 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 142);
          stepped w_133
      | c_107 when c_107 = tag_cont_11 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 5;
          let keep_vals_15 = env_call w_133 [ 2 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_vals_15; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 126);
          stepped w_133
      | c_107 when c_107 = tag_cont_12 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 5;
          let keep_vals_16 = env_call w_133 [ 0; 1; 2 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_vals_16; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_13 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          let keep_vals_17 = env_call w_133 [ 0; 1 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_vals_17; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_133
      | c_107 when c_107 = tag_cont_14 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          w_133.state.c <- pc_to_exp (int_to_pc 144);
          stepped w_133
      | c_107 when c_107 = tag_cont_15 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          let ctor_arg_18 = pop_env w_133 in
          let ctor_arg_19 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_19; ctor_arg_18 ]);
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          return_n w_133 1 (pc_to_exp (int_to_pc 0))
      | c_107 when c_107 = tag_cont_16 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          let ctor_arg_20 = pop_env w_133 in
          let ctor_arg_21 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_21; ctor_arg_20 ]);
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          return_n w_133 1 (pc_to_exp (int_to_pc 0))
      | c_107 when c_107 = tag_cont_17 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Memo.from_constructor tag_Unit);
          assert_env_length w_133 5;
          let keep_vals_18 = env_call w_133 [ 0; 1; 2; 3 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_vals_18; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_133
      | c_107 when c_107 = tag_cont_18 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 146);
          stepped w_133
      | c_107 when c_107 = tag_cont_19 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 4 tl_0;
          assert_env_length w_133 5;
          let keep_vals_32 = env_call w_133 [ 0; 1; 2 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_33; keep_vals_32; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 96);
          stepped w_133
      | c_107 when c_107 = tag_cont_20 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          let ctor_arg_22 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_VQuote; ctor_arg_22 ]);
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          return_n w_133 1 (pc_to_exp (int_to_pc 0))
      | c_107 when c_107 = tag_cont_21 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_33 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_34; keep_vals_33; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_22 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_34 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_35; keep_vals_34; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_23 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_35 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_36; keep_vals_35; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_24 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 4;
          let keep_vals_36 = env_call w_133 [ 0; 1 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_37; keep_vals_36; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_25 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_37 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_38; keep_vals_37; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_26 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_38 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_39; keep_vals_38; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_27 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_39 = env_call w_133 [ 0; 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_40; keep_vals_39; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 30);
          stepped w_133
      | c_107 when c_107 = tag_cont_28 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 4;
          let keep_vals_40 = env_call w_133 [ 0; 1 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_41; keep_vals_40; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_29 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 127);
          stepped w_133
      | c_107 when c_107 = tag_cont_30 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_41 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_42; keep_vals_41; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_31 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_42 = env_call w_133 [] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_43; keep_vals_42; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_32 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 147);
          stepped w_133
      | c_107 when c_107 = tag_cont_33 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 148);
          stepped w_133
      | c_107 when c_107 = tag_cont_34 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          ignore (env_call w_133 [] 1);
          w_133.state.c <- pc_to_exp (int_to_pc 109);
          stepped w_133
      | c_107 when c_107 = tag_cont_35 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          ignore (env_call w_133 [] 1);
          w_133.state.c <- pc_to_exp (int_to_pc 111);
          stepped w_133
      | c_107 when c_107 = tag_cont_36 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          ignore (env_call w_133 [] 1);
          w_133.state.c <- pc_to_exp (int_to_pc 114);
          stepped w_133
      | c_107 when c_107 = tag_cont_37 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_46 = env_call w_133 [ 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_47; keep_vals_46; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 30);
          stepped w_133
      | c_107 when c_107 = tag_cont_38 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          ignore (env_call w_133 [] 1);
          w_133.state.c <- pc_to_exp (int_to_pc 103);
          stepped w_133
      | c_107 when c_107 = tag_cont_39 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          ignore (env_call w_133 [] 1);
          w_133.state.c <- pc_to_exp (int_to_pc 106);
          stepped w_133
      | c_107 when c_107 = tag_cont_40 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 5;
          let keep_vals_47 = env_call w_133 [ 1; 2; 3 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_48; keep_vals_47; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 34);
          stepped w_133
      | c_107 when c_107 = tag_cont_41 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_48 = env_call w_133 [ 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_49; keep_vals_48; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 30);
          stepped w_133
      | c_107 when c_107 = tag_cont_42 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          ignore (env_call w_133 [] 1);
          w_133.state.c <- pc_to_exp (int_to_pc 124);
          stepped w_133
      | c_107 when c_107 = tag_cont_43 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          assert_env_length w_133 1;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 2;
          let keep_vals_49 = env_call w_133 [] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_50; keep_vals_49; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_133
      | c_107 when c_107 = tag_cont_44 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          let keep_vals_50 = env_call w_133 [ 0; 1 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_51; keep_vals_50; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_133
      | c_107 when c_107 = tag_cont_45 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          let keep_vals_51 = env_call w_133 [ 0; 1 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_52; keep_vals_51; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 13);
          stepped w_133
      | c_107 when c_107 = tag_cont_46 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_47 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_52 = env_call w_133 [ 1 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_53; keep_vals_52; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_48 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 5;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 6;
          let keep_vals_53 = env_call w_133 [ 0; 2; 3 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_54; keep_vals_53; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_49 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_54 = env_call w_133 [ 1 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_55; keep_vals_54; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_50 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 150);
          stepped w_133
      | c_107 when c_107 = tag_cont_51 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 152);
          stepped w_133
      | c_107 when c_107 = tag_cont_52 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 153);
          stepped w_133
      | c_107 when c_107 = tag_cont_53 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 116);
          stepped w_133
      | c_107 when c_107 = tag_cont_54 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 3);
          assert_env_length w_133 5;
          push_env w_133 (Memo.from_constructor tag_Unit);
          assert_env_length w_133 6;
          let keep_vals_61 = env_call w_133 [ 0; 1; 2; 4 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_62; keep_vals_61; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_133
      | c_107 when c_107 = tag_cont_55 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          let ctor_arg_25 = pop_env w_133 in
          let ctor_arg_26 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_VCons; ctor_arg_26; ctor_arg_25 ]);
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          drop_n w_133 1 0;
          assert_env_length w_133 1;
          return_n w_133 1 (pc_to_exp (int_to_pc 0))
      | c_107 when c_107 = tag_cont_56 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          let keep_vals_62 = env_call w_133 [ 0; 1 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_63; keep_vals_62; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 121);
          stepped w_133
      | c_107 when c_107 = tag_cont_57 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          let ctor_arg_27 = pop_env w_133 in
          let ctor_arg_28 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_28; ctor_arg_27 ]);
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_63 = env_call w_133 [ 0; 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_64; keep_vals_63; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 52);
          stepped w_133
      | c_107 when c_107 = tag_cont_58 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 4;
          let keep_vals_64 = env_call w_133 [ 0; 1 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_65; keep_vals_64; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_59 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 155);
          stepped w_133
      | c_107 when c_107 = tag_cont_60 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 156);
          stepped w_133
      | c_107 when c_107 = tag_cont_61 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 0 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 158);
          stepped w_133
      | c_107 when c_107 = tag_cont_62 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 4 tl_0;
          assert_env_length w_133 5;
          let keep_vals_66 = env_call w_133 [ 0; 1; 2 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_67; keep_vals_66; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 96);
          stepped w_133
      | c_107 when c_107 = tag_cont_63 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_67 = env_call w_133 [ 0; 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_68; keep_vals_67; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 56);
          stepped w_133
      | c_107 when c_107 = tag_cont_64 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          let keep_vals_68 = env_call w_133 [ 0; 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_69; keep_vals_68; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_133
      | c_107 when c_107 = tag_cont_65 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          let keep_vals_69 = env_call w_133 [ 0; 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_70; keep_vals_69; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 52);
          stepped w_133
      | c_107 when c_107 = tag_cont_66 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 2 tl_0;
          assert_env_length w_133 3;
          let ctor_arg_29 = pop_env w_133 in
          let ctor_arg_30 = pop_env w_133 in
          push_env w_133 (Memo.appends [ Memo.from_constructor tag_ECons; ctor_arg_30; ctor_arg_29 ]);
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_67 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 159);
          stepped w_133
      | c_107 when c_107 = tag_cont_68 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 5;
          let keep_vals_70 = env_call w_133 [ 1; 2; 3 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_71; keep_vals_70; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 63);
          stepped w_133
      | c_107 when c_107 = tag_cont_69 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 160);
          stepped w_133
      | c_107 when c_107 = tag_cont_70 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          let keep_vals_72 = env_call w_133 [ 0; 1; 2 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_73; keep_vals_72; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_133
      | c_107 when c_107 = tag_cont_71 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 5;
          let keep_vals_73 = env_call w_133 [ 0; 1; 2 ] 2 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_74; keep_vals_73; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 126);
          stepped w_133
      | c_107 when c_107 = tag_cont_72 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_74 = env_call w_133 [ 1 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_75; keep_vals_74; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_133
      | c_107 when c_107 = tag_cont_73 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          w_133.state.c <- pc_to_exp (int_to_pc 161);
          stepped w_133
      | c_107 when c_107 = tag_cont_74 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 3 tl_0;
          assert_env_length w_133 4;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 5;
          push_env w_133 (Dynarray.get w_133.state.e 3);
          assert_env_length w_133 6;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 7;
          let keep_vals_76 = env_call w_133 [ 2 ] 3 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_77; keep_vals_76; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 118);
          stepped w_133
      | c_107 when c_107 = tag_cont_75 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_76 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          let keep_vals_77 = env_call w_133 [ 1 ] 1 in
          w_133.state.k <- Memo.appends [ Memo.from_constructor tag_cont_78; keep_vals_77; w_133.state.k ];
          w_133.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_133
      | c_107 when c_107 = tag_cont_77 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 4;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
      | c_107 when c_107 = tag_cont_78 ->
          w_133.state.k <- get_next_cont tl_0;
          restore_env w_133 1 tl_0;
          assert_env_length w_133 2;
          push_env w_133 (Dynarray.get w_133.state.e 1);
          assert_env_length w_133 3;
          push_env w_133 (Dynarray.get w_133.state.e 0);
          assert_env_length w_133 4;
          ignore (env_call w_133 [] 2);
          w_133.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_133
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
      w_9.state.c <- pc_to_exp (int_to_pc 12);
      stepped w_9)
    10

let () =
  add_exp
    (fun w_11 ->
      assert_env_length w_11 3;
      let last_4 = Source.E 2 in
      let x_4 = resolve w_11 last_4 in
      match Word.get_value (fst x_4) with
      | c_4 when c_4 = tag_ASymbol ->
          let splits_4 = Memo.splits (snd x_4) in
          let split0_4 = List.nth splits_4 0 in
          ignore (pop_env w_11);
          push_env w_11 split0_4;
          assert_env_length w_11 3;
          push_env w_11 (Dynarray.get w_11.state.e 2);
          assert_env_length w_11 4;
          let ctor_arg_4 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_4 ]);
          assert_env_length w_11 4;
          drop_n w_11 4 1;
          assert_env_length w_11 3;
          drop_n w_11 3 1;
          assert_env_length w_11 2;
          return_n w_11 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_11);
          assert_env_length w_11 2;
          push_env w_11 (Memo.from_constructor tag_None);
          assert_env_length w_11 3;
          drop_n w_11 3 1;
          assert_env_length w_11 2;
          return_n w_11 2 (pc_to_exp (int_to_pc 0))
      | c_4 -> failwith ("unreachable:" ^ string_of_int c_4 ^ "(11)"))
    11

let () =
  add_exp
    (fun w_10 ->
      assert_env_length w_10 2;
      let last_3 = Source.E 1 in
      let x_3 = resolve w_10 last_3 in
      match Word.get_value (fst x_3) with
      | c_3 when c_3 = tag_EAtom ->
          let splits_3 = Memo.splits (snd x_3) in
          let split0_3 = List.nth splits_3 0 in
          ignore (pop_env w_10);
          push_env w_10 split0_3;
          assert_env_length w_10 2;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          w_10.state.c <- pc_to_exp (int_to_pc 11);
          stepped w_10
      | _ ->
          ignore (pop_env w_10);
          assert_env_length w_10 1;
          push_env w_10 (Memo.from_constructor tag_None);
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | c_3 -> failwith ("unreachable:" ^ string_of_int c_3 ^ "(12)"))
    12

let () =
  add_exp
    (fun w_12 ->
      assert_env_length w_12 1;
      push_env w_12 (Dynarray.get w_12.state.e 0);
      w_12.state.c <- pc_to_exp (int_to_pc 15);
      stepped w_12)
    13

let () =
  add_exp
    (fun w_14 ->
      assert_env_length w_14 3;
      let last_6 = Source.E 2 in
      let x_6 = resolve w_14 last_6 in
      match Word.get_value (fst x_6) with
      | c_6 when c_6 = tag_AVar ->
          let splits_6 = Memo.splits (snd x_6) in
          let split0_6 = List.nth splits_6 0 in
          ignore (pop_env w_14);
          push_env w_14 split0_6;
          assert_env_length w_14 3;
          push_env w_14 (Dynarray.get w_14.state.e 2);
          assert_env_length w_14 4;
          let ctor_arg_5 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_5 ]);
          assert_env_length w_14 4;
          drop_n w_14 4 1;
          assert_env_length w_14 3;
          drop_n w_14 3 1;
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_14);
          assert_env_length w_14 2;
          push_env w_14 (Memo.from_constructor tag_None);
          assert_env_length w_14 3;
          drop_n w_14 3 1;
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | c_6 -> failwith ("unreachable:" ^ string_of_int c_6 ^ "(14)"))
    14

let () =
  add_exp
    (fun w_13 ->
      assert_env_length w_13 2;
      let last_5 = Source.E 1 in
      let x_5 = resolve w_13 last_5 in
      match Word.get_value (fst x_5) with
      | c_5 when c_5 = tag_EAtom ->
          let splits_5 = Memo.splits (snd x_5) in
          let split0_5 = List.nth splits_5 0 in
          ignore (pop_env w_13);
          push_env w_13 split0_5;
          assert_env_length w_13 2;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          w_13.state.c <- pc_to_exp (int_to_pc 14);
          stepped w_13
      | _ ->
          ignore (pop_env w_13);
          assert_env_length w_13 1;
          push_env w_13 (Memo.from_constructor tag_None);
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | c_5 -> failwith ("unreachable:" ^ string_of_int c_5 ^ "(15)"))
    15

let () =
  add_exp
    (fun w_15 ->
      assert_env_length w_15 1;
      push_env w_15 (Dynarray.get w_15.state.e 0);
      w_15.state.c <- pc_to_exp (int_to_pc 18);
      stepped w_15)
    16

let () =
  add_exp
    (fun w_17 ->
      assert_env_length w_17 3;
      let last_8 = Source.E 2 in
      let x_8 = resolve w_17 last_8 in
      match Word.get_value (fst x_8) with
      | c_8 when c_8 = tag_ANumber ->
          let splits_9 = Memo.splits (snd x_8) in
          let split0_9 = List.nth splits_9 0 in
          ignore (pop_env w_17);
          push_env w_17 split0_9;
          assert_env_length w_17 3;
          push_env w_17 (Dynarray.get w_17.state.e 2);
          assert_env_length w_17 4;
          let ctor_arg_6 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_6 ]);
          assert_env_length w_17 4;
          drop_n w_17 4 1;
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          return_n w_17 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_17);
          assert_env_length w_17 2;
          push_env w_17 (Memo.from_constructor tag_None);
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          return_n w_17 2 (pc_to_exp (int_to_pc 0))
      | c_8 -> failwith ("unreachable:" ^ string_of_int c_8 ^ "(17)"))
    17

let () =
  add_exp
    (fun w_16 ->
      assert_env_length w_16 2;
      let last_7 = Source.E 1 in
      let x_7 = resolve w_16 last_7 in
      match Word.get_value (fst x_7) with
      | c_7 when c_7 = tag_ECons ->
          let splits_7 = Memo.splits (snd x_7) in
          let split0_7 = List.nth splits_7 0 in
          let split1_3 = List.nth splits_7 1 in
          ignore (pop_env w_16);
          push_env w_16 split0_7;
          push_env w_16 split1_3;
          assert_env_length w_16 3;
          push_env w_16 (Memo.from_constructor tag_None);
          assert_env_length w_16 4;
          drop_n w_16 4 2;
          assert_env_length w_16 2;
          return_n w_16 2 (pc_to_exp (int_to_pc 0))
      | c_7 when c_7 = tag_EAtom ->
          let splits_8 = Memo.splits (snd x_7) in
          let split0_8 = List.nth splits_8 0 in
          ignore (pop_env w_16);
          push_env w_16 split0_8;
          assert_env_length w_16 2;
          push_env w_16 (Dynarray.get w_16.state.e 1);
          w_16.state.c <- pc_to_exp (int_to_pc 17);
          stepped w_16
      | c_7 -> failwith ("unreachable:" ^ string_of_int c_7 ^ "(18)"))
    18

let () =
  add_exp
    (fun w_18 ->
      assert_env_length w_18 1;
      push_env w_18 (Dynarray.get w_18.state.e 0);
      w_18.state.c <- pc_to_exp (int_to_pc 21);
      stepped w_18)
    19

let () =
  add_exp
    (fun w_20 ->
      assert_env_length w_20 3;
      let last_10 = Source.E 2 in
      let x_10 = resolve w_20 last_10 in
      match Word.get_value (fst x_10) with
      | c_10 when c_10 = tag_ANIL ->
          ignore (pop_env w_20);
          assert_env_length w_20 2;
          push_env w_20 (Memo.from_int 1);
          assert_env_length w_20 3;
          drop_n w_20 3 1;
          assert_env_length w_20 2;
          return_n w_20 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_20);
          assert_env_length w_20 2;
          push_env w_20 (Memo.from_int 0);
          assert_env_length w_20 3;
          drop_n w_20 3 1;
          assert_env_length w_20 2;
          return_n w_20 2 (pc_to_exp (int_to_pc 0))
      | c_10 -> failwith ("unreachable:" ^ string_of_int c_10 ^ "(20)"))
    20

let () =
  add_exp
    (fun w_19 ->
      assert_env_length w_19 2;
      let last_9 = Source.E 1 in
      let x_9 = resolve w_19 last_9 in
      match Word.get_value (fst x_9) with
      | c_9 when c_9 = tag_ECons ->
          let splits_10 = Memo.splits (snd x_9) in
          let split0_10 = List.nth splits_10 0 in
          let split1_4 = List.nth splits_10 1 in
          ignore (pop_env w_19);
          push_env w_19 split0_10;
          push_env w_19 split1_4;
          assert_env_length w_19 3;
          push_env w_19 (Memo.from_int 0);
          assert_env_length w_19 4;
          drop_n w_19 4 2;
          assert_env_length w_19 2;
          return_n w_19 2 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_EAtom ->
          let splits_11 = Memo.splits (snd x_9) in
          let split0_11 = List.nth splits_11 0 in
          ignore (pop_env w_19);
          push_env w_19 split0_11;
          assert_env_length w_19 2;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          w_19.state.c <- pc_to_exp (int_to_pc 20);
          stepped w_19
      | c_9 -> failwith ("unreachable:" ^ string_of_int c_9 ^ "(21)"))
    21

let () =
  add_exp
    (fun w_21 ->
      assert_env_length w_21 1;
      push_env w_21 (Dynarray.get w_21.state.e 0);
      w_21.state.c <- pc_to_exp (int_to_pc 23);
      stepped w_21)
    22

let () =
  add_exp
    (fun w_22 ->
      assert_env_length w_22 2;
      let last_11 = Source.E 1 in
      let x_11 = resolve w_22 last_11 in
      match Word.get_value (fst x_11) with
      | c_11 when c_11 = tag_VNumber ->
          let splits_12 = Memo.splits (snd x_11) in
          let split0_12 = List.nth splits_12 0 in
          ignore (pop_env w_22);
          push_env w_22 split0_12;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 3;
          let ctor_arg_7 = pop_env w_22 in
          push_env w_22 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_7 ]);
          assert_env_length w_22 3;
          drop_n w_22 3 1;
          assert_env_length w_22 2;
          return_n w_22 2 (pc_to_exp (int_to_pc 0))
      | c_11 when c_11 = tag_VQuote ->
          let splits_13 = Memo.splits (snd x_11) in
          let split0_13 = List.nth splits_13 0 in
          ignore (pop_env w_22);
          push_env w_22 split0_13;
          assert_env_length w_22 2;
          push_env w_22 (Dynarray.get w_22.state.e 1);
          assert_env_length w_22 3;
          ignore (env_call w_22 [] 1);
          w_22.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_22
      | _ ->
          ignore (pop_env w_22);
          assert_env_length w_22 1;
          push_env w_22 (Memo.from_constructor tag_None);
          assert_env_length w_22 2;
          return_n w_22 2 (pc_to_exp (int_to_pc 0))
      | c_11 -> failwith ("unreachable:" ^ string_of_int c_11 ^ "(23)"))
    23

let () =
  add_exp
    (fun w_23 ->
      assert_env_length w_23 1;
      push_env w_23 (Memo.from_constructor tag_VNIL);
      assert_env_length w_23 2;
      return_n w_23 2 (pc_to_exp (int_to_pc 0)))
    24

let () =
  add_exp
    (fun w_24 ->
      assert_env_length w_24 1;
      push_env w_24 (Memo.from_int 0);
      assert_env_length w_24 2;
      let ctor_arg_8 = pop_env w_24 in
      push_env w_24 (Memo.appends [ Memo.from_constructor tag_VNumber; ctor_arg_8 ]);
      assert_env_length w_24 2;
      return_n w_24 2 (pc_to_exp (int_to_pc 0)))
    25

let () =
  add_exp
    (fun w_25 ->
      assert_env_length w_25 1;
      push_env w_25 (Dynarray.get w_25.state.e 0);
      w_25.state.c <- pc_to_exp (int_to_pc 27);
      stepped w_25)
    26

let () =
  add_exp
    (fun w_26 ->
      assert_env_length w_26 2;
      let last_12 = Source.E 1 in
      let x_12 = resolve w_26 last_12 in
      match Word.get_value (fst x_12) with
      | c_12 when c_12 = tag_MkEnv ->
          let splits_14 = Memo.splits (snd x_12) in
          let split0_14 = List.nth splits_14 0 in
          ignore (pop_env w_26);
          push_env w_26 split0_14;
          assert_env_length w_26 2;
          push_env w_26 (Dynarray.get w_26.state.e 1);
          assert_env_length w_26 3;
          drop_n w_26 3 1;
          assert_env_length w_26 2;
          return_n w_26 2 (pc_to_exp (int_to_pc 0))
      | c_12 -> failwith ("unreachable:" ^ string_of_int c_12 ^ "(27)"))
    27

let () =
  add_exp
    (fun w_27 ->
      assert_env_length w_27 2;
      push_env w_27 (Dynarray.get w_27.state.e 1);
      w_27.state.c <- pc_to_exp (int_to_pc 29);
      stepped w_27)
    28

let () =
  add_exp
    (fun w_28 ->
      assert_env_length w_28 3;
      let last_13 = Source.E 2 in
      let x_13 = resolve w_28 last_13 in
      match Word.get_value (fst x_13) with
      | c_13 when c_13 = tag_MkEnv ->
          let splits_15 = Memo.splits (snd x_13) in
          let split0_15 = List.nth splits_15 0 in
          ignore (pop_env w_28);
          push_env w_28 split0_15;
          assert_env_length w_28 3;
          push_env w_28 (Dynarray.get w_28.state.e 0);
          assert_env_length w_28 4;
          push_env w_28 (Dynarray.get w_28.state.e 2);
          assert_env_length w_28 5;
          let ctor_arg_9 = pop_env w_28 in
          let ctor_arg_10 = pop_env w_28 in
          push_env w_28 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_10; ctor_arg_9 ]);
          assert_env_length w_28 4;
          let ctor_arg_11 = pop_env w_28 in
          push_env w_28 (Memo.appends [ Memo.from_constructor tag_MkEnv; ctor_arg_11 ]);
          assert_env_length w_28 4;
          drop_n w_28 4 1;
          assert_env_length w_28 3;
          return_n w_28 3 (pc_to_exp (int_to_pc 0))
      | c_13 -> failwith ("unreachable:" ^ string_of_int c_13 ^ "(29)"))
    29

let () =
  add_exp
    (fun w_29 ->
      assert_env_length w_29 1;
      push_env w_29 (Dynarray.get w_29.state.e 0);
      w_29.state.c <- pc_to_exp (int_to_pc 33);
      stepped w_29)
    30

let () =
  add_exp
    (fun w_32 ->
      assert_env_length w_32 6;
      let last_16 = Source.E 5 in
      let x_16 = resolve w_32 last_16 in
      match Word.get_value (fst x_16) with
      | c_16 when c_16 = tag_ECons ->
          let splits_18 = Memo.splits (snd x_16) in
          let split0_18 = List.nth splits_18 0 in
          let split1_7 = List.nth splits_18 1 in
          ignore (pop_env w_32);
          push_env w_32 split0_18;
          push_env w_32 split1_7;
          assert_env_length w_32 7;
          push_env w_32 (Dynarray.get w_32.state.e 5);
          assert_env_length w_32 8;
          drop_n w_32 8 2;
          assert_env_length w_32 6;
          drop_n w_32 6 2;
          assert_env_length w_32 4;
          drop_n w_32 4 2;
          assert_env_length w_32 2;
          return_n w_32 2 (pc_to_exp (int_to_pc 0))
      | c_16 -> failwith ("unreachable:" ^ string_of_int c_16 ^ "(31)"))
    31

let () =
  add_exp
    (fun w_31 ->
      assert_env_length w_31 4;
      let last_15 = Source.E 3 in
      let x_15 = resolve w_31 last_15 in
      match Word.get_value (fst x_15) with
      | c_15 when c_15 = tag_ECons ->
          let splits_17 = Memo.splits (snd x_15) in
          let split0_17 = List.nth splits_17 0 in
          let split1_6 = List.nth splits_17 1 in
          ignore (pop_env w_31);
          push_env w_31 split0_17;
          push_env w_31 split1_6;
          assert_env_length w_31 5;
          push_env w_31 (Dynarray.get w_31.state.e 4);
          w_31.state.c <- pc_to_exp (int_to_pc 31);
          stepped w_31
      | c_15 -> failwith ("unreachable:" ^ string_of_int c_15 ^ "(32)"))
    32

let () =
  add_exp
    (fun w_30 ->
      assert_env_length w_30 2;
      let last_14 = Source.E 1 in
      let x_14 = resolve w_30 last_14 in
      match Word.get_value (fst x_14) with
      | c_14 when c_14 = tag_ECons ->
          let splits_16 = Memo.splits (snd x_14) in
          let split0_16 = List.nth splits_16 0 in
          let split1_5 = List.nth splits_16 1 in
          ignore (pop_env w_30);
          push_env w_30 split0_16;
          push_env w_30 split1_5;
          assert_env_length w_30 3;
          push_env w_30 (Dynarray.get w_30.state.e 2);
          w_30.state.c <- pc_to_exp (int_to_pc 32);
          stepped w_30
      | c_14 -> failwith ("unreachable:" ^ string_of_int c_14 ^ "(33)"))
    33

let () =
  add_exp
    (fun w_33 ->
      assert_env_length w_33 1;
      push_env w_33 (Dynarray.get w_33.state.e 0);
      w_33.state.c <- pc_to_exp (int_to_pc 38);
      stepped w_33)
    34

let () =
  add_exp
    (fun w_37 ->
      assert_env_length w_37 8;
      let last_20 = Source.E 7 in
      let x_20 = resolve w_37 last_20 in
      match Word.get_value (fst x_20) with
      | c_20 when c_20 = tag_ECons ->
          let splits_22 = Memo.splits (snd x_20) in
          let split0_22 = List.nth splits_22 0 in
          let split1_11 = List.nth splits_22 1 in
          ignore (pop_env w_37);
          push_env w_37 split0_22;
          push_env w_37 split1_11;
          assert_env_length w_37 9;
          push_env w_37 (Dynarray.get w_37.state.e 7);
          assert_env_length w_37 10;
          drop_n w_37 10 2;
          assert_env_length w_37 8;
          drop_n w_37 8 2;
          assert_env_length w_37 6;
          drop_n w_37 6 2;
          assert_env_length w_37 4;
          drop_n w_37 4 2;
          assert_env_length w_37 2;
          return_n w_37 2 (pc_to_exp (int_to_pc 0))
      | c_20 -> failwith ("unreachable:" ^ string_of_int c_20 ^ "(35)"))
    35

let () =
  add_exp
    (fun w_36 ->
      assert_env_length w_36 6;
      let last_19 = Source.E 5 in
      let x_19 = resolve w_36 last_19 in
      match Word.get_value (fst x_19) with
      | c_19 when c_19 = tag_ECons ->
          let splits_21 = Memo.splits (snd x_19) in
          let split0_21 = List.nth splits_21 0 in
          let split1_10 = List.nth splits_21 1 in
          ignore (pop_env w_36);
          push_env w_36 split0_21;
          push_env w_36 split1_10;
          assert_env_length w_36 7;
          push_env w_36 (Dynarray.get w_36.state.e 6);
          w_36.state.c <- pc_to_exp (int_to_pc 35);
          stepped w_36
      | c_19 -> failwith ("unreachable:" ^ string_of_int c_19 ^ "(36)"))
    36

let () =
  add_exp
    (fun w_35 ->
      assert_env_length w_35 4;
      let last_18 = Source.E 3 in
      let x_18 = resolve w_35 last_18 in
      match Word.get_value (fst x_18) with
      | c_18 when c_18 = tag_ECons ->
          let splits_20 = Memo.splits (snd x_18) in
          let split0_20 = List.nth splits_20 0 in
          let split1_9 = List.nth splits_20 1 in
          ignore (pop_env w_35);
          push_env w_35 split0_20;
          push_env w_35 split1_9;
          assert_env_length w_35 5;
          push_env w_35 (Dynarray.get w_35.state.e 4);
          w_35.state.c <- pc_to_exp (int_to_pc 36);
          stepped w_35
      | c_18 -> failwith ("unreachable:" ^ string_of_int c_18 ^ "(37)"))
    37

let () =
  add_exp
    (fun w_34 ->
      assert_env_length w_34 2;
      let last_17 = Source.E 1 in
      let x_17 = resolve w_34 last_17 in
      match Word.get_value (fst x_17) with
      | c_17 when c_17 = tag_ECons ->
          let splits_19 = Memo.splits (snd x_17) in
          let split0_19 = List.nth splits_19 0 in
          let split1_8 = List.nth splits_19 1 in
          ignore (pop_env w_34);
          push_env w_34 split0_19;
          push_env w_34 split1_8;
          assert_env_length w_34 3;
          push_env w_34 (Dynarray.get w_34.state.e 2);
          w_34.state.c <- pc_to_exp (int_to_pc 37);
          stepped w_34
      | c_17 -> failwith ("unreachable:" ^ string_of_int c_17 ^ "(38)"))
    38

let () =
  add_exp
    (fun w_38 ->
      assert_env_length w_38 1;
      push_env w_38 (Dynarray.get w_38.state.e 0);
      w_38.state.c <- pc_to_exp (int_to_pc 41);
      stepped w_38)
    39

let () =
  add_exp
    (fun w_40 ->
      assert_env_length w_40 4;
      let last_22 = Source.E 3 in
      let x_22 = resolve w_40 last_22 in
      match Word.get_value (fst x_22) with
      | c_22 when c_22 = tag_ECons ->
          let splits_24 = Memo.splits (snd x_22) in
          let split0_24 = List.nth splits_24 0 in
          let split1_13 = List.nth splits_24 1 in
          ignore (pop_env w_40);
          push_env w_40 split0_24;
          push_env w_40 split1_13;
          assert_env_length w_40 5;
          push_env w_40 (Dynarray.get w_40.state.e 3);
          assert_env_length w_40 6;
          drop_n w_40 6 2;
          assert_env_length w_40 4;
          drop_n w_40 4 2;
          assert_env_length w_40 2;
          return_n w_40 2 (pc_to_exp (int_to_pc 0))
      | c_22 -> failwith ("unreachable:" ^ string_of_int c_22 ^ "(40)"))
    40

let () =
  add_exp
    (fun w_39 ->
      assert_env_length w_39 2;
      let last_21 = Source.E 1 in
      let x_21 = resolve w_39 last_21 in
      match Word.get_value (fst x_21) with
      | c_21 when c_21 = tag_ECons ->
          let splits_23 = Memo.splits (snd x_21) in
          let split0_23 = List.nth splits_23 0 in
          let split1_12 = List.nth splits_23 1 in
          ignore (pop_env w_39);
          push_env w_39 split0_23;
          push_env w_39 split1_12;
          assert_env_length w_39 3;
          push_env w_39 (Dynarray.get w_39.state.e 2);
          w_39.state.c <- pc_to_exp (int_to_pc 40);
          stepped w_39
      | c_21 -> failwith ("unreachable:" ^ string_of_int c_21 ^ "(41)"))
    41

let () =
  add_exp
    (fun w_41 ->
      assert_env_length w_41 1;
      push_env w_41 (Dynarray.get w_41.state.e 0);
      w_41.state.c <- pc_to_exp (int_to_pc 44);
      stepped w_41)
    42

let () =
  add_exp
    (fun w_43 ->
      assert_env_length w_43 4;
      let last_24 = Source.E 3 in
      let x_24 = resolve w_43 last_24 in
      match Word.get_value (fst x_24) with
      | c_24 when c_24 = tag_ECons ->
          let splits_26 = Memo.splits (snd x_24) in
          let split0_26 = List.nth splits_26 0 in
          let split1_15 = List.nth splits_26 1 in
          ignore (pop_env w_43);
          push_env w_43 split0_26;
          push_env w_43 split1_15;
          assert_env_length w_43 5;
          push_env w_43 (Dynarray.get w_43.state.e 3);
          assert_env_length w_43 6;
          drop_n w_43 6 2;
          assert_env_length w_43 4;
          drop_n w_43 4 2;
          assert_env_length w_43 2;
          return_n w_43 2 (pc_to_exp (int_to_pc 0))
      | c_24 -> failwith ("unreachable:" ^ string_of_int c_24 ^ "(43)"))
    43

let () =
  add_exp
    (fun w_42 ->
      assert_env_length w_42 2;
      let last_23 = Source.E 1 in
      let x_23 = resolve w_42 last_23 in
      match Word.get_value (fst x_23) with
      | c_23 when c_23 = tag_ECons ->
          let splits_25 = Memo.splits (snd x_23) in
          let split0_25 = List.nth splits_25 0 in
          let split1_14 = List.nth splits_25 1 in
          ignore (pop_env w_42);
          push_env w_42 split0_25;
          push_env w_42 split1_14;
          assert_env_length w_42 3;
          push_env w_42 (Dynarray.get w_42.state.e 1);
          w_42.state.c <- pc_to_exp (int_to_pc 43);
          stepped w_42
      | c_23 -> failwith ("unreachable:" ^ string_of_int c_23 ^ "(44)"))
    44

let () =
  add_exp
    (fun w_44 ->
      assert_env_length w_44 1;
      push_env w_44 (Dynarray.get w_44.state.e 0);
      w_44.state.c <- pc_to_exp (int_to_pc 47);
      stepped w_44)
    45

let () =
  add_exp
    (fun w_46 ->
      assert_env_length w_46 4;
      let last_26 = Source.E 3 in
      let x_26 = resolve w_46 last_26 in
      match Word.get_value (fst x_26) with
      | c_26 when c_26 = tag_ECons ->
          let splits_28 = Memo.splits (snd x_26) in
          let split0_28 = List.nth splits_28 0 in
          let split1_17 = List.nth splits_28 1 in
          ignore (pop_env w_46);
          push_env w_46 split0_28;
          push_env w_46 split1_17;
          assert_env_length w_46 5;
          push_env w_46 (Dynarray.get w_46.state.e 4);
          assert_env_length w_46 6;
          drop_n w_46 6 2;
          assert_env_length w_46 4;
          drop_n w_46 4 2;
          assert_env_length w_46 2;
          return_n w_46 2 (pc_to_exp (int_to_pc 0))
      | c_26 -> failwith ("unreachable:" ^ string_of_int c_26 ^ "(46)"))
    46

let () =
  add_exp
    (fun w_45 ->
      assert_env_length w_45 2;
      let last_25 = Source.E 1 in
      let x_25 = resolve w_45 last_25 in
      match Word.get_value (fst x_25) with
      | c_25 when c_25 = tag_ECons ->
          let splits_27 = Memo.splits (snd x_25) in
          let split0_27 = List.nth splits_27 0 in
          let split1_16 = List.nth splits_27 1 in
          ignore (pop_env w_45);
          push_env w_45 split0_27;
          push_env w_45 split1_16;
          assert_env_length w_45 3;
          push_env w_45 (Dynarray.get w_45.state.e 1);
          w_45.state.c <- pc_to_exp (int_to_pc 46);
          stepped w_45
      | c_25 -> failwith ("unreachable:" ^ string_of_int c_25 ^ "(47)"))
    47

let () =
  add_exp
    (fun w_47 ->
      assert_env_length w_47 1;
      push_env w_47 (Dynarray.get w_47.state.e 0);
      w_47.state.c <- pc_to_exp (int_to_pc 51);
      stepped w_47)
    48

let () =
  add_exp
    (fun w_50 ->
      assert_env_length w_50 6;
      let last_29 = Source.E 5 in
      let x_29 = resolve w_50 last_29 in
      match Word.get_value (fst x_29) with
      | c_29 when c_29 = tag_ECons ->
          let splits_31 = Memo.splits (snd x_29) in
          let split0_31 = List.nth splits_31 0 in
          let split1_20 = List.nth splits_31 1 in
          ignore (pop_env w_50);
          push_env w_50 split0_31;
          push_env w_50 split1_20;
          assert_env_length w_50 7;
          push_env w_50 (Dynarray.get w_50.state.e 6);
          assert_env_length w_50 8;
          drop_n w_50 8 2;
          assert_env_length w_50 6;
          drop_n w_50 6 2;
          assert_env_length w_50 4;
          drop_n w_50 4 2;
          assert_env_length w_50 2;
          return_n w_50 2 (pc_to_exp (int_to_pc 0))
      | c_29 -> failwith ("unreachable:" ^ string_of_int c_29 ^ "(49)"))
    49

let () =
  add_exp
    (fun w_49 ->
      assert_env_length w_49 4;
      let last_28 = Source.E 3 in
      let x_28 = resolve w_49 last_28 in
      match Word.get_value (fst x_28) with
      | c_28 when c_28 = tag_ECons ->
          let splits_30 = Memo.splits (snd x_28) in
          let split0_30 = List.nth splits_30 0 in
          let split1_19 = List.nth splits_30 1 in
          ignore (pop_env w_49);
          push_env w_49 split0_30;
          push_env w_49 split1_19;
          assert_env_length w_49 5;
          push_env w_49 (Dynarray.get w_49.state.e 4);
          w_49.state.c <- pc_to_exp (int_to_pc 49);
          stepped w_49
      | c_28 -> failwith ("unreachable:" ^ string_of_int c_28 ^ "(50)"))
    50

let () =
  add_exp
    (fun w_48 ->
      assert_env_length w_48 2;
      let last_27 = Source.E 1 in
      let x_27 = resolve w_48 last_27 in
      match Word.get_value (fst x_27) with
      | c_27 when c_27 = tag_ECons ->
          let splits_29 = Memo.splits (snd x_27) in
          let split0_29 = List.nth splits_29 0 in
          let split1_18 = List.nth splits_29 1 in
          ignore (pop_env w_48);
          push_env w_48 split0_29;
          push_env w_48 split1_18;
          assert_env_length w_48 3;
          push_env w_48 (Dynarray.get w_48.state.e 1);
          w_48.state.c <- pc_to_exp (int_to_pc 50);
          stepped w_48
      | c_27 -> failwith ("unreachable:" ^ string_of_int c_27 ^ "(51)"))
    51

let () =
  add_exp
    (fun w_51 ->
      assert_env_length w_51 1;
      push_env w_51 (Dynarray.get w_51.state.e 0);
      w_51.state.c <- pc_to_exp (int_to_pc 55);
      stepped w_51)
    52

let () =
  add_exp
    (fun w_54 ->
      assert_env_length w_54 6;
      let last_32 = Source.E 5 in
      let x_32 = resolve w_54 last_32 in
      match Word.get_value (fst x_32) with
      | c_32 when c_32 = tag_ECons ->
          let splits_34 = Memo.splits (snd x_32) in
          let split0_34 = List.nth splits_34 0 in
          let split1_23 = List.nth splits_34 1 in
          ignore (pop_env w_54);
          push_env w_54 split0_34;
          push_env w_54 split1_23;
          assert_env_length w_54 7;
          push_env w_54 (Dynarray.get w_54.state.e 5);
          assert_env_length w_54 8;
          drop_n w_54 8 2;
          assert_env_length w_54 6;
          drop_n w_54 6 2;
          assert_env_length w_54 4;
          drop_n w_54 4 2;
          assert_env_length w_54 2;
          return_n w_54 2 (pc_to_exp (int_to_pc 0))
      | c_32 -> failwith ("unreachable:" ^ string_of_int c_32 ^ "(53)"))
    53

let () =
  add_exp
    (fun w_53 ->
      assert_env_length w_53 4;
      let last_31 = Source.E 3 in
      let x_31 = resolve w_53 last_31 in
      match Word.get_value (fst x_31) with
      | c_31 when c_31 = tag_ECons ->
          let splits_33 = Memo.splits (snd x_31) in
          let split0_33 = List.nth splits_33 0 in
          let split1_22 = List.nth splits_33 1 in
          ignore (pop_env w_53);
          push_env w_53 split0_33;
          push_env w_53 split1_22;
          assert_env_length w_53 5;
          push_env w_53 (Dynarray.get w_53.state.e 4);
          w_53.state.c <- pc_to_exp (int_to_pc 53);
          stepped w_53
      | c_31 -> failwith ("unreachable:" ^ string_of_int c_31 ^ "(54)"))
    54

let () =
  add_exp
    (fun w_52 ->
      assert_env_length w_52 2;
      let last_30 = Source.E 1 in
      let x_30 = resolve w_52 last_30 in
      match Word.get_value (fst x_30) with
      | c_30 when c_30 = tag_ECons ->
          let splits_32 = Memo.splits (snd x_30) in
          let split0_32 = List.nth splits_32 0 in
          let split1_21 = List.nth splits_32 1 in
          ignore (pop_env w_52);
          push_env w_52 split0_32;
          push_env w_52 split1_21;
          assert_env_length w_52 3;
          push_env w_52 (Dynarray.get w_52.state.e 1);
          w_52.state.c <- pc_to_exp (int_to_pc 54);
          stepped w_52
      | c_30 -> failwith ("unreachable:" ^ string_of_int c_30 ^ "(55)"))
    55

let () =
  add_exp
    (fun w_55 ->
      assert_env_length w_55 1;
      push_env w_55 (Dynarray.get w_55.state.e 0);
      w_55.state.c <- pc_to_exp (int_to_pc 60);
      stepped w_55)
    56

let () =
  add_exp
    (fun w_59 ->
      assert_env_length w_59 8;
      let last_36 = Source.E 7 in
      let x_36 = resolve w_59 last_36 in
      match Word.get_value (fst x_36) with
      | c_36 when c_36 = tag_ECons ->
          let splits_38 = Memo.splits (snd x_36) in
          let split0_38 = List.nth splits_38 0 in
          let split1_27 = List.nth splits_38 1 in
          ignore (pop_env w_59);
          push_env w_59 split0_38;
          push_env w_59 split1_27;
          assert_env_length w_59 9;
          push_env w_59 (Dynarray.get w_59.state.e 7);
          assert_env_length w_59 10;
          drop_n w_59 10 2;
          assert_env_length w_59 8;
          drop_n w_59 8 2;
          assert_env_length w_59 6;
          drop_n w_59 6 2;
          assert_env_length w_59 4;
          drop_n w_59 4 2;
          assert_env_length w_59 2;
          return_n w_59 2 (pc_to_exp (int_to_pc 0))
      | c_36 -> failwith ("unreachable:" ^ string_of_int c_36 ^ "(57)"))
    57

let () =
  add_exp
    (fun w_58 ->
      assert_env_length w_58 6;
      let last_35 = Source.E 5 in
      let x_35 = resolve w_58 last_35 in
      match Word.get_value (fst x_35) with
      | c_35 when c_35 = tag_ECons ->
          let splits_37 = Memo.splits (snd x_35) in
          let split0_37 = List.nth splits_37 0 in
          let split1_26 = List.nth splits_37 1 in
          ignore (pop_env w_58);
          push_env w_58 split0_37;
          push_env w_58 split1_26;
          assert_env_length w_58 7;
          push_env w_58 (Dynarray.get w_58.state.e 6);
          w_58.state.c <- pc_to_exp (int_to_pc 57);
          stepped w_58
      | c_35 -> failwith ("unreachable:" ^ string_of_int c_35 ^ "(58)"))
    58

let () =
  add_exp
    (fun w_57 ->
      assert_env_length w_57 4;
      let last_34 = Source.E 3 in
      let x_34 = resolve w_57 last_34 in
      match Word.get_value (fst x_34) with
      | c_34 when c_34 = tag_ECons ->
          let splits_36 = Memo.splits (snd x_34) in
          let split0_36 = List.nth splits_36 0 in
          let split1_25 = List.nth splits_36 1 in
          ignore (pop_env w_57);
          push_env w_57 split0_36;
          push_env w_57 split1_25;
          assert_env_length w_57 5;
          push_env w_57 (Dynarray.get w_57.state.e 4);
          w_57.state.c <- pc_to_exp (int_to_pc 58);
          stepped w_57
      | c_34 -> failwith ("unreachable:" ^ string_of_int c_34 ^ "(59)"))
    59

let () =
  add_exp
    (fun w_56 ->
      assert_env_length w_56 2;
      let last_33 = Source.E 1 in
      let x_33 = resolve w_56 last_33 in
      match Word.get_value (fst x_33) with
      | c_33 when c_33 = tag_ECons ->
          let splits_35 = Memo.splits (snd x_33) in
          let split0_35 = List.nth splits_35 0 in
          let split1_24 = List.nth splits_35 1 in
          ignore (pop_env w_56);
          push_env w_56 split0_35;
          push_env w_56 split1_24;
          assert_env_length w_56 3;
          push_env w_56 (Dynarray.get w_56.state.e 1);
          w_56.state.c <- pc_to_exp (int_to_pc 59);
          stepped w_56
      | c_33 -> failwith ("unreachable:" ^ string_of_int c_33 ^ "(60)"))
    60

let () =
  add_exp
    (fun w_60 ->
      assert_env_length w_60 1;
      push_env w_60 (Dynarray.get w_60.state.e 0);
      w_60.state.c <- pc_to_exp (int_to_pc 62);
      stepped w_60)
    61

let () =
  add_exp
    (fun w_61 ->
      assert_env_length w_61 2;
      let last_37 = Source.E 1 in
      let x_37 = resolve w_61 last_37 in
      match Word.get_value (fst x_37) with
      | c_37 when c_37 = tag_ECons ->
          let splits_39 = Memo.splits (snd x_37) in
          let split0_39 = List.nth splits_39 0 in
          let split1_28 = List.nth splits_39 1 in
          ignore (pop_env w_61);
          push_env w_61 split0_39;
          push_env w_61 split1_28;
          assert_env_length w_61 3;
          push_env w_61 (Dynarray.get w_61.state.e 1);
          assert_env_length w_61 4;
          drop_n w_61 4 2;
          assert_env_length w_61 2;
          return_n w_61 2 (pc_to_exp (int_to_pc 0))
      | c_37 -> failwith ("unreachable:" ^ string_of_int c_37 ^ "(62)"))
    62

let () =
  add_exp
    (fun w_62 ->
      assert_env_length w_62 1;
      push_env w_62 (Dynarray.get w_62.state.e 0);
      w_62.state.c <- pc_to_exp (int_to_pc 64);
      stepped w_62)
    63

let () =
  add_exp
    (fun w_63 ->
      assert_env_length w_63 2;
      let last_38 = Source.E 1 in
      let x_38 = resolve w_63 last_38 in
      match Word.get_value (fst x_38) with
      | c_38 when c_38 = tag_ECons ->
          let splits_40 = Memo.splits (snd x_38) in
          let split0_40 = List.nth splits_40 0 in
          let split1_29 = List.nth splits_40 1 in
          ignore (pop_env w_63);
          push_env w_63 split0_40;
          push_env w_63 split1_29;
          assert_env_length w_63 3;
          push_env w_63 (Dynarray.get w_63.state.e 2);
          assert_env_length w_63 4;
          drop_n w_63 4 2;
          assert_env_length w_63 2;
          return_n w_63 2 (pc_to_exp (int_to_pc 0))
      | c_38 -> failwith ("unreachable:" ^ string_of_int c_38 ^ "(64)"))
    64

let () =
  add_exp
    (fun w_64 ->
      assert_env_length w_64 2;
      push_env w_64 (Dynarray.get w_64.state.e 0);
      w_64.state.c <- pc_to_exp (int_to_pc 84);
      stepped w_64)
    65

let () =
  add_exp
    (fun w_66 ->
      assert_env_length w_66 3;
      let last_40 = Source.E 2 in
      let x_40 = resolve w_66 last_40 in
      match Word.get_value (fst x_40) with
      | c_40 when c_40 = tag_SLambda ->
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
    (fun w_67 ->
      assert_env_length w_67 3;
      let last_41 = Source.E 2 in
      let x_41 = resolve w_67 last_41 in
      match Word.get_value (fst x_41) with
      | c_41 when c_41 = tag_SDefine ->
          ignore (pop_env w_67);
          assert_env_length w_67 2;
          push_env w_67 (Memo.from_int 1);
          assert_env_length w_67 3;
          return_n w_67 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_67);
          assert_env_length w_67 2;
          push_env w_67 (Memo.from_int 0);
          assert_env_length w_67 3;
          return_n w_67 3 (pc_to_exp (int_to_pc 0))
      | c_41 -> failwith ("unreachable:" ^ string_of_int c_41 ^ "(67)"))
    67

let () =
  add_exp
    (fun w_68 ->
      assert_env_length w_68 3;
      let last_42 = Source.E 2 in
      let x_42 = resolve w_68 last_42 in
      match Word.get_value (fst x_42) with
      | c_42 when c_42 = tag_SQuote ->
          ignore (pop_env w_68);
          assert_env_length w_68 2;
          push_env w_68 (Memo.from_int 1);
          assert_env_length w_68 3;
          return_n w_68 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_68);
          assert_env_length w_68 2;
          push_env w_68 (Memo.from_int 0);
          assert_env_length w_68 3;
          return_n w_68 3 (pc_to_exp (int_to_pc 0))
      | c_42 -> failwith ("unreachable:" ^ string_of_int c_42 ^ "(68)"))
    68

let () =
  add_exp
    (fun w_69 ->
      assert_env_length w_69 3;
      let last_43 = Source.E 2 in
      let x_43 = resolve w_69 last_43 in
      match Word.get_value (fst x_43) with
      | c_43 when c_43 = tag_SEq ->
          ignore (pop_env w_69);
          assert_env_length w_69 2;
          push_env w_69 (Memo.from_int 1);
          assert_env_length w_69 3;
          return_n w_69 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_69);
          assert_env_length w_69 2;
          push_env w_69 (Memo.from_int 0);
          assert_env_length w_69 3;
          return_n w_69 3 (pc_to_exp (int_to_pc 0))
      | c_43 -> failwith ("unreachable:" ^ string_of_int c_43 ^ "(69)"))
    69

let () =
  add_exp
    (fun w_70 ->
      assert_env_length w_70 3;
      let last_44 = Source.E 2 in
      let x_44 = resolve w_70 last_44 in
      match Word.get_value (fst x_44) with
      | c_44 when c_44 = tag_SDefvar ->
          ignore (pop_env w_70);
          assert_env_length w_70 2;
          push_env w_70 (Memo.from_int 1);
          assert_env_length w_70 3;
          return_n w_70 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_70);
          assert_env_length w_70 2;
          push_env w_70 (Memo.from_int 0);
          assert_env_length w_70 3;
          return_n w_70 3 (pc_to_exp (int_to_pc 0))
      | c_44 -> failwith ("unreachable:" ^ string_of_int c_44 ^ "(70)"))
    70

let () =
  add_exp
    (fun w_71 ->
      assert_env_length w_71 3;
      let last_45 = Source.E 2 in
      let x_45 = resolve w_71 last_45 in
      match Word.get_value (fst x_45) with
      | c_45 when c_45 = tag_SIf ->
          ignore (pop_env w_71);
          assert_env_length w_71 2;
          push_env w_71 (Memo.from_int 1);
          assert_env_length w_71 3;
          return_n w_71 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_71);
          assert_env_length w_71 2;
          push_env w_71 (Memo.from_int 0);
          assert_env_length w_71 3;
          return_n w_71 3 (pc_to_exp (int_to_pc 0))
      | c_45 -> failwith ("unreachable:" ^ string_of_int c_45 ^ "(71)"))
    71

let () =
  add_exp
    (fun w_72 ->
      assert_env_length w_72 3;
      let last_46 = Source.E 2 in
      let x_46 = resolve w_72 last_46 in
      match Word.get_value (fst x_46) with
      | c_46 when c_46 = tag_SCons ->
          ignore (pop_env w_72);
          assert_env_length w_72 2;
          push_env w_72 (Memo.from_int 1);
          assert_env_length w_72 3;
          return_n w_72 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_72);
          assert_env_length w_72 2;
          push_env w_72 (Memo.from_int 0);
          assert_env_length w_72 3;
          return_n w_72 3 (pc_to_exp (int_to_pc 0))
      | c_46 -> failwith ("unreachable:" ^ string_of_int c_46 ^ "(72)"))
    72

let () =
  add_exp
    (fun w_73 ->
      assert_env_length w_73 3;
      let last_47 = Source.E 2 in
      let x_47 = resolve w_73 last_47 in
      match Word.get_value (fst x_47) with
      | c_47 when c_47 = tag_SCond ->
          ignore (pop_env w_73);
          assert_env_length w_73 2;
          push_env w_73 (Memo.from_int 1);
          assert_env_length w_73 3;
          return_n w_73 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_73);
          assert_env_length w_73 2;
          push_env w_73 (Memo.from_int 0);
          assert_env_length w_73 3;
          return_n w_73 3 (pc_to_exp (int_to_pc 0))
      | c_47 -> failwith ("unreachable:" ^ string_of_int c_47 ^ "(73)"))
    73

let () =
  add_exp
    (fun w_74 ->
      assert_env_length w_74 3;
      let last_48 = Source.E 2 in
      let x_48 = resolve w_74 last_48 in
      match Word.get_value (fst x_48) with
      | c_48 when c_48 = tag_SAtom ->
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
      | c_48 -> failwith ("unreachable:" ^ string_of_int c_48 ^ "(74)"))
    74

let () =
  add_exp
    (fun w_75 ->
      assert_env_length w_75 3;
      let last_49 = Source.E 2 in
      let x_49 = resolve w_75 last_49 in
      match Word.get_value (fst x_49) with
      | c_49 when c_49 = tag_SPair ->
          ignore (pop_env w_75);
          assert_env_length w_75 2;
          push_env w_75 (Memo.from_int 1);
          assert_env_length w_75 3;
          return_n w_75 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_75);
          assert_env_length w_75 2;
          push_env w_75 (Memo.from_int 0);
          assert_env_length w_75 3;
          return_n w_75 3 (pc_to_exp (int_to_pc 0))
      | c_49 -> failwith ("unreachable:" ^ string_of_int c_49 ^ "(75)"))
    75

let () =
  add_exp
    (fun w_76 ->
      assert_env_length w_76 3;
      let last_50 = Source.E 2 in
      let x_50 = resolve w_76 last_50 in
      match Word.get_value (fst x_50) with
      | c_50 when c_50 = tag_SSymbol ->
          ignore (pop_env w_76);
          assert_env_length w_76 2;
          push_env w_76 (Memo.from_int 1);
          assert_env_length w_76 3;
          return_n w_76 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_76);
          assert_env_length w_76 2;
          push_env w_76 (Memo.from_int 0);
          assert_env_length w_76 3;
          return_n w_76 3 (pc_to_exp (int_to_pc 0))
      | c_50 -> failwith ("unreachable:" ^ string_of_int c_50 ^ "(76)"))
    76

let () =
  add_exp
    (fun w_77 ->
      assert_env_length w_77 3;
      let last_51 = Source.E 2 in
      let x_51 = resolve w_77 last_51 in
      match Word.get_value (fst x_51) with
      | c_51 when c_51 = tag_STrue ->
          ignore (pop_env w_77);
          assert_env_length w_77 2;
          push_env w_77 (Memo.from_int 1);
          assert_env_length w_77 3;
          return_n w_77 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_77);
          assert_env_length w_77 2;
          push_env w_77 (Memo.from_int 0);
          assert_env_length w_77 3;
          return_n w_77 3 (pc_to_exp (int_to_pc 0))
      | c_51 -> failwith ("unreachable:" ^ string_of_int c_51 ^ "(77)"))
    77

let () =
  add_exp
    (fun w_78 ->
      assert_env_length w_78 3;
      let last_52 = Source.E 2 in
      let x_52 = resolve w_78 last_52 in
      match Word.get_value (fst x_52) with
      | c_52 when c_52 = tag_SFalse ->
          ignore (pop_env w_78);
          assert_env_length w_78 2;
          push_env w_78 (Memo.from_int 1);
          assert_env_length w_78 3;
          return_n w_78 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_78);
          assert_env_length w_78 2;
          push_env w_78 (Memo.from_int 0);
          assert_env_length w_78 3;
          return_n w_78 3 (pc_to_exp (int_to_pc 0))
      | c_52 -> failwith ("unreachable:" ^ string_of_int c_52 ^ "(78)"))
    78

let () =
  add_exp
    (fun w_79 ->
      assert_env_length w_79 3;
      let last_53 = Source.E 2 in
      let x_53 = resolve w_79 last_53 in
      match Word.get_value (fst x_53) with
      | c_53 when c_53 = tag_SCar ->
          ignore (pop_env w_79);
          assert_env_length w_79 2;
          push_env w_79 (Memo.from_int 1);
          assert_env_length w_79 3;
          return_n w_79 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_79);
          assert_env_length w_79 2;
          push_env w_79 (Memo.from_int 0);
          assert_env_length w_79 3;
          return_n w_79 3 (pc_to_exp (int_to_pc 0))
      | c_53 -> failwith ("unreachable:" ^ string_of_int c_53 ^ "(79)"))
    79

let () =
  add_exp
    (fun w_80 ->
      assert_env_length w_80 3;
      let last_54 = Source.E 2 in
      let x_54 = resolve w_80 last_54 in
      match Word.get_value (fst x_54) with
      | c_54 when c_54 = tag_SCdr ->
          ignore (pop_env w_80);
          assert_env_length w_80 2;
          push_env w_80 (Memo.from_int 1);
          assert_env_length w_80 3;
          return_n w_80 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_80);
          assert_env_length w_80 2;
          push_env w_80 (Memo.from_int 0);
          assert_env_length w_80 3;
          return_n w_80 3 (pc_to_exp (int_to_pc 0))
      | c_54 -> failwith ("unreachable:" ^ string_of_int c_54 ^ "(80)"))
    80

let () =
  add_exp
    (fun w_81 ->
      assert_env_length w_81 3;
      let last_55 = Source.E 2 in
      let x_55 = resolve w_81 last_55 in
      match Word.get_value (fst x_55) with
      | c_55 when c_55 = tag_SNull ->
          ignore (pop_env w_81);
          assert_env_length w_81 2;
          push_env w_81 (Memo.from_int 1);
          assert_env_length w_81 3;
          return_n w_81 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_81);
          assert_env_length w_81 2;
          push_env w_81 (Memo.from_int 0);
          assert_env_length w_81 3;
          return_n w_81 3 (pc_to_exp (int_to_pc 0))
      | c_55 -> failwith ("unreachable:" ^ string_of_int c_55 ^ "(81)"))
    81

let () =
  add_exp
    (fun w_82 ->
      assert_env_length w_82 3;
      let last_56 = Source.E 2 in
      let x_56 = resolve w_82 last_56 in
      match Word.get_value (fst x_56) with
      | c_56 when c_56 = tag_SError ->
          ignore (pop_env w_82);
          assert_env_length w_82 2;
          push_env w_82 (Memo.from_int 1);
          assert_env_length w_82 3;
          return_n w_82 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_82);
          assert_env_length w_82 2;
          push_env w_82 (Memo.from_int 0);
          assert_env_length w_82 3;
          return_n w_82 3 (pc_to_exp (int_to_pc 0))
      | c_56 -> failwith ("unreachable:" ^ string_of_int c_56 ^ "(82)"))
    82

let () =
  add_exp
    (fun w_83 ->
      assert_env_length w_83 3;
      let last_57 = Source.E 2 in
      let x_57 = resolve w_83 last_57 in
      match Word.get_value (fst x_57) with
      | c_57 when c_57 = tag_SVar ->
          ignore (pop_env w_83);
          assert_env_length w_83 2;
          push_env w_83 (Memo.from_int 1);
          assert_env_length w_83 3;
          return_n w_83 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_83);
          assert_env_length w_83 2;
          push_env w_83 (Memo.from_int 0);
          assert_env_length w_83 3;
          return_n w_83 3 (pc_to_exp (int_to_pc 0))
      | c_57 -> failwith ("unreachable:" ^ string_of_int c_57 ^ "(83)"))
    83

let () =
  add_exp
    (fun w_65 ->
      assert_env_length w_65 3;
      let last_39 = Source.E 2 in
      let x_39 = resolve w_65 last_39 in
      match Word.get_value (fst x_39) with
      | c_39 when c_39 = tag_SLambda ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 66);
          stepped w_65
      | c_39 when c_39 = tag_SDefine ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 67);
          stepped w_65
      | c_39 when c_39 = tag_SQuote ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 68);
          stepped w_65
      | c_39 when c_39 = tag_SEq ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 69);
          stepped w_65
      | c_39 when c_39 = tag_SDefvar ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 70);
          stepped w_65
      | c_39 when c_39 = tag_SIf ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 71);
          stepped w_65
      | c_39 when c_39 = tag_SCons ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 72);
          stepped w_65
      | c_39 when c_39 = tag_SCond ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 73);
          stepped w_65
      | c_39 when c_39 = tag_SAtom ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 74);
          stepped w_65
      | c_39 when c_39 = tag_SPair ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 75);
          stepped w_65
      | c_39 when c_39 = tag_SSymbol ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 76);
          stepped w_65
      | c_39 when c_39 = tag_STrue ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 77);
          stepped w_65
      | c_39 when c_39 = tag_SFalse ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 78);
          stepped w_65
      | c_39 when c_39 = tag_SCar ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 79);
          stepped w_65
      | c_39 when c_39 = tag_SCdr ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 80);
          stepped w_65
      | c_39 when c_39 = tag_SNull ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 81);
          stepped w_65
      | c_39 when c_39 = tag_SError ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 82);
          stepped w_65
      | c_39 when c_39 = tag_SVar ->
          ignore (pop_env w_65);
          assert_env_length w_65 2;
          push_env w_65 (Dynarray.get w_65.state.e 1);
          w_65.state.c <- pc_to_exp (int_to_pc 83);
          stepped w_65
      | c_39 -> failwith ("unreachable:" ^ string_of_int c_39 ^ "(84)"))
    84

let () =
  add_exp
    (fun w_84 ->
      assert_env_length w_84 2;
      push_env w_84 (Dynarray.get w_84.state.e 0);
      w_84.state.c <- pc_to_exp (int_to_pc 92);
      stepped w_84)
    85

let () =
  add_exp
    (fun w_87 ->
      assert_env_length w_87 6;
      let x0_1 = resolve w_87 (Source.E 4) in
      let x1_1 = resolve w_87 (Source.E 5) in
      ignore (pop_env w_87);
      ignore (pop_env w_87);
      push_env w_87 (Memo.from_int (if Word.get_value (fst x0_1) = Word.get_value (fst x1_1) then 1 else 0));
      assert_env_length w_87 5;
      drop_n w_87 5 1;
      assert_env_length w_87 4;
      drop_n w_87 4 1;
      assert_env_length w_87 3;
      return_n w_87 3 (pc_to_exp (int_to_pc 0)))
    86

let () =
  add_exp
    (fun w_86 ->
      assert_env_length w_86 4;
      let last_59 = Source.E 3 in
      let x_59 = resolve w_86 last_59 in
      match Word.get_value (fst x_59) with
      | c_59 when c_59 = tag_AVar ->
          let splits_42 = Memo.splits (snd x_59) in
          let split0_42 = List.nth splits_42 0 in
          ignore (pop_env w_86);
          push_env w_86 split0_42;
          assert_env_length w_86 4;
          push_env w_86 (Dynarray.get w_86.state.e 2);
          assert_env_length w_86 5;
          push_env w_86 (Dynarray.get w_86.state.e 3);
          w_86.state.c <- pc_to_exp (int_to_pc 86);
          stepped w_86
      | _ ->
          ignore (pop_env w_86);
          assert_env_length w_86 3;
          push_env w_86 (Memo.from_int 0);
          assert_env_length w_86 4;
          drop_n w_86 4 1;
          assert_env_length w_86 3;
          return_n w_86 3 (pc_to_exp (int_to_pc 0))
      | c_59 -> failwith ("unreachable:" ^ string_of_int c_59 ^ "(87)"))
    87

let () =
  add_exp
    (fun w_89 ->
      assert_env_length w_89 6;
      let x0_2 = resolve w_89 (Source.E 4) in
      let x1_2 = resolve w_89 (Source.E 5) in
      ignore (pop_env w_89);
      ignore (pop_env w_89);
      push_env w_89 (Memo.from_int (if Word.get_value (fst x0_2) = Word.get_value (fst x1_2) then 1 else 0));
      assert_env_length w_89 5;
      drop_n w_89 5 1;
      assert_env_length w_89 4;
      drop_n w_89 4 1;
      assert_env_length w_89 3;
      return_n w_89 3 (pc_to_exp (int_to_pc 0)))
    88

let () =
  add_exp
    (fun w_88 ->
      assert_env_length w_88 4;
      let last_60 = Source.E 3 in
      let x_60 = resolve w_88 last_60 in
      match Word.get_value (fst x_60) with
      | c_60 when c_60 = tag_ANumber ->
          let splits_44 = Memo.splits (snd x_60) in
          let split0_44 = List.nth splits_44 0 in
          ignore (pop_env w_88);
          push_env w_88 split0_44;
          assert_env_length w_88 4;
          push_env w_88 (Dynarray.get w_88.state.e 2);
          assert_env_length w_88 5;
          push_env w_88 (Dynarray.get w_88.state.e 3);
          w_88.state.c <- pc_to_exp (int_to_pc 88);
          stepped w_88
      | _ ->
          ignore (pop_env w_88);
          assert_env_length w_88 3;
          push_env w_88 (Memo.from_int 0);
          assert_env_length w_88 4;
          drop_n w_88 4 1;
          assert_env_length w_88 3;
          return_n w_88 3 (pc_to_exp (int_to_pc 0))
      | c_60 -> failwith ("unreachable:" ^ string_of_int c_60 ^ "(89)"))
    89

let () =
  add_exp
    (fun w_90 ->
      assert_env_length w_90 4;
      let last_61 = Source.E 3 in
      let x_61 = resolve w_90 last_61 in
      match Word.get_value (fst x_61) with
      | c_61 when c_61 = tag_ASymbol ->
          let splits_46 = Memo.splits (snd x_61) in
          let split0_46 = List.nth splits_46 0 in
          ignore (pop_env w_90);
          push_env w_90 split0_46;
          assert_env_length w_90 4;
          push_env w_90 (Dynarray.get w_90.state.e 2);
          assert_env_length w_90 5;
          push_env w_90 (Dynarray.get w_90.state.e 3);
          assert_env_length w_90 6;
          ignore (env_call w_90 [] 2);
          w_90.state.c <- pc_to_exp (int_to_pc 65);
          stepped w_90
      | _ ->
          ignore (pop_env w_90);
          assert_env_length w_90 3;
          push_env w_90 (Memo.from_int 0);
          assert_env_length w_90 4;
          drop_n w_90 4 1;
          assert_env_length w_90 3;
          return_n w_90 3 (pc_to_exp (int_to_pc 0))
      | c_61 -> failwith ("unreachable:" ^ string_of_int c_61 ^ "(90)"))
    90

let () =
  add_exp
    (fun w_91 ->
      assert_env_length w_91 3;
      let last_62 = Source.E 2 in
      let x_62 = resolve w_91 last_62 in
      match Word.get_value (fst x_62) with
      | c_62 when c_62 = tag_ANIL ->
          ignore (pop_env w_91);
          assert_env_length w_91 2;
          push_env w_91 (Memo.from_int 1);
          assert_env_length w_91 3;
          return_n w_91 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_91);
          assert_env_length w_91 2;
          push_env w_91 (Memo.from_int 0);
          assert_env_length w_91 3;
          return_n w_91 3 (pc_to_exp (int_to_pc 0))
      | c_62 -> failwith ("unreachable:" ^ string_of_int c_62 ^ "(91)"))
    91

let () =
  add_exp
    (fun w_85 ->
      assert_env_length w_85 3;
      let last_58 = Source.E 2 in
      let x_58 = resolve w_85 last_58 in
      match Word.get_value (fst x_58) with
      | c_58 when c_58 = tag_AVar ->
          let splits_41 = Memo.splits (snd x_58) in
          let split0_41 = List.nth splits_41 0 in
          ignore (pop_env w_85);
          push_env w_85 split0_41;
          assert_env_length w_85 3;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          w_85.state.c <- pc_to_exp (int_to_pc 87);
          stepped w_85
      | c_58 when c_58 = tag_ANumber ->
          let splits_43 = Memo.splits (snd x_58) in
          let split0_43 = List.nth splits_43 0 in
          ignore (pop_env w_85);
          push_env w_85 split0_43;
          assert_env_length w_85 3;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          w_85.state.c <- pc_to_exp (int_to_pc 89);
          stepped w_85
      | c_58 when c_58 = tag_ASymbol ->
          let splits_45 = Memo.splits (snd x_58) in
          let split0_45 = List.nth splits_45 0 in
          ignore (pop_env w_85);
          push_env w_85 split0_45;
          assert_env_length w_85 3;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          w_85.state.c <- pc_to_exp (int_to_pc 90);
          stepped w_85
      | c_58 when c_58 = tag_ANIL ->
          ignore (pop_env w_85);
          assert_env_length w_85 2;
          push_env w_85 (Dynarray.get w_85.state.e 1);
          w_85.state.c <- pc_to_exp (int_to_pc 91);
          stepped w_85
      | c_58 -> failwith ("unreachable:" ^ string_of_int c_58 ^ "(92)"))
    92

let () =
  add_exp
    (fun w_92 ->
      assert_env_length w_92 2;
      push_env w_92 (Dynarray.get w_92.state.e 0);
      w_92.state.c <- pc_to_exp (int_to_pc 95);
      stepped w_92)
    93

let () =
  add_exp
    (fun w_94 ->
      assert_env_length w_94 4;
      let last_64 = Source.E 3 in
      let x_64 = resolve w_94 last_64 in
      match Word.get_value (fst x_64) with
      | c_64 when c_64 = tag_EAtom ->
          let splits_48 = Memo.splits (snd x_64) in
          let split0_48 = List.nth splits_48 0 in
          ignore (pop_env w_94);
          push_env w_94 split0_48;
          assert_env_length w_94 4;
          push_env w_94 (Dynarray.get w_94.state.e 2);
          assert_env_length w_94 5;
          push_env w_94 (Dynarray.get w_94.state.e 3);
          assert_env_length w_94 6;
          ignore (env_call w_94 [] 2);
          w_94.state.c <- pc_to_exp (int_to_pc 85);
          stepped w_94
      | _ ->
          ignore (pop_env w_94);
          assert_env_length w_94 3;
          push_env w_94 (Memo.from_int 0);
          assert_env_length w_94 4;
          drop_n w_94 4 1;
          assert_env_length w_94 3;
          return_n w_94 3 (pc_to_exp (int_to_pc 0))
      | c_64 -> failwith ("unreachable:" ^ string_of_int c_64 ^ "(94)"))
    94

let () =
  add_exp
    (fun w_93 ->
      assert_env_length w_93 3;
      let last_63 = Source.E 2 in
      let x_63 = resolve w_93 last_63 in
      match Word.get_value (fst x_63) with
      | c_63 when c_63 = tag_EAtom ->
          let splits_47 = Memo.splits (snd x_63) in
          let split0_47 = List.nth splits_47 0 in
          ignore (pop_env w_93);
          push_env w_93 split0_47;
          assert_env_length w_93 3;
          push_env w_93 (Dynarray.get w_93.state.e 1);
          w_93.state.c <- pc_to_exp (int_to_pc 94);
          stepped w_93
      | c_63 when c_63 = tag_ECons ->
          let splits_49 = Memo.splits (snd x_63) in
          let split0_49 = List.nth splits_49 0 in
          let split1_30 = List.nth splits_49 1 in
          ignore (pop_env w_93);
          push_env w_93 split0_49;
          push_env w_93 split1_30;
          assert_env_length w_93 4;
          push_env w_93 (Memo.from_int 0);
          assert_env_length w_93 5;
          drop_n w_93 5 2;
          assert_env_length w_93 3;
          return_n w_93 3 (pc_to_exp (int_to_pc 0))
      | c_63 -> failwith ("unreachable:" ^ string_of_int c_63 ^ "(95)"))
    95

let () =
  add_exp
    (fun w_95 ->
      assert_env_length w_95 2;
      push_env w_95 (Dynarray.get w_95.state.e 0);
      w_95.state.c <- pc_to_exp (int_to_pc 102);
      stepped w_95)
    96

let () =
  add_exp
    (fun w_98 ->
      assert_env_length w_98 6;
      let x0_3 = resolve w_98 (Source.E 4) in
      let x1_3 = resolve w_98 (Source.E 5) in
      ignore (pop_env w_98);
      ignore (pop_env w_98);
      push_env w_98 (Memo.from_int (if Word.get_value (fst x0_3) = Word.get_value (fst x1_3) then 1 else 0));
      assert_env_length w_98 5;
      drop_n w_98 5 1;
      assert_env_length w_98 4;
      drop_n w_98 4 1;
      assert_env_length w_98 3;
      return_n w_98 3 (pc_to_exp (int_to_pc 0)))
    97

let () =
  add_exp
    (fun w_97 ->
      assert_env_length w_97 4;
      let last_66 = Source.E 3 in
      let x_66 = resolve w_97 last_66 in
      match Word.get_value (fst x_66) with
      | c_66 when c_66 = tag_VNumber ->
          let splits_51 = Memo.splits (snd x_66) in
          let split0_51 = List.nth splits_51 0 in
          ignore (pop_env w_97);
          push_env w_97 split0_51;
          assert_env_length w_97 4;
          push_env w_97 (Dynarray.get w_97.state.e 2);
          assert_env_length w_97 5;
          push_env w_97 (Dynarray.get w_97.state.e 3);
          w_97.state.c <- pc_to_exp (int_to_pc 97);
          stepped w_97
      | c_66 when c_66 = tag_VQuote ->
          let splits_52 = Memo.splits (snd x_66) in
          let split0_52 = List.nth splits_52 0 in
          ignore (pop_env w_97);
          push_env w_97 split0_52;
          assert_env_length w_97 4;
          push_env w_97 (Dynarray.get w_97.state.e 3);
          assert_env_length w_97 5;
          let keep_vals_0 = env_call w_97 [ 2 ] 1 in
          w_97.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_vals_0; w_97.state.k ];
          w_97.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_97
      | _ ->
          ignore (pop_env w_97);
          assert_env_length w_97 3;
          push_env w_97 (Memo.from_int 0);
          assert_env_length w_97 4;
          drop_n w_97 4 1;
          assert_env_length w_97 3;
          return_n w_97 3 (pc_to_exp (int_to_pc 0))
      | c_66 -> failwith ("unreachable:" ^ string_of_int c_66 ^ "(98)"))
    98

let () =
  add_exp
    (fun w_99 ->
      assert_env_length w_99 4;
      let last_67 = Source.E 3 in
      let x_67 = resolve w_99 last_67 in
      match Word.get_value (fst x_67) with
      | c_67 when c_67 = tag_VSymbol ->
          let splits_54 = Memo.splits (snd x_67) in
          let split0_54 = List.nth splits_54 0 in
          ignore (pop_env w_99);
          push_env w_99 split0_54;
          assert_env_length w_99 4;
          push_env w_99 (Dynarray.get w_99.state.e 2);
          assert_env_length w_99 5;
          push_env w_99 (Dynarray.get w_99.state.e 3);
          assert_env_length w_99 6;
          ignore (env_call w_99 [] 2);
          w_99.state.c <- pc_to_exp (int_to_pc 65);
          stepped w_99
      | _ ->
          ignore (pop_env w_99);
          assert_env_length w_99 3;
          push_env w_99 (Memo.from_int 0);
          assert_env_length w_99 4;
          drop_n w_99 4 1;
          assert_env_length w_99 3;
          return_n w_99 3 (pc_to_exp (int_to_pc 0))
      | c_67 -> failwith ("unreachable:" ^ string_of_int c_67 ^ "(99)"))
    99

let () =
  add_exp
    (fun w_100 ->
      assert_env_length w_100 4;
      let last_68 = Source.E 3 in
      let x_68 = resolve w_100 last_68 in
      match Word.get_value (fst x_68) with
      | c_68 when c_68 = tag_VQuote ->
          let splits_56 = Memo.splits (snd x_68) in
          let split0_56 = List.nth splits_56 0 in
          ignore (pop_env w_100);
          push_env w_100 split0_56;
          assert_env_length w_100 4;
          push_env w_100 (Dynarray.get w_100.state.e 2);
          assert_env_length w_100 5;
          push_env w_100 (Dynarray.get w_100.state.e 3);
          assert_env_length w_100 6;
          ignore (env_call w_100 [] 2);
          w_100.state.c <- pc_to_exp (int_to_pc 93);
          stepped w_100
      | c_68 when c_68 = tag_VNumber ->
          let splits_57 = Memo.splits (snd x_68) in
          let split0_57 = List.nth splits_57 0 in
          ignore (pop_env w_100);
          push_env w_100 split0_57;
          assert_env_length w_100 4;
          push_env w_100 (Dynarray.get w_100.state.e 2);
          assert_env_length w_100 5;
          let keep_vals_1 = env_call w_100 [ 3 ] 1 in
          w_100.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_vals_1; w_100.state.k ];
          w_100.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_100
      | _ ->
          ignore (pop_env w_100);
          assert_env_length w_100 3;
          push_env w_100 (Memo.from_int 0);
          assert_env_length w_100 4;
          drop_n w_100 4 1;
          assert_env_length w_100 3;
          return_n w_100 3 (pc_to_exp (int_to_pc 0))
      | c_68 -> failwith ("unreachable:" ^ string_of_int c_68 ^ "(100)"))
    100

let () =
  add_exp
    (fun w_101 ->
      assert_env_length w_101 3;
      let last_69 = Source.E 2 in
      let x_69 = resolve w_101 last_69 in
      match Word.get_value (fst x_69) with
      | c_69 when c_69 = tag_VNIL ->
          ignore (pop_env w_101);
          assert_env_length w_101 2;
          push_env w_101 (Memo.from_int 1);
          assert_env_length w_101 3;
          return_n w_101 3 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_101);
          assert_env_length w_101 2;
          push_env w_101 (Memo.from_int 0);
          assert_env_length w_101 3;
          return_n w_101 3 (pc_to_exp (int_to_pc 0))
      | c_69 -> failwith ("unreachable:" ^ string_of_int c_69 ^ "(101)"))
    101

let () =
  add_exp
    (fun w_96 ->
      assert_env_length w_96 3;
      let last_65 = Source.E 2 in
      let x_65 = resolve w_96 last_65 in
      match Word.get_value (fst x_65) with
      | c_65 when c_65 = tag_VNumber ->
          let splits_50 = Memo.splits (snd x_65) in
          let split0_50 = List.nth splits_50 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_50;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 98);
          stepped w_96
      | c_65 when c_65 = tag_VSymbol ->
          let splits_53 = Memo.splits (snd x_65) in
          let split0_53 = List.nth splits_53 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_53;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 99);
          stepped w_96
      | c_65 when c_65 = tag_VQuote ->
          let splits_55 = Memo.splits (snd x_65) in
          let split0_55 = List.nth splits_55 0 in
          ignore (pop_env w_96);
          push_env w_96 split0_55;
          assert_env_length w_96 3;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 100);
          stepped w_96
      | c_65 when c_65 = tag_VNIL ->
          ignore (pop_env w_96);
          assert_env_length w_96 2;
          push_env w_96 (Dynarray.get w_96.state.e 1);
          w_96.state.c <- pc_to_exp (int_to_pc 101);
          stepped w_96
      | c_65 when c_65 = tag_VCons ->
          let splits_58 = Memo.splits (snd x_65) in
          let split0_58 = List.nth splits_58 0 in
          let split1_31 = List.nth splits_58 1 in
          ignore (pop_env w_96);
          push_env w_96 split0_58;
          push_env w_96 split1_31;
          assert_env_length w_96 4;
          push_env w_96 (Memo.from_int 0);
          assert_env_length w_96 5;
          drop_n w_96 5 2;
          assert_env_length w_96 3;
          return_n w_96 3 (pc_to_exp (int_to_pc 0))
      | c_65 when c_65 = tag_VClosure ->
          let splits_59 = Memo.splits (snd x_65) in
          let split0_59 = List.nth splits_59 0 in
          let split1_32 = List.nth splits_59 1 in
          ignore (pop_env w_96);
          push_env w_96 split0_59;
          push_env w_96 split1_32;
          assert_env_length w_96 4;
          push_env w_96 (Memo.from_int 0);
          assert_env_length w_96 5;
          drop_n w_96 5 2;
          assert_env_length w_96 3;
          return_n w_96 3 (pc_to_exp (int_to_pc 0))
      | c_65 -> failwith ("unreachable:" ^ string_of_int c_65 ^ "(102)"))
    102

let () =
  add_exp
    (fun w_102 ->
      assert_env_length w_102 1;
      push_env w_102 (Dynarray.get w_102.state.e 0);
      w_102.state.c <- pc_to_exp (int_to_pc 105);
      stepped w_102)
    103

let () =
  add_exp
    (fun w_104 ->
      assert_env_length w_104 3;
      let last_71 = Source.E 2 in
      let x_71 = resolve w_104 last_71 in
      match Word.get_value (fst x_71) with
      | c_71 when c_71 = tag_ECons ->
          let splits_63 = Memo.splits (snd x_71) in
          let split0_63 = List.nth splits_63 0 in
          let split1_33 = List.nth splits_63 1 in
          ignore (pop_env w_104);
          push_env w_104 split0_63;
          push_env w_104 split1_33;
          assert_env_length w_104 4;
          push_env w_104 (Dynarray.get w_104.state.e 1);
          assert_env_length w_104 5;
          let keep_vals_2 = env_call w_104 [] 1 in
          w_104.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_vals_2; w_104.state.k ];
          w_104.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_104
      | _ ->
          ignore (pop_env w_104);
          failwith "car: PAIR expected"
      | c_71 -> failwith ("unreachable:" ^ string_of_int c_71 ^ "(104)"))
    104

let () =
  add_exp
    (fun w_103 ->
      assert_env_length w_103 2;
      let last_70 = Source.E 1 in
      let x_70 = resolve w_103 last_70 in
      match Word.get_value (fst x_70) with
      | c_70 when c_70 = tag_VNumber ->
          let splits_60 = Memo.splits (snd x_70) in
          let split0_60 = List.nth splits_60 0 in
          ignore (pop_env w_103);
          push_env w_103 split0_60;
          failwith "car: cannot apply on NUMBER"
      | c_70 when c_70 = tag_VSymbol ->
          let splits_61 = Memo.splits (snd x_70) in
          let split0_61 = List.nth splits_61 0 in
          ignore (pop_env w_103);
          push_env w_103 split0_61;
          failwith "car: cannot apply on SYMBOL"
      | c_70 when c_70 = tag_VQuote ->
          let splits_62 = Memo.splits (snd x_70) in
          let split0_62 = List.nth splits_62 0 in
          ignore (pop_env w_103);
          push_env w_103 split0_62;
          assert_env_length w_103 2;
          push_env w_103 (Dynarray.get w_103.state.e 1);
          w_103.state.c <- pc_to_exp (int_to_pc 104);
          stepped w_103
      | c_70 when c_70 = tag_VNIL ->
          ignore (pop_env w_103);
          failwith "car: cannot apply on NIL"
      | c_70 when c_70 = tag_VCons ->
          let splits_64 = Memo.splits (snd x_70) in
          let split0_64 = List.nth splits_64 0 in
          let split1_34 = List.nth splits_64 1 in
          ignore (pop_env w_103);
          push_env w_103 split0_64;
          push_env w_103 split1_34;
          assert_env_length w_103 3;
          push_env w_103 (Dynarray.get w_103.state.e 1);
          assert_env_length w_103 4;
          drop_n w_103 4 2;
          assert_env_length w_103 2;
          return_n w_103 2 (pc_to_exp (int_to_pc 0))
      | c_70 when c_70 = tag_VClosure ->
          let splits_65 = Memo.splits (snd x_70) in
          let split0_65 = List.nth splits_65 0 in
          let split1_35 = List.nth splits_65 1 in
          ignore (pop_env w_103);
          push_env w_103 split0_65;
          push_env w_103 split1_35;
          failwith "car: cannot apply on CLOSURE"
      | c_70 -> failwith ("unreachable:" ^ string_of_int c_70 ^ "(105)"))
    105

let () =
  add_exp
    (fun w_105 ->
      assert_env_length w_105 1;
      push_env w_105 (Dynarray.get w_105.state.e 0);
      w_105.state.c <- pc_to_exp (int_to_pc 108);
      stepped w_105)
    106

let () =
  add_exp
    (fun w_107 ->
      assert_env_length w_107 3;
      let last_73 = Source.E 2 in
      let x_73 = resolve w_107 last_73 in
      match Word.get_value (fst x_73) with
      | c_73 when c_73 = tag_ECons ->
          let splits_69 = Memo.splits (snd x_73) in
          let split0_69 = List.nth splits_69 0 in
          let split1_36 = List.nth splits_69 1 in
          ignore (pop_env w_107);
          push_env w_107 split0_69;
          push_env w_107 split1_36;
          assert_env_length w_107 4;
          push_env w_107 (Dynarray.get w_107.state.e 1);
          assert_env_length w_107 5;
          let keep_vals_3 = env_call w_107 [] 1 in
          w_107.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_vals_3; w_107.state.k ];
          w_107.state.c <- pc_to_exp (int_to_pc 63);
          stepped w_107
      | _ ->
          ignore (pop_env w_107);
          failwith "cdr: PAIR expected"
      | c_73 -> failwith ("unreachable:" ^ string_of_int c_73 ^ "(107)"))
    107

let () =
  add_exp
    (fun w_106 ->
      assert_env_length w_106 2;
      let last_72 = Source.E 1 in
      let x_72 = resolve w_106 last_72 in
      match Word.get_value (fst x_72) with
      | c_72 when c_72 = tag_VNumber ->
          let splits_66 = Memo.splits (snd x_72) in
          let split0_66 = List.nth splits_66 0 in
          ignore (pop_env w_106);
          push_env w_106 split0_66;
          failwith "cdr: cannot apply on NUMBER"
      | c_72 when c_72 = tag_VSymbol ->
          let splits_67 = Memo.splits (snd x_72) in
          let split0_67 = List.nth splits_67 0 in
          ignore (pop_env w_106);
          push_env w_106 split0_67;
          failwith "cdr: cannot apply on SYMBOL"
      | c_72 when c_72 = tag_VQuote ->
          let splits_68 = Memo.splits (snd x_72) in
          let split0_68 = List.nth splits_68 0 in
          ignore (pop_env w_106);
          push_env w_106 split0_68;
          assert_env_length w_106 2;
          push_env w_106 (Dynarray.get w_106.state.e 1);
          w_106.state.c <- pc_to_exp (int_to_pc 107);
          stepped w_106
      | c_72 when c_72 = tag_VNIL ->
          ignore (pop_env w_106);
          failwith "cdr: cannot apply on NIL"
      | c_72 when c_72 = tag_VCons ->
          let splits_70 = Memo.splits (snd x_72) in
          let split0_70 = List.nth splits_70 0 in
          let split1_37 = List.nth splits_70 1 in
          ignore (pop_env w_106);
          push_env w_106 split0_70;
          push_env w_106 split1_37;
          assert_env_length w_106 3;
          push_env w_106 (Dynarray.get w_106.state.e 2);
          assert_env_length w_106 4;
          drop_n w_106 4 2;
          assert_env_length w_106 2;
          return_n w_106 2 (pc_to_exp (int_to_pc 0))
      | c_72 when c_72 = tag_VClosure ->
          let splits_71 = Memo.splits (snd x_72) in
          let split0_71 = List.nth splits_71 0 in
          let split1_38 = List.nth splits_71 1 in
          ignore (pop_env w_106);
          push_env w_106 split0_71;
          push_env w_106 split1_38;
          failwith "cdr: cannot apply on CLOSURE"
      | c_72 -> failwith ("unreachable:" ^ string_of_int c_72 ^ "(108)"))
    108

let () =
  add_exp
    (fun w_108 ->
      assert_env_length w_108 1;
      push_env w_108 (Dynarray.get w_108.state.e 0);
      w_108.state.c <- pc_to_exp (int_to_pc 110);
      stepped w_108)
    109

let () =
  add_exp
    (fun w_109 ->
      assert_env_length w_109 2;
      let last_74 = Source.E 1 in
      let x_74 = resolve w_109 last_74 in
      match Word.get_value (fst x_74) with
      | c_74 when c_74 = tag_VNumber ->
          let splits_72 = Memo.splits (snd x_74) in
          let split0_72 = List.nth splits_72 0 in
          ignore (pop_env w_109);
          push_env w_109 split0_72;
          assert_env_length w_109 2;
          push_env w_109 (Memo.from_constructor tag_Unit);
          assert_env_length w_109 3;
          ignore (env_call w_109 [] 1);
          w_109.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_109
      | c_74 when c_74 = tag_VSymbol ->
          let splits_73 = Memo.splits (snd x_74) in
          let split0_73 = List.nth splits_73 0 in
          ignore (pop_env w_109);
          push_env w_109 split0_73;
          assert_env_length w_109 2;
          push_env w_109 (Memo.from_constructor tag_Unit);
          assert_env_length w_109 3;
          ignore (env_call w_109 [] 1);
          w_109.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_109
      | c_74 when c_74 = tag_VQuote ->
          let splits_74 = Memo.splits (snd x_74) in
          let split0_74 = List.nth splits_74 0 in
          ignore (pop_env w_109);
          push_env w_109 split0_74;
          assert_env_length w_109 2;
          push_env w_109 (Memo.from_constructor tag_Unit);
          assert_env_length w_109 3;
          ignore (env_call w_109 [] 1);
          w_109.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_109
      | c_74 when c_74 = tag_VNIL ->
          ignore (pop_env w_109);
          assert_env_length w_109 1;
          push_env w_109 (Memo.from_constructor tag_Unit);
          assert_env_length w_109 2;
          ignore (env_call w_109 [] 1);
          w_109.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_109
      | c_74 when c_74 = tag_VCons ->
          let splits_75 = Memo.splits (snd x_74) in
          let split0_75 = List.nth splits_75 0 in
          let split1_39 = List.nth splits_75 1 in
          ignore (pop_env w_109);
          push_env w_109 split0_75;
          push_env w_109 split1_39;
          assert_env_length w_109 3;
          push_env w_109 (Memo.from_constructor tag_Unit);
          assert_env_length w_109 4;
          ignore (env_call w_109 [] 1);
          w_109.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_109
      | c_74 when c_74 = tag_VClosure ->
          let splits_76 = Memo.splits (snd x_74) in
          let split0_76 = List.nth splits_76 0 in
          let split1_40 = List.nth splits_76 1 in
          ignore (pop_env w_109);
          push_env w_109 split0_76;
          push_env w_109 split1_40;
          assert_env_length w_109 3;
          push_env w_109 (Memo.from_constructor tag_Unit);
          assert_env_length w_109 4;
          ignore (env_call w_109 [] 1);
          w_109.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_109
      | c_74 -> failwith ("unreachable:" ^ string_of_int c_74 ^ "(110)"))
    110

let () =
  add_exp
    (fun w_110 ->
      assert_env_length w_110 1;
      push_env w_110 (Dynarray.get w_110.state.e 0);
      w_110.state.c <- pc_to_exp (int_to_pc 113);
      stepped w_110)
    111

let () =
  add_exp
    (fun w_112 ->
      assert_env_length w_112 3;
      let last_76 = Source.E 2 in
      let x_76 = resolve w_112 last_76 in
      match Word.get_value (fst x_76) with
      | c_76 when c_76 = tag_ECons ->
          let splits_80 = Memo.splits (snd x_76) in
          let split0_80 = List.nth splits_80 0 in
          let split1_41 = List.nth splits_80 1 in
          ignore (pop_env w_112);
          push_env w_112 split0_80;
          push_env w_112 split1_41;
          assert_env_length w_112 4;
          push_env w_112 (Memo.from_constructor tag_Unit);
          assert_env_length w_112 5;
          ignore (env_call w_112 [] 1);
          w_112.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_112
      | _ ->
          ignore (pop_env w_112);
          assert_env_length w_112 2;
          push_env w_112 (Memo.from_constructor tag_Unit);
          assert_env_length w_112 3;
          ignore (env_call w_112 [] 1);
          w_112.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_112
      | c_76 -> failwith ("unreachable:" ^ string_of_int c_76 ^ "(112)"))
    112

let () =
  add_exp
    (fun w_111 ->
      assert_env_length w_111 2;
      let last_75 = Source.E 1 in
      let x_75 = resolve w_111 last_75 in
      match Word.get_value (fst x_75) with
      | c_75 when c_75 = tag_VNumber ->
          let splits_77 = Memo.splits (snd x_75) in
          let split0_77 = List.nth splits_77 0 in
          ignore (pop_env w_111);
          push_env w_111 split0_77;
          assert_env_length w_111 2;
          push_env w_111 (Memo.from_constructor tag_Unit);
          assert_env_length w_111 3;
          ignore (env_call w_111 [] 1);
          w_111.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_111
      | c_75 when c_75 = tag_VSymbol ->
          let splits_78 = Memo.splits (snd x_75) in
          let split0_78 = List.nth splits_78 0 in
          ignore (pop_env w_111);
          push_env w_111 split0_78;
          assert_env_length w_111 2;
          push_env w_111 (Memo.from_constructor tag_Unit);
          assert_env_length w_111 3;
          ignore (env_call w_111 [] 1);
          w_111.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_111
      | c_75 when c_75 = tag_VQuote ->
          let splits_79 = Memo.splits (snd x_75) in
          let split0_79 = List.nth splits_79 0 in
          ignore (pop_env w_111);
          push_env w_111 split0_79;
          assert_env_length w_111 2;
          push_env w_111 (Dynarray.get w_111.state.e 1);
          w_111.state.c <- pc_to_exp (int_to_pc 112);
          stepped w_111
      | c_75 when c_75 = tag_VNIL ->
          ignore (pop_env w_111);
          assert_env_length w_111 1;
          push_env w_111 (Memo.from_constructor tag_Unit);
          assert_env_length w_111 2;
          ignore (env_call w_111 [] 1);
          w_111.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_111
      | c_75 when c_75 = tag_VCons ->
          let splits_81 = Memo.splits (snd x_75) in
          let split0_81 = List.nth splits_81 0 in
          let split1_42 = List.nth splits_81 1 in
          ignore (pop_env w_111);
          push_env w_111 split0_81;
          push_env w_111 split1_42;
          assert_env_length w_111 3;
          push_env w_111 (Memo.from_constructor tag_Unit);
          assert_env_length w_111 4;
          ignore (env_call w_111 [] 1);
          w_111.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_111
      | c_75 when c_75 = tag_VClosure ->
          let splits_82 = Memo.splits (snd x_75) in
          let split0_82 = List.nth splits_82 0 in
          let split1_43 = List.nth splits_82 1 in
          ignore (pop_env w_111);
          push_env w_111 split0_82;
          push_env w_111 split1_43;
          assert_env_length w_111 3;
          push_env w_111 (Memo.from_constructor tag_Unit);
          assert_env_length w_111 4;
          ignore (env_call w_111 [] 1);
          w_111.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_111
      | c_75 -> failwith ("unreachable:" ^ string_of_int c_75 ^ "(113)"))
    113

let () =
  add_exp
    (fun w_113 ->
      assert_env_length w_113 1;
      push_env w_113 (Dynarray.get w_113.state.e 0);
      w_113.state.c <- pc_to_exp (int_to_pc 115);
      stepped w_113)
    114

let () =
  add_exp
    (fun w_114 ->
      assert_env_length w_114 2;
      let last_77 = Source.E 1 in
      let x_77 = resolve w_114 last_77 in
      match Word.get_value (fst x_77) with
      | c_77 when c_77 = tag_VNumber ->
          let splits_83 = Memo.splits (snd x_77) in
          let split0_83 = List.nth splits_83 0 in
          ignore (pop_env w_114);
          push_env w_114 split0_83;
          assert_env_length w_114 2;
          push_env w_114 (Memo.from_constructor tag_Unit);
          assert_env_length w_114 3;
          ignore (env_call w_114 [] 1);
          w_114.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_114
      | c_77 when c_77 = tag_VSymbol ->
          let splits_84 = Memo.splits (snd x_77) in
          let split0_84 = List.nth splits_84 0 in
          ignore (pop_env w_114);
          push_env w_114 split0_84;
          assert_env_length w_114 2;
          push_env w_114 (Memo.from_constructor tag_Unit);
          assert_env_length w_114 3;
          ignore (env_call w_114 [] 1);
          w_114.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_114
      | c_77 when c_77 = tag_VQuote ->
          let splits_85 = Memo.splits (snd x_77) in
          let split0_85 = List.nth splits_85 0 in
          ignore (pop_env w_114);
          push_env w_114 split0_85;
          assert_env_length w_114 2;
          push_env w_114 (Dynarray.get w_114.state.e 1);
          assert_env_length w_114 3;
          let keep_vals_4 = env_call w_114 [] 1 in
          w_114.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_vals_4; w_114.state.k ];
          w_114.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_114
      | c_77 when c_77 = tag_VNIL ->
          ignore (pop_env w_114);
          assert_env_length w_114 1;
          push_env w_114 (Memo.from_constructor tag_Unit);
          assert_env_length w_114 2;
          ignore (env_call w_114 [] 1);
          w_114.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_114
      | c_77 when c_77 = tag_VCons ->
          let splits_86 = Memo.splits (snd x_77) in
          let split0_86 = List.nth splits_86 0 in
          let split1_44 = List.nth splits_86 1 in
          ignore (pop_env w_114);
          push_env w_114 split0_86;
          push_env w_114 split1_44;
          assert_env_length w_114 3;
          push_env w_114 (Memo.from_constructor tag_Unit);
          assert_env_length w_114 4;
          ignore (env_call w_114 [] 1);
          w_114.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_114
      | c_77 when c_77 = tag_VClosure ->
          let splits_87 = Memo.splits (snd x_77) in
          let split0_87 = List.nth splits_87 0 in
          let split1_45 = List.nth splits_87 1 in
          ignore (pop_env w_114);
          push_env w_114 split0_87;
          push_env w_114 split1_45;
          assert_env_length w_114 3;
          push_env w_114 (Memo.from_constructor tag_Unit);
          assert_env_length w_114 4;
          ignore (env_call w_114 [] 1);
          w_114.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_114
      | c_77 -> failwith ("unreachable:" ^ string_of_int c_77 ^ "(115)"))
    115

let () =
  add_exp
    (fun w_115 ->
      assert_env_length w_115 2;
      push_env w_115 (Dynarray.get w_115.state.e 0);
      assert_env_length w_115 3;
      push_env w_115 (Dynarray.get w_115.state.e 1);
      assert_env_length w_115 4;
      let keep_vals_5 = env_call w_115 [] 2 in
      w_115.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_vals_5; w_115.state.k ];
      w_115.state.c <- pc_to_exp (int_to_pc 96);
      stepped w_115)
    116

let () =
  add_exp
    (fun w_116 ->
      assert_env_length w_116 2;
      push_env w_116 (Dynarray.get w_116.state.e 1);
      assert_env_length w_116 3;
      let keep_vals_6 = env_call w_116 [ 0 ] 1 in
      w_116.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_vals_6; w_116.state.k ];
      w_116.state.c <- pc_to_exp (int_to_pc 26);
      stepped w_116)
    117

let () =
  add_exp
    (fun w_117 ->
      assert_env_length w_117 3;
      push_env w_117 (Dynarray.get w_117.state.e 0);
      w_117.state.c <- pc_to_exp (int_to_pc 120);
      stepped w_117)
    118

let () =
  add_exp
    (fun w_119 ->
      assert_env_length w_119 6;
      let last_79 = Source.E 5 in
      let x_79 = resolve w_119 last_79 in
      match Word.get_value (fst x_79) with
      | c_79 when c_79 = tag_Nil ->
          ignore (pop_env w_119);
          failwith "pairlis: arguments too few"
      | c_79 when c_79 = tag_Cons ->
          let splits_89 = Memo.splits (snd x_79) in
          let split0_89 = List.nth splits_89 0 in
          let split1_47 = List.nth splits_89 1 in
          ignore (pop_env w_119);
          push_env w_119 split0_89;
          push_env w_119 split1_47;
          assert_env_length w_119 7;
          push_env w_119 (Dynarray.get w_119.state.e 4);
          assert_env_length w_119 8;
          push_env w_119 (Dynarray.get w_119.state.e 6);
          assert_env_length w_119 9;
          push_env w_119 (Dynarray.get w_119.state.e 3);
          assert_env_length w_119 10;
          push_env w_119 (Dynarray.get w_119.state.e 5);
          assert_env_length w_119 11;
          let ctor_arg_12 = pop_env w_119 in
          let ctor_arg_13 = pop_env w_119 in
          push_env w_119 (Memo.appends [ Memo.from_constructor tag_EnvEntry; ctor_arg_13; ctor_arg_12 ]);
          assert_env_length w_119 10;
          push_env w_119 (Dynarray.get w_119.state.e 2);
          assert_env_length w_119 11;
          let keep_vals_7 = env_call w_119 [ 7; 8 ] 2 in
          w_119.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_vals_7; w_119.state.k ];
          w_119.state.c <- pc_to_exp (int_to_pc 28);
          stepped w_119
      | c_79 -> failwith ("unreachable:" ^ string_of_int c_79 ^ "(119)"))
    119

let () =
  add_exp
    (fun w_118 ->
      assert_env_length w_118 4;
      let last_78 = Source.E 3 in
      let x_78 = resolve w_118 last_78 in
      match Word.get_value (fst x_78) with
      | c_78 when c_78 = tag_Nil ->
          ignore (pop_env w_118);
          assert_env_length w_118 3;
          push_env w_118 (Dynarray.get w_118.state.e 2);
          assert_env_length w_118 4;
          return_n w_118 4 (pc_to_exp (int_to_pc 0))
      | c_78 when c_78 = tag_Cons ->
          let splits_88 = Memo.splits (snd x_78) in
          let split0_88 = List.nth splits_88 0 in
          let split1_46 = List.nth splits_88 1 in
          ignore (pop_env w_118);
          push_env w_118 split0_88;
          push_env w_118 split1_46;
          assert_env_length w_118 5;
          push_env w_118 (Dynarray.get w_118.state.e 1);
          w_118.state.c <- pc_to_exp (int_to_pc 119);
          stepped w_118
      | c_78 -> failwith ("unreachable:" ^ string_of_int c_78 ^ "(120)"))
    120

let () =
  add_exp
    (fun w_120 ->
      assert_env_length w_120 1;
      push_env w_120 (Dynarray.get w_120.state.e 0);
      w_120.state.c <- pc_to_exp (int_to_pc 123);
      stepped w_120)
    121

let () =
  add_exp
    (fun w_122 ->
      assert_env_length w_122 3;
      let last_81 = Source.E 2 in
      let x_81 = resolve w_122 last_81 in
      match Word.get_value (fst x_81) with
      | c_81 when c_81 = tag_ANIL ->
          ignore (pop_env w_122);
          assert_env_length w_122 2;
          push_env w_122 (Memo.from_constructor tag_Nil);
          assert_env_length w_122 3;
          drop_n w_122 3 1;
          assert_env_length w_122 2;
          return_n w_122 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          ignore (pop_env w_122);
          failwith "destruct_names: impossible"
      | c_81 -> failwith ("unreachable:" ^ string_of_int c_81 ^ "(122)"))
    122

let () =
  add_exp
    (fun w_121 ->
      assert_env_length w_121 2;
      let last_80 = Source.E 1 in
      let x_80 = resolve w_121 last_80 in
      match Word.get_value (fst x_80) with
      | c_80 when c_80 = tag_ECons ->
          let splits_90 = Memo.splits (snd x_80) in
          let split0_90 = List.nth splits_90 0 in
          let split1_48 = List.nth splits_90 1 in
          ignore (pop_env w_121);
          push_env w_121 split0_90;
          push_env w_121 split1_48;
          assert_env_length w_121 3;
          push_env w_121 (Dynarray.get w_121.state.e 1);
          assert_env_length w_121 4;
          let keep_vals_8 = env_call w_121 [ 2 ] 1 in
          w_121.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_vals_8; w_121.state.k ];
          w_121.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_121
      | c_80 when c_80 = tag_EAtom ->
          let splits_91 = Memo.splits (snd x_80) in
          let split0_91 = List.nth splits_91 0 in
          ignore (pop_env w_121);
          push_env w_121 split0_91;
          assert_env_length w_121 2;
          push_env w_121 (Dynarray.get w_121.state.e 1);
          w_121.state.c <- pc_to_exp (int_to_pc 122);
          stepped w_121
      | c_80 -> failwith ("unreachable:" ^ string_of_int c_80 ^ "(123)"))
    123

let () =
  add_exp
    (fun w_123 ->
      assert_env_length w_123 1;
      push_env w_123 (Dynarray.get w_123.state.e 0);
      w_123.state.c <- pc_to_exp (int_to_pc 125);
      stepped w_123)
    124

let () =
  add_exp
    (fun w_124 ->
      assert_env_length w_124 2;
      let last_82 = Source.E 1 in
      let x_82 = resolve w_124 last_82 in
      match Word.get_value (fst x_82) with
      | c_82 when c_82 = tag_VQuote ->
          let splits_92 = Memo.splits (snd x_82) in
          let split0_92 = List.nth splits_92 0 in
          ignore (pop_env w_124);
          push_env w_124 split0_92;
          assert_env_length w_124 2;
          push_env w_124 (Dynarray.get w_124.state.e 1);
          assert_env_length w_124 3;
          let keep_vals_9 = env_call w_124 [] 1 in
          w_124.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_vals_9; w_124.state.k ];
          w_124.state.c <- pc_to_exp (int_to_pc 19);
          stepped w_124
      | _ ->
          ignore (pop_env w_124);
          assert_env_length w_124 1;
          push_env w_124 (Memo.from_constructor tag_Unit);
          assert_env_length w_124 2;
          ignore (env_call w_124 [] 1);
          w_124.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_124
      | c_82 -> failwith ("unreachable:" ^ string_of_int c_82 ^ "(125)"))
    125

let () =
  add_exp
    (fun w_125 ->
      assert_env_length w_125 2;
      push_env w_125 (Dynarray.get w_125.state.e 0);
      w_125.state.c <- pc_to_exp (int_to_pc 129);
      stepped w_125)
    126

let () =
  add_exp
    (fun w_127 ->
      assert_env_length w_127 2;
      push_env w_127 (Dynarray.get w_127.state.e 0);
      w_127.state.c <- pc_to_exp (int_to_pc 130);
      stepped w_127)
    127

let () =
  add_exp
    (fun w_129 ->
      assert_env_length w_129 2;
      push_env w_129 (Dynarray.get w_129.state.e 0);
      w_129.state.c <- pc_to_exp (int_to_pc 133);
      stepped w_129)
    128

let () =
  add_exp
    (fun w_126 ->
      assert_env_length w_126 3;
      let last_83 = Source.E 2 in
      let x_83 = resolve w_126 last_83 in
      match Word.get_value (fst x_83) with
      | c_83 when c_83 = tag_ECons ->
          let splits_93 = Memo.splits (snd x_83) in
          let split0_93 = List.nth splits_93 0 in
          let split1_49 = List.nth splits_93 1 in
          ignore (pop_env w_126);
          push_env w_126 split0_93;
          push_env w_126 split1_49;
          assert_env_length w_126 4;
          push_env w_126 (Dynarray.get w_126.state.e 2);
          assert_env_length w_126 5;
          push_env w_126 (Dynarray.get w_126.state.e 1);
          assert_env_length w_126 6;
          let keep_vals_10 = env_call w_126 [ 1; 3 ] 2 in
          w_126.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_vals_10; w_126.state.k ];
          w_126.state.c <- pc_to_exp (int_to_pc 128);
          stepped w_126
      | _ ->
          ignore (pop_env w_126);
          assert_env_length w_126 2;
          push_env w_126 (Memo.from_constructor tag_Nil);
          assert_env_length w_126 3;
          return_n w_126 3 (pc_to_exp (int_to_pc 0))
      | c_83 -> failwith ("unreachable:" ^ string_of_int c_83 ^ "(129)"))
    129

let () =
  add_exp
    (fun w_128 ->
      assert_env_length w_128 3;
      let last_84 = Source.E 2 in
      let x_84 = resolve w_128 last_84 in
      match Word.get_value (fst x_84) with
      | c_84 when c_84 = tag_ECons ->
          let splits_94 = Memo.splits (snd x_84) in
          let split0_94 = List.nth splits_94 0 in
          let split1_50 = List.nth splits_94 1 in
          ignore (pop_env w_128);
          push_env w_128 split0_94;
          push_env w_128 split1_50;
          assert_env_length w_128 4;
          push_env w_128 (Dynarray.get w_128.state.e 2);
          assert_env_length w_128 5;
          let keep_vals_11 = env_call w_128 [ 1; 2; 3 ] 1 in
          w_128.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_vals_11; w_128.state.k ];
          w_128.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_128
      | _ ->
          ignore (pop_env w_128);
          failwith "no cond clause matched"
      | c_84 -> failwith ("unreachable:" ^ string_of_int c_84 ^ "(130)"))
    130

let () =
  add_exp
    (fun w_132 ->
      assert_env_length w_132 5;
      let last_87 = Source.E 4 in
      let x_87 = resolve w_132 last_87 in
      match Word.get_value (fst x_87) with
      | c_87 when c_87 = tag_STrue ->
          ignore (pop_env w_132);
          assert_env_length w_132 4;
          push_env w_132 (Memo.from_constructor tag_Unit);
          assert_env_length w_132 5;
          ignore (env_call w_132 [] 1);
          w_132.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_132
      | c_87 when c_87 = tag_SFalse ->
          ignore (pop_env w_132);
          assert_env_length w_132 4;
          push_env w_132 (Memo.from_constructor tag_Unit);
          assert_env_length w_132 5;
          ignore (env_call w_132 [] 1);
          w_132.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_132
      | _ ->
          ignore (pop_env w_132);
          failwith "cannot directly evaluate this symbol"
      | c_87 -> failwith ("unreachable:" ^ string_of_int c_87 ^ "(131)"))
    131

let () =
  add_exp
    (fun w_131 ->
      assert_env_length w_131 4;
      let last_86 = Source.E 3 in
      let x_86 = resolve w_131 last_86 in
      match Word.get_value (fst x_86) with
      | c_86 when c_86 = tag_AVar ->
          let splits_96 = Memo.splits (snd x_86) in
          let split0_96 = List.nth splits_96 0 in
          ignore (pop_env w_131);
          push_env w_131 split0_96;
          assert_env_length w_131 4;
          push_env w_131 (Dynarray.get w_131.state.e 3);
          assert_env_length w_131 5;
          push_env w_131 (Dynarray.get w_131.state.e 1);
          assert_env_length w_131 6;
          ignore (env_call w_131 [] 2);
          w_131.state.c <- pc_to_exp (int_to_pc 117);
          stepped w_131
      | c_86 when c_86 = tag_ANumber ->
          let splits_97 = Memo.splits (snd x_86) in
          let split0_97 = List.nth splits_97 0 in
          ignore (pop_env w_131);
          push_env w_131 split0_97;
          assert_env_length w_131 4;
          push_env w_131 (Dynarray.get w_131.state.e 3);
          assert_env_length w_131 5;
          let ctor_arg_14 = pop_env w_131 in
          push_env w_131 (Memo.appends [ Memo.from_constructor tag_VNumber; ctor_arg_14 ]);
          assert_env_length w_131 5;
          drop_n w_131 5 1;
          assert_env_length w_131 4;
          drop_n w_131 4 1;
          assert_env_length w_131 3;
          return_n w_131 3 (pc_to_exp (int_to_pc 0))
      | c_86 when c_86 = tag_ASymbol ->
          let splits_98 = Memo.splits (snd x_86) in
          let split0_98 = List.nth splits_98 0 in
          ignore (pop_env w_131);
          push_env w_131 split0_98;
          assert_env_length w_131 4;
          push_env w_131 (Dynarray.get w_131.state.e 3);
          w_131.state.c <- pc_to_exp (int_to_pc 131);
          stepped w_131
      | c_86 when c_86 = tag_ANIL ->
          ignore (pop_env w_131);
          failwith "ill-formed expression: NIL"
      | c_86 -> failwith ("unreachable:" ^ string_of_int c_86 ^ "(132)"))
    132

let () =
  add_exp
    (fun w_130 ->
      assert_env_length w_130 3;
      let last_85 = Source.E 2 in
      let x_85 = resolve w_130 last_85 in
      match Word.get_value (fst x_85) with
      | c_85 when c_85 = tag_EAtom ->
          let splits_95 = Memo.splits (snd x_85) in
          let split0_95 = List.nth splits_95 0 in
          ignore (pop_env w_130);
          push_env w_130 split0_95;
          assert_env_length w_130 3;
          push_env w_130 (Dynarray.get w_130.state.e 2);
          w_130.state.c <- pc_to_exp (int_to_pc 132);
          stepped w_130
      | c_85 when c_85 = tag_ECons ->
          let splits_99 = Memo.splits (snd x_85) in
          let split0_99 = List.nth splits_99 0 in
          let split1_51 = List.nth splits_99 1 in
          ignore (pop_env w_130);
          push_env w_130 split0_99;
          push_env w_130 split1_51;
          assert_env_length w_130 4;
          push_env w_130 (Dynarray.get w_130.state.e 0);
          assert_env_length w_130 5;
          let keep_vals_12 = env_call w_130 [ 0; 1 ] 1 in
          w_130.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_vals_12; w_130.state.k ];
          w_130.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_130
      | c_85 -> failwith ("unreachable:" ^ string_of_int c_85 ^ "(133)"))
    133

let () =
  add_exp
    (fun w_135 ->
      assert_env_length w_135 4;
      let x0_4 = resolve w_135 (Source.E 2) in
      let x1_4 = resolve w_135 (Source.E 3) in
      ignore (pop_env w_135);
      ignore (pop_env w_135);
      push_env w_135 (Memo.from_int (if Word.get_value (fst x0_4) = Word.get_value (fst x1_4) then 1 else 0));
      assert_env_length w_135 3;
      drop_n w_135 3 1;
      assert_env_length w_135 2;
      drop_n w_135 2 0;
      assert_env_length w_135 2;
      drop_n w_135 2 1;
      assert_env_length w_135 1;
      return_n w_135 1 (pc_to_exp (int_to_pc 0)))
    134

let () =
  add_exp
    (fun w_134 ->
      assert_env_length w_134 2;
      let last_88 = Source.E 1 in
      let x_88 = resolve w_134 last_88 in
      match Word.get_value (fst x_88) with
      | c_88 when c_88 = tag_None ->
          ignore (pop_env w_134);
          assert_env_length w_134 1;
          push_env w_134 (Memo.from_int 0);
          assert_env_length w_134 2;
          drop_n w_134 2 0;
          assert_env_length w_134 2;
          drop_n w_134 2 1;
          assert_env_length w_134 1;
          return_n w_134 1 (pc_to_exp (int_to_pc 0))
      | c_88 when c_88 = tag_Some ->
          let splits_100 = Memo.splits (snd x_88) in
          let split0_100 = List.nth splits_100 0 in
          ignore (pop_env w_134);
          push_env w_134 split0_100;
          assert_env_length w_134 2;
          push_env w_134 (Dynarray.get w_134.state.e 0);
          assert_env_length w_134 3;
          push_env w_134 (Dynarray.get w_134.state.e 1);
          w_134.state.c <- pc_to_exp (int_to_pc 134);
          stepped w_134
      | c_88 -> failwith ("unreachable:" ^ string_of_int c_88 ^ "(135)"))
    135

let () =
  add_exp
    (fun w_137 ->
      assert_env_length w_137 4;
      let x0_5 = resolve w_137 (Source.E 2) in
      let x1_5 = resolve w_137 (Source.E 3) in
      ignore (pop_env w_137);
      ignore (pop_env w_137);
      push_env w_137 (Memo.from_int (if Word.get_value (fst x0_5) = Word.get_value (fst x1_5) then 1 else 0));
      assert_env_length w_137 3;
      drop_n w_137 3 1;
      assert_env_length w_137 2;
      drop_n w_137 2 1;
      assert_env_length w_137 1;
      drop_n w_137 1 0;
      assert_env_length w_137 1;
      return_n w_137 1 (pc_to_exp (int_to_pc 0)))
    136

let () =
  add_exp
    (fun w_136 ->
      assert_env_length w_136 2;
      let last_89 = Source.E 1 in
      let x_89 = resolve w_136 last_89 in
      match Word.get_value (fst x_89) with
      | c_89 when c_89 = tag_None ->
          ignore (pop_env w_136);
          assert_env_length w_136 1;
          push_env w_136 (Memo.from_int 0);
          assert_env_length w_136 2;
          drop_n w_136 2 1;
          assert_env_length w_136 1;
          drop_n w_136 1 0;
          assert_env_length w_136 1;
          return_n w_136 1 (pc_to_exp (int_to_pc 0))
      | c_89 when c_89 = tag_Some ->
          let splits_101 = Memo.splits (snd x_89) in
          let split0_101 = List.nth splits_101 0 in
          ignore (pop_env w_136);
          push_env w_136 split0_101;
          assert_env_length w_136 2;
          push_env w_136 (Dynarray.get w_136.state.e 0);
          assert_env_length w_136 3;
          push_env w_136 (Dynarray.get w_136.state.e 1);
          w_136.state.c <- pc_to_exp (int_to_pc 136);
          stepped w_136
      | c_89 -> failwith ("unreachable:" ^ string_of_int c_89 ^ "(137)"))
    137

let () =
  add_exp
    (fun w_138 ->
      assert_env_length w_138 1;
      let last_90 = Source.E 0 in
      let x_90 = resolve w_138 last_90 in
      match Word.get_value (fst x_90) with
      | c_90 when c_90 = tag_Some ->
          let splits_102 = Memo.splits (snd x_90) in
          let split0_102 = List.nth splits_102 0 in
          ignore (pop_env w_138);
          push_env w_138 split0_102;
          assert_env_length w_138 1;
          push_env w_138 (Memo.from_constructor tag_Unit);
          assert_env_length w_138 2;
          ignore (env_call w_138 [] 1);
          w_138.state.c <- pc_to_exp (int_to_pc 25);
          stepped w_138
      | _ ->
          ignore (pop_env w_138);
          assert_env_length w_138 0;
          push_env w_138 (Memo.from_constructor tag_Unit);
          assert_env_length w_138 1;
          ignore (env_call w_138 [] 1);
          w_138.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_138
      | c_90 -> failwith ("unreachable:" ^ string_of_int c_90 ^ "(138)"))
    138

let () =
  add_exp
    (fun w_139 ->
      assert_env_length w_139 1;
      let cond_0 = resolve w_139 (Source.E 0) in
      ignore (pop_env w_139);
      let if_kont_0 =
       fun _ ->
        assert_env_length w_139 1;
        return_n w_139 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_139 0;
        push_env w_139 (Memo.from_constructor tag_Unit);
        assert_env_length w_139 1;
        ignore (env_call w_139 [] 1);
        w_139.state.c <- pc_to_exp (int_to_pc 25);
        stepped w_139)
      else (
        assert_env_length w_139 0;
        push_env w_139 (Memo.from_constructor tag_Unit);
        assert_env_length w_139 1;
        ignore (env_call w_139 [] 1);
        w_139.state.c <- pc_to_exp (int_to_pc 24);
        stepped w_139))
    139

let () =
  add_exp
    (fun w_140 ->
      assert_env_length w_140 2;
      let last_91 = Source.E 1 in
      let x_91 = resolve w_140 last_91 in
      match Word.get_value (fst x_91) with
      | c_91 when c_91 = tag_Nil ->
          ignore (pop_env w_140);
          failwith "empty environment"
      | c_91 when c_91 = tag_Cons ->
          let splits_103 = Memo.splits (snd x_91) in
          let split0_103 = List.nth splits_103 0 in
          let split1_52 = List.nth splits_103 1 in
          ignore (pop_env w_140);
          push_env w_140 split0_103;
          push_env w_140 split1_52;
          assert_env_length w_140 3;
          push_env w_140 (Dynarray.get w_140.state.e 1);
          assert_env_length w_140 4;
          let keep_vals_13 = env_call w_140 [ 0; 1; 2 ] 1 in
          w_140.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_vals_13; w_140.state.k ];
          w_140.state.c <- pc_to_exp (int_to_pc 6);
          stepped w_140
      | c_91 -> failwith ("unreachable:" ^ string_of_int c_91 ^ "(140)"))
    140

let () =
  add_exp
    (fun w_141 ->
      assert_env_length w_141 2;
      let last_92 = Source.E 1 in
      let x_92 = resolve w_141 last_92 in
      match Word.get_value (fst x_92) with
      | c_92 when c_92 = tag_Some ->
          let splits_104 = Memo.splits (snd x_92) in
          let split0_104 = List.nth splits_104 0 in
          ignore (pop_env w_141);
          push_env w_141 split0_104;
          assert_env_length w_141 2;
          push_env w_141 (Dynarray.get w_141.state.e 1);
          assert_env_length w_141 3;
          push_env w_141 (Dynarray.get w_141.state.e 0);
          assert_env_length w_141 4;
          let keep_vals_14 = env_call w_141 [ 2 ] 1 in
          w_141.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_vals_14; w_141.state.k ];
          w_141.state.c <- pc_to_exp (int_to_pc 121);
          stepped w_141
      | _ ->
          ignore (pop_env w_141);
          failwith "destruct_names: impossible, names must be int literals"
      | c_92 -> failwith ("unreachable:" ^ string_of_int c_92 ^ "(141)"))
    141

let () =
  add_exp
    (fun w_142 ->
      assert_env_length w_142 1;
      let cond_1 = resolve w_142 (Source.E 0) in
      ignore (pop_env w_142);
      let if_kont_1 =
       fun _ ->
        assert_env_length w_142 1;
        drop_n w_142 1 0;
        assert_env_length w_142 1;
        return_n w_142 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_142 0;
        push_env w_142 (Memo.from_constructor tag_Unit);
        assert_env_length w_142 1;
        ignore (env_call w_142 [] 1);
        w_142.state.c <- pc_to_exp (int_to_pc 25);
        stepped w_142)
      else (
        assert_env_length w_142 0;
        push_env w_142 (Memo.from_constructor tag_Unit);
        assert_env_length w_142 1;
        ignore (env_call w_142 [] 1);
        w_142.state.c <- pc_to_exp (int_to_pc 24);
        stepped w_142))
    142

let () =
  add_exp
    (fun w_144 ->
      assert_env_length w_144 4;
      let cond_2 = resolve w_144 (Source.E 3) in
      ignore (pop_env w_144);
      let if_kont_2 =
       fun _ ->
        assert_env_length w_144 4;
        drop_n w_144 4 2;
        assert_env_length w_144 2;
        return_n w_144 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_144 3;
        push_env w_144 (Dynarray.get w_144.state.e 1);
        assert_env_length w_144 4;
        ignore (env_call w_144 [] 1);
        w_144.state.c <- pc_to_exp (int_to_pc 8);
        stepped w_144)
      else (
        assert_env_length w_144 3;
        push_env w_144 (Dynarray.get w_144.state.e 0);
        assert_env_length w_144 4;
        push_env w_144 (Dynarray.get w_144.state.e 2);
        assert_env_length w_144 5;
        let ctor_arg_17 = pop_env w_144 in
        push_env w_144 (Memo.appends [ Memo.from_constructor tag_MkEnv; ctor_arg_17 ]);
        assert_env_length w_144 5;
        ignore (env_call w_144 [] 2);
        w_144.state.c <- pc_to_exp (int_to_pc 117);
        stepped w_144))
    143

let () =
  add_exp
    (fun w_143 ->
      assert_env_length w_143 5;
      let x0_6 = resolve w_143 (Source.E 3) in
      let x1_6 = resolve w_143 (Source.E 4) in
      ignore (pop_env w_143);
      ignore (pop_env w_143);
      push_env w_143 (Memo.from_int (if Word.get_value (fst x0_6) = Word.get_value (fst x1_6) then 1 else 0));
      w_143.state.c <- pc_to_exp (int_to_pc 143);
      stepped w_143)
    144

let () =
  add_exp
    (fun w_146 ->
      assert_env_length w_146 4;
      let last_94 = Source.E 3 in
      let x_94 = resolve w_146 last_94 in
      match Word.get_value (fst x_94) with
      | c_94 when c_94 = tag_SQuote ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_19 = env_call w_146 [] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_20; keep_vals_19; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SAtom ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_20 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_21; keep_vals_20; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SPair ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_21 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_22; keep_vals_21; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SSymbol ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_22 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_23; keep_vals_22; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SEq ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_23 = env_call w_146 [ 0; 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_24; keep_vals_23; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SCar ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_24 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_25; keep_vals_24; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SCdr ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_25 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_26; keep_vals_25; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SIf ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_26 = env_call w_146 [ 0; 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_27; keep_vals_26; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SCons ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_27 = env_call w_146 [ 0; 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_28; keep_vals_27; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_SCond ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_28 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_29; keep_vals_28; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 63);
          stepped w_146
      | c_94 when c_94 = tag_SNull ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_29 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_30; keep_vals_29; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | c_94 when c_94 = tag_STrue ->
          ignore (pop_env w_146);
          failwith "true is not PROCEDURE"
      | c_94 when c_94 = tag_SFalse ->
          ignore (pop_env w_146);
          failwith "false is not PROCEDURE"
      | c_94 when c_94 = tag_SVar ->
          ignore (pop_env w_146);
          failwith "eval: symbol var is not intended to be used this way"
      | c_94 when c_94 = tag_SError ->
          ignore (pop_env w_146);
          assert_env_length w_146 3;
          push_env w_146 (Dynarray.get w_146.state.e 0);
          assert_env_length w_146 4;
          let keep_vals_30 = env_call w_146 [ 1 ] 1 in
          w_146.state.k <- Memo.appends [ Memo.from_constructor tag_cont_31; keep_vals_30; w_146.state.k ];
          w_146.state.c <- pc_to_exp (int_to_pc 39);
          stepped w_146
      | _ ->
          ignore (pop_env w_146);
          failwith "invalid symbol here"
      | c_94 -> failwith ("unreachable:" ^ string_of_int c_94 ^ "(145)"))
    145

let () =
  add_exp
    (fun w_145 ->
      assert_env_length w_145 3;
      let last_93 = Source.E 2 in
      let x_93 = resolve w_145 last_93 in
      match Word.get_value (fst x_93) with
      | c_93 when c_93 = tag_Some ->
          let splits_105 = Memo.splits (snd x_93) in
          let split0_105 = List.nth splits_105 0 in
          ignore (pop_env w_145);
          push_env w_145 split0_105;
          assert_env_length w_145 3;
          push_env w_145 (Dynarray.get w_145.state.e 2);
          w_145.state.c <- pc_to_exp (int_to_pc 145);
          stepped w_145
      | c_93 when c_93 = tag_None ->
          ignore (pop_env w_145);
          assert_env_length w_145 2;
          push_env w_145 (Dynarray.get w_145.state.e 0);
          assert_env_length w_145 3;
          let keep_vals_31 = env_call w_145 [ 0; 1 ] 1 in
          w_145.state.k <- Memo.appends [ Memo.from_constructor tag_cont_32; keep_vals_31; w_145.state.k ];
          w_145.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_145
      | c_93 -> failwith ("unreachable:" ^ string_of_int c_93 ^ "(146)"))
    146

let () =
  add_exp
    (fun w_147 ->
      assert_env_length w_147 3;
      let last_95 = Source.E 2 in
      let x_95 = resolve w_147 last_95 in
      match Word.get_value (fst x_95) with
      | c_95 when c_95 = tag_ECons ->
          let splits_106 = Memo.splits (snd x_95) in
          let split0_106 = List.nth splits_106 0 in
          let split1_53 = List.nth splits_106 1 in
          ignore (pop_env w_147);
          push_env w_147 split0_106;
          push_env w_147 split1_53;
          assert_env_length w_147 4;
          push_env w_147 (Dynarray.get w_147.state.e 0);
          assert_env_length w_147 5;
          let keep_vals_43 = env_call w_147 [ 0; 1 ] 1 in
          w_147.state.k <- Memo.appends [ Memo.from_constructor tag_cont_44; keep_vals_43; w_147.state.k ];
          w_147.state.c <- pc_to_exp (int_to_pc 42);
          stepped w_147
      | _ ->
          ignore (pop_env w_147);
          assert_env_length w_147 2;
          push_env w_147 (Dynarray.get w_147.state.e 0);
          assert_env_length w_147 3;
          let keep_vals_44 = env_call w_147 [ 0; 1 ] 1 in
          w_147.state.k <- Memo.appends [ Memo.from_constructor tag_cont_45; keep_vals_44; w_147.state.k ];
          w_147.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_147
      | c_95 -> failwith ("unreachable:" ^ string_of_int c_95 ^ "(147)"))
    147

let () =
  add_exp
    (fun w_148 ->
      assert_env_length w_148 4;
      let cond_3 = resolve w_148 (Source.E 3) in
      ignore (pop_env w_148);
      let if_kont_3 =
       fun _ ->
        assert_env_length w_148 4;
        drop_n w_148 4 2;
        assert_env_length w_148 2;
        return_n w_148 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_148 3;
        push_env w_148 (Dynarray.get w_148.state.e 2);
        assert_env_length w_148 4;
        push_env w_148 (Dynarray.get w_148.state.e 0);
        assert_env_length w_148 5;
        ignore (env_call w_148 [] 2);
        w_148.state.c <- pc_to_exp (int_to_pc 127);
        stepped w_148)
      else (
        assert_env_length w_148 3;
        push_env w_148 (Dynarray.get w_148.state.e 1);
        assert_env_length w_148 4;
        let keep_vals_45 = env_call w_148 [ 0 ] 1 in
        w_148.state.k <- Memo.appends [ Memo.from_constructor tag_cont_46; keep_vals_45; w_148.state.k ];
        w_148.state.c <- pc_to_exp (int_to_pc 39);
        stepped w_148))
    148

let () =
  add_exp
    (fun w_150 ->
      let err_0 = resolve w_150 (Source.E 1) in
      ignore (pop_env w_150);
      failwith (string_of_int (Word.get_value (fst err_0))))
    149

let () =
  add_exp
    (fun w_149 ->
      assert_env_length w_149 1;
      let last_96 = Source.E 0 in
      let x_96 = resolve w_149 last_96 in
      match Word.get_value (fst x_96) with
      | c_96 when c_96 = tag_None ->
          ignore (pop_env w_149);
          failwith "eval: user error code must evalutes to integer"
      | c_96 when c_96 = tag_Some ->
          let splits_107 = Memo.splits (snd x_96) in
          let split0_107 = List.nth splits_107 0 in
          ignore (pop_env w_149);
          push_env w_149 split0_107;
          assert_env_length w_149 1;
          push_env w_149 (Dynarray.get w_149.state.e 0);
          assert_env_length w_149 2;
          drop_n w_149 2 1;
          assert_env_length w_149 1;
          push_env w_149 (Dynarray.get w_149.state.e 0);
          w_149.state.c <- pc_to_exp (int_to_pc 149);
          stepped w_149
      | c_96 -> failwith ("unreachable:" ^ string_of_int c_96 ^ "(150)"))
    150

let () =
  add_exp
    (fun w_152 ->
      assert_env_length w_152 4;
      let last_98 = Source.E 3 in
      let x_98 = resolve w_152 last_98 in
      match Word.get_value (fst x_98) with
      | c_98 when c_98 = tag_SLambda ->
          ignore (pop_env w_152);
          assert_env_length w_152 3;
          push_env w_152 (Dynarray.get w_152.state.e 0);
          assert_env_length w_152 4;
          let keep_vals_55 = env_call w_152 [ 0; 1 ] 1 in
          w_152.state.k <- Memo.appends [ Memo.from_constructor tag_cont_56; keep_vals_55; w_152.state.k ];
          w_152.state.c <- pc_to_exp (int_to_pc 52);
          stepped w_152
      | c_98 when c_98 = tag_SDefine ->
          ignore (pop_env w_152);
          assert_env_length w_152 3;
          push_env w_152 (Memo.from_constructor tag_SLambda);
          assert_env_length w_152 4;
          let ctor_arg_23 = pop_env w_152 in
          push_env w_152 (Memo.appends [ Memo.from_constructor tag_ASymbol; ctor_arg_23 ]);
          assert_env_length w_152 4;
          let ctor_arg_24 = pop_env w_152 in
          push_env w_152 (Memo.appends [ Memo.from_constructor tag_EAtom; ctor_arg_24 ]);
          assert_env_length w_152 4;
          push_env w_152 (Dynarray.get w_152.state.e 0);
          assert_env_length w_152 5;
          let keep_vals_56 = env_call w_152 [ 0; 1; 3 ] 1 in
          w_152.state.k <- Memo.appends [ Memo.from_constructor tag_cont_57; keep_vals_56; w_152.state.k ];
          w_152.state.c <- pc_to_exp (int_to_pc 48);
          stepped w_152
      | c_98 when c_98 = tag_SDefvar ->
          ignore (pop_env w_152);
          assert_env_length w_152 3;
          push_env w_152 (Dynarray.get w_152.state.e 0);
          assert_env_length w_152 4;
          let keep_vals_57 = env_call w_152 [ 0; 1 ] 1 in
          w_152.state.k <- Memo.appends [ Memo.from_constructor tag_cont_58; keep_vals_57; w_152.state.k ];
          w_152.state.c <- pc_to_exp (int_to_pc 56);
          stepped w_152
      | c_98 when c_98 = tag_SQuote ->
          ignore (pop_env w_152);
          failwith "unexpected function quote"
      | c_98 when c_98 = tag_SEq ->
          ignore (pop_env w_152);
          failwith "unexpected function eq"
      | c_98 when c_98 = tag_SCons ->
          ignore (pop_env w_152);
          failwith "unexpected function cons"
      | c_98 when c_98 = tag_SIf ->
          ignore (pop_env w_152);
          failwith "unexpected function if"
      | c_98 when c_98 = tag_SCond ->
          ignore (pop_env w_152);
          failwith "unexpected function cond"
      | c_98 when c_98 = tag_SAtom ->
          ignore (pop_env w_152);
          failwith "unexpected function atom"
      | c_98 when c_98 = tag_SPair ->
          ignore (pop_env w_152);
          failwith "unexpected function pair"
      | c_98 when c_98 = tag_SSymbol ->
          ignore (pop_env w_152);
          failwith "unexpected function symbol"
      | c_98 when c_98 = tag_STrue ->
          ignore (pop_env w_152);
          failwith "unexpected function true"
      | c_98 when c_98 = tag_SFalse ->
          ignore (pop_env w_152);
          failwith "unexpected function false"
      | c_98 when c_98 = tag_SCar ->
          ignore (pop_env w_152);
          failwith "unexpected function car"
      | c_98 when c_98 = tag_SCdr ->
          ignore (pop_env w_152);
          failwith "unexpected function cdr"
      | c_98 when c_98 = tag_SNull ->
          ignore (pop_env w_152);
          failwith "unexpected function null"
      | c_98 when c_98 = tag_SError ->
          ignore (pop_env w_152);
          failwith "unexpected function error"
      | c_98 when c_98 = tag_SVar ->
          ignore (pop_env w_152);
          failwith "eval: symbol var is not intended to be used this way"
      | _ ->
          ignore (pop_env w_152);
          failwith "1"
      | c_98 -> failwith ("unreachable:" ^ string_of_int c_98 ^ "(151)"))
    151

let () =
  add_exp
    (fun w_151 ->
      assert_env_length w_151 3;
      let last_97 = Source.E 2 in
      let x_97 = resolve w_151 last_97 in
      match Word.get_value (fst x_97) with
      | c_97 when c_97 = tag_Some ->
          let splits_108 = Memo.splits (snd x_97) in
          let split0_108 = List.nth splits_108 0 in
          ignore (pop_env w_151);
          push_env w_151 split0_108;
          assert_env_length w_151 3;
          push_env w_151 (Dynarray.get w_151.state.e 2);
          w_151.state.c <- pc_to_exp (int_to_pc 151);
          stepped w_151
      | c_97 when c_97 = tag_None ->
          ignore (pop_env w_151);
          assert_env_length w_151 2;
          push_env w_151 (Dynarray.get w_151.state.e 0);
          assert_env_length w_151 3;
          let keep_vals_58 = env_call w_151 [] 1 in
          w_151.state.k <- Memo.appends [ Memo.from_constructor tag_cont_59; keep_vals_58; w_151.state.k ];
          w_151.state.c <- pc_to_exp (int_to_pc 42);
          stepped w_151
      | c_97 -> failwith ("unreachable:" ^ string_of_int c_97 ^ "(152)"))
    152

let () =
  add_exp
    (fun w_153 ->
      assert_env_length w_153 3;
      let last_99 = Source.E 2 in
      let x_99 = resolve w_153 last_99 in
      match Word.get_value (fst x_99) with
      | c_99 when c_99 = tag_Some ->
          let splits_109 = Memo.splits (snd x_99) in
          let split0_109 = List.nth splits_109 0 in
          ignore (pop_env w_153);
          push_env w_153 split0_109;
          assert_env_length w_153 3;
          push_env w_153 (Dynarray.get w_153.state.e 2);
          assert_env_length w_153 4;
          push_env w_153 (Dynarray.get w_153.state.e 1);
          assert_env_length w_153 5;
          let keep_vals_59 = env_call w_153 [ 0; 1 ] 2 in
          w_153.state.k <- Memo.appends [ Memo.from_constructor tag_cont_60; keep_vals_59; w_153.state.k ];
          w_153.state.c <- pc_to_exp (int_to_pc 117);
          stepped w_153
      | c_99 when c_99 = tag_None ->
          ignore (pop_env w_153);
          assert_env_length w_153 2;
          push_env w_153 (Dynarray.get w_153.state.e 0);
          assert_env_length w_153 3;
          let keep_vals_60 = env_call w_153 [] 1 in
          w_153.state.k <- Memo.appends [ Memo.from_constructor tag_cont_61; keep_vals_60; w_153.state.k ];
          w_153.state.c <- pc_to_exp (int_to_pc 61);
          stepped w_153
      | c_99 -> failwith ("unreachable:" ^ string_of_int c_99 ^ "(153)"))
    153

let () =
  add_exp
    (fun w_155 ->
      assert_env_length w_155 2;
      let last_101 = Source.E 1 in
      let x_101 = resolve w_155 last_101 in
      match Word.get_value (fst x_101) with
      | c_101 when c_101 = tag_AVar ->
          let splits_111 = Memo.splits (snd x_101) in
          let split0_111 = List.nth splits_111 0 in
          ignore (pop_env w_155);
          push_env w_155 split0_111;
          failwith "unexpected function var"
      | c_101 when c_101 = tag_ANumber ->
          let splits_112 = Memo.splits (snd x_101) in
          let split0_112 = List.nth splits_112 0 in
          ignore (pop_env w_155);
          push_env w_155 split0_112;
          failwith "unexpected function number"
      | c_101 when c_101 = tag_ASymbol ->
          let splits_113 = Memo.splits (snd x_101) in
          let split0_113 = List.nth splits_113 0 in
          ignore (pop_env w_155);
          push_env w_155 split0_113;
          failwith "invalid symbol here"
      | c_101 when c_101 = tag_ANIL ->
          ignore (pop_env w_155);
          failwith "unexpected function nil"
      | c_101 -> failwith ("unreachable:" ^ string_of_int c_101 ^ "(154)"))
    154

let () =
  add_exp
    (fun w_154 ->
      assert_env_length w_154 1;
      let last_100 = Source.E 0 in
      let x_100 = resolve w_154 last_100 in
      match Word.get_value (fst x_100) with
      | c_100 when c_100 = tag_EAtom ->
          let splits_110 = Memo.splits (snd x_100) in
          let split0_110 = List.nth splits_110 0 in
          ignore (pop_env w_154);
          push_env w_154 split0_110;
          assert_env_length w_154 1;
          push_env w_154 (Dynarray.get w_154.state.e 0);
          w_154.state.c <- pc_to_exp (int_to_pc 154);
          stepped w_154
      | c_100 when c_100 = tag_ECons ->
          let splits_114 = Memo.splits (snd x_100) in
          let split0_114 = List.nth splits_114 0 in
          let split1_54 = List.nth splits_114 1 in
          ignore (pop_env w_154);
          push_env w_154 split0_114;
          push_env w_154 split1_54;
          failwith "unexpected CONS"
      | c_100 -> failwith ("unreachable:" ^ string_of_int c_100 ^ "(155)"))
    155

let () =
  add_exp
    (fun w_156 ->
      assert_env_length w_156 3;
      let last_102 = Source.E 2 in
      let x_102 = resolve w_156 last_102 in
      match Word.get_value (fst x_102) with
      | c_102 when c_102 = tag_VNumber ->
          let splits_115 = Memo.splits (snd x_102) in
          let split0_115 = List.nth splits_115 0 in
          ignore (pop_env w_156);
          push_env w_156 split0_115;
          failwith "NUMBER is not PROCEDURE"
      | c_102 when c_102 = tag_VSymbol ->
          let splits_116 = Memo.splits (snd x_102) in
          let split0_116 = List.nth splits_116 0 in
          ignore (pop_env w_156);
          push_env w_156 split0_116;
          failwith "SYMBOL is not PROCEDURE"
      | c_102 when c_102 = tag_VQuote ->
          let splits_117 = Memo.splits (snd x_102) in
          let split0_117 = List.nth splits_117 0 in
          ignore (pop_env w_156);
          push_env w_156 split0_117;
          failwith "QUOTE is not PROCEDURE"
      | c_102 when c_102 = tag_VNIL ->
          ignore (pop_env w_156);
          failwith "NIL is not PROCEDURE"
      | c_102 when c_102 = tag_VCons ->
          let splits_118 = Memo.splits (snd x_102) in
          let split0_118 = List.nth splits_118 0 in
          let split1_55 = List.nth splits_118 1 in
          ignore (pop_env w_156);
          push_env w_156 split0_118;
          push_env w_156 split1_55;
          failwith "PAIR is not PROCEDURE"
      | c_102 when c_102 = tag_VClosure ->
          let splits_119 = Memo.splits (snd x_102) in
          let split0_119 = List.nth splits_119 0 in
          let split1_56 = List.nth splits_119 1 in
          ignore (pop_env w_156);
          push_env w_156 split0_119;
          push_env w_156 split1_56;
          assert_env_length w_156 4;
          push_env w_156 (Dynarray.get w_156.state.e 3);
          assert_env_length w_156 5;
          push_env w_156 (Dynarray.get w_156.state.e 0);
          assert_env_length w_156 6;
          let keep_vals_65 = env_call w_156 [ 1; 4 ] 1 in
          w_156.state.k <- Memo.appends [ Memo.from_constructor tag_cont_66; keep_vals_65; w_156.state.k ];
          w_156.state.c <- pc_to_exp (int_to_pc 63);
          stepped w_156
      | c_102 -> failwith ("unreachable:" ^ string_of_int c_102 ^ "(156)"))
    156

let () =
  add_exp
    (fun w_158 ->
      assert_env_length w_158 2;
      let last_104 = Source.E 1 in
      let x_104 = resolve w_158 last_104 in
      match Word.get_value (fst x_104) with
      | c_104 when c_104 = tag_ANumber ->
          let splits_121 = Memo.splits (snd x_104) in
          let split0_121 = List.nth splits_121 0 in
          ignore (pop_env w_158);
          push_env w_158 split0_121;
          failwith "NUMBER is not PROCEDURE"
      | c_104 when c_104 = tag_ANIL ->
          ignore (pop_env w_158);
          failwith "NIL is not PROCEDURE"
      | _ ->
          ignore (pop_env w_158);
          failwith "impossible"
      | c_104 -> failwith ("unreachable:" ^ string_of_int c_104 ^ "(157)"))
    157

let () =
  add_exp
    (fun w_157 ->
      assert_env_length w_157 1;
      let last_103 = Source.E 0 in
      let x_103 = resolve w_157 last_103 in
      match Word.get_value (fst x_103) with
      | c_103 when c_103 = tag_EAtom ->
          let splits_120 = Memo.splits (snd x_103) in
          let split0_120 = List.nth splits_120 0 in
          ignore (pop_env w_157);
          push_env w_157 split0_120;
          assert_env_length w_157 1;
          push_env w_157 (Dynarray.get w_157.state.e 0);
          w_157.state.c <- pc_to_exp (int_to_pc 157);
          stepped w_157
      | _ ->
          ignore (pop_env w_157);
          failwith "impossible"
      | c_103 -> failwith ("unreachable:" ^ string_of_int c_103 ^ "(158)"))
    158

let () =
  add_exp
    (fun w_159 ->
      assert_env_length w_159 4;
      let cond_4 = resolve w_159 (Source.E 3) in
      ignore (pop_env w_159);
      let if_kont_4 =
       fun _ ->
        assert_env_length w_159 4;
        drop_n w_159 4 0;
        assert_env_length w_159 4;
        drop_n w_159 4 1;
        assert_env_length w_159 3;
        drop_n w_159 3 1;
        assert_env_length w_159 2;
        drop_n w_159 2 0;
        assert_env_length w_159 2;
        drop_n w_159 2 0;
        assert_env_length w_159 2;
        drop_n w_159 2 0;
        assert_env_length w_159 2;
        return_n w_159 2 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_4) <> 0 then (
        assert_env_length w_159 3;
        push_env w_159 (Dynarray.get w_159.state.e 2);
        assert_env_length w_159 4;
        push_env w_159 (Dynarray.get w_159.state.e 0);
        assert_env_length w_159 5;
        ignore (env_call w_159 [] 2);
        w_159.state.c <- pc_to_exp (int_to_pc 128);
        stepped w_159)
      else (
        assert_env_length w_159 3;
        push_env w_159 (Dynarray.get w_159.state.e 1);
        assert_env_length w_159 4;
        push_env w_159 (Dynarray.get w_159.state.e 0);
        assert_env_length w_159 5;
        ignore (env_call w_159 [] 2);
        w_159.state.c <- pc_to_exp (int_to_pc 128);
        stepped w_159))
    159

let () =
  add_exp
    (fun w_160 ->
      assert_env_length w_160 4;
      let last_105 = Source.E 3 in
      let x_105 = resolve w_160 last_105 in
      match Word.get_value (fst x_105) with
      | c_105 when c_105 = tag_None ->
          ignore (pop_env w_160);
          failwith "eval: function name must be int literal"
      | c_105 when c_105 = tag_Some ->
          let splits_122 = Memo.splits (snd x_105) in
          let split0_122 = List.nth splits_122 0 in
          ignore (pop_env w_160);
          push_env w_160 split0_122;
          assert_env_length w_160 4;
          push_env w_160 (Dynarray.get w_160.state.e 3);
          assert_env_length w_160 5;
          drop_n w_160 5 1;
          assert_env_length w_160 4;
          push_env w_160 (Dynarray.get w_160.state.e 3);
          assert_env_length w_160 5;
          push_env w_160 (Dynarray.get w_160.state.e 2);
          assert_env_length w_160 6;
          let ctor_arg_31 = pop_env w_160 in
          let ctor_arg_32 = pop_env w_160 in
          push_env w_160 (Memo.appends [ Memo.from_constructor tag_VClosure; ctor_arg_32; ctor_arg_31 ]);
          assert_env_length w_160 5;
          push_env w_160 (Dynarray.get w_160.state.e 3);
          assert_env_length w_160 6;
          push_env w_160 (Dynarray.get w_160.state.e 4);
          assert_env_length w_160 7;
          let ctor_arg_33 = pop_env w_160 in
          let ctor_arg_34 = pop_env w_160 in
          push_env w_160 (Memo.appends [ Memo.from_constructor tag_EnvEntry; ctor_arg_34; ctor_arg_33 ]);
          assert_env_length w_160 6;
          push_env w_160 (Dynarray.get w_160.state.e 1);
          assert_env_length w_160 7;
          let keep_vals_71 = env_call w_160 [ 0 ] 2 in
          w_160.state.k <- Memo.appends [ Memo.from_constructor tag_cont_72; keep_vals_71; w_160.state.k ];
          w_160.state.c <- pc_to_exp (int_to_pc 28);
          stepped w_160
      | c_105 -> failwith ("unreachable:" ^ string_of_int c_105 ^ "(160)"))
    160

let () =
  add_exp
    (fun w_161 ->
      assert_env_length w_161 4;
      let last_106 = Source.E 3 in
      let x_106 = resolve w_161 last_106 in
      match Word.get_value (fst x_106) with
      | c_106 when c_106 = tag_None ->
          ignore (pop_env w_161);
          failwith "eval: defvar name must be int literal"
      | c_106 when c_106 = tag_Some ->
          let splits_123 = Memo.splits (snd x_106) in
          let split0_123 = List.nth splits_123 0 in
          ignore (pop_env w_161);
          push_env w_161 split0_123;
          assert_env_length w_161 4;
          push_env w_161 (Dynarray.get w_161.state.e 3);
          assert_env_length w_161 5;
          drop_n w_161 5 1;
          assert_env_length w_161 4;
          push_env w_161 (Dynarray.get w_161.state.e 3);
          assert_env_length w_161 5;
          push_env w_161 (Dynarray.get w_161.state.e 2);
          assert_env_length w_161 6;
          let ctor_arg_35 = pop_env w_161 in
          let ctor_arg_36 = pop_env w_161 in
          push_env w_161 (Memo.appends [ Memo.from_constructor tag_EnvEntry; ctor_arg_36; ctor_arg_35 ]);
          assert_env_length w_161 5;
          push_env w_161 (Dynarray.get w_161.state.e 1);
          assert_env_length w_161 6;
          let keep_vals_75 = env_call w_161 [ 0 ] 2 in
          w_161.state.k <- Memo.appends [ Memo.from_constructor tag_cont_76; keep_vals_75; w_161.state.k ];
          w_161.state.c <- pc_to_exp (int_to_pc 28);
          stepped w_161
      | c_106 -> failwith ("unreachable:" ^ string_of_int c_106 ^ "(161)"))
    161

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
let () = Words.set_constructor_degree 15 1
let () = Words.set_constructor_degree 16 1
let () = Words.set_constructor_degree 17 1
let () = Words.set_constructor_degree 18 1
let () = Words.set_constructor_degree 19 1
let () = Words.set_constructor_degree 20 1
let () = Words.set_constructor_degree 21 1
let () = Words.set_constructor_degree 22 1
let () = Words.set_constructor_degree 23 1
let () = Words.set_constructor_degree 24 0
let () = Words.set_constructor_degree 25 0
let () = Words.set_constructor_degree 26 0
let () = Words.set_constructor_degree 27 1
let () = Words.set_constructor_degree 28 0
let () = Words.set_constructor_degree 29 (-1)
let () = Words.set_constructor_degree 30 0
let () = Words.set_constructor_degree 31 0
let () = Words.set_constructor_degree 32 0
let () = Words.set_constructor_degree 33 1
let () = Words.set_constructor_degree 34 (-1)
let () = Words.set_constructor_degree 35 (-1)
let () = Words.set_constructor_degree 36 (-1)
let () = Words.set_constructor_degree 37 0
let () = Words.set_constructor_degree 38 (-1)
let () = Words.set_constructor_degree 39 (-1)
let () = Words.set_constructor_degree 40 0
let () = Words.set_constructor_degree 41 0
let () = Words.set_constructor_degree 42 0
let () = Words.set_constructor_degree 43 0
let () = Words.set_constructor_degree 44 (-1)
let () = Words.set_constructor_degree 45 (-2)
let () = Words.set_constructor_degree 46 (-1)
let () = Words.set_constructor_degree 47 0
let () = Words.set_constructor_degree 48 (-2)
let () = Words.set_constructor_degree 49 (-3)
let () = Words.set_constructor_degree 50 (-2)
let () = Words.set_constructor_degree 51 (-3)
let () = Words.set_constructor_degree 52 (-1)
let () = Words.set_constructor_degree 53 (-1)
let () = Words.set_constructor_degree 54 (-3)
let () = Words.set_constructor_degree 55 (-2)
let () = Words.set_constructor_degree 56 (-4)
let () = Words.set_constructor_degree 57 0
let () = Words.set_constructor_degree 58 (-1)
let () = Words.set_constructor_degree 59 (-1)
let () = Words.set_constructor_degree 60 (-1)
let () = Words.set_constructor_degree 61 (-2)
let () = Words.set_constructor_degree 62 (-1)
let () = Words.set_constructor_degree 63 (-1)
let () = Words.set_constructor_degree 64 (-2)
let () = Words.set_constructor_degree 65 (-2)
let () = Words.set_constructor_degree 66 (-1)
let () = Words.set_constructor_degree 67 (-1)
let () = Words.set_constructor_degree 68 (-1)
let () = Words.set_constructor_degree 69 (-2)
let () = Words.set_constructor_degree 70 (-3)
let () = Words.set_constructor_degree 71 0
let () = Words.set_constructor_degree 72 0
let () = Words.set_constructor_degree 73 0
let () = Words.set_constructor_degree 74 (-2)
let () = Words.set_constructor_degree 75 0
let () = Words.set_constructor_degree 76 0
let () = Words.set_constructor_degree 77 (-3)
let () = Words.set_constructor_degree 78 (-2)
let () = Words.set_constructor_degree 79 0
let () = Words.set_constructor_degree 80 0
let () = Words.set_constructor_degree 81 (-2)
let () = Words.set_constructor_degree 82 (-2)
let () = Words.set_constructor_degree 83 (-1)
let () = Words.set_constructor_degree 84 (-2)
let () = Words.set_constructor_degree 85 (-3)
let () = Words.set_constructor_degree 86 (-2)
let () = Words.set_constructor_degree 87 0
let () = Words.set_constructor_degree 88 (-2)
let () = Words.set_constructor_degree 89 (-2)
let () = Words.set_constructor_degree 90 (-1)
let () = Words.set_constructor_degree 91 (-3)
let () = Words.set_constructor_degree 92 (-1)
let () = Words.set_constructor_degree 93 (-2)
let () = Words.set_constructor_degree 94 (-3)
let () = Words.set_constructor_degree 95 (-2)
let () = Words.set_constructor_degree 96 0
let () = Words.set_constructor_degree 97 (-2)
let () = Words.set_constructor_degree 98 0
let () = Words.set_constructor_degree 99 (-4)
let () = Words.set_constructor_degree 100 (-2)
let () = Words.set_constructor_degree 101 (-3)
let () = Words.set_constructor_degree 102 (-2)
let () = Words.set_constructor_degree 103 (-2)
let () = Words.set_constructor_degree 104 (-3)
let () = Words.set_constructor_degree 105 (-3)
let () = Words.set_constructor_degree 106 (-3)
let () = Words.set_constructor_degree 107 (-3)
let () = Words.set_constructor_degree 108 (-3)
let () = Words.set_constructor_degree 109 (-1)
let () = Words.set_constructor_degree 110 (-3)
let () = Words.set_constructor_degree 111 (-3)
let () = Words.set_constructor_degree 112 (-1)
let () = Words.set_constructor_degree 113 (-1)
let () = Words.set_constructor_degree 114 (-1)
let () = Words.set_constructor_degree 115 (-1)
