open Ant
open Word
open Memo
open Value
open Common

let memo = init_memo ()
let tag_cont_done = 0
let tag_Z = 1
let tag_S = 2
let tag_Nil = 3
let tag_Cons = 4
let tag_None = 5
let tag_Some = 6
let tag_EInt = 7
let tag_EPlus = 8
let tag_ELt = 9
let tag_ELe = 10
let tag_EGt = 11
let tag_EGe = 12
let tag_EVar = 13
let tag_EAbs = 14
let tag_EApp = 15
let tag_ELet = 16
let tag_ETrue = 17
let tag_EFalse = 18
let tag_EIf = 19
let tag_ENil = 20
let tag_ECons = 21
let tag_EMatchList = 22
let tag_EFix = 23
let tag_EHole = 24
let tag_VInt = 25
let tag_VAbs = 26
let tag_VTrue = 27
let tag_VFalse = 28
let tag_VNil = 29
let tag_VCons = 30
let tag_VFix = 31
let tag_VStuck = 32
let tag_VTInt = 33
let tag_VTFunc = 34
let tag_VTBool = 35
let tag_VTList = 36
let tag_SHole = 37
let tag_STypeError = 38
let tag_SIndexError = 39
let tag_SApp = 40
let tag_SAdd0 = 41
let tag_SAdd1 = 42
let tag_SGt0 = 43
let tag_SGt1 = 44
let tag_SIf = 45
let tag_SMatchList = 46
let tag_cont_1 = 47
let tag_cont_2 = 48
let tag_cont_3 = 49
let tag_cont_4 = 50
let tag_cont_5 = 51
let tag_cont_6 = 52
let tag_cont_7 = 53
let tag_cont_8 = 54
let tag_cont_9 = 55
let tag_cont_10 = 56
let tag_cont_11 = 57
let tag_cont_12 = 58
let tag_cont_13 = 59
let tag_cont_14 = 60
let tag_cont_15 = 61
let tag_cont_16 = 62
let tag_cont_17 = 63
let tag_cont_18 = 64
let tag_cont_19 = 65

type nat = Z | S of nat

let rec from_ocaml_nat x =
  match x with
  | Z -> Memo.appends [ Memo.from_constructor tag_Z ]
  | S x0 -> Memo.appends [ Memo.from_constructor tag_S; from_ocaml_nat x0 ]

let rec to_ocaml_nat x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_Z -> Z
  | c when c = tag_S ->
      let x0 = Memo.splits_1 t in
      S (to_ocaml_nat x0)
  | _ -> failwith "unreachable"

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

type expr =
  | EInt of int
  | EPlus of expr * expr
  | ELt of expr * expr
  | ELe of expr * expr
  | EGt of expr * expr
  | EGe of expr * expr
  | EVar of nat
  | EAbs of expr
  | EApp of expr * expr
  | ELet of expr * expr
  | ETrue
  | EFalse
  | EIf of expr * expr * expr
  | ENil
  | ECons of expr * expr
  | EMatchList of expr * expr * expr
  | EFix of expr
  | EHole

let rec from_ocaml_expr x =
  match x with
  | EInt x0 -> Memo.appends [ Memo.from_constructor tag_EInt; Memo.from_int x0 ]
  | EPlus (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EPlus; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ELt (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ELt; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ELe (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ELe; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EGt (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EGt; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EGe (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EGe; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EVar x0 -> Memo.appends [ Memo.from_constructor tag_EVar; from_ocaml_nat x0 ]
  | EAbs x0 -> Memo.appends [ Memo.from_constructor tag_EAbs; from_ocaml_expr x0 ]
  | EApp (x0, x1) -> Memo.appends [ Memo.from_constructor tag_EApp; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ELet (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ELet; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | ETrue -> Memo.appends [ Memo.from_constructor tag_ETrue ]
  | EFalse -> Memo.appends [ Memo.from_constructor tag_EFalse ]
  | EIf (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_EIf; from_ocaml_expr x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | ENil -> Memo.appends [ Memo.from_constructor tag_ENil ]
  | ECons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_ECons; from_ocaml_expr x0; from_ocaml_expr x1 ]
  | EMatchList (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_EMatchList; from_ocaml_expr x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | EFix x0 -> Memo.appends [ Memo.from_constructor tag_EFix; from_ocaml_expr x0 ]
  | EHole -> Memo.appends [ Memo.from_constructor tag_EHole ]

let rec to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_EInt ->
      let x0 = Memo.splits_1 t in
      EInt (Word.get_value (Memo.to_word x0))
  | c when c = tag_EPlus ->
      let x0, x1 = Memo.splits_2 t in
      EPlus (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_ELt ->
      let x0, x1 = Memo.splits_2 t in
      ELt (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_ELe ->
      let x0, x1 = Memo.splits_2 t in
      ELe (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_EGt ->
      let x0, x1 = Memo.splits_2 t in
      EGt (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_EGe ->
      let x0, x1 = Memo.splits_2 t in
      EGe (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_EVar ->
      let x0 = Memo.splits_1 t in
      EVar (to_ocaml_nat x0)
  | c when c = tag_EAbs ->
      let x0 = Memo.splits_1 t in
      EAbs (to_ocaml_expr x0)
  | c when c = tag_EApp ->
      let x0, x1 = Memo.splits_2 t in
      EApp (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_ELet ->
      let x0, x1 = Memo.splits_2 t in
      ELet (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_ETrue -> ETrue
  | c when c = tag_EFalse -> EFalse
  | c when c = tag_EIf ->
      let x0, x1, x2 = Memo.splits_3 t in
      EIf (to_ocaml_expr x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | c when c = tag_ENil -> ENil
  | c when c = tag_ECons ->
      let x0, x1 = Memo.splits_2 t in
      ECons (to_ocaml_expr x0, to_ocaml_expr x1)
  | c when c = tag_EMatchList ->
      let x0, x1, x2 = Memo.splits_3 t in
      EMatchList (to_ocaml_expr x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | c when c = tag_EFix ->
      let x0 = Memo.splits_1 t in
      EFix (to_ocaml_expr x0)
  | c when c = tag_EHole -> EHole
  | _ -> failwith "unreachable"

type value =
  | VInt of int
  | VAbs of expr * value list
  | VTrue
  | VFalse
  | VNil
  | VCons of value * value
  | VFix of expr * value list
  | VStuck of stuck

and vtype = VTInt | VTFunc | VTBool | VTList

and stuck =
  | SHole of value list
  | STypeError of value * vtype
  | SIndexError
  | SApp of stuck * expr
  | SAdd0 of stuck * expr
  | SAdd1 of value * stuck
  | SGt0 of stuck * expr
  | SGt1 of value * stuck
  | SIf of stuck * expr * expr
  | SMatchList of stuck * expr * expr

let rec from_ocaml_value x =
  match x with
  | VInt x0 -> Memo.appends [ Memo.from_constructor tag_VInt; Memo.from_int x0 ]
  | VAbs (x0, x1) ->
      Memo.appends
        [ Memo.from_constructor tag_VAbs; from_ocaml_expr x0; from_ocaml_list (fun x -> from_ocaml_value x) x1 ]
  | VTrue -> Memo.appends [ Memo.from_constructor tag_VTrue ]
  | VFalse -> Memo.appends [ Memo.from_constructor tag_VFalse ]
  | VNil -> Memo.appends [ Memo.from_constructor tag_VNil ]
  | VCons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_VCons; from_ocaml_value x0; from_ocaml_value x1 ]
  | VFix (x0, x1) ->
      Memo.appends
        [ Memo.from_constructor tag_VFix; from_ocaml_expr x0; from_ocaml_list (fun x -> from_ocaml_value x) x1 ]
  | VStuck x0 -> Memo.appends [ Memo.from_constructor tag_VStuck; from_ocaml_stuck x0 ]

and from_ocaml_vtype x =
  match x with
  | VTInt -> Memo.appends [ Memo.from_constructor tag_VTInt ]
  | VTFunc -> Memo.appends [ Memo.from_constructor tag_VTFunc ]
  | VTBool -> Memo.appends [ Memo.from_constructor tag_VTBool ]
  | VTList -> Memo.appends [ Memo.from_constructor tag_VTList ]

and from_ocaml_stuck x =
  match x with
  | SHole x0 -> Memo.appends [ Memo.from_constructor tag_SHole; from_ocaml_list (fun x -> from_ocaml_value x) x0 ]
  | STypeError (x0, x1) ->
      Memo.appends [ Memo.from_constructor tag_STypeError; from_ocaml_value x0; from_ocaml_vtype x1 ]
  | SIndexError -> Memo.appends [ Memo.from_constructor tag_SIndexError ]
  | SApp (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SApp; from_ocaml_stuck x0; from_ocaml_expr x1 ]
  | SAdd0 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SAdd0; from_ocaml_stuck x0; from_ocaml_expr x1 ]
  | SAdd1 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SAdd1; from_ocaml_value x0; from_ocaml_stuck x1 ]
  | SGt0 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SGt0; from_ocaml_stuck x0; from_ocaml_expr x1 ]
  | SGt1 (x0, x1) -> Memo.appends [ Memo.from_constructor tag_SGt1; from_ocaml_value x0; from_ocaml_stuck x1 ]
  | SIf (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_SIf; from_ocaml_stuck x0; from_ocaml_expr x1; from_ocaml_expr x2 ]
  | SMatchList (x0, x1, x2) ->
      Memo.appends [ Memo.from_constructor tag_SMatchList; from_ocaml_stuck x0; from_ocaml_expr x1; from_ocaml_expr x2 ]

let rec to_ocaml_value x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_VInt ->
      let x0 = Memo.splits_1 t in
      VInt (Word.get_value (Memo.to_word x0))
  | c when c = tag_VAbs ->
      let x0, x1 = Memo.splits_2 t in
      VAbs (to_ocaml_expr x0, to_ocaml_list (fun x -> to_ocaml_value x) x1)
  | c when c = tag_VTrue -> VTrue
  | c when c = tag_VFalse -> VFalse
  | c when c = tag_VNil -> VNil
  | c when c = tag_VCons ->
      let x0, x1 = Memo.splits_2 t in
      VCons (to_ocaml_value x0, to_ocaml_value x1)
  | c when c = tag_VFix ->
      let x0, x1 = Memo.splits_2 t in
      VFix (to_ocaml_expr x0, to_ocaml_list (fun x -> to_ocaml_value x) x1)
  | c when c = tag_VStuck ->
      let x0 = Memo.splits_1 t in
      VStuck (to_ocaml_stuck x0)
  | _ -> failwith "unreachable"

and to_ocaml_vtype x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_VTInt -> VTInt
  | c when c = tag_VTFunc -> VTFunc
  | c when c = tag_VTBool -> VTBool
  | c when c = tag_VTList -> VTList
  | _ -> failwith "unreachable"

and to_ocaml_stuck x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | c when c = tag_SHole ->
      let x0 = Memo.splits_1 t in
      SHole (to_ocaml_list (fun x -> to_ocaml_value x) x0)
  | c when c = tag_STypeError ->
      let x0, x1 = Memo.splits_2 t in
      STypeError (to_ocaml_value x0, to_ocaml_vtype x1)
  | c when c = tag_SIndexError -> SIndexError
  | c when c = tag_SApp ->
      let x0, x1 = Memo.splits_2 t in
      SApp (to_ocaml_stuck x0, to_ocaml_expr x1)
  | c when c = tag_SAdd0 ->
      let x0, x1 = Memo.splits_2 t in
      SAdd0 (to_ocaml_stuck x0, to_ocaml_expr x1)
  | c when c = tag_SAdd1 ->
      let x0, x1 = Memo.splits_2 t in
      SAdd1 (to_ocaml_value x0, to_ocaml_stuck x1)
  | c when c = tag_SGt0 ->
      let x0, x1 = Memo.splits_2 t in
      SGt0 (to_ocaml_stuck x0, to_ocaml_expr x1)
  | c when c = tag_SGt1 ->
      let x0, x1 = Memo.splits_2 t in
      SGt1 (to_ocaml_value x0, to_ocaml_stuck x1)
  | c when c = tag_SIf ->
      let x0, x1, x2 = Memo.splits_3 t in
      SIf (to_ocaml_stuck x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | c when c = tag_SMatchList ->
      let x0, x1, x2 = Memo.splits_3 t in
      SMatchList (to_ocaml_stuck x0, to_ocaml_expr x1, to_ocaml_expr x2)
  | _ -> failwith "unreachable"

let rec index (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let rec eval (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_5 ->
      assert_env_length w_5 1;
      let hd_0, tl_0 = resolve w_5 K in
      match Word.get_value hd_0 with
      | c_17 when c_17 = tag_cont_done -> exec_done w_5
      | c_17 when c_17 = tag_cont_1 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 6);
          stepped w_5
      | c_17 when c_17 = tag_cont_2 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 7);
          stepped w_5
      | c_17 when c_17 = tag_cont_3 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 8);
          stepped w_5
      | c_17 when c_17 = tag_cont_4 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 9);
          stepped w_5
      | c_17 when c_17 = tag_cont_5 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_5
      | c_17 when c_17 = tag_cont_6 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 0 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 11);
          stepped w_5
      | c_17 when c_17 = tag_cont_7 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 4;
          let ctor_arg_39 = pop_env w_5 in
          let ctor_arg_40 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_40; ctor_arg_39 ]);
          assert_env_length w_5 3;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_17 when c_17 = tag_cont_8 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 12);
          stepped w_5
      | c_17 when c_17 = tag_cont_9 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 13);
          stepped w_5
      | c_17 when c_17 = tag_cont_10 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          let keep_18 = env_call w_5 [ 2 ] 2 in
          w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_19; keep_18; w_5.state.k ];
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_17 when c_17 = tag_cont_11 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 14);
          stepped w_5
      | c_17 when c_17 = tag_cont_12 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 16);
          stepped w_5
      | c_17 when c_17 = tag_cont_13 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 18);
          stepped w_5
      | c_17 when c_17 = tag_cont_14 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 20);
          stepped w_5
      | c_17 when c_17 = tag_cont_15 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 22);
          stepped w_5
      | c_17 when c_17 = tag_cont_16 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 24);
          stepped w_5
      | c_17 when c_17 = tag_cont_17 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 4;
          let ctor_arg_96 = pop_env w_5 in
          let ctor_arg_97 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_97; ctor_arg_96 ]);
          assert_env_length w_5 3;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_17 when c_17 = tag_cont_18 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 6;
          let ctor_arg_98 = pop_env w_5 in
          let ctor_arg_99 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_99; ctor_arg_98 ]);
          assert_env_length w_5 5;
          let ctor_arg_100 = pop_env w_5 in
          let ctor_arg_101 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_101; ctor_arg_100 ]);
          assert_env_length w_5 4;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_17 when c_17 = tag_cont_19 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 1 tl_0;
          assert_env_length w_5 2;
          let ctor_arg_102 = pop_env w_5 in
          let ctor_arg_103 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_VCons; ctor_arg_103; ctor_arg_102 ]);
          assert_env_length w_5 1;
          drop_n w_5 1 0;
          assert_env_length w_5 1;
          return_n w_5 1 (pc_to_exp (int_to_pc 0))
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
      assert_env_length w_2 5;
      let last_1 = Source.E 4 in
      let x_1 = resolve w_2 last_1 in
      ignore (pop_env w_2);
      match Word.get_value (fst x_1) with
      | c_1 when c_1 = tag_Z ->
          assert_env_length w_2 4;
          push_env w_2 (Dynarray.get w_2.state.e 2);
          assert_env_length w_2 5;
          let ctor_arg_0 = pop_env w_2 in
          push_env w_2 (Memo.appends [ Memo.from_constructor tag_Some; ctor_arg_0 ]);
          assert_env_length w_2 5;
          drop_n w_2 5 2;
          assert_env_length w_2 3;
          return_n w_2 3 (pc_to_exp (int_to_pc 0))
      | c_1 when c_1 = tag_S ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          push_env w_2 split0_1;
          assert_env_length w_2 5;
          push_env w_2 (Dynarray.get w_2.state.e 3);
          assert_env_length w_2 6;
          push_env w_2 (Dynarray.get w_2.state.e 4);
          assert_env_length w_2 7;
          ignore (env_call w_2 [] 2);
          w_2.state.c <- pc_to_exp (int_to_pc 1);
          stepped w_2
      | _ -> failwith "unreachable (2)")
    2

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      let last_0 = Source.E 2 in
      let x_0 = resolve w_1 last_0 in
      ignore (pop_env w_1);
      match Word.get_value (fst x_0) with
      | c_0 when c_0 = tag_Cons ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 4;
          push_env w_1 (Dynarray.get w_1.state.e 1);
          w_1.state.c <- pc_to_exp (int_to_pc 2);
          stepped w_1
      | _ ->
          assert_env_length w_1 2;
          push_env w_1 (Memo.from_constructor tag_None);
          assert_env_length w_1 3;
          return_n w_1 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (3)")
    3

let () =
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      push_env w_3 (Dynarray.get w_3.state.e 0);
      w_3.state.c <- pc_to_exp (int_to_pc 5);
      stepped w_3)
    4

let () =
  add_exp
    (fun w_4 ->
      assert_env_length w_4 3;
      let last_2 = Source.E 2 in
      let x_2 = resolve w_4 last_2 in
      ignore (pop_env w_4);
      match Word.get_value (fst x_2) with
      | c_2 when c_2 = tag_EInt ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          push_env w_4 split0_2;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          let ctor_arg_1 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VInt; ctor_arg_1 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EPlus ->
          let splits_3 = Memo.splits (snd x_2) in
          let split0_3 = List.nth splits_3 0 in
          let split1_1 = List.nth splits_3 1 in
          push_env w_4 split0_3;
          push_env w_4 split1_1;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_0 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_ELt ->
          let splits_4 = Memo.splits (snd x_2) in
          let split0_4 = List.nth splits_4 0 in
          let split1_2 = List.nth splits_4 1 in
          push_env w_4 split0_4;
          push_env w_4 split1_2;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_1 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_ELe ->
          let splits_5 = Memo.splits (snd x_2) in
          let split0_5 = List.nth splits_5 0 in
          let split1_3 = List.nth splits_5 1 in
          push_env w_4 split0_5;
          push_env w_4 split1_3;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_2 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_EGt ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          let split1_4 = List.nth splits_6 1 in
          push_env w_4 split0_6;
          push_env w_4 split1_4;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_3 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_EGe ->
          let splits_7 = Memo.splits (snd x_2) in
          let split0_7 = List.nth splits_7 0 in
          let split1_5 = List.nth splits_7 1 in
          push_env w_4 split0_7;
          push_env w_4 split1_5;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_4 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_EVar ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          push_env w_4 split0_8;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          let keep_5 = env_call w_4 [] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 1);
          stepped w_4
      | c_2 when c_2 = tag_EAbs ->
          let splits_9 = Memo.splits (snd x_2) in
          let split0_9 = List.nth splits_9 0 in
          push_env w_4 split0_9;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let ctor_arg_2 = pop_env w_4 in
          let ctor_arg_3 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VAbs; ctor_arg_3; ctor_arg_2 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_ELet ->
          let splits_10 = Memo.splits (snd x_2) in
          let split0_10 = List.nth splits_10 0 in
          let split1_6 = List.nth splits_10 1 in
          push_env w_4 split0_10;
          push_env w_4 split1_6;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 3);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_6 = env_call w_4 [ 1; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_EFix ->
          let splits_11 = Memo.splits (snd x_2) in
          let split0_11 = List.nth splits_11 0 in
          push_env w_4 split0_11;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let ctor_arg_4 = pop_env w_4 in
          let ctor_arg_5 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VFix; ctor_arg_5; ctor_arg_4 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EApp ->
          let splits_12 = Memo.splits (snd x_2) in
          let split0_12 = List.nth splits_12 0 in
          let split1_7 = List.nth splits_12 1 in
          push_env w_4 split0_12;
          push_env w_4 split1_7;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_7 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_EHole ->
          assert_env_length w_4 2;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 3;
          let ctor_arg_6 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_SHole; ctor_arg_6 ]);
          assert_env_length w_4 3;
          let ctor_arg_7 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_7 ]);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_ETrue ->
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VTrue);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EFalse ->
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VFalse);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_EIf ->
          let splits_13 = Memo.splits (snd x_2) in
          let split0_13 = List.nth splits_13 0 in
          let split1_8 = List.nth splits_13 1 in
          let split2_0 = List.nth splits_13 2 in
          push_env w_4 split0_13;
          push_env w_4 split1_8;
          push_env w_4 split2_0;
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_8 = env_call w_4 [ 1; 3; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_ENil ->
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VNil);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_2 when c_2 = tag_ECons ->
          let splits_14 = Memo.splits (snd x_2) in
          let split0_14 = List.nth splits_14 0 in
          let split1_9 = List.nth splits_14 1 in
          push_env w_4 split0_14;
          push_env w_4 split1_9;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_9 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_2 when c_2 = tag_EMatchList ->
          let splits_15 = Memo.splits (snd x_2) in
          let split0_15 = List.nth splits_15 0 in
          let split1_10 = List.nth splits_15 1 in
          let split2_1 = List.nth splits_15 2 in
          push_env w_4 split0_15;
          push_env w_4 split1_10;
          push_env w_4 split2_1;
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_10 = env_call w_4 [ 1; 3; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | _ -> failwith "unreachable (5)")
    5

let () =
  add_exp
    (fun w_6 ->
      assert_env_length w_6 4;
      let last_3 = Source.E 3 in
      let x_3 = resolve w_6 last_3 in
      ignore (pop_env w_6);
      match Word.get_value (fst x_3) with
      | c_3 when c_3 = tag_VInt ->
          let splits_16 = Memo.splits (snd x_3) in
          let split0_16 = List.nth splits_16 0 in
          push_env w_6 split0_16;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 5;
          push_env w_6 (Dynarray.get w_6.state.e 0);
          assert_env_length w_6 6;
          let keep_11 = env_call w_6 [ 2; 3 ] 2 in
          w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_12; keep_11; w_6.state.k ];
          w_6.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_6
      | c_3 when c_3 = tag_VStuck ->
          let splits_17 = Memo.splits (snd x_3) in
          let split0_17 = List.nth splits_17 0 in
          push_env w_6 split0_17;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 3);
          assert_env_length w_6 5;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 6;
          let ctor_arg_8 = pop_env w_6 in
          let ctor_arg_9 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_SAdd0; ctor_arg_9; ctor_arg_8 ]);
          assert_env_length w_6 5;
          let ctor_arg_10 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_10 ]);
          assert_env_length w_6 5;
          drop_n w_6 5 1;
          assert_env_length w_6 4;
          drop_n w_6 4 1;
          assert_env_length w_6 3;
          drop_n w_6 3 1;
          assert_env_length w_6 2;
          return_n w_6 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_6 3;
          push_env w_6 (Dynarray.get w_6.state.e 2);
          assert_env_length w_6 4;
          push_env w_6 (Memo.from_constructor tag_VTInt);
          assert_env_length w_6 5;
          let ctor_arg_11 = pop_env w_6 in
          let ctor_arg_12 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_12; ctor_arg_11 ]);
          assert_env_length w_6 4;
          let ctor_arg_13 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_13 ]);
          assert_env_length w_6 4;
          drop_n w_6 4 1;
          assert_env_length w_6 3;
          drop_n w_6 3 1;
          assert_env_length w_6 2;
          return_n w_6 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (6)")
    6

let () =
  add_exp
    (fun w_7 ->
      assert_env_length w_7 4;
      let last_4 = Source.E 3 in
      let x_4 = resolve w_7 last_4 in
      ignore (pop_env w_7);
      match Word.get_value (fst x_4) with
      | c_4 when c_4 = tag_VInt ->
          let splits_18 = Memo.splits (snd x_4) in
          let split0_18 = List.nth splits_18 0 in
          push_env w_7 split0_18;
          assert_env_length w_7 4;
          push_env w_7 (Dynarray.get w_7.state.e 1);
          assert_env_length w_7 5;
          push_env w_7 (Dynarray.get w_7.state.e 0);
          assert_env_length w_7 6;
          let keep_12 = env_call w_7 [ 2; 3 ] 2 in
          w_7.state.k <- Memo.appends [ Memo.from_constructor tag_cont_13; keep_12; w_7.state.k ];
          w_7.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_7
      | c_4 when c_4 = tag_VStuck ->
          let splits_19 = Memo.splits (snd x_4) in
          let split0_19 = List.nth splits_19 0 in
          push_env w_7 split0_19;
          assert_env_length w_7 4;
          push_env w_7 (Dynarray.get w_7.state.e 3);
          assert_env_length w_7 5;
          push_env w_7 (Dynarray.get w_7.state.e 1);
          assert_env_length w_7 6;
          let ctor_arg_14 = pop_env w_7 in
          let ctor_arg_15 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_15; ctor_arg_14 ]);
          assert_env_length w_7 5;
          let ctor_arg_16 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_16 ]);
          assert_env_length w_7 5;
          drop_n w_7 5 1;
          assert_env_length w_7 4;
          drop_n w_7 4 1;
          assert_env_length w_7 3;
          drop_n w_7 3 1;
          assert_env_length w_7 2;
          return_n w_7 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_7 3;
          push_env w_7 (Dynarray.get w_7.state.e 2);
          assert_env_length w_7 4;
          push_env w_7 (Memo.from_constructor tag_VTInt);
          assert_env_length w_7 5;
          let ctor_arg_17 = pop_env w_7 in
          let ctor_arg_18 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_18; ctor_arg_17 ]);
          assert_env_length w_7 4;
          let ctor_arg_19 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_19 ]);
          assert_env_length w_7 4;
          drop_n w_7 4 1;
          assert_env_length w_7 3;
          drop_n w_7 3 1;
          assert_env_length w_7 2;
          return_n w_7 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (7)")
    7

let () =
  add_exp
    (fun w_8 ->
      assert_env_length w_8 4;
      let last_5 = Source.E 3 in
      let x_5 = resolve w_8 last_5 in
      ignore (pop_env w_8);
      match Word.get_value (fst x_5) with
      | c_5 when c_5 = tag_VInt ->
          let splits_20 = Memo.splits (snd x_5) in
          let split0_20 = List.nth splits_20 0 in
          push_env w_8 split0_20;
          assert_env_length w_8 4;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 0);
          assert_env_length w_8 6;
          let keep_13 = env_call w_8 [ 2; 3 ] 2 in
          w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_14; keep_13; w_8.state.k ];
          w_8.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_8
      | c_5 when c_5 = tag_VStuck ->
          let splits_21 = Memo.splits (snd x_5) in
          let split0_21 = List.nth splits_21 0 in
          push_env w_8 split0_21;
          assert_env_length w_8 4;
          push_env w_8 (Dynarray.get w_8.state.e 3);
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 6;
          let ctor_arg_20 = pop_env w_8 in
          let ctor_arg_21 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_21; ctor_arg_20 ]);
          assert_env_length w_8 5;
          let ctor_arg_22 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_22 ]);
          assert_env_length w_8 5;
          drop_n w_8 5 1;
          assert_env_length w_8 4;
          drop_n w_8 4 1;
          assert_env_length w_8 3;
          drop_n w_8 3 1;
          assert_env_length w_8 2;
          return_n w_8 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_8 3;
          push_env w_8 (Dynarray.get w_8.state.e 2);
          assert_env_length w_8 4;
          push_env w_8 (Memo.from_constructor tag_VTInt);
          assert_env_length w_8 5;
          let ctor_arg_23 = pop_env w_8 in
          let ctor_arg_24 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_24; ctor_arg_23 ]);
          assert_env_length w_8 4;
          let ctor_arg_25 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_25 ]);
          assert_env_length w_8 4;
          drop_n w_8 4 1;
          assert_env_length w_8 3;
          drop_n w_8 3 1;
          assert_env_length w_8 2;
          return_n w_8 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (8)")
    8

let () =
  add_exp
    (fun w_9 ->
      assert_env_length w_9 4;
      let last_6 = Source.E 3 in
      let x_6 = resolve w_9 last_6 in
      ignore (pop_env w_9);
      match Word.get_value (fst x_6) with
      | c_6 when c_6 = tag_VInt ->
          let splits_22 = Memo.splits (snd x_6) in
          let split0_22 = List.nth splits_22 0 in
          push_env w_9 split0_22;
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 1);
          assert_env_length w_9 5;
          push_env w_9 (Dynarray.get w_9.state.e 0);
          assert_env_length w_9 6;
          let keep_14 = env_call w_9 [ 2; 3 ] 2 in
          w_9.state.k <- Memo.appends [ Memo.from_constructor tag_cont_15; keep_14; w_9.state.k ];
          w_9.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_9
      | c_6 when c_6 = tag_VStuck ->
          let splits_23 = Memo.splits (snd x_6) in
          let split0_23 = List.nth splits_23 0 in
          push_env w_9 split0_23;
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 3);
          assert_env_length w_9 5;
          push_env w_9 (Dynarray.get w_9.state.e 1);
          assert_env_length w_9 6;
          let ctor_arg_26 = pop_env w_9 in
          let ctor_arg_27 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_27; ctor_arg_26 ]);
          assert_env_length w_9 5;
          let ctor_arg_28 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_28 ]);
          assert_env_length w_9 5;
          drop_n w_9 5 1;
          assert_env_length w_9 4;
          drop_n w_9 4 1;
          assert_env_length w_9 3;
          drop_n w_9 3 1;
          assert_env_length w_9 2;
          return_n w_9 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_9 3;
          push_env w_9 (Dynarray.get w_9.state.e 2);
          assert_env_length w_9 4;
          push_env w_9 (Memo.from_constructor tag_VTInt);
          assert_env_length w_9 5;
          let ctor_arg_29 = pop_env w_9 in
          let ctor_arg_30 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_30; ctor_arg_29 ]);
          assert_env_length w_9 4;
          let ctor_arg_31 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_31 ]);
          assert_env_length w_9 4;
          drop_n w_9 4 1;
          assert_env_length w_9 3;
          drop_n w_9 3 1;
          assert_env_length w_9 2;
          return_n w_9 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (9)")
    9

let () =
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      let last_7 = Source.E 3 in
      let x_7 = resolve w_10 last_7 in
      ignore (pop_env w_10);
      match Word.get_value (fst x_7) with
      | c_7 when c_7 = tag_VInt ->
          let splits_24 = Memo.splits (snd x_7) in
          let split0_24 = List.nth splits_24 0 in
          push_env w_10 split0_24;
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 5;
          push_env w_10 (Dynarray.get w_10.state.e 0);
          assert_env_length w_10 6;
          let keep_15 = env_call w_10 [ 2; 3 ] 2 in
          w_10.state.k <- Memo.appends [ Memo.from_constructor tag_cont_16; keep_15; w_10.state.k ];
          w_10.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_10
      | c_7 when c_7 = tag_VStuck ->
          let splits_25 = Memo.splits (snd x_7) in
          let split0_25 = List.nth splits_25 0 in
          push_env w_10 split0_25;
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 3);
          assert_env_length w_10 5;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 6;
          let ctor_arg_32 = pop_env w_10 in
          let ctor_arg_33 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_SGt0; ctor_arg_33; ctor_arg_32 ]);
          assert_env_length w_10 5;
          let ctor_arg_34 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_34 ]);
          assert_env_length w_10 5;
          drop_n w_10 5 1;
          assert_env_length w_10 4;
          drop_n w_10 4 1;
          assert_env_length w_10 3;
          drop_n w_10 3 1;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_10 3;
          push_env w_10 (Dynarray.get w_10.state.e 2);
          assert_env_length w_10 4;
          push_env w_10 (Memo.from_constructor tag_VTInt);
          assert_env_length w_10 5;
          let ctor_arg_35 = pop_env w_10 in
          let ctor_arg_36 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_36; ctor_arg_35 ]);
          assert_env_length w_10 4;
          let ctor_arg_37 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_37 ]);
          assert_env_length w_10 4;
          drop_n w_10 4 1;
          assert_env_length w_10 3;
          drop_n w_10 3 1;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (10)")
    10

let () =
  add_exp
    (fun w_11 ->
      assert_env_length w_11 1;
      let last_8 = Source.E 0 in
      let x_8 = resolve w_11 last_8 in
      ignore (pop_env w_11);
      match Word.get_value (fst x_8) with
      | c_8 when c_8 = tag_Some ->
          let splits_26 = Memo.splits (snd x_8) in
          let split0_26 = List.nth splits_26 0 in
          push_env w_11 split0_26;
          assert_env_length w_11 1;
          push_env w_11 (Dynarray.get w_11.state.e 0);
          assert_env_length w_11 2;
          drop_n w_11 2 1;
          assert_env_length w_11 1;
          drop_n w_11 1 0;
          assert_env_length w_11 1;
          return_n w_11 1 (pc_to_exp (int_to_pc 0))
      | c_8 when c_8 = tag_None ->
          assert_env_length w_11 0;
          push_env w_11 (Memo.from_constructor tag_SIndexError);
          assert_env_length w_11 1;
          let ctor_arg_38 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_38 ]);
          assert_env_length w_11 1;
          drop_n w_11 1 0;
          assert_env_length w_11 1;
          return_n w_11 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (11)")
    11

let () =
  add_exp
    (fun w_12 ->
      assert_env_length w_12 4;
      let last_9 = Source.E 3 in
      let x_9 = resolve w_12 last_9 in
      ignore (pop_env w_12);
      match Word.get_value (fst x_9) with
      | c_9 when c_9 = tag_VAbs ->
          let splits_27 = Memo.splits (snd x_9) in
          let split0_27 = List.nth splits_27 0 in
          let split1_11 = List.nth splits_27 1 in
          push_env w_12 split0_27;
          push_env w_12 split1_11;
          assert_env_length w_12 5;
          push_env w_12 (Dynarray.get w_12.state.e 3);
          assert_env_length w_12 6;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          assert_env_length w_12 7;
          push_env w_12 (Dynarray.get w_12.state.e 0);
          assert_env_length w_12 8;
          let keep_16 = env_call w_12 [ 4; 5 ] 2 in
          w_12.state.k <- Memo.appends [ Memo.from_constructor tag_cont_17; keep_16; w_12.state.k ];
          w_12.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_12
      | c_9 when c_9 = tag_VFix ->
          let splits_28 = Memo.splits (snd x_9) in
          let split0_28 = List.nth splits_28 0 in
          let split1_12 = List.nth splits_28 1 in
          push_env w_12 split0_28;
          push_env w_12 split1_12;
          assert_env_length w_12 5;
          push_env w_12 (Dynarray.get w_12.state.e 3);
          assert_env_length w_12 6;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          assert_env_length w_12 7;
          push_env w_12 (Dynarray.get w_12.state.e 0);
          assert_env_length w_12 8;
          let keep_17 = env_call w_12 [ 2; 4; 5 ] 2 in
          w_12.state.k <- Memo.appends [ Memo.from_constructor tag_cont_18; keep_17; w_12.state.k ];
          w_12.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_12
      | c_9 when c_9 = tag_VStuck ->
          let splits_29 = Memo.splits (snd x_9) in
          let split0_29 = List.nth splits_29 0 in
          push_env w_12 split0_29;
          assert_env_length w_12 4;
          push_env w_12 (Dynarray.get w_12.state.e 3);
          assert_env_length w_12 5;
          push_env w_12 (Dynarray.get w_12.state.e 1);
          assert_env_length w_12 6;
          let ctor_arg_41 = pop_env w_12 in
          let ctor_arg_42 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_SApp; ctor_arg_42; ctor_arg_41 ]);
          assert_env_length w_12 5;
          let ctor_arg_43 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_43 ]);
          assert_env_length w_12 5;
          drop_n w_12 5 1;
          assert_env_length w_12 4;
          drop_n w_12 4 1;
          assert_env_length w_12 3;
          drop_n w_12 3 1;
          assert_env_length w_12 2;
          return_n w_12 2 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_12 3;
          push_env w_12 (Dynarray.get w_12.state.e 2);
          assert_env_length w_12 4;
          push_env w_12 (Memo.from_constructor tag_VTFunc);
          assert_env_length w_12 5;
          let ctor_arg_44 = pop_env w_12 in
          let ctor_arg_45 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_45; ctor_arg_44 ]);
          assert_env_length w_12 4;
          let ctor_arg_46 = pop_env w_12 in
          push_env w_12 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_46 ]);
          assert_env_length w_12 4;
          drop_n w_12 4 1;
          assert_env_length w_12 3;
          drop_n w_12 3 1;
          assert_env_length w_12 2;
          return_n w_12 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (12)")
    12

let () =
  add_exp
    (fun w_13 ->
      assert_env_length w_13 4;
      let last_10 = Source.E 3 in
      let x_10 = resolve w_13 last_10 in
      ignore (pop_env w_13);
      match Word.get_value (fst x_10) with
      | c_10 when c_10 = tag_VTrue ->
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 0);
          assert_env_length w_13 5;
          ignore (env_call w_13 [] 2);
          w_13.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_13
      | c_10 when c_10 = tag_VFalse ->
          assert_env_length w_13 3;
          push_env w_13 (Dynarray.get w_13.state.e 2);
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 0);
          assert_env_length w_13 5;
          ignore (env_call w_13 [] 2);
          w_13.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_13
      | c_10 when c_10 = tag_VStuck ->
          let splits_30 = Memo.splits (snd x_10) in
          let split0_30 = List.nth splits_30 0 in
          push_env w_13 split0_30;
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 3);
          assert_env_length w_13 5;
          push_env w_13 (Dynarray.get w_13.state.e 1);
          assert_env_length w_13 6;
          push_env w_13 (Dynarray.get w_13.state.e 2);
          assert_env_length w_13 7;
          let ctor_arg_47 = pop_env w_13 in
          let ctor_arg_48 = pop_env w_13 in
          let ctor_arg_49 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_SIf; ctor_arg_49; ctor_arg_48; ctor_arg_47 ]);
          assert_env_length w_13 5;
          let ctor_arg_50 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_50 ]);
          assert_env_length w_13 5;
          drop_n w_13 5 1;
          assert_env_length w_13 4;
          drop_n w_13 4 2;
          assert_env_length w_13 2;
          return_n w_13 2 (pc_to_exp (int_to_pc 0))
      | iv ->
          assert_env_length w_13 4;
          push_env w_13 (Dynarray.get w_13.state.e 3);
          assert_env_length w_13 5;
          push_env w_13 (Memo.from_constructor tag_VTBool);
          assert_env_length w_13 6;
          let ctor_arg_51 = pop_env w_13 in
          let ctor_arg_52 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_52; ctor_arg_51 ]);
          assert_env_length w_13 5;
          let ctor_arg_53 = pop_env w_13 in
          push_env w_13 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_53 ]);
          assert_env_length w_13 5;
          drop_n w_13 5 2;
          assert_env_length w_13 3;
          return_n w_13 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (13)")
    13

let () =
  add_exp
    (fun w_14 ->
      assert_env_length w_14 4;
      let last_11 = Source.E 3 in
      let x_11 = resolve w_14 last_11 in
      ignore (pop_env w_14);
      match Word.get_value (fst x_11) with
      | c_11 when c_11 = tag_VNil ->
          assert_env_length w_14 3;
          push_env w_14 (Dynarray.get w_14.state.e 1);
          assert_env_length w_14 4;
          push_env w_14 (Dynarray.get w_14.state.e 0);
          assert_env_length w_14 5;
          ignore (env_call w_14 [] 2);
          w_14.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_14
      | c_11 when c_11 = tag_VCons ->
          let splits_31 = Memo.splits (snd x_11) in
          let split0_31 = List.nth splits_31 0 in
          let split1_13 = List.nth splits_31 1 in
          push_env w_14 split0_31;
          push_env w_14 split1_13;
          assert_env_length w_14 5;
          push_env w_14 (Dynarray.get w_14.state.e 2);
          assert_env_length w_14 6;
          push_env w_14 (Dynarray.get w_14.state.e 4);
          assert_env_length w_14 7;
          push_env w_14 (Dynarray.get w_14.state.e 3);
          assert_env_length w_14 8;
          push_env w_14 (Dynarray.get w_14.state.e 0);
          assert_env_length w_14 9;
          let ctor_arg_54 = pop_env w_14 in
          let ctor_arg_55 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_55; ctor_arg_54 ]);
          assert_env_length w_14 8;
          let ctor_arg_56 = pop_env w_14 in
          let ctor_arg_57 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_Cons; ctor_arg_57; ctor_arg_56 ]);
          assert_env_length w_14 7;
          ignore (env_call w_14 [] 2);
          w_14.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_14
      | c_11 when c_11 = tag_VStuck ->
          let splits_32 = Memo.splits (snd x_11) in
          let split0_32 = List.nth splits_32 0 in
          push_env w_14 split0_32;
          assert_env_length w_14 4;
          push_env w_14 (Dynarray.get w_14.state.e 3);
          assert_env_length w_14 5;
          push_env w_14 (Dynarray.get w_14.state.e 1);
          assert_env_length w_14 6;
          push_env w_14 (Dynarray.get w_14.state.e 2);
          assert_env_length w_14 7;
          let ctor_arg_58 = pop_env w_14 in
          let ctor_arg_59 = pop_env w_14 in
          let ctor_arg_60 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_SMatchList; ctor_arg_60; ctor_arg_59; ctor_arg_58 ]);
          assert_env_length w_14 5;
          let ctor_arg_61 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_61 ]);
          assert_env_length w_14 5;
          drop_n w_14 5 1;
          assert_env_length w_14 4;
          drop_n w_14 4 2;
          assert_env_length w_14 2;
          return_n w_14 2 (pc_to_exp (int_to_pc 0))
      | vv ->
          assert_env_length w_14 4;
          push_env w_14 (Dynarray.get w_14.state.e 3);
          assert_env_length w_14 5;
          push_env w_14 (Memo.from_constructor tag_VTList);
          assert_env_length w_14 6;
          let ctor_arg_62 = pop_env w_14 in
          let ctor_arg_63 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_63; ctor_arg_62 ]);
          assert_env_length w_14 5;
          let ctor_arg_64 = pop_env w_14 in
          push_env w_14 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_64 ]);
          assert_env_length w_14 5;
          drop_n w_14 5 2;
          assert_env_length w_14 3;
          return_n w_14 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (14)")
    14

let () =
  add_exp
    (fun w_16 ->
      assert_env_length w_16 6;
      let x0_0 = resolve w_16 (Source.E 4) in
      let x1_0 = resolve w_16 (Source.E 5) in
      ignore (pop_env w_16);
      ignore (pop_env w_16);
      push_env w_16 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_16 5;
      let ctor_arg_65 = pop_env w_16 in
      push_env w_16 (Memo.appends [ Memo.from_constructor tag_VInt; ctor_arg_65 ]);
      assert_env_length w_16 5;
      drop_n w_16 5 1;
      assert_env_length w_16 4;
      drop_n w_16 4 1;
      assert_env_length w_16 3;
      drop_n w_16 3 1;
      assert_env_length w_16 2;
      drop_n w_16 2 1;
      assert_env_length w_16 1;
      drop_n w_16 1 0;
      assert_env_length w_16 1;
      return_n w_16 1 (pc_to_exp (int_to_pc 0)))
    15

let () =
  add_exp
    (fun w_15 ->
      assert_env_length w_15 4;
      let last_12 = Source.E 3 in
      let x_12 = resolve w_15 last_12 in
      ignore (pop_env w_15);
      match Word.get_value (fst x_12) with
      | c_12 when c_12 = tag_VInt ->
          let splits_33 = Memo.splits (snd x_12) in
          let split0_33 = List.nth splits_33 0 in
          push_env w_15 split0_33;
          assert_env_length w_15 4;
          push_env w_15 (Dynarray.get w_15.state.e 1);
          assert_env_length w_15 5;
          push_env w_15 (Dynarray.get w_15.state.e 3);
          w_15.state.c <- pc_to_exp (int_to_pc 15);
          stepped w_15
      | c_12 when c_12 = tag_VStuck ->
          let splits_34 = Memo.splits (snd x_12) in
          let split0_34 = List.nth splits_34 0 in
          push_env w_15 split0_34;
          assert_env_length w_15 4;
          push_env w_15 (Dynarray.get w_15.state.e 0);
          assert_env_length w_15 5;
          push_env w_15 (Dynarray.get w_15.state.e 3);
          assert_env_length w_15 6;
          let ctor_arg_66 = pop_env w_15 in
          let ctor_arg_67 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_SAdd1; ctor_arg_67; ctor_arg_66 ]);
          assert_env_length w_15 5;
          let ctor_arg_68 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_68 ]);
          assert_env_length w_15 5;
          drop_n w_15 5 1;
          assert_env_length w_15 4;
          drop_n w_15 4 1;
          assert_env_length w_15 3;
          drop_n w_15 3 1;
          assert_env_length w_15 2;
          drop_n w_15 2 1;
          assert_env_length w_15 1;
          drop_n w_15 1 0;
          assert_env_length w_15 1;
          return_n w_15 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_15 3;
          push_env w_15 (Dynarray.get w_15.state.e 2);
          assert_env_length w_15 4;
          push_env w_15 (Memo.from_constructor tag_VTInt);
          assert_env_length w_15 5;
          let ctor_arg_69 = pop_env w_15 in
          let ctor_arg_70 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_70; ctor_arg_69 ]);
          assert_env_length w_15 4;
          let ctor_arg_71 = pop_env w_15 in
          push_env w_15 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_71 ]);
          assert_env_length w_15 4;
          drop_n w_15 4 1;
          assert_env_length w_15 3;
          drop_n w_15 3 1;
          assert_env_length w_15 2;
          drop_n w_15 2 1;
          assert_env_length w_15 1;
          drop_n w_15 1 0;
          assert_env_length w_15 1;
          return_n w_15 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (16)")
    16

let () =
  add_exp
    (fun w_18 ->
      assert_env_length w_18 6;
      let x0_1 = resolve w_18 (Source.E 4) in
      let x1_1 = resolve w_18 (Source.E 5) in
      ignore (pop_env w_18);
      ignore (pop_env w_18);
      push_env w_18 (Memo.from_int (if Word.get_value (fst x0_1) < Word.get_value (fst x1_1) then 1 else 0));
      assert_env_length w_18 5;
      let cond_0 = resolve w_18 (Source.E 4) in
      ignore (pop_env w_18);
      let if_kont_0 =
       fun _ ->
        assert_env_length w_18 5;
        drop_n w_18 5 1;
        assert_env_length w_18 4;
        drop_n w_18 4 1;
        assert_env_length w_18 3;
        drop_n w_18 3 1;
        assert_env_length w_18 2;
        drop_n w_18 2 1;
        assert_env_length w_18 1;
        drop_n w_18 1 0;
        assert_env_length w_18 1;
        return_n w_18 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_0) <> 0 then (
        assert_env_length w_18 4;
        push_env w_18 (Memo.from_constructor tag_VTrue);
        if_kont_0 ())
      else (
        assert_env_length w_18 4;
        push_env w_18 (Memo.from_constructor tag_VFalse);
        if_kont_0 ()))
    17

let () =
  add_exp
    (fun w_17 ->
      assert_env_length w_17 4;
      let last_13 = Source.E 3 in
      let x_13 = resolve w_17 last_13 in
      ignore (pop_env w_17);
      match Word.get_value (fst x_13) with
      | c_13 when c_13 = tag_VInt ->
          let splits_35 = Memo.splits (snd x_13) in
          let split0_35 = List.nth splits_35 0 in
          push_env w_17 split0_35;
          assert_env_length w_17 4;
          push_env w_17 (Dynarray.get w_17.state.e 1);
          assert_env_length w_17 5;
          push_env w_17 (Dynarray.get w_17.state.e 3);
          w_17.state.c <- pc_to_exp (int_to_pc 17);
          stepped w_17
      | c_13 when c_13 = tag_VStuck ->
          let splits_36 = Memo.splits (snd x_13) in
          let split0_36 = List.nth splits_36 0 in
          push_env w_17 split0_36;
          assert_env_length w_17 4;
          push_env w_17 (Dynarray.get w_17.state.e 0);
          assert_env_length w_17 5;
          push_env w_17 (Dynarray.get w_17.state.e 3);
          assert_env_length w_17 6;
          let ctor_arg_72 = pop_env w_17 in
          let ctor_arg_73 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_73; ctor_arg_72 ]);
          assert_env_length w_17 5;
          let ctor_arg_74 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_74 ]);
          assert_env_length w_17 5;
          drop_n w_17 5 1;
          assert_env_length w_17 4;
          drop_n w_17 4 1;
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          drop_n w_17 2 1;
          assert_env_length w_17 1;
          drop_n w_17 1 0;
          assert_env_length w_17 1;
          return_n w_17 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_17 3;
          push_env w_17 (Dynarray.get w_17.state.e 2);
          assert_env_length w_17 4;
          push_env w_17 (Memo.from_constructor tag_VTInt);
          assert_env_length w_17 5;
          let ctor_arg_75 = pop_env w_17 in
          let ctor_arg_76 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_76; ctor_arg_75 ]);
          assert_env_length w_17 4;
          let ctor_arg_77 = pop_env w_17 in
          push_env w_17 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_77 ]);
          assert_env_length w_17 4;
          drop_n w_17 4 1;
          assert_env_length w_17 3;
          drop_n w_17 3 1;
          assert_env_length w_17 2;
          drop_n w_17 2 1;
          assert_env_length w_17 1;
          drop_n w_17 1 0;
          assert_env_length w_17 1;
          return_n w_17 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (18)")
    18

let () =
  add_exp
    (fun w_20 ->
      assert_env_length w_20 6;
      let x0_2 = resolve w_20 (Source.E 4) in
      let x1_2 = resolve w_20 (Source.E 5) in
      ignore (pop_env w_20);
      ignore (pop_env w_20);
      push_env w_20 (Memo.from_int (if Word.get_value (fst x0_2) <= Word.get_value (fst x1_2) then 1 else 0));
      assert_env_length w_20 5;
      let cond_1 = resolve w_20 (Source.E 4) in
      ignore (pop_env w_20);
      let if_kont_1 =
       fun _ ->
        assert_env_length w_20 5;
        drop_n w_20 5 1;
        assert_env_length w_20 4;
        drop_n w_20 4 1;
        assert_env_length w_20 3;
        drop_n w_20 3 1;
        assert_env_length w_20 2;
        drop_n w_20 2 1;
        assert_env_length w_20 1;
        drop_n w_20 1 0;
        assert_env_length w_20 1;
        return_n w_20 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_1) <> 0 then (
        assert_env_length w_20 4;
        push_env w_20 (Memo.from_constructor tag_VTrue);
        if_kont_1 ())
      else (
        assert_env_length w_20 4;
        push_env w_20 (Memo.from_constructor tag_VFalse);
        if_kont_1 ()))
    19

let () =
  add_exp
    (fun w_19 ->
      assert_env_length w_19 4;
      let last_14 = Source.E 3 in
      let x_14 = resolve w_19 last_14 in
      ignore (pop_env w_19);
      match Word.get_value (fst x_14) with
      | c_14 when c_14 = tag_VInt ->
          let splits_37 = Memo.splits (snd x_14) in
          let split0_37 = List.nth splits_37 0 in
          push_env w_19 split0_37;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 1);
          assert_env_length w_19 5;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          w_19.state.c <- pc_to_exp (int_to_pc 19);
          stepped w_19
      | c_14 when c_14 = tag_VStuck ->
          let splits_38 = Memo.splits (snd x_14) in
          let split0_38 = List.nth splits_38 0 in
          push_env w_19 split0_38;
          assert_env_length w_19 4;
          push_env w_19 (Dynarray.get w_19.state.e 0);
          assert_env_length w_19 5;
          push_env w_19 (Dynarray.get w_19.state.e 3);
          assert_env_length w_19 6;
          let ctor_arg_78 = pop_env w_19 in
          let ctor_arg_79 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_79; ctor_arg_78 ]);
          assert_env_length w_19 5;
          let ctor_arg_80 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_80 ]);
          assert_env_length w_19 5;
          drop_n w_19 5 1;
          assert_env_length w_19 4;
          drop_n w_19 4 1;
          assert_env_length w_19 3;
          drop_n w_19 3 1;
          assert_env_length w_19 2;
          drop_n w_19 2 1;
          assert_env_length w_19 1;
          drop_n w_19 1 0;
          assert_env_length w_19 1;
          return_n w_19 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_19 3;
          push_env w_19 (Dynarray.get w_19.state.e 2);
          assert_env_length w_19 4;
          push_env w_19 (Memo.from_constructor tag_VTInt);
          assert_env_length w_19 5;
          let ctor_arg_81 = pop_env w_19 in
          let ctor_arg_82 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_82; ctor_arg_81 ]);
          assert_env_length w_19 4;
          let ctor_arg_83 = pop_env w_19 in
          push_env w_19 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_83 ]);
          assert_env_length w_19 4;
          drop_n w_19 4 1;
          assert_env_length w_19 3;
          drop_n w_19 3 1;
          assert_env_length w_19 2;
          drop_n w_19 2 1;
          assert_env_length w_19 1;
          drop_n w_19 1 0;
          assert_env_length w_19 1;
          return_n w_19 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (20)")
    20

let () =
  add_exp
    (fun w_22 ->
      assert_env_length w_22 6;
      let x0_3 = resolve w_22 (Source.E 4) in
      let x1_3 = resolve w_22 (Source.E 5) in
      ignore (pop_env w_22);
      ignore (pop_env w_22);
      push_env w_22 (Memo.from_int (if Word.get_value (fst x0_3) > Word.get_value (fst x1_3) then 1 else 0));
      assert_env_length w_22 5;
      let cond_2 = resolve w_22 (Source.E 4) in
      ignore (pop_env w_22);
      let if_kont_2 =
       fun _ ->
        assert_env_length w_22 5;
        drop_n w_22 5 1;
        assert_env_length w_22 4;
        drop_n w_22 4 1;
        assert_env_length w_22 3;
        drop_n w_22 3 1;
        assert_env_length w_22 2;
        drop_n w_22 2 1;
        assert_env_length w_22 1;
        drop_n w_22 1 0;
        assert_env_length w_22 1;
        return_n w_22 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_2) <> 0 then (
        assert_env_length w_22 4;
        push_env w_22 (Memo.from_constructor tag_VTrue);
        if_kont_2 ())
      else (
        assert_env_length w_22 4;
        push_env w_22 (Memo.from_constructor tag_VFalse);
        if_kont_2 ()))
    21

let () =
  add_exp
    (fun w_21 ->
      assert_env_length w_21 4;
      let last_15 = Source.E 3 in
      let x_15 = resolve w_21 last_15 in
      ignore (pop_env w_21);
      match Word.get_value (fst x_15) with
      | c_15 when c_15 = tag_VInt ->
          let splits_39 = Memo.splits (snd x_15) in
          let split0_39 = List.nth splits_39 0 in
          push_env w_21 split0_39;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 1);
          assert_env_length w_21 5;
          push_env w_21 (Dynarray.get w_21.state.e 3);
          w_21.state.c <- pc_to_exp (int_to_pc 21);
          stepped w_21
      | c_15 when c_15 = tag_VStuck ->
          let splits_40 = Memo.splits (snd x_15) in
          let split0_40 = List.nth splits_40 0 in
          push_env w_21 split0_40;
          assert_env_length w_21 4;
          push_env w_21 (Dynarray.get w_21.state.e 0);
          assert_env_length w_21 5;
          push_env w_21 (Dynarray.get w_21.state.e 3);
          assert_env_length w_21 6;
          let ctor_arg_84 = pop_env w_21 in
          let ctor_arg_85 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_85; ctor_arg_84 ]);
          assert_env_length w_21 5;
          let ctor_arg_86 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_86 ]);
          assert_env_length w_21 5;
          drop_n w_21 5 1;
          assert_env_length w_21 4;
          drop_n w_21 4 1;
          assert_env_length w_21 3;
          drop_n w_21 3 1;
          assert_env_length w_21 2;
          drop_n w_21 2 1;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          return_n w_21 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_21 3;
          push_env w_21 (Dynarray.get w_21.state.e 2);
          assert_env_length w_21 4;
          push_env w_21 (Memo.from_constructor tag_VTInt);
          assert_env_length w_21 5;
          let ctor_arg_87 = pop_env w_21 in
          let ctor_arg_88 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_88; ctor_arg_87 ]);
          assert_env_length w_21 4;
          let ctor_arg_89 = pop_env w_21 in
          push_env w_21 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_89 ]);
          assert_env_length w_21 4;
          drop_n w_21 4 1;
          assert_env_length w_21 3;
          drop_n w_21 3 1;
          assert_env_length w_21 2;
          drop_n w_21 2 1;
          assert_env_length w_21 1;
          drop_n w_21 1 0;
          assert_env_length w_21 1;
          return_n w_21 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (22)")
    22

let () =
  add_exp
    (fun w_24 ->
      assert_env_length w_24 6;
      let x0_4 = resolve w_24 (Source.E 4) in
      let x1_4 = resolve w_24 (Source.E 5) in
      ignore (pop_env w_24);
      ignore (pop_env w_24);
      push_env w_24 (Memo.from_int (if Word.get_value (fst x0_4) >= Word.get_value (fst x1_4) then 1 else 0));
      assert_env_length w_24 5;
      let cond_3 = resolve w_24 (Source.E 4) in
      ignore (pop_env w_24);
      let if_kont_3 =
       fun _ ->
        assert_env_length w_24 5;
        drop_n w_24 5 1;
        assert_env_length w_24 4;
        drop_n w_24 4 1;
        assert_env_length w_24 3;
        drop_n w_24 3 1;
        assert_env_length w_24 2;
        drop_n w_24 2 1;
        assert_env_length w_24 1;
        drop_n w_24 1 0;
        assert_env_length w_24 1;
        return_n w_24 1 (pc_to_exp (int_to_pc 0))
      in
      if Word.get_value (fst cond_3) <> 0 then (
        assert_env_length w_24 4;
        push_env w_24 (Memo.from_constructor tag_VTrue);
        if_kont_3 ())
      else (
        assert_env_length w_24 4;
        push_env w_24 (Memo.from_constructor tag_VFalse);
        if_kont_3 ()))
    23

let () =
  add_exp
    (fun w_23 ->
      assert_env_length w_23 4;
      let last_16 = Source.E 3 in
      let x_16 = resolve w_23 last_16 in
      ignore (pop_env w_23);
      match Word.get_value (fst x_16) with
      | c_16 when c_16 = tag_VInt ->
          let splits_41 = Memo.splits (snd x_16) in
          let split0_41 = List.nth splits_41 0 in
          push_env w_23 split0_41;
          assert_env_length w_23 4;
          push_env w_23 (Dynarray.get w_23.state.e 1);
          assert_env_length w_23 5;
          push_env w_23 (Dynarray.get w_23.state.e 3);
          w_23.state.c <- pc_to_exp (int_to_pc 23);
          stepped w_23
      | c_16 when c_16 = tag_VStuck ->
          let splits_42 = Memo.splits (snd x_16) in
          let split0_42 = List.nth splits_42 0 in
          push_env w_23 split0_42;
          assert_env_length w_23 4;
          push_env w_23 (Dynarray.get w_23.state.e 0);
          assert_env_length w_23 5;
          push_env w_23 (Dynarray.get w_23.state.e 3);
          assert_env_length w_23 6;
          let ctor_arg_90 = pop_env w_23 in
          let ctor_arg_91 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_SGt1; ctor_arg_91; ctor_arg_90 ]);
          assert_env_length w_23 5;
          let ctor_arg_92 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_92 ]);
          assert_env_length w_23 5;
          drop_n w_23 5 1;
          assert_env_length w_23 4;
          drop_n w_23 4 1;
          assert_env_length w_23 3;
          drop_n w_23 3 1;
          assert_env_length w_23 2;
          drop_n w_23 2 1;
          assert_env_length w_23 1;
          drop_n w_23 1 0;
          assert_env_length w_23 1;
          return_n w_23 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_23 3;
          push_env w_23 (Dynarray.get w_23.state.e 2);
          assert_env_length w_23 4;
          push_env w_23 (Memo.from_constructor tag_VTInt);
          assert_env_length w_23 5;
          let ctor_arg_93 = pop_env w_23 in
          let ctor_arg_94 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_STypeError; ctor_arg_94; ctor_arg_93 ]);
          assert_env_length w_23 4;
          let ctor_arg_95 = pop_env w_23 in
          push_env w_23 (Memo.appends [ Memo.from_constructor tag_VStuck; ctor_arg_95 ]);
          assert_env_length w_23 4;
          drop_n w_23 4 1;
          assert_env_length w_23 3;
          drop_n w_23 3 1;
          assert_env_length w_23 2;
          drop_n w_23 2 1;
          assert_env_length w_23 1;
          drop_n w_23 1 0;
          assert_env_length w_23 1;
          return_n w_23 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (24)")
    24

let () = Words.set_constructor_degree 0 1
let () = Words.set_constructor_degree 1 1
let () = Words.set_constructor_degree 2 0
let () = Words.set_constructor_degree 3 1
let () = Words.set_constructor_degree 4 (-1)
let () = Words.set_constructor_degree 5 1
let () = Words.set_constructor_degree 6 0
let () = Words.set_constructor_degree 7 0
let () = Words.set_constructor_degree 8 (-1)
let () = Words.set_constructor_degree 9 (-1)
let () = Words.set_constructor_degree 10 (-1)
let () = Words.set_constructor_degree 11 (-1)
let () = Words.set_constructor_degree 12 (-1)
let () = Words.set_constructor_degree 13 0
let () = Words.set_constructor_degree 14 0
let () = Words.set_constructor_degree 15 (-1)
let () = Words.set_constructor_degree 16 (-1)
let () = Words.set_constructor_degree 17 1
let () = Words.set_constructor_degree 18 1
let () = Words.set_constructor_degree 19 (-2)
let () = Words.set_constructor_degree 20 1
let () = Words.set_constructor_degree 21 (-1)
let () = Words.set_constructor_degree 22 (-2)
let () = Words.set_constructor_degree 23 0
let () = Words.set_constructor_degree 24 1
let () = Words.set_constructor_degree 25 0
let () = Words.set_constructor_degree 26 (-1)
let () = Words.set_constructor_degree 27 1
let () = Words.set_constructor_degree 28 1
let () = Words.set_constructor_degree 29 1
let () = Words.set_constructor_degree 30 (-1)
let () = Words.set_constructor_degree 31 (-1)
let () = Words.set_constructor_degree 32 0
let () = Words.set_constructor_degree 33 1
let () = Words.set_constructor_degree 34 1
let () = Words.set_constructor_degree 35 1
let () = Words.set_constructor_degree 36 1
let () = Words.set_constructor_degree 37 0
let () = Words.set_constructor_degree 38 (-1)
let () = Words.set_constructor_degree 39 1
let () = Words.set_constructor_degree 40 (-1)
let () = Words.set_constructor_degree 41 (-1)
let () = Words.set_constructor_degree 42 (-1)
let () = Words.set_constructor_degree 43 (-1)
let () = Words.set_constructor_degree 44 (-1)
let () = Words.set_constructor_degree 45 (-2)
let () = Words.set_constructor_degree 46 (-2)
let () = Words.set_constructor_degree 47 (-2)
let () = Words.set_constructor_degree 48 (-2)
let () = Words.set_constructor_degree 49 (-2)
let () = Words.set_constructor_degree 50 (-2)
let () = Words.set_constructor_degree 51 (-2)
let () = Words.set_constructor_degree 52 0
let () = Words.set_constructor_degree 53 (-2)
let () = Words.set_constructor_degree 54 (-2)
let () = Words.set_constructor_degree 55 (-3)
let () = Words.set_constructor_degree 56 (-2)
let () = Words.set_constructor_degree 57 (-3)
let () = Words.set_constructor_degree 58 (-2)
let () = Words.set_constructor_degree 59 (-2)
let () = Words.set_constructor_degree 60 (-2)
let () = Words.set_constructor_degree 61 (-2)
let () = Words.set_constructor_degree 62 (-2)
let () = Words.set_constructor_degree 63 (-2)
let () = Words.set_constructor_degree 64 (-3)
let () = Words.set_constructor_degree 65 (-1)
