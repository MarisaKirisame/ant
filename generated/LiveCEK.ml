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
let tag_EVar = 9
let tag_EAbs = 10
let tag_EApp = 11
let tag_ELet = 12
let tag_ETrue = 13
let tag_EFalse = 14
let tag_EIf = 15
let tag_ENil = 16
let tag_ECons = 17
let tag_EMatchList = 18
let tag_EFix = 19
let tag_EHole = 20
let tag_VInt = 21
let tag_VAbs = 22
let tag_VTrue = 23
let tag_VFalse = 24
let tag_VNil = 25
let tag_VCons = 26
let tag_VFix = 27
let tag_VStuck = 28
let tag_VTInt = 29
let tag_VTFunc = 30
let tag_VTBool = 31
let tag_VTList = 32
let tag_SHole = 33
let tag_STypeError = 34
let tag_SIndexError = 35
let tag_SApp = 36
let tag_SAdd0 = 37
let tag_SAdd1 = 38
let tag_SIf = 39
let tag_SMatchList = 40
let tag_cont_1 = 41
let tag_cont_2 = 42
let tag_cont_3 = 43
let tag_cont_4 = 44
let tag_cont_5 = 45
let tag_cont_6 = 46
let tag_cont_7 = 47
let tag_cont_8 = 48
let tag_cont_9 = 49
let tag_cont_10 = 50
let tag_cont_11 = 51

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
      | c_16 when c_16 = tag_cont_done -> exec_done w_5
      | c_16 when c_16 = tag_cont_1 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 6);
          stepped w_5
      | c_16 when c_16 = tag_cont_2 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 0 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 7);
          stepped w_5
      | c_16 when c_16 = tag_cont_3 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 4;
          let x1_20 = pop_env w_5 in
          let x0_36 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; x0_36; x1_20 ]);
          assert_env_length w_5 3;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_16 when c_16 = tag_cont_4 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 8);
          stepped w_5
      | c_16 when c_16 = tag_cont_5 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 9);
          stepped w_5
      | c_16 when c_16 = tag_cont_6 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          let keep_10 = env_call w_5 [ 2 ] 2 in
          w_5.state.k <- Memo.appends [ Memo.from_constructor tag_cont_11; keep_10; w_5.state.k ];
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_16 when c_16 = tag_cont_7 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          w_5.state.c <- pc_to_exp (int_to_pc 10);
          stepped w_5
      | c_16 when c_16 = tag_cont_8 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 2);
          w_5.state.c <- pc_to_exp (int_to_pc 12);
          stepped w_5
      | c_16 when c_16 = tag_cont_9 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 2 tl_0;
          assert_env_length w_5 3;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 4;
          let x1_32 = pop_env w_5 in
          let x0_57 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; x0_57; x1_32 ]);
          assert_env_length w_5 3;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_16 when c_16 = tag_cont_10 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 3 tl_0;
          assert_env_length w_5 4;
          push_env w_5 (Dynarray.get w_5.state.e 0);
          assert_env_length w_5 5;
          push_env w_5 (Dynarray.get w_5.state.e 1);
          assert_env_length w_5 6;
          let x1_33 = pop_env w_5 in
          let x0_58 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; x0_58; x1_33 ]);
          assert_env_length w_5 5;
          let x1_34 = pop_env w_5 in
          let x0_59 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_Cons; x0_59; x1_34 ]);
          assert_env_length w_5 4;
          ignore (env_call w_5 [] 2);
          w_5.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_5
      | c_16 when c_16 = tag_cont_11 ->
          w_5.state.k <- get_next_cont tl_0;
          restore_env w_5 1 tl_0;
          assert_env_length w_5 2;
          let x1_35 = pop_env w_5 in
          let x0_60 = pop_env w_5 in
          push_env w_5 (Memo.appends [ Memo.from_constructor tag_VCons; x0_60; x1_35 ]);
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
      | c_8 when c_8 = tag_Z ->
          assert_env_length w_2 4;
          push_env w_2 (Dynarray.get w_2.state.e 2);
          assert_env_length w_2 5;
          let x0_25 = pop_env w_2 in
          push_env w_2 (Memo.appends [ Memo.from_constructor tag_Some; x0_25 ]);
          assert_env_length w_2 5;
          drop_n w_2 5 2;
          assert_env_length w_2 3;
          return_n w_2 3 (pc_to_exp (int_to_pc 0))
      | c_8 when c_8 = tag_S ->
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
      | c_7 when c_7 = tag_Cons ->
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
      | c_9 when c_9 = tag_EInt ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          push_env w_4 split0_2;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          let x0_26 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VInt; x0_26 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_EPlus ->
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
      | c_9 when c_9 = tag_EVar ->
          let splits_4 = Memo.splits (snd x_2) in
          let split0_4 = List.nth splits_4 0 in
          push_env w_4 split0_4;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          let keep_1 = env_call w_4 [] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_2; keep_1; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 1);
          stepped w_4
      | c_9 when c_9 = tag_EAbs ->
          let splits_5 = Memo.splits (snd x_2) in
          let split0_5 = List.nth splits_5 0 in
          push_env w_4 split0_5;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let x1_16 = pop_env w_4 in
          let x0_27 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VAbs; x0_27; x1_16 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_ELet ->
          let splits_6 = Memo.splits (snd x_2) in
          let split0_6 = List.nth splits_6 0 in
          let split1_2 = List.nth splits_6 1 in
          push_env w_4 split0_6;
          push_env w_4 split1_2;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 3);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_2 = env_call w_4 [ 1; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_3; keep_2; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_9 when c_9 = tag_EFix ->
          let splits_7 = Memo.splits (snd x_2) in
          let split0_7 = List.nth splits_7 0 in
          push_env w_4 split0_7;
          assert_env_length w_4 3;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 5;
          let x1_17 = pop_env w_4 in
          let x0_28 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VFix; x0_28; x1_17 ]);
          assert_env_length w_4 4;
          drop_n w_4 4 1;
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_EApp ->
          let splits_8 = Memo.splits (snd x_2) in
          let split0_8 = List.nth splits_8 0 in
          let split1_3 = List.nth splits_8 1 in
          push_env w_4 split0_8;
          push_env w_4 split1_3;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_3 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_4; keep_3; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_9 when c_9 = tag_EHole ->
          assert_env_length w_4 2;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 3;
          let x0_29 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_SHole; x0_29 ]);
          assert_env_length w_4 3;
          let x0_30 = pop_env w_4 in
          push_env w_4 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_30 ]);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_ETrue ->
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VTrue);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_EFalse ->
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VFalse);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_EIf ->
          let splits_9 = Memo.splits (snd x_2) in
          let split0_9 = List.nth splits_9 0 in
          let split1_4 = List.nth splits_9 1 in
          let split2_0 = List.nth splits_9 2 in
          push_env w_4 split0_9;
          push_env w_4 split1_4;
          push_env w_4 split2_0;
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_4 = env_call w_4 [ 1; 3; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_5; keep_4; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_9 when c_9 = tag_ENil ->
          assert_env_length w_4 2;
          push_env w_4 (Memo.from_constructor tag_VNil);
          assert_env_length w_4 3;
          return_n w_4 3 (pc_to_exp (int_to_pc 0))
      | c_9 when c_9 = tag_ECons ->
          let splits_10 = Memo.splits (snd x_2) in
          let split0_10 = List.nth splits_10 0 in
          let split1_5 = List.nth splits_10 1 in
          push_env w_4 split0_10;
          push_env w_4 split1_5;
          assert_env_length w_4 4;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 6;
          let keep_5 = env_call w_4 [ 1; 3 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_6; keep_5; w_4.state.k ];
          w_4.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_4
      | c_9 when c_9 = tag_EMatchList ->
          let splits_11 = Memo.splits (snd x_2) in
          let split0_11 = List.nth splits_11 0 in
          let split1_6 = List.nth splits_11 1 in
          let split2_1 = List.nth splits_11 2 in
          push_env w_4 split0_11;
          push_env w_4 split1_6;
          push_env w_4 split2_1;
          assert_env_length w_4 5;
          push_env w_4 (Dynarray.get w_4.state.e 2);
          assert_env_length w_4 6;
          push_env w_4 (Dynarray.get w_4.state.e 1);
          assert_env_length w_4 7;
          let keep_6 = env_call w_4 [ 1; 3; 4 ] 2 in
          w_4.state.k <- Memo.appends [ Memo.from_constructor tag_cont_7; keep_6; w_4.state.k ];
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
      | c_10 when c_10 = tag_VInt ->
          let splits_12 = Memo.splits (snd x_3) in
          let split0_12 = List.nth splits_12 0 in
          push_env w_6 split0_12;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 5;
          push_env w_6 (Dynarray.get w_6.state.e 0);
          assert_env_length w_6 6;
          let keep_7 = env_call w_6 [ 2; 3 ] 2 in
          w_6.state.k <- Memo.appends [ Memo.from_constructor tag_cont_8; keep_7; w_6.state.k ];
          w_6.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_6
      | c_10 when c_10 = tag_VStuck ->
          let splits_13 = Memo.splits (snd x_3) in
          let split0_13 = List.nth splits_13 0 in
          push_env w_6 split0_13;
          assert_env_length w_6 4;
          push_env w_6 (Dynarray.get w_6.state.e 3);
          assert_env_length w_6 5;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 6;
          let x1_18 = pop_env w_6 in
          let x0_31 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_SAdd0; x0_31; x1_18 ]);
          assert_env_length w_6 5;
          let x0_32 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_32 ]);
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
          let x1_19 = pop_env w_6 in
          let x0_33 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_STypeError; x0_33; x1_19 ]);
          assert_env_length w_6 4;
          let x0_34 = pop_env w_6 in
          push_env w_6 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_34 ]);
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
      assert_env_length w_7 1;
      let last_4 = Source.E 0 in
      let x_4 = resolve w_7 last_4 in
      ignore (pop_env w_7);
      match Word.get_value (fst x_4) with
      | c_11 when c_11 = tag_Some ->
          let splits_14 = Memo.splits (snd x_4) in
          let split0_14 = List.nth splits_14 0 in
          push_env w_7 split0_14;
          assert_env_length w_7 1;
          push_env w_7 (Dynarray.get w_7.state.e 0);
          assert_env_length w_7 2;
          drop_n w_7 2 1;
          assert_env_length w_7 1;
          drop_n w_7 1 0;
          assert_env_length w_7 1;
          return_n w_7 1 (pc_to_exp (int_to_pc 0))
      | c_11 when c_11 = tag_None ->
          assert_env_length w_7 0;
          push_env w_7 (Memo.from_constructor tag_SIndexError);
          assert_env_length w_7 1;
          let x0_35 = pop_env w_7 in
          push_env w_7 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_35 ]);
          assert_env_length w_7 1;
          drop_n w_7 1 0;
          assert_env_length w_7 1;
          return_n w_7 1 (pc_to_exp (int_to_pc 0))
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
      | c_12 when c_12 = tag_VAbs ->
          let splits_15 = Memo.splits (snd x_5) in
          let split0_15 = List.nth splits_15 0 in
          let split1_7 = List.nth splits_15 1 in
          push_env w_8 split0_15;
          push_env w_8 split1_7;
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 3);
          assert_env_length w_8 6;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 7;
          push_env w_8 (Dynarray.get w_8.state.e 0);
          assert_env_length w_8 8;
          let keep_8 = env_call w_8 [ 4; 5 ] 2 in
          w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_9; keep_8; w_8.state.k ];
          w_8.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_8
      | c_12 when c_12 = tag_VFix ->
          let splits_16 = Memo.splits (snd x_5) in
          let split0_16 = List.nth splits_16 0 in
          let split1_8 = List.nth splits_16 1 in
          push_env w_8 split0_16;
          push_env w_8 split1_8;
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 3);
          assert_env_length w_8 6;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 7;
          push_env w_8 (Dynarray.get w_8.state.e 0);
          assert_env_length w_8 8;
          let keep_9 = env_call w_8 [ 2; 4; 5 ] 2 in
          w_8.state.k <- Memo.appends [ Memo.from_constructor tag_cont_10; keep_9; w_8.state.k ];
          w_8.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_8
      | c_12 when c_12 = tag_VStuck ->
          let splits_17 = Memo.splits (snd x_5) in
          let split0_17 = List.nth splits_17 0 in
          push_env w_8 split0_17;
          assert_env_length w_8 4;
          push_env w_8 (Dynarray.get w_8.state.e 3);
          assert_env_length w_8 5;
          push_env w_8 (Dynarray.get w_8.state.e 1);
          assert_env_length w_8 6;
          let x1_21 = pop_env w_8 in
          let x0_37 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_SApp; x0_37; x1_21 ]);
          assert_env_length w_8 5;
          let x0_38 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_38 ]);
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
          push_env w_8 (Memo.from_constructor tag_VTFunc);
          assert_env_length w_8 5;
          let x1_22 = pop_env w_8 in
          let x0_39 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_STypeError; x0_39; x1_22 ]);
          assert_env_length w_8 4;
          let x0_40 = pop_env w_8 in
          push_env w_8 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_40 ]);
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
      | c_13 when c_13 = tag_VTrue ->
          assert_env_length w_9 3;
          push_env w_9 (Dynarray.get w_9.state.e 1);
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 0);
          assert_env_length w_9 5;
          ignore (env_call w_9 [] 2);
          w_9.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_9
      | c_13 when c_13 = tag_VFalse ->
          assert_env_length w_9 3;
          push_env w_9 (Dynarray.get w_9.state.e 2);
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 0);
          assert_env_length w_9 5;
          ignore (env_call w_9 [] 2);
          w_9.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_9
      | c_13 when c_13 = tag_VStuck ->
          let splits_18 = Memo.splits (snd x_6) in
          let split0_18 = List.nth splits_18 0 in
          push_env w_9 split0_18;
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 3);
          assert_env_length w_9 5;
          push_env w_9 (Dynarray.get w_9.state.e 1);
          assert_env_length w_9 6;
          push_env w_9 (Dynarray.get w_9.state.e 2);
          assert_env_length w_9 7;
          let x2_4 = pop_env w_9 in
          let x1_23 = pop_env w_9 in
          let x0_41 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_SIf; x0_41; x1_23; x2_4 ]);
          assert_env_length w_9 5;
          let x0_42 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_42 ]);
          assert_env_length w_9 5;
          drop_n w_9 5 1;
          assert_env_length w_9 4;
          drop_n w_9 4 2;
          assert_env_length w_9 2;
          return_n w_9 2 (pc_to_exp (int_to_pc 0))
      | iv ->
          assert_env_length w_9 4;
          push_env w_9 (Dynarray.get w_9.state.e 3);
          assert_env_length w_9 5;
          push_env w_9 (Memo.from_constructor tag_VTBool);
          assert_env_length w_9 6;
          let x1_24 = pop_env w_9 in
          let x0_43 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_STypeError; x0_43; x1_24 ]);
          assert_env_length w_9 5;
          let x0_44 = pop_env w_9 in
          push_env w_9 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_44 ]);
          assert_env_length w_9 5;
          drop_n w_9 5 2;
          assert_env_length w_9 3;
          return_n w_9 3 (pc_to_exp (int_to_pc 0))
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
      | c_14 when c_14 = tag_VNil ->
          assert_env_length w_10 3;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 0);
          assert_env_length w_10 5;
          ignore (env_call w_10 [] 2);
          w_10.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_10
      | c_14 when c_14 = tag_VCons ->
          let splits_19 = Memo.splits (snd x_7) in
          let split0_19 = List.nth splits_19 0 in
          let split1_9 = List.nth splits_19 1 in
          push_env w_10 split0_19;
          push_env w_10 split1_9;
          assert_env_length w_10 5;
          push_env w_10 (Dynarray.get w_10.state.e 2);
          assert_env_length w_10 6;
          push_env w_10 (Dynarray.get w_10.state.e 4);
          assert_env_length w_10 7;
          push_env w_10 (Dynarray.get w_10.state.e 3);
          assert_env_length w_10 8;
          push_env w_10 (Dynarray.get w_10.state.e 0);
          assert_env_length w_10 9;
          let x1_25 = pop_env w_10 in
          let x0_45 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_Cons; x0_45; x1_25 ]);
          assert_env_length w_10 8;
          let x1_26 = pop_env w_10 in
          let x0_46 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_Cons; x0_46; x1_26 ]);
          assert_env_length w_10 7;
          ignore (env_call w_10 [] 2);
          w_10.state.c <- pc_to_exp (int_to_pc 4);
          stepped w_10
      | c_14 when c_14 = tag_VStuck ->
          let splits_20 = Memo.splits (snd x_7) in
          let split0_20 = List.nth splits_20 0 in
          push_env w_10 split0_20;
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 3);
          assert_env_length w_10 5;
          push_env w_10 (Dynarray.get w_10.state.e 1);
          assert_env_length w_10 6;
          push_env w_10 (Dynarray.get w_10.state.e 2);
          assert_env_length w_10 7;
          let x2_5 = pop_env w_10 in
          let x1_27 = pop_env w_10 in
          let x0_47 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_SMatchList; x0_47; x1_27; x2_5 ]);
          assert_env_length w_10 5;
          let x0_48 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_48 ]);
          assert_env_length w_10 5;
          drop_n w_10 5 1;
          assert_env_length w_10 4;
          drop_n w_10 4 2;
          assert_env_length w_10 2;
          return_n w_10 2 (pc_to_exp (int_to_pc 0))
      | vv ->
          assert_env_length w_10 4;
          push_env w_10 (Dynarray.get w_10.state.e 3);
          assert_env_length w_10 5;
          push_env w_10 (Memo.from_constructor tag_VTList);
          assert_env_length w_10 6;
          let x1_28 = pop_env w_10 in
          let x0_49 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_STypeError; x0_49; x1_28 ]);
          assert_env_length w_10 5;
          let x0_50 = pop_env w_10 in
          push_env w_10 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_50 ]);
          assert_env_length w_10 5;
          drop_n w_10 5 2;
          assert_env_length w_10 3;
          return_n w_10 3 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (10)")
    10

let () =
  add_exp
    (fun w_12 ->
      assert_env_length w_12 6;
      let x0_51 = resolve w_12 (Source.E 4) in
      let x1_29 = resolve w_12 (Source.E 5) in
      ignore (pop_env w_12);
      ignore (pop_env w_12);
      push_env w_12 (Memo.from_int (Word.get_value (fst x0_51) + Word.get_value (fst x1_29)));
      assert_env_length w_12 5;
      let x0_52 = pop_env w_12 in
      push_env w_12 (Memo.appends [ Memo.from_constructor tag_VInt; x0_52 ]);
      assert_env_length w_12 5;
      drop_n w_12 5 1;
      assert_env_length w_12 4;
      drop_n w_12 4 1;
      assert_env_length w_12 3;
      drop_n w_12 3 1;
      assert_env_length w_12 2;
      drop_n w_12 2 1;
      assert_env_length w_12 1;
      drop_n w_12 1 0;
      assert_env_length w_12 1;
      return_n w_12 1 (pc_to_exp (int_to_pc 0)))
    11

let () =
  add_exp
    (fun w_11 ->
      assert_env_length w_11 4;
      let last_8 = Source.E 3 in
      let x_8 = resolve w_11 last_8 in
      ignore (pop_env w_11);
      match Word.get_value (fst x_8) with
      | c_15 when c_15 = tag_VInt ->
          let splits_21 = Memo.splits (snd x_8) in
          let split0_21 = List.nth splits_21 0 in
          push_env w_11 split0_21;
          assert_env_length w_11 4;
          push_env w_11 (Dynarray.get w_11.state.e 1);
          assert_env_length w_11 5;
          push_env w_11 (Dynarray.get w_11.state.e 3);
          w_11.state.c <- pc_to_exp (int_to_pc 11);
          stepped w_11
      | c_15 when c_15 = tag_VStuck ->
          let splits_22 = Memo.splits (snd x_8) in
          let split0_22 = List.nth splits_22 0 in
          push_env w_11 split0_22;
          assert_env_length w_11 4;
          push_env w_11 (Dynarray.get w_11.state.e 0);
          assert_env_length w_11 5;
          push_env w_11 (Dynarray.get w_11.state.e 3);
          assert_env_length w_11 6;
          let x1_30 = pop_env w_11 in
          let x0_53 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_SAdd1; x0_53; x1_30 ]);
          assert_env_length w_11 5;
          let x0_54 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_54 ]);
          assert_env_length w_11 5;
          drop_n w_11 5 1;
          assert_env_length w_11 4;
          drop_n w_11 4 1;
          assert_env_length w_11 3;
          drop_n w_11 3 1;
          assert_env_length w_11 2;
          drop_n w_11 2 1;
          assert_env_length w_11 1;
          drop_n w_11 1 0;
          assert_env_length w_11 1;
          return_n w_11 1 (pc_to_exp (int_to_pc 0))
      | _ ->
          assert_env_length w_11 3;
          push_env w_11 (Dynarray.get w_11.state.e 2);
          assert_env_length w_11 4;
          push_env w_11 (Memo.from_constructor tag_VTInt);
          assert_env_length w_11 5;
          let x1_31 = pop_env w_11 in
          let x0_55 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_STypeError; x0_55; x1_31 ]);
          assert_env_length w_11 4;
          let x0_56 = pop_env w_11 in
          push_env w_11 (Memo.appends [ Memo.from_constructor tag_VStuck; x0_56 ]);
          assert_env_length w_11 4;
          drop_n w_11 4 1;
          assert_env_length w_11 3;
          drop_n w_11 3 1;
          assert_env_length w_11 2;
          drop_n w_11 2 1;
          assert_env_length w_11 1;
          drop_n w_11 1 0;
          assert_env_length w_11 1;
          return_n w_11 1 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (12)")
    12

let () = Words.set_constructor_degree 0 1
let () = Words.set_constructor_degree 1 1
let () = Words.set_constructor_degree 2 0
let () = Words.set_constructor_degree 3 1
let () = Words.set_constructor_degree 4 (-1)
let () = Words.set_constructor_degree 5 1
let () = Words.set_constructor_degree 6 0
let () = Words.set_constructor_degree 7 0
let () = Words.set_constructor_degree 8 (-1)
let () = Words.set_constructor_degree 9 0
let () = Words.set_constructor_degree 10 0
let () = Words.set_constructor_degree 11 (-1)
let () = Words.set_constructor_degree 12 (-1)
let () = Words.set_constructor_degree 13 1
let () = Words.set_constructor_degree 14 1
let () = Words.set_constructor_degree 15 (-2)
let () = Words.set_constructor_degree 16 1
let () = Words.set_constructor_degree 17 (-1)
let () = Words.set_constructor_degree 18 (-2)
let () = Words.set_constructor_degree 19 0
let () = Words.set_constructor_degree 20 1
let () = Words.set_constructor_degree 21 0
let () = Words.set_constructor_degree 22 (-1)
let () = Words.set_constructor_degree 23 1
let () = Words.set_constructor_degree 24 1
let () = Words.set_constructor_degree 25 1
let () = Words.set_constructor_degree 26 (-1)
let () = Words.set_constructor_degree 27 (-1)
let () = Words.set_constructor_degree 28 0
let () = Words.set_constructor_degree 29 1
let () = Words.set_constructor_degree 30 1
let () = Words.set_constructor_degree 31 1
let () = Words.set_constructor_degree 32 1
let () = Words.set_constructor_degree 33 0
let () = Words.set_constructor_degree 34 (-1)
let () = Words.set_constructor_degree 35 1
let () = Words.set_constructor_degree 36 (-1)
let () = Words.set_constructor_degree 37 (-1)
let () = Words.set_constructor_degree 38 (-1)
let () = Words.set_constructor_degree 39 (-2)
let () = Words.set_constructor_degree 40 (-2)
let () = Words.set_constructor_degree 41 (-2)
let () = Words.set_constructor_degree 42 0
let () = Words.set_constructor_degree 43 (-2)
let () = Words.set_constructor_degree 44 (-2)
let () = Words.set_constructor_degree 45 (-3)
let () = Words.set_constructor_degree 46 (-2)
let () = Words.set_constructor_degree 47 (-3)
let () = Words.set_constructor_degree 48 (-2)
let () = Words.set_constructor_degree 49 (-2)
let () = Words.set_constructor_degree 50 (-3)
let () = Words.set_constructor_degree 51 (-1)
