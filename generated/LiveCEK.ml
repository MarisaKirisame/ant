open Ant
open Word
open Memo
open Value
open Common

let memo = Array.init 13 (fun _ -> ref State.BlackHole)

type ocaml_nat = Z | S of Value.seq

let nat_Z : Value.seq = Memo.appends [ Memo.from_constructor 1 ]
let nat_S x0 : Value.seq = Memo.appends [ Memo.from_constructor 2; x0 ]
let from_ocaml_nat x = match x with Z -> nat_Z | S x0 -> nat_S x0

let to_ocaml_nat x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 -> Z
  | 2 ->
      let [ x0 ] = Memo.splits t in
      S x0
  | _ -> failwith "unreachable"

type ocaml_list = Nil | Cons of Value.seq * Value.seq

let list_Nil : Value.seq = Memo.appends [ Memo.from_constructor 3 ]
let list_Cons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 4; x0; x1 ]
let from_ocaml_list x = match x with Nil -> list_Nil | Cons (x0, x1) -> list_Cons x0 x1

let to_ocaml_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 3 -> Nil
  | 4 ->
      let [ x0; x1 ] = Memo.splits t in
      Cons (x0, x1)
  | _ -> failwith "unreachable"

type ocaml_option = None | Some of Value.seq

let option_None : Value.seq = Memo.appends [ Memo.from_constructor 5 ]
let option_Some x0 : Value.seq = Memo.appends [ Memo.from_constructor 6; x0 ]
let from_ocaml_option x = match x with None -> option_None | Some x0 -> option_Some x0

let to_ocaml_option x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 5 -> None
  | 6 ->
      let [ x0 ] = Memo.splits t in
      Some x0
  | _ -> failwith "unreachable"

type ocaml_expr =
  | EInt of Value.seq
  | EPlus of Value.seq * Value.seq
  | EVar of Value.seq
  | EAbs of Value.seq
  | EApp of Value.seq * Value.seq
  | ELet of Value.seq * Value.seq
  | ETrue
  | EFalse
  | EIf of Value.seq * Value.seq * Value.seq
  | ENil
  | ECons of Value.seq * Value.seq
  | EMatchList of Value.seq * Value.seq * Value.seq
  | EFix of Value.seq
  | EHole

let expr_EInt x0 : Value.seq = Memo.appends [ Memo.from_constructor 7; x0 ]
let expr_EPlus x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 8; x0; x1 ]
let expr_EVar x0 : Value.seq = Memo.appends [ Memo.from_constructor 9; x0 ]
let expr_EAbs x0 : Value.seq = Memo.appends [ Memo.from_constructor 10; x0 ]
let expr_EApp x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 11; x0; x1 ]
let expr_ELet x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 12; x0; x1 ]
let expr_ETrue : Value.seq = Memo.appends [ Memo.from_constructor 13 ]
let expr_EFalse : Value.seq = Memo.appends [ Memo.from_constructor 14 ]
let expr_EIf x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 15; x0; x1; x2 ]
let expr_ENil : Value.seq = Memo.appends [ Memo.from_constructor 16 ]
let expr_ECons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 17; x0; x1 ]
let expr_EMatchList x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 18; x0; x1; x2 ]
let expr_EFix x0 : Value.seq = Memo.appends [ Memo.from_constructor 19; x0 ]
let expr_EHole : Value.seq = Memo.appends [ Memo.from_constructor 20 ]

let from_ocaml_expr x =
  match x with
  | EInt x0 -> expr_EInt x0
  | EPlus (x0, x1) -> expr_EPlus x0 x1
  | EVar x0 -> expr_EVar x0
  | EAbs x0 -> expr_EAbs x0
  | EApp (x0, x1) -> expr_EApp x0 x1
  | ELet (x0, x1) -> expr_ELet x0 x1
  | ETrue -> expr_ETrue
  | EFalse -> expr_EFalse
  | EIf (x0, x1, x2) -> expr_EIf x0 x1 x2
  | ENil -> expr_ENil
  | ECons (x0, x1) -> expr_ECons x0 x1
  | EMatchList (x0, x1, x2) -> expr_EMatchList x0 x1 x2
  | EFix x0 -> expr_EFix x0
  | EHole -> expr_EHole

let to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 7 ->
      let [ x0 ] = Memo.splits t in
      EInt x0
  | 8 ->
      let [ x0; x1 ] = Memo.splits t in
      EPlus (x0, x1)
  | 9 ->
      let [ x0 ] = Memo.splits t in
      EVar x0
  | 10 ->
      let [ x0 ] = Memo.splits t in
      EAbs x0
  | 11 ->
      let [ x0; x1 ] = Memo.splits t in
      EApp (x0, x1)
  | 12 ->
      let [ x0; x1 ] = Memo.splits t in
      ELet (x0, x1)
  | 13 -> ETrue
  | 14 -> EFalse
  | 15 ->
      let [ x0; x1; x2 ] = Memo.splits t in
      EIf (x0, x1, x2)
  | 16 -> ENil
  | 17 ->
      let [ x0; x1 ] = Memo.splits t in
      ECons (x0, x1)
  | 18 ->
      let [ x0; x1; x2 ] = Memo.splits t in
      EMatchList (x0, x1, x2)
  | 19 ->
      let [ x0 ] = Memo.splits t in
      EFix x0
  | 20 -> EHole
  | _ -> failwith "unreachable"

type ocaml_value =
  | VInt of Value.seq
  | VAbs of Value.seq * Value.seq
  | VTrue
  | VFalse
  | VNil
  | VCons of Value.seq * Value.seq
  | VFix of Value.seq * Value.seq
  | VStuck of Value.seq

let value_VInt x0 : Value.seq = Memo.appends [ Memo.from_constructor 21; x0 ]
let value_VAbs x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 22; x0; x1 ]
let value_VTrue : Value.seq = Memo.appends [ Memo.from_constructor 23 ]
let value_VFalse : Value.seq = Memo.appends [ Memo.from_constructor 24 ]
let value_VNil : Value.seq = Memo.appends [ Memo.from_constructor 25 ]
let value_VCons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 26; x0; x1 ]
let value_VFix x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 27; x0; x1 ]
let value_VStuck x0 : Value.seq = Memo.appends [ Memo.from_constructor 28; x0 ]

let from_ocaml_value x =
  match x with
  | VInt x0 -> value_VInt x0
  | VAbs (x0, x1) -> value_VAbs x0 x1
  | VTrue -> value_VTrue
  | VFalse -> value_VFalse
  | VNil -> value_VNil
  | VCons (x0, x1) -> value_VCons x0 x1
  | VFix (x0, x1) -> value_VFix x0 x1
  | VStuck x0 -> value_VStuck x0

let to_ocaml_value x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 21 ->
      let [ x0 ] = Memo.splits t in
      VInt x0
  | 22 ->
      let [ x0; x1 ] = Memo.splits t in
      VAbs (x0, x1)
  | 23 -> VTrue
  | 24 -> VFalse
  | 25 -> VNil
  | 26 ->
      let [ x0; x1 ] = Memo.splits t in
      VCons (x0, x1)
  | 27 ->
      let [ x0; x1 ] = Memo.splits t in
      VFix (x0, x1)
  | 28 ->
      let [ x0 ] = Memo.splits t in
      VStuck x0
  | _ -> failwith "unreachable"

type ocaml_vtype = VTInt | VTFunc | VTBool | VTList

let vtype_VTInt : Value.seq = Memo.appends [ Memo.from_constructor 29 ]
let vtype_VTFunc : Value.seq = Memo.appends [ Memo.from_constructor 30 ]
let vtype_VTBool : Value.seq = Memo.appends [ Memo.from_constructor 31 ]
let vtype_VTList : Value.seq = Memo.appends [ Memo.from_constructor 32 ]

let from_ocaml_vtype x =
  match x with VTInt -> vtype_VTInt | VTFunc -> vtype_VTFunc | VTBool -> vtype_VTBool | VTList -> vtype_VTList

let to_ocaml_vtype x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with 29 -> VTInt | 30 -> VTFunc | 31 -> VTBool | 32 -> VTList | _ -> failwith "unreachable"

type ocaml_stuck =
  | SHole of Value.seq
  | STypeError of Value.seq * Value.seq
  | SIndexError
  | SApp of Value.seq * Value.seq
  | SAdd0 of Value.seq * Value.seq
  | SAdd1 of Value.seq * Value.seq
  | SIf of Value.seq * Value.seq * Value.seq
  | SMatchList of Value.seq * Value.seq * Value.seq

let stuck_SHole x0 : Value.seq = Memo.appends [ Memo.from_constructor 33; x0 ]
let stuck_STypeError x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 34; x0; x1 ]
let stuck_SIndexError : Value.seq = Memo.appends [ Memo.from_constructor 35 ]
let stuck_SApp x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 36; x0; x1 ]
let stuck_SAdd0 x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 37; x0; x1 ]
let stuck_SAdd1 x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 38; x0; x1 ]
let stuck_SIf x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 39; x0; x1; x2 ]
let stuck_SMatchList x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 40; x0; x1; x2 ]

let from_ocaml_stuck x =
  match x with
  | SHole x0 -> stuck_SHole x0
  | STypeError (x0, x1) -> stuck_STypeError x0 x1
  | SIndexError -> stuck_SIndexError
  | SApp (x0, x1) -> stuck_SApp x0 x1
  | SAdd0 (x0, x1) -> stuck_SAdd0 x0 x1
  | SAdd1 (x0, x1) -> stuck_SAdd1 x0 x1
  | SIf (x0, x1, x2) -> stuck_SIf x0 x1 x2
  | SMatchList (x0, x1, x2) -> stuck_SMatchList x0 x1 x2

let to_ocaml_stuck x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 33 ->
      let [ x0 ] = Memo.splits t in
      SHole x0
  | 34 ->
      let [ x0; x1 ] = Memo.splits t in
      STypeError (x0, x1)
  | 35 -> SIndexError
  | 36 ->
      let [ x0; x1 ] = Memo.splits t in
      SApp (x0, x1)
  | 37 ->
      let [ x0; x1 ] = Memo.splits t in
      SAdd0 (x0, x1)
  | 38 ->
      let [ x0; x1 ] = Memo.splits t in
      SAdd1 (x0, x1)
  | 39 ->
      let [ x0; x1; x2 ] = Memo.splits t in
      SIf (x0, x1, x2)
  | 40 ->
      let [ x0; x1; x2 ] = Memo.splits t in
      SMatchList (x0, x1, x2)
  | _ -> failwith "unreachable"

let rec index (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let rec eval (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let () =
  add_exp
    (fun w_5 ->
      assert_env_length w_5 1;
      match resolve w_5 K with
      | None -> ()
      | Some (hd, tl) -> (
          match Word.get_value hd with
          | 0 -> exec_done w_5
          | 41 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 42 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 2 tl;
              assert_env_length w_5 3;
              push_env w_5 (Dynarray.get w_5.state.e 2);
              w_5.state.c <- pc_to_exp (int_to_pc 6);
              stepped w_5
          | 43 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              w_5.state.c <- pc_to_exp (int_to_pc 7);
              stepped w_5
          | 44 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 45 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 2 tl;
              assert_env_length w_5 3;
              push_env w_5 (Dynarray.get w_5.state.e 0);
              assert_env_length w_5 4;
              let x1_4 = pop_env w_5 in
              let x0_11 = pop_env w_5 in
              push_env w_5 (Memo.appends [ Memo.from_constructor 4; x0_11; x1_4 ]);
              assert_env_length w_5 3;
              let keep_9 = env_call w_5 [] 2 in
              w_5.state.k <- Memo.appends [ Memo.from_constructor 44; keep_9; w_5.state.k ];
              w_5.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_5
          | 46 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 2 tl;
              assert_env_length w_5 3;
              push_env w_5 (Dynarray.get w_5.state.e 2);
              w_5.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_5
          | 47 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 4 tl;
              w_5.state.c <- pc_to_exp (int_to_pc 9);
              stepped w_5
          | 48 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 2 tl;
              assert_env_length w_5 3;
              push_env w_5 (Dynarray.get w_5.state.e 1);
              assert_env_length w_5 4;
              push_env w_5 (Dynarray.get w_5.state.e 0);
              assert_env_length w_5 5;
              let keep_14 = env_call w_5 [ 2 ] 2 in
              w_5.state.k <- Memo.appends [ Memo.from_constructor 57; keep_14; w_5.state.k ];
              w_5.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_5
          | 49 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 3 tl;
              w_5.state.c <- pc_to_exp (int_to_pc 10);
              stepped w_5
          | 50 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 2 tl;
              assert_env_length w_5 3;
              push_env w_5 (Dynarray.get w_5.state.e 2);
              w_5.state.c <- pc_to_exp (int_to_pc 12);
              stepped w_5
          | 51 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 1 tl;
              assert_env_length w_5 2;
              drop_n w_5 2 0;
              assert_env_length w_5 2;
              drop_n w_5 2 1;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 52 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 3 tl;
              assert_env_length w_5 4;
              push_env w_5 (Dynarray.get w_5.state.e 1);
              assert_env_length w_5 5;
              let x1_15 = pop_env w_5 in
              let x0_32 = pop_env w_5 in
              push_env w_5 (Memo.appends [ Memo.from_constructor 4; x0_32; x1_15 ]);
              assert_env_length w_5 4;
              let keep_17 = env_call w_5 [ 2 ] 2 in
              w_5.state.k <- Memo.appends [ Memo.from_constructor 51; keep_17; w_5.state.k ];
              w_5.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_5
          | 53 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 1 tl;
              assert_env_length w_5 2;
              drop_n w_5 2 0;
              assert_env_length w_5 2;
              drop_n w_5 2 1;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 54 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 3 tl;
              assert_env_length w_5 4;
              push_env w_5 (Dynarray.get w_5.state.e 0);
              assert_env_length w_5 5;
              push_env w_5 (Dynarray.get w_5.state.e 1);
              assert_env_length w_5 6;
              let x1_16 = pop_env w_5 in
              let x0_33 = pop_env w_5 in
              push_env w_5 (Memo.appends [ Memo.from_constructor 4; x0_33; x1_16 ]);
              assert_env_length w_5 5;
              let x1_17 = pop_env w_5 in
              let x0_34 = pop_env w_5 in
              push_env w_5 (Memo.appends [ Memo.from_constructor 4; x0_34; x1_17 ]);
              assert_env_length w_5 4;
              let keep_18 = env_call w_5 [ 2 ] 2 in
              w_5.state.k <- Memo.appends [ Memo.from_constructor 53; keep_18; w_5.state.k ];
              w_5.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_5
          | 55 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 56 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 57 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 1 tl;
              assert_env_length w_5 2;
              let x1_18 = pop_env w_5 in
              let x0_35 = pop_env w_5 in
              push_env w_5 (Memo.appends [ Memo.from_constructor 26; x0_35; x1_18 ]);
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 58 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | 59 ->
              w_5.state.k <- get_next_cont tl;
              restore_env w_5 0 tl;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              drop_n w_5 1 0;
              assert_env_length w_5 1;
              return_n w_5 1 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
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
      match resolve w_2 last_1 with
      | None -> ()
      | Some x_1 -> (
          ignore (pop_env w_2);
          match Word.get_value (fst x_1) with
          | 1 ->
              assert_env_length w_2 4;
              push_env w_2 (Dynarray.get w_2.state.e 2);
              assert_env_length w_2 5;
              let x0_0 = pop_env w_2 in
              push_env w_2 (Memo.appends [ Memo.from_constructor 6; x0_0 ]);
              assert_env_length w_2 5;
              drop_n w_2 5 2;
              assert_env_length w_2 3;
              return_n w_2 3 (pc_to_exp (int_to_pc 0))
          | 2 ->
              let splits_1 = Memo.splits (snd x_1) in
              let split0_1 = List.nth splits_1 0 in
              push_env w_2 split0_1;
              assert_env_length w_2 5;
              push_env w_2 (Dynarray.get w_2.state.e 3);
              assert_env_length w_2 6;
              push_env w_2 (Dynarray.get w_2.state.e 4);
              assert_env_length w_2 7;
              let keep_0 = env_call w_2 [] 2 in
              w_2.state.k <- Memo.appends [ Memo.from_constructor 41; keep_0; w_2.state.k ];
              w_2.state.c <- pc_to_exp (int_to_pc 1);
              stepped w_2
          | _ -> failwith "unreachable"))
    2

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      let last_0 = Source.E 2 in
      match resolve w_1 last_0 with
      | None -> ()
      | Some x_0 -> (
          ignore (pop_env w_1);
          match Word.get_value (fst x_0) with
          | 4 ->
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
              push_env w_1 (Memo.from_constructor 5);
              assert_env_length w_1 3;
              return_n w_1 3 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
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
      match resolve w_4 last_2 with
      | None -> ()
      | Some x_2 -> (
          ignore (pop_env w_4);
          match Word.get_value (fst x_2) with
          | 7 ->
              let splits_2 = Memo.splits (snd x_2) in
              let split0_2 = List.nth splits_2 0 in
              push_env w_4 split0_2;
              assert_env_length w_4 3;
              push_env w_4 (Dynarray.get w_4.state.e 2);
              assert_env_length w_4 4;
              let x0_1 = pop_env w_4 in
              push_env w_4 (Memo.appends [ Memo.from_constructor 21; x0_1 ]);
              assert_env_length w_4 4;
              drop_n w_4 4 1;
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 8 ->
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
              let keep_1 = env_call w_4 [ 1; 3 ] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 42; keep_1; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_4
          | 9 ->
              let splits_4 = Memo.splits (snd x_2) in
              let split0_4 = List.nth splits_4 0 in
              push_env w_4 split0_4;
              assert_env_length w_4 3;
              push_env w_4 (Dynarray.get w_4.state.e 1);
              assert_env_length w_4 4;
              push_env w_4 (Dynarray.get w_4.state.e 2);
              assert_env_length w_4 5;
              let keep_2 = env_call w_4 [] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 43; keep_2; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 1);
              stepped w_4
          | 10 ->
              let splits_5 = Memo.splits (snd x_2) in
              let split0_5 = List.nth splits_5 0 in
              push_env w_4 split0_5;
              assert_env_length w_4 3;
              push_env w_4 (Dynarray.get w_4.state.e 2);
              assert_env_length w_4 4;
              push_env w_4 (Dynarray.get w_4.state.e 1);
              assert_env_length w_4 5;
              let x1_0 = pop_env w_4 in
              let x0_2 = pop_env w_4 in
              push_env w_4 (Memo.appends [ Memo.from_constructor 22; x0_2; x1_0 ]);
              assert_env_length w_4 4;
              drop_n w_4 4 1;
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 12 ->
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
              let keep_3 = env_call w_4 [ 1; 4 ] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 45; keep_3; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_4
          | 19 ->
              let splits_7 = Memo.splits (snd x_2) in
              let split0_7 = List.nth splits_7 0 in
              push_env w_4 split0_7;
              assert_env_length w_4 3;
              push_env w_4 (Dynarray.get w_4.state.e 2);
              assert_env_length w_4 4;
              push_env w_4 (Dynarray.get w_4.state.e 1);
              assert_env_length w_4 5;
              let x1_1 = pop_env w_4 in
              let x0_3 = pop_env w_4 in
              push_env w_4 (Memo.appends [ Memo.from_constructor 27; x0_3; x1_1 ]);
              assert_env_length w_4 4;
              drop_n w_4 4 1;
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 11 ->
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
              let keep_4 = env_call w_4 [ 1; 3 ] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 46; keep_4; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_4
          | 20 ->
              assert_env_length w_4 2;
              push_env w_4 (Dynarray.get w_4.state.e 1);
              assert_env_length w_4 3;
              let x0_4 = pop_env w_4 in
              push_env w_4 (Memo.appends [ Memo.from_constructor 33; x0_4 ]);
              assert_env_length w_4 3;
              let x0_5 = pop_env w_4 in
              push_env w_4 (Memo.appends [ Memo.from_constructor 28; x0_5 ]);
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 13 ->
              assert_env_length w_4 2;
              push_env w_4 (Memo.from_constructor 23);
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 14 ->
              assert_env_length w_4 2;
              push_env w_4 (Memo.from_constructor 24);
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 15 ->
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
              let keep_5 = env_call w_4 [ 1; 2; 3; 4 ] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 47; keep_5; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_4
          | 16 ->
              assert_env_length w_4 2;
              push_env w_4 (Memo.from_constructor 25);
              assert_env_length w_4 3;
              return_n w_4 3 (pc_to_exp (int_to_pc 0))
          | 17 ->
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
              let keep_6 = env_call w_4 [ 1; 3 ] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 48; keep_6; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_4
          | 18 ->
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
              let keep_7 = env_call w_4 [ 1; 3; 4 ] 2 in
              w_4.state.k <- Memo.appends [ Memo.from_constructor 49; keep_7; w_4.state.k ];
              w_4.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_4
          | _ -> failwith "unreachable"))
    5

let () =
  add_exp
    (fun w_6 ->
      assert_env_length w_6 4;
      let last_3 = Source.E 3 in
      match resolve w_6 last_3 with
      | None -> ()
      | Some x_3 -> (
          ignore (pop_env w_6);
          match Word.get_value (fst x_3) with
          | 21 ->
              let splits_12 = Memo.splits (snd x_3) in
              let split0_12 = List.nth splits_12 0 in
              push_env w_6 split0_12;
              assert_env_length w_6 4;
              push_env w_6 (Dynarray.get w_6.state.e 1);
              assert_env_length w_6 5;
              push_env w_6 (Dynarray.get w_6.state.e 0);
              assert_env_length w_6 6;
              let keep_8 = env_call w_6 [ 2; 3 ] 2 in
              w_6.state.k <- Memo.appends [ Memo.from_constructor 50; keep_8; w_6.state.k ];
              w_6.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_6
          | 28 ->
              let splits_13 = Memo.splits (snd x_3) in
              let split0_13 = List.nth splits_13 0 in
              push_env w_6 split0_13;
              assert_env_length w_6 4;
              push_env w_6 (Dynarray.get w_6.state.e 2);
              assert_env_length w_6 5;
              push_env w_6 (Dynarray.get w_6.state.e 1);
              assert_env_length w_6 6;
              let x1_2 = pop_env w_6 in
              let x0_6 = pop_env w_6 in
              push_env w_6 (Memo.appends [ Memo.from_constructor 37; x0_6; x1_2 ]);
              assert_env_length w_6 5;
              let x0_7 = pop_env w_6 in
              push_env w_6 (Memo.appends [ Memo.from_constructor 28; x0_7 ]);
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
              push_env w_6 (Memo.from_constructor 29);
              assert_env_length w_6 5;
              let x1_3 = pop_env w_6 in
              let x0_8 = pop_env w_6 in
              push_env w_6 (Memo.appends [ Memo.from_constructor 34; x0_8; x1_3 ]);
              assert_env_length w_6 4;
              let x0_9 = pop_env w_6 in
              push_env w_6 (Memo.appends [ Memo.from_constructor 28; x0_9 ]);
              assert_env_length w_6 4;
              drop_n w_6 4 1;
              assert_env_length w_6 3;
              drop_n w_6 3 1;
              assert_env_length w_6 2;
              return_n w_6 2 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
    6

let () =
  add_exp
    (fun w_7 ->
      assert_env_length w_7 1;
      let last_4 = Source.E 0 in
      match resolve w_7 last_4 with
      | None -> ()
      | Some x_4 -> (
          ignore (pop_env w_7);
          match Word.get_value (fst x_4) with
          | 6 ->
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
          | 5 ->
              assert_env_length w_7 0;
              push_env w_7 (Memo.from_constructor 35);
              assert_env_length w_7 1;
              let x0_10 = pop_env w_7 in
              push_env w_7 (Memo.appends [ Memo.from_constructor 28; x0_10 ]);
              assert_env_length w_7 1;
              drop_n w_7 1 0;
              assert_env_length w_7 1;
              return_n w_7 1 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
    7

let () =
  add_exp
    (fun w_8 ->
      assert_env_length w_8 4;
      let last_5 = Source.E 3 in
      match resolve w_8 last_5 with
      | None -> ()
      | Some x_5 -> (
          ignore (pop_env w_8);
          match Word.get_value (fst x_5) with
          | 22 ->
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
              let keep_10 = env_call w_8 [ 2; 4; 5 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 52; keep_10; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_8
          | 27 ->
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
              let keep_11 = env_call w_8 [ 2; 4; 5 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 54; keep_11; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_8
          | 28 ->
              let splits_17 = Memo.splits (snd x_5) in
              let split0_17 = List.nth splits_17 0 in
              push_env w_8 split0_17;
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 3);
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 6;
              let x1_5 = pop_env w_8 in
              let x0_12 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 36; x0_12; x1_5 ]);
              assert_env_length w_8 5;
              let x0_13 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 28; x0_13 ]);
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
              push_env w_8 (Memo.from_constructor 30);
              assert_env_length w_8 5;
              let x1_6 = pop_env w_8 in
              let x0_14 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 34; x0_14; x1_6 ]);
              assert_env_length w_8 4;
              let x0_15 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 28; x0_15 ]);
              assert_env_length w_8 4;
              drop_n w_8 4 1;
              assert_env_length w_8 3;
              drop_n w_8 3 1;
              assert_env_length w_8 2;
              return_n w_8 2 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
    8

let () =
  add_exp
    (fun w_9 ->
      assert_env_length w_9 5;
      let last_6 = Source.E 4 in
      match resolve w_9 last_6 with
      | None -> ()
      | Some x_6 -> (
          ignore (pop_env w_9);
          match Word.get_value (fst x_6) with
          | 23 ->
              assert_env_length w_9 4;
              push_env w_9 (Dynarray.get w_9.state.e 2);
              assert_env_length w_9 5;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              assert_env_length w_9 6;
              let keep_12 = env_call w_9 [] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 55; keep_12; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_9
          | 24 ->
              assert_env_length w_9 4;
              push_env w_9 (Dynarray.get w_9.state.e 3);
              assert_env_length w_9 5;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              assert_env_length w_9 6;
              let keep_13 = env_call w_9 [] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 56; keep_13; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_9
          | 28 ->
              let splits_18 = Memo.splits (snd x_6) in
              let split0_18 = List.nth splits_18 0 in
              push_env w_9 split0_18;
              assert_env_length w_9 5;
              push_env w_9 (Dynarray.get w_9.state.e 4);
              assert_env_length w_9 6;
              push_env w_9 (Dynarray.get w_9.state.e 2);
              assert_env_length w_9 7;
              push_env w_9 (Dynarray.get w_9.state.e 3);
              assert_env_length w_9 8;
              let x2_0 = pop_env w_9 in
              let x1_7 = pop_env w_9 in
              let x0_16 = pop_env w_9 in
              push_env w_9 (Memo.appends [ Memo.from_constructor 39; x0_16; x1_7; x2_0 ]);
              assert_env_length w_9 6;
              let x0_17 = pop_env w_9 in
              push_env w_9 (Memo.appends [ Memo.from_constructor 28; x0_17 ]);
              assert_env_length w_9 6;
              drop_n w_9 6 1;
              assert_env_length w_9 5;
              drop_n w_9 5 3;
              assert_env_length w_9 2;
              return_n w_9 2 (pc_to_exp (int_to_pc 0))
          | _ ->
              assert_env_length w_9 4;
              push_env w_9 (Dynarray.get w_9.state.e 1);
              assert_env_length w_9 5;
              push_env w_9 (Memo.from_constructor 31);
              assert_env_length w_9 6;
              let x1_8 = pop_env w_9 in
              let x0_18 = pop_env w_9 in
              push_env w_9 (Memo.appends [ Memo.from_constructor 34; x0_18; x1_8 ]);
              assert_env_length w_9 5;
              let x0_19 = pop_env w_9 in
              push_env w_9 (Memo.appends [ Memo.from_constructor 28; x0_19 ]);
              assert_env_length w_9 5;
              drop_n w_9 5 3;
              assert_env_length w_9 2;
              return_n w_9 2 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
    9

let () =
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      let last_7 = Source.E 3 in
      match resolve w_10 last_7 with
      | None -> ()
      | Some x_7 -> (
          ignore (pop_env w_10);
          match Word.get_value (fst x_7) with
          | 25 ->
              assert_env_length w_10 3;
              push_env w_10 (Dynarray.get w_10.state.e 1);
              assert_env_length w_10 4;
              push_env w_10 (Dynarray.get w_10.state.e 0);
              assert_env_length w_10 5;
              let keep_15 = env_call w_10 [] 2 in
              w_10.state.k <- Memo.appends [ Memo.from_constructor 58; keep_15; w_10.state.k ];
              w_10.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_10
          | 26 ->
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
              let x1_9 = pop_env w_10 in
              let x0_20 = pop_env w_10 in
              push_env w_10 (Memo.appends [ Memo.from_constructor 4; x0_20; x1_9 ]);
              assert_env_length w_10 8;
              let x1_10 = pop_env w_10 in
              let x0_21 = pop_env w_10 in
              push_env w_10 (Memo.appends [ Memo.from_constructor 4; x0_21; x1_10 ]);
              assert_env_length w_10 7;
              let keep_16 = env_call w_10 [] 2 in
              w_10.state.k <- Memo.appends [ Memo.from_constructor 59; keep_16; w_10.state.k ];
              w_10.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_10
          | 28 ->
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
              let x2_1 = pop_env w_10 in
              let x1_11 = pop_env w_10 in
              let x0_22 = pop_env w_10 in
              push_env w_10 (Memo.appends [ Memo.from_constructor 40; x0_22; x1_11; x2_1 ]);
              assert_env_length w_10 5;
              let x0_23 = pop_env w_10 in
              push_env w_10 (Memo.appends [ Memo.from_constructor 28; x0_23 ]);
              assert_env_length w_10 5;
              drop_n w_10 5 1;
              assert_env_length w_10 4;
              drop_n w_10 4 2;
              assert_env_length w_10 2;
              return_n w_10 2 (pc_to_exp (int_to_pc 0))
          | _ ->
              assert_env_length w_10 3;
              push_env w_10 (Memo.from_constructor 32);
              assert_env_length w_10 4;
              let x0_24 = pop_env w_10 in
              push_env w_10 (Memo.appends [ Memo.from_constructor 34; x0_24 ]);
              assert_env_length w_10 4;
              let x0_25 = pop_env w_10 in
              push_env w_10 (Memo.appends [ Memo.from_constructor 28; x0_25 ]);
              assert_env_length w_10 4;
              drop_n w_10 4 2;
              assert_env_length w_10 2;
              return_n w_10 2 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
    10

let () =
  add_exp
    (fun w_12 ->
      assert_env_length w_12 6;
      match resolve w_12 (Source.E 4) with
      | None -> ()
      | Some x0_26 -> (
          match resolve w_12 (Source.E 5) with
          | None -> ()
          | Some x1_12 ->
              ignore (pop_env w_12);
              ignore (pop_env w_12);
              push_env w_12 (Memo.from_int (Word.to_int (fst x0_26) + Word.to_int (fst x1_12)));
              assert_env_length w_12 5;
              let x0_27 = pop_env w_12 in
              push_env w_12 (Memo.appends [ Memo.from_constructor 21; x0_27 ]);
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
              return_n w_12 1 (pc_to_exp (int_to_pc 0))))
    11

let () =
  add_exp
    (fun w_11 ->
      assert_env_length w_11 4;
      let last_8 = Source.E 3 in
      match resolve w_11 last_8 with
      | None -> ()
      | Some x_8 -> (
          ignore (pop_env w_11);
          match Word.get_value (fst x_8) with
          | 21 ->
              let splits_21 = Memo.splits (snd x_8) in
              let split0_21 = List.nth splits_21 0 in
              push_env w_11 split0_21;
              assert_env_length w_11 4;
              push_env w_11 (Dynarray.get w_11.state.e 1);
              assert_env_length w_11 5;
              push_env w_11 (Dynarray.get w_11.state.e 3);
              w_11.state.c <- pc_to_exp (int_to_pc 11);
              stepped w_11
          | 28 ->
              let splits_22 = Memo.splits (snd x_8) in
              let split0_22 = List.nth splits_22 0 in
              push_env w_11 split0_22;
              assert_env_length w_11 4;
              push_env w_11 (Dynarray.get w_11.state.e 0);
              assert_env_length w_11 5;
              push_env w_11 (Dynarray.get w_11.state.e 3);
              assert_env_length w_11 6;
              let x1_13 = pop_env w_11 in
              let x0_28 = pop_env w_11 in
              push_env w_11 (Memo.appends [ Memo.from_constructor 38; x0_28; x1_13 ]);
              assert_env_length w_11 5;
              let x0_29 = pop_env w_11 in
              push_env w_11 (Memo.appends [ Memo.from_constructor 28; x0_29 ]);
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
              push_env w_11 (Memo.from_constructor 21);
              assert_env_length w_11 5;
              let x1_14 = pop_env w_11 in
              let x0_30 = pop_env w_11 in
              push_env w_11 (Memo.appends [ Memo.from_constructor 34; x0_30; x1_14 ]);
              assert_env_length w_11 4;
              let x0_31 = pop_env w_11 in
              push_env w_11 (Memo.appends [ Memo.from_constructor 28; x0_31 ]);
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
          | _ -> failwith "unreachable"))
    12

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 0
let () = Value.set_constructor_degree 3 1
let () = Value.set_constructor_degree 4 (-1)
let () = Value.set_constructor_degree 5 1
let () = Value.set_constructor_degree 6 0
let () = Value.set_constructor_degree 7 0
let () = Value.set_constructor_degree 8 (-1)
let () = Value.set_constructor_degree 9 0
let () = Value.set_constructor_degree 10 0
let () = Value.set_constructor_degree 11 (-1)
let () = Value.set_constructor_degree 12 (-1)
let () = Value.set_constructor_degree 13 1
let () = Value.set_constructor_degree 14 1
let () = Value.set_constructor_degree 15 (-2)
let () = Value.set_constructor_degree 16 1
let () = Value.set_constructor_degree 17 (-1)
let () = Value.set_constructor_degree 18 (-2)
let () = Value.set_constructor_degree 19 0
let () = Value.set_constructor_degree 20 1
let () = Value.set_constructor_degree 21 0
let () = Value.set_constructor_degree 22 (-1)
let () = Value.set_constructor_degree 23 1
let () = Value.set_constructor_degree 24 1
let () = Value.set_constructor_degree 25 1
let () = Value.set_constructor_degree 26 (-1)
let () = Value.set_constructor_degree 27 (-1)
let () = Value.set_constructor_degree 28 0
let () = Value.set_constructor_degree 29 1
let () = Value.set_constructor_degree 30 1
let () = Value.set_constructor_degree 31 1
let () = Value.set_constructor_degree 32 1
let () = Value.set_constructor_degree 33 0
let () = Value.set_constructor_degree 34 (-1)
let () = Value.set_constructor_degree 35 1
let () = Value.set_constructor_degree 36 (-1)
let () = Value.set_constructor_degree 37 (-1)
let () = Value.set_constructor_degree 38 (-1)
let () = Value.set_constructor_degree 39 (-2)
let () = Value.set_constructor_degree 40 (-2)
let () = Value.set_constructor_degree 41 0
let () = Value.set_constructor_degree 42 (-2)
let () = Value.set_constructor_degree 43 0
let () = Value.set_constructor_degree 44 0
let () = Value.set_constructor_degree 45 (-2)
let () = Value.set_constructor_degree 46 (-2)
let () = Value.set_constructor_degree 47 (-4)
let () = Value.set_constructor_degree 48 (-2)
let () = Value.set_constructor_degree 49 (-3)
let () = Value.set_constructor_degree 50 (-2)
let () = Value.set_constructor_degree 51 (-1)
let () = Value.set_constructor_degree 52 (-3)
let () = Value.set_constructor_degree 53 (-1)
let () = Value.set_constructor_degree 54 (-3)
let () = Value.set_constructor_degree 55 0
let () = Value.set_constructor_degree 56 0
let () = Value.set_constructor_degree 57 (-1)
let () = Value.set_constructor_degree 58 0
let () = Value.set_constructor_degree 59 0
