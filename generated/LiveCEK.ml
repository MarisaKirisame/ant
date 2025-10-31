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

let expr_EInt x0 : Value.seq = Memo.appends [ Memo.from_constructor 5; x0 ]
let expr_EPlus x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 6; x0; x1 ]
let expr_EVar x0 : Value.seq = Memo.appends [ Memo.from_constructor 7; x0 ]
let expr_EAbs x0 : Value.seq = Memo.appends [ Memo.from_constructor 8; x0 ]
let expr_EApp x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 9; x0; x1 ]
let expr_ELet x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 10; x0; x1 ]
let expr_ETrue : Value.seq = Memo.appends [ Memo.from_constructor 11 ]
let expr_EFalse : Value.seq = Memo.appends [ Memo.from_constructor 12 ]
let expr_EIf x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 13; x0; x1; x2 ]
let expr_ENil : Value.seq = Memo.appends [ Memo.from_constructor 14 ]
let expr_ECons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 15; x0; x1 ]
let expr_EMatchList x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 16; x0; x1; x2 ]
let expr_EFix x0 : Value.seq = Memo.appends [ Memo.from_constructor 17; x0 ]
let expr_EHole : Value.seq = Memo.appends [ Memo.from_constructor 18 ]

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
  | 5 ->
      let [ x0 ] = Memo.splits t in
      EInt x0
  | 6 ->
      let [ x0; x1 ] = Memo.splits t in
      EPlus (x0, x1)
  | 7 ->
      let [ x0 ] = Memo.splits t in
      EVar x0
  | 8 ->
      let [ x0 ] = Memo.splits t in
      EAbs x0
  | 9 ->
      let [ x0; x1 ] = Memo.splits t in
      EApp (x0, x1)
  | 10 ->
      let [ x0; x1 ] = Memo.splits t in
      ELet (x0, x1)
  | 11 -> ETrue
  | 12 -> EFalse
  | 13 ->
      let [ x0; x1; x2 ] = Memo.splits t in
      EIf (x0, x1, x2)
  | 14 -> ENil
  | 15 ->
      let [ x0; x1 ] = Memo.splits t in
      ECons (x0, x1)
  | 16 ->
      let [ x0; x1; x2 ] = Memo.splits t in
      EMatchList (x0, x1, x2)
  | 17 ->
      let [ x0 ] = Memo.splits t in
      EFix x0
  | 18 -> EHole
  | _ -> failwith "unreachable"

type ocaml_value =
  | VInt of Value.seq
  | VAbs of Value.seq * Value.seq
  | VTrue
  | VFalse
  | VNil
  | VCons of Value.seq * Value.seq
  | VFix of Value.seq * Value.seq

let value_VInt x0 : Value.seq = Memo.appends [ Memo.from_constructor 19; x0 ]
let value_VAbs x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 20; x0; x1 ]
let value_VTrue : Value.seq = Memo.appends [ Memo.from_constructor 21 ]
let value_VFalse : Value.seq = Memo.appends [ Memo.from_constructor 22 ]
let value_VNil : Value.seq = Memo.appends [ Memo.from_constructor 23 ]
let value_VCons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 24; x0; x1 ]
let value_VFix x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 25; x0; x1 ]

let from_ocaml_value x =
  match x with
  | VInt x0 -> value_VInt x0
  | VAbs (x0, x1) -> value_VAbs x0 x1
  | VTrue -> value_VTrue
  | VFalse -> value_VFalse
  | VNil -> value_VNil
  | VCons (x0, x1) -> value_VCons x0 x1
  | VFix (x0, x1) -> value_VFix x0 x1

let to_ocaml_value x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 19 ->
      let [ x0 ] = Memo.splits t in
      VInt x0
  | 20 ->
      let [ x0; x1 ] = Memo.splits t in
      VAbs (x0, x1)
  | 21 -> VTrue
  | 22 -> VFalse
  | 23 -> VNil
  | 24 ->
      let [ x0; x1 ] = Memo.splits t in
      VCons (x0, x1)
  | 25 ->
      let [ x0; x1 ] = Memo.splits t in
      VFix (x0, x1)
  | _ -> failwith "unreachable"

let rec index (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let rec vadd (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp (int_to_pc 4)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let rec eval (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp (int_to_pc 8)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let () =
  add_exp
    (fun w_9 ->
      assert_env_length w_9 1;
      match resolve w_9 K with
      | None -> ()
      | Some (hd, tl) -> (
          match Word.get_value hd with
          | 0 -> exec_done w_9
          | 26 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 27 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 28 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 2 tl;
              assert_env_length w_9 3;
              push_env w_9 (Dynarray.get w_9.state.e 1);
              assert_env_length w_9 4;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              assert_env_length w_9 5;
              let keep_8 = env_call w_9 [ 2 ] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 36; keep_8; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_9
          | 29 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 30 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 31 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 2 tl;
              assert_env_length w_9 3;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              assert_env_length w_9 4;
              let x1_3 = pop_env w_9 in
              let x0_5 = pop_env w_9 in
              push_env w_9 (Memo.appends [ Memo.from_constructor 4; x0_5; x1_3 ]);
              assert_env_length w_9 3;
              let keep_9 = env_call w_9 [] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 30; keep_9; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_9
          | 32 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 2 tl;
              assert_env_length w_9 3;
              push_env w_9 (Dynarray.get w_9.state.e 1);
              assert_env_length w_9 4;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              assert_env_length w_9 5;
              let keep_10 = env_call w_9 [ 2 ] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 37; keep_10; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_9
          | 33 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 3 tl;
              w_9.state.c <- pc_to_exp (int_to_pc 10);
              stepped w_9
          | 34 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 2 tl;
              assert_env_length w_9 3;
              push_env w_9 (Dynarray.get w_9.state.e 1);
              assert_env_length w_9 4;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              assert_env_length w_9 5;
              let keep_13 = env_call w_9 [ 2 ] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 40; keep_13; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_9
          | 35 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 3 tl;
              w_9.state.c <- pc_to_exp (int_to_pc 11);
              stepped w_9
          | 36 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 1 tl;
              assert_env_length w_9 2;
              let keep_16 = env_call w_9 [] 2 in
              w_9.state.k <- Memo.appends [ Memo.from_constructor 27; keep_16; w_9.state.k ];
              w_9.state.c <- pc_to_exp (int_to_pc 4);
              stepped w_9
          | 37 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 1 tl;
              assert_env_length w_9 2;
              push_env w_9 (Dynarray.get w_9.state.e 0);
              w_9.state.c <- pc_to_exp (int_to_pc 12);
              stepped w_9
          | 38 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 39 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 40 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 1 tl;
              assert_env_length w_9 2;
              let x1_9 = pop_env w_9 in
              let x0_11 = pop_env w_9 in
              push_env w_9 (Memo.appends [ Memo.from_constructor 24; x0_11; x1_9 ]);
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 41 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 42 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 0 tl;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 43 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 2 tl;
              assert_env_length w_9 3;
              drop_n w_9 3 0;
              assert_env_length w_9 3;
              drop_n w_9 3 1;
              assert_env_length w_9 2;
              drop_n w_9 2 1;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
          | 44 ->
              w_9.state.k <- get_next_cont tl;
              restore_env w_9 2 tl;
              assert_env_length w_9 3;
              drop_n w_9 3 0;
              assert_env_length w_9 3;
              drop_n w_9 3 1;
              assert_env_length w_9 2;
              drop_n w_9 2 1;
              assert_env_length w_9 1;
              drop_n w_9 1 0;
              assert_env_length w_9 1;
              return_n w_9 1 (pc_to_exp (int_to_pc 0))
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
              w_2.state.k <- Memo.appends [ Memo.from_constructor 26; keep_0; w_2.state.k ];
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
          | _ -> failwith "unreachable"))
    3

let () =
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      push_env w_3 (Dynarray.get w_3.state.e 0);
      w_3.state.c <- pc_to_exp (int_to_pc 7);
      stepped w_3)
    4

let () =
  add_exp
    (fun w_6 ->
      assert_env_length w_6 6;
      match resolve w_6 (Source.E 4) with
      | None -> ()
      | Some x0_0 -> (
          match resolve w_6 (Source.E 5) with
          | None -> ()
          | Some x1_0 ->
              ignore (pop_env w_6);
              ignore (pop_env w_6);
              push_env w_6 (Memo.from_int (Word.to_int (fst x0_0) + Word.to_int (fst x1_0)));
              assert_env_length w_6 5;
              let x0_1 = pop_env w_6 in
              push_env w_6 (Memo.appends [ Memo.from_constructor 19; x0_1 ]);
              assert_env_length w_6 5;
              drop_n w_6 5 1;
              assert_env_length w_6 4;
              drop_n w_6 4 1;
              assert_env_length w_6 3;
              return_n w_6 3 (pc_to_exp (int_to_pc 0))))
    5

let () =
  add_exp
    (fun w_5 ->
      assert_env_length w_5 4;
      let last_3 = Source.E 3 in
      match resolve w_5 last_3 with
      | None -> ()
      | Some x_3 -> (
          ignore (pop_env w_5);
          match Word.get_value (fst x_3) with
          | 19 ->
              let splits_3 = Memo.splits (snd x_3) in
              let split0_3 = List.nth splits_3 0 in
              push_env w_5 split0_3;
              assert_env_length w_5 4;
              push_env w_5 (Dynarray.get w_5.state.e 2);
              assert_env_length w_5 5;
              push_env w_5 (Dynarray.get w_5.state.e 3);
              w_5.state.c <- pc_to_exp (int_to_pc 5);
              stepped w_5
          | _ -> failwith "unreachable"))
    6

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
          | 19 ->
              let splits_2 = Memo.splits (snd x_2) in
              let split0_2 = List.nth splits_2 0 in
              push_env w_4 split0_2;
              assert_env_length w_4 3;
              push_env w_4 (Dynarray.get w_4.state.e 1);
              w_4.state.c <- pc_to_exp (int_to_pc 6);
              stepped w_4
          | _ -> failwith "unreachable"))
    7

let () =
  add_exp
    (fun w_7 ->
      assert_env_length w_7 2;
      push_env w_7 (Dynarray.get w_7.state.e 0);
      w_7.state.c <- pc_to_exp (int_to_pc 9);
      stepped w_7)
    8

let () =
  add_exp
    (fun w_8 ->
      assert_env_length w_8 3;
      let last_4 = Source.E 2 in
      match resolve w_8 last_4 with
      | None -> ()
      | Some x_4 -> (
          ignore (pop_env w_8);
          match Word.get_value (fst x_4) with
          | 5 ->
              let splits_4 = Memo.splits (snd x_4) in
              let split0_4 = List.nth splits_4 0 in
              push_env w_8 split0_4;
              assert_env_length w_8 3;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 4;
              let x0_2 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 19; x0_2 ]);
              assert_env_length w_8 4;
              drop_n w_8 4 1;
              assert_env_length w_8 3;
              return_n w_8 3 (pc_to_exp (int_to_pc 0))
          | 6 ->
              let splits_5 = Memo.splits (snd x_4) in
              let split0_5 = List.nth splits_5 0 in
              let split1_1 = List.nth splits_5 1 in
              push_env w_8 split0_5;
              push_env w_8 split1_1;
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 6;
              let keep_1 = env_call w_8 [ 1; 3 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 28; keep_1; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_8
          | 7 ->
              let splits_6 = Memo.splits (snd x_4) in
              let split0_6 = List.nth splits_6 0 in
              push_env w_8 split0_6;
              assert_env_length w_8 3;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 5;
              let keep_2 = env_call w_8 [] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 29; keep_2; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 1);
              stepped w_8
          | 8 ->
              let splits_7 = Memo.splits (snd x_4) in
              let split0_7 = List.nth splits_7 0 in
              push_env w_8 split0_7;
              assert_env_length w_8 3;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 5;
              let x1_1 = pop_env w_8 in
              let x0_3 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 20; x0_3; x1_1 ]);
              assert_env_length w_8 4;
              drop_n w_8 4 1;
              assert_env_length w_8 3;
              return_n w_8 3 (pc_to_exp (int_to_pc 0))
          | 10 ->
              let splits_8 = Memo.splits (snd x_4) in
              let split0_8 = List.nth splits_8 0 in
              let split1_2 = List.nth splits_8 1 in
              push_env w_8 split0_8;
              push_env w_8 split1_2;
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 3);
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 6;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 7;
              let keep_3 = env_call w_8 [ 1; 4 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 31; keep_3; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_8
          | 17 ->
              let splits_9 = Memo.splits (snd x_4) in
              let split0_9 = List.nth splits_9 0 in
              push_env w_8 split0_9;
              assert_env_length w_8 3;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 5;
              let x1_2 = pop_env w_8 in
              let x0_4 = pop_env w_8 in
              push_env w_8 (Memo.appends [ Memo.from_constructor 25; x0_4; x1_2 ]);
              assert_env_length w_8 4;
              drop_n w_8 4 1;
              assert_env_length w_8 3;
              return_n w_8 3 (pc_to_exp (int_to_pc 0))
          | 9 ->
              let splits_10 = Memo.splits (snd x_4) in
              let split0_10 = List.nth splits_10 0 in
              let split1_3 = List.nth splits_10 1 in
              push_env w_8 split0_10;
              push_env w_8 split1_3;
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 6;
              let keep_4 = env_call w_8 [ 1; 3 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 32; keep_4; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_8
          | 11 ->
              assert_env_length w_8 2;
              push_env w_8 (Memo.from_constructor 21);
              assert_env_length w_8 3;
              return_n w_8 3 (pc_to_exp (int_to_pc 0))
          | 12 ->
              assert_env_length w_8 2;
              push_env w_8 (Memo.from_constructor 22);
              assert_env_length w_8 3;
              return_n w_8 3 (pc_to_exp (int_to_pc 0))
          | 13 ->
              let splits_11 = Memo.splits (snd x_4) in
              let split0_11 = List.nth splits_11 0 in
              let split1_4 = List.nth splits_11 1 in
              let split2_0 = List.nth splits_11 2 in
              push_env w_8 split0_11;
              push_env w_8 split1_4;
              push_env w_8 split2_0;
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 6;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 7;
              let keep_5 = env_call w_8 [ 1; 3; 4 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 33; keep_5; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_8
          | 14 ->
              assert_env_length w_8 2;
              push_env w_8 (Memo.from_constructor 23);
              assert_env_length w_8 3;
              return_n w_8 3 (pc_to_exp (int_to_pc 0))
          | 15 ->
              let splits_12 = Memo.splits (snd x_4) in
              let split0_12 = List.nth splits_12 0 in
              let split1_5 = List.nth splits_12 1 in
              push_env w_8 split0_12;
              push_env w_8 split1_5;
              assert_env_length w_8 4;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 6;
              let keep_6 = env_call w_8 [ 1; 3 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 34; keep_6; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_8
          | 16 ->
              let splits_13 = Memo.splits (snd x_4) in
              let split0_13 = List.nth splits_13 0 in
              let split1_6 = List.nth splits_13 1 in
              let split2_1 = List.nth splits_13 2 in
              push_env w_8 split0_13;
              push_env w_8 split1_6;
              push_env w_8 split2_1;
              assert_env_length w_8 5;
              push_env w_8 (Dynarray.get w_8.state.e 2);
              assert_env_length w_8 6;
              push_env w_8 (Dynarray.get w_8.state.e 1);
              assert_env_length w_8 7;
              let keep_7 = env_call w_8 [ 1; 3; 4 ] 2 in
              w_8.state.k <- Memo.appends [ Memo.from_constructor 35; keep_7; w_8.state.k ];
              w_8.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_8
          | _ -> failwith "unreachable"))
    9

let () =
  add_exp
    (fun w_10 ->
      assert_env_length w_10 4;
      let last_5 = Source.E 3 in
      match resolve w_10 last_5 with
      | None -> ()
      | Some x_5 -> (
          ignore (pop_env w_10);
          match Word.get_value (fst x_5) with
          | 21 ->
              assert_env_length w_10 3;
              push_env w_10 (Dynarray.get w_10.state.e 1);
              assert_env_length w_10 4;
              push_env w_10 (Dynarray.get w_10.state.e 0);
              assert_env_length w_10 5;
              let keep_11 = env_call w_10 [] 2 in
              w_10.state.k <- Memo.appends [ Memo.from_constructor 38; keep_11; w_10.state.k ];
              w_10.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_10
          | 22 ->
              assert_env_length w_10 3;
              push_env w_10 (Dynarray.get w_10.state.e 2);
              assert_env_length w_10 4;
              push_env w_10 (Dynarray.get w_10.state.e 0);
              assert_env_length w_10 5;
              let keep_12 = env_call w_10 [] 2 in
              w_10.state.k <- Memo.appends [ Memo.from_constructor 39; keep_12; w_10.state.k ];
              w_10.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_10
          | _ -> failwith "unreachable"))
    10

let () =
  add_exp
    (fun w_11 ->
      assert_env_length w_11 4;
      let last_6 = Source.E 3 in
      match resolve w_11 last_6 with
      | None -> ()
      | Some x_6 -> (
          ignore (pop_env w_11);
          match Word.get_value (fst x_6) with
          | 23 ->
              assert_env_length w_11 3;
              push_env w_11 (Dynarray.get w_11.state.e 1);
              assert_env_length w_11 4;
              push_env w_11 (Dynarray.get w_11.state.e 0);
              assert_env_length w_11 5;
              let keep_14 = env_call w_11 [] 2 in
              w_11.state.k <- Memo.appends [ Memo.from_constructor 41; keep_14; w_11.state.k ];
              w_11.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_11
          | 24 ->
              let splits_14 = Memo.splits (snd x_6) in
              let split0_14 = List.nth splits_14 0 in
              let split1_7 = List.nth splits_14 1 in
              push_env w_11 split0_14;
              push_env w_11 split1_7;
              assert_env_length w_11 5;
              push_env w_11 (Dynarray.get w_11.state.e 2);
              assert_env_length w_11 6;
              push_env w_11 (Dynarray.get w_11.state.e 4);
              assert_env_length w_11 7;
              push_env w_11 (Dynarray.get w_11.state.e 3);
              assert_env_length w_11 8;
              push_env w_11 (Dynarray.get w_11.state.e 0);
              assert_env_length w_11 9;
              let x1_4 = pop_env w_11 in
              let x0_6 = pop_env w_11 in
              push_env w_11 (Memo.appends [ Memo.from_constructor 4; x0_6; x1_4 ]);
              assert_env_length w_11 8;
              let x1_5 = pop_env w_11 in
              let x0_7 = pop_env w_11 in
              push_env w_11 (Memo.appends [ Memo.from_constructor 4; x0_7; x1_5 ]);
              assert_env_length w_11 7;
              let keep_15 = env_call w_11 [] 2 in
              w_11.state.k <- Memo.appends [ Memo.from_constructor 42; keep_15; w_11.state.k ];
              w_11.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_11
          | _ -> failwith "unreachable"))
    11

let () =
  add_exp
    (fun w_12 ->
      assert_env_length w_12 3;
      let last_7 = Source.E 2 in
      match resolve w_12 last_7 with
      | None -> ()
      | Some x_7 -> (
          ignore (pop_env w_12);
          match Word.get_value (fst x_7) with
          | 20 ->
              let splits_15 = Memo.splits (snd x_7) in
              let split0_15 = List.nth splits_15 0 in
              let split1_8 = List.nth splits_15 1 in
              push_env w_12 split0_15;
              push_env w_12 split1_8;
              assert_env_length w_12 4;
              push_env w_12 (Dynarray.get w_12.state.e 2);
              assert_env_length w_12 5;
              push_env w_12 (Dynarray.get w_12.state.e 1);
              assert_env_length w_12 6;
              push_env w_12 (Dynarray.get w_12.state.e 3);
              assert_env_length w_12 7;
              let x1_6 = pop_env w_12 in
              let x0_8 = pop_env w_12 in
              push_env w_12 (Memo.appends [ Memo.from_constructor 4; x0_8; x1_6 ]);
              assert_env_length w_12 6;
              let keep_17 = env_call w_12 [ 0; 1 ] 2 in
              w_12.state.k <- Memo.appends [ Memo.from_constructor 43; keep_17; w_12.state.k ];
              w_12.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_12
          | 25 ->
              let splits_16 = Memo.splits (snd x_7) in
              let split0_16 = List.nth splits_16 0 in
              let split1_9 = List.nth splits_16 1 in
              push_env w_12 split0_16;
              push_env w_12 split1_9;
              assert_env_length w_12 4;
              push_env w_12 (Dynarray.get w_12.state.e 2);
              assert_env_length w_12 5;
              push_env w_12 (Dynarray.get w_12.state.e 1);
              assert_env_length w_12 6;
              push_env w_12 (Dynarray.get w_12.state.e 0);
              assert_env_length w_12 7;
              push_env w_12 (Dynarray.get w_12.state.e 3);
              assert_env_length w_12 8;
              let x1_7 = pop_env w_12 in
              let x0_9 = pop_env w_12 in
              push_env w_12 (Memo.appends [ Memo.from_constructor 4; x0_9; x1_7 ]);
              assert_env_length w_12 7;
              let x1_8 = pop_env w_12 in
              let x0_10 = pop_env w_12 in
              push_env w_12 (Memo.appends [ Memo.from_constructor 4; x0_10; x1_8 ]);
              assert_env_length w_12 6;
              let keep_18 = env_call w_12 [ 0; 1 ] 2 in
              w_12.state.k <- Memo.appends [ Memo.from_constructor 44; keep_18; w_12.state.k ];
              w_12.state.c <- pc_to_exp (int_to_pc 8);
              stepped w_12
          | _ -> failwith "unreachable"))
    12

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 0
let () = Value.set_constructor_degree 3 1
let () = Value.set_constructor_degree 4 (-1)
let () = Value.set_constructor_degree 5 0
let () = Value.set_constructor_degree 6 (-1)
let () = Value.set_constructor_degree 7 0
let () = Value.set_constructor_degree 8 0
let () = Value.set_constructor_degree 9 (-1)
let () = Value.set_constructor_degree 10 (-1)
let () = Value.set_constructor_degree 11 1
let () = Value.set_constructor_degree 12 1
let () = Value.set_constructor_degree 13 (-2)
let () = Value.set_constructor_degree 14 1
let () = Value.set_constructor_degree 15 (-1)
let () = Value.set_constructor_degree 16 (-2)
let () = Value.set_constructor_degree 17 0
let () = Value.set_constructor_degree 18 1
let () = Value.set_constructor_degree 19 0
let () = Value.set_constructor_degree 20 (-1)
let () = Value.set_constructor_degree 21 1
let () = Value.set_constructor_degree 22 1
let () = Value.set_constructor_degree 23 1
let () = Value.set_constructor_degree 24 (-1)
let () = Value.set_constructor_degree 25 (-1)
let () = Value.set_constructor_degree 26 0
let () = Value.set_constructor_degree 27 0
let () = Value.set_constructor_degree 28 (-2)
let () = Value.set_constructor_degree 29 0
let () = Value.set_constructor_degree 30 0
let () = Value.set_constructor_degree 31 (-2)
let () = Value.set_constructor_degree 32 (-2)
let () = Value.set_constructor_degree 33 (-3)
let () = Value.set_constructor_degree 34 (-2)
let () = Value.set_constructor_degree 35 (-3)
let () = Value.set_constructor_degree 36 (-1)
let () = Value.set_constructor_degree 37 (-1)
let () = Value.set_constructor_degree 38 0
let () = Value.set_constructor_degree 39 0
let () = Value.set_constructor_degree 40 (-1)
let () = Value.set_constructor_degree 41 0
let () = Value.set_constructor_degree 42 0
let () = Value.set_constructor_degree 43 (-2)
let () = Value.set_constructor_degree 44 (-2)
