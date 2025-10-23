open Ant
open Word
open Memo
open Value

let memo = Array.init 12 (fun _ -> ref State.BlackHole)

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

let rec index (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp 1) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

type ocaml_expr =
  | EInt of Value.seq
  | EPlus of Value.seq * Value.seq
  | EVar of Value.seq
  | EAbs of Value.seq
  | EApp of Value.seq * Value.seq
  | ETrue
  | EFalse
  | EIf of Value.seq * Value.seq * Value.seq
  | ENil
  | ECons of Value.seq * Value.seq
  | EMatchList of Value.seq * Value.seq * Value.seq
  | EFix of Value.seq

let expr_EInt x0 : Value.seq = Memo.appends [ Memo.from_constructor 6; x0 ]
let expr_EPlus x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 7; x0; x1 ]
let expr_EVar x0 : Value.seq = Memo.appends [ Memo.from_constructor 8; x0 ]
let expr_EAbs x0 : Value.seq = Memo.appends [ Memo.from_constructor 9; x0 ]
let expr_EApp x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 10; x0; x1 ]
let expr_ETrue : Value.seq = Memo.appends [ Memo.from_constructor 11 ]
let expr_EFalse : Value.seq = Memo.appends [ Memo.from_constructor 12 ]
let expr_EIf x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 13; x0; x1; x2 ]
let expr_ENil : Value.seq = Memo.appends [ Memo.from_constructor 14 ]
let expr_ECons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 15; x0; x1 ]
let expr_EMatchList x0 x1 x2 : Value.seq = Memo.appends [ Memo.from_constructor 16; x0; x1; x2 ]
let expr_EFix x0 : Value.seq = Memo.appends [ Memo.from_constructor 17; x0 ]

let from_ocaml_expr x =
  match x with
  | EInt x0 -> expr_EInt x0
  | EPlus (x0, x1) -> expr_EPlus x0 x1
  | EVar x0 -> expr_EVar x0
  | EAbs x0 -> expr_EAbs x0
  | EApp (x0, x1) -> expr_EApp x0 x1
  | ETrue -> expr_ETrue
  | EFalse -> expr_EFalse
  | EIf (x0, x1, x2) -> expr_EIf x0 x1 x2
  | ENil -> expr_ENil
  | ECons (x0, x1) -> expr_ECons x0 x1
  | EMatchList (x0, x1, x2) -> expr_EMatchList x0 x1 x2
  | EFix x0 -> expr_EFix x0

let to_ocaml_expr x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 6 ->
      let [ x0 ] = Memo.splits t in
      EInt x0
  | 7 ->
      let [ x0; x1 ] = Memo.splits t in
      EPlus (x0, x1)
  | 8 ->
      let [ x0 ] = Memo.splits t in
      EVar x0
  | 9 ->
      let [ x0 ] = Memo.splits t in
      EAbs x0
  | 10 ->
      let [ x0; x1 ] = Memo.splits t in
      EApp (x0, x1)
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

type ocaml_value =
  | VInt of Value.seq
  | VAbs of Value.seq * Value.seq
  | VTrue
  | VFalse
  | VNil
  | VCons of Value.seq * Value.seq
  | VFix of Value.seq * Value.seq

let value_VInt x0 : Value.seq = Memo.appends [ Memo.from_constructor 18; x0 ]
let value_VAbs x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 19; x0; x1 ]
let value_VTrue : Value.seq = Memo.appends [ Memo.from_constructor 20 ]
let value_VFalse : Value.seq = Memo.appends [ Memo.from_constructor 21 ]
let value_VNil : Value.seq = Memo.appends [ Memo.from_constructor 22 ]
let value_VCons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 23; x0; x1 ]
let value_VFix x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 24; x0; x1 ]

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
  | 18 ->
      let [ x0 ] = Memo.splits t in
      VInt x0
  | 19 ->
      let [ x0; x1 ] = Memo.splits t in
      VAbs (x0, x1)
  | 20 -> VTrue
  | 21 -> VFalse
  | 22 -> VNil
  | 23 ->
      let [ x0; x1 ] = Memo.splits t in
      VCons (x0, x1)
  | 24 ->
      let [ x0; x1 ] = Memo.splits t in
      VFix (x0, x1)

let rec vadd (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp 4) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let rec eval (x0 : Value.seq) (x1 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp 8) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor 0) memo

let () =
  add_exp
    (fun w_9 ->
      (assert_env_length w_9) 1;
      match resolve w_9 K with
      | None -> ()
      | Some (hd, tl) -> (
          match Word.get_value hd with
          | 0 -> exec_done w_9
          | 5 ->
              ();
              (assert_env_length w_9) 0;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 0) tl;
              ();
              (assert_env_length w_9) 1;
              ((drop_n w_9) 1) 0;
              ();
              (assert_env_length w_9) 1;
              ((drop_n w_9) 1) 0;
              (assert_env_length w_9) 1;
              ((return_n w_9) 1) (pc_to_exp 0)
          | 25 ->
              ();
              (assert_env_length w_9) 0;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 0) tl;
              ();
              (assert_env_length w_9) 1;
              ((drop_n w_9) 1) 0;
              (assert_env_length w_9) 1;
              ((return_n w_9) 1) (pc_to_exp 0)
          | 26 ->
              ();
              (assert_env_length w_9) 2;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 2) tl;
              ();
              (assert_env_length w_9) 3;
              (push_env w_9) ((Dynarray.get w_9.state.e) 1);
              ();
              (assert_env_length w_9) 4;
              (push_env w_9) ((Dynarray.get w_9.state.e) 0);
              (();
               (assert_env_length w_9) 5;
               let keep_7 = ((env_call w_9) [ 2 ]) 2 in
               w_9.state.k <- Memo.appends [ Memo.from_constructor 32; keep_7; w_9.state.k ]);
              w_9.state.c <- pc_to_exp 8;
              stepped w_9
          | 27 ->
              ();
              (assert_env_length w_9) 0;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 0) tl;
              ();
              (assert_env_length w_9) 1;
              ((drop_n w_9) 1) 0;
              (assert_env_length w_9) 1;
              ((return_n w_9) 1) (pc_to_exp 0)
          | 28 ->
              ();
              (assert_env_length w_9) 2;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 2) tl;
              ();
              (assert_env_length w_9) 4;
              (push_env w_9) ((Dynarray.get w_9.state.e) 1);
              ();
              (assert_env_length w_9) 5;
              (push_env w_9) ((Dynarray.get w_9.state.e) 0);
              (();
               (assert_env_length w_9) 6;
               let keep_8 = ((env_call w_9) [ 2; 3 ]) 2 in
               w_9.state.k <- Memo.appends [ Memo.from_constructor 33; keep_8; w_9.state.k ]);
              w_9.state.c <- pc_to_exp 8;
              stepped w_9
          | 29 ->
              ();
              (assert_env_length w_9) 3;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 3) tl;
              w_9.state.c <- pc_to_exp 10;
              stepped w_9
          | 30 ->
              ();
              (assert_env_length w_9) 2;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 2) tl;
              ();
              (assert_env_length w_9) 3;
              (push_env w_9) ((Dynarray.get w_9.state.e) 1);
              ();
              (assert_env_length w_9) 4;
              (push_env w_9) ((Dynarray.get w_9.state.e) 0);
              (();
               (assert_env_length w_9) 5;
               let keep_11 = ((env_call w_9) [ 2 ]) 2 in
               w_9.state.k <- Memo.appends [ Memo.from_constructor 36; keep_11; w_9.state.k ]);
              w_9.state.c <- pc_to_exp 8;
              stepped w_9
          | 31 ->
              ();
              (assert_env_length w_9) 3;
              w_9.state.k <- get_next_cont tl;
              ((restore_env w_9) 3) tl;
              w_9.state.c <- pc_to_exp 11;
              stepped w_9))
    0

let () =
  add_exp
    (fun w_0 ->
      ();
      (assert_env_length w_0) 2;
      (push_env w_0) ((Dynarray.get w_0.state.e) 0);
      w_0.state.c <- pc_to_exp 3;
      stepped w_0)
    1

let () =
  add_exp
    (fun w_2 ->
      (assert_env_length w_2) 5;
      let last_1 = Source.E 4 in
      match (resolve w_2) last_1 with
      | None -> ()
      | Some x_1 -> (
          ignore (pop_env w_2);
          match Word.get_value (fst x_1) with
          | 1 ->
              ();
              (assert_env_length w_2) 4;
              (push_env w_2) ((Dynarray.get w_2.state.e) 2);
              ();
              (assert_env_length w_2) 5;
              ((drop_n w_2) 5) 2;
              (assert_env_length w_2) 3;
              ((return_n w_2) 3) (pc_to_exp 0)
          | 2 ->
              let [ x0_1 ] = Memo.splits (snd x_1) in
              (push_env w_2) x0_1;
              ();
              (assert_env_length w_2) 5;
              (push_env w_2) ((Dynarray.get w_2.state.e) 3);
              ();
              (assert_env_length w_2) 6;
              (push_env w_2) ((Dynarray.get w_2.state.e) 4);
              (();
               (assert_env_length w_2) 7;
               let keep_0 = ((env_call w_2) []) 2 in
               w_2.state.k <- Memo.appends [ Memo.from_constructor 5; keep_0; w_2.state.k ]);
              w_2.state.c <- pc_to_exp 1;
              stepped w_2))
    2

let () =
  add_exp
    (fun w_1 ->
      (assert_env_length w_1) 3;
      let last_0 = Source.E 2 in
      match (resolve w_1) last_0 with
      | None -> ()
      | Some x_0 -> (
          ignore (pop_env w_1);
          match Word.get_value (fst x_0) with
          | 4 ->
              let [ x0_0; x1_0 ] = Memo.splits (snd x_0) in
              (push_env w_1) x0_0;
              (push_env w_1) x1_0;
              ();
              (assert_env_length w_1) 4;
              (push_env w_1) ((Dynarray.get w_1.state.e) 1);
              w_1.state.c <- pc_to_exp 2;
              stepped w_1))
    3

let () =
  add_exp
    (fun w_3 ->
      ();
      (assert_env_length w_3) 2;
      (push_env w_3) ((Dynarray.get w_3.state.e) 0);
      w_3.state.c <- pc_to_exp 7;
      stepped w_3)
    4

let () =
  add_exp
    (fun w_6 ->
      ();
      (assert_env_length w_6) 6;
      match (resolve w_6) (Source.E 4) with
      | None -> ()
      | Some x0_4 -> (
          match (resolve w_6) (Source.E 5) with
          | None -> ()
          | Some x1_1 ->
              ();
              ignore (pop_env w_6);
              ignore (pop_env w_6);
              (push_env w_6) (Memo.from_int (Word.to_int (fst x0_4) + Word.to_int (fst x1_1)));
              (();
               (assert_env_length w_6) 5;
               let x0_5 = pop_env w_6 in
               (push_env w_6) (Memo.appends [ Memo.from_constructor 18; x0_5 ]));
              ();
              (assert_env_length w_6) 5;
              ((drop_n w_6) 5) 1;
              ();
              (assert_env_length w_6) 4;
              ((drop_n w_6) 4) 1;
              (assert_env_length w_6) 3;
              ((return_n w_6) 3) (pc_to_exp 0)))
    5

let () =
  add_exp
    (fun w_5 ->
      (assert_env_length w_5) 4;
      let last_3 = Source.E 3 in
      match (resolve w_5) last_3 with
      | None -> ()
      | Some x_3 -> (
          ignore (pop_env w_5);
          match Word.get_value (fst x_3) with
          | 18 ->
              let [ x0_3 ] = Memo.splits (snd x_3) in
              (push_env w_5) x0_3;
              ();
              (assert_env_length w_5) 4;
              (push_env w_5) ((Dynarray.get w_5.state.e) 2);
              ();
              (assert_env_length w_5) 5;
              (push_env w_5) ((Dynarray.get w_5.state.e) 3);
              w_5.state.c <- pc_to_exp 5;
              stepped w_5))
    6

let () =
  add_exp
    (fun w_4 ->
      (assert_env_length w_4) 3;
      let last_2 = Source.E 2 in
      match (resolve w_4) last_2 with
      | None -> ()
      | Some x_2 -> (
          ignore (pop_env w_4);
          match Word.get_value (fst x_2) with
          | 18 ->
              let [ x0_2 ] = Memo.splits (snd x_2) in
              (push_env w_4) x0_2;
              ();
              (assert_env_length w_4) 3;
              (push_env w_4) ((Dynarray.get w_4.state.e) 1);
              w_4.state.c <- pc_to_exp 6;
              stepped w_4))
    7

let () =
  add_exp
    (fun w_7 ->
      ();
      (assert_env_length w_7) 2;
      (push_env w_7) ((Dynarray.get w_7.state.e) 0);
      w_7.state.c <- pc_to_exp 9;
      stepped w_7)
    8

let () =
  add_exp
    (fun w_8 ->
      (assert_env_length w_8) 3;
      let last_4 = Source.E 2 in
      match (resolve w_8) last_4 with
      | None -> ()
      | Some x_4 -> (
          ignore (pop_env w_8);
          match Word.get_value (fst x_4) with
          | 6 ->
              let [ x0_6 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_6;
              ();
              (assert_env_length w_8) 3;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              (();
               (assert_env_length w_8) 4;
               let x0_7 = pop_env w_8 in
               (push_env w_8) (Memo.appends [ Memo.from_constructor 18; x0_7 ]));
              ();
              (assert_env_length w_8) 4;
              ((drop_n w_8) 4) 1;
              (assert_env_length w_8) 3;
              ((return_n w_8) 3) (pc_to_exp 0)
          | 7 ->
              let [ x0_8; x1_2 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_8;
              (push_env w_8) x1_2;
              ();
              (assert_env_length w_8) 4;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 5;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 6;
               let keep_1 = ((env_call w_8) [ 1; 3 ]) 2 in
               w_8.state.k <- Memo.appends [ Memo.from_constructor 26; keep_1; w_8.state.k ]);
              w_8.state.c <- pc_to_exp 8;
              stepped w_8
          | 8 ->
              let [ x0_9 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_9;
              ();
              (assert_env_length w_8) 3;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              ();
              (assert_env_length w_8) 4;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              (();
               (assert_env_length w_8) 5;
               let keep_2 = ((env_call w_8) []) 2 in
               w_8.state.k <- Memo.appends [ Memo.from_constructor 27; keep_2; w_8.state.k ]);
              w_8.state.c <- pc_to_exp 1;
              stepped w_8
          | 9 ->
              let [ x0_10 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_10;
              ();
              (assert_env_length w_8) 3;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 4;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 5;
               let x1_3 = pop_env w_8 in
               let x0_11 = pop_env w_8 in
               (push_env w_8) (Memo.appends [ Memo.from_constructor 19; x0_11; x1_3 ]));
              ();
              (assert_env_length w_8) 4;
              ((drop_n w_8) 4) 1;
              (assert_env_length w_8) 3;
              ((return_n w_8) 3) (pc_to_exp 0)
          | 17 ->
              let [ x0_12 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_12;
              ();
              (assert_env_length w_8) 3;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 4;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 5;
               let x1_4 = pop_env w_8 in
               let x0_13 = pop_env w_8 in
               (push_env w_8) (Memo.appends [ Memo.from_constructor 24; x0_13; x1_4 ]));
              ();
              (assert_env_length w_8) 4;
              ((drop_n w_8) 4) 1;
              (assert_env_length w_8) 3;
              ((return_n w_8) 3) (pc_to_exp 0)
          | 10 ->
              let [ x0_14; x1_5 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_14;
              (push_env w_8) x1_5;
              ();
              (assert_env_length w_8) 4;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 5;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 6;
               let keep_3 = ((env_call w_8) [ 1; 3 ]) 2 in
               w_8.state.k <- Memo.appends [ Memo.from_constructor 28; keep_3; w_8.state.k ]);
              w_8.state.c <- pc_to_exp 8;
              stepped w_8
          | 11 ->
              ();
              (assert_env_length w_8) 2;
              (push_env w_8) (Memo.from_constructor 20);
              (assert_env_length w_8) 3;
              ((return_n w_8) 3) (pc_to_exp 0)
          | 12 ->
              ();
              (assert_env_length w_8) 2;
              (push_env w_8) (Memo.from_constructor 21);
              (assert_env_length w_8) 3;
              ((return_n w_8) 3) (pc_to_exp 0)
          | 13 ->
              let [ x0_15; x1_6; x2_0 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_15;
              ();
              (push_env w_8) x1_6;
              (push_env w_8) x2_0;
              ();
              (assert_env_length w_8) 5;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 6;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 7;
               let keep_4 = ((env_call w_8) [ 1; 3; 4 ]) 2 in
               w_8.state.k <- Memo.appends [ Memo.from_constructor 29; keep_4; w_8.state.k ]);
              w_8.state.c <- pc_to_exp 8;
              stepped w_8
          | 14 ->
              ();
              (assert_env_length w_8) 2;
              (push_env w_8) (Memo.from_constructor 22);
              (assert_env_length w_8) 3;
              ((return_n w_8) 3) (pc_to_exp 0)
          | 15 ->
              let [ x0_16; x1_7 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_16;
              (push_env w_8) x1_7;
              ();
              (assert_env_length w_8) 4;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 5;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 6;
               let keep_5 = ((env_call w_8) [ 1; 3 ]) 2 in
               w_8.state.k <- Memo.appends [ Memo.from_constructor 30; keep_5; w_8.state.k ]);
              w_8.state.c <- pc_to_exp 8;
              stepped w_8
          | 16 ->
              let [ x0_17; x1_8; x2_1 ] = Memo.splits (snd x_4) in
              (push_env w_8) x0_17;
              ();
              (push_env w_8) x1_8;
              (push_env w_8) x2_1;
              ();
              (assert_env_length w_8) 5;
              (push_env w_8) ((Dynarray.get w_8.state.e) 2);
              ();
              (assert_env_length w_8) 6;
              (push_env w_8) ((Dynarray.get w_8.state.e) 1);
              (();
               (assert_env_length w_8) 7;
               let keep_6 = ((env_call w_8) [ 1; 3; 4 ]) 2 in
               w_8.state.k <- Memo.appends [ Memo.from_constructor 31; keep_6; w_8.state.k ]);
              w_8.state.c <- pc_to_exp 8;
              stepped w_8))
    9

let () =
  add_exp
    (fun w_10 ->
      (assert_env_length w_10) 4;
      let last_5 = Source.E 3 in
      match (resolve w_10) last_5 with
      | None -> ()
      | Some x_5 -> (
          ignore (pop_env w_10);
          match Word.get_value (fst x_5) with
          | 20 ->
              ();
              (assert_env_length w_10) 3;
              (push_env w_10) ((Dynarray.get w_10.state.e) 1);
              ();
              (assert_env_length w_10) 4;
              (push_env w_10) ((Dynarray.get w_10.state.e) 0);
              (();
               (assert_env_length w_10) 5;
               let keep_9 = ((env_call w_10) []) 2 in
               w_10.state.k <- Memo.appends [ Memo.from_constructor 34; keep_9; w_10.state.k ]);
              w_10.state.c <- pc_to_exp 8;
              stepped w_10
          | 21 ->
              ();
              (assert_env_length w_10) 3;
              (push_env w_10) ((Dynarray.get w_10.state.e) 2);
              ();
              (assert_env_length w_10) 4;
              (push_env w_10) ((Dynarray.get w_10.state.e) 0);
              (();
               (assert_env_length w_10) 5;
               let keep_10 = ((env_call w_10) []) 2 in
               w_10.state.k <- Memo.appends [ Memo.from_constructor 35; keep_10; w_10.state.k ]);
              w_10.state.c <- pc_to_exp 8;
              stepped w_10))
    10

let () =
  add_exp
    (fun w_11 ->
      (assert_env_length w_11) 4;
      let last_6 = Source.E 3 in
      match (resolve w_11) last_6 with
      | None -> ()
      | Some x_6 -> (
          ignore (pop_env w_11);
          match Word.get_value (fst x_6) with
          | 22 ->
              ();
              (assert_env_length w_11) 3;
              (push_env w_11) ((Dynarray.get w_11.state.e) 1);
              ();
              (assert_env_length w_11) 4;
              (push_env w_11) ((Dynarray.get w_11.state.e) 0);
              (();
               (assert_env_length w_11) 5;
               let keep_12 = ((env_call w_11) []) 2 in
               w_11.state.k <- Memo.appends [ Memo.from_constructor 37; keep_12; w_11.state.k ]);
              w_11.state.c <- pc_to_exp 8;
              stepped w_11
          | 23 ->
              let [ x0_18; x1_9 ] = Memo.splits (snd x_6) in
              (push_env w_11) x0_18;
              (push_env w_11) x1_9;
              ();
              (assert_env_length w_11) 5;
              (push_env w_11) ((Dynarray.get w_11.state.e) 2);
              ();
              (assert_env_length w_11) 6;
              (push_env w_11) ((Dynarray.get w_11.state.e) 4);
              ();
              (assert_env_length w_11) 7;
              (push_env w_11) ((Dynarray.get w_11.state.e) 3);
              ();
              (assert_env_length w_11) 8;
              (push_env w_11) ((Dynarray.get w_11.state.e) 0);
              (();
               (assert_env_length w_11) 9;
               let x1_10 = pop_env w_11 in
               let x0_19 = pop_env w_11 in
               (push_env w_11) (Memo.appends [ Memo.from_constructor 4; x0_19; x1_10 ]));
              (();
               (assert_env_length w_11) 8;
               let x1_11 = pop_env w_11 in
               let x0_20 = pop_env w_11 in
               (push_env w_11) (Memo.appends [ Memo.from_constructor 4; x0_20; x1_11 ]));
              (();
               (assert_env_length w_11) 7;
               let keep_13 = ((env_call w_11) []) 2 in
               w_11.state.k <- Memo.appends [ Memo.from_constructor 38; keep_13; w_11.state.k ]);
              w_11.state.c <- pc_to_exp 8;
              stepped w_11))
    11

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 0
let () = Value.set_constructor_degree 3 1
let () = Value.set_constructor_degree 4 (-1)
let () = Value.set_constructor_degree 5 0
let () = Value.set_constructor_degree 6 0
let () = Value.set_constructor_degree 7 (-1)
let () = Value.set_constructor_degree 8 0
let () = Value.set_constructor_degree 9 0
let () = Value.set_constructor_degree 10 (-1)
let () = Value.set_constructor_degree 11 1
let () = Value.set_constructor_degree 12 1
let () = Value.set_constructor_degree 13 (-2)
let () = Value.set_constructor_degree 14 1
let () = Value.set_constructor_degree 15 (-1)
let () = Value.set_constructor_degree 16 (-2)
let () = Value.set_constructor_degree 17 0
let () = Value.set_constructor_degree 18 0
let () = Value.set_constructor_degree 19 (-1)
let () = Value.set_constructor_degree 20 1
let () = Value.set_constructor_degree 21 1
let () = Value.set_constructor_degree 22 1
let () = Value.set_constructor_degree 23 (-1)
let () = Value.set_constructor_degree 24 (-1)
let () = Value.set_constructor_degree 25 0
let () = Value.set_constructor_degree 26 (-2)
let () = Value.set_constructor_degree 27 0
let () = Value.set_constructor_degree 28 (-2)
let () = Value.set_constructor_degree 29 (-3)
let () = Value.set_constructor_degree 30 (-2)
let () = Value.set_constructor_degree 31 (-3)
let () = Value.set_constructor_degree 32 (-1)
let () = Value.set_constructor_degree 33 (-2)
let () = Value.set_constructor_degree 34 0
let () = Value.set_constructor_degree 35 0
let () = Value.set_constructor_degree 36 (-1)
let () = Value.set_constructor_degree 37 0
let () = Value.set_constructor_degree 38 0
