open Ant
open Word
open Memo
open Value

let memo = Array.init 14 (fun _ -> ref State.BlackHole)

type ocaml_int_list = Nil | Cons of int * Value.seq

let int_list_Nil : Value.seq = Memo.appends [ Memo.from_constructor 1 ]
let int_list_Cons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 2; Memo.from_int x0; x1 ]
let from_ocaml_int_list x = match x with Nil -> int_list_Nil | Cons (x0, x1) -> int_list_Cons x0 x1

let to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 -> Nil
  | 2 ->
      let [ x0; x1 ] = Memo.splits t in
      Cons (Memo.to_int x0, x1)

let rec list_incr (x0 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp 2) (Dynarray.of_list [ x0 ]) (Memo.from_constructor 0) memo

let () =
  add_exp
    (fun x store update ->
      assert_env_length x 1;
      match resolve x store K update with
      | None -> x
      | Some (hd, tl) -> (
          match Word.get_value hd with
          | 0 -> (fun x tl store upate -> exec_done x update) x tl store update
          | 3 ->
              (fun x_7 tl_1 store_7 update_7 ->
                assert_env_length x_7 1;
                restore_env x_7 1 tl_1;
                x_7.k <- get_next_cont tl_1;
                x_7.c <- pc_to_exp 9;
                stepped x_7)
                x tl store update))
    0

let () =
  add_exp
    (fun x store update ->
      x.c <- pc_to_exp 2;
      stepped x)
    1

let () =
  add_exp
    (fun x_0 store_0 update_0 ->
      assert_env_length x_0 1;
      push_env x_0 (Dynarray.get x_0.e 0);
      x_0.c <- pc_to_exp 3;
      stepped x_0)
    2

let () =
  add_exp
    (fun x_1 store_1 update_1 ->
      assert_env_length x_1 2;
      let last_0 = Source.E 1 in
      match resolve x_1 store_1 last_0 update_1 with
      | None -> x_1
      | Some (hd_0, tl_0) -> (
          Dynarray.remove_last x_1.e;
          match Word.get_value hd_0 with
          | 1 ->
              x_1.c <- pc_to_exp 4;
              stepped x_1
          | 2 ->
              let [ x0_0; x1_0 ] = Memo.splits tl_0 in
              push_env x_1 x0_0;
              push_env x_1 x1_0;
              x_1.c <- pc_to_exp 6;
              stepped x_1))
    3

let () =
  add_exp
    (fun x_2 store_2 update_2 ->
      assert_env_length x_2 1;
      push_env x_2 (Memo.from_constructor 1);
      x_2.c <- pc_to_exp 5;
      stepped x_2)
    4

let () =
  add_exp
    (fun x_3 store_3 update_3 ->
      assert_env_length x_3 2;
      return_n x_3 2 (pc_to_exp 0) store_3 update_3)
    5

let () =
  add_exp
    (fun x_4 store_4 update_4 ->
      assert_env_length x_4 3;
      push_env x_4 (Dynarray.get x_4.e 1);
      x_4.c <- pc_to_exp 7;
      stepped x_4)
    6

let () =
  add_exp
    (fun x_5 store_5 update_5 ->
      assert_env_length x_5 4;
      push_env x_5 (Memo.from_int 1);
      x_5.c <- pc_to_exp 8;
      stepped x_5)
    7

let () =
  add_exp
    (fun x_6 store_6 update_6 ->
      assert_env_length x_6 5;
      match resolve x_6 store_6 (Source.E 3) update_6 with
      | None -> x_6
      | Some (x0_1, _) -> (
          match resolve x_6 store_6 (Source.E 4) update_6 with
          | None -> x_6
          | Some (x1_1, _) ->
              Dynarray.remove_last x_6.e;
              Dynarray.remove_last x_6.e;
              push_env x_6 (Memo.from_int (Word.to_int x0_1 + Word.to_int x1_1));
              x_6.c <- pc_to_exp 12;
              stepped x_6))
    8

let () =
  add_exp
    (fun x_8 store_8 update_8 ->
      assert_env_length x_8 2;
      let x1_2 = pop_env x_8 in
      let x0_2 = pop_env x_8 in
      push_env x_8 (Memo.appends [ Memo.from_constructor 2; x0_2; x1_2 ]);
      x_8.c <- pc_to_exp 10;
      stepped x_8)
    9

let () =
  add_exp
    (fun x_9 store_9 update_9 ->
      assert_env_length x_9 1;
      drop_n x_9 1 0 (pc_to_exp 11))
    10

let () =
  add_exp
    (fun x_10 store_10 update_10 ->
      assert_env_length x_10 1;
      return_n x_10 1 (pc_to_exp 0) store_10 update_10)
    11

let () =
  add_exp
    (fun x_11 store_11 update_11 ->
      assert_env_length x_11 4;
      push_env x_11 (Dynarray.get x_11.e 2);
      x_11.c <- pc_to_exp 13;
      stepped x_11)
    12

let () =
  add_exp
    (fun x_12 store_12 update_12 ->
      assert_env_length x_12 5;
      let keep_0 = env_call x_12 [ 3 ] 1 in
      x_12.k <- Memo.appends [ Memo.from_constructor 3; keep_0; x_12.k ];
      x_12.c <- pc_to_exp 1;
      stepped x_12)
    13

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 (-1)
let () = Value.set_constructor_degree 3 (-1)
