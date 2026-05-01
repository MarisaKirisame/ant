open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_cont_1 = 3

type int_list = Nil | Cons of int * int_list

let rec from_ocaml_int_list x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Cons; Memo.from_int x0; from_ocaml_int_list x1 ]

let rec to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 (* tag_Nil *) -> Nil
  | 2 (* tag_Cons *) ->
      let x0, x1 = Memo.splits_2 t in
      Cons (Word.get_value (Memo.to_word x0), to_ocaml_int_list x1)
  | _ -> failwith "unreachable"

let rec list_incr memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_2 ->
      assert_env_length w_2 1;
      let hd_0, tl_0 = resolve w_2 K in
      match Word.get_value hd_0 with
      | 0 (* tag_cont_done *) -> exec_done w_2
      | 3 (* tag_cont_1 *) ->
          w_2.state.k <- get_next_cont tl_0;
          restore_env w_2 1 tl_0;
          Dynarray.set w_2.state.e 0
            (Memo.appends [ Memo.from_constructor tag_Cons; Dynarray.get w_2.state.e 0; Dynarray.get w_2.state.e 1 ]);
          assert_env_length w_2 2;
          return_n w_2 0 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      let x_0 = resolve w_0 (Source.E 0) in
      match Word.get_value (fst x_0) with
      | 1 (* tag_Nil *) ->
          Dynarray.set w_0.state.e 0 (Memo.from_constructor tag_Nil);
          assert_env_length w_0 1;
          return_n w_0 0 (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          Dynarray.set w_0.state.e 0 split0_0;
          push_env w_0 split1_0;
          push_env w_0 (Memo.from_int 1);
          w_0.state.c <- pc_to_exp (int_to_pc 2)
      | _ -> failwith "unreachable (3)")
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      let x0_0 = resolve w_1 (Source.E 0) in
      let x1_0 = resolve w_1 (Source.E 2) in
      Dynarray.set w_1.state.e 0 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_1 3;
      let keep_0 = env_call w_1 [ 0 ] [ 1 ] in
      w_1.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_1.state.k ];
      w_1.state.c <- pc_to_exp (int_to_pc 1))
    2;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1);
  Words.set_constructor_degree 3 (-1)
