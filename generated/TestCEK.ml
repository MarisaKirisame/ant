open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_Pair = 3
let tag_Wrap = 4

type 'a list = Nil | Cons of 'a * 'a list

let rec from_ocaml_list from_generic_a x =
  match x with
  | Nil -> Memo.appends [ Memo.from_constructor tag_Nil ]
  | Cons (x0, x1) ->
      Memo.appends [ Memo.from_constructor tag_Cons; from_generic_a x0; from_ocaml_list (fun x -> from_generic_a x) x1 ]

let rec to_ocaml_list to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 (* tag_Nil *) -> Nil
  | 2 (* tag_Cons *) ->
      let x0, x1 = Memo.splits_2 t in
      Cons (to_generic_a x0, to_ocaml_list (fun x -> to_generic_a x) x1)
  | _ -> failwith "unreachable"

type ('a, 'b) pair = Pair of 'a * 'b

let rec from_ocaml_pair from_generic_a from_generic_b x =
  match x with Pair (x0, x1) -> Memo.appends [ Memo.from_constructor tag_Pair; from_generic_a x0; from_generic_b x1 ]

let rec to_ocaml_pair to_generic_a to_generic_b x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 3 (* tag_Pair *) ->
      let x0, x1 = Memo.splits_2 t in
      Pair (to_generic_a x0, to_generic_b x1)
  | _ -> failwith "unreachable"

type 'a wrap = Wrap of 'a

let rec from_ocaml_wrap from_generic_a x =
  match x with Wrap x0 -> Memo.appends [ Memo.from_constructor tag_Wrap; from_generic_a x0 ]

let rec to_ocaml_wrap to_generic_a x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 4 (* tag_Wrap *) ->
      let x0 = Memo.splits_1 t in
      Wrap (to_generic_a x0)
  | _ -> failwith "unreachable"

let rec test_match memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_match2 memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let rec test_wrap memo (x0 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 6)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_7 ->
      assert_env_length w_7 1;
      let hd_0, tl_0 = resolve w_7 K in
      match Word.get_value hd_0 with c_3 when c_3 = tag_cont_done -> exec_done w_7 | _ -> failwith "unreachable (0)")
    0

let () =
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 2))
    1

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      let last_0 = Source.E 1 in
      let x_0 = resolve w_1 last_0 in
      match Word.get_value (fst x_0) with
      | c_0 when c_0 = tag_Nil ->
          ignore (pop_env w_1);
          assert_env_length w_1 1;
          push_env w_1 (Memo.from_int 0);
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | c_0 when c_0 = tag_Cons ->
          let splits_0 = Memo.splits (snd x_0) in
          let split0_0 = List.nth splits_0 0 in
          let split1_0 = List.nth splits_0 1 in
          ignore (pop_env w_1);
          push_env w_1 split0_0;
          push_env w_1 split1_0;
          assert_env_length w_1 3;
          push_env w_1 (Memo.from_int 1);
          assert_env_length w_1 4;
          drop_n w_1 4 2;
          assert_env_length w_1 2;
          return_n w_1 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (2)")
    2

let () =
  add_exp
    (fun w_2 ->
      assert_env_length w_2 1;
      push_env w_2 (Dynarray.get w_2.state.e 0);
      w_2.state.c <- pc_to_exp (int_to_pc 5))
    3

let () =
  add_exp
    (fun w_4 ->
      assert_env_length w_4 5;
      let x0_0 = resolve w_4 (Source.E 3) in
      let x1_0 = resolve w_4 (Source.E 4) in
      ignore (pop_env w_4);
      ignore (pop_env w_4);
      push_env w_4 (Memo.from_int (Word.get_value (fst x0_0) + Word.get_value (fst x1_0)));
      assert_env_length w_4 4;
      drop_n w_4 4 2;
      assert_env_length w_4 2;
      return_n w_4 2 (pc_to_exp (int_to_pc 0)))
    4

let () =
  add_exp
    (fun w_3 ->
      assert_env_length w_3 2;
      let last_1 = Source.E 1 in
      let x_1 = resolve w_3 last_1 in
      match Word.get_value (fst x_1) with
      | c_1 when c_1 = tag_Pair ->
          let splits_1 = Memo.splits (snd x_1) in
          let split0_1 = List.nth splits_1 0 in
          let split1_1 = List.nth splits_1 1 in
          ignore (pop_env w_3);
          push_env w_3 split0_1;
          push_env w_3 split1_1;
          assert_env_length w_3 3;
          push_env w_3 (Dynarray.get w_3.state.e 1);
          assert_env_length w_3 4;
          push_env w_3 (Dynarray.get w_3.state.e 2);
          w_3.state.c <- pc_to_exp (int_to_pc 4)
      | _ -> failwith "unreachable (5)")
    5

let () =
  add_exp
    (fun w_5 ->
      assert_env_length w_5 1;
      push_env w_5 (Dynarray.get w_5.state.e 0);
      w_5.state.c <- pc_to_exp (int_to_pc 7))
    6

let () =
  add_exp
    (fun w_6 ->
      assert_env_length w_6 2;
      let last_2 = Source.E 1 in
      let x_2 = resolve w_6 last_2 in
      match Word.get_value (fst x_2) with
      | c_2 when c_2 = tag_Wrap ->
          let splits_2 = Memo.splits (snd x_2) in
          let split0_2 = List.nth splits_2 0 in
          ignore (pop_env w_6);
          push_env w_6 split0_2;
          assert_env_length w_6 2;
          push_env w_6 (Dynarray.get w_6.state.e 1);
          assert_env_length w_6 3;
          drop_n w_6 3 1;
          assert_env_length w_6 2;
          return_n w_6 2 (pc_to_exp (int_to_pc 0))
      | _ -> failwith "unreachable (7)")
    7

let () = Words.set_constructor_degree 0 1
let () = Words.set_constructor_degree 1 1
let () = Words.set_constructor_degree 2 (-1)
let () = Words.set_constructor_degree 3 (-1)
let () = Words.set_constructor_degree 4 0
