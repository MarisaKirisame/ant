open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2

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

let sum memo (x0 : Value.seq) (x1 : Value.seq) : exec_result =
  exec_cek (pc_to_exp (int_to_pc 3)) (Dynarray.of_list [ x0; x1 ]) (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      let hd_0, tl_0 = resolve w_3 K in
      match Word.get_value hd_0 with _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 3;
      return_value w_0 (get_env_slot w_0 0) (pc_to_exp (int_to_pc 0)))
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      assert_env_length w_1 3;
      let lhs_0 = Memo.to_word (get_env_slot w_1 2) in
      let rhs_0 = Memo.to_word (get_env_slot w_1 0) in
      set_env_slot w_1 0 (Memo.from_int (Word.get_value lhs_0 + Word.get_value rhs_0));
      let arg0_0 = get_env_slot w_1 1 in
      let arg1_0 = get_env_slot w_1 0 in
      assert_env_length w_1 3;
      init_frame w_1 3 (Memo.from_constructor tag_cont_done);
      set_env_slot w_1 1 arg0_0;
      set_env_slot w_1 0 arg1_0;
      w_1.state.c <- pc_to_exp (int_to_pc 3))
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 3;
      assert_env_length w_2 3;
      match Memo.list_match (get_env_slot w_2 1) with
      | None -> failwith "unreachable (3)"
      | Some pair_0 -> (
          let tag_0 = Word.get_value (fst pair_0) in
          match tag_0 with
          | 1 (* tag_Nil *) -> w_2.state.c <- pc_to_exp (int_to_pc 1)
          | 2 (* tag_Cons *) ->
              let parts_0 = Memo.splits (snd pair_0) in
              if List.length parts_0 = 2 then (
                let part0_0 = List.nth parts_0 0 in
                let part1_0 = List.nth parts_0 1 in
                set_env_slot w_2 2 part0_0;
                set_env_slot w_2 1 part1_0;
                w_2.state.c <- pc_to_exp (int_to_pc 2))
              else failwith "unreachable (3)"
          | _ -> failwith "unreachable (3)"))
    3;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1)
