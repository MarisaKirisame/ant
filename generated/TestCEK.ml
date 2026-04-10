open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_cont_0 = 3

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

let list_incr memo (x0 : Value.seq) : exec_result =
  let initial_env = Dynarray.init 1 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  exec_cek (pc_to_exp (int_to_pc 2)) initial_env (Memo.from_constructor tag_cont_done) memo

let populate_state () =
  Memo.reset ();
  Words.reset ();
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      let hd_0, tl_0 = resolve w_3 K in
      match Word.get_value hd_0 with
      | 3 (* tag_cont_0 *) ->
          let ret_0 = get_env_slot w_3 0 in
          assert_env_length w_3 1;
          w_3.state.k <- get_next_cont tl_0;
          init_frame w_3 2 (Memo.from_int 0);
          restore_env_slots w_3 [ 1 ] tl_0;
          set_env_slot w_3 0 ret_0;
          w_3.state.c <- pc_to_exp (int_to_pc 3)
      | _ -> failwith "unreachable (0)")
    0;
  add_exp
    (fun w_0 ->
      assert_env_length w_0 2;
      assert_env_length w_0 2;
      let resolved_0 = resolve w_0 (Source.E 1) in
      set_env_slot w_0 1
        (Memo.from_int (Word.get_value (fst resolved_0) + Word.get_value (Memo.to_word (Memo.from_int 1))));
      let arg0_0 = get_env_slot w_0 0 in
      assert_env_length w_0 2;
      w_0.state.k <- Memo.appends [ Memo.from_constructor tag_cont_0; collect_env_slots w_0 [ 1 ]; w_0.state.k ];
      init_frame w_0 1 (Memo.from_int 0);
      set_env_slot w_0 0 arg0_0;
      w_0.state.c <- pc_to_exp (int_to_pc 2))
    1;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 1;
      assert_env_length w_2 1;
      let resolved_1 = resolve w_2 (Source.E 0) in
      let tag_0 = Word.get_value (fst resolved_1) in
      match tag_0 with
      | 1 (* tag_Nil *) -> return_value w_2 (Memo.from_constructor tag_Nil) (pc_to_exp (int_to_pc 0))
      | 2 (* tag_Cons *) ->
          let parts_0 = Memo.splits (snd resolved_1) in
          if List.length parts_0 = 2 then (
            let part0_0 = List.nth parts_0 0 in
            let part1_0 = List.nth parts_0 1 in
            shuffle_frame w_2 [| NewValue part1_0; NewValue part0_0 |] (Memo.from_int 0);
            w_2.state.c <- pc_to_exp (int_to_pc 1))
          else failwith "unreachable (2)"
      | _ -> failwith "unreachable (2)")
    2;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      assert_env_length w_1 2;
      set_env_slot w_1 0 (Memo.appends [ Memo.from_constructor tag_Cons; get_env_slot w_1 1; get_env_slot w_1 0 ]);
      return_value w_1 (get_env_slot w_1 0) (pc_to_exp (int_to_pc 0)))
    3;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1);
  Words.set_constructor_degree 3 (-1)
