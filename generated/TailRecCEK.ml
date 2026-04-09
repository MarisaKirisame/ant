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
  let initial_env = Dynarray.init 2 (fun _ -> Memo.from_int 0) in
  Dynarray.set initial_env 0 x0;
  Dynarray.set initial_env 1 x1;
  exec_cek (pc_to_exp (int_to_pc 3)) initial_env (Memo.from_constructor tag_cont_done) memo

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
      assert_env_length w_0 1;
      return_value w_0 (get_env_slot w_0 0) (pc_to_exp (int_to_pc 0)))
    1;
  add_exp
    (fun w_1 ->
      assert_env_length w_1 3;
      assert_env_length w_1 3;
      let resolved_0 = resolve w_1 (Source.E 2) in
      let resolved_1 = resolve w_1 (Source.E 1) in
      set_env_slot w_1 1 (Memo.from_int (Word.get_value (fst resolved_0) + Word.get_value (fst resolved_1)));
      let arg0_0 = get_env_slot w_1 0 in
      let arg1_0 = get_env_slot w_1 1 in
      assert_env_length w_1 3;
      init_frame w_1 2 (Memo.from_int 0);
      set_env_slot w_1 0 arg0_0;
      set_env_slot w_1 1 arg1_0;
      w_1.state.c <- pc_to_exp (int_to_pc 3))
    2;
  add_exp
    (fun w_2 ->
      assert_env_length w_2 2;
      assert_env_length w_2 2;
      let resolved_2 = resolve w_2 (Source.E 0) in
      let tag_0 = Word.get_value (fst resolved_2) in
      match tag_0 with
      | 1 (* tag_Nil *) ->
          let edge0_0 = get_env_slot w_2 1 in
          init_frame w_2 1 (Memo.from_int 0);
          set_env_slot w_2 0 edge0_0;
          w_2.state.c <- pc_to_exp (int_to_pc 1)
      | 2 (* tag_Cons *) ->
          let parts_0 = Memo.splits (snd resolved_2) in
          if List.length parts_0 = 2 then (
            let part0_0 = List.nth parts_0 0 in
            let part1_0 = List.nth parts_0 1 in
            let edge0_1 = get_env_slot w_2 1 in
            let edge1_0 = part0_0 in
            let edge2_0 = part1_0 in
            init_frame w_2 3 (Memo.from_int 0);
            set_env_slot w_2 1 edge0_1;
            set_env_slot w_2 2 edge1_0;
            set_env_slot w_2 0 edge2_0;
            w_2.state.c <- pc_to_exp (int_to_pc 2))
          else failwith "unreachable (3)"
      | _ -> failwith "unreachable (3)")
    3;
  Words.set_constructor_degree 0 1;
  Words.set_constructor_degree 1 1;
  Words.set_constructor_degree 2 (-1)
