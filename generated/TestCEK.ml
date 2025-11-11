open Ant
open Word
open Memo
open Value
open Common

let tag_cont_done = 0
let tag_Nil = 1
let tag_Cons = 2
let tag_cont_1 = 3
let memo = Array.init 4 (fun _ -> ref State.BlackHole)

type ocaml_int_list = Nil | Cons of Value.seq * Value.seq

let int_list_Nil : Value.seq = Memo.appends [ Memo.from_constructor 1 ]
let int_list_Cons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 2; x0; x1 ]
let from_ocaml_int_list x = match x with Nil -> int_list_Nil | Cons (x0, x1) -> int_list_Cons x0 x1

let to_ocaml_int_list x =
  let h_0, t_0 = Option.get (Memo.list_match x) in
  match Word.get_value h_0 with
  | c_0 when c_0 = tag_Nil -> Nil
  | c_0 when c_0 = tag_Cons ->
      let [ x0_0; x1_0 ] = Memo.splits t_0 in
      Cons (x0_0, x1_0)
  | _ -> failwith "unreachable"

let rec list_incr (x0 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp (int_to_pc 1)) (Dynarray.of_list [ x0 ]) (Memo.from_constructor tag_cont_done) memo

let () =
  add_exp
    (fun w_3 ->
      assert_env_length w_3 1;
      match resolve w_3 K with
      | None -> ()
      | Some (hd_0, tl_0) -> (
          match Word.get_value hd_0 with
          | c_2 when c_2 = tag_cont_done -> exec_done w_3
          | c_2 when c_2 = tag_cont_1 ->
              w_3.state.k <- get_next_cont tl_0;
              restore_env w_3 1 tl_0;
              assert_env_length w_3 2;
              let x1_2 = pop_env w_3 in
              let x0_2 = pop_env w_3 in
              push_env w_3 (Memo.appends [ Memo.from_constructor tag_Cons; x0_2; x1_2 ]);
              assert_env_length w_3 1;
              drop_n w_3 1 0;
              assert_env_length w_3 1;
              return_n w_3 1 (pc_to_exp (int_to_pc 0))
          | _ -> failwith "unreachable"))
    0

let () =
  add_exp
    (fun w_0 ->
      assert_env_length w_0 1;
      push_env w_0 (Dynarray.get w_0.state.e 0);
      w_0.state.c <- pc_to_exp (int_to_pc 3);
      stepped w_0)
    1

let () =
  add_exp
    (fun w_2 ->
      assert_env_length w_2 5;
      match resolve w_2 (Source.E 3) with
      | None -> ()
      | Some x0_1 -> (
          match resolve w_2 (Source.E 4) with
          | None -> ()
          | Some x1_1 ->
              ignore (pop_env w_2);
              ignore (pop_env w_2);
              push_env w_2 (Memo.from_int (Word.get_value (fst x0_1) + Word.get_value (fst x1_1)));
              assert_env_length w_2 4;
              push_env w_2 (Dynarray.get w_2.state.e 2);
              assert_env_length w_2 5;
              let keep_0 = env_call w_2 [ 3 ] 1 in
              w_2.state.k <- Memo.appends [ Memo.from_constructor tag_cont_1; keep_0; w_2.state.k ];
              w_2.state.c <- pc_to_exp (int_to_pc 1);
              stepped w_2))
    2

let () =
  add_exp
    (fun w_1 ->
      assert_env_length w_1 2;
      let last_0 = Source.E 1 in
      match resolve w_1 last_0 with
      | None -> ()
      | Some x_0 -> (
          ignore (pop_env w_1);
          match Word.get_value (fst x_0) with
          | c_1 when c_1 = tag_Nil ->
              assert_env_length w_1 1;
              push_env w_1 (Memo.from_constructor tag_Nil);
              assert_env_length w_1 2;
              return_n w_1 2 (pc_to_exp (int_to_pc 0))
          | c_1 when c_1 = tag_Cons ->
              let splits_0 = Memo.splits (snd x_0) in
              let split0_0 = List.nth splits_0 0 in
              let split1_0 = List.nth splits_0 1 in
              push_env w_1 split0_0;
              push_env w_1 split1_0;
              assert_env_length w_1 3;
              push_env w_1 (Dynarray.get w_1.state.e 1);
              assert_env_length w_1 4;
              push_env w_1 (Memo.from_int 1);
              w_1.state.c <- pc_to_exp (int_to_pc 2);
              stepped w_1
          | _ -> failwith "unreachable"))
    3

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 (-1)
let () = Value.set_constructor_degree 3 (-1)
