open Ant
open Word
open Memo
open Value

let memo = Array.init 4 (fun _ -> ref State.BlackHole)

type ocaml_int_list = Nil | Cons of Value.seq * Value.seq

let int_list_Nil : Value.seq = Memo.appends [ Memo.from_constructor 1 ]
let int_list_Cons x0 x1 : Value.seq = Memo.appends [ Memo.from_constructor 2; x0; x1 ]
let from_ocaml_int_list x = match x with Nil -> int_list_Nil | Cons (x0, x1) -> int_list_Cons x0 x1

let to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 -> Nil
  | 2 ->
      let [ x0; x1 ] = Memo.splits t in
      Cons (x0, x1)

let rec list_incr (x0 : Value.seq) : Value.seq =
  exec_cek (pc_to_exp 1) (Dynarray.of_list [ x0 ]) (Memo.from_constructor 0) memo

let () =
  add_exp
    (fun w ->
      assert_env_length w 1;
      match resolve w K with
      | None -> ()
      | Some (hd, tl) -> (
          match Word.get_value hd with
          | 0 -> exec_done w
          | 3 ->
              ();
              (assert_env_length w) 1;
              w.state.k <- get_next_cont tl;
              ((restore_env w) 1) tl;
              (();
               (assert_env_length w) 2;
               let x1_2 = pop_env w in
               let x0_2 = pop_env w in
               (push_env w) (Memo.appends [ Memo.from_constructor 2; x0_2; x1_2 ]));
              ();
              (assert_env_length w) 1;
              ((drop_n w) 1) 0;
              (assert_env_length w) 1;
              ((return_n w) 1) (pc_to_exp 0)))
    0

let () =
  add_exp
    (fun w_0 ->
      ();
      (assert_env_length w_0) 1;
      (push_env w_0) ((Dynarray.get w_0.state.e) 0);
      w_0.state.c <- pc_to_exp 3;
      stepped w_0)
    1

let () =
  add_exp
    (fun w_2 ->
      ();
      (assert_env_length w_2) 5;
      match (resolve w_2) (Source.E 3) with
      | None -> ()
      | Some x0_1 -> (
          match (resolve w_2) (Source.E 4) with
          | None -> ()
          | Some x1_1 ->
              ();
              ignore (pop_env w_2);
              ignore (pop_env w_2);
              (push_env w_2) (Memo.from_int (Word.to_int (fst x0_1) + Word.to_int (fst x1_1)));
              ();
              (assert_env_length w_2) 4;
              (push_env w_2) ((Dynarray.get w_2.state.e) 2);
              (();
               (assert_env_length w_2) 5;
               let keep_0 = ((env_call w_2) [ 3 ]) 1 in
               w_2.state.k <- Memo.appends [ Memo.from_constructor 3; keep_0; w_2.state.k ]);
              w_2.state.c <- pc_to_exp 1;
              stepped w_2))
    2

let () =
  add_exp
    (fun w_1 ->
      (assert_env_length w_1) 2;
      let last_0 = Source.E 1 in
      match (resolve w_1) last_0 with
      | None -> ()
      | Some x_0 -> (
          ignore (pop_env w_1);
          match Word.get_value (fst x_0) with
          | 1 ->
              ();
              (assert_env_length w_1) 1;
              (push_env w_1) (Memo.from_constructor 1);
              (assert_env_length w_1) 2;
              ((return_n w_1) 2) (pc_to_exp 0)
          | 2 ->
              let [ x0_0; x1_0 ] = Memo.splits (snd x_0) in
              (push_env w_1) x0_0;
              (push_env w_1) x1_0;
              ();
              (assert_env_length w_1) 3;
              (push_env w_1) ((Dynarray.get w_1.state.e) 1);
              ();
              (assert_env_length w_1) 4;
              (push_env w_1) (Memo.from_int 1);
              w_1.state.c <- pc_to_exp 2;
              stepped w_1))
    3

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 (-1)
let () = Value.set_constructor_degree 3 (-1)
