1

open Ant
open Word
open Memo

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
    (fun x ->
      assert_env_length x 1;
      match resolve_seq x x.k.seq with
      | None -> resolve_failed x memo
      | Some (hd, tl) -> (
          match Word.get_value hd with
          | 0 -> (fun x tl -> exec_done x) x tl
          | 3 ->
              (fun x tl ->
                restore_env x 1 tl;
                x.k <- value_at_depth (get_next_cont tl) x.d;
                x.c <- pc_to_exp 9;
                stepped x)
                x tl))
    0

let () =
  add_exp
    (fun x ->
      x.c <- pc_to_exp 2;
      x)
    1

let () =
  add_exp
    (fun x ->
      assert_env_length x 1;
      push_env x (Dynarray.get x.e 0);
      x.c <- pc_to_exp 3;
      stepped x)
    2

let () =
  add_exp
    (fun x ->
      assert_env_length x 2;
      let last = (Dynarray.get_last x.e).seq in
      match resolve_seq x last with
      | None -> resolve_failed x memo
      | Some (hd, tl) -> (
          Dynarray.remove_last x.e;
          match Word.get_value hd with
          | 1 ->
              x.c <- pc_to_exp 4;
              x
          | 2 ->
              let [ x0; x1 ] = Memo.splits tl in
              push_env x (value_at_depth x0 x.d);
              push_env x (value_at_depth x1 x.d);
              x.c <- pc_to_exp 6;
              stepped x))
    3

let () =
  add_exp
    (fun x ->
      assert_env_length x 1;
      push_env x (value_at_depth (Memo.from_constructor 1) x.d);
      x.c <- pc_to_exp 5;
      stepped x)
    4

let () =
  add_exp
    (fun x ->
      assert_env_length x 2;
      return_n x 2 (pc_to_exp 0))
    5

let () =
  add_exp
    (fun x ->
      assert_env_length x 3;
      push_env x (Dynarray.get x.e 1);
      x.c <- pc_to_exp 7;
      stepped x)
    6

let () =
  add_exp
    (fun x ->
      assert_env_length x 4;
      push_env x (value_at_depth (Memo.from_int 1) x.d);
      x.c <- pc_to_exp 8;
      stepped x)
    7

let () =
  add_exp
    (fun x ->
      assert_env_length x 5;
      match resolve_seq x (Dynarray.get x.e 3).seq with
      | None -> resolve_failed x memo
      | Some (x0, _) -> (
          match resolve_seq x (Dynarray.get x.e 4).seq with
          | None -> resolve_failed x memo
          | Some (x1, _) ->
              Dynarray.remove_last x.e;
              Dynarray.remove_last x.e;
              push_env x (value_at_depth (Memo.from_int (x0 + x1)) x.d);
              x.c <- pc_to_exp 12;
              stepped x))
    8

let () =
  add_exp
    (fun x ->
      assert_env_length x 2;
      let x1 = (pop_env x).seq in
      let x0 = (pop_env x).seq in
      push_env x (value_at_depth (Memo.appends [ Memo.from_constructor 2; x0; x1 ]) x.d);
      x.c <- pc_to_exp 10;
      stepped x)
    9

let () =
  add_exp
    (fun x ->
      assert_env_length x 1;
      drop_n x 1 0 (pc_to_exp 11))
    10

let () =
  add_exp
    (fun x ->
      assert_env_length x 1;
      return_n x 1 (pc_to_exp 0))
    11

let () =
  add_exp
    (fun x ->
      assert_env_length x 4;
      push_env x (Dynarray.get x.e 2);
      x.c <- pc_to_exp 13;
      stepped x)
    12

let () =
  add_exp
    (fun x ->
      assert_env_length x 5;
      let keep = env_call x [ 3 ] 1 in
      x.k <- value_at_depth (Memo.appends [ Memo.from_constructor 3; keep; x.k.seq ]) x.d;
      x.c <- pc_to_exp 1;
      stepped x)
    13

let () = Value.set_constructor_degree 0 1
let () = Value.set_constructor_degree 1 1
let () = Value.set_constructor_degree 2 (-1)
let () = Value.set_constructor_degree 3 (-1)
