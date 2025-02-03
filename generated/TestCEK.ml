open Ant
open Word
open Memo
open Common

type ocaml_int_list = Nil | Cons of int * Memo.seq

let int_list_Nil : Memo.seq = Memo.appends [ Memo.from_constructor 1 ]
let int_list_Cons x0 x1 : Memo.seq = Memo.appends [ Memo.from_constructor 2; Memo.from_int x0; x1 ]
let from_ocaml_int_list x = match x with Nil -> int_list_Nil | Cons (x0, x1) -> int_list_Cons x0 x1

let to_ocaml_int_list x =
  let h, t = Option.get (Memo.list_match x) in
  match Word.get_value h with
  | 1 -> Nil
  | 2 ->
      let [ x0; x1 ] = Memo.splits t in
      Cons (Memo.to_int x0, x1)

let rec list_incr (x0 : seq) : seq = exec_cek (pc_to_exp 10) (Dynarray.of_list [ x0 ]) (Memo.from_constructor 0)

let 0 =
  add_exp (fun x ->
      todo "return last element in env by applying the continuation";
      x)

let 1 =
  add_exp (fun x ->
      x.c <- pc_to_exp 10;
      x)

let 2 = add_exp (fun x -> todo "Nil")

let 3 =
  add_exp (fun x ->
      let x1 = (Dynarray.pop_last x.e).seq in
      let x0 = (Dynarray.pop_last x.e).seq in
      Dynarray.add_last x.e (value_at_depth (Memo.appends [ Memo.from_constructor 2; x0; x1 ]) x.d);
      x.c <- pc_to_exp 0;
      x)

let 4 =
  add_exp (fun x ->
      let sf = env_keep_last_n x 1 in
      x.c <- pc_to_exp 1;
      x)

let 5 =
  add_exp (fun x ->
      Dynarray.add_last x.e (Dynarray.get x.e 2);
      x.c <- pc_to_exp 4;
      x)

let 6 =
  add_exp (fun x ->
      let x1 = (Dynarray.pop_last x.e).seq in
      let x0 = (Dynarray.pop_last x.e).seq in
      Dynarray.add_last x.e (value_at_depth (Memo.from_int (Memo.to_int x0 + Memo.to_int x1)) x.d);
      x.c <- pc_to_exp 5;
      x)

let 7 =
  add_exp (fun x ->
      Dynarray.add_last x.e (value_at_depth (Memo.from_int 1) x.d);
      x.c <- pc_to_exp 6;
      x)

let 8 =
  add_exp (fun x ->
      Dynarray.add_last x.e (Dynarray.get x.e 1);
      x.c <- pc_to_exp 7;
      x)

let 9 =
  add_exp (fun x ->
      let last = (Dynarray.pop_last x.e).seq in
      let hd, tl = Option.get (resolve_seq x last) in
      match Word.get_value hd with
      | 1 ->
          x.c <- pc_to_exp 2;
          x
      | 2 ->
          let [ x0; x1 ] = Memo.splits tl in
          Dynarray.add_last x.e (value_at_depth x0 x.d);
          Dynarray.add_last x.e (value_at_depth x1 x.d);
          x.c <- pc_to_exp 8;
          x)

let 10 =
  add_exp (fun x ->
      Dynarray.add_last x.e (Dynarray.get x.e 0);
      x.c <- pc_to_exp 9;
      x)

let () = Memo.set_constructor_degree 0 1
let () = Memo.set_constructor_degree 1 1
let () = Memo.set_constructor_degree 2 (-1)
